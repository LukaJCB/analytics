package analytics

import cats.effect.{ContextShift, IO}
import cats.effect.concurrent.Ref
import cats.implicits._
import fs2.Stream
import fs2.concurrent.Queue
import io.circe.Decoder

import scala.annotation.tailrec
import scala.collection.immutable.SortedMap

object StreamOps {
  def createResultStream[A: Decoder](ops: DatasetOpProgram[Unit, A],
                                     fold: DatasetFoldProgram[A],
                                     runner: AnalyticsRunner)(implicit ctx: ContextShift[IO]): Stream[IO, (A, BigInt)] = {


    val startDriver: IO[Queue[IO, Either[Throwable, Option[(A, BigInt)]]]] =
      Queue.unbounded[IO, Either[Throwable, Option[(A, BigInt)]]]
        .flatTap(queue => runner.run(ops, fold)(e => queue.enqueue1(e).unsafeRunAsyncAndForget()))

    Stream.eval(startDriver).flatMap(_.dequeue.rethrow.unNoneTerminate)

  }

  def reorderResults[A](stream: Stream[IO, (A, BigInt)])(implicit ctx: ContextShift[IO]): Stream[IO, A] = {
    val orderedQueue = for {
      searchIdRef <- Ref[IO].of(BigInt(0))
      searchTreeRef <- Ref[IO].of(SortedMap.empty[BigInt, A])
      queue <- Queue.unbounded[IO, Option[A]]
      _ <- emitOrderedIntoQueue(stream, searchIdRef, searchTreeRef, queue)
    } yield queue

    Stream.eval(orderedQueue).flatMap(_.dequeue.unNoneTerminate)
  }

  def emitOrderedIntoQueue[A](stream: Stream[IO, (A, BigInt)],
                              searchIdRef: Ref[IO, BigInt],
                              searchTreeRef: Ref[IO, SortedMap[BigInt, A]],
                              queue: Queue[IO, Option[A]]): IO[Unit] =
    (stream.evalMap {
      case (a, id) => for {
        searchId <- searchIdRef.get
        searchTree <- searchTreeRef.get
        (nextTree, nextElems, nextId) = searchNext(id, a, searchTree, searchId)
        _ <- searchIdRef.set(nextId)
        _ <- searchTreeRef.set(nextTree)
        _ <- nextElems.traverse_(a => queue.enqueue1(Some(a)))
      } yield ()
    } ++ Stream.eval(queue.enqueue1(None))).compile.drain



  def searchNext[A](currentId: BigInt,
                    currentElem: A,
                    searchTree: SortedMap[BigInt, A],
                    searchId: BigInt): (SortedMap[BigInt, A], List[A], BigInt) = {

    @tailrec
    def go(currentId: BigInt,
           currentElem: A,
           searchTree: SortedMap[BigInt, A],
           searchId: BigInt,
           lastElems: List[A]): (SortedMap[BigInt, A], List[A], BigInt) =
      if (searchId == currentId) {
        searchTree.headOption match {
          case Some((nextId, next)) => go(nextId, next, searchTree.tail, searchId + 1, currentElem :: lastElems)
          case None => (SortedMap.empty, currentElem :: lastElems, searchId + 1)
        }
      } else (searchTree + (currentId -> currentElem), lastElems, searchId)

    val reversed = go(currentId, currentElem, searchTree, searchId, Nil)

    reversed.copy(_2 = reversed._2.reverse)
  }
}
