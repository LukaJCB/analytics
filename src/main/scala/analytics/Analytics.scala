package analytics

import cats.effect.{ContextShift, IO}
import cats.implicits._
import io.circe.Decoder
import fs2.Stream
import fs2.concurrent.Queue


object Analytics {

  def createResultStream[A: Decoder](ops: DatasetOpProgram[Unit, A],
                                     fold: DatasetFoldProgram[A],
                                     runner: AnalyticsRunner)(implicit ctx: ContextShift[IO]): Stream[IO, A] = {


    val startDriver: IO[Queue[IO, Either[Throwable, Option[A]]]] =
      Queue.unbounded[IO, Either[Throwable, Option[A]]]
        .flatTap(queue => runner.run(ops, fold)(e => queue.enqueue1(e).unsafeRunAsyncAndForget()))

    Stream.eval(startDriver).flatMap(_.dequeue.rethrow.unNoneTerminate)

  }


  type Data[A] = DatasetOpProgram[Unit, A]



  implicit class DSLSyntax[A: Type](data: Data[A]) {
    def map[B: Type](f: Fn[A, A] => Fn[A, B]): Data[B] = new DatasetOpProgram[Unit, B] {
      def apply[F[_, _]](F: DatasetOp[F]): F[Unit, B] =
        F.compose(data(F), F.fmap(f(Fn.Identity())))
    }
  }

}
