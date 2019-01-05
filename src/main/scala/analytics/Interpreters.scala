package analytics

import analytics.DatasetFold.RDatasetFold
import analytics.DatasetOp.RDataSetOp
import analytics.classes.{CommutativeMonoidFn, MonoidFn}
import cats.effect.{ContextShift, IO}
import cats.implicits._
import fs2.{Pipe, Stream}

object Interpreters {

  implicit val ctx: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.global)

  def fs2Interp(srcs: Map[Int, SourcePair]): DatasetOp[Pipe[IO, ?, ?]] = new DatasetOp[Pipe[IO, ?, ?]] {
    def source(s: DatasetSource): Pipe[IO, Unit, String] = _ =>
      srcs(s.hash.hash(s)).toStream

    def fmap[A: Type, B: Type](f: Fn[A, B]): Pipe[IO, A, B] = _.map(Fn.interpret(f))

    def filter[A: Type](f: Fn[A, Boolean]): Pipe[IO, A, A] = _.filter(Fn.interpret(f))

    def literal[A: Type](a: => A): Pipe[IO, Unit, A] = _ => Stream.emit(a)

    def concatMap[A: Type, B: Type](f: Fn[A, List[B]]): Pipe[IO, A, B] =
      _.flatMap(a => Stream.emits(Fn.interpret(f)(a)))

    def compose[A: Type, B: Type, C: Type](x: Pipe[IO, A, B], y: Pipe[IO, B, C]): Pipe[IO, A, C] =
      x andThen y

    def merge[A: Type, B: Type, C: Type](x: Pipe[IO, A, C], y: Pipe[IO, B, C]): Pipe[IO, (A, B), C] =
      stream => x(stream.map(_._1)).merge(y(stream.map(_._2)))

    def split[A: Type, B: Type, C: Type, D: Type](x: Pipe[IO, A, B], y: Pipe[IO, C, D]): Pipe[IO, (A, C), (B, D)] =
      stream => (x.compose((s: Stream[IO, (A, C)]) => s.map(_._1))(stream),
        y.compose((s: Stream[IO, (A, C)]) => s.map(_._2))(stream)).tupled
  }


  type TaggedStreamFold[A] = Stream[IO, (A, BigInt)] => IO[A]


  def masterStreamFoldInterp: DatasetFold[TaggedStreamFold] = new DatasetFold[TaggedStreamFold] {
    def fold[A: Type: MonoidFn]: Stream[IO, (A, BigInt)] => IO[A] = stream =>
      StreamOps.reorderResults(stream)
        .compile
        .fold(MonoidFn[A].empty)((x, y) => Fn.interpret(MonoidFn[A].combine)((x, y)))

    def scan[A: Type : MonoidFn]: TaggedStreamFold[A] =
      _ => IO.raiseError(new Exception("Not a bounded operation"))

    def commutativeFold[A: Type: CommutativeMonoidFn]: Stream[IO, (A, BigInt)] => IO[A] =
      _.map(_._1).compile.fold(MonoidFn[A].empty)((x, y) => Fn.interpret(MonoidFn[A].combine)((x, y)))

    def commutativeScan[A: Type: CommutativeMonoidFn]: Stream[IO, (A, BigInt)] => IO[A] =
      _ => IO.raiseError(new Exception("Not a bounded operation"))
  }

  type StreamScan[A] = Stream[IO, (A, BigInt)] => Stream[IO, A]

  def masterStreamScanInterp: DatasetFold[StreamScan] = new DatasetFold[StreamScan] {
    def fold[A: Type: MonoidFn]: StreamScan[A] = stream =>
      StreamOps.reorderResults(stream)
        .fold(MonoidFn[A].empty)((x, y) => Fn.interpret(MonoidFn[A].combine)((x, y)))

    def scan[A: Type : MonoidFn]: StreamScan[A] = stream =>
      StreamOps.reorderResults(stream)
        .scan(MonoidFn[A].empty)((x, y) => Fn.interpret(MonoidFn[A].combine)((x, y)))

    def commutativeFold[A: Type: CommutativeMonoidFn]: StreamScan[A] = stream =>
      stream.map(_._1).fold(MonoidFn[A].empty)((x, y) => Fn.interpret(MonoidFn[A].combine)((x, y)))

    def commutativeScan[A: Type: CommutativeMonoidFn]: StreamScan[A] = stream =>
      stream.map(_._1).scan(MonoidFn[A].empty)((x, y) => Fn.interpret(MonoidFn[A].combine)((x, y)))
  }

  type StreamFold[A] = Stream[IO, A] => IO[A]

  def agentStreamFoldInterp: DatasetFold[StreamFold] = new DatasetFold[StreamFold] {
    def fold[A: Type : MonoidFn]: Stream[IO, A] => IO[A] =
      _.compile.fold(MonoidFn[A].empty)((x, y) => Fn.interpret(MonoidFn[A].combine)((x, y)))

    def scan[A: Type : MonoidFn]: StreamFold[A] =
      _.compile.fold(MonoidFn[A].empty)((x, y) => Fn.interpret(MonoidFn[A].combine)((x, y)))

    def commutativeFold[A: Type : CommutativeMonoidFn]: Stream[IO, A] => IO[A] =
      _.compile.fold(MonoidFn[A].empty)((x, y) => Fn.interpret(MonoidFn[A].combine)((x, y)))

    def commutativeScan[A: Type : CommutativeMonoidFn]: Stream[IO, A] => IO[A] =
      _.compile.fold(MonoidFn[A].empty)((x, y) => Fn.interpret(MonoidFn[A].combine)((x, y)))
  }



  def decode[A](rd: RDataSetOp, rf: RDatasetFold, tpeA: Reified, tpeB: Reified)
               (interp: DatasetOp[Pipe[IO, ?, ?]]): Either[AnalyticsError, IO[A]] =
    DatasetOp.unfree[Any, A](rd, tpeA, tpeB).apply[Pipe[IO, ?, ?]](interp).flatMap(pipe =>
      DatasetFold.unfree[A](rf, tpeB).apply[StreamFold](agentStreamFoldInterp).map(streamFold =>
        streamFold(
          pipe(Stream[IO, Unit](()))
        )
      )
    )

}
