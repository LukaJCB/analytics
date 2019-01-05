package analytics

import cats.effect.{ContextShift, IO}
import io.circe.Decoder
import fs2.Stream

object Analytics {

  def runBounded[A: Decoder](ops: DatasetOpProgram[Unit, A],
    fold: DatasetFoldProgram[A],
    runner: AnalyticsRunner)(implicit ctx: ContextShift[IO]): IO[A] = {
    val stream = StreamOps.createResultStream(ops, fold, runner)
    fold(Interpreters.masterStreamFoldInterp)(stream)
  }


  def runUnbounded[A: Decoder](ops: DatasetOpProgram[Unit, A],
                               fold: DatasetFoldProgram[A],
                               runner: AnalyticsRunner)(implicit ctx: ContextShift[IO]): Stream[IO, A] = {
    val stream = StreamOps.createResultStream(ops, fold, runner)
    fold(Interpreters.masterStreamScanInterp)(stream)
  }

  type Data[A] = DatasetOpProgram[Unit, A]



  implicit class DSLSyntax[A: Type](data: Data[A]) {
    def map[B: Type](f: Fn[A, A] => Fn[A, B]): Data[B] = new DatasetOpProgram[Unit, B] {
      def apply[F[_, _]](F: DatasetOp[F]): F[Unit, B] =
        F.compose(data(F), F.fmap(f(Fn.Identity())))
    }
  }

}
