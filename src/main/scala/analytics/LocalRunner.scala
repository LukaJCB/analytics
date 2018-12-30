package analytics
import cats.effect.IO
import io.circe.Decoder
import fs2.{Pipe, Stream}

object LocalRunner extends AnalyticsRunner {
  def run[A: Decoder](ops: DatasetOpProgram[Unit, A],
                      fold: DatasetFoldProgram[A])
                     (cb: Either[Throwable, Option[A]] => Unit): IO[Unit] = {
    val sources = DatasetSource.createPayloads(ops(DatasetOp.allSources).getConst)
    val producer = sources.flatMap { src =>
      val pipe = ops[Pipe[IO, ?, ?]](Interpreters.fs2Interp(src))

      pipe(Stream.emit(()))
    }
    val stream = producer.map(Some(_)).attempt ++ Stream.emit(Right(None))

    stream.evalMap(etoa => IO(cb(etoa))).compile.drain
  }

}
