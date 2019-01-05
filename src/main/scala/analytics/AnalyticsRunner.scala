package analytics

import cats.effect.IO
import io.circe.Decoder

trait AnalyticsRunner {
  def run[A: Decoder](ops: DatasetOpProgram[Unit, A],
                      fold: DatasetFoldProgram[A])
                     (cb: Either[Throwable, Option[(A, BigInt)]] => Unit): IO[Unit]
}
