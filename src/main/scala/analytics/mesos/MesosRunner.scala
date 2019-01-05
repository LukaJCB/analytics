package analytics.mesos

import analytics.{AnalyticsRunner, DatasetFoldProgram, DatasetOpProgram}
import cats.effect.IO
import io.circe.Decoder
import org.apache.mesos.{MesosSchedulerDriver, Protos}

case class MesosRunner(frameworkInfo: Protos.FrameworkInfo, mesosMaster: String) extends AnalyticsRunner {
  def run[A: Decoder](ops: DatasetOpProgram[Unit, A],
                      fold: DatasetFoldProgram[A])(cb: Either[Throwable, Option[(A, BigInt)]] => Unit): IO[Unit] =
    IO(new DefaultScheduler[A](cb, fold, ops))
      .flatMap(scheduler => IO {
        new MesosSchedulerDriver(scheduler, frameworkInfo, mesosMaster).run()
        ()
      })
}
