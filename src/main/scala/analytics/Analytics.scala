package analytics

import analytics.mesos.DefaultScheduler
import cats.effect.{ContextShift, IO}
import io.circe.Decoder
import org.apache.mesos._

import scala.concurrent.duration._

object Analytics {
  def createMesosJob[A: Decoder](ops: DatasetOpProgram[Unit, A],
                                 fold: DatasetFoldProgram[A],
                                 init: A,
                                 frameworkInfo: Protos.FrameworkInfo,
                                 mesosMaster: String)
                                (implicit ctx: ContextShift[IO]): IO[A] =
    IO.cancelable { k =>
      val scheduler = new DefaultScheduler[A](k, fold, ops, init)
      val driver: SchedulerDriver =
        new MesosSchedulerDriver(scheduler, frameworkInfo, mesosMaster)
      IO(driver.run()).start.unsafeRunAsyncAndForget()

      IO(scheduler.shutdown(5.minutes) { driver.stop(); () })
    }

}
