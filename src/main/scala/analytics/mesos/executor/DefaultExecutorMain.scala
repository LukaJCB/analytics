package analytics.mesos.executor

import org.apache.mesos.MesosExecutorDriver
import org.apache.mesos.Protos.Status

object DefaultExecutorMain {
  def main(args: Array[String]): Unit = {
    val driver = new MesosExecutorDriver(DefaultExecutor)
    System.exit(if (driver.run() == Status.DRIVER_STOPPED) 0 else 1)
  }
}
