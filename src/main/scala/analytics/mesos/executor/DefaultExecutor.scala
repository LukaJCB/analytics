package analytics.mesos.executor

import java.nio.charset.Charset

import analytics._
import analytics.mesos.TaskPayload
import cats.implicits._
import cats.effect.IO
import io.circe.parser.decode
import io.circe.Encoder
import org.apache.mesos.{Executor, ExecutorDriver, Protos}
import org.apache.mesos.Protos.{TaskState, TaskStatus}



object DefaultExecutor extends Executor {
  def registered(driver: ExecutorDriver, executorInfo: Protos.ExecutorInfo, frameworkInfo: Protos.FrameworkInfo, slaveInfo: Protos.SlaveInfo): Unit =
    ()

  def reregistered(driver: ExecutorDriver, slaveInfo: Protos.SlaveInfo): Unit = ()

  def disconnected(driver: ExecutorDriver): Unit = println("Disconnected")

  def launchTask(driver: ExecutorDriver, task: Protos.TaskInfo): Unit = {
    val status = TaskStatus.newBuilder().setTaskId(task.getTaskId).setState(TaskState.TASK_RUNNING)
    driver.sendStatusUpdate(status.build())

    val result: IO[Either[AnalyticsError, Array[Byte]]] = decode[TaskPayload](task.getData.toStringUtf8)
      .leftMap(CirceErr(_): AnalyticsError)
      .flatTraverse { p =>
        implicit val anyType: Type[Any] = Type.typeFromReified(p.tpeB)

        Interpreters.decode[Any](p.ops, p.fold, p.tpeA, p.tpeB)(Interpreters
          .fs2Interp(p.data)).traverse(io =>

          io.map(payload =>
            Encoder.encodeTuple2(anyType.encoder, Encoder[BigInt]).apply(payload -> p.id).noSpaces.getBytes)
        )
      }
    result.flatMap {
      case Left(e) => IO {
        println("Err" + e.toString)
        driver.sendStatusUpdate(TaskStatus.newBuilder.setTaskId(task.getTaskId).setState(TaskState.TASK_FAILED).build)
      }
      case Right(bytes) => IO {
          driver.sendFrameworkMessage(bytes)
          val finished = TaskStatus.newBuilder.setTaskId(task.getTaskId).setState(TaskState.TASK_FINISHED).build
          driver.sendStatusUpdate(finished)
        }
    }.onError { case e => IO {
      println("Error" + e.toString)
      driver.sendStatusUpdate(TaskStatus.newBuilder.setTaskId(task.getTaskId).setState(TaskState.TASK_FAILED).build)
      ()
    }}.unsafeRunAsyncAndForget()
  }

  def killTask(driver: ExecutorDriver, taskId: Protos.TaskID): Unit = println(s"$taskId was killed :(")

  def frameworkMessage(driver: ExecutorDriver, data: Array[Byte]): Unit = {
    println(new String(data, Charset.forName("UTF-8")))
  }

  def shutdown(driver: ExecutorDriver): Unit = println("Shutdown")

  def error(driver: ExecutorDriver, message: String): Unit = println(message)
}
