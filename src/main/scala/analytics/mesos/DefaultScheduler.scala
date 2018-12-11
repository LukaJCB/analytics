package analytics.mesos

import java.nio.charset.Charset

import analytics._
import cats.effect.IO
import cats.implicits._
import io.circe.Decoder
import io.circe.parser._
import org.apache.mesos._

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Try

class DefaultScheduler[A](val callback: Either[Throwable, A] => Unit,
                          val fold: DatasetFoldProgram[A],
                          val ops: DatasetOpProgram[Unit, A],
                          val init: A)(implicit A: Decoder[A])
    extends Scheduler
    with TaskUtils {

  private val iterator = DatasetSource
    .createPayloads(ops(DatasetOp.allSources).getConst).compile.to[Iterator].unsafeRunSync()
  private var tasksCreated = 0
  private var tasksRunning = 0
  private var result = init
  private var shuttingDown: Boolean = false


  def waitForRunningTasks(): Unit = {
    while (tasksRunning > 0) {
      println(s"Shutting down but still have $tasksRunning tasks running.")
      Thread.sleep(3000)
    }
  }

  def shutdown(maxWait: Duration)(callback: => Unit): Unit = {
    println("Scheduler shutting down...")
    shuttingDown = true

    val f = Future { waitForRunningTasks() }
    Try { Await.ready(f, maxWait) }

    callback
  }

  def printQueueStatistics(): Unit = println(s"""
    |Queue Statistics:
    |  Running tasks:       [$tasksRunning]
  """.stripMargin)

  def disconnected(driver: SchedulerDriver): Unit =
    println("Disconnected from the Mesos master...")

  def error(driver: SchedulerDriver, msg: String): Unit =
    println(s"ERROR: [$msg]")

  def executorLost(
    driver: SchedulerDriver,
    executorId: Protos.ExecutorID,
    slaveId: Protos.SlaveID,
    status: Int): Unit =
    println(s"EXECUTOR LOST: [${executorId.getValue}]")

  def frameworkMessage(
    driver: SchedulerDriver,
    executorId: Protos.ExecutorID,
    slaveId: Protos.SlaveID,
    data: Array[Byte]): Unit = {

    println(s"Received a framework message from [${executorId.getValue}]")

    val jsonString = new String(data, Charset.forName("UTF-8"))

    executorId.getValue match {
      case id if id == defaultExecutor.getExecutorId.getValue =>
        val decoded = decode[A](jsonString)

        decoded match {
          case Right(n) =>
            result = fold(Interpreters.foldInterp)(result, n)
          case Left(e) =>
            fail(e, driver).unsafeRunSync()

        }

      case _ => ()
    }
  }

  def offerRescinded(
    driver: SchedulerDriver,
    offerId: Protos.OfferID): Unit =
    println(s"Offer [${offerId.getValue}] has been rescinded")

  def registered(
    driver: SchedulerDriver,
    frameworkId: Protos.FrameworkID,
    masterInfo: Protos.MasterInfo): Unit = {
    val host = masterInfo.getHostname
    val port = masterInfo.getPort
    println(s"Registered with Mesos master [$host:$port]")
  }


  def finish(driver: SchedulerDriver): IO[Unit] = IO {
    callback(Right(result))
    driver.stop()
    ()
  }


  def fail(err: Exception, driver: SchedulerDriver): IO[Unit] = IO {
    callback(Left(err))
    driver.abort()
    ()
  }

  def reregistered(
    driver: SchedulerDriver,
    masterInfo: Protos.MasterInfo): Unit = ()

  def resourceOffers(
    driver: SchedulerDriver,
    offers: java.util.List[Protos.Offer]): Unit = {

    printQueueStatistics()

    for (offer <- offers.asScala) {

      if (shuttingDown) {
        println(s"Shutting down: declining offer on [${offer.getHostname}]")
        driver.declineOffer(offer.getId)
      }
      else {
        val maxTasks = maxTasksForOffer(offer)

        def createPayload(pl: Map[Int, SourcePair]) = TaskPayload(
          pl,
          ops(DatasetOp.free).getConst,
          fold(DatasetFold.free).getConst,
          Type[Unit].reify,
          Type[Int].reify
        )

        val tasks = tasksCreated.until(tasksCreated + maxTasks).toList.mapFilter { n =>
          if (iterator.hasNext) {
            val next = iterator.next()
            Some(makeDefaultTask(s"$n", createPayload(next), offer))
          }
          else None
        }
        tasksCreated += tasks.length

        println(s"Launching ${tasks.length} tasks")

        if (maxTasks == 0)
          driver.declineOffer(offer.getId)
        else if (tasks.nonEmpty)
          driver.launchTasks(Seq(offer.getId).asJava, tasks.asJava)
      }
    }
  }

  def slaveLost(
    driver: SchedulerDriver,
    slaveId: Protos.SlaveID): Unit =
    println("SLAVE LOST: [${slaveId.getValue}]")

  def statusUpdate(
    driver: SchedulerDriver,
    taskStatus: Protos.TaskStatus): Unit = {
    val taskId = taskStatus.getTaskId.getValue
    val state = taskStatus.getState
    println(s"Task [$taskId] is in state [$state]")
    if (state == Protos.TaskState.TASK_RUNNING)
      tasksRunning = tasksRunning + 1
    else if (isTerminal(state)) {
      tasksRunning = math.max(0, tasksRunning - 1)
      if (tasksRunning == 0 && !iterator.hasNext)
        finish(driver).unsafeRunSync()
    }
  }

}