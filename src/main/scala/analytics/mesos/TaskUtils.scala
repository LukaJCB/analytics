package analytics.mesos

import com.google.protobuf.ByteString
import io.circe.syntax._
import org.apache.mesos._

import scala.collection.JavaConverters._


object TaskUtils extends TaskUtils

trait TaskUtils {
  val TASK_CPUS = 1.0
  val TASK_MEM = 256.0

  val uris: Seq[Protos.CommandInfo.URI] =
    Seq(
      System.getProperty("user.dir") + "/target/scala-2.12/analytics-assembly-0.1.0.jar",
      System.getProperty("user.dir") + "/enwik9"
    ).map {
        fName =>
          Protos.CommandInfo.URI.newBuilder
            .setValue(fName)
            .setExtract(false)
            .build
      }

  def makeTaskPrototype(id: String, offer: Protos.Offer): Protos.TaskInfo =
    Protos.TaskInfo.newBuilder
      .setTaskId(Protos.TaskID.newBuilder.setValue(id))
      .setName("")
      .setSlaveId((offer.getSlaveId))
      .addAllResources(
        Seq(
          scalarResource("cpus", TASK_CPUS),
          scalarResource("mem", TASK_MEM)
        ).asJava
      )
      .build

  protected def scalarResource(name: String, value: Double): Protos.Resource =
    Protos.Resource.newBuilder
      .setType(Protos.Value.Type.SCALAR)
      .setName(name)
      .setScalar(Protos.Value.Scalar.newBuilder.setValue(value))
      .build

  lazy val defaultExecutor: Protos.ExecutorInfo = {
    val command = Protos.CommandInfo.newBuilder
      .setValue("java -cp analytics-assembly-0.1.0.jar analytics.mesos.executor.DefaultExecutorMain")
      .addAllUris(uris.asJava)
    Protos.ExecutorInfo.newBuilder
      .setExecutorId(Protos.ExecutorID.newBuilder.setValue("default-executor"))
      .setName("Default")
      .setCommand(command)
      .build
  }

  def makeDefaultTask(
    id: String,
    payload: TaskPayload,
    offer: Protos.Offer): Protos.TaskInfo =
    makeTaskPrototype(id, offer).toBuilder
      .setName(s"default_$id")
      .setExecutor(defaultExecutor)
      .setData(ByteString.copyFromUtf8(payload.asJson.noSpaces))
      .build


  def maxTasksForOffer(
    offer: Protos.Offer): Int = {
    var count = 0
    var cpus = 0.0
    var mem = 0.0

    for (resource <- offer.getResourcesList.asScala) {
      resource.getName match {
        case "cpus" => cpus = resource.getScalar.getValue
        case "mem"  => mem = resource.getScalar.getValue
        case _      => ()
      }
    }

    while (cpus >= TASK_CPUS && mem >= TASK_MEM) {
      count = count + 1
      cpus = cpus - TASK_CPUS
      mem = mem - TASK_MEM
    }

    count
  }

  def isTerminal(state: Protos.TaskState): Boolean = {
    import Protos.TaskState._
    state match {
      case TASK_FINISHED | TASK_FAILED | TASK_KILLED | TASK_LOST =>
        true
      case _ =>
        false
    }
  }

}


