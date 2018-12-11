package analytics

import analytics.DatasetSource.HDFSSource
import cats.effect.IO
import org.apache.hadoop.fs.Path
import org.apache.mesos._

import scala.concurrent.ExecutionContext.global
import scala.concurrent.duration._

object Main {

  lazy val frameworkInfo: Protos.FrameworkInfo =
    Protos.FrameworkInfo.newBuilder
      .setName("ANALYTICS")
      .setFailoverTimeout(60.seconds.toMillis)
      .setCheckpoint(false)
      .setUser("") // Mesos can do this for us
      .build

  def printUsage(): Unit = {
    println("""
      |Usage:
      |  run <mesos-master>
    """.stripMargin)
  }

  def main(args: Array[String]): Unit = {

    if (args.length != 1) {
      printUsage()
      sys.exit(1)
    }

    val Seq(mesosMaster) = args.toSeq

    println(s"""
      |ANALYTICS
      |=======
      |
      |mesosMaster: [$mesosMaster]
      |
    """.stripMargin)


    def exampleOpProgram[F[_, _]](F: DatasetOp[F]): F[Unit, Int] =
      F.compose(
        F.compose(
          F.combineWith(
            F.source(HDFSSource(new Path("./enwik9"))),
            F.literal(" ")
          )(Fn.Split),
          F.concatMap(Fn.Identity[List[String]])
        ),
        F.fmap[String, Int](Fn.Literal(1, Type[Int]))
      )

    val opProgram = new DatasetOpProgram[Unit, Int] {
      def apply[F[_, _]](F: DatasetOp[F]): F[Unit, Int] = exampleOpProgram(F)
    }

    def exampleFoldProgram[F[_]](F: DatasetFold[F]): F[Int] =
      F.commutativeFold[Int]

    val foldProgram = new DatasetFoldProgram[Int] {
      def apply[F[_]](F: DatasetFold[F]): F[Int] = exampleFoldProgram(F)
    }

    implicit val ctx = IO.contextShift(global)

    val cancelToken = Analytics.createMesosJob(opProgram, foldProgram, 0, frameworkInfo, mesosMaster)
        .unsafeRunCancelable(eta => println("Result: " + eta))


    val newLine = '\n'.toInt
    while (System.in.read != newLine) {
      Thread.sleep(1000)
    }

    cancelToken.unsafeRunSync()
    sys.exit(0)
  }
}
