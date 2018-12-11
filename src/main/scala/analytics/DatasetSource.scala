package analytics

import java.nio.charset.Charset

import cats.effect.IO
import cats.implicits._
import cats.kernel.Hash
import fs2.Stream
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}
import org.apache.hadoop.conf.Configuration
import org.apache.hadoop.fs.{FileSystem, Path}

import scala.collection.immutable.SortedMap

sealed trait DatasetSource {
  type Payload

  def toStream(payload: Payload): Stream[IO, String]

  def payloadEncoder: Encoder[Payload]
  def payloadDecoder: Decoder[Payload]
  def hash: Hash[this.type]

  def getPayloads: Stream[IO, SourcePair]
}

object DatasetSource {

  def createPayloads(srcs: SortedMap[Int, DatasetSource]): Stream[IO, Map[Int, SourcePair]] =
    srcs.traverse(_.getPayloads)

  case class HDFSSource(path: Path) extends DatasetSource {
    type Payload = HDFSPayload

    def payloadDecoder: Decoder[HDFSPayload] = deriveDecoder
    def payloadEncoder: Encoder[HDFSPayload] = deriveEncoder
    def hash: Hash[this.type] = Hash.fromUniversalHashCode

    def getFileSystem: Stream[IO, FileSystem] = Stream.eval(
      IO(FileSystem.get(new Configuration()))
    )


    def toStream(payload: HDFSPayload): Stream[IO, String] = for {
      fs <- getFileSystem
      blockSize <- Stream.eval(IO(fs.getFileStatus(path).getBlockSize))
      arr <- Stream.eval(IO(new Array[Byte](blockSize.toInt)))
      ioStream <- Stream.bracket(IO(fs.open(path)))(s => IO(s.close()))
      _ <- Stream.eval(IO(ioStream.read(blockSize * payload.blockOffset, arr, 0, blockSize.toInt)))
    } yield new String(arr, Charset.forName("UTF-8"))

    def grabNumberOfBlocks: IO[Int] = IO {
      val fs = FileSystem.get(new Configuration())
      val status = fs.getFileStatus(path)
      val length = status.getLen
      val blockSize = status.getBlockSize
      if (length < blockSize) 1
      else (length / blockSize).toInt
    }

    def getPayloads: Stream[IO, SourcePair] = Stream.eval(grabNumberOfBlocks)
      .flatMap(blocks => Stream.emits(0.until(blocks).map(n => SourcePair(this)(HDFSPayload(n)))))
  }
  case class MemorySource(list: List[String], maxParallel: Int) extends DatasetSource {
    type Payload = List[String]

    def payloadDecoder: Decoder[List[String]] = implicitly
    def payloadEncoder: Encoder[List[String]] = implicitly
    def hash: Hash[this.type] = Hash.fromUniversalHashCode

    def toStream(payload: List[String]): Stream[IO, String] =
      Stream.emits(payload)

    def getPayloads: Stream[IO, SourcePair] = {
      val grouped =
        if (maxParallel > 1) {
          val groups = list.length / maxParallel
          println(groups)
          if (groups > 0)
            list.grouped(groups).map(SourcePair(this)).toList
          else
            list.grouped(1).map(SourcePair(this)).toList
        }
        else SourcePair(this)(list) :: Nil
      Stream.emits(grouped)
    }
  }

  implicit def pathEncoder: Encoder[Path] = Encoder[String].contramap(_.toString)
  implicit def pathDecoder: Decoder[Path] = Decoder[String].map(s => new Path(s))

  implicit def encoder: Encoder[DatasetSource] = deriveEncoder
  implicit def decoder: Decoder[DatasetSource] = deriveDecoder

}

case class HDFSPayload(blockOffset: Int)
