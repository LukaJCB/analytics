package analytics

import io.circe.Decoder.Result
import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.syntax._

abstract class SourcePair(val src: DatasetSource) {
  def payload: src.Payload

  def toStream = src.toStream(payload)
}

object SourcePair {
  def apply(a: DatasetSource)(pl: a.Payload): SourcePair =
    new SourcePair(a) {
      def payload: src.Payload = pl.asInstanceOf[src.Payload]
      override def toString: String = s"$a, $pl"
    }

  implicit def decoder: Decoder[SourcePair] = new Decoder[SourcePair] {
    def apply(c: HCursor): Result[SourcePair] = for {
      src <- c.downField("src").as[DatasetSource]
      pl <- c.downField("payload").as[src.Payload](src.payloadDecoder)
    } yield SourcePair(src)(pl)
  }
  implicit def encoder: Encoder[SourcePair] = new Encoder[SourcePair] {
    def apply(a: SourcePair): Json = Json.obj(
      "src" -> a.src.asJson,
      "payload" -> a.src.payloadEncoder(a.payload)
    )
  }
}
