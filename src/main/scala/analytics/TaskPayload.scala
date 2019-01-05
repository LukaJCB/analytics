package analytics

import analytics.DatasetFold.RDatasetFold
import analytics.DatasetOp.RDataSetOp
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

import scala.collection.immutable.Map

case class TaskPayload(data: Map[Int, SourcePair], id: BigInt, ops: RDataSetOp, fold: RDatasetFold, tpeA: Reified, tpeB: Reified)
object TaskPayload {
  implicit def encoder: Encoder[TaskPayload] = deriveEncoder
  implicit def decoder: Decoder[TaskPayload] = deriveDecoder
}
