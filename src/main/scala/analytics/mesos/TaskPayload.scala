package analytics.mesos

import analytics.DatasetFold.RDatasetFold
import analytics.DatasetOp.RDataSetOp
import analytics.{Reified, SourcePair}
import io.circe.{Decoder, Encoder}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}

import scala.collection.immutable.Map

case class TaskPayload(data: Map[Int, SourcePair], ops: RDataSetOp, fold: RDatasetFold, tpeA: Reified, tpeB: Reified)
object TaskPayload {
  implicit def encoder: Encoder[TaskPayload] = deriveEncoder
  implicit def decoder: Decoder[TaskPayload] = deriveDecoder
}
