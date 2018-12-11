package analytics

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

sealed trait Reified

object Reified {
  case object Int extends Reified
  case object Unit extends Reified
  case object Null extends Reified
  case object String extends Reified
  case object Boolean extends Reified
  case object Any extends Reified
  case class Tuple2(a: Reified, b: Reified) extends Reified
  case class Either(a: Reified, b: Reified) extends Reified
  case class List(a: Reified) extends Reified

  implicit def encoder: Encoder[Reified] = deriveEncoder
  implicit def decoder: Decoder[Reified] = deriveDecoder
}
