package analytics

import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder}

sealed trait Schema

object Schema {
  case object Int extends Schema
  case object Long extends Schema
  case object BigInt extends Schema
  case object Rational extends Schema
  case object Double extends Schema
  case object Float extends Schema
  case object Unit extends Schema
  case object Null extends Schema
  case object String extends Schema
  case object Boolean extends Schema
  case object Any extends Schema
  case class Tuple2(a: Schema, b: Schema) extends Schema
  case class Either(a: Schema, b: Schema) extends Schema
  case class List(a: Schema) extends Schema

  implicit def encoder: Encoder[Schema] = deriveEncoder
  implicit def decoder: Decoder[Schema] = deriveDecoder
}
