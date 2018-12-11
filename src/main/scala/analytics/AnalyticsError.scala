package analytics

import io.circe.{DecodingFailure, Error}

sealed trait AnalyticsError
case class DecodingErr(d: DecodingFailure) extends AnalyticsError
case class CirceErr(c: Error) extends AnalyticsError
