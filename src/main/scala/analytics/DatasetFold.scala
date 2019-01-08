package analytics

import analytics.classes.{CommutativeMonoidFn, MonoidFn}
import cats.data.Const
import cats.implicits._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._

trait DatasetFold[F[_]] {
  def fold[A: Type: MonoidFn]: F[A]
  def scan[A: Type: MonoidFn]: F[A]
  def commutativeFold[A: Type: CommutativeMonoidFn]: F[A]
  def commutativeScan[A: Type: CommutativeMonoidFn]: F[A]
}

object DatasetFold {
  sealed trait RDatasetFold
  case class Fold(f: Json, init: Json) extends RDatasetFold
  case class Scan(f: Json, init: Json) extends RDatasetFold
  case class CommutativeFold(f: Json, init: Json) extends RDatasetFold
  case class CommutativeScan(f: Json, init: Json) extends RDatasetFold

  object RDatasetFold {
    implicit def encoder: Encoder[RDatasetFold] = deriveEncoder

    implicit def decoder: Decoder[RDatasetFold] = deriveDecoder
  }

  def free: DatasetFold[Const[RDatasetFold, ?]] = new DatasetFold[Const[RDatasetFold, ?]] {

    def create[A: Type: MonoidFn, F](f: (Json, Json) => F): Const[F, A] =
      Const(f(MonoidFn[A].combine.asJson, Type.typeAEncoder[A].apply(MonoidFn[A].empty)))

    def fold[A: Type: MonoidFn]: Const[RDatasetFold, A] =
      create(Fold)

    def scan[A: Type: MonoidFn]: Const[RDatasetFold, A] =
      create(Scan)

    def commutativeFold[A: Type: CommutativeMonoidFn]: Const[RDatasetFold, A] =
      create(CommutativeFold)

    def commutativeScan[A: Type: CommutativeMonoidFn]: Const[RDatasetFold, A] =
      create(CommutativeScan)
  }

  def unfree[A](rd: RDatasetFold, tpe: Schema): DatasetFoldProgramErr[A] = new DatasetFoldProgramErr[A] {
    def apply[F[_]](F: DatasetFold[F]): Either[AnalyticsError, F[A]] = {

      implicit val anyType: Type[A] = Type.typeFromReified(tpe)

      def decodeWith(combine: Json, init: Json, constructor: (Fn[(A, A), A], A) => F[A]): Either[AnalyticsError, F[A]] =
        Decoder[(A, A) Fn A].decodeJson(combine).leftMap(DecodingErr).flatMap(f =>
          anyType.decoder.decodeJson(init)
            .bimap(DecodingErr, (i => constructor(f, i))))


      rd match {
        case Fold(c, i) =>
          decodeWith(c, i, ((fn, init) => F.fold[A](anyType, MonoidFn.create(fn, init))))
        case CommutativeFold(c, i) =>
          decodeWith(c, i, ((fn, init) => F.commutativeFold[A](anyType, CommutativeMonoidFn.create(fn, init))))
        case CommutativeScan(c, i) =>
          decodeWith(c, i, ((fn, init) => F.commutativeScan[A](anyType, CommutativeMonoidFn.create(fn, init))))
        case Scan(c, i) =>
          decodeWith(c, i, ((fn, init) => F.scan[A](anyType, CommutativeMonoidFn.create(fn, init))))
      }

    }
  }
}
