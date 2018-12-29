package analytics

import analytics.classes.{CommutativeMonoidFn, MonoidFn}
import cats.data.Const
import cats.implicits._
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax._

trait DatasetFold[F[_]] {
  def fold[A: Type: MonoidFn]: F[A]
  def commutativeFold[A: Type: CommutativeMonoidFn]: F[A]
  def commutativeScan[A: Type: CommutativeMonoidFn]: F[A]
}

object DatasetFold {
  sealed trait RDatasetFold
  case class Fold(f: Json, init: Json) extends RDatasetFold
  case class CommutativeFold(f: Json, init: Json) extends RDatasetFold
  case class CommutativeScan(f: Json, init: Json) extends RDatasetFold

  object RDatasetFold {
    implicit def encoder: Encoder[RDatasetFold] = deriveEncoder

    implicit def decoder: Decoder[RDatasetFold] = deriveDecoder
  }

  def free: DatasetFold[Const[RDatasetFold, ?]] = new DatasetFold[Const[RDatasetFold, ?]] {
    def fold[A: Type: MonoidFn]: Const[RDatasetFold, A] =
      Const(Fold(MonoidFn[A].combine.asJson, Type.typeAEncoder[A].apply(MonoidFn[A].empty)))

    def commutativeFold[A: Type: CommutativeMonoidFn]: Const[RDatasetFold, A] =
      Const(CommutativeFold(MonoidFn[A].combine.asJson, Type.typeAEncoder[A].apply(MonoidFn[A].empty)))

    def commutativeScan[A: Type: CommutativeMonoidFn]: Const[RDatasetFold, A] =
      Const(CommutativeScan(MonoidFn[A].combine.asJson, Type.typeAEncoder[A].apply(MonoidFn[A].empty)))
  }

  def unfree[A](rd: RDatasetFold, tpe: Reified): DatasetFoldProgramErr[A] = new DatasetFoldProgramErr[A] {
    def apply[F[_]](F: DatasetFold[F]): Either[AnalyticsError, F[A]] = {

      implicit val anyType: Type[A] = Type.typeFromReified(tpe)

      rd match {
        case Fold(d, i) => Decoder[(A, A) Fn A].decodeJson(d).leftMap(DecodingErr).flatMap { f =>
          anyType.decoder.decodeJson(i).bimap(DecodingErr, { init =>
            F.fold[A](anyType, new MonoidFn[A] {
              def combine: Fn[(A, A), A] = f
              def empty: A = init
            })
          })
        }
        case CommutativeFold(d, i) => Decoder[(A, A) Fn A].decodeJson(d).leftMap(DecodingErr).flatMap { f =>
          anyType.decoder.decodeJson(i).bimap(DecodingErr, { init =>
            F.fold[A](anyType, new CommutativeMonoidFn[A] {
              def combine: Fn[(A, A), A] = f
              def empty: A = init
            })
          })
        }
        case CommutativeScan(d, i) => Decoder[(A, A) Fn A].decodeJson(d).leftMap(DecodingErr).flatMap { f =>
          anyType.decoder.decodeJson(i).bimap(DecodingErr, { init =>
            F.commutativeScan[A](anyType, new CommutativeMonoidFn[A] {
              def combine: Fn[(A, A), A] = f
              def empty: A = init
            })
          })
        }
      }

    }
  }
}
