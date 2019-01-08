package analytics

import cats.implicits._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._
import analytics.util.Biconst
import scala.collection.immutable.SortedMap


trait DatasetOp[F[_, _]] {
  def source(s: DatasetSource): F[Unit, String]
  def fmap[A: Type, B: Type](f: A Fn B): F[A, B]
  def filter[A: Type](f: A Fn Boolean): F[A, A]
  def literal[A: Type](a: => A): F[Unit, A]
  def concatMap[A: Type, B: Type](f: A Fn List[B]): F[A, B]

  def compose[A: Type, B: Type, C: Type](x: F[A, B], y: F[B, C]): F[A, C]

  def merge[A: Type, B: Type, C: Type](x: F[A, C], y: F[B, C]): F[(A, B), C]

  def split[A: Type, B: Type, C: Type, D: Type](x: F[A, B], y: F[C, D]): F[(A, C), (B, D)]

  def combineWith[A: Type, B: Type, C: Type](fa: F[Unit, A], fb: F[Unit, B])(f: Fn[(A, B), C]): F[Unit, C] =
    compose(literal(((), ())), compose(split(fa, fb), fmap(f)))

}

object DatasetOp {
  sealed trait RDataSetOp
  case class Source(s: DatasetSource) extends RDataSetOp
  case class Fmap(f: Json) extends RDataSetOp
  case class Filter(f: Json) extends RDataSetOp
  case class ConcatMap(f: Json) extends RDataSetOp
  case class Literal(a: Json) extends RDataSetOp
  case class Merge(x: RDataSetOp, y: RDataSetOp, tpeA: Schema, tpeB: Schema, tpeC: Schema) extends RDataSetOp
  case class Compose(x: RDataSetOp, y: RDataSetOp, tpeA: Schema, tpeB: Schema, tpeC: Schema) extends RDataSetOp
  case class Split(x: RDataSetOp, y: RDataSetOp, tpeA: Schema, tpeB: Schema, tpeC: Schema, tpeD: Schema) extends RDataSetOp

  object RDataSetOp {
    implicit def encoder: Encoder[RDataSetOp] = deriveEncoder

    implicit def decoder: Decoder[RDataSetOp] = deriveDecoder
  }

  def allSources: DatasetOp[Biconst[SortedMap[Int, DatasetSource], ?, ?]] = new DatasetOp[Biconst[SortedMap[Int, DatasetSource], ?, ?]] {
    def source(s: DatasetSource): Biconst[SortedMap[Int, DatasetSource], Unit, String] = Biconst(SortedMap(s.hash.hash(s) -> s))

    def fmap[A: Type, B: Type](f: Fn[A, B]): Biconst[SortedMap[Int, DatasetSource], A, B] = Biconst(SortedMap.empty)

    def filter[A: Type](f: Fn[A, Boolean]): Biconst[SortedMap[Int, DatasetSource], A, A] = Biconst(SortedMap.empty)

    def literal[A: Type](a: => A): Biconst[SortedMap[Int, DatasetSource], Unit, A] = Biconst(SortedMap.empty)

    def concatMap[A: Type, B: Type](f: Fn[A, List[B]]): Biconst[SortedMap[Int, DatasetSource], A, B] = Biconst(SortedMap.empty)

    def compose[A: Type, B: Type, C: Type](x: Biconst[SortedMap[Int, DatasetSource], A, B], y: Biconst[SortedMap[Int, DatasetSource], B, C]): Biconst[SortedMap[Int, DatasetSource], A, C] =
      Biconst(x.getConst ++ y.getConst)

    def merge[A: Type, B: Type, C: Type](x: Biconst[SortedMap[Int, DatasetSource], A, C], y: Biconst[SortedMap[Int, DatasetSource], B, C]): Biconst[SortedMap[Int, DatasetSource], (A, B), C] =
      Biconst(x.getConst ++ y.getConst)

    def split[A: Type, B: Type, C: Type, D: Type](x: Biconst[SortedMap[Int, DatasetSource], A, B], y: Biconst[SortedMap[Int, DatasetSource], C, D]): Biconst[SortedMap[Int, DatasetSource], (A, C), (B, D)] =
      Biconst(x.getConst ++ y.getConst)
  }

  def free: DatasetOp[Biconst[RDataSetOp, ?, ?]] =
    new DatasetOp[Biconst[RDataSetOp, ?, ?]] {
      def source(s: DatasetSource): Biconst[RDataSetOp, Unit, String] = Biconst(Source(s))

      def fmap[A: Type, B: Type](f: Fn[A, B]): Biconst[RDataSetOp, A, B] = Biconst(Fmap(f.asJson))

      def literal[A: Type](a: => A): Biconst[RDataSetOp, Unit, A] = Biconst(Literal(Type[A].encoder(a)))

      def filter[A: Type](f: Fn[A, Boolean]): Biconst[RDataSetOp, A, A] = Biconst(Filter(f.asJson))

      def merge[A: Type, B: Type, C: Type](x: Biconst[RDataSetOp, A, C], y: Biconst[RDataSetOp, B, C]): Biconst[RDataSetOp, (A, B), C] =
        Biconst(Merge(x.getConst, y.getConst, Type[A].reify, Type[B].reify, Type[C].reify))

      def compose[A: Type, B: Type, C: Type](x: Biconst[RDataSetOp, A, B], y: Biconst[RDataSetOp, B, C]): Biconst[RDataSetOp, A, C] =
        Biconst(Compose(x.getConst, y.getConst, Type[A].reify, Type[B].reify, Type[C].reify))

      def split[A: Type, B: Type, C: Type, D: Type](x: Biconst[RDataSetOp, A, B], y: Biconst[RDataSetOp, C, D]): Biconst[RDataSetOp, (A, C), (B, D)] =
        Biconst(Split(x.getConst, y.getConst, Type[A].reify, Type[B].reify, Type[C].reify, Type[D].reify))

      def concatMap[A: Type, B: Type](f: Fn[A, List[B]]): Biconst[RDataSetOp, A, B] =
        Biconst(ConcatMap(f.asJson))
    }

  def unfree[A, B](rd: RDataSetOp, tpeA: Schema, tpeB: Schema): DatasetOpProgramErr[A, B] = new DatasetOpProgramErr[A, B] {

    implicit val typeA: Type[A] = Type.typeFromReified(tpeA)
    implicit val typeB: Type[B] = Type.typeFromReified(tpeB)

    def result[F[_, _]](F: DatasetOp[F]): Either[AnalyticsError, F[A, B]] =
      rd match {
        case Source(s) => Right(F.source(s).asInstanceOf[F[A, B]])
        case Fmap(f) => Decoder[A Fn B].decodeJson(f).bimap(DecodingErr, F.fmap[A, B])
        case Literal(b) => typeB.decoder.decodeJson(b).bimap(DecodingErr, b => F.literal(b).asInstanceOf[F[A, B]])
        case Filter(f) => Decoder[A Fn Boolean].decodeJson(f).bimap(DecodingErr, a => F.filter[A](a).asInstanceOf[F[A, B]])
        case ConcatMap(f) => Decoder[A Fn List[B]].decodeJson(f).bimap(DecodingErr, F.concatMap[A, B])
        case Merge(x, y, a, b, c) => unfree[Any, B](x, a, c)(F).flatMap(fx =>
          unfree[Any, B](y, b, c)(F).map(fy =>
            F.merge(fx, fy)(Type.typeFromReified(a), Type.typeFromReified(b), typeB).asInstanceOf[F[A, B]]))
        case Compose(x, y, a, b, c) => unfree[A, Any](x, a, b)(F).flatMap(fx =>
          unfree[Any, B](y, b, c)(F).map(fy =>
            F.compose(fx, fy)(Type.typeFromReified(a), Type.typeFromReified(b), Type.typeFromReified(c)).asInstanceOf[F[A, B]]))
        case Split(x, y, a, b, c, d) => unfree[Any, Any](x, a, b)(F).flatMap(fx =>
          unfree[Any, Any](y, c, d)(F).map(fy =>
            F.split(fx, fy)(Type.typeFromReified(a),
              Type.typeFromReified(b),
              Type.typeFromReified(c),
              Type.typeFromReified(d)).asInstanceOf[F[A, B]]))
      }

    def apply[F[_, _]](F: DatasetOp[F]): Either[AnalyticsError, F[A, B]] =
      result(F).asInstanceOf[Either[AnalyticsError, F[A, B]]]
  }

}
