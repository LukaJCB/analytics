package analytics

import analytics.classes.InvariantSemiringal
import io.circe.{Decoder, DecodingFailure, Encoder, Json}

trait Type[A] {
  def reify: Reified
  def encoder: Encoder[A]
  def decoder: Decoder[A]
}


object Type {

  def newType[A: Encoder: Decoder](r: Reified): Type[A] = new Type[A] {
    def reify: Reified = r
    def encoder: Encoder[A] = Encoder[A]
    def decoder: Decoder[A] = Decoder[A]
  }


  def imap[A, B](ta: Type[A])(f: A => B)(g: B => A): Type[B] = new Type[B] {
    def reify: Reified = ta.reify
    def encoder: Encoder[B] = ta.encoder.contramap(g)
    def decoder: Decoder[B] = ta.decoder.map(f)
  }

  implicit def invariantSemiringalType: InvariantSemiringal[Type] = new InvariantSemiringal[Type] {
    def imap[A, B](fa: Type[A])(f: A => B)(g: B => A): Type[B] = Type.imap(fa)(f)(g)
    def product[A, B](fa: Type[A], fb: Type[B]): Type[(A, B)] = tupleType(fa, fb)
    def sum[A, B](fa: Type[A], fb: Type[B]): Type[Either[A, B]] = eitherType(fa, fb)
    def zero: Type[Nothing] = nothingType
    def one: Type[Unit] = unitType
  }

  def apply[A: Type] = implicitly[Type[A]]

  implicit def unitType = newType[Unit](Reified.Unit)
  implicit def nothingType = new Type[Nothing] {
    def reify: Reified = Reified.Null
    def encoder: Encoder[Nothing] = Encoder.instance[Nothing]((_: Nothing) => Json.Null)
    def decoder: Decoder[Nothing] =
      Decoder.instance(_ => Left(DecodingFailure("Cannot decode Nothing", List.empty)))
  }
  implicit def intType = newType[Int](Reified.Int)
  implicit def booleanType = newType[Boolean](Reified.Boolean)
  implicit def stringType = newType[String](Reified.String)

  implicit def tupleType[A: Type, B: Type]: Type[(A, B)] = new Type[(A, B)] {
    def reify: Reified = Reified.Tuple2(Type[A].reify, Type[B].reify)
    def encoder: Encoder[(A, B)] = Encoder.encodeTuple2(Type[A].encoder, Type[B].encoder)
    def decoder: Decoder[(A, B)] = Decoder.decodeTuple2(Type[A].decoder, Type[B].decoder)
  }

  implicit def eitherType[A: Type, B: Type]: Type[Either[A, B]] = new Type[Either[A, B]] {
    def reify: Reified = Reified.Either(Type[A].reify, Type[B].reify)
    def encoder: Encoder[Either[A, B]] = Encoder.encodeEither("Left", "Right")(Type[A].encoder, Type[B].encoder)
    def decoder: Decoder[Either[A, B]] = Decoder.decodeEither("Left", "Right")(Type[A].decoder, Type[B].decoder)
  }

  implicit def listType[A: Type]: Type[List[A]] = new Type[List[A]] {
    def reify: Reified = Reified.List(Type[A].reify)
    def encoder: Encoder[List[A]] = implicitly
    def decoder: Decoder[List[A]] = implicitly
  }

  implicit def typeEncoder[A]: Encoder[Type[A]] = Encoder[Reified].contramap(_.reify)
  implicit def typeDecoder[A: Encoder: Decoder]: Decoder[Type[A]] = Decoder[Reified].map(r =>
    new Type[A] {
      def reify: Reified = r
      def encoder: Encoder[A] = Encoder[A]
      def decoder: Decoder[A] = Decoder[A]
    })

  implicit def typeADecoder[A: Type]: Decoder[A] =
    Type[A].decoder

  implicit def typeAEncoder[A: Type]: Encoder[A] =
    Type[A].encoder

  def deriveInstanceFrom[A: Type] = new DeriveInfereceHelper[A]

  class DeriveInfereceHelper[A: Type] {
    def to[B](f: A => B)(g: B => A): Type[B] = new Type[B] {
      def reify: Reified = Type[A].reify
      def encoder: Encoder[B] = Type[A].encoder.contramap(g)
      def decoder: Decoder[B] = Type[A].decoder.map(f)
    }
  }

  def typeFromReified[A](tpe: Reified): Type[A] = (tpe match {
    case Reified.Int => intType
    case Reified.String => stringType
    case Reified.Boolean => booleanType
    case Reified.Unit => unitType
    case Reified.Tuple2(a, b) => tupleType(typeFromReified(a), typeFromReified(b))
    case Reified.Either(a, b) => eitherType(typeFromReified(a), typeFromReified(b))
    case Reified.List(a) => listType(typeFromReified(a))
    case Reified.Any => imap(stringType)(s => (s: Any))(_.toString)
    case Reified.Null => nothingType
  }).asInstanceOf[Type[A]]

}
