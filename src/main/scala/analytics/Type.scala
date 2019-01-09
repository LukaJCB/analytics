package analytics

import analytics.classes.InvariantSemiringal
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.generic.semiauto._
import io.circe.spire._
import spire.math.{Algebraic, Natural, Polynomial, Rational}

trait Type[A] {
  def reify: Schema
  def encoder: Encoder[A]
  def decoder: Decoder[A]
}


object Type {


  implicit def y: Encoder[Polynomial[BigInt]] = ???
  implicit def x: Encoder[Algebraic.Expr] = deriveEncoder

  def newType[A: Encoder: Decoder](r: Schema): Type[A] = new Type[A] {
    def reify: Schema = r
    def encoder: Encoder[A] = Encoder[A]
    def decoder: Decoder[A] = Decoder[A]
  }


  def imap[A, B](ta: Type[A])(f: A => B)(g: B => A): Type[B] = new Type[B] {
    def reify: Schema = ta.reify
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

  implicit def unitType = newType[Unit](Schema.Unit)
  implicit def nothingType = new Type[Nothing] {
    def reify: Schema = Schema.Null
    def encoder: Encoder[Nothing] = Encoder.instance[Nothing]((_: Nothing) => Json.Null)
    def decoder: Decoder[Nothing] =
      Decoder.instance(_ => Left(DecodingFailure("Cannot decode Nothing", List.empty)))
  }
  implicit def intType = newType[Int](Schema.Int)
  implicit def longType = newType[Long](Schema.Long)
  implicit def floatType = newType[Float](Schema.Float)
  implicit def doubleType = newType[Double](Schema.Double)
  implicit def naturalType = newType[Natural](Schema.Natural)
  implicit def bigIntType = newType[BigInt](Schema.BigInt)
  implicit def rationalType = newType[Rational](Schema.Rational)
  implicit def booleanType = newType[Boolean](Schema.Boolean)
  implicit def stringType = newType[String](Schema.String)

  implicit def tupleType[A: Type, B: Type]: Type[(A, B)] = new Type[(A, B)] {
    def reify: Schema = Schema.Tuple2(Type[A].reify, Type[B].reify)
    def encoder: Encoder[(A, B)] = Encoder.encodeTuple2(Type[A].encoder, Type[B].encoder)
    def decoder: Decoder[(A, B)] = Decoder.decodeTuple2(Type[A].decoder, Type[B].decoder)
  }

  implicit def eitherType[A: Type, B: Type]: Type[Either[A, B]] = new Type[Either[A, B]] {
    def reify: Schema = Schema.Either(Type[A].reify, Type[B].reify)
    def encoder: Encoder[Either[A, B]] = Encoder.encodeEither("Left", "Right")(Type[A].encoder, Type[B].encoder)
    def decoder: Decoder[Either[A, B]] = Decoder.decodeEither("Left", "Right")(Type[A].decoder, Type[B].decoder)
  }

  implicit def listType[A: Type]: Type[List[A]] = new Type[List[A]] {
    def reify: Schema = Schema.List(Type[A].reify)
    def encoder: Encoder[List[A]] = implicitly
    def decoder: Decoder[List[A]] = implicitly
  }

  implicit def typeEncoder[A]: Encoder[Type[A]] = Encoder[Schema].contramap(_.reify)
  implicit def typeDecoder[A: Encoder: Decoder]: Decoder[Type[A]] = Decoder[Schema].map(r =>
    new Type[A] {
      def reify: Schema = r
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
      def reify: Schema = Type[A].reify
      def encoder: Encoder[B] = Type[A].encoder.contramap(g)
      def decoder: Decoder[B] = Type[A].decoder.map(f)
    }
  }

  def typeFromSchema[A](tpe: Schema): Type[A] = (tpe match {
    case Schema.Int => intType
    case Schema.Long => longType
    case Schema.Float => floatType
    case Schema.Double => doubleType
    case Schema.Natural => naturalType
    case Schema.BigInt => bigIntType
    case Schema.Rational => rationalType
    case Schema.String => stringType
    case Schema.Boolean => booleanType
    case Schema.Unit => unitType
    case Schema.Tuple2(a, b) => tupleType(typeFromSchema(a), typeFromSchema(b))
    case Schema.Either(a, b) => eitherType(typeFromSchema(a), typeFromSchema(b))
    case Schema.List(a) => listType(typeFromSchema(a))
    case Schema.Any => imap(stringType)(s => (s: Any))(_.toString)
    case Schema.Null => nothingType
  }).asInstanceOf[Type[A]]

}
