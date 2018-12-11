package analytics

import cats.arrow.Category
import io.circe.Decoder.Result
import io.circe._
import io.circe.syntax._

sealed trait Fn[A, B]

object Fn {

  case object Plus extends Fn[(Int, Int), Int]
  case object ToString extends Fn[Int, String]
  case object ToInt extends Fn[String, Int]
  case object LargerThan extends Fn[(Int, Int), Boolean]
  case class Swap[A, B]() extends Fn[(A, B), (B, A)]
  case class Fst[A, B]() extends Fn[(A, B), A]
  case class Snd[A, B]() extends Fn[(A, B), B]
  case object Concat extends Fn[(String, String), String]
  case object Split extends Fn[(String, String), List[String]]

  case class Uncurry[A, B, C](f: Fn[A, Fn[B, C]]) extends Fn[(A, B), C]
  case class ComposeFn[A, B, C](x: Fn[A, B], y: Fn[B, C]) extends Fn[A, C]
  case class Identity[A]() extends Fn[A, A]
  case class First[A, B, C](fa: Fn[A, B]) extends Fn[(A, C), (B, C)]
  case class Second[A, B, C](fa: Fn[A, B]) extends Fn[(C, A), (C, B)]
  case class Merge[A, B, C](f: Fn[A, B], g: Fn[A, C]) extends Fn[A, (B, C)]
  case class Choice[A, B, C](f: Fn[A, C], g: Fn[B, C]) extends Fn[Either[A, B], C]
  case class Literal[A, B](b: B, typeB: Type[B]) extends Fn[A, B]

  def interpret[A, B](fn: Fn[A, B]): A => B = fn match {
    case Plus => (t: (Int, Int)) => t._1 + t._2
    case ToString => _.toString
    case LargerThan => (t: (Int, Int)) => t._1 > t._2
    case ToInt => _.toInt
    case Concat => (t: (String, String)) => t._1 + t._2
    case Split => (t: (String, String)) => t._1.split(t._2).toList
    case ComposeFn(x, y) => interpret(x) andThen interpret(y)
    case Identity() => a => a.asInstanceOf[B]
    case Uncurry(f) => t => interpret(interpret(f)(t._1))(t._2)
    case Fst() => _._1
    case Snd() => _._2
    case Swap() => _.swap
    case First(fab) => t => (interpret(fab)(t._1), t._2)
    case Second(fab) => t => (t._1, interpret(fab)(t._2))
    case Merge(f, g) => a => (interpret(f)(a), interpret(g)(a))
    case Literal(b, _) => _ => b
    case Choice(f, g) => {
      case Left(a) => interpret(f)(a)
      case Right(b) => interpret(g)(b)
    }
  }

  implicit def categoryFn = new Category[Fn] {
    def id[A]: Fn[A, A] = Identity[A]()

    def compose[A, B, C](f: Fn[B, C], g: Fn[A, B]): Fn[A, C] =
      ComposeFn(g, f)
  }


  implicit def encoder[A, B]: Encoder[Fn[A, B]] = new Encoder[Fn[A, B]] {
    def apply(a: Fn[A, B]): Json = a match {
      case ToString => Json.obj(
        "type" -> Json.fromString("ToString"))
      case Plus => Json.obj(
        "type" -> Json.fromString("Plus"))
      case LargerThan => Json.obj(
        "type" -> Json.fromString("LargerThan"))
      case ToInt => Json.obj(
        "type" -> Json.fromString("ToInt"))
      case Concat => Json.obj(
        "type" -> Json.fromString("Concat"))
      case Split => Json.obj(
        "type" -> Json.fromString("Split"))
      case ComposeFn(x, y) => Json.obj(
        "type" -> Json.fromString("Compose"),
        "x" -> x.asJson,
        "y" -> y.asJson)
      case Identity() => Json.obj(
        "type" -> Json.fromString("Identity"))
      case Fst() => Json.obj(
        "type" -> Json.fromString("Fst"))
      case Snd() => Json.obj(
        "type" -> Json.fromString("Snd"))
      case First(fab) => Json.obj(
        "type" -> Json.fromString("First"),
        "f" -> fab.asJson)
      case Second(fab) => Json.obj(
        "type" -> Json.fromString("Second"),
        "f" -> fab.asJson)
      case Swap() => Json.obj(
        "type" -> Json.fromString("Swap"))
      case Merge(f, g) => Json.obj(
        "type" -> Json.fromString("Merge"),
        "f" -> f.asJson,
        "g" -> g.asJson)
      case Choice(f, g) => Json.obj(
        "type" -> Json.fromString("Choice"),
        "f" -> f.asJson,
        "g" -> g.asJson)
      case Literal(b, tb) => Json.obj(
        "type" -> Json.fromString("Literal"),
        "b" -> Type.typeAEncoder(tb).apply(b))
      case Uncurry(f) => Json.obj(
        "type" -> Json.fromString("Uncurry"),
        "f" -> f.asJson
      )
    }
  }


  implicit def decoder[A, B: Type]: Decoder[Fn[A, B]] = new Decoder[Fn[A, B]] {
    def apply(c: HCursor): Result[Fn[A, B]] = c.downField("type").as[String].flatMap {
      case "ToString" => Right(ToString).asInstanceOf[Result[Fn[A, B]]]
      case "Plus" => Right(Plus).asInstanceOf[Result[Fn[A, B]]]
      case "ToInt" => Right(ToInt).asInstanceOf[Result[Fn[A, B]]]
      case "LargerThan" => Right(LargerThan).asInstanceOf[Result[Fn[A, B]]]
      case "Concat" => Right(Concat).asInstanceOf[Result[Fn[A, B]]]
      case "Literal" => Type[B].decoder.tryDecode(c.downField("b")).map(b => Literal(b, Type[B]))
      case "Identity" => Right(Identity()).asInstanceOf[Result[Fn[A, B]]]
      case "Split" => Right(Split).asInstanceOf[Result[Fn[A, B]]]
      case "Compose" => tryDecode(c.downField("x")).flatMap(x => tryDecode(c.downField("y"))
        .map(y => ComposeFn(x.asInstanceOf[Fn[A, Any]], y.asInstanceOf[Fn[Any, B]])))
      case t => Left(DecodingFailure(s"Analytics: No recognized type: $t", List.empty))
    }
  }
}
