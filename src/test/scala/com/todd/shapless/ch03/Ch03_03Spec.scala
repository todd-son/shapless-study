package com.todd.shapless.ch03

import org.scalatest.WordSpec
import shapeless._

class Ch03_03Spec extends WordSpec {
  trait CsvEncoder[A] {
    def encode(a: A): List[String]
  }

  object CsvEncoder {
    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

    def instance[A](func: A => List[String]): CsvEncoder[A] =
      (a: A) => func(a)
  }

  import CsvEncoder._

  implicit val stringEncoder: CsvEncoder[String] = instance((s: String) => List(s))

  implicit val intEncoder: CsvEncoder[Int] = instance((i: Int) => List(i.toString))

  implicit val booleanEncoder: CsvEncoder[Boolean] = instance((b: Boolean) => List(b.toString))

  implicit val hnilEncoder: CsvEncoder[HNil] = instance((_: HNil) => Nil)

  implicit def hlistEncoder[H, T <: HList](implicit hEncoder: CsvEncoder[H], tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] =
    instance {
      case h :: t => hEncoder.encode(h) ++ tEncoder.encode(t)
    }

  implicit val cnilEncoder: CsvEncoder[CNil] = instance((_: CNil) => throw new Exception("Inconceivable!"))

  implicit def coproductEncoder[H, T <: Coproduct](implicit hEncoder: CsvEncoder[H],
                                                   tEncoder: CsvEncoder[T]): CsvEncoder[H :+: T]
  = instance {
    case Inl(h) => hEncoder.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

  implicit def genericEncoder[A, R](implicit gen: Generic.Aux[A, R], enc: CsvEncoder[R]): CsvEncoder[A]
  = instance((a: A) => enc.encode(gen.to(a)))

  sealed trait Shape

  final case class Rectangle(width: Int, height: Int) extends Shape

  final case class Circle(radius: Int) extends Shape

  def writeCsv[A](values: List[A])(implicit encoder: CsvEncoder[A]) =
    values.map(a => encoder.encode(a).mkString(",")).mkString("\n")

  "coproductEncoder" should {
    "encode shape" in {
      val shapes: List[Shape] = List(
        Rectangle(10, 10),
        Circle(1)
      )

      println(writeCsv(shapes))
    }
  }

}
