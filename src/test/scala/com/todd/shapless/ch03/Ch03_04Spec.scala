package com.todd.shapless.ch03

import org.scalatest.WordSpec
import shapeless.{:+:, ::, CNil, Coproduct, Generic, HList, HNil, Inl, Inr, Lazy}

class Ch03_04Spec extends WordSpec {
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

  implicit def hlistEncoder[H, T <: HList](implicit hEncoder: Lazy[CsvEncoder[H]], tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] =
    instance {
      case h :: t => hEncoder.value.encode(h) ++ tEncoder.encode(t)
    }

  implicit val cnilEncoder: CsvEncoder[CNil] = instance((_: CNil) => throw new Exception("Inconceivable!"))

  implicit def coproductEncoder[H, T <: Coproduct](implicit hEncoder: Lazy[CsvEncoder[H]],
                                                   tEncoder: CsvEncoder[T]): CsvEncoder[H :+: T]
  = instance {
    case Inl(h) => hEncoder.value.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

  implicit def genericEncoder[A, R](implicit gen: Generic.Aux[A, R], enc: Lazy[CsvEncoder[R]]): CsvEncoder[A]
  = instance((a: A) => enc.value.encode(gen.to(a)))

  def writeCsv[A](values: List[A])(implicit encoder: CsvEncoder[A]) =
    values.map(a => encoder.encode(a).mkString(",")).mkString("\n")


  sealed trait Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  case class Leaf[A](value: A) extends Tree[A]


  "Tree" should {
    "encode successfully" in {
      val result = writeCsv(List(Leaf[String]("A")))

      assert(result == "A")

      val result2 = writeCsv(
        List(
          Branch[String](
            Leaf("A"),
            Leaf("B")
          )
        )
      )

      assert(result2 == "B")
    }
  }

}
