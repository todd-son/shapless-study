package com.todd.shapless.ch03

import org.scalatest.WordSpec
import shapeless._

class Ch03_02Spec extends WordSpec {

  trait CsvEncoder[A] {
    def encode(a: A): List[String]
  }

  object CsvEncoder {
    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

    def instance[A](func: A => List[String]): CsvEncoder[A] =
      (a: A) => func(a)
  }

  def createEncoder[A](func: A => List[String]): CsvEncoder[A] = (a: A) => func(a)

  implicit val stringEncoder: CsvEncoder[String] = createEncoder((s: String) => List(s))

  implicit val intEncoder: CsvEncoder[Int] = createEncoder((i: Int) => List(i.toString))

  implicit val booleanEncoder: CsvEncoder[Boolean] = createEncoder((b: Boolean) => List(b.toString))

  implicit val hnilEncoder: CsvEncoder[HNil] = createEncoder((_: HNil) => Nil)

  implicit def hlistEncoder[H, T <: HList](implicit hEncoder: CsvEncoder[H], tEncoder: CsvEncoder[T]): CsvEncoder[H :: T] =
    createEncoder {
      case h :: t => hEncoder.encode(h) ++ tEncoder.encode(t)
    }

  case class IceCream(name: String, price: Int, incone: Boolean)

  def writeCsv[A](values: List[A])(implicit encoder: CsvEncoder[A]) =
    values.map(a => encoder.encode(a).mkString(",")).mkString("\n")

  "ReprEncoder" should {
    "encode hlist" in {
      val reprEncoder: CsvEncoder[String :: Int :: Boolean :: HNil] = implicitly

      val result = reprEncoder.encode("abc" :: 123 :: true :: HNil)
      assert(result == List("abc", "123", "true"))
    }

    "encode icecream case class" in {
      implicit val iceCreamEncoder: CsvEncoder[IceCream] = {
        val gen = Generic[IceCream]
        val enc = CsvEncoder[gen.Repr]
        createEncoder((iceCream: IceCream) => enc.encode(gen.to(iceCream)))
      }

      val result = iceCreamEncoder.encode(IceCream("pigbar", 100, false))
      assert(result == List("pigbar", "100", "false"))
    }

    "encode any case class" in {
      implicit val gen = Generic[IceCream]

      implicit def genericEncoder[A, R](implicit gen: Generic[A] { type Repr = R }, enc: CsvEncoder[R]): CsvEncoder[A]
      = createEncoder((a: A) => enc.encode(gen.to(a)))

      val result = genericEncoder.encode(IceCream("pigbar", 200, false))
      assert(result == List("pigbar", "200", "false"))
    }

    "encode any case class2" in {

      implicit val gen = Generic[IceCream]

      implicit def genericEncoder[A, R](implicit gen: Generic.Aux[A, R], enc: CsvEncoder[R]): CsvEncoder[A]
      = createEncoder((a: A) => enc.encode(gen.to(a)))

      val result = genericEncoder.encode(IceCream("pigbar", 200, false))
      assert(result == List("pigbar", "200", "false"))
    }

    "write csv" in {
      implicit def genericEncoder[A, R](implicit gen: Generic.Aux[A, R], enc: CsvEncoder[R]): CsvEncoder[A]
      = createEncoder((a: A) => enc.encode(gen.to(a)))

      val result = writeCsv(List(IceCream("merona", 300, false)))

      assert(result == "merona,300,false")
    }
  }

}
