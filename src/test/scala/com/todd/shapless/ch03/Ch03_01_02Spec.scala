package com.todd.shapless.ch03

class Ch03_01_02Spec {
  trait CsvEncoder[A] {
    def encode(a : A): List[String]
  }

  object CsvEncoder {
    def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] = enc

    def instance[A](func: A => List[String]): CsvEncoder[A] =
      (a: A) => func(a)
  }

  implicit val booleanEncoder: CsvEncoder[Boolean] =
    CsvEncoder.instance((b: Boolean) => if (b) List("yes") else List("no"))


//  the[CsvEncoder[IceCream]]

}
