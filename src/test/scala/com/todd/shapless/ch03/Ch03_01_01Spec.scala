package com.todd.shapless.ch03

import org.scalatest.WordSpec

class Ch03_01_01Spec extends WordSpec {
  "CsvEncoder" should {
    trait CsvEncoder[A] {
      def encode(value: A): List[String]
    }

    case class Employee(name: String, age: Int)

    implicit val employeeEncoder: CsvEncoder[Employee] =
      (e: Employee) => {
        List(
          e.name,
          e.age.toString
        )
      }

    def writeCsv[A](values: List[A])(implicit encoder: CsvEncoder[A]) =
      values.map(a => encoder.encode(a).mkString(",")).mkString("\n")

    "encode employee successfully" in {
      val result = writeCsv(
        List(
          Employee("todd", 30),
          Employee("eileen", 31)
        )
      )

      assert(result == "todd,30\neileen,31")
    }
  }


}
