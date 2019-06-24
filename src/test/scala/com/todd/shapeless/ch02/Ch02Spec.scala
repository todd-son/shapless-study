package com.todd.shapeless.ch02

import org.scalatest.WordSpec
import shapeless._

class Ch02Spec extends WordSpec {
  "Employee" should {
    "convert to generic representation" in {
      val employee = Employee("todd", 1)
      val repr = Generic[Employee].to(employee)

      assert(repr == "todd" :: 1 :: HNil)
    }

    "convert from generic representations" in {
      val repr = "todd" :: 1 :: HNil
      val employee = Generic[Employee].from(repr)

      assert(employee == Employee("todd", 1))
    }

    case class Employee(name: String, number: Int)
  }

  "Products" should  {
    "encode using coproduct" in {
      val rectangle = Rectangle(1.0, 1.0)
      val circle = Circle(2.0)

      val gen = Generic[Shape]

      val result1 = gen.to(rectangle)
      val result2 = gen.to(circle)

      assert(rectangle == gen.from(result1))
      assert(circle == gen.from(result2))
    }
  }
}
