package com.todd.shapeless.ch01

import org.scalatest.WordSpec
import shapeless._

class Ch01Spec extends WordSpec {
  "shapeless" should {
    "convert case class to hlist" in {

      val employeeGeneric = Generic[Employee].to(Employee("todd", 10, false))
      val iceCreamGeneric = Generic[IceCream].to(IceCream("todd", 10, false))

      def genericCsv(gen: String :: Int :: Boolean :: HNil): List[String] =
        List(gen(0), gen(1).toString, gen(2).toString)

      assert(genericCsv(employeeGeneric) == genericCsv(iceCreamGeneric))
    }
  }

  case class Employee(name: String, number: Int, manager: Boolean)

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)
}
