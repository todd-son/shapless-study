package com.todd.shapeless

import shapeless._

package object ch02 {
  sealed trait Shape

  final case class Rectangle(width: Double, height: Double) extends Shape
  final case class Circle(radius: Double) extends Shape

  type Rectangle2 = (Double, Double)
  type Circle2 = Double
  type Shape2 = Either[Rectangle2, Circle2]

  val product: String :: Int :: Boolean :: HNil = "Sunday" :: 1 :: false :: HNil

  case class Red()

  case class Amber()

  case class Green()

  type Light = Red :+: Amber :+: Green :+: CNil
}
