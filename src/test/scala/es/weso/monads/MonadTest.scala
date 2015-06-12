package es.weso.monads

import cats.data.{Validated, Xor}
import cats.syntax.apply._ // For |@| syntax
import cats.std.list._ // For semigroup (append) on List

import org.scalatest._
import org.scalatest.prop.Checkers
import scalaz.Validation.ValidationFlatMapDeprecated

class MonadTest
    extends FunSpec
    with Matchers
    with Checkers {
  
  type ErrorType = String
  type Comp[A] = Validated[ErrorType,A]
  
  def computeNumber(n:Int): Comp[Int] = {
    valid(n)
  }
  
  def err(msg: String): Comp[Int] = {
   val e: Comp[Int] = invalid("Error: " + msg)
   e
  }
  
  def add(m:Int, n:Int): Comp[Int] = for {
    x <- computeNumber(m)
    y <- computeNumber(n)
  } yield (x + y)
  
  describe("Scalaz tests") {
    
    it("should add 2 numbers") {
      add(2,3).getOrElse(0) should be(5)
    }
    
    it("should recover from error") {
      err("xxx").getOrElse(0) should be(0)
    }

    it("should show error ") {
      println(err("xxx").show)
      err("xxx").isFailure should be(true)
    }
    
  }
}
