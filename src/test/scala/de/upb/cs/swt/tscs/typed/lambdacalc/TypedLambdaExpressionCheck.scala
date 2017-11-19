package de.upb.cs.swt.tscs.typed.lambdacalc

import org.scalatest.{FlatSpec, Matchers}

import scala.util.{Failure, Success}

/**
  * Checks for the type checker
  */
class TypedLambdaExpressionCheck extends FlatSpec with Matchers {
  "λx.x : A" should "successfully typecheck" in {
    var parserResult = new TypedLambdaCalculusSyntax("λx.x : A").Term.run()
    parserResult shouldBe a [Success[_]]
    parserResult.get.typecheck() shouldBe a [Success[_]]
  }

  "λx : [A->A].λy : A.((x y) x)" should "successfully parse" in {
    var parserResult = new TypedLambdaCalculusSyntax("λx : [A->A].λy : A.((x y) x)").Term.run()
    parserResult shouldBe a [Success[_]]
  }

  it should "not successfully typecheck" in {
    var parserResult = new TypedLambdaCalculusSyntax("λx : [A->A].λy : A.((x y) x)").Term.run()
    parserResult.get.typecheck() shouldBe a [Failure[_]]
  }

  "λz : A.λx : [A->A].λy : [A->A].(x (y z))" should "successfully typecheck" in {
    var parserResult = new TypedLambdaCalculusSyntax("λz : A.λx : [A->A].λy : [A->A].(x (y z))").Term.run()
    parserResult shouldBe a [Success[_]]
    parserResult.get.typecheck() shouldBe a [Success[_]]
  }

  "(λx.x x)" should "successfully typecheck" in {
    var parserResult = new TypedLambdaCalculusSyntax("(λx.x x)").Term.run()
    parserResult shouldBe a [Success[_]]
    parserResult.get.typecheck() shouldBe a [Success[_]]
  }

  "λx : [[A->A]->[A->A]].λy : [A->A].((x y) c)" should "successfully typecheck" in {
    var parserResult = new TypedLambdaCalculusSyntax("λx : [[A->A]->[A->A]].λy : [A->A].((x y) c)").Term.run()
    parserResult shouldBe a [Success[_]]
    parserResult.get.typecheck() shouldBe a [Success[_]]
  }
  "{λx : [A->A].y,λx : [A->A].c}:[[A->A]X[A->A]] " should "successfully typecheck" in {
    var parserResult = new TypedLambdaCalculusSyntax("{λx : [A->A].y,λx : [A->A].c}:[[A->A]X[A->A]]").Term.run()
    parserResult shouldBe a [Success[_]]
    parserResult.get.typecheck() shouldBe a [Success[_]]
  }
  "{v,c}.1:A " should "successfully typecheck" in {
    var parserResult = new TypedLambdaCalculusSyntax("{v,c}.1:A").Term.run()
    parserResult shouldBe a [Success[_]]
    parserResult.get.typecheck() shouldBe a [Success[_]]
  }

}
