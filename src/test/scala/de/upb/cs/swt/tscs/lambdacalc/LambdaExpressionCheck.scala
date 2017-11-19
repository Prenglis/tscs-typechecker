package de.upb.cs.swt.tscs.lambdacalc

import org.scalatest.{FlatSpec, Matchers}

import scala.util.Success

/**
  * Checks for Lambda Calculus Expressions
  */
class LambdaExpressionCheck extends FlatSpec with Matchers {
  "λx.x" should "be valid" in {
    var parserResult = new LambdaCalculusSyntax("λx.x").Term.run()
    parserResult shouldBe a [Success[_]]
    parserResult.get shouldBe new LambdaAbstraction(new UntypedLambdaVariable("x"), new UntypedLambdaVariable("x"))
  }

  "λx.λy.((x y) x)" should "be valid" in {
    var parserResult = new LambdaCalculusSyntax("λx.λy.((x y) x)").Term.run()
    parserResult shouldBe a [Success[_]]
    parserResult.get shouldBe new LambdaAbstraction(UntypedLambdaVariable("x"),
      new LambdaAbstraction(UntypedLambdaVariable("y"),
        LambdaApplication(
          LambdaApplication(UntypedLambdaVariable("x"),UntypedLambdaVariable("y"))
          ,UntypedLambdaVariable("x"))))
  }

  "λz.λx.λy.(x (y z))" should "be valid" in {
    var parserResult = new LambdaCalculusSyntax("λz.λx.λy.(x (y z))").Term.run()
    parserResult shouldBe a [Success[_]]
    parserResult.get shouldBe
      new LambdaAbstraction(UntypedLambdaVariable("z"),
        LambdaAbstraction(UntypedLambdaVariable("x"),
          LambdaAbstraction(UntypedLambdaVariable("y"),
            LambdaApplication(
              UntypedLambdaVariable("x"),
              LambdaApplication(UntypedLambdaVariable("y"),UntypedLambdaVariable("z"))))))
  }

  "(λx.x x)" should "be valid" in {
    var parserResult = new LambdaCalculusSyntax("(λx.x x)").Term.run()
    parserResult shouldBe a [Success[_]]
    parserResult.get shouldBe new LambdaApplication(
      LambdaAbstraction(UntypedLambdaVariable("x"), UntypedLambdaVariable("x")),
      UntypedLambdaVariable("x"))
  }

  "(λx.λy.((x y) x) c)" should "be valid" in {
    var parserResult = new LambdaCalculusSyntax("(λx.λy.((x y) x) c)").Term.run()
    parserResult shouldBe a [Success[_]]
    parserResult.get shouldBe new LambdaApplication(
      LambdaAbstraction(UntypedLambdaVariable("x"),
        LambdaAbstraction(UntypedLambdaVariable("y"),
          LambdaApplication(
            LambdaApplication(
              UntypedLambdaVariable("x"),
              UntypedLambdaVariable("y")),
            UntypedLambdaVariable("x")))),
      UntypedLambdaVariable("c"))
  }

  it should "evaluate to a substituted form" in {
    var parserResult = new LambdaCalculusSyntax("(λx.λy.((x y) x) c)").Term.run()

    parserResult.get.-->*() shouldBe new LambdaAbstraction(UntypedLambdaVariable("y"),
      LambdaApplication(
        LambdaApplication(
          UntypedLambdaVariable("c"),
          UntypedLambdaVariable("y")),
        UntypedLambdaVariable("c")))
  }
  "{v,c}.1" should "should evaluate" in {
    var parserResult = new LambdaCalculusSyntax("{v,c}.1").Term.run()
    parserResult shouldBe a [Success[_]]
    //var eval_form = new LambdaCalculusSyntax("{v,c}.1").InputLine.run()
    var eval_form = parserResult.get.-->*()
    eval_form shouldBe a [Success[_]]

  }

}
