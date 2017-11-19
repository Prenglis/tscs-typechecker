package de.upb.cs.swt.tscs.lambdacalc


import org.parboiled2.{CharPredicate, Parser, ParserInput, Rule1}

/**
  * A syntax definition for the λ calculus
  */
class LambdaCalculusSyntax (val input : ParserInput) extends Parser {
  def InputLine = rule {
    Term | EOI
  }

  def Term: Rule1[LambdaExpression] = rule {
    LambdaVariableTerm |
      LambdaAbstractionTerm |
      LambdaApplicationTerm |
      LambdaPairTerm |
      LambdaPairProjection
  }
  def LambdaPairTerm : Rule1[LambdaPair] = rule {
    "{" ~ Term ~ "," ~ Term ~ "}" ~> LambdaPair


  }

  def LambdaPairProjection : Rule1[LambdaProjection] = rule {
    LambdaPairProjectionFirst | LambdaPairProjectionSecond
  }

  def LambdaPairProjectionFirst : Rule1[LambdaProjection] = rule {
    Term ~ ".1" ~> PairProjectionFirst
  }
  def LambdaPairProjectionSecond : Rule1[LambdaProjection] = rule {
    Term ~ ".2" ~> PairProjectionSecond
  }
    def LambdaVariableTerm : Rule1[LambdaVariable] = rule {
      capture(oneOrMore(CharPredicate.Alpha)) ~> UntypedLambdaVariable

    }

    def LambdaAbstractionTerm : Rule1[LambdaAbstraction] = rule {
      'λ' ~ LambdaVariableTerm ~ "." ~ Term ~> LambdaAbstraction
    }

    def LambdaApplicationTerm : Rule1[LambdaApplication] = rule {
      "(" ~ Term ~ " " ~ Term ~ ")" ~> LambdaApplication
    }
  }
