package de.upb.cs.swt.tscs.typed.lambdacalc


import de.upb.cs.swt.tscs.lambdacalc._
import de.upb.cs.swt.tscs.typed.{BaseTypeInformation, FunctionTypeInformation, TypeInformation}
import org.parboiled2.{CharPredicate, Parser, ParserInput, Rule1}

/**
  * A syntax definition for the typed λ calculus
  */
class TypedLambdaCalculusSyntax(input : ParserInput) extends LambdaCalculusSyntax(input) {

  override def Term: Rule1[TypedLambdaExpression] = rule {
    LambdaVariableTerm |
      LambdaAbstractionTerm ~> (widen(_ : LambdaAbstraction)) |
      LambdaApplicationTerm ~> (widen(_ : LambdaApplication))
  }

  override def LambdaVariableTerm : Rule1[TypedLambdaVariable] = rule {
    capture(oneOrMore(CharPredicate.Alpha)) ~ optional (" : " ~ TypeInfo) ~> TypedLambdaVariable
  }

  def TypeInfo = rule {
    BaseTypeInfo | FunctionType
  }

  def BaseTypeInfo : Rule1[TypeInformation] = rule {
    capture("A") ~> BaseTypeInformation
  }

  def FunctionType : Rule1[TypeInformation] = rule {
    "[" ~ TypeInfo ~ "->" ~ TypeInfo ~ "]" ~> FunctionTypeInformation
  }


  def widen(expression: LambdaExpression) : TypedLambdaExpression = {
    expression match {
      case UntypedLambdaVariable(v) => new TypedLambdaVariable(v, Option.empty)
      case LambdaApplication(f,a) => new LambdaApplication(f,a) with TypedLambdaExpression
      case LambdaAbstraction(v,t) => new LambdaAbstraction(v,t) with TypedLambdaExpression
    }
  }
}
