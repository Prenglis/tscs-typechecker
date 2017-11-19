package de.upb.cs.swt.tscs.lambdacalc

import de.upb.cs.swt.tscs.{Evaluation, Expression, Value}

/**
  * Specifies the operation semantics of the λ calculus
  */
trait LambdaExpression extends Evaluation {
  override def -->(): Expression = -->(this)

  override def -->(expr: Expression): Expression = {
    println("Evaluating: " + expr.toString)
    expr match {
      /* E-App1   */ case LambdaApplication(t1, t2) if progressPossible(t1) && !isValue(t1) => LambdaApplication(-->(t1), t2)
    /* E-App2   */ case LambdaApplication(v1, t2) if isValue(v1) && progressPossible(t2) => LambdaApplication(v1, -->(t2))
    /* E-AppAbs */ case LambdaApplication(LambdaAbstraction(x,t12), v2) if isValue(v2) => substitute(matcher(x,v2), t12)
    case λ : LambdaAbstraction => λ
    /*E-PairBeta1*/ case PairProjectionFirst(LambdaPair(first, second)) if isValue(first) && isValue(second) => first
    /*E-PairBeta2*/ case PairProjectionSecond(LambdaPair(first, second)) if isValue(first) && isValue(second) => second
    /*E-Proj1*/ case PairProjectionFirst(t) if progressPossible(t) && !isValue(t) => PairProjectionFirst(-->(t))
    /*E-Proj2*/ case PairProjectionSecond(t) if progressPossible(t) && !isValue(t) => PairProjectionSecond(-->(t))
    /*E-Pair1*/ case LambdaPair(t1,t2) if !isValue(t1) && !isValue(t2) && progressPossible(t1) => LambdaPair( -->(t1), t2)
    /*E-Pair2*/ case LambdaPair(v1,t2) if isValue(v1) && !isValue(t2) && progressPossible(t2) => LambdaPair( v1, -->(t2))
    case _ => null
    }
  }

  def substitute(f : PartialFunction[LambdaVariable, Expression], term : Expression) : Expression = {
    term match {
      case v : LambdaVariable if f.isDefinedAt(v) => f(v)
      case v : LambdaVariable => v
      case LambdaApplication(t1, t2) => LambdaApplication(substitute(f, t1), substitute(f, t2))
      case LambdaAbstraction(v, t) => LambdaAbstraction(v, substitute(f, t))
    }
  }

  def matcher(variable : LambdaVariable,  expression : Expression) : PartialFunction[LambdaVariable, Expression] =
    new PartialFunction[LambdaVariable, Expression] {
      def apply(pv: LambdaVariable) = if (pv == variable) expression else null
      override def isDefinedAt(pv: LambdaVariable): Boolean = (pv == variable)
    }


}
