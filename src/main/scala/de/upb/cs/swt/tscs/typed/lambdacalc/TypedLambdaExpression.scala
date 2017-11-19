package de.upb.cs.swt.tscs.typed.lambdacalc

import de.upb.cs.swt.tscs.lambdacalc.{LambdaAbstraction, LambdaApplication, LambdaExpression, UntypedLambdaVariable, LambdaProjection, LambdaPair, PairProjectionFirst, PairProjectionSecond}
import de.upb.cs.swt.tscs.typed._
import de.upb.cs.swt.tscs.{Evaluation, Expression}

import scala.util.{Failure, Success, Try}

/**
  * Defines a type checker for the typed λ calculus
  */
trait TypedLambdaExpression extends LambdaExpression with Typecheck {

  override def typecheck(): Try[_] = typecheck(this)

  val Gamma = new scala.collection.mutable.HashMap[Expression, TypeInformation]

  def storeAndWrap(e: Expression, t: TypeInformation): Try[TypeInformation] = {
    Gamma.put(e, t)
    Success(t)
  }

  override def typecheck(expr: Expression): Try[TypeInformation] = {
    // Just an optimization
    if (Gamma.contains(expr)) return new Success(Gamma.get(expr).get)

    val result = expr match {
      /* T-Var */
      // Assume abstract base type for variables
      case v: TypedLambdaVariable if v.typeAnnotation.isEmpty => storeAndWrap(v, TypeInformations.AbstractBaseType)
      // Or simply trust the annotation
      case v: TypedLambdaVariable if !v.typeAnnotation.isEmpty => storeAndWrap(v, v.typeAnnotation.get)

      /* T-Abs */
      case abs : LambdaAbstraction
        if typecheck(abs.variable).isSuccess && typecheck(abs.term).isSuccess
      => storeAndWrap(abs, new FunctionTypeInformation(typecheck(abs.variable).get, typecheck(abs.term).get))

      /* T-App */
      case app : LambdaApplication
        if typecheck(app.function).isSuccess &&
          typecheck(app.function).get.isInstanceOf[FunctionTypeInformation] &&
          typecheck(app.argument).isSuccess &&
          typecheck(app.argument).get == typecheck(app.function).get.asInstanceOf[FunctionTypeInformation].sourceType
      => storeAndWrap(app, typecheck(app.function).get.asInstanceOf[FunctionTypeInformation].targetType)
      /*T-Pair **/
      case pair: LambdaPair
        if typecheck(pair.first).isSuccess &&
          typecheck(pair.second).isSuccess
      => storeAndWrap(pair, new ProductTypeInformation(typecheck(pair.first).get, typecheck(pair.second).get))
      /*T-Proj1*/
      case proj: PairProjectionFirst
        if typecheck(proj.pexpr).isSuccess &&
          typecheck(proj.pexpr).get.isInstanceOf[ProductTypeInformation]
      => storeAndWrap(proj.pexpr.asInstanceOf[LambdaPair].first, typecheck(proj.pexpr).get.asInstanceOf[ProductTypeInformation].firstType)
      /*T-Proj2*/
      case proj: PairProjectionSecond
        if typecheck(proj.pexpr).isSuccess &&
          typecheck(proj.pexpr).get.isInstanceOf[ProductTypeInformation]
      => storeAndWrap(proj.pexpr.asInstanceOf[LambdaPair].second, typecheck(proj.pexpr).get.asInstanceOf[ProductTypeInformation].secondType)
      case _
      => Failure[TypeInformation](new TypingException(expr))
    }
    /*
    print(expr.toString + " --- ")
    println(result match {
      case Success(t) => "Successfully typed to " + t
      case Failure(e) => "Typing failed"
    })*/
    result
  }



}
