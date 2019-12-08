package lambda

import lambda.PreTerm.{Abstraction, Application, PreTerm, Variable}

import scala.annotation.tailrec

object AlphaEquivalence {

  def apply(a: PreTerm, b: PreTerm): Boolean = equal(a, b, Seq())

  @tailrec def eqVars(a: Variable, b: Variable, boundVars: Seq[(Variable, Variable)]): Boolean = boundVars match {
    case (`a`, `b`) :: _ => true
    case (`a`, _) :: _ | (_, `b`) :: _ => false
    case _ :: tail => eqVars(a, b, tail)
    case Nil => a == b
  }

  private def equal(a: PreTerm, b: PreTerm, vars: Seq[(Variable, Variable)]): Boolean = (a, b) match {
    case (x@Variable(_), y@Variable(_)) => eqVars(x, y, vars)
    case (Abstraction(x, p), Abstraction(y, q)) => equal(p, q, (x, y) +: vars)
    case (Application(x, p), Application(y, q)) => equal(x, y, vars) && equal(p, q,vars)
    case _ => false
  }



}
