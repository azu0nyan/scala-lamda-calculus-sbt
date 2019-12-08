package lambda

import lambda.PreTerm._

object FV extends FreeVariables
object FreeVariables extends FreeVariables

trait FreeVariables extends (PreTerm  => Set[Variable]){
  override def apply(preTerm: PreTerm): Set[Variable] = preTerm match {
    case Variable(v) => Set(v)
    case Abstraction(x, p) => FreeVariables(p) - x
    case Application(m, n) => FreeVariables(m) | FreeVariables(n)
  }

}
