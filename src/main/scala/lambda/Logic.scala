package lambda

import lambda.PreTerm.{Abstraction, Application, PreTerm, Variable}

object Logic {
  private val x: Variable = "x"
  private val y: Variable = "y"
  /*
  true = λx.λy.x;
  false = λx.λy.y;
  if B then P else Q = B P Q.
  Then
  if true then P else Q =β P ;
  if false then P else Q =β Q.
   */
  val TRUE: PreTerm = x ~> (y ~> x)
  val FALSE: PreTerm = x ~> (y ~> y)

  //  val and:PreTerm =

  def pair(P: PreTerm, Q: PreTerm): PreTerm = {
    val xx:Variable  = Alphabet.getLeastNotContainedIn(Set(P, Q)).head
    xx ~> ((xx ^ P) ^ Q)
  }

  val proj1 = x ~> (y ~> x) // == TRUE
  val proj2 = x ~> (y ~> y) // FALSE

  def tupleN(p: Seq[PreTerm]): PreTerm = {
    val x_ = Alphabet.getLeastNotContainedIn(p.toSet).head
//    x_ ~> p.foldLeft[PreTerm](x_)(_ ^ _)
    x_ ~> p.foldLeft[PreTerm](x_)((a, b)=> Application(a, b))
  }

  def projN(number:Int,total:Int):PreTerm = {
    require(number <= total && number > 0 )
    val vars:Seq[Variable ] = Alphabet.variables.take(total)
    val nth:Variable = vars(number - 1)
    vars.foldRight[PreTerm](nth)(_ ~> _)
  }


}
