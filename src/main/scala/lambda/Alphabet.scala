package lambda

import lambda.PreTerm.Variable

import scala.collection.immutable

object Alphabet {


  val variables:LazyList[Variable] = for(
    j <- ( "" ++ (0 to 100_000)).to(LazyList);
    i <- 'a' to 'z'
  )yield (Variable(i.toString + j.toString))

  def getLeastNotContainedIn(vars:Set[Variable]):Variable = variables.find(v => !vars.contains(v)).get

}
