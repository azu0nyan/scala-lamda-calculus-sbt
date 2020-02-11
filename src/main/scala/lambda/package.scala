import lambda.PreTerm._

//import scala.language.implicitConversions

package object lambda {


  def applyPowerFToA(f:PreTerm, a:PreTerm, power:Int):PreTerm = if(power == 0) a else
    Application(f, applyPowerFToA(f, a, power - 1 ))

  implicit def preTermToLTerm(p:PreTerm):LTerm = LTerm(p)
  implicit def lTermToPreTer(l:LTerm):PreTerm = l.p

  object Vars extends Vars

  trait Vars {


    val x: Variable = "x"
    val y: Variable = "y"
    val z: Variable = "z"
    val u: Variable = "u"
    val w: Variable = "w"
    val s: Variable = "w"
  }

  def replaceVars(p:PreTerm, map:Map[Variable, Variable]) :PreTerm = p match {
    case v@Variable(_) => map.getOrElse(v, v)
    case Abstraction(x, m) => Abstraction(map.getOrElse(x, x), replaceVars(m, map))
    case Application(m, n) =>Application(replaceVars(m, map), replaceVars(n, map))
  }
}
