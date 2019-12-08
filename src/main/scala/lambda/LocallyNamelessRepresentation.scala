package lambda

import lambda.PreTerm._

object LocallyNamelessRepresentation {

  sealed trait LocallyNamelessRepresentedTerm {
    implicit def toPreTerm: PreTerm
    implicit def toLambdaTerm: LTerm = LTerm(toPreTerm)
  }

  case class BVar(i: Int) extends LocallyNamelessRepresentedTerm {
    override def toPreTerm: Variable = Variable("#" + i.toString)
  }

  case class FVar(v: String) extends LocallyNamelessRepresentedTerm {
    override def toPreTerm: Variable = Variable(v)
  }

  case class App(t1: LocallyNamelessRepresentedTerm, t2: LocallyNamelessRepresentedTerm) extends LocallyNamelessRepresentedTerm {
    override def toPreTerm: Application = Application(t1.toPreTerm, t2.toPreTerm)
  }

  case class Abs(v: BVar, f: LocallyNamelessRepresentedTerm) extends LocallyNamelessRepresentedTerm {
    override def toPreTerm: Abstraction = Abstraction(v.toPreTerm, f.toPreTerm)
  }

  def apply(preTerm: PreTerm): LocallyNamelessRepresentedTerm = convert(preTerm, Map())

  private def convert(preTerm: PreTerm, replacements: Map[Variable, BVar]): LocallyNamelessRepresentedTerm = preTerm match {
    case v@Variable(_) =>
      replacements.get(v) match {
        case Some(value) => value
        case None => FVar(v.v)
      }
    case Abstraction(x, m) =>
      val newVar = BVar(replacements.size)
      Abs(newVar, convert(m, replacements + (x -> newVar)))
    case Application(m, n) =>
      App(convert(m, replacements), convert(n, replacements))
  }
}
