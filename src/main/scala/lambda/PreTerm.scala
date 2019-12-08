package lambda




object PreTerm {

  //  trait Alphabet[A]

  type V = String

  sealed trait PreTerm {

    def <=>(ot:PreTerm):Boolean = AlphaEquivalence(this, ot)

    def !<=>(ot:PreTerm):Boolean = !AlphaEquivalence(this, ot)

    def alphaEquivalent: PreTerm => Boolean = <=>

    def notAlphaEquivalent: PreTerm => Boolean = !<=>

    lazy val closed:Boolean = FV(this).isEmpty

    //def apply(ot: PreTerm): PreTerm = Application(this, ot)

    def ^(ot:PreTerm) :Application = Application(this, ot)

    override def toString: V = this match {
      case Variable(v) => v.toString
      case Abstraction(x, m) => s"($x -> $m)"
      case Application(m, n) => s"($m $n)"
    }
  }

  case class Variable(v: V) extends PreTerm {
    def ~>(m: PreTerm): Abstraction = Abstraction(this, m)
  }

  case class Abstraction(x: Variable, m: PreTerm) extends PreTerm

  case class Application(m: PreTerm, n: PreTerm) extends PreTerm


  //syntax sugar
  // "x" ~> (("x" ^ "y") ^ ("z" ~> "w" ))
  //  x ~> ((x ^ y) ^ (z ~> w ))

  implicit def toStringToVariable(s: String): Variable = Variable(s)

   def application(pair:(PreTerm, PreTerm)): Application = Application(pair._1, pair._2)


}

