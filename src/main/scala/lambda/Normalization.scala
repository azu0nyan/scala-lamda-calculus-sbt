package lambda

import lambda.PreTerm._

//import lambda.PreTerm._

object Normalization {

  def apply(p: PreTerm): PreTerm = {
    val res = p match {
      case v@Variable(_) => v
      case Application(Abstraction(x, b), n) => Substitution(b, x, n)
      case Application(a, b) => Application(Normalization(a), Normalization(b))
      case Abstraction(x, m) => Abstraction(x, Normalization(m))
    }
    if(res !<=> p) Normalization(res) else p
  }

}
