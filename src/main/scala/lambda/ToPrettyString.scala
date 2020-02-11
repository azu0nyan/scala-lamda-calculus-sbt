package lambda

import lambda.PreTerm._

object ToPrettyString {
  //→
  //λ
  def apply(p: PreTerm): String = outer(p)

  private def outer(p: PreTerm): String = p match {
    case Variable(v) =>
      v
    case Abstraction(x, m) =>
      s"λ$x.${outer(m)}"
    case Application(a@Abstraction(_, _), n@Abstraction(_, _)) =>
      s"(${outer(a)}) (${outer(n)})"
    case Application(a@Abstraction(_, _), n) =>
      s"(${outer(a)}) ${outer(n)}"
    case Application(a@Application(_,_), b@Application(_, _)) =>
      s"(${outer(a)}) (${outer(b)})"
    case Application(a@Variable(_), b@Application(_, _)) =>
      s"${outer(a)} (${outer(b)})"
    case Application(a, b@Abstraction(_,_)) =>
      s"${outer(a)} (${outer(b)})"
    case Application(a, b) =>
      s"${outer(a)} ${outer(b)}"
  }
//(((x -> (y -> (w -> (z -> ((x w) ((y w) z)))))) (w -> (z -> (w (w z))))) (w -> (z -> (w z))))
//  private def inner(p: PreTerm): String = p match {
//    case Variable(v) => v
//    case Abstraction(x, m) => s"(λ$x.${inner(m)})"
//    case Application(m, a@Application(_, _)) =>
//  }
}
