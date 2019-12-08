package lambda

import lambda.PreTerm.{Abstraction, Application, PreTerm, Variable}

//object Substitution extends Substitution
//
//object M extends Substitution
object Substitution {

  /** the substitution of N for x in M
   * M[x := N]
   */
  implicit class SubstitutionImplicit(val m: PreTerm) {

    def apply(x: Variable, n: PreTerm): PreTerm = m.substitute(x, n)

    def substitute(x: Variable, n: PreTerm): PreTerm = (m, x) match {
      //x[x := N] = N;
      case (`x`, `x`) => n
      //y[x := N] = y;
      case (y@Variable(_), _) => y
      //(P Q)[x := N] = P[x := N] Q[x := N];
      case (Application(p, q), _) => Application(p(x, n), q(x, n))
      //(λx.P)[x := N] = λx.P;
      case (a@Abstraction(`x`, _), `x`) => a
      //(λy.P)[x := N] = λy.P[x := N], if y !∈ FV(N) or x !∈ FV(P);
      case (Abstraction(y, p), _) if !FV(n).contains(y) || !FV(p).contains(x) => Abstraction(y, p(x, n))
      //(λy.P)[x := N] = λz.P[y := z][x := N], if y ∈ FV(N) and x ∈ FV(P).
      // where z is chosen as the vi ∈ V with minimal i such that vi !∈ FV(P)∪FV(N)  in the last clause.
      //если у свободен в  N и x свободен в Р  то будет выполгена подстановка в которой будет у в неправильной роли
      //поэтому ищем ему замену
      case (Abstraction(y, p), _) =>
        val z = Alphabet.getLeastNotContainedIn(FV(p) | FV(n))
        Abstraction(z, p.apply(y, z)(x, n))
    }
  }


  def apply(m: PreTerm, x: Variable, n: PreTerm): PreTerm = m.substitute(x, n)


}
