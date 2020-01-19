package lambda

import lambda.PreTerm._
import lambda.Vars._

object ChurchNumeral {
  //c n = λs.λz.s n (z)
  def apply(n: Int): PreTerm = s ~> (z ~> applyPowerFToA(s, z, n))

  //  A + = λx.λy.λs.λz.x s (y s z);
  def sum:PreTerm = x ~> (y ~> (s ~> (z ~> ((x ^ s) ^ ((y ^ s) ^ z )))))

  def sum(first:PreTerm, second:PreTerm) :PreTerm = (sum ^ first) ^ second
  //  A ∗ = λx.λy.λs.x (y s);
  def product:PreTerm = x ~> (y ~> (s ~>(x ^(y ^ s))))

  def product(first:PreTerm, second:PreTerm) :PreTerm = (product ^ first) ^ second
  //  A e = λx.λy.y x.
  def power:PreTerm = x ~> (y ~> (y ^ x))

  def power(first:PreTerm, second:PreTerm) :PreTerm = (power ^ first) ^ second
}
