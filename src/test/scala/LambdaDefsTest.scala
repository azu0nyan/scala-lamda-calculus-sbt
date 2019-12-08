
import lambda.PreTerm._
import lambda._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._
import Substitution._

class LambdaDefsTest extends AnyFunSuite with Vars {
  test("FreeVariables") {
    assert(FreeVariables(x) == Set(x))
    assert(FreeVariables(x ^ y) == Set(x, y))
    assert(FreeVariables(x ^ y ^ z) == Set(x, y, z))
    assert(FreeVariables(x ~> (x ^ y)) == Set(y))
    assert(FreeVariables((x ~> (x ^ x)) ^ (y ~> (y ^ y))) == Set())
  }

  test("closed set") {
    assert(!x.closed)
    assert(!(x ^ y).closed)
    assert((x ~> x).closed)
  }

  test("substitution errors") {
    assert(((x ~> y) substitute("a", "b")) == (x ~> y))
    assert((Variable("a") substitute("x", "q" ~> "w")) == Variable("a"))
    assert((("a" ^ "b") substitute("x", "q" ~> "w")) == ("a" ^ "b"))
    assert((z ~> ((x ^ y) ^ z) substitute(x, y)) == (z ~> ((y ^ y) ^ z)))
  }

  test("substitution rules") {
    val n = "q" ~> "w"
    assert(x.substitute(x, n) == n) //1 x[x := N] = N;
    assert(y.substitute(x, n) == y) //2 y[x := N] = y;
    //3 (P Q)[x := N] = P[x := N] Q[x := N];
    val pairs: Seq[(PreTerm, PreTerm)] = Seq(
      ("a", "b"),
      (x, y),
      ("q", "w"),
      ("w", "q"),
      ("a", n),
      (n, "a"),
    )
    pairs.foreach { case (a, b) =>
      val left = (a ^ b).substitute(x, n)
      val right = Application(Substitution(a, x, n), Substitution(b, x, n))
      println(
        s"""(P Q)[x := N] = P[x := N] Q[x := N];
           | P = $a;
           | Q = $b;
           | x = $x
           | N = $n
           | result
           | $left
           | ==
           | $right"""".stripMargin)

      assert(left == right)
    }
    //4 (λx.P)[x := N] = λx.P;
    assert((x ~> y).substitute(x, "a") == (x ~> y))
    assert((x ~> n).substitute(x, n) == (x ~> n))

    //5 (λy.P)[x := N] = λy.P[x := N], if y !∈ FV(N) or x !∈ FV(P);
    assert((y ~> z).substitute(z, "a") == (y ~> "a"))
    assert((y ~> (z ^ w)).substitute(z, "a") == (y ~> ("a" ^ w)))
    assert((y ~> (x ~> (z ^ w))).substitute(y, "a") == (y ~> (x ~> (z ^ w))))
    assert((y ~> (x ~> (z ^ w))).substitute(x, n) == (y ~> (x ~> (z ^ w))))

    //6 (λy.P)[x := N] = λz.P[y := z][x := N], if y ∈ FV(N) and x ∈ FV(P).
    //todo??

  }

  test("substitutuon example") {
    //((λx.x yz) (λy.x y z) (λz.x y z))[x := y]=(λx.x yz) (λu.y u z) (λz.y y z)
    val left = (x ~> ((x ^ y) ^ z)) ^ (y ~> ((x ^ y) ^ z)) ^ (z ~> ((x ^ y) ^ z))
    println(left)
    println(Substitution(left, x, y))
  }

  test("alpha equivalence examples") {
    assert(x ~> x <=> y ~> y)
    assert(x ~> (x ^ z) <=> y ~> (y ^ z))
    assert(x ~> (y ~> (x ^ y)) <=> y ~> (x ~> (y ^ x)))
    assert(x ~> (x ^ y) !<=> x ~> (x ^ z))
  }
}
