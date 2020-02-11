package testsGenerator.Questions

import scalatags.Text.all._
import lambda.PreTerm.{PreTerm, Variable}
import lambda.ToPrettyString
import testsGenerator.{Question, QuestionType}

import scala.util.Random

object Question1AlphaEquv extends QuestionType {

  import lambda.Vars._

  var preTerms: Seq[PreTerm] = Seq(
    x ~> y,
    z ~> y,
    z ~> w,
    x ~> x,
    y ~> y,
    x ~> (x ^ y),
    x ~> (x ^ w),
    y ~> (y ^ w),
    x ~> (w ^ x),
    x ~> (y ~> (x ^ y)),
    u ~> (w ~> (u ^ w)),
    u ~> (w ~> (u ^ z)),
    x ~> (y ~> (x ^ z)),
    (x ^ y) ^ w,
    (x ^ w) ^ y,
    x ^ (y ^ w),
    x ^ (w ^ y)
  )

  val a: Variable = "a"
  val b: Variable = "b"
  val c: Variable = "c"
  val d: Variable = "d"
  val mappings: Seq[Map[Variable, Variable]] = Seq(
    Map(),
    Map(x -> y, y -> x, w -> u, u -> w),
    Map(x -> z, z -> x),
    Map(x -> a, a -> x, w -> b, b -> w),
    Map(x -> c, c -> x, w -> d, d -> w),
  )


  def generate(seed: Int): Question = {
    val r = new Random(seed)
    val randMap = mappings(r.nextInt(mappings.size))
    val shuffledMappedIndexed = //r.shuffle(preTerms)
      preTerms
        .map(lambda.replaceVars(_, randMap))
        .zipWithIndex.map { case (x, y) => (x, y + 1) }

    val answ = for (
      (termi, i) <- shuffledMappedIndexed;
      (termj, j) <- shuffledMappedIndexed if i < j && termi <=> termj
    ) yield (i, j)

    return new Question {
      //      override def html: String = shuffledMappedIndexed
      //        .map { case (t, i) => f"$i%2d | ${ToPrettyString(t)} <br>\n" }.reduce(_ + _)


      override def answHtml: String = answ.toString()

      override def html: String =
        div(
          h3("Какие из перечисленных претермов α-эквивалентны"),
          ol(`type` := 1, textIndent := "10px")(
            shuffledMappedIndexed.map { case (t, _) => li(ToPrettyString(t)) }
          )).toString()
    }

  }
}
