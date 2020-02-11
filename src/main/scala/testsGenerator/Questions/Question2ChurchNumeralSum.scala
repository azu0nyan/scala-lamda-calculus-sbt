package testsGenerator.Questions

import lambda.PreTerm._
import lambda._
import testsGenerator.{Question, QuestionType}

import scala.util.Random

object Question2ChurchNumeralSum extends QuestionType {

  import lambda.Vars._


  val sums = Seq(
    (2, 3),
    (2, 4),
    (2, 2),
    (2, 3),
    (3, 2),
    (3, 2),
    (4, 2),
  )


  override def generate(seed: Int): Question = {
    val r = new Random(seed)
    val rPow = sums(r.nextInt(sums.size))
    val qTerm = ChurchNumeral.sum(ChurchNumeral(rPow._1), ChurchNumeral(rPow._2))

    val answTerm = Normalization(qTerm)

    return new Question {
      override def html: String =
        s"""<h3>Приведите к β-нормальной  форме</h3>
            ${ToPrettyString(qTerm)}"""

      override def answHtml: String =
        s"${ToPrettyString(answTerm)} \n ${rPow._1} + ${rPow._2} = ${(rPow._1 + rPow._2).intValue()}"
    }
  }
}
