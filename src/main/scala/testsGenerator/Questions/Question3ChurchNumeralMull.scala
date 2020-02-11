package testsGenerator.Questions

import lambda.PreTerm._
import lambda._
import testsGenerator.{Question, QuestionType}

import scala.util.Random

object Question3ChurchNumeralMull extends QuestionType {

  import lambda.Vars._


  val mulls = Seq(
    (2, 3),
    (2, 4),
    (2, 2),
    (2, 3),
    (3, 2),
    (3, 2),
    (4, 2),
    (1, 5),
    (5, 1),
    (6, 1)
  )


  override def generate(seed: Int): Question = {
    val r = new Random(seed)
    val rPow = mulls(r.nextInt(mulls.size))
    val qTerm = ChurchNumeral.product(ChurchNumeral(rPow._1), ChurchNumeral(rPow._2))

    val answTerm = Normalization(qTerm)

    return new Question {
      override def html: String =
        s"""<h3>Приведите к β-нормальной  форме</h3>
            ${ToPrettyString(qTerm)}"""

      override def answHtml: String =
        s"${ToPrettyString(answTerm)} \n ${rPow._1} * ${rPow._2} = ${(rPow._1 * rPow._2).intValue()}"
    }
  }
}
