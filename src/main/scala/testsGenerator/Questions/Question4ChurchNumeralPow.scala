package testsGenerator.Questions
import lambda.PreTerm._
import lambda._
import testsGenerator.{Question, QuestionType}

import scala.util.Random

object Question4ChurchNumeralPow extends QuestionType {

  import lambda.Vars._


  val pows = Seq(
    (1,3),
    (1,4),
    (1,5),
    (2,2),
    (2,3),
    (3,2),
    (4,1),
  )



  override def generate(seed: Int): Question = {
    val r = new Random(seed)
    val rPow = pows(r.nextInt(pows.size))
    val qTerm = ChurchNumeral.power(ChurchNumeral(rPow._1 ), ChurchNumeral(rPow._2) )

    val answTerm = Normalization(qTerm)

    return new Question {
      override def html: String =
        s"""<h3>Приведите к β-нормальной  форме</h3>
            ${ToPrettyString(qTerm)}"""

      override def answHtml: String =
        s"${ToPrettyString(answTerm)} \n ${rPow._1}^${rPow._2} = ${math.pow(rPow._1, rPow._2).intValue()}"
    }
  }
}
