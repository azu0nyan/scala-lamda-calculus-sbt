package testsGenerator

import java.io.{File, PrintWriter}

import lambda.{ChurchNumeral, ToPrettyString}
import testsGenerator.Questions.{Question1AlphaEquv, Question2ChurchNumeralSum, Question3ChurchNumeralMull, Question4ChurchNumeralPow}
import testsGenerator.htmlGen.Page

import scala.util.Random

object GenTest extends App{
//  println(ChurchNumeral(3))
//  println(ToPrettyString(ChurchNumeral(3)))
//  println((ChurchNumeral.sum(ChurchNumeral(2), ChurchNumeral(1))))
//  println(ToPrettyString(ChurchNumeral.sum(ChurchNumeral(2), ChurchNumeral(1))))

  val r = new Random(1)
  val variantCount = 10
  val questions:Seq[QuestionType] = Seq(
    Question1AlphaEquv,
    Question2ChurchNumeralSum,
    Question3ChurchNumeralMull,
    Question4ChurchNumeralPow)

  val variants:Seq[Variant] = (1 to variantCount).map(id => Variant.generate(id, r.nextInt(), questions))

  val res = Page.genPages(variants)


  val questFile = new PrintWriter(new File("out.html"), "UTF8")
  questFile.println(res.toString().replaceAll("<p>&nbsp;</p>", ""))
  questFile.close()

  val answFile = new PrintWriter(new File("answ.txt"), "UTF8")
  variants.foreach{ v=>
    answFile.println("######################################################################")
    answFile.println(v.title)
    v.withIndices.foreach{ case (i, question) =>
      answFile.println(i)
      answFile.println(question.answHtml)
    }
  }

  answFile.close()




  //  val q = Question1AlphaEquv.generate(1)
//
//  println(q.html)
//  println(q.answHtml)
}
