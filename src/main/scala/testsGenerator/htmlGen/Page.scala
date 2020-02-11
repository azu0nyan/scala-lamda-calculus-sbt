package testsGenerator.htmlGen

import scalatags.Text.all._
import testsGenerator.{Question, Variant}

object Page {
  def genPage(innerHtml:Frag ) = html(
    head(
      meta(charset := "UTF-8"),
      raw(
        s"""
           |<style>
           |.left_margin{
           | margin:2px;
           | padding:2px;
           |         }
           |         </style>
           |""".stripMargin
      )
    ),
    body(
      innerHtml
    )
  )

  def genPages(variants:Seq[Variant]):Frag =
    genPage(
      variants.map(v => genPageFromQuestions(v.title, v.withIndices))
    )


  def genPageFromQuestions(title:String, questions:Seq[(Int, Question)]):Frag =
    div(
      style := "break-after:page"
    )(
      h2(title),
      questions.map(q => genQuestionHtml(q._1.toString, q._2.html ))
    )


  def genQuestionHtml(number:String, inner:String) =
    div(
      display:= "flex",
      borderStyle := "solid",
      padding := "10px",
      lineHeight := 1,
      margin := "10px")(
      h2(float := "left", margin := "10px")(number + ")"),
      div(float := "left" ,flex := 1)(
        raw(inner)
      )
    )

}
