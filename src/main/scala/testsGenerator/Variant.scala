package testsGenerator

import scala.util.Random

object Variant {
  def generate(id:Int, seed:Int, questions:Seq[QuestionType]):Variant = {
    val r = new Random(seed)
    Variant(id, seed, questions.map(qt => qt.generate(r.nextInt())))
  }
}

case class Variant(id:Int, seed:Int, questions:Seq[Question]){
  def title:String = s"Вариант $id seed $id"

  def withIndices:Seq[(Int, Question)] = questions.zipWithIndex
    .map{_.swap}
    .map{case (i, q) => (i + 1, q)}
}
