import lambda.PreTerm._

package object lambda {




  object Vars extends Vars

  trait Vars {


    val x: Variable = "x"
    val y: Variable = "y"
    val z: Variable = "z"
    val u: Variable = "u"
    val w: Variable = "w"
  }

}
