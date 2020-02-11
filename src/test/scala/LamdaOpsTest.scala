
import lambda.PreTerm._
import lambda._
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.AppendedClues._
import Substitution._

class LamdaOpsTest extends AnyFunSuite with Vars {
  test("ChurchNumeralsAddition") {
    val x10 = ChurchNumeral(10)
    val x5 = ChurchNumeral(5)
    val sum = ChurchNumeral.sum(x10, x5)
    println(sum)
    println(Normalization(sum))
//    assert(Normalization(sum) <=> Normalization(ChurchNumeral(15)))
    assert(sum <<>> ChurchNumeral(15))
  }
  test("ChurchNumeralsAddition2") {
    for (i <- 0 to 10;
         j <- 0 to 10
         ) {
      println(s"sum $i $j")
      assert(ChurchNumeral.sum(ChurchNumeral(i), ChurchNumeral(j)) <<>> ChurchNumeral(i + j))
    }
  }

  test("ChurchNumeralsProduct") {
    for (i <- 0 to 10;
         j <- 0 to 10
         ) {
      println(s"prod $i $j")
      assert(ChurchNumeral.product(ChurchNumeral(i), ChurchNumeral(j)) <<>> ChurchNumeral(i * j))
    }
  }

  test("ChurchNumeralsPower") {
    def pow(x:Int, y:Int):Int = if(y == 0) 1 else x * pow(x, y - 1 )
    for (i <- 0 to 5;
         j <- 1 to 5
         ) {
      println(s"pow $i $j")
      assert(ChurchNumeral.power(ChurchNumeral(i), ChurchNumeral(j)) <<>> ChurchNumeral(pow(i, j)))
    }
  }

  test("projections"){
    import Logic._
    println(pair(x, y) ^ proj1)

    assert((pair(x, y) ^ proj1) <<>> x )
    assert((pair(x, y) ^ proj2) <<>> y )
  }

  test("projections n "){
    import Logic._
    for( total <- 1 to 6;
         elem <- 1 to total){
      val vars = Alphabet.variables.take(total)
      val tuple = Logic.tupleN(vars)
      assert((tuple ^ Logic.projN(elem, total)) <<>> vars(elem - 1))
    }

  }

}
