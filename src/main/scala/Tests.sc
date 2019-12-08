import lambda.PreTerm._
import lambda._

LocallyNamelessRepresentation("x" ~> ("x" ^ ("y" ~> ("z" ^"x")))).toPreTerm

