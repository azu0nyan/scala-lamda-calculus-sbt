package lambda

import lambda.LocallyNamelessRepresentation.LocallyNamelessRepresentedTerm
import lambda.PreTerm.PreTerm

case class LTerm(p: PreTerm) {

  lazy val locallyNamelessRepresentation: LocallyNamelessRepresentedTerm = LocallyNamelessRepresentation(p)

  lazy val hash: Int = locallyNamelessRepresentation.toString.##

  override def hashCode(): Int = hash

  override def equals(obj: Any): Boolean = obj match {
    case t:LTerm => p <=> t.p
    case _ => false
  }
}
