package lambda

import lambda.PreTerm.PreTerm

object AllRecursiveFunctionsAreLDefinable {

  import lambda.Vars._;

  //  1.5.15. Definition. The class of recursive functions is the smallest class of
  //numeric functions containing the initial functions
  //(i) projections: U i m (n 1 , . . . , n m ) = n i for all 1 ≤ i ≤ m;
  def U(i: Int, m: Int): PreTerm = Logic.projN(i, m)

  //(ii) successor: S + (n) = n + 1;
  val Splus: PreTerm = x ~> (s ~> (z ~> (s ^ ((x ^ s) ^ z))))
  //(iii) zero: Z(n) = 0
  val Z:PreTerm = x ~>ChurchNumeral(0)
  //

      //todo
//  and closed under composition, primitive recursion, and minimization:
//    (i) composition: if g : N k → N and h 1 , . . . , h k : N m → N are recursive,
//  then so is f : N m → N defined by

//    f (n 1 , . . . , n m ) = g(h 1 (n 1 , . . . , n m ), . . . , h k (n 1 , . . . , n m )).
//  (ii) primitive recursion: if g : N m → N and h : N m+2 → N are recursive,
//  then so is f : N m+1 → N defined by
//    f (0, n 1 , . . . , n m )
//  = g(n 1 , . . . , n m );
//  f (n + 1, n 1 , . . . , n m ) = h(f (n, n 1 , . . . , n m ), n, n 1 , . . . , n m ).
//  (iii) minimization: if g : N m+1 → N is recursive and for all n 1 , . . . , n m there
//  is an n such that g(n, n 1 , . . . , n m ) = 0, then f : N m → N defined as
//    follows is also recursive 3
//  f (n 1 , . . . , n m ) = μn.g(n, n 1 , . . . , n m ) = 0


}
