package cernoch.scalogic.tools

/**
 * Memory efficient routines
 */
object Mef {

  def map
    [A<:AnyRef, B>:A<:AnyRef]
    (l:List[A])
    (f:A=>B)
  : List[B]
  = l match {
    case Nil => l
    case head :: tail => {
      val tran = f(head)
      val rest = map[A, B](tail)(f)
      if (head.eq(tran) && tail.eq(rest))
        l else tran :: rest
    }.asInstanceOf[List[B]]
  }

  def map
    [A<:AnyRef]
    (s:Set[A])
    (f:A=>A)
  : Set[A]
  = {
    var eq = true;
    val out = for (a <- s) yield {
      val b = f(a)
      eq = eq && a.eq(b)
      b
    }
    if (eq) s else out
  }

  def mapChk
    [A<:AnyRef,C]
    (s:Set[A])
    (f:A=>A)
    (chk:(A,A)=>Boolean)
  : (Set[A], Boolean)
  = {
    var eq = true;
    var ok = true;
    val out = for (a <- s) yield {
      val b = f(a)
      eq = eq && a.eq(b)
      ok = ok && chk(a,b)
      b
    }
    if (eq) (s,ok) else (out,ok)
  }
}