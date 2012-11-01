package cernoch.scalogic

import tools.{Mef, CircRef}


trait Hook {

  def substitute
    (dict: Term => Option[Term])
  : Hook

  def mapArgs
    (dict: Term => Option[Term])
  : Hook

  def equivs
    (s: Set[Btom[Term]])
  : Set[Btom[Term]]
  = s

  def minSucc = 0

  def maxSucc: Option[Int] = None
}


object Determined extends Hook {
  override def substitute(dict: Term => Option[Term]) = this
  override def mapArgs(dict: Term => Option[Term]) = this

  override def minSucc = 1
}


object Functional extends Hook {
  override def substitute(dict: Term => Option[Term]) = this
  override def mapArgs(dict: Term => Option[Term]) = this

  override def maxSucc = Some(1)
}


class Permutable
    (swappable: Set[Term])
  extends Hook {

  override def substitute
    (dict: Term => Option[Term])
  = {
    val sSwaps = Mef.map(swappable){_.substitute(dict)}
    if (sSwaps == swappable) this else new Permutable(swappable)
  }

  override def mapArgs(dict: Term => Option[Term])
  = {
    val sSwaps = Mef.map(swappable){x => dict(x).getOrElse(x)}
    if (sSwaps == swappable) this else new Permutable(swappable)
  } 

  override def equivs
    (s: Set[Btom[Term]])
  = swappable
    .toList.permutations
    .map { CircRef(_) }
    .map { map => (x: Term) => map.get(x) }
    .map { dict => s.map { _.substitute(dict) } }
    .flatten.toSet

  override def hashCode = swappable.hashCode()

  override def toString = "Permutable(" + swappable + ")"
}


class ForceNonEq(
    disjoint: Set[Term],
    possible: Boolean = true)
  extends Hook {

  private def derive(newDisj: Set[Term])
  = {
    val sizeFix = disjoint.size == newDisj.size
    if (newDisj == disjoint && sizeFix)
      this else new ForceNonEq(newDisj, sizeFix)
  }
  
  override def substitute
    (dict: Term => Option[Term])
  = derive(Mef.map(disjoint){_.substitute(dict)})


  override def mapArgs
    (dict: Term => Option[Term])
  = derive(Mef.map(disjoint){x => dict(x).getOrElse(x)})

  override def maxSucc = if (possible) None else Some(0)

  override def hashCode
  = (if (possible) 1 else -1) * disjoint.hashCode()

  override def toString
  = "ForceNonEq" +
    (if (possible) "" else "[tooLate]") +
    "(" + disjoint.toString + ")"
}
