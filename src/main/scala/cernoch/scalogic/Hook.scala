package cernoch.scalogic

import tools.StringUtils._

trait Hook extends Substituable[Hook] {

  def equivs(s: Set[Atom]): Set[Atom] = s

  def minSucc = 0
  def maxSucc: Option[Int] = None
}


object Determined extends Hook {
  override def subst(dict: Term => Option[Term]) = this
  override def minSucc = 1
}


object Functional extends Hook {
  override def subst(dict: Term => Option[Term]) = this
  override def maxSucc = Some(1)
}


class Permutable
	(swappable: List[Term])
  extends Hook {

  override def subst
    (dict: Term => Option[Term])
  = {
    val sSwaps = swappable.mapConserve{_.subst(dict)}
    if (sSwaps == swappable) this else new Permutable(swappable)
  }

	override def equivs(atoms: Set[Atom])
	= swappable.partitions.flatMap{_.map{
		_.permutations.toList.cartesian
			.foldLeft(Map[Term,Term]()){_ ++ _.circularMap}
		}}
		.flatMap{dict => atoms.view.map{_.subst{dict.get(_)}}}.toSet

  override def hashCode = swappable.hashCode()

  override def toString = "Permutable(" + swappable + ")"
}


class ForceNonEq
	(disjoint: List[Term])
  extends Hook {

  private def derive
	(newDisj: List[Term])
  = if (newDisj == disjoint)
      this else new ForceNonEq(newDisj)

	private def possible
	= disjoint.distinct.size == disjoint.size

  override def subst
    (dict: Term => Option[Term])
  = derive(disjoint.mapConserve{_.subst(dict)})

  override def maxSucc
	= if (possible) None else Some(0)

  override def hashCode
  = disjoint.hashCode()

  override def toString
  = "ForceNonEq" +
    (if (possible) "" else "[tooLate]") +
    ("("|:: disjoint.map{_.toString()} ::| ")" join ",")
}
