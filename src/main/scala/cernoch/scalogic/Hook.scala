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
	= (List(Map[Term,Term]()) :: (swappable.partitions.toList.map{
		_.filter{_.size > 1} // Partitions of size 1 do not cause term mapping
		 .map{_.permutations.toList}
		 .cartesian.filter{_.size > 0}
		 .map{_.foldLeft(Map[Term,Term]()){_ ++ _.circularMap}
		}})).toSet.flatten
		.flatMap{dict => atoms.view.map{_.subst{dict.get(_)}}}

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
