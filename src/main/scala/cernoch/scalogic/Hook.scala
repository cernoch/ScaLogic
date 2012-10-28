package cernoch.scalogic

import tools.CircRef

trait Hook {

  def subst
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
  def subst(dict: Term => Option[Term]) = this

  override def minSucc = 1
}


object Functional extends Hook {
  def subst(dict: Term => Option[Term]) = this

  override def maxSucc = Some(1)
}


class Permutable
(swappable: Set[Term])
  extends Hook {

  override def subst
  (dict: Term => Option[Term])
  = new Permutable(swappable.map {
    _.subst(dict)
  })

  override def equivs
  (s: Set[Btom[Term]])
  = swappable
    .toList.permutations
    .map {
    CircRef(_)
  }
    .map {
    map => (x: Term) => map.get(x)
  }
    .map {
    dict => s.map {
      _.subst(dict)
    }
  }
    .flatten.toSet

  override def hashCode = swappable.hashCode()

  override def toString = "Permutable(" + swappable + ")"
}


class ForceNonEq(
                  disjoint: Set[Term],
                  possible: Boolean = true)
  extends Hook {

  override def subst
  (dict: Term => Option[Term])
  = {
    val substed = disjoint.map {
      _.subst(dict)
    }
    new ForceNonEq(substed,
      disjoint.size == substed.size)
  }

  override def maxSucc = if (possible) None else Some(0)

  override def hashCode
  = (if (possible) 1 else -1) * disjoint.hashCode()

  override def toString
  = "ForceNonEq" +
    (if (possible) "" else "[tooLate]") +
    "(" + disjoint.toString + ")"
}
