package cernoch.scalogic

import collection.mutable.ArrayBuffer
import tools.Labeler
import tools.StringUtils.mkStringIfNonEmpty

/**
 * Clause is a set or a list of positive and negative literals
 */
class Clause
  [+Head <: Iterable[Atom[Term]],
   +Body <: Iterable[Atom[Term]]]
  (val headAtoms: Head, val bodyAtoms: Body) {

  override def hashCode = headAtoms.hashCode + 3 * bodyAtoms.hashCode
  override def equals(o:Any) = o match {
    case c:Clause[_,_] => (headAtoms == c.headAtoms) &&
                          (bodyAtoms == c.bodyAtoms)
    case r:AnyRef => this.eq(r)
    case _ => false
  }

  def toShort(): String = toShort(Var.globalNames)
  override def toString() = toString(Var.globalNames)

  def toString(names: Labeler[Var,String])
  = headAtoms.map{_.toString(names)}.mkString(" \\/ ") +
    mkStringIfNonEmpty(
      bodyAtoms.view.map{_.toString(names)})(
      " <- ", " /\\ ", "") +
    "."

  def toShort(names: Labeler[Var,String])
  = headAtoms.map{_.toShort(names)}.mkString(" \\/ ") +
    mkStringIfNonEmpty(
      bodyAtoms.view.map{_.toShort(names)})(
      " <- ", " /\\ ", "") +
    "."

  def variables
  = {

    val buf = new ArrayBuffer[Var]()

    def termVars(t: Term)
    : Unit
    = t match {
      case _:Val[_] =>
      case v:Var => buf += v
      case f:Fun => f.args.foreach(termVars)
    }

    headAtoms.foreach(_.args.foreach(termVars))
    bodyAtoms.foreach(_.args.foreach(termVars))

    buf.toList
  }
}

/**
 * Horn clause has at most 1 head atom
 */
class Horn
  [+Head <: Atom[Term],
   +Body <: Iterable[Atom[Term]]]
  (val head: Head, body: Body)
    extends
      Clause[Iterable[Head], Body](
        if (head eq null) Iterable() else Iterable(head),
        body
      )
  {}

object Horn {

  def appply
    [H<:Atom[Term],
     B<:Iterable[Atom[Term]]]
    (head: H,
     body: B)
  = new Horn(head, body)
  
  def apply
    [B<:Iterable[Atom[Term]]]
    (body: B)
  = new Horn(null, body)
  
  def unapply
    [H<:Atom[Term],
     T<:Iterable[Atom[Term]]]
    (c: Clause[Iterable[H],T])
  = c.headAtoms.size match {
      case 0 => Some(            null, c.bodyAtoms)
      case 1 => Some(c.headAtoms.head, c.bodyAtoms)
      case _ => None
    }
}


/**
 * Body-less clause has 1 head atom and 0 body atoms
 */
class BLC
    [+H<:Atom[Term]]
    (head:H)
	extends Horn[H, Iterable[Nothing] ](head, Iterable()){ }

object BLC {

  def apply
    [H <: Atom[Term]]
    (head: H)
  = new BLC(head)

  def unapply
    [H <: Atom[Term]]
    (c: Clause[Iterable[H],Iterable[Atom[Term]]])
  = if (c.headAtoms.size == 1 && c.bodyAtoms.size == 0)
      Some(c.headAtoms.iterator.next) else None
}
