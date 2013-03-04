package cernoch.scalogic

import tools.Labeler
import collection.generic.Growable
import tools.StringUtils._

/**
 * Clause is a set or a list of positive and negative literals
 */
class Clause
	[H<: Iterable[Atom], B<: Iterable[Atom]]
	(val headAtoms: H, val bodyAtoms: B)
	extends HasVariables {

	override def hashCode = headAtoms.hashCode + 11 * bodyAtoms.hashCode
	override def equals(o:Any) = o match {
		case c:Clause[_,_] =>
			(headAtoms == c.headAtoms) &&
			(bodyAtoms == c.bodyAtoms)
		case _ => false
	}

	override def toString
	(sb: StringBuilder, names: Labeler[Var,String], short: Boolean)
  = {
		if (!headAtoms.isEmpty)
			(headAtoms, names, short) ::| " <- " into
				sb join (""" \/ """)

		if (!bodyAtoms.isEmpty)
			(bodyAtoms, names, short) into
				sb join (""" /\ """)

		sb append "."
	}

	private[scalogic] def addVarsTo
	(buffer: Growable[Var]) {
		headAtoms.foreach{_.addVarsTo(buffer)}
		bodyAtoms.foreach{_.addVarsTo(buffer)}
	}
}



/**
 * Horn clause has at most 1 head atom
 */
class Horn
	[B <: Iterable[Atom]]
	(val head: Atom, bodyAtoms: B)
	extends Clause[Iterable[Atom],B](
		if (head == null) Iterable() else Iterable(head),
		bodyAtoms
	)

object Horn {

	def apply
	[B<:Iterable[Atom]]
	(head: Atom, bodyAtoms: B)
	= new Horn(head, bodyAtoms)

	def apply[B<:Iterable[Atom]]
	(body: B) = new Horn(null, body)
  
	def unapply[B<:Iterable[Atom]]
	(c: Clause[Iterable[Atom],B])
	= c.headAtoms.size match {
		case 0 => Some(            null, c.bodyAtoms)
		case 1 => Some(c.headAtoms.head, c.bodyAtoms)
		case _ => None
	}
}


/**
 * Body-less clause has 1 head atom and 0 body atoms
 */
class BLC(head:Atom) extends Horn(head, Iterable())

object BLC {

  def apply(head: Atom) = new BLC(head)

  def unapply(c: Clause[Iterable[Atom],Iterable[Atom]])
  = if (c.headAtoms.size == 1 && c.bodyAtoms.size == 0)
      Some(c.headAtoms.iterator.next) else None
}
