package cernoch.scalogic

import collection.mutable.ListBuffer
import tools.Labeler
import collection.generic.Growable

trait HasVariables {

	/**
	 * All vars in this term
	 * @return List of all subterms that are vars
	 */
	def vars = {
		val b = new ListBuffer[Var]()
		addVarsTo(b)
		b.toList
	}

	private[scalogic] def addVarsTo
	(buffer: Growable[Var])

	/**
	 * String representation (using global variable names)
	 *
	 * As all vars must keep unique names,
	 * this function keeps their references forever
	 * and may lead to "memory leaks".
	 */
	override def toString() = toString(true)

	/**
	 * Short representation (using global variable names)
	 *
	 * As all vars must keep unique names,
	 * this function keeps their references forever
	 * and may lead to "memory leaks".
	 */
	def toString(short: Boolean)
	: String = {
		val sb = new StringBuilder()
		toString(sb, Var.globalNames, short)
		sb.toString()
	}

	/** String representation of this term (using scoped variable names) */
	def toString
	(sb: StringBuilder,
	 names: Labeler[Var,String],
	 short: Boolean) {}
}
