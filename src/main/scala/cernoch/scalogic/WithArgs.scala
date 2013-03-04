package cernoch.scalogic

import tools.Labeler
import collection.generic.Growable
import tools.StringUtils._

trait WithArgs[Self]
	extends HasVariables {

	def args: List[Term]

	/**
	 * Translates arguments
	 */
	def mapArgs(dict: Term => Option[Term]): Self

	/**
	 * String representation of this term (using scoped variable names)
	 * */
	override def toString
	(sb: StringBuilder,
	 names: Labeler[Var,String],
	 short: Boolean) {
		"("|:: (args, names, short) ::|")" into sb join ", "
		super.toString(sb, names, short)
	}

	private[scalogic] def addVarsTo
	(buffer: Growable[Var]) {
		args.foreach{_.addVarsTo(buffer)}
	}
}
