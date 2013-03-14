package cernoch.scalogic

import collection.generic.Growable
import tools.Labeler

class Mode(
		val atom: Atom,
		val iVar: Set[Var],
		val hook: Set[Hook])
	extends Substituable[Mode]
	with HasVariables {

	def equivs
	= hook.foldLeft(Set(atom)){_ ++ _.equivs(Set(atom))}

	def minSucc = hook
		.map{_.minSucc}
		.foldLeft(0){math.max(_,_)}

	def maxSucc
	= hook.map{_.maxSucc}
		.collect{case Some(v) => v}
		.foldLeft[Option[Int]](None){ (out,v) =>
			Some(math.min(v, out getOrElse v)) }

	def subst
	(dict: (Term) => Option[Term])
	= new Mode(
		atom.subst(dict),
		iVar.map(dict).collect{case Some(v: Var) => v},
		hook.map{_.subst(dict)})


	def determined = new Mode(
		atom, iVar, hook + Determined)

	def functional = new Mode(
		atom, iVar, hook + Functional)

	def permutable
	(a: Var, b:Var, c: Var*)
	= new Mode(atom, iVar,
		hook + new Permutable(a :: b :: c.toList) )

	def forceNonEq
	(a: Var, b:Var, c: Var*)
	= new Mode(atom, iVar,
		hook + new ForceNonEq(a :: b :: c.toList) )

	private[scalogic] def addVarsTo
	(buffer: Growable[Var])
	= atom.addVarsTo(buffer)

	override def toString
	(sb: StringBuilder,
	 names: Labeler[Var,String],
	 short: Boolean) {

		sb.append("Mode{")

		if (iVar.size > 0) {
			sb.append(" in=[")

			var first = true
			for (v <- iVar) {
				if (!first) sb.append(",")
				v.toString(sb,names,short)
				first = false
			}
			sb.append("]")
		}

		sb.append(" atom = (")
		atom.toString(sb, names, short)
		sb.append(")")

		if (hook.size > 0) {
			sb.append(" hooks = [")
			var first = true
			for (h <- hook) {
				if (!first) sb.append(",")
				h.toString(sb,names,short)
				first = false
			}
			sb.append("]")
		}
		sb.append("}")
	}
}

object Mode {
	def apply(atom: Atom,
		iVar: Set[Var] = Set())
	= new Mode(atom, iVar, Set())
}
