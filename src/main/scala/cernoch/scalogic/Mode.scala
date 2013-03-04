package cernoch.scalogic

class Mode(
		val atom: Atom,
		val iVar: Set[Var],
		val hook: Set[Hook])
	extends Substituable[Mode] {

	def equivs = hook
		.foldLeft(Set(atom)){
			(a,b) => b.equivs(a)}

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
}

object Mode {
	def apply(atom: Atom,
		iVar: Set[Var] = Set())
	= new Mode(atom, iVar, Set())
}
