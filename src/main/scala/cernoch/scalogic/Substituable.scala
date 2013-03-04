package cernoch.scalogic

private[scalogic] trait Substituable[T] {

	/**
	 * Substitution algorithm
	 *
	 * @param dict Maps replaced terms to replacements
	 * @return Subterms replaced by the values
	 */
	def subst(dict: Term => Option[Term]): T

	def subst(dict: (Term, Term)*) : T
	= subst((Map() ++ dict).get(_))
}
