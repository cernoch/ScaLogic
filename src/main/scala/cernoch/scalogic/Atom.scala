package cernoch.scalogic

import tools._

/**
 * @author Radomir Cernoch (radomir.cernoch at gmail.com)
 */
class Atom
	(val pred: String, val args: List[Term])
	extends WithArgs[Atom]
	with Substituable[Atom] {

	def subst(dict: Term => Option[Term])
	= {
		val sArg = args.mapConserve{_.subst(dict)}
		if (sArg.eq(args)) this else Atom(pred,sArg)
	}

	def mapArgs(dict: Term => Option[Term])
	= {
		val sArg = args.mapConserve{x => dict(x).getOrElse(x)}
		if (sArg.eq(args)) this else Atom(pred,sArg)
	}

	override def toString
	(b: StringBuilder,
	 l: Labeler[Var,String],
	 s: Boolean) {

		args.size match {

			case 1 => {
				b.append("(")
				b.append(pred)
				b.append(" ")
				args(0).toString(b,l,s)
				b.append(")")
			}

			case 2 => {
				b.append("(")
				args(0).toString(b,l,s)
				b.append(" ")
				b.append(pred)
				b.append(" ")
				args(1).toString(b,l,s)
				b.append(")")
			}

			case _ => {
				b append pred
				super.toString(b,l,s)
			}
		}
	}

	override def hashCode()
	= pred.hashCode + 31 * args.hashCode

  override def equals(o:Any)
	= o match {
    case a:Atom =>
			(pred == a.pred) &&
			(args == a.args)
    case _ => false
  }
}

object Atom {

	def apply(p: String, a: List[Term])
	: Atom = new Atom(p,a)

	def apply(p: String, a: Term*)
	: Atom = Atom(p, a.toList)

	def unapply(a: Atom)
	= Some((a.pred,a.args))
}
