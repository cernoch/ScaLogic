package cernoch.scalogic

import tools.Labeler
import tools.StringUtils._
import tools.NameGen.Alphabet
import collection.generic.Growable
import math.{BigDecimal => BigDec, BigInt}
import java.util.Date

/**
 * Term is either a constant value, a function or a variable
 */
abstract sealed class Term
	(val dom: Domain)
	extends HasVariables
	with Substituable[Term] {

	/**
	 * Substitution algorithm
	 *
	 * @param dict Mapping from replaced terms to their replacements
	 * @return All subterms in map's keys replaced by the values
	 */
	def subst(dict: Term => Option[Term])
	= dict(this).getOrElse(this)

  override def toString
	(sb: StringBuilder,
	 names: Labeler[Var,String],
	 short: Boolean) {
		if (!short)
			sb.append(":")
				.append(dom.name)
	}
}



/**
 * Function-free term
 */
sealed abstract class FFT(dom: Domain) extends Term(dom)

/**
 * Variable
 */
final class Var(dom: Domain)
	extends FFT(dom) {

	override def toString
	(sb: StringBuilder,
	 names: Labeler[Var,String],
	 short: Boolean) {
		sb.append(names(this))
		super.toString(sb, names, short)
	}

	private[scalogic] def addVarsTo
	(buffer: Growable[Var]) { buffer += this }
}

object Var {
	val globalNames = Labeler[Var](Alphabet)

	def apply(name:String, dom:Domain)
	: Var = globalNames.inv(name, Var(dom))

	def apply(d:Domain) = new Var(d)
}

/**
 * Function value is a symbol and a list of arguments
 */
final class Fun
	(val name: String, val args: List[Term], dom: Domain)
	extends Term(dom) with WithArgs[Fun] {

	override def toString
	(sb: StringBuilder,
	 names: Labeler[Var,String],
	 short: Boolean) {
		sb.append(name)
		super.toString(sb, names, short)
	}

	override def hashCode
	      = name.hashCode
	  + 7 * args.hashCode
	  + 31 * dom.hashCode

	override def equals(o:Any) = o match {
	  case f:Fun =>
	    name == f.name &&
	    args == f.args &&
	     dom == f.dom
	  case _ => false
	}

	override def subst
	(dict: Term => Option[Term])
	= dict(this).getOrElse(mapArgs(dict))

	def mapArgs
	(dict: (Term) => Option[Term])
	= {
		val sArgs = args.mapConserve{i => dict(i).getOrElse(i)}
		if (sArgs eq args) this else Fun(name, sArgs, dom)
	}
}

object Fun {
  def apply
  (name: String, args: List[Term], dom:Domain)
  = new Fun(name, args, dom)
  
  object args {
    def unapplySeq(f:Fun) = Some(f.args)
  }
}

/**
 * Constant value
 */
abstract class Val(dom: Domain) extends FFT(dom) {

	type Type<: Any
	val value : Type

	private[scalogic] def addVarsTo
	(buffer: Growable[Var]) {}

	override def toString
	(b: StringBuilder,
	 n: Labeler[Var,String],
	 s: Boolean) {
		b.append(ident(value))
		super.toString(b,n,s)
	}

	override def hashCode = dom.hashCode() +
		( if (value == null) 0 else value.hashCode() )


	override def equals(o:Any) = o match {
    case v:Val => value == v.value && dom == v.dom
    case _ => false
  }
}

object Val {
	def apply(v:String, d:Domain) = new Val(d) {
		type Type = String
		val value = v
	}

	def apply(v:Int,    d:Domain with Integral[Int]) = new IntVal(v,d)
	def apply(v:Long,   d:Domain with Integral[Long]) = new IntVal(v,d)
	def apply(v:Date,   d:Domain with Integral[Date]) = new IntVal(v,d)
	def apply(v:BigInt, d:Domain with Integral[BigInt]) = new IntVal(v,d)

	def apply(v:Float,  d:Domain with Fractional[Float]) = new DecVal(v,d)
	def apply(v:Double, d:Domain with Fractional[Double]) = new DecVal(v,d)
	def apply(v:BigDec, d:Domain with Fractional[BigDec]) = new DecVal(v,d)
}


object StrVal {

	def toStr(o:Any) = if (o == null) null else o.toString

	def unapply(v:Val)
	= v.dom match {
		case l:Iterable[_] => Some(toStr(v.value), l.view.map{toStr(_)})
		case _             => Some(toStr(v.value), Iterable())
	}
}


class IntVal[I]
	(val value:I, override val
	dom: Domain with Integral[I])
	extends Val(dom) {
	type Type = I
}

object IntVal {
	def unapply(t:Val)
	: Option[(I,Integral[I])] forSome {type I}
	= t match {
		case n:IntVal[_] => Some(n.value, n.dom)
		case _ => None
	}
}

class DecVal[F](val value:F, override val
	dom: Domain with Fractional[F])
	extends Val(dom) {
	type Type = F
}

object DecVal {
	def unapply(t:Val)
	: Option[(F,Fractional[F])] forSome {type F}
	= t match {
		case n:DecVal[_] => Some(n.value, n.dom)
		case _ => None
	}
}

object NumVal {

	private def cast(o:Any)
	= o.asInstanceOf[Option[(T,Numeric[T])] forSome {type T}]

	def unapply(t:Val)
	: Option[(T,Numeric[T])] forSome {type T}
	= t match {
		case DecVal(v,t) => cast(Some(v,t))
		case IntVal(v,t) => cast(Some(v,t))
		case _ => None
	}
}
