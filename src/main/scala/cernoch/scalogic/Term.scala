package cernoch.scalogic

import tools.Labeler
import tools.StringUtils._
import tools.NameGen.Alphabet
import collection.generic.Growable
import math.{BigDecimal => BigDec, BigInt}

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
	= dict(this).getOrElse({
		val mapArgs = args.mapConserve{_.subst(dict)}
		if (mapArgs eq args) this else Fun(name, mapArgs, dom)
	})

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
class Val
	(val value: Any, dom: Domain)
	extends FFT(dom) {

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
	def apply(v:String, d:Domain) = new Cat(v,d)
	def apply(v:String, d:Domain with Ordering[String]) = new Ord(v,d)

	def apply(v:Int,    d:Domain with Integral[Int]) = new Num(v,d)
	def apply(v:Long,   d:Domain with Integral[Long]) = new Num(v,d)
	def apply(v:BigInt, d:Domain with Integral[BigInt]) = new Num(v,d)

	def apply(v:Float,  d:Domain with Fractional[Float]) = new Dec(v,d)
	def apply(v:Double, d:Domain with Fractional[Double]) = new Dec(v,d)
	def apply(v:BigDec, d:Domain with Fractional[BigDec]) = new Dec(v,d)
}

class Cat[T](s: T, d: Domain) extends Val(s,d) { def get = Option(s) }
class Ord[T](s: T, override val dom: Domain with Ordering[T]) extends Cat[T](s,dom)
class Num[T](s: T, override val dom: Domain with Integral[T]) extends Val(s,dom) { def get = Option(s) }
class Dec[T](s: T, override val dom: Domain with Fractional[T]) extends Val(s,dom) { def get = Option(s) }
