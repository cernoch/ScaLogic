package cernoch.scalogic

import tools.NameGen.ALPHABET
import scala.math.{BigInt, BigDecimal => BigDec}
import tools.{Labeler, Mef}

/**
 * Term is either a constant value, a function or a variable
 */
abstract sealed class Term(val dom:Domain[_]) {

  /**
   * Substitution algorithm
   * @param dict Mapping from replaced terms to their replacements
   * @return All subterms in map's keys replaced by the values
   */
  def subst
    (dict: Term => Option[Term])
  : Term
  = dict(this).getOrElse{ this match {
      case f:Fun => f.mefMap{_.subst(dict)}
      case fft => fft
    } }

  /**
   * All variables in this term
   * @return List of all subterms that are variables
   */
  def vars = this match {
    case _:Val[_] => List()
    case v:Var => List(v)
    case f:Fun => Term.vars(f.args)
  }

  /**
   * String representation of this term (using global variable names)
   *
   * Note that since all variables must keep unique names,
   * this function keeps their references forever.
   */
  override def toString = toString(Var.globalNames)

  /**
   * String representation of this term (using scoped variable names)
   *
   * {{{
   *   val x: Term = ...
   *   x.toString(new Labeler[Var,String](NameGen.ALPHABET))
   * }}}
   *
   * @param names Extendable mapping from variables to their names
   * @return
   */
  def toString(names: Labeler[Var,String]) : String = this match {
    case l:Val[_] => l.toString
    case r:Var => names(r)
    case f:Fun => f.name + f.args.mkString("(", ",", ")")
    case _ => super.toString
  }
}



object Term {

  /**
   * All variables in the list of terms
   * @return List of all subterms that are variables
   */
  def vars
    (l:List[Term])
  : List[Var]
  = l match {
      case Nil => List()
      case head :: tail => head.vars ++ vars(tail)
    }
}

/**
 * Function-free term
 */
abstract sealed class FFT(dom:Domain[_]) extends Term(dom) {}

/**
 * Variable can be substituted or unified
 */
final class Var(dom:Domain[_]) extends FFT(dom) {}

object Var {
  val globalNames = new Labeler[Var,String](ALPHABET)

  def apply[T<:Term](name:String, dom:Domain[_]) = globVars((name,dom))
  private val globVars = new Labeler[(String,Domain[_]),Var](d => {
    new Var(d._2)
  })

  def apply[T<:Term](d:Domain[_]) = new Var(d)
  def unapply(s:Var) = Some(s.dom)
}

/**
 * Function value is a symbol and a list of arguments
 */
final class Fun
	(val name: String, val args:List[Term], dom:Domain[_])
	extends Term(dom) {
  
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
  
  private[scalogic] def mefMap
    (f: Term => Term)
  = {
      val mappedArgs = Mef.map[Term,Term](args)(f)
      if (mappedArgs eq args)
        this else new Fun(name, mappedArgs, dom)
    }
}

object Fun {
  def apply
    (name:String, args:List[Term], dom:Domain[_])
  = new Fun(name, args, dom)
  
  def unapply(f:Fun)
  = Some((f.name,f.args,f.dom))
  
  object args {
    def unapplySeq(f:Fun) = Some(f.args)
  }
}

/**
 * Constant value
 */
abstract sealed class Val[+T]
    (dom:Domain[T]) extends FFT(dom) {
  
  def get: T

  override def toString = {if (get == null) "null" else get.toString} + dom.toString
  override def hashCode = (if (get == null) 0 else get.hashCode) + 7 * dom.hashCode
  override def equals(o:Any) = o match {
    case v:Val[_] => get == v.get && dom == v.dom
    case _ => false
  }
}

object Val {
  def apply(v:String, d:CatDom) = new Cat(v,d)
  def apply(v:String, d:String) = new Cat(v, CatDom(d))

  def apply(v:BigInt, d:NumDom) = new Num(v,d)
  def apply(v:Int,    d:NumDom) = new Num(BigInt(v),d)
  def apply(v:BigInt, d:String) = new Num(v,NumDom(d))
  def apply(v:Int,    d:String) = new Num(BigInt(d),NumDom(d))

  def apply(v:BigDec, d:DecDom) = new Dec(v,d)
  def apply(v:Double, d:DecDom) = new Dec(BigDec(v),d)
  def apply(v:BigDec, d:String) = new Dec(v,DecDom(d))
  def apply(v:Double, d:String) = new Dec(BigDec(v),DecDom(d))

  def unapply[T](v:Val[T]) = Some((v.get, v.dom))
}

final class Num(val v:BigInt, override val dom:NumDom) extends Val[BigInt](dom) { def get = v }
final class Dec(val v:BigDec, override val dom:DecDom) extends Val[BigDec](dom) { def get = v }
final class Cat(val v:String, override val dom:CatDom) extends Val[String](dom) {
  def get = v

  override def toString = super.toString match {
    case Cat.NO_QUOTE(s) => s
    case s => "'" + s.replace("'", "\\'") + "'"
  }
}

object Cat {
  def unapply(v:Cat) = Some(v.get)
  val NO_QUOTE = "[a-z][A-Z0-9_]*".r
}
object Num { def unapply(v:Num) = Some(v.get) }
object Dec { def unapply(v:Dec) = Some(v.get) }