package cernoch.scalogic

import tools.NameGen.ALPHABET
import scala.math.{BigInt, BigDecimal => BigDec}
import tools.{Labeler, Mef}

/**
 * Term is either a constant value, a function or a variable
 */
abstract sealed class Term(val dom:Domain[_]) {
  
  def subst[T<:Term](u:Var[T], v:T) : Term = this match {

    case _:Val[_] => this

    case w:Var[T] => if (u == this) v else this

    case Fun(name,args,dom) => {
      val uAr = Term.subst(args, u, v)
      if (uAr.eq(args)) this else new Fun(name, uAr, dom)
    }
  }
  
  def matching(dictionary: Map[Term,Term]) : Term = {
    dictionary.get(this).getOrElse{ this match {
      case Fun(name,args,dom) =>
        new Fun( name,
          args.map{_.matching(dictionary)}.toList,
          dom)
      case x => x      
    }}
  }

  def unify(t:Term)
  : Option[Map[Var[Term],Term]]
  = Term.unify(this,t) match {
    case Some(x) => Some(x._1)
    case None => None
  }

  def vars = this match {
    case   Val(_,_)   => List()
    case v@Var(_)     => List(v)
    case f@Fun(_,_,_) => Term.vars(f.args)
  }

  override def toString = toString(Var.globalNames)
  def toString(names: Labeler[Var[_],String]) : String = this match {
    case l@Val(_,_) => l.toString
    case r@Var( _ ) => names(r)
    case Fun(name,args,dom) => name + args.mkString("(", ",", ")")
    case _ => super.toString
  }
}

object Term {

  def unapply(t:Term) = Some()

  def vars(l:List[Term]) : List[Var[Term]] = l match {
    case Nil => List()
    case head :: tail => head.vars ++ vars(tail)
  }

  def subst[T<:Term](list:List[Term], u:Var[T], v:T)
  : List[Term]
  = Mef.map[Term,Term](list){_.subst(u,v)}

  val NOUNI = Map[Var[Term],Term]()

  def unify(a:Term, b:Term)
  : Option[(Map[Var[Term],Term], Map[Var[Term],Term])]
  = (a,b) match {

    case (a@Val(_,_), b@Val(_,_)) => if (a == b) Some((NOUNI,NOUNI)) else None
    case (a@Var(_),   b@Var(_))   => Some(Map(a -> b), Map(b -> a))

    case (a@Var(_), b) => Some(Map(a -> b), NOUNI)
    case (a, b@Var(_)) => Some(NOUNI, Map(b -> a))

    case (Fun(aName,aArgs,aDom), Fun(bName,bArgs,bDom))
      => if (aName == bName) Term.unify(aArgs, bArgs) else None

    case _ => None
  }


  def unify(a:List[Term], b:List[Term])
  : Option[(Map[Var[Term],Term], Map[Var[Term],Term])]
  = (a,b) match {
    case (h1 :: t1, h2 :: t2) =>
      (unify(h1,h2) , unify(t1,t2)) match {
        case ( Some((a1,b1)) , Some((a2,b2)) ) =>
          (compat(a1,a2) && compat(b1,b2)) match {
            case true => Some((a1 ++ a2, b1 ++ b2))
            case _ => None
          }
        case _ => None
      }
    case (Nil,Nil) => Some(NOUNI,NOUNI)
    case _ => None // Lists do not have equal length
  }

  private def compat(
    a:Map[Var[Term],Term],
    b:Map[Var[Term],Term])
  = ((a keySet) intersect (b keySet))
    .foldLeft(true){
      (bool,war) => bool &&
        a.get(war).get == b.get(war).get
    }
}

/**
 * Function-free term
 */
abstract sealed class FFT(dom:Domain[_]) extends Term(dom) {}

/**
 * Variable can be substituted or unified
 */
final class Var[+S<:Term](dom:Domain[_]) extends FFT(dom) {}

object Var {
  val globalNames = new Labeler[Var[_],String](ALPHABET)

  def apply[T<:Term](name:String, dom:Domain[_]) = globVars((name,dom))
  private val globVars = new Labeler[(String,Domain[_]),Var[Term]](d => {
    new Var[Term](d._2)
  })

  def apply[T<:Term](d:Domain[_]) = new Var[T](d)
  def unapply(s:Var[Term]) = Some(s.dom)
}

/**
 * Function value is a symbol and a list of arguments
 */
final class Fun[+S<:Term]
	(val name: String, val args:List[S], dom:Domain[_])
	extends Term(dom) {
  
  override def hashCode
        = name.hashCode
    + 7 * args.hashCode
    + 31 * dom.hashCode
    
  override def equals(o:Any) = o match {
    case Fun(oName,oArgs,oDom) =>
      name == oName &&
      args == oArgs &&
       dom == oDom
    case _ => false
  }
}

object Fun {
  def apply[S<:Term]
    (name:String, args:List[S], dom:Domain[_])
  = new Fun(name, args, dom)
  
  def unapply[T<:Term](f:Fun[T])
  = Some((f.name,f.args,f.dom))
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
    case Val(oVal, oDom) => get == oVal && dom == oDom
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
    case s => "'" + s + "'"
  }
}

object Cat {
  def unapply(v:Cat) = Some(v.get)
  val NO_QUOTE = "[a-z][A-Z0-9_]*".r
}
object Num { def unapply(v:Num) = Some(v.get) }
object Dec { def unapply(v:Dec) = Some(v.get) }