package cernoch.scalogic

import tools._

/**
 *
 * @param args
 * 
 * @author Radomir Cernoch (radomir.cernoch at gmail.com)
 */
class Atom
  [+S <: Term]
  (val pred: String, val args: List[S]) {

  def subst
    (dict: Term => Option[Term])
  = {
    val sArg = Mef.map[Term,Term](args){_.subst(dict)}
    if (sArg.eq(args)) this else new Atom[Term](pred,sArg)
  }

  /**
   * Flat (non-recursive) substitution
   *
   * @param dict
   * @tparam T
   * @return
   */
  def sflat
    [T <: Term]
    (dict: S => T)
  = new Atom[T](pred, args.map{dict(_)})

  def vars = Term.vars(args)

  override def toString() = toString(Var.globalNames)
  def toString(names: Labeler[Var,String]) =
    pred + args.map{_.toString(names)}.mkString("(", ",", ")")

  override def hashCode = pred.hashCode + 31 * args.hashCode
  override def equals(o:Any) = o match {
    case a:Atom[_] => pred == a.pred && args == a.args
    case _ => false
  }  
}

object Atom {
  def apply[T<:Term](p:String, a:List[T]) = new Atom[T](p,a)
  def unapply[T<:Term](a:Atom[T]) = Some(a.pred, a.args)
}
