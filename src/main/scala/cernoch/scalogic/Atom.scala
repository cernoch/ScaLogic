package cernoch.scalogic

import tools._

/**
 *
 *
 * @param args
 * 
 * @author Radomir Cernoch (radomir.cernoch at gmail.com)
 */
class Atom
  [+S <: Term]
  (val pred: String, val args: List[S]) {

  /**
   * Translates all subterms using a dictionary
   */
  def substitute
    (dict: Term => Option[Term])
  = {
    val sArg = Mef.map[Term,Term](args){_.substitute(dict)}
    if (sArg.eq(args)) this else new Atom[Term](pred,sArg)
  }

  /**
   * Translates arguments using the dictionary
   */
  def mapAllArgs
    [T <: Term]
    (dict: Term => T)
  = Atom(pred, args.map{dict(_)})

  /**
   * Translates arguments that are found in the map
   */
  def mapSomeArg
    [T >: S <: Term]
    (dict: Term => Option[T])
  = {
    val sArg = Mef.map[S,T](args){x => dict(x).getOrElse(x)}
    if (sArg.eq(args)) this else new Atom[T](pred,sArg)
  }


  def variables = Term.variables(args)

  override def toString() = toString(Var.globalNames)

  def toString(names: Labeler[Var,String])
  = pred + StringUtils.mkStringIfNonEmpty(
      args.map{_.toString(names)} )( "( ",", ",")" )

  override def hashCode = pred.hashCode + 31 * args.hashCode
  override def equals(o:Any) = o match {
    case a:Atom[_] => pred == a.pred && args == a.args
    case _ => false
  }  
}

object Atom {

  def apply
    [T <: Term]
    (p: String, a: List[T])
  : Atom[T]
  = new Atom[T](p,a)

  def apply
    [T <: Term]
    (p: String, a: T*)
  : Atom[T]
  = Atom(p, a.toList)

  def unapply
    [T <: Term]
    (a: Atom[T])
  = Some(a.pred, a.args)
}
