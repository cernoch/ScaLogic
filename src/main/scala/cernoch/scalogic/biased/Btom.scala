package cernoch.scalogic.biased

import cernoch.scalogic._
import tools.{Labeler, Mef}

class Btom[+S<:Term](
    pred: String,
    args: List[S],
    val hooks: Set[Hook],
    val modeIn: Set[Term])
  extends Atom[S](pred, args) {

  override def subst
    (dict: Term => Option[Term])
  : Btom[Term]
  = {
    val sArgs = Mef.map[Term,Term](args){_.subst(dict)}
    val sHooks = Mef.map(hooks){_.subst(dict)}
    val sModeIn = Mef.map(modeIn){_.subst(dict)}

    if (args == sArgs && hooks == sHooks && modeIn == sModeIn)
      this else new Btom[Term](pred, sArgs, sHooks, sModeIn)
  }

  def sflat
    [T <: Term]
    (dict: Term => Option[T])
  : Btom[T]
  = {
    new Btom[T](pred,
      args.map{dict(_).get},
      hooks.map{_.subst(dict)},
      modeIn.map{_.subst(dict)})
  }



  def satisfiable = maxSucc.getOrElse(1) >= 1

  def minSucc = hooks.foldLeft(0){(min,hook) => scala.math.max(min, hook.minSucc)}
  def maxSucc = hooks.foldLeft(None.asInstanceOf[Option[Int]]){
    (valSoFar,hook) => ((valSoFar,hook.maxSucc)) match {
      case (Some(x), Some(y)) => Some(scala.math.min(x,y))
      case (None,    Some(x)) => Some(x)
      case (Some(x), None)    => Some(x)
      case (None,    None)    => None
    }
  }

  def equivalents
  = hooks.foldLeft( Set[Btom[Term]](this) )
      { (eqSoFar,hook) => hook.equivs(eqSoFar) }



  override def toString() = toString(Var.globalNames)
  override def toString(names: Labeler[Var,String]) =
    pred + args.map{ arg =>
        (if (modeIn.contains(arg)) "+" else "-") +
          arg.toString(names) }
      .mkString("(", ",", ")")

  override def hashCode = modeIn.hashCode + 17 * super.hashCode
  override def equals(o:Any) = o match {
    case a:Btom[_] => pred == a.pred && args == a.args && modeIn == a.modeIn
    case a:Atom[_] => pred == a.pred && args == a.args
    case _ => false
  }
}
