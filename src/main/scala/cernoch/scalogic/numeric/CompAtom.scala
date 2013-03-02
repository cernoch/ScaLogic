package cernoch.scalogic.numeric

import cernoch.scalogic._
import tools.{Mef, Labeler}


/**
 *
 *
 * @author Radomír Černoch (radomir.cernoch at gmail.com)
 */
class LessThan
    [S <: Term]
    (val x: S, val y: S)
  extends Atom[S]("lessThan", List(x,y)) {

  /**
   * Translates all subterms using a dictionary
   */
  override def substitute
    (dict: Term => Option[Term])
  = {
    val sArg = Mef.map[Term,Term](args){_.substitute(dict)}
    if (sArg.eq(args)) this else new LessThan(sArg(0), sArg(1))
  }

  /**
   * Translates arguments using the dictionary
   */
  override def mapAllArgs
    [T <: Term]
    (dict: Term => T)
  = {
    val sArgs = args.map{dict(_)}
    new LessThan[T](sArgs(0),sArgs(1))
  }

  /**
   * Translates arguments that are found in the map
   */
  override def mapSomeArg
    [T >: S <: Term]
    (dict: Term => Option[T])
  = {
    val sArg = Mef.map[S,T](args){x => dict(x).getOrElse(x)}
    if (sArg.eq(args)) this else new LessThan(sArg(0), sArg(1))
  }

  override def toString(names: Labeler[Var, String])
  = x.toString(names) + " < " + y.toString(names);
}





class LessOrEq
    [S <: Term]
    (val x: S, val y: S)
  extends Atom[S]("lessOrEq", List(x,y)) {

  /**
   * Translates all subterms using a dictionary
   */
  override def substitute
  (dict: Term => Option[Term])
  = {
    val sArg = Mef.map[Term,Term](args){_.substitute(dict)}
    if (sArg.eq(args)) this else new LessOrEq(sArg(0), sArg(1))
  }

  /**
   * Translates arguments using the dictionary
   */
  override def mapAllArgs
  [T <: Term]
  (dict: Term => T)
  = {
    val sArgs = args.map{dict(_)}
    new LessOrEq[T](sArgs(0),sArgs(1))
  }

  /**
   * Translates arguments that are found in the map
   */
  override def mapSomeArg
  [T >: S <: Term]
  (dict: Term => Option[T])
  = {
    val sArg = Mef.map[S,T](args){x => dict(x).getOrElse(x)}
    if (sArg.eq(args)) this else new LessOrEq(sArg(0), sArg(1))
  }

  override def toString(names: Labeler[Var, String])
  = x.toString(names) + " <= " + y.toString(names);
}
