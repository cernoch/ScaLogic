package cernoch.scalogic

/**
 * Clause is a set or a list of literals
 */
class Clause
  [+Head <: Atom[Term],
   +Body <: Iterable[Atom[Term]]]
  (val head: Head, val body: Body) {

  def vars = Term.vars(head.args) ++
    body.foldLeft( List[Var]() ){ (a,b) => Term.vars(b.args) ++ a }
  
  override def toString
  = head.toString +
    ( if (body.isEmpty) ""
      else " <- " + body.map{_.toString}.reduceLeft{_ + ", " + _}
    ) + "."

  override def hashCode = head.hashCode + 3 * body.hashCode
  override def equals(o:Any) = o match {
    case c:Clause[_,_] => (head == c.head) && (body == c.body)
    case r:AnyRef => this.eq(r)
    case _ => false
  }
}

object Clause {
  def appply[H<:Atom[Term], T<:Iterable[Atom[Term]]](h:H,i:T) = new Clause(h,i)
  
  def unapply[H<:Atom[Term], T<:Iterable[Atom[Term]]]
    (c:Clause[H,T]) = Some((c.head, c.body))
}

class BLC[+H<:Atom[Term]](head:H)
	extends Clause[H,Iterable[Nothing]](head, List()) {

  override def hashCode = head.hashCode
  override def equals(o:Any) = o match {
    case c:BLC[_] => head == c.head
    case r:AnyRef => this.eq(r)
    case _ => false
  }  
}

object BLC {
  def   apply[H<:Atom[Term]](h:H) = new BLC(h)
  def unapply[H<:Atom[Term]](c:BLC[H]) = Some(c.head)
}