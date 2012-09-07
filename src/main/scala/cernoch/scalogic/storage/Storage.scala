package cernoch.scalogic.storage

import cernoch.scalogic._
import collection.immutable.Set

trait Dumpable
    [C<:Clause[Atom[Term],
      Iterable[Atom[Term]]]] {

  def dump: Iterable[C]

}

trait Queriable[H<:Atom[Term], B<:Iterable[Atom[Term]]] {

  def histQuery[V<:Term](c:Clause[H,B], q:Var[V]) : Iterable[V]
}

trait SchemaAware[C<:Clause[Atom[Term],
                   Iterable[Atom[Term]]]] {

  private var _schema:  Set[C] = null
  private var _domain: Set[Domain[_]] = null

  private var varCache: Map[C, List[Var[_]]] = Map();

  def schemaVars(c:C)
    = varCache.get(c)
      .getOrElse(c.vars)

  def schema = _schema
  def schema_=(c:Set[C]) = {
    _schema = c
    varCache = schema.map{t => t -> t.vars}.toMap
  }

  def domain = _domain
  def domain_=(c:Set[Domain[_]]) = _domain = c
}
