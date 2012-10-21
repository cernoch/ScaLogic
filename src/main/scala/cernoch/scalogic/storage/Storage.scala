package cernoch.scalogic.storage

import cernoch.scalogic._
import collection.immutable.Set

trait Dumpable
    [C<:Clause[Atom[Term],
      Iterable[Atom[Term]]] ]
{
  def dump: Iterable[C]
}

trait Queriable
    [H<:Atom[Term]
    ,C<:Clause[H,Iterable[Atom[Term]]] ]
{
  def query
    (c: C)
  : Iterable[H]
}

trait Writeable
    [C<:Clause[Atom[Term],
      Iterable[Atom[Term]]] ]
{
  def put(c: C)
}

trait Transactioned
    [S, C<:Clause[Atom[Term],
         Iterable[Atom[Term]]] ]
{
  def open: S
  def reset: Importer

  trait Importer
      extends Writeable[C]
  {
    def close: S
  }
}

trait SchemaAware[C<:Clause[Atom[Term],
                   Iterable[Atom[Term]]]]
{
  var schema: Set[C]
}
