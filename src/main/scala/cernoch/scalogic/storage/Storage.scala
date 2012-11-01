package cernoch.scalogic.storage

import cernoch.scalogic._
import collection.immutable.Set

trait Dumpable
    [C<:Clause[Iterable[Atom[Term]],
               Iterable[Atom[Term]]] ]
{
  def dump: Iterable[C]
}

trait Queriable
    [C<:Clause[Iterable[Atom[Term]],
               Iterable[Atom[Term]]]
    ,Out<:Term ]
{
  def query
    (c: C)
  : Iterable[Map[Var,Out]]
}

trait Writeable
    [C<:Clause[Iterable[Atom[Term]],
               Iterable[Atom[Term]]] ]
{
  def put(c: C)
}

trait Transactioned
    [S, C<:Clause[Iterable[Atom[Term]],
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

trait SchemaAware[C<:Clause[Iterable[Atom[Term]],
                            Iterable[Atom[Term]]]]
{
  var schema: Set[C]
}
