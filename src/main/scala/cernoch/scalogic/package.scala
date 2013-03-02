package cernoch.scalogic

object `package` {

  implicit def atom2blc [T<:Term] (a: Atom[T]) = BLC(a)
  
  implicit def blc2atom [A<:Atom[Term]] (a: BLC[A]) = a.head

  object Dict {
    def apply[T <: Term](x: (Term,T)*) = x.toMap
  }
}
