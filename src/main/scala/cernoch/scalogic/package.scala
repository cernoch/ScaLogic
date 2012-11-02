package cernoch.scalogic

object `package` {

  implicit def convertAtomToBLC
    [T<:Term]
    (a: Atom[T])
  = BLC(a)
  
  implicit def convertBLCtoAtom
    [T<:Term]
    (a: Atom[T])
  = a.head

  object Dict {
    def apply[T <: Term](x: (Term,T)*) = x.toMap
  }
}
