package cernoch.scalogic.tools

object CircRef {
  def apply
    [T]
    (l: List[T])
  = {

    def internal(head:T, l:List[T])
    : Map[T,T]
    = l match {
      case Nil => Map()
      case last :: Nil => Map(last -> head)
      case elem :: next :: tail => internal(head, next :: tail) + (elem -> next)
    }

    l match {
      case Nil => throw new NoSuchElementException("List must have at least 1 value")
      case h :: _ => internal(h,l)
    }

  }
}

