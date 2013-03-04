package cernoch.scalogic

import collection.mutable.ListBuffer
import collection.mutable

object `package` { pac =>
	implicit def atom2blc (a: Atom) = BLC(a)
	implicit def blc2atom (m: BLC)  = m.head

	implicit def atom2mode (a: Atom) = Mode(a)
	implicit def mode2atom (m: Mode) = m.atom

	def forEachOne[T](list:List[T])(f: T => T)
	: List[List[T]]
	= list match {
		case Nil => Nil
		case h::t => (f(h)::t) :: forEachOne(t)(f).map{h::_}
	}

	/** Carthesian product of lists */
	def cartesian[T]
	(l: List[List[T]])
	: List[List[T]]
	= l match {
		case Nil => List(Nil)
		case head :: tail =>
			for(xh <- head;
					xt <- cartesian(tail))
			yield xh :: xt
	}

	/** Creates a circular map from a list */
	def circularMap[T](head:T, l:List[T])
	: Map[T,T]
	= l match {
		case Nil => Map()
		case last :: Nil => Map(last -> head)
		case elem :: next :: tail
		=> circularMap(head, next :: tail) + (elem -> next)
	}

	/** All subsets */
	def subsets[T](l: Iterable[T]) = {
		val buf = ListBuffer(List[T]())
		for (i <- l) {
			buf ++= buf.map{i :: _}
			buf += List(i)
		}
		buf.toList
	}

	/** All equivalence relations */
	def partitions[T](l: Iterable[T]) = {
		val buf = ListBuffer[List[List[T]]]()
		for (i <- l) {
			val createdSepRel = buf.map{List(i) :: _}
			val addToExisting = buf.flatMap{forEachOne(_){i :: _}}
			buf ++= createdSepRel
			buf ++= addToExisting
		}
		buf.toList
	}

	/** Iterable tools */
	implicit def iterTools[T](l: Iterable[T]) = new Object {
		/** All equivalence relations */
		def partitions = pac.partitions(l)

		/** All subsets */
		def subsets = pac.subsets(l)
	}

	/** List tools */
	implicit def listTools[T](l: List[T]) = new Object {
		/** Creates a circular map from a list */
		def circularMap = l match {
			case h::t => pac.circularMap(h,t)
			case _ => throw new IllegalArgumentException("Iterable must not be empty!")
		}
	}

	/** List-of-lists tools */
	implicit def listListTools[T](l: List[List[T]]) = new Object {
		/** Cartesian product of lists */
		def cartesian = pac.cartesian(l)
	}
}
