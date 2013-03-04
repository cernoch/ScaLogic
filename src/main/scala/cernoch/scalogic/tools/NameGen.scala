package cernoch.scalogic.tools

import scala.collection.IndexedSeqOptimized
import scala.collection.mutable.ArraySeq

class NameGen
	(alphabet:IndexedSeqOptimized[String,_])
	extends Iterable[String] {

	def iterator = new Iterator[String] {
		private var i = -1

		def hasNext = i < Int.MaxValue;
		def next = {
			val l = alphabet.length
			def fold(i:Int) : StringBuilder
			= (i/l match {
				case 0 => new StringBuilder()
				case x => fold(x-1)
			}).append(alphabet.apply(i%l))

			i = i+1
			fold(i).toString()
		}
	}
}

object NameGen {

	val Alphabet = new NameGen(ArraySeq( ('A' to 'Z').map(_.toString) :_* ))

	val AlphaNum = new NameGen(ArraySeq( (('A' to 'Z') ++ ('0' to '9')).map(_.toString) :_* ))

	/*
	def iter2tran[T](i:Iterable[T])
	: (Any => T) = iter2tran(i.iterator)

	def iter2tran[T](i:Iterator[T])
	: (Any => T) = new (Any => T) {
		def apply(a:Any) : T
		= i.hasNext match {
			case true => i.next()
			case false => throw new
					StackOverflowError(
						"The iterator is too small")
		}
	}*/
}
