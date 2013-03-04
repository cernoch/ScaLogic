package cernoch.scalogic.tools

import NameGen._
import scala.collection.mutable.HashMap

class Labeler[A,L]
	(names: Iterator[L])
	extends Function[A,L]
{ self =>

	val map = HashMap[A,L]()

	def inv(l: L, a: => A)
	= self.synchronized {
			map.find(_ == l) match {
				case Some((k,v)) => k
				case None => { map.put(a,l); a }
			}
		}

	def apply(a:A)
	= map.get(a).getOrElse(
		self.synchronized {
			val lab = names.next()
			map.put(a,lab)
			lab
		}
	)
}

@Deprecated
object Labeler {

	def apply[L](i:Iterable[String])
	= new Labeler[L,String](i.iterator)

	def alphabet[T] = Labeler[T](Alphabet)
	def alphaNum[T] = Labeler[T](AlphaNum)
}
