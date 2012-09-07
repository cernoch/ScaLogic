package cernoch.scalogic.tools

import scala.collection.IndexedSeqOptimized
import scala.collection.mutable.ArraySeq
import scala.collection.immutable.HashMap

class Labeler[A,L](f: A => L) extends Function[A,L] { self =>

  def this(i:Iterable[L]) = this(NameGen.iter2tran(i))
  def this(i:Iterator[L]) = this(NameGen.iter2tran(i))

  var map = HashMap[A,L]()
  def get(a:A) = map.get(a)

  def apply(a:A)
  = map.get(a).getOrElse(
    self.synchronized {
      val label = f(a)
      map = map + ((a,label))
      label
    }
  )
}

object Labeler {
  def alphabet[T] = new Labeler[T,String](NameGen.ALPHABET)
  def alphanum[T] = new Labeler[T,String](NameGen.ALPHANUM)
}

class NameGen(alphabet:IndexedSeqOptimized[String,_])
	extends Iterable[String] {
  
  def iterator = new Iterator[String] {

    var i = -1
  
    def hasNext = i < Int.MaxValue;
    def next = {
      val l = alphabet.length
      def conv(i:Int) = ('A'.toInt + i).toChar 
      def fold(i:Int) : StringBuilder = (i/l match {
        case 0 => new StringBuilder()
        case x => fold(x-1)
      }).append(alphabet.apply(i%l))
    
      i = i+1
      fold(i).toString
    }
  }
}

object NameGen {
  
  val ALPHABET = new NameGen(ArraySeq[String](
    ('A' to 'Z').map(_.toString) :_*
  ))

  val ALPHANUM = new NameGen(ArraySeq[String](
    (('A' to 'Z') ++ ('0' to '9')).map(_.toString) :_*
  ))

  def iter2tran[T](i:Iterable[T]) : (Any => T) = new Object {
    val y = i.iterator
    def f(a:Any) : T = y.hasNext match {
      case true => y.next()
      case false => throw new StackOverflowError("The iterator is too small")
    }
  }.f

  def iter2tran[T](i:Iterator[T]) : (Any => T) = new Object {
    def f(a:Any) : T = i.hasNext match {
      case true => i.next()
      case false => throw new StackOverflowError("The iterator is too small")
    }
  }.f
}