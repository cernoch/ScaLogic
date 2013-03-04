package cernoch.scalogic

import math.Numeric._
import collection.SortedSet

trait Domain {

	def name: String

	override def toString = name
	override def hashCode() = name.hashCode()
	override def equals(o: Any) = o match {
		case d:Domain => name == d.name
		case _ => false
	}
}

trait IsKey extends Domain {
	def isKey: Boolean

	override def toString()
	= (if (isKey) "@" else "&") + super.toString
}

trait Limited[S] {
	def isAllowed(s: S): Boolean
}

trait Bounded[S] {
	def minValue: S
	def maxValue: S
}

object Domain {
	def apply(domName: String)
	= new Domain {
		def name = domName
	}

	def int(n: String)
	= new Domain with IntIsIntegral {
		def compare(x: Int, y: Int): Int = x - y
		def name: String = n
	}

	def int(domName: String, minVal: Int, maxVal: Int)
	= new Domain with IntIsIntegral
		with Bounded[Int] with Limited[Int] {
		def name = domName
		def minValue = minVal
		def maxValue = maxVal
		def compare(x: Int, y: Int) = x - y
		def isAllowed(s: Int)
		= s >= minVal && s <= maxVal
	}

	def long(domName: String)
	= new Domain with LongIsIntegral {
		def compare(x: Long, y: Long)
		= if (x == y) 0 else if (x > y) 1 else -1
		def name: String = domName
	}

	def long(domName: String, minVal: Long, maxVal: Long)
	= new Domain with LongIsIntegral
		with Bounded[Long] with Limited[Long] {
		def name = domName
		def minValue = minVal
		def maxValue = maxVal
		def isAllowed(s: Long)
		= s >= minVal && s <= maxVal
		def compare(x: Long, y: Long)
		= if (x == y) 0 else if (x > y) 1 else -1
	}

	def bigNum(domName: String)
	= new Domain with BigIntIsIntegral {
		def name: String = domName
		def compare(x: BigInt, y: BigInt)
		= if (x == y) 0 else if (x > y) 1 else -1
	}

	def bigNum(domName: String, minVal: BigInt, maxVal: BigInt)
	= new Domain with BigIntIsIntegral
		with Bounded[BigInt] with Limited[BigInt] {
		def name = domName
		def minValue = minVal
		def maxValue = maxVal
		def compare(x: BigInt, y: BigInt)
		= if (x == y) 0 else if (x > y) 1 else -1
		def isAllowed(s: BigInt)
		= s >= minVal && s <= maxVal
	}

	def float(domName: String)
	= new Domain with DoubleIsFractional {
		def name = domName
		def compare(x: Double, y: Double)
		= if (x == y) 0 else if (x > y) 1 else -1
	}

	def float
	(domName: String, minVal: Float, maxVal: Float)
	= new Domain with FloatIsFractional
		with Bounded[Float] with Limited[Float] {
		def name = domName
		def minValue = minVal
		def maxValue = maxVal
		def compare(x: Float, y: Float)
		= if (x == y) 0 else if (x > y) 1 else -1
		def isAllowed(s: Float)
		= s >= minVal && s <= maxVal
	}

	def dec(domName: String)
	= new Domain with DoubleIsFractional {
		def name = domName
		def compare(x: Double, y: Double)
		= if (x == y) 0 else if (x > y) 1 else -1
	}

	def dec
	(domName: String, minVal: Double, maxVal: Double)
	= new Domain with DoubleIsFractional
		with Bounded[Double] with Limited[Double] {
		def name = domName
		def minValue = minVal
		def maxValue = maxVal
		def compare(x: Double, y: Double)
		= if (x == y) 0 else if (x > y) 1 else -1
		def isAllowed(s: Double)
		= s >= minVal && s <= maxVal
	}


	def bigDec(domName: String)
	= new Domain with DoubleIsFractional {
		def name = domName
		def compare(x: Double, y: Double)
		= if (x == y) 0 else if (x > y) 1 else -1
	}

	def bigDec
	(domName: String, minVal: BigDecimal, maxVal: BigDecimal)
	= new Domain with BigDecimalIsFractional
		with Bounded[BigDecimal] with Limited[BigDecimal] {
		def name = domName
		def minValue = minVal
		def maxValue = maxVal
		def compare(x: BigDecimal, y: BigDecimal)
		= if (x == y) 0 else if (x > y) 1 else -1
		def isAllowed(s: BigDecimal)
		= s >= minVal && s <= maxVal
	}

	def cat(domName: String)
	= new Domain {
		def name = domName
	}

	def cat(domName: String, allowed: Set[String])
	= new Domain with Iterable[String] with Limited[String] {
		def name = domName
		def iterator = allowed.iterator
		def isAllowed(s: String)
		= allowed.contains(s)
	}

	def ord(domName: String, allowed: SortedSet[String])
	= new Domain with Iterable[String] with Ordering[String]
		with Limited[String] with Bounded[String] {
		def name = domName
		def minValue = allowed.firstKey
		def maxValue = allowed.lastKey
		def iterator = allowed.iterator
		def isAllowed(s: String)
		= allowed.contains(s)
		def compare(x: String, y: String)
		= allowed.compare(x,y)
	}
}