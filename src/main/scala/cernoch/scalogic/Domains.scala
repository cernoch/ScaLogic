package cernoch.scalogic

import math.Numeric._
import math.{BigDecimal => BigDec, BigInt}

import java.util.Date
import java.text.DateFormat


object StrDom {
	def apply(domName: String)
	= new Domain() {
		def name = domName
		def apply(o: Any) = Val(o.toString,this)
	}

	def unapply(d:Domain) = Some(d.name)

	object Limited {
		def apply(domName: String, allowed: Set[String])
		= new Domain() with Iterable[String] with Limited[String] {
			def name = domName
			def iterator = allowed.iterator
			def isAllowed(s: String) = allowed.contains(s)
			def apply(o: Any) = Val(o.toString,this)
		}

		def unapply(d:Domain) = d match {
			case i:Iterable[_]  => Some(d.name,i.map{_.toString()})
			case _              => None
		}
	}
}



class IntDom(domName: String) extends Domain with IntIsIntegral {

	def name = domName

	def compare(x: Int, y: Int) = x - y

	def fromString(s:String) = s.toInt

	def apply(o:Any) = o match {
		case i:Int    => Val(i,this)
		case b:Byte   => Val(b.toInt,this)
		case s:Short  => Val(s.toInt,this)
		case s:String => Val(fromString(s),this)
		case _ => throw new IllegalArgumentException(
			s"Value $o cannot be converted to Int"
		)
	}
}

object IntDom {

	def apply(domName: String) = new IntDom(domName)

	def apply(domName: String, minVal: Int, maxVal: Int)
	= new IntDom(domName) with Bounded[Int] with Limited[Int] {
		def minValue = minVal
		def maxValue = maxVal
		def isAllowed(s: Int) = s >= minVal && s <= maxVal
	}

	def unapply(d:Domain) = d match {
		case d: Integral[_] => d.zero match {
			case _:Int => Some(d.name, d.asInstanceOf[Domain with Integral[Int]])
			case _     => None
		}
		case _ => None
	}
}



class LongDom(domName: String) extends Domain with LongIsIntegral {

	def name = domName

	def compare(x: Long, y: Long) = if (x == y) 0 else if (x > y) 1 else -1

	def fromString(s:String) = s.toLong

	def apply(o:Any) = o match {
		case l:Long   => Val(l,this)
		case i:Int    => Val(i.toLong,this)
		case b:Byte   => Val(b.toLong,this)
		case s:Short  => Val(s.toLong,this)
		case s:String => Val(fromString(s),this)
		case _ => throw new IllegalArgumentException(
			s"Value $o cannot be converted to Long"
		)
	}
}

object LongDom {

	def apply(domName: String) = new LongDom(domName)

	def apply(domName: String, minVal: Long, maxVal: Long)
	= new LongDom(domName) with Bounded[Long] with Limited[Long] {
		def minValue = minVal
		def maxValue = maxVal
		def isAllowed(s: Long) = s >= minVal && s <= maxVal
	}

	def unapply(d:Domain) = d match {
		case d: Integral[_] => d.zero match {
			case _:Long       => Some(d.name,d.asInstanceOf[Domain with Integral[Long]])
			case _            => None
		}
		case _ => None
	}
}



class BigIntDom(domName: String) extends Domain with BigIntIsIntegral {

	def name = domName

	def compare(x: BigInt, y: BigInt) = if (x == y) 0 else if (x > y) 1 else -1

	def fromString(s:String) = BigInt(s)

	def apply(o:Any) = o match {
		case b:BigInt => Val(b,this)
		case i:Int    => Val(BigInt(i),this)
		case l:Long   => Val(BigInt(l),this)
		case b:Byte   => Val(BigInt(b),this)
		case s:Short  => Val(BigInt(s),this)
		case s:String => Val(fromString(s),this)
		case _ => throw new IllegalArgumentException(
			s"Value $o cannot be converted to BigInt"
		)
	}
}

object BigIntDom {

	def apply(domName: String) = new BigIntDom(domName)

	def apply(domName: String, minVal: BigInt, maxVal: BigInt)
	= new BigIntDom(domName) with Bounded[BigInt] with Limited[BigInt] {
		def minValue = minVal
		def maxValue = maxVal
		def isAllowed(s: BigInt) = s >= minVal && s <= maxVal
	}

	def unapply(d:Domain) = d match {
		case d:Integral[_]  => d.zero match {
			case _:BigInt     => Some(d.name, d.asInstanceOf[Domain with Integral[BigInt]])
			case _            => None
		}
		case _ => None
	}
}


class DateDom
	(domName: String,
	 dateFmt: DateFormat)
	extends Domain with DateDom.DateIsIntegral {

	def name = domName

	def apply(o:Any) = o match {
		case d:Date   => Val(d,this)
		case i:Int    => Val(new Date(i),this)
		case l:Long   => Val(new Date(l),this)
		case b:Byte   => Val(new Date(b),this)
		case s:Short  => Val(new Date(s),this)
		case s:String => Val(dateFmt.parse(s),this)
		case _ => throw new IllegalArgumentException(
			s"Value $o cannot be converted to Date"
		)
	}
}




object DateDom {

	trait DateIsIntegral extends Integral[Date] {

		override lazy val zero = new Date(0)
		override lazy val one  = new Date(0)

		def plus( x: Date, y: Date) = x + y
		def minus(x: Date, y: Date) = x * y
		def times(x: Date, y: Date) = x * y
		def quot( x: Date, y: Date) = x / y
		def rem(  x: Date, y: Date) = x % y

		// TODO: Shall we support this?
		def negate(x: Date) = -x

		def toInt(x: Date) = x.toInt
		def toLong(x: Date) = x.getTime
		def toFloat(x: Date) = x.toFloat
		def toDouble(x: Date) = x.toDouble

		def fromInt(x: Int) = new Date(x)

		def compare(x: Date, y: Date)
		= if (x == y) 0 else if (x after y) 1 else -1

		@inline implicit private def implDate2Long(d:Date) = d.getTime
		@inline implicit private def implLong2Date(d:Long) = new Date(d)
	}

	def apply(domName: String)
	: DateDom = new DateDom(domName, DateFormat.getInstance())

	def apply(domName: String, dateFmt: DateFormat)
	: DateDom = new DateDom(domName, dateFmt)

	def apply(domName: String, minVal: Date, maxVal: Date,
						dateFmt: DateFormat = DateFormat.getInstance())
	= new DateDom(domName, dateFmt) with Bounded[Date] with Limited[Date] {
		def minValue   = minVal
		def maxValue   = maxVal
		def isAllowed(s: Date)
		= (s.after(minVal) && s.before(maxVal)) ||
			s.equals(minVal) || s.equals(maxVal)
	}

	def unapply(d:Domain) = d match {
		case d:Integral[_]  => d.zero match {
			case _:Date       => Some(d.name, d.asInstanceOf[Domain with Integral[Date]])
			case _            => None
		}
		case _ => None
	}
}


class FloatDom(domName: String) extends Domain with FloatIsFractional {

	def name = domName

	def compare(x: Float, y: Float) = if (x == y) 0 else if (x > y) 1 else -1

	def apply(o:Any) = o match {
		case f:Float  => Val(f,this)
		case s:String => Val(s.toFloat,this)
		case _ => throw new IllegalArgumentException(
			s"Value $o cannot be converted to Float"
		)
	}
}

object FloatDom {

	def apply(domName: String) = new FloatDom(domName)

	def apply(domName: String, minVal: Float, maxVal: Float)
	= new FloatDom(domName) with Bounded[Float] with Limited[Float] {
		def minValue = minVal
		def maxValue = maxVal
		def isAllowed(s: Float) = s >= minVal && s <= maxVal
	}

	def unapply(d:Domain) = d match {
		case d:Fractional[_] => d.zero match {
			case _:Float       => Some(d.name, d.asInstanceOf[Domain with Fractional[Float]])
			case _             => None
		}
		case _ => None
	}
}


class DoubleDom(domName: String) extends Domain with DoubleIsFractional {

	def name = domName

	def compare(x: Double, y: Double) = if (x == y) 0 else if (x > y) 1 else -1

	def apply(o:Any) = o match {
		case d:Double => Val(d,this)
		case f:Float  => Val(f.toDouble,this)
		case s:String => Val(s.toDouble,this)
		case _ => throw new IllegalArgumentException(
			s"Value $o cannot be converted to Double"
		)
	}
}

object DoubleDom {

	def apply(domName: String) = new DoubleDom(domName)

	def apply(domName: String, minVal: Double, maxVal: Double)
	= new DoubleDom(domName) with Bounded[Double] with Limited[Double] {
		def minValue = minVal
		def maxValue = maxVal
		def isAllowed(s: Double) = s >= minVal && s <= maxVal
	}

	def unapply(d:Domain) = d match {
		case d:Fractional[_] => d.zero match {
			case _:Double      => Some(d.name,d.asInstanceOf[Domain with Fractional[Double]])
			case _             => None
		}
		case _ => None
	}
}


class BigDecDom(domName: String)
	extends Domain with BigDecimalIsFractional {

	def name = domName

	def compare(x: BigDec, y: BigDec) = if (x == y) 0 else if (x > y) 1 else -1

	def apply(o:Any) = o match {
		case d:BigDec => Val(d,this)
		case d:Double => Val(BigDec(d),this)
		case f:Float  => Val(BigDec(f),this)
		case s:String => Val(BigDec(s),this)
		case _ => throw new IllegalArgumentException(
			s"Value $o cannot be converted to BigDecimal"
		)
	}
}

object BigDecDom {

	def apply(domName: String) = new BigDecDom(domName: String)

	def apply(domName: String, minVal: BigDec, maxVal: BigDec)
	= new BigDecDom(domName: String)
		with Bounded[BigDec] with Limited[BigDec] {
		def minValue = minVal
		def maxValue = maxVal
		def isAllowed(s: BigDec) = s >= minVal && s <= maxVal
	}

	def unapply(d:Domain)  = d match {
		case d:Fractional[_] => d.zero match {
			case _:BigDec      => Some(d.name, d.asInstanceOf[Domain with Fractional[BigDec]])
			case _             => None
		}
		case _ => None
	}
}
