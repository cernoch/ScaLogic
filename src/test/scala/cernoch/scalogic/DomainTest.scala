package cernoch.scalogic

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DomainTest extends Specification {

  val dec = DecDom("dec")
  val num = NumDom("num")
  val cat = CatDom("cat")
  
  val dec2 = DecDom("dec")

  val decadic = DecDom("decadic")
  val numeric = NumDom("numeric")

  "Domains of different name" should {
    "not be equal even in the type agrees" in {
         (dec must_!= decadic) and (num must_!= numeric)
    }
  }



  "Domains of the same name" should {
    "equal if their names equal" in {
      dec must_== dec2
    }
  }



  "Numeric domain" should {
    "parse integers" in {
      num.valueOf("1") must_== BigInt(1)
      num.valueOf("100") must_== BigInt(100)
      num.valueOf("-11") must_== BigInt(-11)
    }

    "return null on empty string" in {
      num.valueOf("") must_== null
    }

    "fail on decimal values" in {
      num.valueOf("1.1") must throwAn[NumberFormatException]
    }

    "fail on alphabetic string" in {
      num.valueOf("hello") must throwAn[NumberFormatException]
    }
  }



  "Decimal domain" should {

    "parse floats" in {
      dec.valueOf("1.1") must_== BigDecimal(1.1)
    }

    "parse integers as floats" in {
      dec.valueOf("1") must_== BigDecimal(1)
    }

    "return null on alphabetic values" in {
      dec.valueOf("hello") must throwAn[NumberFormatException]
    }
  }



  "Categorical domain" can {

    "parse any string" in {
      cat valueOf "ahoj lidi" must_== "ahoj lidi"
    }

    "parse empty string as null" in {
      cat valueOf "" must_== null
    }
  }
}