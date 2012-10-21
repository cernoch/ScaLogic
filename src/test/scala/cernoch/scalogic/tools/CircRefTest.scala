package cernoch.scalogic.tools

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CircRefTest extends Specification {

  "Circular reference" should {

    "create a circular reference map" in {
      CircRef(List(1,2,3))  must_== Map(1->2, 2->3, 3->1)
    }

    "work if list has 1 element" in {
      CircRef(List("x")) must_== Map("x" -> "x")
    }

    "work if list has 2 elements" in {
      CircRef(List('x', 'y')) must_== Map('x'->'y', 'y'->'x')
    }

    "fail on empty list" in {
      CircRef(List()) must throwAn[NoSuchElementException]
    }
  }
}
