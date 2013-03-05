package cernoch.scalogic

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ToolsTest extends Specification {

  "Circular reference" should {

    "create a circular reference map" in {
      List(1,2,3).circularMap  must_== Map(1->2, 2->3, 3->1)
    }

    "work if list has 1 element" in {
      List("x").circularMap must_== Map("x" -> "x")
    }

    "work if list has 2 elements" in {
      List('x','y').circularMap must_== Map('x'->'y', 'y'->'x')
    }

    "fail on empty list" in {
      Nil.circularMap must throwAn[NoSuchElementException]
    }
  }

	"Subsets" should {
		"list all subsets" in {
			List(1,2).subsets.toSet must_==
				Set(List(1), List(2), List(2,1))
		}
	}

	"Equivalence relations" should {
		"list all of them" in {
			List(1,2).partitions
				.map{_.map{_.toSet}.toSet}.toSet must_==
				Set(
					Set(Set(1),Set(2)),
					Set(Set(1,2))
				)
		}
	}

	"Cartesian product" should {
		"work well on lists" in {
			List( List(1),List(2,3) ).cartesian must_==
				List(List(1,2), List(1,3))
		}
	}
}
