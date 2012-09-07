package cernoch.scalogic

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AtomTest extends Specification {

  "Dictonary" should {

    "create a circular reference map" in {
      Dict fromList List(1,2,3)  must_== Map(1->2, 2->3, 3->1)
    }

    "work if list has 1 element" in {
      Dict fromList List("x")  must_== Map("x" -> "x")
    }

    "work if list has 2 elements" in {
      Dict fromList List('x', 'y')  must_== Map('x'->'y', 'y'->'x')
    }

    "fail on empty list" in {
      Dict fromList List() must throwAn[NoSuchElementException]
    }
  }

  val num1a = NumDom("num1")
  val num1b = NumDom("num1")

  val num2 = NumDom("num2")

  val x = Var(num1a)
  val y = Var(num1b)
  val z = Var(num2)

  val n1 = Val(1, num2)
  val n2 = Val(2, num2)

  val axyz = Atom("a", List(x,y,z))

  val ayz0p1 = Atom("a", List(y,z,n1))

  "Substitituion" should {
    "replace arbitrary items" in {
      axyz.subst(z,n1).subst(y,z).subst(x,y) must_== ayz0p1
    }
  }

  "Unification" should {
    "work and ignore occurs check" in {
      axyz unify ayz0p1 must_== Map(x->y, y->z, z->n1)
    }
  }

  // TODO: Test biased atom
}