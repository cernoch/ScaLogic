package cernoch.scalogic

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AtomTest extends Specification {

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

    def sa(x: (Term,Term)*)
    = (y:Term)
      => x.foldLeft(Map[Term,Term]())
        {(x,y) => x + y}.get(y)

    def sb(x: (Term,Term)*)
    = (y:Term)
      => x.foldLeft(Map[Term,Term]())
        {(x,y) => x + y}.get(y).get



    "replace arbitrary items" in {
      axyz.subst(sa(z -> n1, y -> z, x-> y)) must_== ayz0p1
    }

    "replace all arguments" in {
      axyz.sflat(sb(z -> n1, y -> z, x-> y)) must_== ayz0p1
    }
  }

  // TODO: Test biased atom
}