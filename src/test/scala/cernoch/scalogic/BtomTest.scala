package cernoch.scalogic

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BtomTest extends Specification {

  val num1a = NumDom("num1")
  val num1b = NumDom("num1")

  val num2 = NumDom("num2")

  val x = Var(num1a)
  val y = Var(num1b)
  val z = Var(num2)

  val n1 = Val(1, num2)
  val n2 = Val(2, num2)

  val axyz = Atom("a", List(x, y, z))

  val ayz0p1 = Atom("a", List(y, z, n1))

  "Parser" should {

    "parse basic syntax" in {

      val atom = Btom("atom(+num1, -num2)", Set(num1a, num2))

      val v1 = atom.args(0)
      val v2 = atom.args(1)

      (atom.pred must_== "atom") and
        (atom.modeIn must_== Set(v1)) and
        (atom.hooks must_== Set())
    }

    "understand functional tag" in {
      val atom = Btom("atom{func}()", Set(num2))

      (atom.maxSucc must_== Some(1)) and
        (atom.hooks.contains(Functional) must_== true)
    }

    "understand non-unifiability" in {

      val atom = Btom(
        "atom{fneq=1:2}(+num1, -num2)",
        Set(num1a, num2))

      val v1 = atom.args(0)
      val v2 = atom.args(1)

      val atom2 = atom.subst(
        Map[Term, Term](v1 -> v2).get)

      atom.satisfiable && !atom2.satisfiable
    }

    "understand determined atoms" in {
      val atom = Btom("atom{det}()", Set())

      (atom.minSucc must_== 1) and
        (atom.hooks.contains(Determined) must_== true)
    }

    "understand permutability" in {
      val atom = Btom(
        "atom{perm=1:2}(+num1, -num2)",
        Set(num1a, num2))

      val v1 = atom.args(0)
      val v2 = atom.args(1)

      val atom2 = atom.subst(
        Map[Term, Term](
          v1 -> v2, v2 -> v1
        ).get)

      atom.equivalents.contains(atom2)
    }
  }
}