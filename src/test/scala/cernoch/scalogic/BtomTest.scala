package cernoch.scalogic

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class BtomTest extends Specification {

  val dom = NumDom("dom")

  val x = Var(dom)
  val y = Var(dom)
  val z = Var(dom)

  val axyz = Atom("a", x,y,z)

  "Parser" should {

    "parse basic syntax" in {

      val atom = Btom("atom(+dom, -dom)", Set(dom,dom))

      val v1 = atom.args(0)
      val v2 = atom.args(1)

      (atom.pred must_== "atom") and
        (atom.modeIn must_== Set(v1)) and
        (atom.hooks must_== Set())
    }

    "understand functional tag" in {
      val atom = Btom("atom{func}()", Set(dom))

      (atom.maxSucc must_== Some(1)) and
        (atom.hooks.contains(Functional) must_== true)
    }

    "understand non-unifiability" in {

      val atom = Btom(
        "atom{fneq=1:2}(+dom, -dom)",
        Set(dom,dom))

      val v1 = atom.args(0)
      val v2 = atom.args(1)

      val atom2 = atom.substitute(Dict(v1 -> v2).get)

      atom.satisfiable && !atom2.satisfiable
    }

    "understand determined atoms" in {
      val atom = Btom("atom{det}()", Set())

      (atom.minSucc must_== 1) and
      (atom.hooks.contains(Determined) must_== true)
    }

    "understand permutability" in {
      val atom = Btom(
        "atom{perm=1:2}(+dom, -dom)",
        Set(dom))

      val v1 = atom.args(0)
      val v2 = atom.args(1)

      val atom2 = atom.substitute(Dict(v1 -> v2, v2 -> v1).get)

      atom.equivalents.contains(atom2)
    }
  }
}