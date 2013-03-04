package cernoch.scalogic

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ModeTest extends Specification {

  val d = Domain.int("num1")

  val x = Var(d)
  val y = Var(d)
  val z = Var(d)

  val one = Val(1,d)
  val two = Val(2,d)

	val axyz = Atom("a", x,y,z)

	"Functional atom" should {
		"have 1 instantiation at most" in {
			Mode(axyz).functional
				.maxSucc must_== Some(1)
		}
	}

	"Determined atom" should {
		"have 1 instantiation at least" in {
			Mode(axyz).determined.hook.map{_.minSucc}.foreach(println)
			Mode(axyz).determined.minSucc must_== 1
		}
	}

	"Permutable atom" should {
		"iterate over all permutations" in {
			Mode(axyz).permutable(x,y,z)
				.equivs must_== Set(
					Atom("a", x,y,z),
					Atom("a", y,z,x),
					Atom("a", z,x,y),
					Atom("a", z,y,x),
					Atom("a", y,x,z),
					Atom("a", x,z,y)
				)
		}
	}

	"Determined atom" should {
		"have 1 instantiation at most" in {
			Mode(axyz).forceNonEq(x,y)
				.subst(x -> y)
				.maxSucc must_== Some(1)
		}
	}
}