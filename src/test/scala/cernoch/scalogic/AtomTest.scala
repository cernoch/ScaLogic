package cernoch.scalogic

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AtomTest extends Specification {

	val num1a = IntDom("num1")
	val num1b = IntDom("num1")

	val num2 = IntDom("num2")

	val x = Var(num1a)
	val y = Var(num1b)
	val z = Var(num2)

	val one = Val(1,num2)
	val two = Val(2,num2)

	"Atom" should {
		"subst arbitrary items" in {
			Atom("a",x,y,z) subst (x->z,z->x) must_== Atom("a",z,y,x)
		}
	}
}