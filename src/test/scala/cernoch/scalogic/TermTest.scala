package cernoch.scalogic

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TermTest extends Specification {

	"Variable" should {
		val domain = IntDom("d")
		val x = Var(domain)

		"be equal for equal instances" in {
			x must_== x
		}

		"not be equal equality for different instances" in {
			Var(domain) must_!= Var(domain)
		}

		"has only 1 variable (itself)" in {
			x.vars must_== List(x)
		}
	}



	"Value" should {

		val o1a = Val(1,IntDom("d1"))
		val o1b = Val(1,IntDom("d1"))
		val o2  = Val(1,IntDom("d2"))
		val o3  = Val(0.0,DoubleDom("d2"))

		val vals = List(o1a,o1b,o2,o3)

		"match numeric values in general" in {
			(o1a match {

				case NumVal(value, numeric) => { import numeric._
					value * (one + one)
				}
				case _ => null

			}) must_== 2
		}

		"match integral and decimal values" in {
			vals.map(_ match {
				case IntVal(value,numeric) => numeric.toInt(value)
				case DecVal(value,decimal) => decimal.toInt(value)
				case _ => null
			}) must_== List(1,1,1,0)
		}

		"be equal if values come from the same domain" in {
			o1a must_== o1b
		}

		"not be equal if of different domain" in {
			o1a must_!= o2
		}

		"has no variables" in {
			o2.vars must_== List()
		}
	}


	"Functions" should {

		"have many deeply nested variables" in {
			Fun("g", List(fx,fx), dom).vars must_== List(x,x)
		}
	}


	val dom = IntDom("d")

	val x = Var(dom)
	val o = Val(1, dom)
	val fx = Fun("f",List(x),dom)
	val fy = Fun("f",List(Var(dom)),dom)

	"Substitution" should {
		"replace a variable with the value" in {
			x subst (x -> o) must_== o
		}

		"replace a variable in a function" in {
			val fo = Fun("f", List(o), dom)

			fx subst (x -> o) must_== fo
		}

		"replace whole function with a value" in {
			fx subst (fx -> o) must_== o
		}

		"treat different vars differently" in {
			val fy = Fun("f",List(Var(dom)), dom)

			fx subst (fy -> o) must_== fx
		}

		"avoid occurs check" in {
			val ffx = Fun("f", List(fx), dom)

			fx subst (x -> fx) must_== ffx
		}
	}
}