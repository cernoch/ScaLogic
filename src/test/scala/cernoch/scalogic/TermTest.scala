package cernoch.scalogic

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TermTest extends Specification {

  val num1a = Domain.int("num1")
  val num1b = Domain.int("num1")

  val num2 = Domain.int("num2")

  val dec = Domain.dec("dec")

  val x = Var(dec)
  val y = Var(dec)
  
  "Two different vars" should {
    "not be equal" in {
      x must_!= y
    }
  }
  
  val o = Val(1, num1a)
  val o1 = Val(1, num1b)
  val o2 = Val(1, num2)

  "Two equal values" should {
    "be equal if from the same domain" in {
      o must_== o1
    }

    "not be equal if of different domain" in {
      o must_!= o2
    }
  }

  val fx = Fun("f",List(x), num2)

  val fo = Fun("f",List(o), num2)

  def sa(x: (Term,Term)*)
  = (y:Term)
    => x.foldLeft(Map[Term,Term]())
        {(x,y) => x + y}.get(y)

  val fy = Fun("f",List(y), num2)
  val ffx = Fun("f", List(fx), num2)

  "Substitution" should {
    "replace a variable with the value" in {
      x subst (x -> o) must_== o
    }

    "replace a variable in a function" in {
      fx subst (x -> o) must_== fo
    }

    "replace whole function with a value" in {
      fx subst (fx -> o) must_== o
    }

    "treat different vars differently" in {
      fx subst (fy -> o) must_== fx
    }

    "avoid occurs check" in {
      fx subst (x -> fx) must_== ffx
    }
  }



  "All vars" should {
    
    "give a variable from a variable" in {
      x.vars must_== List(x)
    }
    
    "give an empty list from a constant" in {
      o.vars must_== List()
    }

    val gfxfy = Fun("g", List(fx,fy), num1a)
    
    "recurse deeply" in {
      gfxfy.vars must_== List(x,y)
    }

    val gfxfx = Fun("g", List(fx,fx), num1a)

    "return a variable more than once if it occurs more than once" in {
      gfxfx.vars must_== List(x,x)
    }
  }

	"Values" should {

		"contain itself when using toString()" in {
			o.toString().contains("1")
		}
	}
}