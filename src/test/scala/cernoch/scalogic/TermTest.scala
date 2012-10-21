package cernoch.scalogic

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TermTest extends Specification {

  val num1a = NumDom("num1")
  val num1b = NumDom("num1")

  val num2 = NumDom("num2")

  val dec = DecDom("dec")

  val x = Var(dec)
  val y = Var(dec)
  
  "Two different variables" should {
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
      x.subst(sa(x -> o)) must_== o
    }

    "replace a variable in a function" in {
      fx.subst(sa(x -> o)) must_== fo
    }

    "replace whole function with a value" in {
      fx.subst(sa(fx->o)) must_== o
    }

    "treat different variables as non-unifiable" in {
      fx.subst(sa(fy->o)) must_== fx
    }

    "avoid occurs check" in {
      fx.subst(sa(x->fx)) must_== ffx
    }
  }



  /*"Unification" should {

    "succeed for two variables" in {
      x.unify(y) must_== Some(Map(x -> y))
    }

    "succeed for a variable and a constant" in {
      y.unify(o) must_== Some(Map(y -> o))
    }

    "return instantiations of the left argument" in {
      o.unify(y) must_== Some(Map())
    }
    
    "succeed for two equal constants" in {
      o.unify(o1) must_== Some(Map())
    }

    "fail for two constants of same value from different domains" in {
      o.unify(o2) must_== None
    }

    "suceed for two 'variant' functions" in {
      fx.unify(fy) must_== Some(Map(x -> y))
    }

    "fail for a function and a constant" in {
      fx.unify(o) must_== None
    }
    
    "avoid occurs check" in {
      x.unify(fx) must_== Some(Map(x -> fx))
    }
  }*/
  
  
  
  "All variables" should {
    
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
}