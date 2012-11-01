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

  val one = Val(1, num2)
  val two = Val(2, num2)

  "Atom modification" should {

    val axyz = Atom("a", x,y,z)
    val azyx = Atom("a", z,y,x)

    val var2varDict = Dict(x -> z, z -> x)

    "substitute arbitrary items" in {
      (axyz substitute var2varDict.get must_== azyx) and
      (axyz mapSomeArg var2varDict.get must_== azyx)
    }

    val a111 = Atom("a", one,one,one)

    val var2valDict = Dict(x -> one, y -> one, z -> one)

    "infer types correctly" in {
      val a: Atom[Term]   = axyz substitute var2valDict.get
      val b: Atom[FFT]    = axyz mapSomeArg var2valDict.get
      val c: Atom[Val[_]] = axyz mapAllArgs var2valDict

      (a must_== a111) and
      (b must_== a111) and
      (c must_== a111)
    }
    
    "fail on mapping all argument with partial dictionary" in {
      axyz mapAllArgs var2varDict must throwA[Exception]
    }
  }
}