package cernoch.scalogic

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DomainTest extends Specification {

  val dec = Domain.dec("dec")
  val dec2 = Domain.dec("dec")
  val decadic = Domain.dec("decadic")

  "Domains of different name" should {
    "not be equal even in the type agrees" in {
      dec must_!= decadic
    }
  }

  "Domains of the same name" should {
    "equal if their names equal" in {
      dec must_== dec2
    }
  }
}