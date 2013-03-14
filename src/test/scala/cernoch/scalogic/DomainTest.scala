package cernoch.scalogic

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class DomainTest extends Specification {

	val dec = DoubleDom("dec")
	val dec2 = DoubleDom("dec")
	val decadic = DoubleDom("decadic")

	"Domains equality" should {
		"equal if their names equal" in {
			dec must_!= decadic
		}

		"hold if names are equal" in {
			dec must_== dec2
		}
	}
}