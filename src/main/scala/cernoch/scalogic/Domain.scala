package cernoch.scalogic

trait Domain extends (Any => Val) {

	def name: String

	override def toString() = name
	override def hashCode() = name.hashCode()
	override def equals(o: Any) = o match {
		case d:Domain => name == d.name
		case _ => false
	}
}

trait Limited[S] extends Domain { def isAllowed(s:S): Boolean }
trait Bounded[S] extends Domain { def minValue: S; def maxValue: S }
