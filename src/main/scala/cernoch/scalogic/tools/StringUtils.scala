package cernoch.scalogic.tools

import scala.StringBuilder
import cernoch.scalogic.{Var, HasVariables}

object StringUtils {

	val SimpleString = "[a-zA-Z0-9_]+".r

	def ident(o: Any)
	= o.toString match {
		case null => "NULL"
		case s@SimpleString() => s
		case s => "'" + s.replaceAllLiterally("'", "\\" + "'") + "'"
	}

	@Deprecated
	def mkStringIfNonEmpty
		(i: TraversableOnce[String])
		(l: String, s: String, r: String)
	= if (i.isEmpty) "" else i.mkString(l,s,r)



	implicit def stringJoiner
	(items: Iterable[String]) = new {

		var sb: StringBuilder = null
		def into(s: StringBuilder) = {sb = s; this}

		private var prefix = ""
		def |::(pref: String) = {prefix = pref; this}

		private var suffix = ""
		def ::|(suff: String) = {suffix = suff; this}

		def mk(sep: String)
		= if (sb == null)
			items.mkString(prefix, sep, suffix)
		else {
			items.addString(sb, prefix, sep, suffix)
			null
		}

		def join(sep: String)
		= if (items.isEmpty) "" else mk(sep)
	}



	implicit def hasVarsJoiner(a:(
		Iterable[HasVariables],
		Labeler[Var,String],
		Boolean ))
	= new Object() {

		val (items, names, short) = a

		var sb: StringBuilder = null
		def into(s: StringBuilder) = {sb = s; this}

		private var prefix = ""
		def |::(pref: String) = {prefix = pref; this}

		private var suffix = ""
		def ::|(suff: String) = {suffix = suff; this}

		def mk(sep: String)
		= {
			val s = if (sb == null)
				new StringBuilder else sb

			s.append(prefix)
			var first = true
			for (item <- items) {
				if (!first)
					s.append(sep)
				item.toString(s, names, short)
				first = false
			}
			s.append(suffix)

			if (sb == null)
				s.toString() else null
		}

		def join(sep: String)
		= if (items.isEmpty) "" else mk(sep)
	}
}
