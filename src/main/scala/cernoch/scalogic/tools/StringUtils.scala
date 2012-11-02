package cernoch.scalogic.tools

object StringUtils {

  val SIMPLE_STRING = "[a-zA-Z_][a-zA-Z0-9_]*".r

  def ident
    (s: String,
     q: String = "'")
  = s match {
    case SIMPLE_STRING() => s
    case null => ""
    case _ =>
      q + s.replaceAllLiterally(q, "\\" + q) + q
  }

  def mkStringIfNonEmpty
    (i: TraversableOnce[String])
    (l: String, s: String, r: String)
  = if (i.isEmpty) "" else i.mkString(l,s,r)
}
