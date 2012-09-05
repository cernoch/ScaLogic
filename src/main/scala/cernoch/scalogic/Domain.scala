package cernoch.scalogic

import exceptions.SyntaxError
import scala.math._

abstract sealed class Domain[+T]
  (val name: String
  ,val isKey: Boolean) {
  
  def valueOf(s:String) : T
  
  override def hashCode() = name.hashCode()
  override def toString() = (if (isKey) "@" else "") + name
}


class DecDom(name:String) extends Domain[BigDecimal](name, false) {
  override def valueOf(s:String) = s match {
    case "" => null
    case _ => BigDecimal(s)
  }
  override def toString() = super.toString() + ":dec";
  override def equals(o: Any) =
    o.isInstanceOf[DecDom] && name.equals(o.asInstanceOf[DecDom].name)
}

object DecDom {
  def apply(name:String) = new DecDom(name)
  def unapply(d:DecDom) = Some(d.name)
}


class NumDom(name:String, isKey:Boolean) extends Domain[BigInt](name, isKey) {
  override def valueOf(s:String) = s match {
    case "" => null
    case _ => BigInt(s)
  }
  override def toString() = super.toString() + ":int";
  override def equals(o: Any) =
    o.isInstanceOf[NumDom] && name.equals(o.asInstanceOf[NumDom].name)
}

object NumDom {
  def apply(name:String, isKey:Boolean = false) = new NumDom(name, isKey)
  def unapply(d:NumDom) = Some(d.name, d.isKey)
}


class CatDom(name: String, isKey: Boolean,
	      val allowed: Set[String])
    extends Domain[String](name, isKey) {
  
  def this(name: String, isKey: Boolean) = this(name, isKey, Set())
  
  override def valueOf(s:String) = {
    if (s.trim.length == 0) null
    else {
      if (allowed.size == 0 || allowed.contains(s)) s
      else throw new Exception("Value '" + s +
          "' is not among the allowed values of domain '" + name + "'.")
    }
  }
  
  override def toString() = {
    super.toString() + ":cat" + " [" +
      allowed.map("\"" + _ + "\"").mkString(", ") + "]"
  } 
  override def equals(o: Any) =
    o.isInstanceOf[CatDom] &&
      name.equals(o.asInstanceOf[CatDom].name) &&
      allowed.equals(o.asInstanceOf[CatDom].allowed)
}

object CatDom {
  def apply(name:String, isKey:Boolean = false, allowed:Set[String] = Set())
    = new CatDom(name, isKey, allowed)
  def unapply(d:CatDom) = Some(d.name, d.isKey, d.allowed)
}




import scala.util.parsing.combinator._


object Domain {
  
  def apply(line: String) : Domain[_] = {
    val g = new DomGrammar
    g.parse(g.line, line) match {
      case g.Success(r,_) => r.asInstanceOf[Domain[_]]
      case x => throw new SyntaxError(
          "Unknown syntax error: " + x)
    }
  }
  
  class DomGrammar extends JavaTokenParsers {

    def line = namePart ~ tipe ~ opt(allowed) <~ opt(comment) <~ " *$".r ^^ {
      case token ~ name ~ tipe ~ allowed => tipe match {
        
        case "num" => new NumDom(name, token)
        
        case "cat" => new CatDom(name, token,
            allowed.getOrElse(Set()).toSet)
        
        case "dec" => {
          if (token == true) throw new SyntaxError(
              "Decadic domain cannot be a key.");
          new DecDom(name)
        }
        
        case _ => throw new SyntaxError(
          "Unknown type '" + tipe + "'. "
          + "Allowed types: num, int, cat.")
      }
    }
    
    def namePart = token ~ mystr <~ ":"
        
    def token = tokenKey | tokenVal
    def tokenKey = "@" ^^ { (x => true) }
    def tokenVal = "" ^^ { (x => false) }
    
    def tipe = "dec" | "num" | "cat"
    
    def allowed = "\\[".r ~> repsep(mystr, ", *".r) <~ "\\]".r    
    
    def comment = """//.*$""".r
    
    def mystr = ident | stringLiteral ^^ { s =>
      if (s.length() > 0 && s.charAt(0) == '"')
        s.substring(1, s.length()-1) else s
    }
  }
}