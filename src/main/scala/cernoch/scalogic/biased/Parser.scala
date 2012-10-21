package cernoch.scalogic.biased

import cernoch.scalogic._
import biased.Parser.AtomGrammar
import cernoch.scalogic.biased._
import cernoch.scalogic.exceptions._
import scala.util.parsing.combinator.JavaTokenParsers

object Parser {

  def apply(line: String, domain:Set[_ <: Domain[_]])
  = {
      val g = new AtomGrammar(domain.map(d => (d.name,d)).toMap)

      g.parse(g.line, line) match {
        case g.Success(r,_) => r.asInstanceOf[Btom[Var]]
        case x => throw new SyntaxError(
            "Unknown syntax error: " + x)
      }
    }

  class AtomGrammar(doms:Map[String,Domain[_]]) extends JavaTokenParsers {

    // My string is either an identified or arbitrary string escaped
    def myString = ident | "\"" ~> stringLiteral <~ "\"" ^^ { _.replace("\\\"", "\"") }
    // Positive integer (does not start with 0)
    def positiveInteger = """[1-9]\d*""".r ^^ { _.toInt }

    // List of argument indexes
    def argNumberList =  "\\{".r ~> repsep(positiveInteger, ", *".r) <~ "\\}".r

    // Instantiation can be IN (+) or OUT (-), converted to boolean
    def instantiation = "+" ^^ { _ => true } | "-" ^^ { _ => false }
    // Single argument with instantiation
    def argument = instantiation ~ myString ^^ {
      case inst ~ domain =>
        // A variable must pick its domain
        (inst, new Var(doms.get(domain).getOrElse(
          throw new DomainNotFound("Domain '" + domain
            + "' was not given to the parser.")
        )))
    }
    // List of arguments (surrounded by braces)
    def arguments = "\\(".r ~> repsep(argument, ", *".r) <~ "\\)".r


    // Commant at the end of a line
    def eolComment = """//.*$""".r
    // End of line (w/o comment)
    def endofline = " *$".r | eolComment

    // Helper class
    abstract sealed class Modifier {}
    case class FunctionalTag() extends Modifier {}
    case class DeterminedTag() extends Modifier {}
    case class ForceNonEqTag(args:List[Int]) extends Modifier {}
    case class PermutableTag(args:List[Int]) extends Modifier {}
    // Parsing of modifiers
    def modifier =
       "det" ^^ { x => new DeterminedTag } |
      "func" ^^ { x => new FunctionalTag } |
      "fneq *= *" ~> argNumberList ^^ { x => new ForceNonEqTag(x) } |
      "perm *= *" ~> argNumberList ^^ { x => new PermutableTag(x) }
    def modifiers = "\\{".r ~> repsep(modifier, ", *".r) <~ "\\}".r

    // Line with the configuration
    def line = myString ~ opt(modifiers) ~ arguments ^^ {
      case name ~ modifierTags ~ argsWithInst => {

        // List of aruments
        val args = argsWithInst.map{_._2}
        // List of "IN" arguments
        val argsIn = argsWithInst.filter{_._1}.map{_._2}

        // Converts argument position to the argument
        def argNo(s:List[Int]) = s.map{args.apply(_)}

        val hooks = modifierTags.getOrElse(List()).map {
          case FunctionalTag() => Functional
          case DeterminedTag() => Determined
          case ForceNonEqTag(idx) => new ForceNonEq(argNo(idx).toSet)
          case PermutableTag(idx) => new Permutable(argNo(idx).toSet)
        }

        new Btom[Var](name, args, hooks.toSet, argsIn.toSet)
      }
    }
  }
}