package cernoch.scalogic

import tools.{Labeler, Mef}

class Btom[+S <: Term](
                        pred: String,
                        args: List[S],
                        val hooks: Set[Hook],
                        val modeIn: Set[Term])
  extends Atom[S](pred, args) {

  override def subst
  (dict: Term => Option[Term])
  : Btom[Term]
  = {
    val sArgs = Mef.map[Term, Term](args) {
      _.subst(dict)
    }
    val sHooks = Mef.map(hooks) {
      _.subst(dict)
    }
    val sModeIn = Mef.map(modeIn) {
      _.subst(dict)
    }

    if (args == sArgs && hooks == sHooks && modeIn == sModeIn)
      this
    else new Btom[Term](pred, sArgs, sHooks, sModeIn)
  }

  def sflat
  [T <: Term]
  (dict: Term => Option[T])
  : Btom[T]
  = {
    new Btom[T](pred,
      args.map {
        dict(_).get
      },
      hooks.map {
        _.subst(dict)
      },
      modeIn.map {
        _.subst(dict)
      })
  }


  def satisfiable = maxSucc.getOrElse(1) >= 1

  def minSucc = hooks.foldLeft(0) {
    (min, hook) => scala.math.max(min, hook.minSucc)
  }

  def maxSucc = hooks.foldLeft(None.asInstanceOf[Option[Int]]) {
    (valSoFar, hook) => ((valSoFar, hook.maxSucc)) match {
      case (Some(x), Some(y)) => Some(scala.math.min(x, y))
      case (None, Some(x)) => Some(x)
      case (Some(x), None) => Some(x)
      case (None, None) => None
    }
  }

  def equivalents
  = hooks.foldLeft(Set[Btom[Term]](this)) {
    (eqSoFar, hook) => hook.equivs(eqSoFar)
  }


  override def toString() = toString(Var.globalNames)

  override def toString(names: Labeler[Var, String]) =
    pred + args.map {
      arg =>
        (if (modeIn.contains(arg)) "+" else "-") +
          arg.toString(names)
    }
      .mkString("(", ",", ")")

  override def hashCode = modeIn.hashCode + 17 * super.hashCode

  override def equals(o: Any) = o match {
    case a: Btom[_] => pred == a.pred && args == a.args && modeIn == a.modeIn
    case a: Atom[_] => pred == a.pred && args == a.args
    case _ => false
  }
}





import cernoch.scalogic.exceptions._
import scala.util.parsing.combinator.JavaTokenParsers

object Btom {

  def apply(line: String, domain: Set[_ <: Domain[_]])
  = {
    val g = new AtomGrammar(domain.map(d => (d.name,d)).toMap)

    g.parse(g.line, line) match {
      case g.Success(r,_) => r.asInstanceOf[Btom[Var]]
      case x => throw new SyntaxError(
        "Unknown syntax error: " + x)
    }
  }

  class AtomGrammar(doms: Map[String,Domain[_]])
    extends JavaTokenParsers {

    // My string is either an identified or arbitrary string escaped
    def myString = ident | "\"" ~> stringLiteral <~ "\"" ^^ { _.replace("\\\"", "\"") }
    // Positive integer (does not start with 0)
    def positiveInteger = """[1-9]\d*""".r ^^ { _.toInt }

    // List of argument indexes
    def argNumberList =  repsep(positiveInteger, ":")

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
      "fneq=" ~> argNumberList ^^ { x => new ForceNonEqTag(x) } |
      "perm=" ~> argNumberList ^^ { x => new PermutableTag(x) }
    def modifiers = "\\{".r ~> repsep(modifier, ", *".r) <~ "\\}".r

    // Line with the configuration
    def line = myString ~ opt(modifiers) ~ arguments ^^ {
      case name ~ modifierTags ~ argsWithInst => {

        // List of aruments
        val args = argsWithInst.map{_._2}
        // List of "IN" arguments
        val argsIn = argsWithInst.filter{_._1}.map{_._2}

        // Converts argument position to the argument
        def argNo(s:List[Int]) = s.map{_ - 1}.map{args}

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
