package cernoch.scalogic

import tools._
import exceptions.SyntaxError
import scala.util.parsing.combinator.JavaTokenParsers

object Dict {
  
  def fromList[T](l:List[T]) = {
    def internal(head:T, l:List[T])
    : Map[T,T]
    = l match {
      case Nil => Map()
      case last :: Nil => Map(last -> head)
      case elem :: next :: tail => internal(head, next :: tail) + (elem -> next) 
    }

    l match {
      case Nil => throw new NoSuchElementException("List must have at least 1 value")
      case h :: _ => internal(h,l)
    }
  }
}

/**
 *
 * @param args
 * 
 * @author Radomir Cernoch (radomir.cernoch at gmail.com)
 */
class Atom[+S<:Term](val pred: String, val args: List[S]) {

  def subst[T<:Term](u:Var[T], v:T) = {
    val conv = new Labeler[Term,Term](_.subst(u,v))
    val sArg = Mef.map[Term,Term](args){conv}
    if (sArg.eq(args)) this else new Atom[Term](pred,sArg)
  }

  def vars = Term.vars(args)

  def unify(a:Atom[Term])
  : Option[Map[Var[Term],Term]]
  = if (a.pred != this.pred)
      None
    else
      Term.unify(this.args, a.args) match {
        case Some((u1,u2)) => Some(u1)
        case _ => None
      }

  override def toString() = toString(Var.globalNames)
  def toString(names: Labeler[Var[_],String]) =
    pred + args.mkString("(", ",", ")")

  override def hashCode = pred.hashCode + 31 * args.hashCode
  override def equals(o:Any) = o match {
    case a:Atom[_] => pred == a.pred && args == a.args
    case _ => false
  }  
}

object Atom {
  def apply[T<:Term](p:String, a:List[T]) = new Atom[T](p,a)
  def unapply[T<:Term](a:Atom[T]) = Some(a.pred, a.args)
}

class BiasAtom[+S<:Term](
        pred: String,
        args: List[S],
    val func: Boolean,
    val mode: Map[Var[Term],Inst],
    val uniq: List[Term],
    val dist: Set[Set[Term]],
    val perm: Set[Set[Term]])
  extends Atom[S](pred, args) {
  
  def this(pred:String, args:List[S])
    = this(pred,args,false,Map(),args,Set(),Set())
  
  def permute = {
    def helper
    (terms: List[Term], langs: List[List[Term]])
    : Set[List[Term]]
    = langs match {
      case Nil => Set()
      case dict :: tail
        => dict
          .permutations
          .map(Dict.fromList(_)) // get dictionaries in a Map
          .map{dict => helper( terms map {_.matching(dict)}, tail )}
          .flatten.toSet
    }
    helper(this.args, perm.map(_.toList).toList)
  }
  
  override def subst[T<:Term](u:Var[T], v:T) = {
    
    val conv = new Labeler[Term,Term](_.subst(u,v))

    val sArg = Mef.map[Term,Term](args)(conv)
    val sKey = Mef.map[Term,Term](uniq)(conv)

    val sMod = {
      var qMod = Map[Var[Term],Inst]()
      
      for (tuple <- mode) {
        val (mv,mi) = tuple
        if (mv == u) {
          if (v.isInstanceOf[Var[_]])
            qMod = qMod + (v.asInstanceOf[Var[Term]] -> mi)
        } else
          qMod = qMod + tuple
      }
      
      if (qMod == mode) mode else qMod // saves memory
    }

    /* Ensure that all terms that should stay distinct
     * have stayed distinct even after substitution. */
    //TODO: Handle allDistinct
    val (sDst, allDistinct) = Mef
      .mapChk(dist)
       {Mef.map(_)(conv)}
       {(a,b) => a.size == b.size}

    /* Reflexivity atoms are permuted as well */
    val sPrm = Mef.map(perm){Mef.map(_)(conv)}
    
    if (sArg.eq(args) && sMod.eq(mode) && sKey.eq(uniq) &&
                         sDst.eq(dist) && sPrm.eq(perm)) this else
       new BiasAtom(pred,sArg,func,sMod,sKey,sDst,sPrm)
  }
}

object BiasAtom {

  def apply(line: String, domain:Set[_ <: Domain[_]]) = {
      val g = new AtomGrammar(domain.foldLeft(
          Map[String,Domain[_]]()
        ){
          (a,b) => a + (b.name -> b)
        })
      g.parse(g.line, line) match {
        case g.Success(r,_) => r.asInstanceOf[BiasAtom[Var[Val[_]]]]
        case x => throw new SyntaxError(
            "Unknown syntax error: " + x)
      }
    }

  def unapply[S<:Term](b:BiasAtom[S])
    = Some((b.pred, b.args, b.func, b.mode, b.uniq, b.dist, b.perm))

  
  class AtomGrammar(doms:Map[String,Domain[_]]) extends JavaTokenParsers {

    abstract sealed class Modifier {}
    case class Functional() extends Modifier {}
    case class Unique(args:List[Int]) extends Modifier {}
    case class Distinct(args:List[Int]) extends Modifier {}
    case class Permutable(args:List[Int]) extends Modifier {}
    
    def line = mystr ~ opt(modifiers) ~ args ^^ {
      case name ~ modifiers ~ arguments => {
        
        val args = arguments.map(_._2)
        val mode = arguments.foldLeft( Map[Var[Term],Inst]() )
                                     { (a,b) =>  a + b.swap }
        
        def argNo(s:List[Int]) = s.map{args.apply(_)}
        
        val default = (false, args, Set[Set[Term]](), Set[Set[Term]]())
        
        val (func,uniq,dist,perm) = modifiers match {
          case None => default
          case Some(x) => x.foldLeft(default)((ms, m) => {
            val (func,uniq,dist,perm) = ms
            m match {
              case _:Functional     => (true, uniq,      dist,                       perm)
              case u:Unique     => (func, argNo(u.args), dist,                       perm)
              case d:Distinct   => (func, uniq,          dist + argNo(d.args).toSet, perm)
              case p:Permutable => (func, uniq,          dist,                       perm + argNo(p.args).toSet)
              case _ => throw new InternalError("Sealed class is not sealed!")
            }
          }) 
        }
        
        new BiasAtom[Var[Val[_]]](name, args, func, mode, uniq, dist, perm)
      }
    }
    
    def modifier = 
      "!" ^^ { x => List(new Functional) } |
      "func" ^^ { x => List(new Functional) } |
      argNumberList ^^ { x => List(new Unique(x)) } |
      "dist *= *" ~> argNumberList ^^ { x => List(new Unique(x)) } |
      "uniq *= *" ~> argNumberList ^^ { x => List(new Unique(x)) } 
    def modifiers = "\\{".r ~> repsep(modifier, ", *".r) <~ "\\}".r
    
      
    def positiveNumber = """[1-9]\d*""".r ^^ { _.toInt }
    
    def argNumberList =  "\\{".r ~> repsep(positiveNumber, ", *".r) <~ "\\}".r
    

    
      
    def args = "\\(".r ~> repsep(arg, ", *".r) <~ "\\)".r
    
    def arg = inst ~ mystr ^^ { case inst ~ domain =>
        (inst, new Var[Val[_]](doms.get(domain).get))
    }
    
    def inst = (Inst.IN.symbol ^^ { x => Inst.IN })       |
               (Inst.OUT.symbol ^^ { x => Inst.OUT })     |
               (Inst.CONST.symbol ^^ { x => Inst.CONST })
    
    def token = tokenFunc | tokenNone
    def tokenFunc = "!" ^^ { (x => true) }
    def tokenNone = "" ^^ { (x => false) }
    
    def tipe = "dec" | "num" | "cat"
    
    def allowed = "\\[".r ~> repsep(mystr, ", *".r) <~ "\\]".r    
    
    def comment = """//.*$""".r
    
    def mystr = ident | stringLiteral ^^ { s =>
      if (s.length() > 0 && s.charAt(0) == '"')
        s.substring(1, s.length()-1) else s
    }
  }
}
