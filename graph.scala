import ammonite.ops._ 
import ammonite.shell._ 
import ammonite.ops.ImplicitWd._

/*
TODO: 
 * add syntax for custom operators
 * file atomicity
 * stronger type-checking (e.g. `write` should only accept `Path`
 * the results list in runMap could keep metadata info for each target that's been added to it (or each rule that's been run)
 * as a Map or List, and this could be pattern-matched on too, and passed along, as in graph.clj, when the type of the target...
 * Key is not a file/file-pattern. 
 * use sorted Map
 * If there are duplicate rules, they should be dissoc'ed between iterations. This way the first rule acts like the first case in a pattern.
 * so if a previous rule doesn't create an optional object (say file), put the rule requiring that object first . . . but would have
 * to make sure that the optional obj. rule is defined before that. Maybe use Maybe Monad for this?
 * currently don't use topological sort, so running this would run rules that weren't necessary. This is bad because we want user
 * to be able to pick what to run (e.g. clean, install, etc.)
*/
/*
what we'd like to see:
{ "*.md" => { ("*.txt", "echo") -> (a, b) => a + b} 
hmmm anonymous function types needed.
*/
val m = Map({ "*.md" -> { List("*.txt") -> ((x:Path) => cp(x, x %= "silly")) } } ) 


val getInputs = ((x:List[String]) => x.map( s => ls! cwd |? (_ ~= s)))
def anyEmpty(seq:Seq[Seq[Any]]) = seq.map(_.isEmpty).reduce(_ || _)

/* Instead of running through by order, runMap accepts a key (target) which is the final goal,
and recursively request the rules necessary to build the target. */
def runMap (m:Map[String, (List[String], (Path*) => Unit)]) = {
  def runMapP (m:Map[String, (List[String], (Path*) => Unit)], results:List[List[Path]]) = {
     if (m.isEmpty) return ()
     val next = m.filterNot(x => anyEmpty(getInputs(x._2._1))).head  // this raises exception if can't keep going
     val newResults = getInputs(m._.2._1).map(_ | (m._2._2)) // this is still wrong
     return runMapP( (m - head._1), results :+ newResults)
}
  runMap(m, List())
}

def fileReg (s:String) = ("(" ++ s.replaceAll("\\.", "\\.").replaceAll("\\*", ".*") ++ ")").r
val noExt =  ((x:String) => x.split('.').init.mkString(".")) 
val swapExt = ( (s:String, ext:String) => (noExt(s)) ++ "." ++ ext)
def dirname (p:Path) : Path = cwd.make(p.segments.init, 0)
implicit class Iff(val a: Path) extends AnyVal { 
  def -= = dirname(a)/noExt(a.last)
  def %= (b: String) : Path  = dirname(a)/swapExt(a.last, b)
  def += (b: String) : Path  = dirname(a)/(a.last ++ "." ++ b)
  def ~= (b: String) : Boolean = fileReg(b).unapplySeq(a.last).isDefined
}


// maybe check for .PHONY results: (create phoney object string wrapper I guess)
//def getInputsP (xs:List[String]) = xs.map( s => ls! cwd |? (_ ~= s)))

// but getInputs needs to include the inputs that have already been created.
// remove from map once completed with `-` `key`

/* Examples */
ls! cwd |? (_.last ~= "*.txt")
ls! cwd |? (_ ~= "*.txt") | (x => cp(x, x %= "silly")) 
ls! cwd |? (_ ~= "*.silly") | (rm!) 
def list? (s:String) = ls! cwd |? (_ ~= s) 


val tupleMap = { "*.md" -> { ("*.yaml", "*.cfg") -> ((x:Path) => cp(x, x %= "silly")) } } 
//scala splat
Function.tupled(add _)(1, 3)
// have to use `import shapeless._` to get the mapping + splatting
// to work together.

def list(s:String) = ls! cwd |? (_ ~= s)


//val inputs =  .map( a => a match {case s :: Nil => List(ls! cwd |? (_ ~= s))
//                        case xs  => xs.map(x => ls! cwd |? (_ ~= x))})

//val universe: scala.reflect.runtime.universe.type = scala.reflect.runtime.universe
//import universe._
//val t = q"""{ case "*.txt" => true
//          case _ => false}"""
//extract the cases, 
//try matching on all of them and catch a match error.
//val l = t.children.map(_.children(0))
//    .getClass -> Ident or Literal
//t.children(0).children(0)
