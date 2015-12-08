
def runMap (map2:Map[String, (List[String], (Path*) => Unit)], tgt:String) = {
  def runMapP (m:Map[String, (List[String], (Path*) => Unit)], results:List[List[Path]]) = {
     val files = ls! cwd
     def getNext(map:Map, target:String) : (Function, Seq[Path]) = {
         val reqs = map(target)._2._1
         val fulfilled = reqs.filterNot(x => anyEmpty(getInputs(x._2._1)))
         reqs.difference(fulfilled) match 
            { case List() => return (map(target)._2._2, fulfilled)
              case needed => {
                  val satisfied = needed.map(getNext
         
     if (m.isEmpty) return ()
     val func, val inputs = getNext(m, tgt) 
     val newResults = inputs.map(_ | func) // this is still wrong
     return runMapP( (m - head._1), results :+ newResults)
}
  runMap(m, List())
}
