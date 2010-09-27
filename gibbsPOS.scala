import io.Source
import collection.mutable.{HashMap, ArrayBuffer}
import util.Random

object gibbsPOS {
    class Lexicon[A] {
	val map = new HashMap[A,Int]()
	private var nextID = 1
	def apply(n:A):Int = 
	    map.get(n) match {
		case Some(i) => i
		case None => {
		    map.put(n,nextID)
		    nextID = nextID+1
		    nextID-1
		}
	    }
	
	def numID = nextID //Includes the 0th ID
    }

    class Counter(N:Int) {
    	val c = {val t = new ArrayBuffer[Int]
		 for (i <- 0 until N) t += 0
		 t}
	def apply(n:Int):Int = c(n)
	def +=(a:Int) = c.update(a,c(a)+1)
	def -=(a:Int) = c.update(a,c(a)-1)
    }

    class POSdata(file:String) {
	val wordLex = new Lexicon[String]
	val tagLex = new Lexicon[String]

	//Given a line from a column file, return (word id, tag id) pair
	//  if a blank line return (0,0) special marker
	def POSColPair(line:String):(Int, Int) = {
	    val cols = line.split("\\s+")
	    if (cols.length < 2) (0,0)	
	    else (wordLex(cols(0)), tagLex(cols(1)))
	}

	def load(source:Source):List[(Int, Int)] = 
	    List((0,0)) ++ source.getLines.map(POSColPair).toList
	
	val data = load(Source.fromFile(file))
	val nWords = wordLex.numID
	val nLabels = tagLex.numID
    }

    class POSState (N: Int, pos: POSdata) {
	val assign = new ArrayBuffer[Int]
	
	//state t->t' transition count
	val tTrans = new ArrayBuffer[Counter]
	for (i <- 0 until N) tTrans += new Counter(N)

	//state t->word w transition count
	val wEmit = new ArrayBuffer[Counter]
	for (i <- 0 until N) wEmit += new Counter(pos.nWords)

	//state t count
	val tCount = new Counter(N)

	//Initialize with uniformly random state assignments to all
	// words
	def initialize(r:Random) = {
	    for (((w,t),i) <- pos.data.zipWithIndex) {
		val s = if (w == 0) 0 else r.nextInt(N)
		assign += s
		tCount += s
		wEmit(s) += w
		if (i > 0) tTrans(assign(i-1)) += s
	    }
	}
    }

    def main(args: Array[String]): Unit = {
	if (args.length < 1) throw new Error("Usage: gibbsPOS <N> <POS col>")
	val posTxt = new POSdata(args(1))
	val state = new POSState(args(0).toInt, posTxt)
	state.initialize(new Random)
	println("Data: " + posTxt.data)
	println("Assignment: "+state.assign)
    }
}
