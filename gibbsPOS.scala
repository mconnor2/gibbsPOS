import io.Source
import collection.mutable.{HashMap, ArrayBuffer}
import util.Random
import scala.math.{log,exp}

object gibbsPOS {
    class Lexicon[A] {
	val map = new HashMap[A,Int]()
	private var nextID = 1
	def apply(n:A):Int = 
	    map.get(n) match {
		case Some(i) => i
		case None => {
		    map.put(n,nextID)
		    nextID += 1
		    nextID-1
		}
	    }
	def numID = nextID //Includes the 0th ID
    }

    class Counter(N:Int) {
    	val c = {val t = new ArrayBuffer[Int]
		 for (i <- 0 until N) t += 0
		 t}
	var total = 0
	def apply(n:Int):Int = c(n)
	def +=(a:Int) = {total += 1; c.update(a,c(a)+1)}
	def -=(a:Int) = {total -= 1; c.update(a,c(a)-1)}
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

    var emitP = 0.01
    var transP = 0.1

    class POSstate (val N: Int, pos: POSdata) {
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
	def initialize() = {
	    for (((w,t),i) <- pos.data.view.zipWithIndex) {
		val s = if (w == 0) 0 else (Random.nextInt(N-1)+1)
		assign += s
		tCount += s
		wEmit(s) += w
		if (i > 0) tTrans(assign(i-1)) += s
	    }
	}

	//Remove state assignment at position i, which emits word w
	def remove (w: Int, i: Int) : Unit = {
	    tTrans(assign(i-1)) -= assign(i)
	    tTrans(assign(i)) -= assign(i+1)
	    tCount -= assign(i)
	    wEmit(assign(i)) -= w
	}

	def add(w: Int, i:Int, s:Int) : Unit = {
	    assign(i) = s
	    tCount += s
	    wEmit(s) += w
	    if (i > 0) tTrans(assign(i-1)) += s
	    if (i < assign.length-1) tTrans(s) += assign(i+1)
	}

	//Find log probability of selecting state s at position i, emitting
	// word w
	def logProb (w: Int, i: Int)(s:Int) : Double = {
//	    println("Log probability of assigning state "+s+
//		    " at index "+i+" emitting word "+w)
	    var logP = log(tTrans(assign(i-1))(s) + transP) - 
		       log(tCount(assign(i-1)) + N*transP)
//	    println("  "+tTrans(assign(i-1))(s) + " # transitions from -1 to s")
	    logP += log(wEmit(s)(w) + emitP) - 
		    log(tCount(s) + pos.nWords*emitP)
//	    println("  "+wEmit(s)(w) + " emissions of w from s")
	    logP += log(tTrans(s)(assign(i+1)) + 
			(if (assign(i-1) == s && assign(i+1) == s) 1 else 0) +
			transP) -
		    log(tCount(s) + (if (assign(i-1) == s) 1 else 0) + N*transP)
//	    println("  "+tTrans(s)(assign(i+1)) + " # transitions from s to +1")
	    logP
	}
    }

    //Converted from Aria Haghihi's standard ML code (as used elsewhere

    def logNormalize (logs:Seq[Double]) = {
	def logSum (logs:Seq[Double]) : Double = {
	    val maxLog = logs.max
	    maxLog + log(logs.map(x => 
		if (x-maxLog > -30) exp(x-maxLog) else 0).reduceLeft(_+_))
	}
	val sum = logSum(logs)
	for (x <- logs) yield exp(x-sum)
    }
	
    def sampleState (probs:Seq[Double]) = {
	val p = Random.nextDouble
	probs.view.scanLeft(0.0)(_+_).zipWithIndex.find(_._1 >= p) match {
	    case None => throw new Error("Can't sample with p: "+p)
	    case Some((p,i)) => i
	}
    }

    def updateState(w: Int, i: Int, state: POSstate) = {
//	println("Updating state for word "+i)
	//Remove counts for current assignment
	state.remove(w,i)
	//Calculate probability of each state given surrounding and word
	// and counts without it (up to normalizing)
	val logProbs = (1 until state.N).map(state.logProb(w,i))
//	println("Log probabilities: "+logProbs)
//	val lognorm = logNormalize(logProbs)
//	println("    normalized: "+lognorm)
//	println("    sum: "+lognorm.reduceLeft(_+_))
	//Sample from this to assign state
	state.add(w, i, sampleState(logNormalize(logProbs)))
    }
    
    //One pass through data, sampling state for each word from the P(t|t_-i,w)
    // which is calculated from current state
    def gibbs(state: POSstate, pos: POSdata) : Unit =
    	for (((w,t),i) <- pos.data.view.zipWithIndex if w > 0) 
	    updateState(w,i,state)

    //For each state, find the tag it is seen with most often, and count
    // that as correct
    def manyToOne(tagMap:Seq[Counter]):Int = 
	tagMap.foldLeft(0)(_+_.c.max)

    // Variance of mutual information between tag clusters and state clusters
    //
    //  VI(Y,T) = H(Y|T) + H(T|Y)
    //  H(Y|T) = H(Y) - I(Y,T)
    //  H(T|Y) = H(T) - I(Y,T)
    //  I(Y,T) = sum(p(y,t) log(p(y,t)/p(y)p(t))
    //  H(Y) = -sum(p(y) log p(y))
    //  H(T) = -sum(p(t) log p(t))
    def VI(tagMap:Seq[Counter], tagCount:Counter, total:Int):Double = {
	def entropy(p:Seq[Double]) = 
	    -p.foldLeft(0.0)((b,x) => b+(if (x==0) 0.0 else x*log(x)/log(2.0)))

	val py = for (y <- tagMap) yield (y.total.toDouble / total.toDouble)
	val HY = entropy(py)
    
//	println("  PY: "+py)
//	println("  HY: "+HY)

	val pt = for (t <- tagCount.c) yield (t.toDouble / total.toDouble)
	val HT = entropy(pt)
	
//	println("  PY: "+pt)
//	println("  HY: "+HT)
	
	var IYT = 0.0
	for ((y,i) <- py.view.zipWithIndex;
	     (t,j) <- pt.view.zipWithIndex if y > 0) {
	    val joint = tagMap(i)(j).toDouble / total.toDouble
	    val indep = joint / (y*t)
	    if (joint > 0) IYT += joint * log(indep)/log(2.0)
	}

	HY+HT-2.0*IYT
    }

    //Find the many to 1 matching and VI of the current assignment
    // in respect to the true tagging in pos
    def evaluate(state: POSstate, pos: POSdata) = {
	//Find counts for mapping each tag to an HMM state
	val tagMap = new ArrayBuffer[Counter]
	for (i <- 0 until state.N) tagMap += new Counter(pos.nLabels)
	
	var length = 0
	val tagCount = new Counter(pos.nLabels)
    	for (((w,t),i) <- pos.data.view.zipWithIndex if w > 0) {
	    tagMap(state.assign(i)) += t
	    tagCount += t
	    length += 1
	}
	
	val manyError = manyToOne(tagMap)
	val vi = VI(tagMap,tagCount,length)
	(manyError.toDouble / length.toDouble, vi)
    }

    def main(args: Array[String]): Unit = {
	if (args.length < 1) throw new Error("Usage: gibbsPOS <N> <POS col>")
	val posTxt = new POSdata(args(1))
	val state = new POSstate(args(0).toInt+1, posTxt)
	state.initialize()
//	println("Data: " + posTxt.data)
//	println("Assignment: "+state.assign)
	var err = evaluate(state, posTxt)
	
	println("Iteration\tMany2One\tVI\tTime(ns)\n")
	var iteration = 0
	println(iteration+"\t"+err._1+"\t"+err._2+"\t"+System.nanoTime)
	while (iteration < 10000) {
	    iteration += 1
	    gibbs(state, posTxt)
//	println("Assignment: "+state.assign)
	    err = evaluate(state, posTxt)
//	    println("  Many to 1 error: "+err)
	    println(iteration+"\t"+err._1+"\t"+err._2+"\t"+System.nanoTime)
	}
    }
}
