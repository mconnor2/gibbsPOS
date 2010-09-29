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
	def ++=(s:Seq[Int]) = for (a <- s) {this += a}
	def -=(a:Int) = {total -= 1; c.update(a,c(a)-1)}
	def --=(s:Seq[Int]) = for (a <- s) {this -= a}
    }

    class POSdata(file:String) {
	val featLexs = new ArrayBuffer[Lexicon[String]]
	val tagLex = new Lexicon[String]

	//Given a line from a column file, return (tag id, feature list) pair
	//  if a blank line return (0,Nil) special marker
	def POSColPair(line:String):(Int, List[Seq[Int]]) = {
	    val cols = line.split("\\s+")
	    if (cols.length < 2) (0,Nil)
	    else (tagLex(cols(0)),
		  cols.tail.zipWithIndex.map(p => {
		      if (featLexs.length <= p._2) 
			  featLexs += new Lexicon[String]
		      p._1.split(',').map(featLexs(p._2).apply).toSeq
		   }).toList)
	}

	def load(source:Source):List[(Int, List[Seq[Int]])] = 
	    List((0,Nil)) ++ source.getLines.map(POSColPair).toList
	
	val data = load(Source.fromFile(file))
	val nLabels = tagLex.numID
    }

    var emitP = 0.001
    var transP = 0.1

    class POSstate (val N: Int, pos: POSdata) {
	val assign = new ArrayBuffer[Int]
	
	//state t->t' transition count
	val tTrans = new ArrayBuffer[Counter]
	for (i <- 0 until N) tTrans += new Counter(N)

	//state t->word w transition count
	val wEmit = new ArrayBuffer[ArrayBuffer[Counter]]
	for (i <- 0 until N) {
	    wEmit += new ArrayBuffer[Counter]
	    for (f <- pos.featLexs) 
		wEmit(i) += new Counter(f.numID)
	}

	//state t count
	val tCount = new Counter(N)

	//Initialize with uniformly random state assignments to all
	// words
	def initialize() = {
	    for (((t,wf),i) <- pos.data.view.zipWithIndex) {
		val s = if (t == 0) 0 else (Random.nextInt(N-1)+1)
		assign += s
		tCount += s
		for ((f,i) <- wf.zipWithIndex) wEmit(s)(i) ++= f
		if (i > 0) tTrans(assign(i-1)) += s
	    }
	}

	//Remove state assignment at position i, which emits word w
	def remove (wf: List[Seq[Int]], i: Int) : Unit = {
	    tTrans(assign(i-1)) -= assign(i)
	    tTrans(assign(i)) -= assign(i+1)
	    tCount -= assign(i)
	    for ((f,j) <- wf.zipWithIndex) wEmit(assign(i))(j) --= f
	}

	def add(wf: List[Seq[Int]], i:Int, s:Int) : Unit = {
	    assign(i) = s
	    tCount += s
	    for ((f,j) <- wf.zipWithIndex) wEmit(s)(j) ++= f
	    if (i > 0) tTrans(assign(i-1)) += s
	    if (i < assign.length-1) tTrans(s) += assign(i+1)
	}

	//Find log probability of selecting state s at position i, emitting
	// word w
	def logProb (wf: List[Seq[Int]], i: Int)(s:Int) : Double = {
//	    println("Log probability of assigning state "+s+
//		    " at index "+i+" emitting word "+w)
	    val abefore = assign(i-1)
	    val aafter = assign(i+1)
	    var logP = log(tTrans(abefore)(s) + transP) - 
		       log(tCount(abefore) + N*transP)

//	    println("  "+tTrans(assign(i-1))(s) + " # transitions from -1 to s")
	    for ((f,j) <- wf.zipWithIndex; v <- f) 
		logP += log(wEmit(s)(j)(v) + emitP) - 
			log(wEmit(s)(j).total + pos.featLexs(j).numID*emitP)

//	    println("  "+wEmit(s)(w) + " emissions of w from s")
	    logP +  log(tTrans(s)(aafter) + 
		        (if (abefore == s && aafter == s) 1 else 0) + transP) -
		    log(tCount(s) + (if (abefore == s) 1 else 0) + N*transP)
//	    println("  "+tTrans(s)(assign(i+1)) + " # transitions from s to +1")
	}
    }

    //Converted from Aria Haghihi's standard ML code (as used elsewhere

    def logNormalize (logs:Seq[Double]) = {
	def logSum (logs:Seq[Double]) : Double = {
	    val maxLog = logs.max
	    maxLog + log(logs.foldLeft(0.0)((b,x) => b +
			    (if (x-maxLog > -30) exp(x-maxLog) else 0)))
	}
	val sum = logSum(logs)
	for (x <- logs) yield exp(x-sum)
    }
	
    def sampleState (probs:Seq[Double]) = {
	val p = Random.nextDouble
	//Note that scanLeft includes a 0 at the beginning of the list, 
	// which matches well with the special 0 state, which cannot be selected
	probs.view.scanLeft(0.0)(_+_).zipWithIndex.find(_._1 >= p) match {
	    case None => throw new Error("Can't sample with p: "+p)
	    case Some((p,i)) => i
	}
    }

    def updateState(wf: List[Seq[Int]], i: Int, state: POSstate) = {
//	println("Updating state for word "+i)
	//Remove counts for current assignment
	state.remove(wf,i)
	//Calculate probability of each state given surrounding and word
	// and counts without it (up to normalizing)
	val logProbs = (1 until state.N).map(state.logProb(wf,i))
//	println("Log probabilities: "+logProbs)
//	val lognorm = logNormalize(logProbs)
//	println("    normalized: "+lognorm)
//	println("    sum: "+lognorm.reduceLeft(_+_))
	//Sample from this to assign state
	state.add(wf, i, sampleState(logNormalize(logProbs)))
    }
    
    //One pass through data, sampling state for each word from the P(t|t_-i,w)
    // which is calculated from current state
    def gibbs(state: POSstate, pos: POSdata) : Unit =
    	for (((t,wf),i) <- pos.data.view.zipWithIndex if t > 0) 
	    updateState(wf,i,state)

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
    	for (((t,wf),i) <- pos.data.view.zipWithIndex if t > 0) {
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
//	println("Loaded "+posTxt.featLexs.length+" feature sets.")
	
	val state = new POSstate(args(0).toInt+1, posTxt)
	state.initialize()
//	println("Data: " + posTxt.data)
//	println("Assignment: "+state.assign)
	var err = evaluate(state, posTxt)
	
	println("Iteration\tMany2One\tVI\tTime(s)")
	var iteration = 0
	val startTime = System.nanoTime
	println(iteration+"\t"+err._1+"\t"+err._2+"\t"+0)
	while (iteration < 10000) {
	    iteration += 1
	    gibbs(state, posTxt)
//	    println("Assignment: "+state.assign)
	    err = evaluate(state, posTxt)
//	    println("  Many to 1 error: "+err)
	    println(iteration+"\t"+err._1+"\t"+err._2+"\t"+
		    (System.nanoTime - startTime)/1e9.toDouble)
	}
    }
}
