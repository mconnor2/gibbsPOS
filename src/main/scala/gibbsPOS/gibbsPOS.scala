import io.Source
import collection.mutable.{HashMap, ArrayBuffer}
import util.Random
import scala.math.{log,exp}

object gibbsPOS {
    class Lexicon[A] {
	val map = new HashMap[A,Int]
	val revMap = new ArrayBuffer[A]
	private var nextID = 1
	def apply(n:A):Int = 
	    map.get(n) match {
		case Some(i) => i
		case None => {
		    map.put(n,nextID)
		    revMap += n
		    nextID += 1
		    nextID-1
		}
	    }
	def rev(n:Int):Option[A] = 
	    if (n > 0) Some(revMap(n-1)) else None
	
	def numID = nextID //Includes the 0th ID
    }

    class Counter(N:Int, prior:Double) {
    	val c = new Array[Int](N)
    	val lp = new Array[Double](N)
//    	val c = ArrayBuffer.fill[Int](N)(0)
//    	val lp = ArrayBuffer.fill[Double](N)(0.0)
	var total = 0
	var totalP = 0.0

	def apply(n:Int) = c(n)
	def logP(n:Int) = lp(n)

	def +=(a:Int) = {
	    total += 1; 
	    c(a) = c(a) + 1
	    totalP = log(total + N*prior)
	    lp(a) = log(c(a) + prior)
	}
	def ++=(s:Seq[Int]) = for (a <- s) {this += a}
	def -=(a:Int) = {
	    total -= 1
	    c(a) = c(a)-1
	    totalP = log(total + N*prior)
	    lp(a) = log(c(a) + prior)
	}
	def --=(s:Seq[Int]) = for (a <- s) {this -= a}
    }

    class POSdata(file:String) {
	val featLexs = new ArrayBuffer[Lexicon[String]]
	val tagLex = new Lexicon[String]

	//Given a line from a column file, return (tag id, feature list) pair
	//  if a blank line return (0,Nil) special marker
	def POSColPair(line:String):(Int, List[Int]) = {
	    val cols = line.split("\\s+")
	    if (cols.length < 2) (0,Nil)
	    else (tagLex(cols(0)),
		  cols.tail.zipWithIndex.map(p => {
		      if (featLexs.length <= p._2) 
			  featLexs += new Lexicon[String]
//		      p._1.split(',').map(featLexs(p._2).apply).toSeq
		      featLexs(p._2)(p._1)}).toList)
	}

	def load(source:Source):List[(Int, List[Int])] = 
	    List((0,Nil)) ++ source.getLines.map(POSColPair).toList
	
	val data = load(Source.fromFile(file))
	val nLabels = tagLex.numID
    }

    var emitP = Array.empty[Double]
    var transP= Array.empty[Double]

    //Parse prior specification of type n1:v1,n2:v2,v3
    //  which gives block structure of n1 copies of v1, n2 of v2, N-n1-n2 of v3
    //  Does no checking to make sure well formed or adds up to N
    def parsePrior(string: String, N: Int):Array[Double] = {
	if (!string.contains(':')) Array.fill(N)(string.toDouble)
	else {
	    val p = new Array[Double](N)
	    var ind = 0
	    for (s <- string.split(',')) {
		if (!s.contains(':')) 
		    for (i <- (ind until N)) p(i) = s.toDouble
		else {
		    //case of n:v
		    val v = s.split(':')
		    for (i <- (0 until v(0).toInt)) p(i+ind) = v(1).toDouble
		    ind += v(0).toInt
		}
	    }
	    p
	}
    }

    class POSstate (val N: Int, pos: POSdata) {
	val assign = new ArrayBuffer[Int]
	
	//state t->t' transition count
	val tTrans = new ArrayBuffer[Counter]
	for (i <- 0 until N) tTrans += new Counter(N, transP(i))

	//state t->word w transition count
	val wEmit = new ArrayBuffer[ArrayBuffer[Counter]]
	for (i <- 0 until N) {
	    wEmit += new ArrayBuffer[Counter]
	    for (f <- pos.featLexs) 
		wEmit(i) += new Counter(f.numID, emitP(i))
	}

	//state t count
	val tCount = new Counter(N,0.0)

	//Initialize with uniformly random state assignments to all
	// words
	def initialize() = {
	    for (((t,wf),i) <- pos.data.view.zipWithIndex) {
		val s = if (t == 0) 0 else (Random.nextInt(N-1)+1)
		assign += s
		tCount += s
		for ((f,i) <- wf.zipWithIndex) wEmit(s)(i) += f
		if (i > 0) tTrans(assign(i-1)) += s
	    }
	}

	//Remove state assignment at position i, which emits word w
	def remove (wf: List[Int], i: Int) : Unit = {
	    tTrans(assign(i-1)) -= assign(i)
	    tTrans(assign(i)) -= assign(i+1)
	    tCount -= assign(i)
	    for ((f,j) <- wf.zipWithIndex) wEmit(assign(i))(j) -= f
	}

	def add(wf: List[Int], i:Int, s:Int) : Unit = {
	    assign(i) = s
	    tCount += s
	    for ((f,j) <- wf.zipWithIndex) wEmit(s)(j) += f
	    if (i > 0) tTrans(assign(i-1)) += s
	    if (i < assign.length-1) tTrans(s) += assign(i+1)
	}

	//Find log probability of selecting state s at position i, emitting
	// word w
	def logProb (wf: List[Int], i: Int)(s:Int) : Double = {
//	    println("Log probability of assigning state "+s+
//		    " at index "+i+" emitting word "+w)
	    
	    var logP = tTrans(assign(i-1)).logP(s)
	    //var logP = log(tTrans(abefore)(s) + transP(abefore))
		  //   log(tCount(abefore) + N*transP(abefore)) //XXX Constant

//	    println("  "+tTrans(assign(i-1))(s) + " # transitions from -1 to s")
	    for ((f,j) <- wf.zipWithIndex) 
		logP += wEmit(s)(j).logP(f) - wEmit(s)(j).totalP
		//logP += log(wEmit(s)(j)(v) + emitP(s)) - 
		//	log(wEmit(s)(j).total+pos.featLexs(j).numID*emitP(s))

//	    println("  "+wEmit(s)(w) + " emissions of w from s")
	    if (assign(i-1) != s) {
		logP + tTrans(s).logP(assign(i+1)) - tTrans(s).totalP
	    } else {
		logP -= log(tCount(s) + 1 + N*transP(s))
		logP + (if (assign(i+1) != s) tTrans(s).logP(assign(i+1))
		        else log(tTrans(s)(assign(i+1)) + 1 + transP(s)))
	    }
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

    def updateState(wf: List[Int], i: Int, state: POSstate) = {
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
	for (i <- 0 until state.N) tagMap += new Counter(pos.nLabels, 0.0)
	
	var length = 0
	val tagCount = new Counter(pos.nLabels, 0.0)
    	for (((t,wf),i) <- pos.data.view.zipWithIndex if t > 0) {
	    tagMap(state.assign(i)) += t
	    tagCount += t
	    length += 1
	}

	val manyError = manyToOne(tagMap)
	val vi = VI(tagMap,tagCount,length)
	(manyError.toDouble / length.toDouble, vi)
    }

    def stateStats(state: POSstate, pos: POSdata) = {
	def printTopN[A](N:Int, count: Counter, lex: Lexicon[A]) {
	    for (t <- (1 until lex.numID).
			sortWith((a,b) => count(a) > count(b)).take(N)) {
		lex.rev(t) match {
		    case Some(s) => print(" "+s)
		    case None => print(" ???")
		}
		print("("+count(t)+")")
	    }
	}

	def printStats(s: Int, tags: Counter, words: Counter) = {
	    println("State "+s+": ")
	    println("  "+tags.total+" occurences")
	    println("  "+tags.c.count(_>0)+" unique POS tags")
	    println("  "+words.c.count(_>0)+" unique Words")
	    
	    print("  Top 10 tags:")
	    printTopN(10,tags, pos.tagLex)
	    println()
	    
	    print("  Top 10 words:")
	    printTopN(10, words, pos.featLexs(0))
	    println()
	}

	//Find counts for mapping each tag to an HMM state
	val tagMap = new ArrayBuffer[Counter]
	val wordMap = new ArrayBuffer[Counter]
	for (i <- 0 until state.N) {
	    tagMap += new Counter(pos.nLabels, 0.0)
	    wordMap += new Counter(pos.featLexs(0).numID, 0.0)
	}
    	
	for (((t,wf),s) <- pos.data.view.zip(state.assign) if t > 0) {
	    tagMap(s) += t
	    wordMap(s) += wf.head
	}
	
	for (i <- 1 until state.N) 
	    printStats(i, tagMap(i), wordMap(i))
    }

    def main(args: Array[String]): Unit = {
	if (args.length < 1) throw new Error("Usage: gibbsPOS <N> <POS col>")
	val arglist = args.toList
	type OptionMap = Map[Symbol, String]

	var maxIter = 10000

	def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
	    def isSwitch(s : String) = (s(0) == '-')
	    list match {
		case Nil => map
		case "-N" :: value :: tail => 
                    nextOption(map ++ Map('N -> value), tail)
		case "-emit" :: value :: tail => 
                    nextOption(map ++ Map('emit -> value), tail)
		case "-trans" :: value :: tail => 
                    nextOption(map ++ Map('trans -> value), tail)
		case "-iter" :: value :: tail => 
		    {maxIter = value.toInt; nextOption(map, tail)}
		case string :: opt2 :: tail if isSwitch(opt2) => 
                    nextOption(map ++ Map('infile -> string), list.tail)
		case string :: Nil =>  
		    nextOption(map ++ Map('infile -> string), list.tail)
		case option :: tail => {
		    println("Unknown option "+option) 
                    exit(1)
		}
	    }
	}
	val options = nextOption(Map(),arglist)
	println(options)

	if (!options.contains('N) || !options.contains('emit) ||
	    !options.contains('trans) || !options.contains('infile)) {
	    println("Must specify N, emission and transition prior, plus infile")
	    exit(1)
	}

	val N = options('N).toInt

	emitP = parsePrior(options('emit), N+1)
	transP = parsePrior(options('trans), N+1)

//	println("Emit: "+emitP.length)
//	println("Trans: "+transP.length)

	val posTxt = new POSdata(options('infile).toString)
//	println("Loaded "+posTxt.featLexs.length+" feature sets.")
//	println("nLabels =  "+posTxt.tagLex.numID)
//	println("nWords =  "+posTxt.featLexs(0).numID)
	
	val state = new POSstate(N+1, posTxt)
	state.initialize()
//	println("Data: " + posTxt.data)
//	println("Assignment: "+state.assign)
	var err = evaluate(state, posTxt)
	
	println("Iteration\tMany2One\tVI\tTime(s)")
	var iteration = 0
	val startTime = System.nanoTime
	println(iteration+"\t"+err._1+"\t"+err._2+"\t"+0)
	while (iteration < maxIter)  {
	    iteration += 1
	    gibbs(state, posTxt)
//	    println("Assignment: "+state.assign)
	    err = evaluate(state, posTxt)
//	    println("  Many to 1 error: "+err)
	    println(iteration+"\t"+err._1+"\t"+err._2+"\t"+
		    (System.nanoTime - startTime)/1e9.toDouble)
	}
	stateStats(state, posTxt)
    }
}
