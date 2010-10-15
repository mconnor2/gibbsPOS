import io.Source
import collection.mutable.{HashMap, ArrayBuffer}
import util.Random
import scala.math.{log,exp}

object gibbsPOSType {
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

    class Counter(val N:Int, prior:Double) {
    	val c = Array.fill[Int](N)(0)
    	val lp = Array.fill[Double](N)(if (prior > 0) log(prior) else 0)
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

    case class Word(ind:Int, prev:Int, next:Int, wf:List[Int])

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
	val nWords = featLexs(0).numID

	def countWords(posData:List[(Int, List[Int])]) = {
	    val t = ArrayBuffer.fill[List[Word]](nWords)(Nil);
	    var ind = 1;
	    for ((_,wf1)::(t2,wf2)::(_,wf3)::Nil <- posData.sliding(3)) { 
		if (t2 > 0) {
		    val w1 = if (wf1 == Nil) 0 else wf1.head
		    val w3 = if (wf3 == Nil) 0 else wf3.head
		    t(wf2.head) = t(wf2.head) :+ Word(ind, w1, w3, wf2)
		}
		ind += 1
	    }
//	    println("Loaded "+ind+" words...")
	    t
	}

	val wordList = countWords(data)
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

	def printCounts() = {
	    println("State -> State")
	    print("State"); for (s <- 0 until N) print("\t"+s)
	    println()
	    for (s <- 0 until N) {
		print(s);
		for (c <- tTrans(s).c) print("\t"+c)
		println()
	    }

	    println("State -> Word")
	    print("State"); for (w <- 0 until pos.nWords) print("\t"+w)
	    println()
	    for (s <- 0 until N) {
		print(s);
		for (c <- wEmit(s)(0).c) print("\t"+c)
		println()
	    }
	    println()
	}

	def printData() {
	    for ((t,wf) <- pos.data) {
		if (wf == Nil) {
		    print(t+",0=>"+assign(0)+" ")
		} else {
		    print(t+","+wf.head+"=>"+assign(wf.head)+" ")
		}
	    }
	}

	//Initialize with uniformly random state assignments to all
	// words
	def initialize() = {
	    //word 0 assigned to state 0
	    assign += 0
	    for (_ <- 1 until pos.nWords) assign += Random.nextInt(N-1)+1

	    var prevS = -1
	    for ((t,wf) <- pos.data) {
		val s = if (wf == Nil) 0 else assign(wf.head)
		tCount += s
		for ((f,i) <- wf.zipWithIndex) wEmit(s)(i) += f
		if (prevS >= 0) tTrans(prevS) += s
		prevS = s
	    }
	}

	//Remove state assignment at position i, which emits word w
	def removeWord (s:Int, wc:Int)(word: Word) : Unit = word match {
	    case Word(i,p,n,wf) => {
//		println("  Word "+wc+" appears at "+i+" p_n: "+p+", "+n)
//		println("   p assign "+assign(p)+" count: "+tTrans(assign(p))(s))
//		println("   n assign "+assign(n)+" count: "+tTrans(s)(assign(n)))
//		println("     -")
		if (wc != p) tTrans(assign(p)) -= s
		tTrans(s) -= assign(n)
		tCount -= s
		for ((f,j) <- wf.zipWithIndex) wEmit(s)(j) -= f
//		println("   p assign "+assign(p)+" count: "+tTrans(assign(p))(s))
//		println("   n assign "+assign(n)+" count: "+tTrans(s)(assign(n)))
	    }
	    case _ => {throw new Error("Lets Hope Not...")}
	}
	def remove (wf: List[Word], wt:Int, s: Int) : Unit = 
	    wf.foreach(removeWord(s, wt))

	def addWord (s:Int, wc:Int)(word: Word) : Unit = word match {
	    case Word(i,p,n,wf) => {
//		println("  appears at "+i+" p_n: "+p+", "+n+" adding state "+s)
//		println("   p assign "+assign(p)+" count: "+tTrans(assign(p))(s))
//		println("   n assign "+assign(n)+" count: "+tTrans(s)(assign(n)))
//		println("     +")
		if (wc != p) tTrans(assign(p)) += s
		tTrans(s) += assign(n)
		tCount += s
		for ((f,j) <- wf.zipWithIndex) wEmit(s)(j) += f
//		println("   p assign "+assign(p)+" count: "+tTrans(assign(p))(s))
//		println("   n assign "+assign(n)+" count: "+tTrans(s)(assign(n)))
	    }
	    case _ => {throw new Error("Lets Hope Not...")}
	}

	def add(wf: List[Word], wt:Int, s:Int) : Unit = {
	    assign(wt) = s
	    wf.foreach(addWord(s, wt))
	}
	
	//Find log probability of selecting state s at position i, emitting
	// word w
	def logProbWord(s:Int, origS:Int)(word:Word) : Double = word match {
	    case Word(_,p,n,wf) => {
	    
	    var logP = tTrans(assign(p)).logP(s)
	    //var logP = log(tTrans(abefore)(s) + transP(abefore))
		  //   log(tCount(abefore) + N*transP(abefore)) //XXX Constant

//	    for ((f,j) <- wf.zipWithIndex) 
//		logP += wEmit(s)(j).logP(f) - wEmit(s)(j).totalP
		//logP += log(wEmit(s)(j)(v) + emitP(s)) - 
		//	log(wEmit(s)(j).total+pos.featLexs(j).numID*emitP(s))

	    //Since we have removed all links between current word and
	    // any state emission, the probability of emitting this word is
	    // 1 + prior / (total + prior)?
	    logP += log(1 + emitP(s)) - wEmit(s)(0).totalP

//	    println("  "+wEmit(s)(w) + " emissions of w from s")
   
	    logP += tTrans(s).logP(assign(n)) - tTrans(s).totalP
/*
	    if (assign(p) != s) {
		logP += tTrans(s).logP(assign(n)) - tTrans(s).totalP
	    } else {
		logP -= log(tCount(s) + 1 + N*transP(s))
		logP += (if (assign(n) != s) tTrans(s).logP(assign(n))
		        else log(tTrans(s)(assign(n)) + 1 + transP(s)))
	    }
*/
	    if (logP.isNaN) {
	    println("  Log probability of assigning state "+s+
		    ", word before:after = "+p+":"+n)
	    println("   Original state assignment: "+origS)
	    println("   p assigned: "+assign(p))
	    println("   n assigned: "+assign(n))
	    println("  "+tTrans(assign(p))(s) + " # transitions from p to s")
	    println("  "+tTrans(assign(p)).logP(s) + " log Prob transition from p to s")
	    println("  "+tTrans(s)(assign(n))+ " # transitions from s to n")
	    println("  "+tTrans(s).logP(assign(n))+ " log Prob transition from s to n")
	    }

	    logP
	    }
	}
	
	def logProbWordFeat(word:Word,s:Int):Double = word match {
	    case Word(_,_,_,wf) => {
		(for ((f,j) <- wf.zipWithIndex; if j > 0) 
		     yield (wEmit(s)(j).logP(f) - wEmit(s)(j).totalP)).sum
	    }
	}

	def logProbType(wf: List[Word], origS: Int)(s:Int) : Double = { 
	    val wordLogP = logProbWordFeat(wf.head,s) 
	    if (wordLogP.isNaN) {
		println("Word probability NaN? state "+s)
		wf.head match { case Word(_,_,_,wf) => {
		    for ((f,j) <- wf.zipWithIndex) {
			println("  Feat: "+j+" "+s+
				" state count: "+wEmit(s)(j)(f))
			println("    log = "+wEmit(s)(j).logP(f))
			println("    total prob: "+wEmit(s)(j).totalP)
		    }
		}}
	    }

	   wordLogP + wf.map(logProbWord(s,origS)).sum
	}
    }

    //Converted from Aria Haghihi's standard ML code (as used elsewhere

    def logNormalize (logs:Seq[Double]) = {
	def logSum (logs:Seq[Double]) : Double = {
	    val maxLog = logs.max
	    maxLog + log(logs.foldLeft(0.0)((b,x) => b +
			    (if (x != Double.NaN && x-maxLog > -30) 
				exp(x-maxLog) 
			     else 0)))
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

    def updateStateType(wf: List[Word], wi: Int, state: POSstate) = {
//	print("Current data: "); state.printData(); println()
//
//	println("Updating state for word "+wi)
//	println("  currently assigned "+state.assign(wi))
	
	val origS = state.assign(wi)

	//Remove counts for current assignment
	state.remove(wf, wi, state.assign(wi))
    
//	println("After removing:")
//	state.printCounts

	//Calculate probability of each state given surrounding and word
	// and counts without it (up to normalizing)
	//val logProbs = (1 until state.N).map(state.logProbType(wf))
	val logProbs = for (newState <- (1 until state.N)) yield {
//	    state.add(wf, wi, newState)
	    state.assign(wi) = newState
	    val lp = state.logProbType(wf, origS)(newState)
//	    state.remove(wf, wi, newState)
	    lp
	}

//	println("Log probabilities: "+logProbs)
//	val lognorm = logNormalize(logProbs)
//	println("    normalized: "+lognorm)
//	println("    sum: "+lognorm.reduceLeft(_+_))
	//Sample from this to assign state
	val newS = sampleState(logNormalize(logProbs))
//	println("Assigned state "+newS)
	state.add(wf, wi, newS)
//	println("After adding:")
//	state.printCounts
    }
    
    //One pass through data, sampling tag for all occurences of each
    // word type. P(t|t_-i,w) which is calculated from current state
    def gibbsType(state: POSstate, pos: POSdata) : Unit =
    	for (wt <- (1 until pos.nWords)) { //Random.shuffle((1 until pos.nWords).toIterator)) {
//	    println("Word type "+wt);
	    updateStateType(pos.wordList(wt), wt, state)
	}

    //For each state, find the tag it is seen with most often, and count
    // that as correct
    def manyToOne(tagMap:Seq[Counter]):Int = 
	tagMap.foldLeft(0)(_+_.c.max)

    //Repeatedly find the max pairing of state and tag, removing those
    // from further consideration until all of one or the other has been
    // assigned
    def oneToOne(tagMap:Seq[Counter]):Int = {
	def findMax(tags:Set[Int], labels:Set[Int], correct:Int):Int = {
	    if (tagMap.size == tags.size || 
		labels.size == tagMap(0).N) correct
	    else {
		var max = -1
		var maxT = -1
		var maxL = -1
		for ((tmap, t) <- tagMap.zipWithIndex; if (!tags.contains(t))) {
		    (for ((c, lab) <-  tmap.c.zipWithIndex
			 if (!labels.contains(lab))) yield (c,lab)).
			 max(Ordering.by((p:(Int,Int))=>p._1)) match {
			case (maxC, maxLabel) => 
			    if (maxC > max) {
				max = maxC; maxT = t; maxL = maxLabel
			    }
		    }
		}
		findMax(tags + maxT, labels + maxL, correct + max)
	    }
	}
	findMax(collection.immutable.HashSet.empty,
		collection.immutable.HashSet.empty, 0)
    }

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
    	for ((t,w::_) <- pos.data if t > 0) {
	    tagMap(state.assign(w)) += t
	    tagCount += t
	    length += 1
	}

	val oneError = oneToOne(tagMap)
	val manyError = manyToOne(tagMap)
	val vi = VI(tagMap,tagCount,length)

	(oneError.toDouble / length.toDouble,
	 manyError.toDouble / length.toDouble, vi)
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
	    wordMap += new Counter(pos.nWords, 0.0)
	}
    	
	for ((t,w::_) <- pos.data if t > 0) {
	    tagMap(state.assign(w)) += t
	    wordMap(state.assign(w)) += w
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
	
	println("Iteration\tOne2One\tMany2One\tVI\tTime(s)")
	var iteration = 0
	val startTime = System.nanoTime
	println(iteration+"\t"+err._1+"\t"+err._2+"\t"+err._3+"\t"+0)
	while (iteration < maxIter)  {
	    iteration += 1
	    gibbsType(state, posTxt)
//	    println("Assignment: "+state.assign)
	    err = evaluate(state, posTxt)
//	    println("  Many to 1 error: "+err)
	    println(iteration+"\t"+err._1+"\t"+err._2+"\t"+err._3+"\t"+
		    (System.nanoTime - startTime)/1e9.toDouble)
	}
//	stateStats(state, posTxt)
    }
}
