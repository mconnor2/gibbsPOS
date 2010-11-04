import io.Source
import collection.mutable.{HashMap, ArrayBuffer}
import util.Random
import scala.math.{log,exp}

object gibbsPOSType {

    case class Word(ind:Int, prev:Int, next:Int, wf:List[Int])
    //case class Word(ind:Int, prev:Int, next:Int, wf:Array[Int])

    class POSTypeData(file:String) extends POSdata(file) {
	def countWords(posData:Seq[(Int, List[Int])]) = {
	    val t = ArrayBuffer.fill[List[Word]](nWords)(Nil);
	    var ind = 1;
	    for ((t1,wf1)::(t2,wf2)::(t3,wf3)::Nil <- posData.sliding(3)) { 
		if (t2 > 0) {
		    val w1 = if (t1 == 0) 0 else wf1.head
		    val w3 = if (t3 == 0) 0 else wf3.head
//		    val w1 = if (wf1 == Nil) 0 else wf1.head
//		    val w3 = if (wf3 == Nil) 0 else wf3.head
		    t(wf2.head) = t(wf2.head) :+ Word(ind, w1, w3, wf2)
		}
		ind += 1
	    }
    //	    println("Loaded "+ind+" words...")
	    t
	}

	val wordList = countWords(data)
    }


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

    class POSTypeState (override val N: Int, pos: POSTypeData, 
			transP:Array[Double], emitP:Array[Double]) extends 
			POSstate(N, pos, transP, emitP) {

	//Initialize with uniformly random state assignments to all
	// words
	def initialize() = {
	    //word 0 assigned to state 0
	    assign += 0
	    for (_ <- 1 until pos.nWords) assign += Random.nextInt(N-1)+1

	    var prevS = -1
/* 
	    for ((t,wf) <- pos.data) {
		val s = if (t == 0) 0 else assign(wf.head)
		tCount += s
		for ((f,i) <- wf.zipWithIndex) wEmit(s)(i) += f
		if (prevS >= 0) tTrans(prevS) += s
		prevS = s
	    }
*/	    
	    for ((ws,i) <- pos.wordList.view.zipWithIndex; w <- ws if i > 0) {
	    	addWord(assign(i), i)(w)
	    }
	}

	//Remove state assignment at position i, which emits word w
	def removeWord (s:Int, wc:Int)(word: Word) : Unit = word match {
	    case Word(i,p,n,wf) => {
//		println("  Word "+wc+" appears at "+i+" p_n: "+p+", "+n)
//		println("   p assign "+assign(p)+" count: "+tTrans(assign(p))(s))
//		println("   n assign "+assign(n)+" count: "+tTrans(s)(assign(n)))
//		println("     -")
		//if (wc != p) tTrans(assign(p)) -= s
		tTrans(assign(p)) -= s
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
		//if (wc != p) tTrans(assign(p)) += s
		tTrans(assign(p)) += s
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

    def updateStateType(wf: List[Word], wi: Int, state: POSTypeState) = {
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
    def gibbsType(state: POSTypeState, pos: POSTypeData) : Unit =
    	//for (wt <- (1 until pos.nWords)) { 
	for (wt <- Random.shuffle((1 until pos.nWords).toIterator)) {
//	    println("Word type "+wt);
	    updateStateType(pos.wordList(wt), wt, state)
	}

    class POSTypeEvaluate extends POSEvaluate {
	override def accumulate(state:POSstate, pos:POSdata,
		       tagMap: ArrayBuffer[Counter],
		       tagCount: Counter,
		       wordMap: ArrayBuffer[Counter]):Int = {
	    var length = 0
//	    for ((t,w::_) <- pos.data if t > 0) {
	    for ((t,wf) <- pos.data if t > 0) {
		val w = wf.head
		tagMap(state.assign(w)) += t
		wordMap(state.assign(w)) += w
		tagCount += t
		length += 1
	    }
	    length
	}
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

	val emitP = parsePrior(options('emit), N+1)
	val transP = parsePrior(options('trans), N+1)

//	println("Emit: "+emitP.length)
//	println("Trans: "+transP.length)

	val posTxt = new POSTypeData(options('infile).toString)
//	println("Loaded "+posTxt.featLexs.length+" feature sets.")
//	println("nLabels =  "+posTxt.tagLex.numID)
//	println("nWords =  "+posTxt.featLexs(0).numID)
	
	val state = new POSTypeState(N+1, posTxt, transP, emitP)
	state.initialize()
//	println("Data: " + posTxt.data)
//	println("Assignment: "+state.assign)
	val eval = new POSTypeEvaluate
	var err = eval.evaluate(state, posTxt)
	
	println("Iteration\tOne2One\tMany2One\tVI\tTime(s)")
	var iteration = 0
	val startTime = System.nanoTime
	println(iteration+"\t"+err._1+"\t"+err._2+"\t"+err._3+"\t"+0)
	while (iteration < maxIter)  {
	    iteration += 1
	    gibbsType(state, posTxt)
//	    println("Assignment: "+state.assign)
	    err = eval.evaluate(state, posTxt)
//	    println("  Many to 1 error: "+err)
	    println(iteration+"\t"+err._1+"\t"+err._2+"\t"+err._3+"\t"+
		    (System.nanoTime - startTime)/1e9.toDouble)
	}
	eval.stateStats(state, posTxt)
    }
}
