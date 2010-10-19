import io.Source
import collection.mutable.{HashMap, ArrayBuffer}
import util.Random
import scala.math.{log,exp}

object gibbsPOS {

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

    class POSWordState (override val N: Int, pos: POSdata, 
			transP: Array[Double], emitP: Array[Double]) extends 
	  POSstate(N, pos, transP, emitP) {
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
	def remove (wf: Seq[Int], i: Int) : Unit = {
	    tTrans(assign(i-1)) -= assign(i)
	    tTrans(assign(i)) -= assign(i+1)
	    tCount -= assign(i)
	    for ((f,j) <- wf.zipWithIndex) wEmit(assign(i))(j) -= f
	}

	def add(wf: Seq[Int], i:Int, s:Int) : Unit = {
	    assign(i) = s
	    tCount += s
	    for ((f,j) <- wf.zipWithIndex) wEmit(s)(j) += f
	    if (i > 0) tTrans(assign(i-1)) += s
	    if (i < assign.length-1) tTrans(s) += assign(i+1)
	}

	//Find log probability of selecting state s at position i, emitting
	// word w
	def logProb (wf: Seq[Int], i: Int)(s:Int) : Double = {
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

    def updateState(wf: Seq[Int], i: Int, state: POSWordState) = {
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
    def gibbs(state: POSWordState, pos: POSdata) : Unit =
    	for (((t,wf),i) <- pos.data.view.zipWithIndex if t > 0) 
	    updateState(wf,i,state)

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

	val posTxt = new POSdata(options('infile).toString)
//	println("Loaded "+posTxt.featLexs.length+" feature sets.")
//	println("nLabels =  "+posTxt.tagLex.numID)
//	println("nWords =  "+posTxt.featLexs(0).numID)
	

	val state = new POSWordState(N+1, posTxt, transP, emitP)
	state.initialize()
//	println("Data: " + posTxt.data)
//	println("Assignment: "+state.assign)
	val eval = new POSEvaluate
	var err = eval.evaluate(state, posTxt)
	
	println("Iteration\tOne2One\tMany2One\tVI\tTime(s)")
	var iteration = 0
	val startTime = System.nanoTime
	println(iteration+"\t"+err._1+"\t"+err._2+"\t"+err._3+"\t"+0)
	while (iteration < maxIter)  {
	    iteration += 1
	    gibbs(state, posTxt)
//	    println("Assignment: "+state.assign)
	    err = eval.evaluate(state, posTxt)
//	    println("  Many to 1 error: "+err)
	    println(iteration+"\t"+err._1+"\t"+err._2+"\t"+err._3+"\t"+
		    (System.nanoTime - startTime)/1e9.toDouble)
	}
	eval.stateStats(state, posTxt)
    }
}
