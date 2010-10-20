import io.Source
import collection.mutable.{HashMap, ArrayBuffer}
import scala.math.{log}

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



class POSdata(file:String) {
    val featLexs = new ArrayBuffer[Lexicon[String]]
    val tagLex = new Lexicon[String]

    //Given a line from a column file, return (tag id, feature list) pair
    //  if a blank line return (0,Nil) special marker
    def POSColPair(line:String):(Int, Array[Int]) = {
	val cols = line.split("\\s+")
	//if (cols.length < 2) (0,Nil)
	if (cols.length < 2) (0,Array.empty[Int])
	else (tagLex(cols(0)),
	      cols.tail.zipWithIndex.map(p => {
		  if (featLexs.length <= p._2) 
		      featLexs += new Lexicon[String]
//		      p._1.split(',').map(featLexs(p._2).apply).toSeq
		  featLexs(p._2)(p._1)}).toArray)
    }

    def load(source:Source) = //ArrayBuffer[(Int, Array[Int])] = 
//	ArrayBuffer((0,Nil)) ++ source.getLines.map(POSColPair)
//	ArrayBuffer((0,Array.empty[Int])) ++ source.getLines.map(POSColPair)
//	List((0,Nil)) ++ source.getLines.map(POSColPair).toList
	List((0,Array.empty[Int])) ++ source.getLines.map(POSColPair).toList
    
    val data = load(Source.fromFile(file))
    val nLabels = tagLex.numID
    val nWords = featLexs(0).numID

}

class POSstate (val N: Int, pos: POSdata, 
		transP:Array[Double], emitP:Array[Double]) {
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
//	    if (wf == Nil) {
	    if (t == 0) {
		print(t+",0=>"+assign(0)+" ")
	    } else {
		print(t+","+wf.head+"=>"+assign(wf.head)+" ")
	    }
	}
    }
}


class POSEvaluate {
    
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
	val wordMap = new ArrayBuffer[Counter]
	for (i <- 0 until state.N) {
	    tagMap += new Counter(pos.nLabels, 0.0)
	    wordMap += new Counter(pos.nWords, 0.0)
	}
	
	val tagCount = new Counter(pos.nLabels, 0.0)
	
	var length = accumulate(state, pos, tagMap, tagCount, wordMap)

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
	val tagCount = new Counter(pos.nLabels, 0.0)

	accumulate(state, pos, tagMap, tagCount, wordMap)
	
	for (i <- 1 until state.N) 
	    printStats(i, tagMap(i), wordMap(i))
    }

    def accumulate(state:POSstate, pos:POSdata,
		   tagMap: ArrayBuffer[Counter],
		   tagCount: Counter,
		   wordMap: ArrayBuffer[Counter]):Int = {
	var length = 0
	for (((t,wf), s) <- pos.data.view.zip(state.assign) if t > 0) {
	    tagMap(s) += t
	    tagCount += t
	    wordMap(s) += wf.head
	    length += 1
	}
	length
    }
}
