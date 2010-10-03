#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <vector>
#include <unordered_map>
#include <string>
#include <functional>
#include <algorithm>

#include <unistd.h>
#include <cstdlib>
#include <ctime>

#include <boost/tokenizer.hpp>

/*
#include "tbb/tbb.h"
#include "tbb/parallel_for.h"
using namespace tbb;
*/

#include "random.h"

using namespace std;

typedef unordered_map<string, int> lexMap;
typedef pair<int, int> tags;
typedef vector<tags> tagged;

int N = -1;
vector<double> emitP;
vector<double> transP;

class Lexicon {
    public:
	Lexicon() {
	    nextID = 1;
	}

	int getID(const string &s) {
	    int id = -1;
	    lexMap::iterator lt = lex.find(s);
	    if (lt == lex.end()) {
		id = nextID++;
		lex.insert(make_pair(s,id));
		rev.push_back(s);
	    } else {
		id = lt->second;
	    }
	    return id;
	}

	string revID(int id) const {
	    return rev[id];
	}

	int numID() const {return nextID;}
    private:
	lexMap lex;
	vector<string> rev;
	int nextID;
};

class Counter {
    public:
	Counter(int _N, double _prior = -1.0) : 
	    N(_N), total(0), totalP(0), c(_N), prior(_prior), lp(_N) 
	{
	    if (prior > 0)
		fill(lp.begin(), lp.end(), log(prior));
	}
	
	int add (int id) {
	    c[id]++;
	    total++;
	    
	    if (prior > 0.0) {
		lp[id] = log(c[id] + prior);
		totalP = log(total + N*prior);
	    }
	}
	
	int remove(int id) {
	    c[id]--;
	    total--;
	    
	    if (prior > 0.0) {
		lp[id] = log(c[id] + prior);
		totalP = log(total + N*prior);
	    }
	}
	
	void reset () {
	    totalP = 0.0;
	    total = 0;
	    for (int i = 0; i<N; ++i) {
		c[i] = 0;
		lp[i] = 0.0;
	    }
	}

	inline int count(int id) const {return c[id];}
	inline double prob(int id) const {return lp[id];}

	inline int max() const {return *max_element(c.begin(), c.end());}

	int N, total;
	double totalP;
	double prior;
    private:
	vector<int> c;
	vector<double> lp;
};
                
void parsePrior(const string &optarg, vector<double> &prior) {
    typedef boost::tokenizer<boost::char_separator<char> > tokenizer;
    boost::char_separator<char> sep(",");
    tokenizer tokens(optarg, sep);

    int ind = 0;
    for (tokenizer::iterator tok_iter = tokens.begin();
	 tok_iter != tokens.end(); ++tok_iter) 
    {
	string s = *tok_iter;
//	cout<<" Parsing token: "<<s<<endl;
	size_t c = s.find(':');
	if (c == string::npos) {
	    //no :, so fill rest of with value
	    double v = atof(s.c_str());
//	    cout<<"  fill rest of values with "<<v<<endl;
	    for (int i = ind; i<N; ++i) prior.push_back(v);
	    break; //there shouldn't be any more prior specification
	} else {
	    int a = atoi(s.substr(0,c).c_str());
	    double v = atof(s.substr(c+1).c_str());
//	    cout<<"  "<<a<<" values of "<<v<<endl;
	    for (int i = 0; i<a; ++i) prior.push_back(v);
	    ind += c;
	}
    }
}


bool loadTrain(const char* filename, 
	       tagged &posData, Lexicon &tagLex, Lexicon &wordLex)
{
    ifstream infile(filename);
    if (!infile) return false;
    
    //special 0,0 symbol for before/after sentences
    posData.push_back(make_pair(0,0));
    
    string line;
    int w = 0, sent = 0;
    while (getline(infile, line) && !infile.eof()) {
	istringstream iss(line);
	string tag, word;
	iss>>tag>>word;
	if (tag.empty()) {
	    //special 0,0 symbol for before/after sentences
	    posData.push_back(make_pair(0,0));
	    sent++;
	} else {
	    posData.push_back(make_pair(tagLex.getID(tag),
					wordLex.getID(word)));
	    w++;
	}
    }

#ifdef __DEBUG
    cout<<"loadTrain: "<<sent<<" sentences, "<<w<<" occurences."<<endl;
    cout<<"loadTrain: "<<tagLex.numID()<<" unique tags."<<endl;
    cout<<"loadTrain: "<<wordLex.numID()<<" unique words."<<endl;
#endif

    return true;
}

void initializeState(const tagged &posData,
		     vector<int> &assignments,
		     vector<Counter *> &tCount,
		     vector<Counter *> &eCount)
{
    for (int i = 0; i<posData.size(); ++i) {
	int rTag = 0;
	if (posData[i].first != 0) {
	    rTag = rand_int()%(N-1)+1;
	}
	
	assignments[i] = rTag;
	//tag->word
	eCount[rTag]->add(posData[i].second);
	//prev->cur tag
	if (i > 0) {
	    tCount[assignments[i-1]]->add(assignments[i]);
	}
    }
}

void removeState(int i, int w,
		 const vector<int> &assignments,
		 vector<Counter *> &tCount,
		 vector<Counter *> &eCount)
{
    tCount[assignments[i-1]]->remove(assignments[i]);
    tCount[assignments[i]]->remove(assignments[i+1]);
    eCount[assignments[i]]->remove(w);
}

void addState(int i, int w,
	      const vector<int> &assignments,
	      vector<Counter *> &tCount,
	      vector<Counter *> &eCount)
{
    tCount[assignments[i-1]]->add(assignments[i]);
    tCount[assignments[i]]->add(assignments[i+1]);
    eCount[assignments[i]]->add(w);
}

inline double logProbState(int state, int word, int prevS, int nextS,
			    const vector<Counter *> &tCount, 
			    const vector<Counter *> &eCount)
{
    double lp = tCount[prevS]->prob(state);
    //double lp = log(tCount[prevS]->count(state) + transP[prevS]);
//		log(tCount[prevS]->total + N*transP); //XXX Constant

/*
    assert(eCount[state]->prior == emitP[state]);
    cout<<"State: "<<state<<endl;
    cout<<"  count: "<<eCount[state]->count(word)<<endl;
    cout<<"  prior: "<<eCount[state]->prior<<" == "<<emitP[state]<<endl;
    cout<<"  precomp: "<<eCount[state]->prob(word)<<endl;
    cout<<"  real: "<<log(eCount[state]->count(word) + emitP[state])<<endl;
    assert(eCount[state]->prob(word) ==
	   log(eCount[state]->count(word) + emitP[state]));
*/
    lp += eCount[state]->prob(word) - eCount[state]->totalP;
    //lp += log(eCount[state]->count(word) + emitP[state]) - 
    //	  log(eCount[state]->total + eCount[state]->N*emitP[state]);

//   println("  "+wEmit(s)(w) + " emissions of w from s")

    if (prevS!=state) {
	lp += tCount[state]->prob(nextS) - tCount[state]->totalP;
    } else {
	if (nextS != state) {
	    lp += tCount[state]->prob(nextS);
	} else {
	    lp += log(tCount[state]->count(nextS) + 1 + transP[state]);
	}
	lp -= log(tCount[state]->total + 1 + N*transP[state]);
    }
    //lp += log(tCount[state]->count(nextS) + 
    //	      (prevS == state and nextS == state) + transP[state]) -
    //	  log(cCount.count(state) + (prevS==state) + N*transP[state]);

    return lp;
}

double logNormalize(vector<double> &logProbs) {
    double maxLog = *max_element(logProbs.begin(), logProbs.end());
    double logSum = 0;
    for (vector<double>::iterator pi = logProbs.begin();
	 pi != logProbs.end(); ++pi) 
    {
	if (*pi - maxLog > -30)
	   logSum += exp(*pi - maxLog);
    }
    logSum = maxLog + log(logSum);
    return logSum;
/*
    for (vector<double>::iterator pi = logProbs.begin();
	 pi != logProbs.end(); ++pi)
    {
	*pi = exp(*pi - logSum);
    }
*/
}

int sample(const vector<double> &probs, const double logSum) {
    double p = rand_double();
//    double p = rand()/(RAND_MAX+1.0);
    for (int i = 0; i<probs.size(); ++i) {
//	double pi = probs[i];
	double pi = exp(probs[i] - logSum);
	if (p < pi) return i;
	p -= pi;
    }
    return -1;
}

/*
class applyLogProb {
    vector<double> *lp;
    const vector<Counter *> *tCount;
    const vector<Counter *> *eCount;
    const int prevS, nextS, word;
public:
    applyLogProb(int _word, int _prevS, int _nextS,
		 const vector<Counter *> &_tCount, 
		 const vector<Counter *> &_eCount,
		 vector<double> &logProbs) :
	word(_word), prevS(_prevS), nextS(_nextS),
	tCount(&_tCount), eCount(&_eCount), lp(&logProbs) {}

    void operator()(const blocked_range<size_t> &r) const {
	for (size_t s=r.begin(); s!=r.end(); ++s)
	    (*lp)[s-1] = logProbState(s, word, prevS, nextS,
				      *tCount, *eCount);
    }
};
*/

void updateGibbs(int i, int word, 
		 vector<int> &assignments,
		 vector<Counter *> &tCount,
		 vector<Counter *> &eCount)
{
    //Remove counts for current assignment
    removeState(i, word, assignments, tCount, eCount);

    //Calculate probability of each state given surrounding and word
    // and counts without it (up to normalizing)
    vector<double> logProbs(N-1);
    
    for (int s = 1; s<N; s++)
	logProbs[s-1] = logProbState(s, word,
				     assignments[i-1], assignments[i+1],
				     tCount, eCount);
    
/*
    static affinity_partitioner ap;
    parallel_for(blocked_range<size_t>(1,N),
		 applyLogProb(word, assignments[i-1],assignments[i+1],
			      tCount, eCount, logProbs), ap);
*/

    double logSum = logNormalize(logProbs);

/* 
    double sum = 0;
    for (vector<double>::iterator pi = logProbs.begin();
	 pi != logProbs.end(); ++pi)
	sum += *pi; 

    cout<<"Log Normalize: sum = "<<sum<<endl;
*/

    //Sample from this to assign state
    assignments[i] = sample(logProbs, logSum)+1;
    addState(i, word, assignments, tCount, eCount);
}

void updateGibbs(const tagged& posData, vector<int> &assignments,
		 vector<Counter *> &tCount,
		 vector<Counter *> &eCount)
{
    for (int i = 0; i<posData.size(); ++i) {
	if (posData[i].second != 0) {
	    updateGibbs(i, posData[i].second, assignments, tCount, eCount);
	}
    }
}

int manyToOne (Counter **tagMap) {
    int correct = 0;
    for (int i = 1; i<N; ++i) {
	correct += tagMap[i]->max();
    }
    return correct;
}


    // Variance of mutual information between tag clusters and state clusters
    //
    //  VI(Y,T) = H(Y|T) + H(T|Y)
    //  H(Y|T) = H(Y) - I(Y,T)
    //  H(T|Y) = H(T) - I(Y,T)
    //  I(Y,T) = sum(p(y,t) log(p(y,t)/p(y)p(t))
    //  H(Y) = -sum(p(y) log p(y))
    //  H(T) = -sum(p(t) log p(t))
double VI(Counter **tagMap, Counter *labelCount, int nLabels, int total) 
{
    vector<double> py(N-1);
    double HY = 0;
    for (int i = 0; i<N-1; ++i) {
	py[i] = (double)tagMap[i+1]->total / total;
	if (py[i] > 0)
	    HY -= py[i] * log(py[i])/log(2.0);
    }
//	println("  PY: "+py)
//	println("  HY: "+HY)

    vector<double> pt(nLabels-1);
    double HT = 0;
    for (int i = 0; i<nLabels-1; ++i) {
	pt[i] = (double)labelCount->count(i+1) / total;
	if (pt[i] > 0)
	    HT -= pt[i] * log(pt[i])/log(2.0);
    }

//	println("  PY: "+pt)
//	println("  HY: "+HT)
	
    double IYT = 0.0;
    for (int ti = 0; ti < N-1; ++ti) {
	for (int li = 0; li < nLabels-1; li++) {
	    double joint = (double)tagMap[ti+1]->count(li+1) / total;
	    double indep = joint / (py[ti] * pt[li]);
	    if (joint > 0) IYT += joint * log(indep)/log(2.0);
	}
    }

    return HY+HT-2.0*IYT;
}

void printEvaluateState(const vector<int> &assignments,
			const tagged &posData, const int nLabels)
{
    static Counter **tagMap = NULL;
    if (tagMap == NULL) {
	tagMap = new Counter*[N];
	for (int i = 0; i<N; ++i) {
	    tagMap[i] = new Counter(nLabels);
	}
    } else {
	for (int i = 1; i<N; ++i) tagMap[i]->reset();
    }

    static Counter *labelCount = new Counter(nLabels);
    labelCount->reset();

    int words = 0;
    for (int i = 0; i<assignments.size(); ++i) 
	if (posData[i].first != 0) {
	    words++;
	    assert(assignments[i] > 0);
//	    cout<<i<<": "<<assignments[i]<<" -> "<<posData[i].first<<"?"<<endl;
	    tagMap[assignments[i]]->add(posData[i].first);
	    labelCount->add(posData[i].first);
	}

    cout<<"\t"<<(double)manyToOne(tagMap)/words;
    
    cout<<"\t"<<VI(tagMap, labelCount, nLabels, words);
}

int main (int argc, char **argv) {
    dev_seed_rand();

    bool loadFile = false;
    int maxIter = 1000;
    
    Lexicon tagLex, wordLex;
    tagged posData;
    
    bool setEPrior = false, setTPrior = false;
    string ePriorString, tPriorString;

    int c;
    while ((c = getopt(argc, argv, "f:e:t:i:N:h?")) != -1) {
        switch (c) {
            case 'e':
		setEPrior = true;
		ePriorString = optarg;
                //parsePrior(optarg, emitP);
                break;
            case 't':
		setTPrior = true;
		tPriorString = optarg;
                //parsePrior(optarg, transP);
                break;
            case 'f':
                loadFile = loadTrain(optarg, posData, tagLex, wordLex);
                break;
            case 'i':
                maxIter = atoi(optarg);
                break;
            case 'N':
                N = atoi(optarg);
                break;
            case 'h':
            case '?':
            default:
                cerr<<"Usage: gibbsPOS <-e> <-t> <-i> -N -f"<<endl
                    <<"               -e emission prior"<<endl
                    <<"               -t transition prior"<<endl
                    <<"               -i max number of iterations"<<endl
                    <<"               -N number of states"<<endl
                    <<"               -f POS column file"<<endl;
                return 1;
        }
    }
    
    if (N == -1 or !loadFile) {
	cerr<<"Must specify N and column file at least."<<endl;
	return 1;
    }

    //for 0th reserved state, horrible hack that will introduce countless bugs
    N+=1;

    if (setEPrior) {
//	cout<<"Loading emission prior"<<endl;
	parsePrior(ePriorString, emitP);
    }
    if (setTPrior) {
//	cout<<"Loading transition prior"<<endl;
	parsePrior(tPriorString, transP);
    }

    vector<int> assignments(posData.size());
    vector<Counter *> tCount(N);
    vector<Counter *> eCount(N);

    int nWords = wordLex.numID();
    int nLabels = tagLex.numID();

    cout<<"nLabels = "<<nLabels<<endl;
    cout<<"nWords = "<<nWords<<endl;

    for (int i = 0; i<N; ++i) {
	tCount[i] = new Counter(N, transP[i]);
	eCount[i] = new Counter(nWords, emitP[i]);
    }

    initializeState(posData,assignments,tCount,eCount);
    
    cout<<fixed<<setprecision(6);

    cout<<"Iteration\tMto1\tVI\tTime(s)"<<endl;
    cout<<"0";
    printEvaluateState(assignments, posData, nLabels);
    cout<<"\t0"<<endl;
    clock_t start = clock();
    for (int i = 1; i<maxIter; ++i) {
	updateGibbs(posData, assignments, tCount, eCount);
	cout<<i;
	printEvaluateState(assignments, posData, nLabels);
	clock_t t = clock();
	cout<<"\t"<<(t-start)/(double)CLOCKS_PER_SEC<<endl;
    }
}
