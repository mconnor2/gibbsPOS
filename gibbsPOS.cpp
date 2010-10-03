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

#include "random.h"

using namespace std;

typedef unordered_map<string, int> lexMap;
typedef pair<int, int> tags;
typedef vector<tags> tagged;

int N = -1;
double emitP = 0.001;
double transP = 0.1;

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
	Counter(int _N, double _prior = transP) : 
	    N(_N), total(0), totalP(0), c(_N), lp(_N), prior(_prior) {}
	
	int add (int id) {
	    c[id]++;
	    total++;
	    
	    lp[id] = log(c[id] + prior);
	    totalP = log(total + N*prior);
	}
	
	int remove(int id) {
	    c[id]--;
	    total--;
	    
	    lp[id] = log(c[id] + prior);
	    totalP = log(total + N*prior);
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
	inline int prob(int id) const {return lp[id];}

	inline int max() const {return *max_element(c.begin(), c.end());}

	int N, total;
	double totalP;
    private:
	vector<int> c;
	vector<double> lp;
	double prior;
};


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

void removeState(int s, int w,
		 const vector<int> &assignments,
		 vector<Counter *> &tCount,
		 vector<Counter *> &eCount)
{
    tCount[assignments[s-1]]->remove(assignments[s]);
    tCount[assignments[s]]->remove(assignments[s+1]);
    eCount[assignments[s]]->remove(w);
}

void addState(int s, int w,
	      const vector<int> &assignments,
	      vector<Counter *> &tCount,
	      vector<Counter *> &eCount)
{
    tCount[assignments[s-1]]->add(assignments[s]);
    tCount[assignments[s]]->add(assignments[s+1]);
    eCount[assignments[s]]->add(w);
}

double logProbState(int state, int word, int prevS, int nextS,
		    const vector<Counter *> &tCount, 
		    const vector<Counter *> &eCount)
{
    double lp = tCount[prevS]->prob(state);
    //double lp = log(tCount[prevS]->count(state) + transP);
//		log(tCount[prevS]->total + N*transP); //XXX Constant

    lp += eCount[state]->prob(word) - eCount[state]->totalP;
    //lp += log(eCount[state]->count(word) + emitP) - 
	  //log(eCount[state]->total + eCount[state]->N*emitP);

//   println("  "+wEmit(s)(w) + " emissions of w from s")
    if (prevS!=state) {
	lp += tCount[state]->prob(nextS) - tCount[state]->totalP;
    } else {
	if (nextS != state) {
	    lp += tCount[state]->prob(nextS);
	} else {
	    lp += log(tCount[state]->count(nextS) + 1 + transP);
	}
	lp -= log(tCount[state]->total + 1 + N*transP);
    }

    return lp;
}

void logNormalize(vector<double> &logProbs) {
    double maxLog = *max_element(logProbs.begin(), logProbs.end());
    double logSum = 0;
    for (vector<double>::iterator pi = logProbs.begin();
	 pi != logProbs.end(); ++pi) 
    {
	if (*pi - maxLog > -30)
	   logSum += exp(*pi - maxLog);
    }
    logSum = maxLog + log(logSum);

    for (vector<double>::iterator pi = logProbs.begin();
	 pi != logProbs.end(); ++pi)
    {
	*pi = exp(*pi - logSum);
    }
}

int sample(const vector<double> &probs) {
    double p = rand_double();
    for (int i = 0; i<probs.size(); ++i) {
	if (p <= probs[i]) return i;
	p -= probs[i];
    }
    return -1;
}

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
    logNormalize(logProbs);
/* 
    double sum = 0;
    for (vector<double>::iterator pi = logProbs.begin();
	 pi != logProbs.end(); ++pi)
	sum += *pi; 

    cout<<"Log Normalize: sum = "<<sum<<endl;
*/

    //Sample from this to assign state
    assignments[i] = sample(logProbs)+1;
    addState(i, word, assignments, tCount, eCount);
}

void updateGibbs(const tagged& posData, vector<int> &assignments,
		 vector<Counter *> &tCount,
		 vector<Counter *> &eCount)
{
    for (int i = 0; i<posData.size(); ++i) {
	if (posData[i].second != 0)
	    updateGibbs(i, posData[i].second, assignments, tCount, eCount);
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

    int c;
    while ((c = getopt(argc, argv, "f:e:t:i:N:h?")) != -1) {
        switch (c) {
            case 'e':
                emitP = atof(optarg);
                break;
            case 't':
                transP = atof(optarg);
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

    vector<int> assignments(posData.size());
    vector<Counter *> tCount(N);
    vector<Counter *> eCount(N);

    int nWords = wordLex.numID();
    int nLabels = tagLex.numID();

    for (int i = 0; i<N; ++i) {
	tCount[i] = new Counter(N, transP);
	eCount[i] = new Counter(nWords, emitP);
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
