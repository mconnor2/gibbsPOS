GibbsPOS
========

Experimental code to run gibbs sampling for unsupervised part of speech, in
both scala and c++ flavors.  Code mostly implements a combination of models
from [Crouching Dirichlet, Hidden Markov Model][1] by Moon, Erk and Baldridge
and [Simple Type-Level Unsupervised POS Tagging][2] by Lee, Haghighi and
Barzilay.  In fact the original inspiration of the code came from a post in [Haghighi's blog][3] where he walked through an implementation of the ideas in his paper in Clojure.

[1]: http://comp.ling.utexas.edu/~tsmoon/moon_emnlp_2010.pdf
[2]: http://people.csail.mit.edu/regina/my_papers/typetagging.pd
[3]: http://aria42.com/blog/?p=48

Installing
----------

There are two seperate implementations in this project.  The original and
fuller Scala version uses SBT to compile.  Hopefully that actually works.

For the C++ code just running make should suffice (you may want to change
compiler parameters in the Makefile).  The C++ version was made after the
original Scala version proved to be a bit slow.  I was curious how much of a
slow down actually existed, so I reimplimented it in C++ and acheived a
considerable speedup (25-30x) with virtually the same code.  After having less
than sucess using java profilers on scala code, I still haven't quite tracked
down where the code is spending so much time.  My current guess is somewhere in
auto boxing and unboxing of scala types, but I am not positive.

Running
-------

Both basic versions of gibbsPOS take the same arguments (short form for c++, longer form in parenthesis for scala code):

* -f (or last argument in scala) the POS column file for training
* -N number of hidden states
* -i (-iter) maximum number of iterations
* -e (-emit) Emission prior
* -t (-trans) Transition prior

The specification of the Emission and Transition priors contains the magic of
the Crouching Dirichlet paper; different priors can be specified for different
states.  The rough intuition behind a different prior for each state is so that
most states will emit fewer words (closed class), while some may emit many
words (open class).  The format for specifying this prior on the command line
is as follows:

-e n_1:p_1,n_2:p_2,...,p 

so the first n_1 states get prior p_1, the next n_2 states get prior p_2, and
the rest of the states get default prior p.  For example, to specify that the
first five states are open class and thus have a flatter prior, while the rest
of the states have a spikier prior, we would use (annoyingly I coded the 0th
state as a special sentence boundary state, so need to include a count for it
at the beginning):

./gibbsPOS -N 50 -e 1:0.001,5:0.1,0.0001 -t 0.1 -t 10000 -f training file

The column format specifies one word per line, empty line between sentences.
In each line the first column is the POS tag, second column is word.  If there
are any other features those can be included in other columns and they may be
used during inference, although I have not had much luck with extra features in
this simple model.


