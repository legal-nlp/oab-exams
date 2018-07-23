#!/usr/bin/python3

import nltk
import os, argparse, json, re, math, statistics, sys

### from: http://www.aclweb.org/anthology/P89-1010.pdf
# How to calculate PMI:

# What is "mutual information"? According to [Fano (1961), p. 28], if
# two points (words), x and y, have probabilities P(x) and P(y), then
# their mutual information, I(x,y), is defined to be

# I(x,y) = log2 (P(x,y) / (P(x) P(y)))

# Informally, mutual information compares the probability of observing
# x and y together (the joint probability) with the probabilities of
# observing x and y independently (chance). If there is a genuine
# association between x and y, then the joint probability P(x,y) will be
# much larger than chance P(x) P(y), and consequently I(x,y) >> 0. If
# there is no interesting relationship between x and y, then P(x,y) ~
# P(x) P(y), and thus, I(x,y) ~ 0. If x and y are in complementary
# distribution, then P(x,y) will be much less than P(x) P(y), forcing
# I(x,y) << O.

# In our application, word probabilities, P(x) and P(y), are estimated
# by counting the number of observations of x and y in a corpus, f(x)
# and f(y), and normalizing by N, the size of the corpus. (Our
# examples use a number of different corpora with different sizes: 15
# million words for the 1987 AP corpus, 36 million words for the 1988
# AP corpus, and 8.6 million tokens for the tagged corpus.) Joint
# probabilities, P(x,y), are estimated by counting the number of times
# that x is followed by y in a window of w words fw(x,y), and
# normalizing by N.

# The window size parameter allows us to look at different
# scales. Smaller window sizes will identify fixed expressions
# (idioms) and other relations that hold over short ranges; larger
# window sizes will highlight semantic concepts and other
# relationships that hold over larger scales. For the remainder of
# this paper, the window size, w, will be set to 5 words as a
# compromise; thissettingislargeenough to show some of the constraints
# between verbs and arguments, but not so large that it would wash out
# constraints that make use of strict adjacency.

### from: https://www.aaai.org/ocs/index.php/AAAI/AAAI16/paper/view/11963

# The PMI solver formalizes a way of computing and applying such
# associational knowledge. Given a question q and an answer option ai,
# it uses pointwise mutual information (Church and Hanks 1989) to
# measure the strength of the associations between parts of q and
# parts of ai. Given a large corpus C, PMI for two n-grams x and y is
# defined as:

# PMI (x, y) = log p(x, y) p(x)p(y)

# Here p(x, y) is the joint probability that x and y occur together in
# the corpus C, within a certain window of text (we use a 10 word
# window). The term p(x)p(y), on the other hand, represents the
# probability with which x and y would occur together if they were
# statistically independent. The ratio of p(x, y) to p(x)p(y) is thus
# the ratio of the observed co-occurrence to the expected
# co-occurrence. The larger this ratio, the stronger the association
# between x and y.

# We extract unigrams, bigrams, trigrams, and skip-bigrams from the
# question q and each answer option ai. We use the SMART stop word
# list (Salton 1971) to filter the extracted n-grams, but allow
# trigrams to have a stop word as their middle word. The answer with
# the largest average PMI, calculated over all pairs of question
# n-grams and answer option n-grams, is the best guess for the PMI
# solver.

# need to remove stopwords
def split(s, stopwords=None):
    split = [ x.lower() for x in re.sub(r'\W+', ' ', s).split() ]
    if stopwords:
        sw_set = set(stopwords)
        return [ x.strip() for x in split if x not in sw_set ]
    return [ x.strip() for x in split ]

def count_occurrences(x, corpus, normalized=True):
    "Count occurrences of n-gram X in CORPUS."
    total_words = 0
    total_occurrences = 0
    for (sentence,sentence_len) in corpus:
        total_occurrences += len(re.findall(x, sentence))
        total_words += sentence_len

    if normalized:
        return total_occurrences / total_words
    return total_occurrences

def count_co_occurrences(x,y, corpus):
    x_y = " ".join([x,y])
    return count_occurrences(x_y, corpus)
    
def pmi(x,y,corpus):
    """Compute PMI of X and Y in CORPUS; here X and Y are strings
representing n-grams (each gram separated by space) and CORPUS is an
array of strings.  For this experiment we are considering the window
size the extension of each string."""
    px = count_occurrences(x, corpus)
    py = count_occurrences(y, corpus)
    pxy = count_co_occurrences(x, y, corpus)
    if pxy == 0:
        return 0
    return math.log( pxy / (px*py), 2)
