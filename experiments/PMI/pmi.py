#!/usr/bin/python3

# from: http://www.aclweb.org/anthology/P89-1010.pdf
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

#The window size parameter allows us to look at different
#scales. Smaller window sizes will identify fixed expressions (idioms)
#and other relations that hold over short ranges; larger window sizes
#will highlight semantic concepts and other relationships that hold
#over larger scales. For the remainder of this paper, the window size,
#w, will be set to 5 words as a compromise; thissettingislargeenough
#to show some of the constraints between verbs and arguments, but not
#so large that it would wash out constraints that make
#use of strict adjacency.

