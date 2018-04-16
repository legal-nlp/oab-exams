#!/usr/bin/python3

import nltk
import os, argparse, json, re, math, statistics, sys
from pmi import *

from multiprocessing import Pool

def load_stopwords():
    sw = []
    sw_file = ""
    if os.path.isfile('../stopwords-pt.txt'):
        sw_file = '../stopwords-pt.txt'
    else:
        sw_file = 'stopwords-pt.txt'
    with open(sw_file, 'r') as f:
        for l in f:
            sw.append(l.strip())
    return set(sw)

def unigram(text, sw):
    return list(nltk.ngrams(split(text, sw), 1))

def bigram(text, sw):
    return list(nltk.ngrams(split(text, sw), 2))

def trigram(text, sw):
    return list(nltk.ngrams(split(text, sw), 3))

def skip_trigram(text, sw):
    return list([ (x[0],x[2]) for x in nltk.ngrams(split(text, sw), 3) ])

def compute_all_pmi(ngram_fn, oab, corpus, sw):
    for q in oab:
        enum = ngram_fn(q['enum'], sw)

        for o in q['options']:
            option_text = ngram_fn(o['text'], sw)

            sum = 0
            len = 0

            for x, y in [(x,y) for x in enum for y in option_text]:
                sum += pmi(x,y,corpus)
                len += 1

            avg = 0
            if len > 0:
                avg = sum/len
                
            if 'pmi' in o:
                o['pmi'].append(avg)
            else:
                o['pmi'] = [avg]

def compute_q_pmi(ngram_fn, q, corpus, sw):
    enum = ngram_fn(q['enum'], sw)

    for o in q['options']:
        option_text = ngram_fn(o['text'], sw)

        sum = 0
        len = 0

        for x, y in [(x,y) for x in enum for y in option_text]:
            sum += pmi(x,y,corpus)
            len += 1

        avg = 0
        if len > 0:
            avg = sum/len

        if 'pmi' in o:
            o['pmi'].append(avg)
        else:
            o['pmi'] = [avg]

def main():

    oab = []
    corpus = []
    sw = load_stopwords()
    
    with open(sys.argv[1],'r') as f:
        oab = json.load(f)
    with open('corpus.json','r') as f:
        corpus = json.load(f)

    preprocessed_corpus = []
    for raw in corpus:
        split_sentence = split(raw['text'], sw)
        preprocessed_corpus.append((' '.join(split_sentence), len(split_sentence)))

    for ngram_fn in [unigram, bigram, trigram, skip_trigram]:
        compute_all_pmi(ngram_fn, oab, preprocessed_corpus, sw)

    with open('pmi' + os.path.basename(sys.argv[1]),'w') as f:
        json.dump(oab, f)

if __name__ == "__main__":
    main()
