#!/usr/bin/python3

from elasticsearch import Elasticsearch
from elasticsearch.helpers import bulk
import json

def load_oab(es):		
    doc = { 'size' : 5000, 'query': {'match_all' : {} } }		
    		
    res = es.search(index="oab", doc_type='doc', body=doc)		
    		
    oab = []		
    		
    for r in res['hits']['hits']:		
        oab.append(r['_source'])		
		
    return oab		
		
def load_corpus(es):		
    doc = { 'size' : 5000, 'query': {'match_all' : {} } }		
		
    corpus = []		
    res = es.search(index="corpus", doc_type="sentence", body=doc)		
    		
    for r in res['hits']['hits']:		
        corpus.append(r['_source'])		
		
    return corpus


if __name__ == "__main__":

    es = Elasticsearch(['http://localhost:9200/'])
    oab = load_oab(es)
    corpus = load_corpus(es)
    with open('oab.tmp.json','w') as f:
        json.dump(oab, f)
    with open('corpus.tmp.json','w') as f:
        json.dump(corpus, f)

