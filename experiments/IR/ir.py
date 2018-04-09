import argparse, elasticsearch, json
from elasticsearch import Elasticsearch
from elasticsearch.helpers import bulk

es = Elasticsearch(['http://localhost:9200/'])
doc = {
    'size' : 5000,
    'query': {
        'match_all' : {}
    }
}

res = es.search(index="oab", doc_type='doc', body=doc)

for r in res['hits']['hits']:
    enum = r['_source']['enum'].replace('\n', ' ')
    options = r['_source']['options']
    max_score = 0
    selected_option = {}
    for o in options:
        enum_plus_option = enum + ' ' + o['text']
        query = {
            'query': {
                'match': {
                    'text': enum_plus_option
                }
            }
        }

        r = es.search(index="corpus", doc_type="sentence", body=query)
        score = r['hits']['hits'][0]['_score']
        if score > max_score:
            max_score = score
            selected_option = o
    print (selected_option['correct'])
    
