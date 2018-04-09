import argparse, elasticsearch, json, re
from elasticsearch import Elasticsearch
from elasticsearch.helpers import bulk

def overlap(a,b,stopwords):
    filtered_a = set([ x.lower() for x in re.sub(r'\W+', ' ', a).split() ]) - stopwords
    filtered_b = set([ x.lower() for x in re.sub(r'\W+', ' ', b).split() ]) - stopwords

    return filtered_a & filtered_b

def load_stopwords():
    sw = []
    with open('../stopwords-pt.txt', 'r') as f:
        for l in f:
            sw.append(l.strip())
    return set(sw)

es = Elasticsearch(['http://localhost:9200/'])
doc = {
    'size' : 5000,
    'query': {
        'match_all' : {}
    }
}

res = es.search(index="oab", doc_type='doc', body=doc)

sw = load_stopwords()

for r in res['hits']['hits']:
    enum = r['_source']['enum'].replace('\n', ' ')
    oab_filename = r['_source']['filename']
    oab_number = r['_source']['number']
    options = r['_source']['options']
    max_score = 0
    selected_option = { 'letter': '?', 'text': 'N/A', 'correct': None }
    justification = "N/A"
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
        text =  r['hits']['hits'][0]['_source']['text']
        if len(overlap(enum_plus_option, text, sw)) > 0:
            if score > max_score:
                max_score = score
                selected_option = o
                justification = "[{}] {}".format(r['hits']['hits'][0]['_source']['filename'], text)

    row = [oab_filename, oab_number,
           enum, selected_option['letter'], selected_option['text'], justification, str(selected_option['correct'])]
    print("\t".join(row))
    
