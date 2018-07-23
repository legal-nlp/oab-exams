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

def oab_questions(es):
    doc = {
        'size' : 5000,
        'query': {
            'match_all' : {}
        }
    }

    res = es.search(index="oab", doc_type='doc', body=doc)

    for r in res['hits']['hits']:
        yield r['_source']

def search_corpus(es, search_text):
    query = {
        'query': {
            'match': {
                'text': search_text
            }
        }
    }

    res = es.search(index="corpus", doc_type="sentence", body=query)

    score = res['hits']['hits'][0]['_score']
    text = res['hits']['hits'][0]['_source']['text']
    filename = res['hits']['hits'][0]['_source']['filename']
    return (score, text, filename)

def main():
    es = Elasticsearch(['http://localhost:9200/'])

    sw = load_stopwords()

    for q in oab_questions(es):
        oab_enum = q['enum'].replace('\n', ' ')
        oab_filename = q['filename']
        oab_number = q['number']
        oab_options = q['options']

        max_score = 0
        selected_option = { 'letter': '?', 'text': 'N/A', 'correct': None }
        justification = "N/A"

        for o in oab_options:
            enum_plus_option = oab_enum + ' ' + o['text']

            (score, text, filename) = search_corpus(es, enum_plus_option)

            if len(overlap(enum_plus_option, text, sw)) > 0:
                if score > max_score:
                    max_score = score
                    selected_option = o
                    justification = "[{}] {}".format(filename, text)

        row = [oab_filename, oab_number,str(selected_option['correct']),
               oab_enum, selected_option['letter'], selected_option['text'], justification]
        print("\t".join(row))

if __name__ == "__main__":
    main()
