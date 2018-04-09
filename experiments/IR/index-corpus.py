#!/usr/bin/python3

# Adapted from:
# https://github.com/allenai/ARC-Solvers/blob/master/scripts/index-corpus.py

# This script uses the Python Elasticsearch API to index a user-specified text corpus in an 
# ElasticSearch cluster. The corpus is expected to be a text file with a sentence per line.
# Each sentence is indexed as a separate document, and per the mappings defined here, the
# Snowball Stemmer is used to stem all tokens.
# If an index with the requested name does not exists, creates it, if not simply adds
# documents to existing index. 

import argparse, elasticsearch, json
from elasticsearch import Elasticsearch
from elasticsearch.helpers import bulk

if __name__ == "__main__":

    # Arguments
    parser = argparse.ArgumentParser(
        description='Add lines from a file to a simple text Elasticsearch index.')
    parser.add_argument('file', help='Path of file to index, e.g. /path/to/my_corpus.txt')
    parser.add_argument('index', help='Name of index to create')
    parser.add_argument('host', help='Elasticsearch host.')
    parser.add_argument('-p', '--port', default=9200, help='port, default is 9200')
    args = parser.parse_args()

    # Get Index Name
    index_name = args.index

    # Document Type constant
    TYPE = "sentence"

    # Get an ElasticSearch client
    es = Elasticsearch(hosts=[{"host": args.host, "port": args.port}], retries=3, timeout=60)

    # Mapping used to index all corpora used in Aristo solvers
    mapping = '''
    {
      "mappings": {
        "sentence": {
          "dynamic": "false",
          "properties": {
            "docId": {
              "type": "keyword"
            },
            "text": {
              "analyzer": "portuguese",
              "type": "text",
              "fields": {
                "raw": {
                  "type": "keyword"
                }
              }
            },
            "tags": {
              "type": "keyword"
            }
          }
        }
      }
    }'''

    # Function that constructs a json body to add each line of the file to index
    def make_documents(f, filename):
        doc_id = 0
        for l in f:
            doc = {
                '_op_type': 'create',
                '_index': index_name,
                '_type': TYPE,
                '_id': doc_id,
                '_source': {'filename': filename, 'text': l.strip()}
            }
            doc_id += 1
            yield (doc)

    # Create an index, ignore if it exists already
    try:
        res = es.indices.create(index=index_name, ignore=400, body=mapping)

        # Bulk-insert documents into index
        with open(args.file, "r") as f:
            res = bulk(es, make_documents(f, args.file))
            doc_count = res[0]

    except Exception as inst:
        print(inst)
