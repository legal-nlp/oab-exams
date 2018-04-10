#!/usr/bin/python3

import json, sys, statistics

for fn in sys.argv[1:]:
    questions = []
    with open(fn,'r') as f:
        questions.extend(json.load(f))
    for q in questions:
        for o in q['options']:
            o['pmi'] = statistics.mean(o['pmi'])
    for q in questions:
        max_pmi = q['options'][0]['pmi']
        chosen_answer = None
        for o in q['options']:
            if o['pmi'] > max_pmi:
                max_pmi = o['pmi']
                chosen_answer = o
        if chosen_answer:
            print(chosen_answer['correct'])
        else:
            print(None)
        
                                   
