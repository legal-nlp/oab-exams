#!/usr/bin/python3

import json, sys, statistics

def calculate_mean_pmi(questions):
    for q in questions:
        for o in q['options']:
            o['pmi'] = statistics.mean(o['pmi'])

for fn in sys.argv[1:]:
    questions = []
    with open(fn,'r') as f:
        questions.extend(json.load(f))

    calculate_mean_pmi(questions)

    ## what happens if there are collisions?
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
        
                                   
