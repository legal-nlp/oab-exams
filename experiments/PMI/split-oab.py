#!/usr/bin/python3

import json

with open('oab.json', 'r') as f:
    oab = json.load(f)
    for q in oab:
        filename = q['filename']+'.'+q['number']+'.json'
        with open(filename, 'w') as g:
            json.dump([q], g)
            
    
        
