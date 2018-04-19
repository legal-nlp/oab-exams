#!/bin/bash

if [ ! -e IR/report.tsv ]; then
    echo please save the output of IR in a file called IR/report.tsv
fi

if [ ! -e PMI/report.tsv ]; then
    echo please save the output of PMI in a file called PMI/report.tsv
fi

cat IR/report.tsv | awk -F'\t' '{OFS="\t"; print $1, $2, $7}' > tmp.ir
cat PMI/report.tsv > tmp.pmi
