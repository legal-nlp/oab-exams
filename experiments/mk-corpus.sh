#!/bin/bash

if [ ! -e IR/report.tsv ]; then
    echo please save the output of IR in a file called IR/report.tsv
fi

if [ ! -e PMI/report.tsv ]; then
    echo please save the output of PMI in a file called PMI/report.tsv
fi

cat IR/report.tsv | awk -F'\t' '$3 == "True" {OFS="\t"; print $1, $2, $3}' > tmp.ir
cat PMI/report.tsv | awk -F'\t' '$3 == "True" {OFS="\t"; print $1, $2, $3}' > tmp.pmi

comm -12 tmp.ir tmp.pmi | awk -F'\t' '{OFS=","; print $1, $2}' > easy.csv

rm tmp.ir tmp.pmi
