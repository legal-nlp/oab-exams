#!/bin/bash

xsltproc ../../src/extract-articles.xslt $1 | xsltproc ../../src/extract-text.xslt - > $(basename $1).txt
