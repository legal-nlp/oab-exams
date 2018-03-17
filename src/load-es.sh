
sbcl --load oab-parser.lisp --eval '(oab-parser::files-es)' --eval '(sb-ext:quit)'
curl -XDELETE 'localhost:9200/oab?pretty'
for f in *.json; do
    curl -s -H "Content-Type: application/x-ndjson" -XPOST localhost:9200/_bulk --data-binary "@$f";
done
rm *.json

