find ../../lexml/ -name \*.xml -exec bash simple.sh {} \;

for f in *.txt; do
	if [ ! -s $f ]; then rm $f; fi
done
