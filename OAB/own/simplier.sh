for f in *.conll; do 
 awk '$5 ~ /^[0-9]+/ {split($5,a,"/"); split(a[1],b,":"); $5=b[1]; print}' $f > $f.simple; 
 awk '$5 ~ /^[0-9]+/ {split($5,a,"/"); split(a[1],b,":"); $5=b[1]; print}' $f.1 > $f.1.simple; 
done
