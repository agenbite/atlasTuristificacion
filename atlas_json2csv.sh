#!/bin/bash 

cat out3.geojson | awk '{print $7 $17 $18}' > coords.csv

cat headers.csv > atlas_unsorted.csv

for file in *.json
do
	echo
	echo $file

	if [ -s $file ]
	then
		json2csv -i $file --no-header >> atlas_unsorted.csv
	else
		printf ',%.0s' {1..120} >> atlas_unsorted.csv
	fi
done

sort --field-separator=',' --key=36 atlas_unsorted.csv > atlas_sorted.csv

paste -d , coords.csv atlas_sorted.csv > atlas.csv
