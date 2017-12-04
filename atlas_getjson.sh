max=11699
for i in `seq 1 $max`
do
    curl "http://turistificacion.300000kms.net/grid/points/$i.json" > $i.json
done
