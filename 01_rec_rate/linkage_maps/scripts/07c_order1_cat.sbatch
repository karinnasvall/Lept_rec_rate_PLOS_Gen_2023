#!/bin/bash -l

#SBATCH -A snic2022-5-34
#SBATCH -p core
#SBATCH -n 1
#SBATCH -t 24:00:00
#SBATCH -J linkage_map_lept_lepmap
#SBATCH -e linkage_map_order1_cat.error
#SBATCH --mail-user karin.nasvall@ebc.uu.se
#SBATCH --mail-type=ALL

#script to run Ordermarkers2 in LepMap3
module load bioinfo-tools

PROG_DIR=/proj/uppstore2017185/b2014034_nobackup/Karin/programs
INPUT_PATH=/proj/uppstore2017185/b2014034_nobackup/Karin/link_map_part2/linkage_map/output/LepMap_cat

PREFIX=cat
OUTPUT_PATH=$INPUT_PATH/ordermarker_${PREFIX}
INPUT_DATA=data_f.call.gz
LG=53
MAP_FILE=map.12.11


cd $INPUT_PATH

mkdir $OUTPUT_PATH/


#make a file with genome position for all markers
zcat $INPUT_DATA |cut -f1-2 | awk 'NR>7{print $0}' > $INPUT_PATH/snps.txt

#run ordermarker

#5 iterations
for i in {1..5}
do
#for nr of lg
for lg in $(seq 1 1 $LG)
do

zcat $INPUT_DATA| java -Xmx2048m -cp $PROG_DIR/lep-map3-code/bin/ OrderMarkers2\
 map=$MAP_FILE\
 data=-\
 numThreads=2\
 recombination2=0\
 chromosome=${lg}\
 informativeMask=1\
 > $OUTPUT_PATH/${PREFIX}_order_${lg}_${i}.txt 2> $OUTPUT_PATH/${PREFIX}_order_${lg}_${i}.log
wait

#maps the markers to genomic coordinates
awk '(NR==FNR){s[NR]=$0}(NR!=FNR){if ($1 in s) $3=s[$1];print}' $INPUT_PATH/snps.txt $OUTPUT_PATH/${PREFIX}_order_${lg}_${i}.txt > $OUTPUT_PATH/${PREFIX}_order_${lg}_${i}.mapped.txt

done
done

wait

#if no iterations (i=1) and comment away likelihood comparisons
#i=1

grep "likelihood" $OUTPUT_PATH/*mapped.txt > $OUTPUT_PATH/likelihoods.txt
#make a list of best likelihoods
sort -k7 $OUTPUT_PATH/likelihoods.txt | sort -k4 | awk 'NR%5==1' | sort -gk4 > $OUTPUT_PATH/list_best_likelihoods.txt

#OBS!! Set nr according to nr of lg
for lg in $(seq 1 1 $LG); do
file=$(awk -v i=$i 'NR==i' $OUTPUT_PATH/list_best_likelihoods.txt| cut -f1 -d ":")
awk -v lg=$lg 'NR>3{print $1, $2, $3, $4, $5=lg}' $file | sed -E 's/HiC_scaffold_//g' | sed 's/\*//g' >> $OUTPUT_PATH/order_all.txt
done
wait

awk 'BEGIN{print "marker_nr male_position scaffold position lg"}{print $0}' $OUTPUT_PATH/order_all.txt > $OUTPUT_PATH/${PREFIX}_order_all.table

