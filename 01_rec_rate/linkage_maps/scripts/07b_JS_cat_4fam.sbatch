#! /bin/bash -l
#SBATCH -A snic2022-5-34
#SBATCH -p core
#SBATCH -n 1
#SBATCH -t 01:00:00
#SBATCH -J linkage_map_lept_lepmap_JS
#SBATCH -e linkage_map_lept_cat_JS
#SBATCH --mail-user karin.nasvall@ebc.uu.se
#SBATCH --mail-type=ALL

cd /proj/uppstore2017185/b2014034_nobackup/Karin/link_map_part2/linkage_map/output/LepMap_cat


#variables
PROG_DIR=/proj/uppstore2017185/b2014034_nobackup/Karin/programs

SIZE_LIM=15
MIN_LOD_JS=5
MAX_LOD_JS=7
MASK=

DATA_FILE=data_f_4fam.call.gz

OUT_sep=Sep_chr_size_lim_4fam
mkdir $OUT_sep

echo "Running SepChr with sizelimit"

#range of lodlimits for best maps from sep chrom
for LOD_LIM in $(seq 6 7)
do

#map with size limit
zcat $DATA_FILE | java -cp $PROG_DIR/lep-map3-code/bin/ SeparateChromosomes2 data=- lodLimit=$LOD_LIM distortionLod=1 sizeLimit=$SIZE_LIM  informativeMask=2 > $OUT_sep/map.${LOD_LIM} 2> $OUT_sep/map.${LOD_LIM}.log

OUT_JS=JS_test_${LOD_LIM}_4fam
mkdir $OUT_JS

echo "Running JoinSingles2All"
#run join singles
for i in $(seq $MIN_LOD_JS $MAX_LOD_JS)
do
echo map.${LOD_LIM}_$i

zcat $DATA_FILE | java -cp $PROG_DIR/lep-map3-code/bin/ JoinSingles2All \
 data=-\
 map=$OUT_sep/map.${LOD_LIM}\
 lodLimit=$i\
 distortionLod=1\
 numThreads=2\
 iterate=1\
 > $OUT_JS/map.${LOD_LIM}_js$i 2> $OUT_JS/map.${LOD_LIM}_js$i.log
done
wait

#create a summary of the maps
(echo "LG";seq 0 60) > $OUT_JS/summary_JS.txt

for i in $(seq $MIN_LOD_JS $MAX_LOD_JS);do cut -f1 $OUT_JS/map.${LOD_LIM}_js${i} | sort | uniq -c | sort -k2g | awk 'NR==1{print $6};NR>1{print $1}' | paste $OUT_JS/summary_JS.txt -  > $OUT_JS/summary_JS_prel.txt && mv $OUT_JS/summary_JS_prel.txt $OUT_JS/summary_JS.txt;done
done

