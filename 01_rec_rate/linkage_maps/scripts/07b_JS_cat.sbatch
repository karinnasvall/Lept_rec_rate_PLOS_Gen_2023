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

SIZE_LIM=20
MIN_LOD_JS=8
MAX_LOD_JS=13
MASK=

DATA_FILE=data_f.call.gz


mkdir Sep_chr_size_lim

echo "Running SepChr with sizelimit"

#range of lodlimits for best maps from sep chrom
for LOD_LIM in $(seq 12 13)
do

#map with size limit
zcat $DATA_FILE | java -cp $PROG_DIR/lep-map3-code/bin/ SeparateChromosomes2 data=- lodLimit=$LOD_LIM distortionLod=1 sizeLimit=$SIZE_LIM > Sep_chr_size_lim/map.${LOD_LIM} 2> Sep_chr_size_lim/map.${LOD_LIM}.log

mkdir JS_test_${LOD_LIM}

echo "Running JoinSingles2All"
#run join singles
for i in $(seq $MIN_LOD_JS $MAX_LOD_JS)
do
echo map.${LOD_LIM}_$i

zcat $DATA_FILE | java -cp $PROG_DIR/lep-map3-code/bin/ JoinSingles2All \
 data=-\
 map=Sep_chr_size_lim/map.${LOD_LIM}\
 lodLimit=$i\
 distortionLod=1\
 numThreads=2\
 iterate=1\
 > JS_test_${LOD_LIM}/map.${LOD_LIM}_js$i 2> JS_test_${LOD_LIM}/map.${LOD_LIM}_js$i.log
done
wait

#create a summary of the maps
(echo "LG";seq 0 60) > JS_test_${LOD_LIM}/summary_JS.txt

for i in $(seq $MIN_LOD_JS $MAX_LOD_JS);do cut -f1 JS_test_${LOD_LIM}/map.${LOD_LIM}_js${i} | sort | uniq -c | sort -k2g | awk 'NR==1{print $6};NR>1{print $1}' | paste JS_test_${LOD_LIM}/summary_JS.txt -  > JS_test_${LOD_LIM}/summary_JS_prel.txt && mv JS_test_${LOD_LIM}/summary_JS_prel.txt JS_test_${LOD_LIM}/summary_JS.txt;done
done

