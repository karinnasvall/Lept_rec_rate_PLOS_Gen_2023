#!/bin/bash -l

#SBATCH -A snic2022-5-34
#SBATCH -p core
#SBATCH -n 1
#SBATCH -t 2:00:00
#SBATCH -J linkage_map_lept_lepmap
#SBATCH -e linkage_map_lept_sep_chrom_swe.error
#SBATCH --mail-user karin.nasvall@ebc.uu.se
#SBATCH --mail-type=ALL

#script to evaluate lod score in LepMap3

module load bioinfo-tools

PROG_DIR=/proj/uppstore2017185/b2014034_nobackup/Karin/programs
INPUT_PATH=/proj/uppstore2017185/b2014034_nobackup/Karin/link_map_part2/linkage_map/output/LepMap_swe

PREFIX=swe_4fam
OUTPUT_PATH=$INPUT_PATH/sep_chr_${PREFIX}
INPUT_DATA=data_f_4fam.call.gz

cd $INPUT_PATH

#run SepChromosomes with different lodLimits to assign optimal LG
#use either output from ParentCall or Filtering2

mkdir $OUTPUT_PATH/
echo ”Running SeparateChromosomes2”
date

for i in {1..30}
do
echo lodLimit${i}
zcat $INPUT_DATA |java -cp $PROG_DIR/lep-map3-code/bin/ SeparateChromosomes2 \
 data=-\
 lodLimit=${i}\
 distortionLod=1\
 numThreads=2\
 informativeMask=2\
 > $OUTPUT_PATH/${PREFIX}.${i}.map  2> $OUTPUT_PATH/${PREFIX}.${i}.log
wait
done

echo ”Finished SeparateChromosomes2”
date


#gleaning the data
(echo "LG";seq 0 60) > $OUTPUT_PATH/${PREFIX}_sepchr_lod.txt

for lg in {1..30}
do
echo $lg
sort $OUTPUT_PATH/${PREFIX}.${lg}.map | uniq -c | sort -k2 -g |  awk 'NR==1{print $5};NR>1{print $1}' | paste $OUTPUT_PATH/${PREFIX}_sepchr_lod.txt - > $OUTPUT_PATH/${PREFIX}_sepchr_lod_prel.txt && mv $OUTPUT_PATH/${PREFIX}_sepchr_lod_prel.txt $OUTPUT_PATH/${PREFIX}_sepchr_lod.txt
wait
done

echo ”Finished”
date

#check resultfile in r
