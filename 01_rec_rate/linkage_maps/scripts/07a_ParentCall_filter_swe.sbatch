#!/bin/bash -l

#SBATCH -A snic2022-5-34
#SBATCH -p core
#SBATCH -n 1
#SBATCH -t 2:00:00
#SBATCH -J linkage_map_lept_ref_lepmap
#SBATCH -e linkage_map_lept_ref_parentcall_swe.error
#SBATCH --mail-user karin.nasvall@ebc.uu.se
#SBATCH --mail-type=ALL

module load bioinfo-tools

PROG_DIR=/proj/uppstore2017185/b2014034_nobackup/Karin/programs
INPUT_DIR=/proj/uppstore2017185/b2014034_nobackup/Karin/link_map_part2/linkage_map/output/LepMap_swe
OUTPUT_DIR=$INPUT_DIR

VCF=post_swe_sc.gz
PEDIGREE=pedigree_swe_zlim.txt

cd $INPUT_DIR
#mkdir $OUTPUT_DIR

#zcat $VCF |java -cp $PROG_DIR/lep-map3-code/bin/ ParentCall2 data=$PEDIGREE posteriorFile=- ZLimit=2 removeNonInformative=1 |gzip > data.call.gz 2> parentCall.log

zcat data.call.gz |java -cp $PROG_DIR/lep-map3-code/bin/ Filtering2 data=- dataTolerance=0.00001 missingLimit=0.5 MAFLimit=0.05 familyInformativeLimit=5 removeNonInformative=1  |gzip > data_f.call.gz 2> filtering.log
