#! /bin/bash -l

#SBATCH -A snic2022-5-34
#SBATCH -p core
#SBATCH -n 1
#SBATCH -t 12:00:00
#SBATCH -J linkmap2_siteTypes
#SBATCH -e linkmap2_siteTypes_swe.error
#SBATCH --mail-user karin.nasvall@ebc.uu.se
#SBATCH --mail-type=ALL


#load modules (ev tabix if vcf not indexed)
module load bioinfo-tools tabix

#working directory
WORK_DIR=/proj/uppstore2017185/b2014034_nobackup/Karin/link_map_part2/02_diversity

PROGRAM_PATH=/proj/uppstore2017185/b2014034_nobackup/Karin/programs/genomics_general-master

#vcf file to analyse
IN_VCF=$WORK_DIR/02_filt_vcf/SweM.repeatmasked.filtered.snps.nonamb.vcf

REF=/proj/uppstore2017185/b2014034_nobackup/Karin/link_map_part2/03_selection/data/LsinapisSweM.fasta
REF_GFF=/proj/uppstore2017185/b2014034_nobackup/Karin/link_map_part2/03_selection/data/LsinapisSweM.all.maker.genes.gff

#path to output and output prefix
OUT=$WORK_DIR/01c_siteTypes/sites_swe.txt

#vcf need to be indexed
bgzip $IN_VCF
wait
tabix -p vcf ${IN_VCF}.gz

#run python script from https://github.com/simonhmartin/genomics_general
python $PROGRAM_PATH/codingSiteTypes.py -a $REF_GFF -o $OUT -v ${IN_VCF}.gz -r $REF --ignoreConflicts
