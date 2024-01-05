#! /bin/bash -l

#SBATCH -A snic2022-5-34
#SBATCH -p core
#SBATCH -n 8
#SBATCH -t 48:00:00
#SBATCH -J linkmap2_pixy_site
#SBATCH -e linkmap2_pixy_site_swe.error
#SBATCH --mail-user karin.nasvall@ebc.uu.se
#SBATCH --mail-type=ALL

#load modules
module load bioinfo-tools
module load pixy/1.2.5.beta1

#go to working directory
WORK_DIR=/proj/uppstore2017185/b2014034_nobackup/Karin/link_map_part2/02_diversity

#vcf file to analyse
VCF_IN=$WORK_DIR/02_filt_vcf/SweM.repeatmasked.filtered.vcf.b.gz

IN_DIR=$WORK_DIR/01c_siteTypes

#population file
POP=swe

OUT_DIR=$WORK_DIR/03_pixy/output_sites_${POP}
mkdir $OUT_DIR

WINDOW_SIZE=50000

#CHR_NR=29

#pixy
#index vcf
#tabix $IN_VCF

#loop over all sites
for SITE in $(cat $WORK_DIR/03_pixy/list_sites.txt)
do

SITE_FILE=$IN_DIR/sites_${POP}/sites_${POP}_${SITE}.txt 

pixy --stats pi \
--vcf $VCF_IN \
--populations population_${POP}.txt \
--window_size $WINDOW_SIZE \
--output_prefix ${POP}_${SITE} \
--n_cores 8 \
--sites_file $SITE_FILE 
#--chromosomes $i

done
wait
