#!/bin/bash -l

#SBATCH -A snic2022-5-34
#SBATCH -p core
#SBATCH -n 2
#SBATCH -t 24:00:00
#SBATCH -J linkage_map_lept_ref_qualimap
#SBATCH -e linkage_map_lept_ref_qualimap_cat_filt.error
#SBATCH --mail-user karin.nasvall@ebc.uu.se
#SBATCH --mail-type=ALL

module load bioinfo-tools
module load R/4.0.0
module load R_packages/4.0.0
module load QualiMap/2.2.1

cd /proj/uppstore2017185/b2014034_nobackup/Karin/link_map_part2/linkage_map/working/qualimap_cat_filt

unset DISPLAY
qualimap multi-bamqc -c -d input_bam_qualimap_cat_filt.txt -r 


