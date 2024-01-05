#!/bin/bash -l

#SBATCH -A snic2022-5-34
#SBATCH -p core
#SBATCH -n 4
#SBATCH -t 48:00:00
#SBATCH -J linkage_map_lept_ref_qualimap
#SBATCH -e linkage_map_lept_ref_qualimap_fem.error
#SBATCH --mail-user karin.nasvall@ebc.uu.se
#SBATCH --mail-type=ALL

module load bioinfo-tools
module load R/4.0.0
module load R_packages/4.0.0
module load QualiMap/2.2.1

cd /proj/uppstore2017185/b2014034_nobackup/Karin/link_map_part2/linkage_map/working

unset DISPLAY
qualimap multi-bamqc -c -d input_bam_qualimap_cat_fem.txt -r &
qualimap multi-bamqc -c -d input_bam_qualimap_swe_fem.txt -r &
wait

