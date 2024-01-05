#!/bin/bash -l

#SBATCH -A snic2022-5-34
#SBATCH -p core
#SBATCH -n 1
#SBATCH -t 06:00:00
#SBATCH -J linkage_map_lept_ref_cat
#SBATCH -e linkage_map_lept_swe_filter.error
#SBATCH --mail-user karin.nasvall@ebc.uu.se
#SBATCH --mail-type=ALL

module load bioinfo-tools
module load samtools/1.9

cd /proj/uppstore2017185/b2014034_nobackup/Karin/link_map_part2/working/
INPUT_PATH=/proj/uppoff2020002/private/group_member_backups/KarinN/projects/2022_06_linkmap_lept_part2/output/04b_bwa_bam_b1_swe
OUTPUT_PATH=/proj/uppoff2020002/private/group_member_backups/KarinN/projects/2022_06_linkmap_lept_part2/output/04b_bwa_bam_b1_swe_filt

mkdir $OUTPUT_PATH

#only 
for file in $(ls $INPUT_PATH/*.bam)
do
baseFilename=`basename $file .sorted.bam`

samtools view -h -q 10 $file | grep -v -e "XA:Z:" -e "SA:Z:" | samtools view -b -h  > $OUTPUT_PATH/${baseFilename}.bam

done
wait
