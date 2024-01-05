#!/bin/bash -l

#SBATCH -A snic2022-5-34
#SBATCH -p core
#SBATCH -n 1
#SBATCH -t 06:00:00
#SBATCH -J linkage_map_lept_ref
#SBATCH -e linkage_map_lept_ref_flag.error
#SBATCH --mail-user karin.nasvall@ebc.uu.se
#SBATCH --mail-type=ALL

module load bioinfo-tools
module load samtools/1.9

cd /proj/uppstore2017185/b2014034_nobackup/Karin/link_map_psrt2/linkage_map/working/
OUTPUT_PATH=/proj/uppoff2020002/private/group_member_backups/KarinN/projects/2022_06_linkmap_lept_part2/output/04b_bwa_bam_b1_cat

#only 
for file in $(ls $OUTPUT_PATH/*.bam)
do
#baseFilename=`basename $file .bam`
#samtools view -h -q 10 $file | grep -v -e "XA:Z:" -e "SA:Z:" | samtools view -b -h  > $OUTPUT_PATH/${baseFilename}.bam
samtools flagstat $OUTPUT_PATH/${file} > $OUTPUT_PATH/${file}_flagstat.txt 
done
wait
