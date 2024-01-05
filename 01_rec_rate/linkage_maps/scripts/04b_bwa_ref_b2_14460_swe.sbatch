#!/bin/bash -l

#SBATCH -A snic2022-5-34
#SBATCH -p core
#SBATCH -n 12
#SBATCH -t 96:00:00
#SBATCH -J linkage_map_lept_ref
#SBATCH -e linkage_map_lept_ref_bwa_b2_swe.error
#SBATCH --mail-user karin.nasvall@ebc.uu.se
#SBATCH --mail-type=ALL

module load bioinfo-tools
module load samtools/1.9
module load bwa/0.7.17

cd /proj/uppstore2017185/b2014034_nobackup/Karin/link_map_part2/linkage_map/working/

assembly=../data/lsin_swe/LsinapisSweM.fasta
input_path=/proj/uppoff2020002/private/group_member_backups/KarinN/projects/2018_02_linkmap_lept/output/03_process_radtag_cf_14460_noradtag
output_path=/proj/uppoff2020002/private/group_member_backups/KarinN/projects/2022_06_linkmap_lept_part2/output/04b_bwa_bam_b1_swe

list_filenames=LIST_FILES_14460

mkdir $output_path

for file in $(grep "R1" $list_filenames)
do
baseFilename=`basename $file _L001_R1_001.1.1.fq.gz`

bwa mem -t 10 -M $assembly $input_path/$file $input_path/${baseFilename}_L001_R2_001.2.2.fq.gz | samtools sort - -o $output_path/${baseFilename}.sorted.bam

done
wait


#bwa
#-t INT nr of threds
#-T INT default quality limit, Don’t output alignment with score lower than INT. This option only affects output. [30]
#-M 	Mark shorter split hits as secondary (for Picard compatibility). 
