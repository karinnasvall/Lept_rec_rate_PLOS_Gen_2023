#!/bin/bash -l

#SBATCH -A snic2022-5-34
#SBATCH -p core
#SBATCH -n 1
#SBATCH -t 96:00:00
#SBATCH -J linkage_map_lept_ref_pileup
#SBATCH -e linkage_map_lept_ref_pileup_cat.error
#SBATCH --mail-user karin.nasvall@ebc.uu.se
#SBATCH --mail-type=ALL

module load bioinfo-tools
module load samtools/1.9

#list_filtered_files.txt created after quality control and exclusion of individuals with low coverage.

INPUT_PATH=/proj/uppoff2020002/private/group_member_backups/KarinN/projects/2022_06_linkmap_lept_part2/output/04b_bwa_bam_b1_cat_filt

PROGRAM_PATH=/proj/uppstore2017185/b2014034_nobackup/Karin/programs

cd $INPUT_PATH

#samtools mpileup -q 10 -Q 10 -d 500 -s $(cat list_filtered_files.txt) > mpileup.txt

java -cp $PROGRAM_PATH/lep-map3-code/bin/ Pileup2Likelihoods pileup=mpileup.txt |gzip  > post_cat.gz


#with default values (minCoverage=3, numLowerCoverage=0.3 (missing ind))
#usage: samtools mpileup -q 10 -Q10 -s $(cat sorted_bams)|java Pileup2Likelihoods [options] >post.txt
#options:
#         pileup=file          load pileup from a file [-]
#         mapping=file         load individual names (same order as bams) from a file [mapping.txt]
#         minCoverage=NUM      minimum coverage per individual [3]
#         numLowerCoverage=NUM number (or proportion) individuals allowed with lower than minCoverage [0.3]
#         minAlleleFreq=NUM    minimum number (or proportion) of an allele [0.1]
#         minQuality=NUM       minimum quality value [0.001]
#         minCoverageSum=NUM   minimum total (summed) coverage
