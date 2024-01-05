#! /bin/bash -l
#SBATCH -A snic2022-5-34
#SBATCH -p core
#SBATCH -n 1
#SBATCH -t 01:00:00
#SBATCH -J Linkage_map_lept_ref
#SBATCH -e Linkage_map_lept_ref_index.error
#SBATCH --mail-user karin.nasvall@ebc.uu.se
#SBATCH --mail-type=ALL

module load bioinfo-tools 
module load bwa/0.7.17

cd /proj/uppstore2017185/b2014034_nobackup/Karin/link_map_part2/linkage_map/data

bwa index GCA_905404315.1_ilLepSina1.1_genomic.fna.gz


