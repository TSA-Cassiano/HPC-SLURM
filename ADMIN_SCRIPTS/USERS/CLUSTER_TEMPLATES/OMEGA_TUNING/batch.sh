#!/bin/bash
#SBATCH -J DNT_tuning
#SBATCH --cpus-per-task=2
#SBATCH --mem=12GB
#SBATCH --output=/dev/null

path=/tmp/$USER/$SLURM_JOB_ID
hm=$(pwd)
rm -rf $path
mkdir -p $path
cp -r . $path
cd $path
module load gaussian

bash $1
cp -r . $hm
rm -rf $path
