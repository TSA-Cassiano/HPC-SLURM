#!/bin/bash
#SBATCH -J GNR_STATIONARY
#SBATCH --cpus-per-task=2
#SBATCH --mem-per-cpu=1G
#SBATCH --array=1-21
#SBATCH --output=/home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/slurm_out/slurm_%A_%a.out

list_jobs=( /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/3.00e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/3.25e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/3.50e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/3.75e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/4.00e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/4.25e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/4.50e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/4.75e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/5.00e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/5.25e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/5.50e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/5.75e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/6.00e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/6.25e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/6.50e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/6.75e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/7.00e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/7.25e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/7.50e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/7.75e+00_alph /home/padawan/cluster/SETUP/USERS/CLUSTER_TEMPLATES/fortran/GNR_nested/8.00e+00_alph)
index=$(echo $SLURM_ARRAY_TASK_ID-1 | bc)
job=${list_jobs[$index]}
path=/tmp/padawan/${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}
rm -rf $path
mkdir -p $path
cp -r $job/. $path
cd $path
module load ifort-2022
ifort -qmkl=parallel -par-num-threads=2 hipergr.f90
./a.out
rm a.out
cp -r . $job
rm -rf $path
