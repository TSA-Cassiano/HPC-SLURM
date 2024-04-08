#!/bin/bash -i 
job_name='slurm_singlejob'
f90=prog.f90
scr=$SCRDIR/$USER
echo Script is running now
echo Scracth path: $scr
##############################################
echo "Submiting the job now!"
rm -rf $job_name.sh
# creating the job.sh file
#******************************************
echo '#!/bin/bash'                                                                    >> ''$job_name'.sh'
echo '#SBATCH -J '$job_name''                                                         >> ''$job_name'.sh'
echo '#SBATCH --cpus-per-task=1'                                             >> ''$job_name'.sh'
echo '#SBATCH --mem=1G'                                                       >> ''$job_name'.sh'
echo ''                                                                               >> ''$job_name'.sh'
echo 'hm='$(pwd)''                                                                    >> ''$job_name'.sh'
echo 'path='$scr'/$SLURM_JOB_ID'                                                >> ''$job_name'.sh'
echo 'rm -rf $path'                                                                   >> ''$job_name'.sh'
echo 'mkdir -p $path'                                                                 >> ''$job_name'.sh'
echo 'cp -r . $path'                                                                  >> ''$job_name'.sh'
echo 'cd $path'                                                                       >> ''$job_name'.sh'
echo 'module load ifort-2022'                                                           >> ''$job_name'.sh'
echo 'ifort '$f90''                                                                  >> ''$job_name'.sh'
echo './a.out '                                                                  >> ''$job_name'.sh'
echo 'rm a.out'                                                                  >> ''$job_name'.sh'
echo 'cp -r . $hm'                                                                    >> ''$job_name'.sh'
echo 'rm -rf $path'                                                                   >> ''$job_name'.sh'
#******************************************
#******************************************
sbatch $job_name.sh

