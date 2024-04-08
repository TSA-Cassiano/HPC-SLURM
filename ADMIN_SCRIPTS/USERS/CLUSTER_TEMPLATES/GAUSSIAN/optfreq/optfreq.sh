#!/bin/bash -i
job_name='slurm_optfreq'
memory=20
threads=30
ginput=optfreq.com
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
echo '#SBATCH --cpus-per-task='$threads''                                             >> ''$job_name'.sh'
echo '#SBATCH --mem='$memory'G'                                                       >> ''$job_name'.sh'
echo '#SBATCH --output=/dev/null'                                                     >> ''$job_name'.sh'
echo ''                                                                               >> ''$job_name'.sh'
echo 'hm='$(pwd)''                                                                    >> ''$job_name'.sh'
echo 'path='$scr'/$SLURM_ARRAY_JOB_ID'                                                >> ''$job_name'.sh'
echo 'procold=$(grep "%NProcShared" '$ginput')'                                       >> ''$job_name'.sh'
echo 'memold=$(grep "%Mem" '$ginput')'                                                >> ''$job_name'.sh'
echo 'sed -i "s|$procold|%NProcShared='$threads'|g" '$ginput''                        >> ''$job_name'.sh'
echo 'sed -i "s|$memold|%Mem='$memory'GB|g" '$ginput''                                >> ''$job_name'.sh'
echo 'rm -rf $path'                                                                   >> ''$job_name'.sh'
echo 'mkdir -p $path'                                                                 >> ''$job_name'.sh'
echo 'cp -r . $path'                                                                  >> ''$job_name'.sh'
echo 'cd $path'                                                                       >> ''$job_name'.sh'
echo 'module load gaussian'                                                           >> ''$job_name'.sh'
echo 'g16 '$ginput''                                                                  >> ''$job_name'.sh'
echo 'cp -r . $hm'                                                                    >> ''$job_name'.sh'
echo 'rm -rf $path'                                                                   >> ''$job_name'.sh'
#******************************************
#******************************************
sbatch $job_name.sh

