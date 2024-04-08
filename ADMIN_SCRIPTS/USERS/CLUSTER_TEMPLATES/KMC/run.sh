#!/bin/bash -i
job_name='kmc'
memory=20
threads=30
kinput=forster_singlet.py
scr=$SCRDIR/$USER
slurmout=slurm_out
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
#echo '#SBATCH --output='$(pwd)/$slurmout''                                            >> ''$job_name'.sh'
echo ''                                                                               >> ''$job_name'.sh'
echo 'hm='$(pwd)''                                                                    >> ''$job_name'.sh'
echo 'path='$scr'/$SLURM_JOB_ID'                                                      >> ''$job_name'.sh'
echo 'rm -rf $path'                                                                   >> ''$job_name'.sh'
echo 'mkdir -p $path'                                                                 >> ''$job_name'.sh'
echo 'cp -r . $path'                                                                  >> ''$job_name'.sh'
echo 'cd $path'                                                                       >> ''$job_name'.sh'
echo 'sed -i "s|XXX|'$threads'|g" '$kinput''                                          >> ''$job_name'.sh'
echo 'kmc '$kinput''                                                                  >> ''$job_name'.sh'
echo 'cp -r . $hm'                                                                    >> ''$job_name'.sh'
echo 'rm -rf $path'                                                                   >> ''$job_name'.sh'
#******************************************
#******************************************
sbatch $job_name.sh

