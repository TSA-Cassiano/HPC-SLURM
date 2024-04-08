#!/bin/bash -i
##############################################
hm=$(pwd)
scr="$SCRDIR/${USER}"
list_jobs=()
slurm_out="slurm_out"
job_name='GNR_STATIONARY'
memory=1
threads=2
echo Script is running now
echo Scracth path: $scr
echo
#############################################
f90="hipergr.f90"
parm="PARM.INC"
init=3
fin=8
num=20
dpar=$(echo "print(($fin - $init)/$num)" | python3)
par=$init
dir=$(pwd)/base
rm -rf *_alph
for (( i=0; i<=$num; i++))
do
	par=$(echo "print($init+ $dpar*$i)"| python3 )
        par=$(echo "print('{:.2e}'.format($par))"| python3 )

        dir_par="$hm/${par}_alph"
        cp -R "$hm/base/." "$dir_par/"

        cd $dir_par

        par=$(sed 's/e/D/g' <<< $par)
        #fucking fortran doesnt accept numbers like XD-0Y. It only understands XD-Y
        n=$(echo $par | cut -d'D' -f1)
        m=$(echo $par | cut -d'D' -f2)
        m=$(echo "print(str(int('$m')))" | python3)
        par=${n}D$m

        sed -i "s|XXX|${par}|g" $f90
	
	list_jobs+=($(pwd))
        cd ../
done 
#END SCRIPT
###########################################
ll=''
Njobs=0
for j in $(ls -vd ${list_jobs[@]} ); do
	ll="$ll $j"
	Njobs=$(echo $Njobs+1| bc)
done
list_jobs=$ll
###########################################
echo "Submiting the jobs now!"
rm -rf $slurm_out
mkdir $slurm_out
rm -rf $job_name.sh
# creating the job.sh file
#******************************************
echo '#!/bin/bash'                                                                    >> ''$job_name'.sh'
echo '#SBATCH -J '$job_name''                                                         >> ''$job_name'.sh'
echo '#SBATCH --cpus-per-task='$threads''                                             >> ''$job_name'.sh'
echo '#SBATCH --mem-per-cpu='$memory'G'                                               >> ''$job_name'.sh'
echo '#SBATCH --array=1-'$Njobs''                                                     >> ''$job_name'.sh'
echo '#SBATCH --output='$(pwd)'/'$slurm_out'/slurm_%A_%a.out'                         >> ''$job_name'.sh'
echo ''                                                                               >> ''$job_name'.sh'
echo 'list_jobs=('$list_jobs')'                                                       >> ''$job_name'.sh'
echo 'index=$(echo $SLURM_ARRAY_TASK_ID-1 | bc)'                                      >> ''$job_name'.sh'
echo 'job=${list_jobs[$index]}'                                                       >> ''$job_name'.sh'
echo 'path='$scr'/${SLURM_ARRAY_JOB_ID}_${SLURM_ARRAY_TASK_ID}'                       >> ''$job_name'.sh'
echo 'rm -rf $path'                                                                   >> ''$job_name'.sh'
echo 'mkdir -p $path'                                                                 >> ''$job_name'.sh'
echo 'cp -r $job/. $path'                                                             >> ''$job_name'.sh'
echo 'cd $path'                                                                       >> ''$job_name'.sh'
echo 'module load ifort-2022'                                                         >> ''$job_name'.sh'
echo 'ifort -qmkl=parallel -par-num-threads='$threads' '$f90''     >> ''$job_name'.sh'
echo './a.out'                                                                        >> ''$job_name'.sh'
echo 'rm a.out'                                                                       >> ''$job_name'.sh'
echo 'cp -r . $job'                                                                   >> ''$job_name'.sh'
echo 'rm -rf $path'                                                                   >> ''$job_name'.sh'
#******************************************
sbatch $job_name.sh
