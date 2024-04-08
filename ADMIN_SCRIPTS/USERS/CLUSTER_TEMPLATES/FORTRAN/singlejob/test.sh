#!/bin/bash -i
f90=prog.f90
#doing something before submission


#rm -f sub.sh
#touch sub.sh

echo '#!/bin/bash'                           >> sub.sh
echo '#SBATCH -J filename'                   >> sub.sh
echo '#SBATCH --cpus-per-task=2'             >> sub.sh
echo '#SBATCH --mem-per-cpu=1G'              >> sub.sh
echo '#SBATCH --output=slurm_%A.out'         >> sub.sh
echo ''                                      >> sub.sh
echo 'module load ifort-2022'                >> sub.sh
echo 'ifort '$f90''                          >> sub.sh
echo './a.out'                               >> sub.sh

sbatch sub.sh
