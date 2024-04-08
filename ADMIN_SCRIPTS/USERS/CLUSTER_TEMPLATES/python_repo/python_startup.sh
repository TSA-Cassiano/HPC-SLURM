#############
#
# This is a aux script to setup the python programs for the user
###############



modules=(numpy scipy matplotlib)
#for mod in ${modules[@]}; do
#	python3 -m pip3 install $mod
#done


progs_fold=/home/$USER/CLUSTER_TEMPLATES/python_repo
repo_list=(LeonardoESousa/KMC LeonardoESousa/xcharge LeonardoESousa/kmcdash LeonardoESousa/LeoX LeonardoESousa/NEMO)
mkdir -p $progs_fold
python3 -m pip install --upgrade pip
cd $progs_fold
for rep in ${repo_list[@]}; do
	fold=$(echo $rep | cut -d'/' -f2)
	git clone https://github.com/$rep
	cd $fold
	python3 -m pip install .
	cd ../
done



