#################################################
#Author Tiago Cassiano 2024/1                   #
#this program holds the that are pending        #
#################################################
hm=$(pwd)
read -s -p "Enter Password for sudo: " sudo_pass

list_jobs=$(squeue -t PD -o "%.18i" | sed 's/_.*//'| xargs) #gettin ID from pending jobs
IDS=($(echo "$list_jobs" | tr ' ' '\n')) #storing in an array
IDS=("${IDS[@]:1}") #slicing to skip the first entry 
for ID in ${IDS[@]};do
	echo $ID
	echo $sudo_pass | sudo -S scontrol hold $ID
done
