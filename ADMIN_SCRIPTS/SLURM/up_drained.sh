#################################################
#Author Tiago Cassiano 2024/1                   #
#this program reinstates drained nodes          #
#################################################
hm=$(pwd)
read -s -p "Enter Password for sudo: " sudo_pass

list_node=$(sinfo --Node --states=drained | awk '{print $1}'| xargs) #gettin ID from pending jobs
NODES=($(echo "$list_node" | tr ' ' '\n')) #storing in an array
NODES=("${NODES[@]:1}") #slicing to skip the first entry 
for NODE in ${NODES[@]};do
	echo $NODE
	echo $sudo_pass | sudo -S scontrol update nodename=$NODE state=resume
	#echo $sudo_pass | sudo -S scontrol release $ID
done
