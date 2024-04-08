#! /bin/bash
#################################################################
# This program sets up nfs folders on slave machines
# must have evioronment-modules installed at all nodes
# folder1= home
# folder2=modules folder
# Author: Tiago Cassiano 2023/1
################################################################
hm=$(pwd) 
read -s -p "Enter Password for sudo: " sudo_pass

pssh_path="../hosts.txt"
aux_script="permission_aux.sh"
##############################################
mapfile -t host_list < $pssh_path

for node in "${host_list[@]}";do
	if [[ ! "$node" =~ "node00" ]]; then
		echo $node
		parallel-ssh -i -h "$pssh_path" -t 0 "cd $hm; echo '$sudo_pass' | sudo -S ./$aux_script"
	fi
done
