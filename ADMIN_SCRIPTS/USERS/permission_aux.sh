#! /bin/bash
#################################################################
# This program sets up nfs folders on slave machines
# must have evioronment-modules installed at all nodes
# folder1= home
# folder2=modules folder
# Author: Tiago Cassiano 2023/1
################################################################
hm=$(pwd) 
#read -s -p "Enter Password for sudo: " sudo_pass

user_path="user_list.txt"
##############################################
mapfile -t user_list < $user_path

for usr in "${user_list[@]}";do
	echo $usr
	#echo $sudo_pass | sudo -S chown -R $usr:$usr /home/$usr
	sudo chown -R $usr:$usr /home/$usr
done
