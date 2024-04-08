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
##############################################
filex=/etc/exports
echo $sudo_pass | sudo -S apt install nfs-server

hm=$(pwd)
cd ../
pssh_path=$(pwd)/hosts.txt
cd $hm

mapfile -t host_list < $pssh_path

#adding the lines at /etc/exports of master node
folder1='/usr/share/modules/modulefiles'
folder2='/home'
l1="$folder1 *(rw,sync)"
#l2="$folder2 *(rw,sync,no_root_squash)"
l2="$folder2 *(rw,sync)"
if ! grep -q -F "$l1" "$filex"; 
then
	echo $l1 | sudo tee -a /etc/exports
fi
if ! grep -q -F "$l2" "$filex"; 
then
	echo $l2 | sudo tee -a /etc/exports
fi
mkdir -p $folder1 >/dev/null 2>&1

echo $sudo_pass | sudo -S  chmod -R 0777 $folder1
##restarting nfs server
echo $sudo_pass | sudo service nfs-kernel-server restart


#adding the lines at /etc/fsub of the slaves
for node in "${host_list[@]}";do
	if [[ ! "$node" =~ "node00" ]]; then
		echo $node

		#creates dir of modules
		###########################################
		make_dir1="echo $sudo_pass | sudo -S mkdir -p $folder1"
		fstabl1="node00:$folder1 $folder1 nfs"
		#checks if /etc/fstab needs to be altered
		write_com="if ! grep -q -F '$fstabl1' /etc/fstab; then echo $sudo_pass | sudo -S bash -c  'echo \"$fstabl1\" >> /etc/fstab'; fi"
		echo $sudo_pass | ssh -t $node $write_com
		echo $sudo_pass | ssh -t $node $make_dir1
		echo $sudo_pass | ssh -t $node "echo $sudo_pass | sudo -S mount -a"
		############################################
		fstabl2="node00:$folder2 $folder2 nfs"
		#checks if /etc/fstab needs to be altered
		write_com2="if ! grep -q -F '$fstabl2' /etc/fstab; then echo $sudo_pass | sudo -S bash -c  'echo \"$fstabl2\" >> /etc/fstab'; fi"
		echo $sudo_pass | ssh -t $node $write_com2
		#not required to create folder since /home already exists
		echo $sudo_pass | ssh -t $node "echo $sudo_pass | sudo -S mount -a"
		############################################

	fi
done
