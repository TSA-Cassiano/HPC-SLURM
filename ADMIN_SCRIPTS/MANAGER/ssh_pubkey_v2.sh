#! /bin/bash
#################################################################
# This program shares a ssh pubkey of the current user with all
# machines
# Author: Tiago Cassiano 2023/1
# note: you must first enter 1 time before doing ssh-copy-id to get the fingerprint
# note2: to make crossed sshcopies, one must to authenticate the host (first ssh session)
# possible solution is, in /etc/ssh/ssh_config, do:
# Host 10.0.0.*
#   StrictHostKeyChecking=no
#   UserKnownHostsFile=/dev/null
################################################################
read -s -p "Enter Password for sudo: " sudo_pass
################################################################
hm=$(pwd)
cd ../
pssh_path=$(pwd)/hosts.txt
cd $hm

mapfile -t host_list < $pssh_path

prinf "\n\n\n" | ssh-keygen -t rsa
for host in ${host_list[@]};do
	echo $host
	sshpass -p $sudo_pass ssh-copy-id $host #fixing connection headnode-->slaves
done
