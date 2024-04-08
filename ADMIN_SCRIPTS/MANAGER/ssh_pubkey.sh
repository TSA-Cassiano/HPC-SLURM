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
hm=$(pwd) 
read -s -p "Enter Password for sudo: " sudo_pass
##############################################
prinf "\n\n\n" | ssh-keygen -t rsa

N_node=15 #master node out
ip_list=()
node_list=()
for (( i=0; i<=$N_node; i++ ))
do
        nodenum=$i
	n=$(echo $i +5| bc)
	ip=10.0.0.$n
        if [ $i -le 9 ]; then
                nodenum=node0$i
	else
		nodenum=node$i
        fi
	
	ip_list+=($ip)
	node_list+=($nodenum)
	line="$ip $nodenum"
done


for i in "${!node_list[@]}";do
	ipi=${ip_list[$i]}
	nodei=${node_list[$i]}
	sshpass -p $sudo_pass ssh-copy-id $USER@$nodei #fixing connection headnode-->slaves
	comm_gen="{ echo; echo; echo} | ssh-keygen -t rsa"
	#sshpass -p $sudo_pass ssh -t $nodei "$comm_gen"
	for j in "${!node_list[@]}";do	
	        ipj=${ip_list[$j]}
        	nodej=${node_list[$j]}
		#echo $nodei $nodej
		comm="sshpass -p $sudo_pass ssh-copy-id $USER@$nodej"
		#sshpass -p $sudo_pass ssh -t $USER@$nodei "$comm"
	done
done
