#! /bin/bash
#################################################################
# This program rewrites /etc/hosts and /etc/host in all machines
# Author: Tiago Cassiano 2023/1
################################################################
hm=$(pwd) 
read -s -p "Enter Password for sudo: " sudo_pass
##############################################
if [ -z "$1" ]; then #if no specific node, do all at the same time
	echo $sudo_pass | sudo -S sed -i '/node/d' /etc/hosts #cleaning hosts
	N_node=31  #master node out
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
                echo "$line" | sudo tee -a /etc/hosts
        done
else #if specific node
        i=$1
        nodenum=$i
        n=$(echo $i +5| bc)
        ip=10.0.0.$n
        if [ $i -le 9 ]; then
                nodenum=node0$i
        else
                nodenum=node$i
        fi
        ip_list=($ip)
        node_list=($nodenum)
fi


for index in "${!ip_list[@]}";do
	ip=${ip_list[$index]}
	node=${node_list[$index]}
	echo $ip $node
	
	chown_comm="echo $sudo_pass | sudo -S chown $USER /etc/hosts" #changing ownership of hosts so scp can work	
	sshpass -p $sudo_pass ssh -t $ip "$chown_comm"
	host_comm='echo '$sudo_pass'| sudo -S chown '$USER' /etc/hostname ; echo '$sudo_pass' | sudo -S truncate -s 0 /etc/hostname; echo '$sudo_pass' | sudo -S echo -en '$node' >> /etc/hostname;' #note to my future self: if this doesnt work, manually change permission like hosts file

	sshpass -p $sudo_pass scp /etc/hosts $USER@$ip:/etc/
	sshpass -p $sudo_pass ssh -t $ip "$host_comm"

	if [ "$node" != "node00" ]; then
		#echo restarting node $node
		#sshpass -p $sudo_pass ssh -t $ip "echo '$sudo_pass' | sudo -S reboot"
		:
	fi
done
echo now the masternode!
#sshpass -p $sudo_pass ssh -t ${ip_list[0]} "echo '$sudo_pass' | sudo -S reboot"
