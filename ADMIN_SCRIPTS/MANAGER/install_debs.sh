############################################################
#Author Tiago 2023/1
#this program installs .deb packages
# requires sshpass installed!
# the user must authenticate the host before (manual ssh session)
###########################################################
read -s -p "Enter Password for sudo: " sudo_pass
deb_list=(htop gcc vim gnuplot sshpass nfs-client environment-modules mpich ntp python3-pip git)
list=''
for deb in ${deb_list[@]};do
	list="$list $deb"
done
echo $list

N_node=31 #master node out
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
        node_list+=($nodenum)
	ip_list+=($ip)
done


#for node in ${node_list[@]};do
for index in "${!ip_list[@]}";do

	ip=${ip_list[$index]}
        node=${node_list[$index]}
	echo $node
	#sshpass -p $sudo pass ssh -t -o BatchMode=yes -o StrictHostKeyChecking=no $node "hostname" #solve authentication, use with caution
	comm="echo $sudo_pass | sudo -S apt update -y; echo $sudo_pass | sudo -S apt install -y $list"		
	sshpass -p $sudo_pass ssh -t $node $comm
	#echo $sudo_pass | ssh -t $ip $comm
	

	time_comm="echo $sudo_pass | sudo -S timedatectl set-timezone UTC"
	sshpass -p $sudo_pass ssh -t $node $time_comm
done    

