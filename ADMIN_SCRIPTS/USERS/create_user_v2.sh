#########################################
#  This script create users across all
#  nodes and mirror a bashr file 
#  Author: Tiago Cassiano 2023/1
#########################################
#! /bin/bash
hm=$(pwd)
read -s -p "Enter Password for sudo: " sudo_pass

###########
hm=$(pwd)
cd ../
pssh_path=$(pwd)/hosts.txt
cd $hm
mapfile -t host_list < $pssh_path
##########

userlist_path="user_list.txt"
userlistpw_path="user_listpw.txt"
bashrc=bashrc
temfold=CLUSTER_TEMPLATES

cp $bashrc ".bashrc"
headnode=$(hostname)
groupname=$USER

if [ ! -f "$userlistpw_path" ]; then
    touch $userlistpw_path
    chmod go-rwx $userlistpw_path
fi
mapfile -t userlist < $userlist_path

echo $sudo_pass | sudo -S chmod -R g=u /home/programs/
i0="5000"
for u in ${userlist[@]};do
	echo $u
	i0=$(echo $i0+1 | bc)

	new_user=1
	isinfile=$(cat $userlistpw_path | grep -c $u)	
	if [ $isinfile -eq 1 ]; then
		pw=$(grep $u  $userlistpw_path | cut -d' ' -f2 | xargs)
		echo user $u already exists, using predefined password $pw
		new_user=0
	else
		pw=$RANDOM
		echo ''$u' '$pw'' >> $userlistpw_path
		echo user $u not found, I will create now $pw.
	fi
	for c in ${host_list[@]};do
		echo $u $c
		comm="id $u"
		exist=$(ssh  $c "$comm" 2>&1)
		sub='no such user'
		if [[ "$exist" == *"$sub"* ]]; then
			add="{ echo $sudo_pass; echo $pw; echo $pw; echo; echo; echo; echo; echo; echo y; } | sudo -S  adduser $u"
			echo user $u will be created at $c
			echo $sudo_pass | ssh -t $c "$add"

		fi
		sshpass -p $sudo_pass ssh -t $c "echo $sudo_pass | sudo -S usermod -u $i0 $u"
		change_gp="echo $sudo_pass | sudo -S usermod -aG $groupname $u"
		sshpass -p $sudo_pass ssh -t $c "$change_gp" #placing the user at the same group of sudo user, needed for gaussian
	done
	echo "sharing bashrc"
	echo 	
	#sharing the bashrc file	
	echo $sudo_pass | sudo -S cp .bashrc /home/$u/
	echo $sudo_pass | sudo -S rm -r /home/$u/$temfold
	echo $sudo_pass | sudo -S cp -R $temfold /home/$u/
	echo $sudo_pass | sudo -S chown $u:$u /home/$u/.bashrc
	echo $sudo_pass | sudo -S chown -R $u:$u /home/$u/$temfold

	sshpass -p $pw ssh -t $u@$headnode "source .bashrc"
	if [ $new_user -eq 1 ]; then
		sshpass -p $pw ssh -t $u@$headnode "cd /home/$u/$temfold/python_repo; ./python_startup.sh"
	fi
done

#doing it for headnode too:
u=$USER
echo $sudo_pass | sudo -S cp .bashrc /home/$u/
echo $sudo_pass | sudo -S rm -r /home/$u/$temfold
echo $sudo_pass | sudo -S cp -R $temfold /home/$u/
echo $sudo_pass | sudo -S chown $u:$u /home/$u/.bashrc
echo $sudo_pass | sudo -S chown -R $u:$u /home/$u/$temfold

source ~/.bashrc
#sshpass -p $sudo_pass ssh -t $u@$headnode "cd /home/$u/$temfold/python_repo; ./python_startup.sh"

