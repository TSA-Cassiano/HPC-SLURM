#! /bin/bash
#This script deletes users by the name Tiago (2023)
password_file="user_listpw.txt"
user_list="user_list.txt"

read -s -p "Enter Password for sudo: " sudoPW
echo
echo -n "Enter the username:"
read user_name

l1=$(grep -n $user_name $user_list | cut -d : -f 1)
l2=$(grep -n $user_name $password_file | cut -d : -f 1)
sed -i "${l1}d" $user_list
sed -i "${l2}d" $password_file

master_node=''
N_node=9
nodelist=()
nodelist+=($master_node)
for (( i=1; i<=$N_node; i++ ))
do
	number=$i
	if [ $i -le 9 ]; then
		number=0$number
	fi
	nodelist+=(node$number)
done	
for c in ${nodelist[@]}; do
        comm="echo $sudoPW | sudo -S userdel -rf  $user_name"
	echo $sudo_pass | ssh -t $c "$comm"
done
