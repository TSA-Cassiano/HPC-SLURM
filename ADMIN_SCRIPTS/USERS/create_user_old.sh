#! /bin/bash
hm=$(pwd) #tem que rodar no home
read -s -p "Enter Password for sudo: " sudo_pass
userlist_path="user_list.txt"
userlistpw_path="user_listpw.txt"

if [ ! -f "$userlistpw_path" ]; then
    touch $userlistpw_path
    chmod go-rwx $userlistpw_path
fi

mapfile -t userlist < $userlist_path

master_node=''
N_node=3
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

for u in ${userlist[@]};do
	
	isinfile=$(cat $userlistpw_path | grep -c $u)	
	if [ $isinfile -eq 1 ]; then
		pw=$(grep $u  $userlistpw_path | cut -d' ' -f2 | xargs)
		echo user $u already exists, using predefined password $pw
	else
		pw=$RANDOM
		echo ''$u' '$pw'' >> $userlistpw_path
		echo user $u not found, I will create now $pw.
	fi
	for c in ${nodelist[@]};do
	
		comm="id $u"
		exist=$(ssh  $c "$comm" 2>&1)
		sub='no such user'
		if [[ "$exist" == *"$sub"* ]]; then
			add="{ echo $sudo_pass; echo $pw; echo $pw; echo; echo; echo; echo; echo; echo y; } | sudo -S  adduser $u"
			echo user $u will be created at $c
			echo $sudo_pass | ssh -t $c "$add"
		fi

	done
done
