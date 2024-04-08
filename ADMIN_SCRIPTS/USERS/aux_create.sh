#tiago 2024/1
#this code is a aux block to create a user if it does not exists in the local machine
u=$1
pw=$2
groupname=$3
i0=$4

#if [[ "$exist" == *"$sub"* ]]; then
#	echo user $u will be created at $(hostname)
	#add="{ echo $sudo_pass; echo $pw; echo $pw; echo; echo; echo; echo; echo; echo y; } | sudo -S adduser $u"
	
	#:
#fi
newuser=0
if id "$u" &>/dev/null; then
	:
else
	l= echo $pw; echo $pw; echo; echo; echo; echo; echo; echo y;
	$l | sudo adduser $u
fi

sudo usermod -u $i0 $u
sudo usermod -aG $groupname $u
sudo chown -R $u:$u /home/$u
id "$u" 
