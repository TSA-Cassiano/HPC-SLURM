############################################################
#Author Tiago 2024/1
# checks modules folder
# requires sshpass installed!
# the user must authenticate the host before (manual ssh session)
###########################################################
read -s -p "Enter Password for sudo: " sudo_pass
hm=$(pwd)
cd ../
pssh_path=$(pwd)/hosts.txt
cd $hm

parallel-ssh -i -h "$pssh_path" -t 0 "hostname; ls /usr/share/modules/modulefiles"
