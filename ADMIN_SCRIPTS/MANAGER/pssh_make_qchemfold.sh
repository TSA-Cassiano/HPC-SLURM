############################################################
#Author Tiago 2023/1
# creates a folder for qchem
# requires sshpass installed!
# the user must authenticate the host before (manual ssh session)
###########################################################
read -s -p "Enter Password for sudo: " sudo_pass
hm=$(pwd)
cd ../
pssh_path=$(pwd)/hosts.txt
cd $hm

fold=/qchem_scratch
parallel-ssh -i -h "$pssh_path" -t 0 "echo $sudo_pass | sudo -S mkdir -p $fold"
parallel-ssh -i -h "$pssh_path" "echo $sudo_pass | sudo -S chown -R $USER:$USER $fold; chmod -R 777 $fold"
