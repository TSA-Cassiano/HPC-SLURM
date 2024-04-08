############################################################
#Author Tiago 2023/1
#this program installs .deb packages
# requires sshpass installed!
# the user must authenticate the host before (manual ssh session)
###########################################################
read -s -p "Enter Password for sudo: " sudo_pass
hm=$(pwd)
cd ../
pssh_path=$(pwd)/hosts.txt
cd $hm
deb_list=(r-base r-base-dev)
list=''
for deb in ${deb_list[@]};do
	list="$list $deb"
done


job="echo $sudo_pass | sudo -S Rscript -e 'install.packages(c(\"igraph\", \"stringr\", \"minpack.lm\"))'"

parallel-ssh -i -h "$pssh_path" -t 0 "echo $sudo_pass | sudo -S apt update -y; echo $sudo_pass | sudo -S apt-get clear -y  ; echo $sudo_pass | sudo -S apt install -y $list"
parallel-ssh -i -h "$pssh_path" -t 0 "$job"



