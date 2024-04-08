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
deb_list=(htop gcc vim gnuplot sshpass nfs-client nfs-common environment-modules mpich ntp python3-pip git r-base r-base-dev screen lm-sensors)
list=''
for deb in ${deb_list[@]};do
	list="$list $deb"
done
echo "Installing.."
parallel-ssh -i -h "$pssh_path" -t 0 "echo $sudo_pass | sudo -S apt update -y; echo $sudo_pass | sudo -S apt-get clear -y  ; echo $sudo_pass | sudo -S apt install -y $list"
echo "Upgrading..."
parallel-ssh -i -h "$pssh_path" -t 0 "echo $sudo_pass | sudo -S apt upgrade -y"
echo "Setting time zone"
parallel-ssh -i -h "$pssh_path" "echo $sudo_pass | sudo -S timedatectl set-timezone UTC"
