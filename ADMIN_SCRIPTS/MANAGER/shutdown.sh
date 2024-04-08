############################################################
# Author Tiago 2024/1
# this program shutdown all slave machines. Then, shutdown the master.
###########################################################
read -s -p "Enter Password for sudo: " sudo_pass
hm=$(pwd)
cd ../
pssh_path=$(pwd)/hosts.txt
cd $hm
mapfile -t host_list < $pssh_path
host_string=''
for host in ${host_list[@]};do
	if [ $host != $USER@node00 ]; then
		host_string="$host_string $host" 
	fi
	#node=$(echo $host | cut -d'@' -f2)
	#scontrol update NodeName=$node State=DRAIN Reason="Maintance" #to be tested
done


#sleep 30
parallel-ssh -i -H "$host_string" "echo $sudo_pass | sudo -S shutdown now"
echo
echo 
echo ========================================
echo All slaves dead. Killing the master now!
echo $sudo_pass | sudo -S shutdown
