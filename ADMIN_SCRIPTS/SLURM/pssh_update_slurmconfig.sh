#################################################
#Author Tiago Cassiano 2023/1                   #
#this program updates slurm.conf on all machines#
#################################################
hm=$(pwd)
read -s -p "Enter Password for sudo: " sudo_pass
#https://sempreupdate.com.br/conheca-o-pssh-e-execute-um-unico-comando-em-varios-servidores-linux/

############
hm=$(pwd)
cd ../
pssh_path=$(pwd)/hosts.txt
cd $hm
mapfile -t host_list < $pssh_path
############


slurm_conf="slurm.conf"
cgroup_conf="cgroup.conf"
config_path="/etc/slurm-llnl"
munge_path="/etc/munge"
sharemunge=$1

parallel-ssh -i -h "$pssh_path" -t 0 "echo $sudo_pass | sudo -S apt-get install slurm-wlm slurm-client munge -y"

#changing the PID path at the service files to match /etc/slurm-llnl/slurm.conf
slurmdserv="/lib/systemd/system/slurmd.service"
oldline="PIDFile=/run/slurmd.pid"
newline="PIDFile=/var/run/slurm-llnl/slurmd.pid"

parallel-ssh -i -h "$pssh_path" "echo $sudo_pass | sudo -S sed -i 's|$oldline|$newline|g' $slurmdserv"


#creating the folders mentioned at /etc/slurm-llnl/slurm.conf	
parallel-ssh -i -h "$pssh_path" "echo $sudo_pass | sudo -S mkdir -p /var/run/slurm-llnl/; echo $sudo_pass | sudo -S chmod -R 755 /var/run/slurm-llnl/ ; echo $sudo_pass | sudo -S chown -R slurm:slurm /var/run/slurm-llnl/"
parallel-ssh -i -h "$pssh_path" "echo $sudo_pass | sudo -S mkdir -p /var/spool/slurmd; echo $sudo_pass | sudo -S chmod -R 755 /var/spool/slurmd ; echo $sudo_pass | sudo -S chown -R slurm:slurm /var/spool/slurmd"

#creating for ctld too
oldline="PIDFile=/run/slurmctld.pid"
newline="PIDFile=/var/run/slurm-llnl/slurmctld.pid"
parallel-ssh -i -H "node00" "echo $sudo_pass | sudo -S sed -i 's|$oldline|$newline|g' $slurmdserv"
parallel-ssh -i -H "node00" "echo $sudo_pass | sudo -S mkdir -p /var/spool/slurmctld; echo $sudo_pass | sudo -S chmod -R 755 /var/spool/slurmctld ; echo $sudo_pass | sudo -S chown -R slurm:slurm /var/spool/slurmctld"

parallel-ssh -i -h "$pssh_path" "echo $sudo_pass | sudo -S cp $(pwd)/$slurm_conf $config_path"
parallel-ssh -i -h "$pssh_path" "echo $sudo_pass | sudo -S cp $(pwd)/$cgroup_conf $config_path"
parallel-ssh -i -h "$pssh_path" "echo $sudo_pass | sudo -S chown -R slurm:slurm /etc/slurm-llnl"

#munge
if [ $sharemunge = 1 ]; then
	echo munge time!

	#for host in ${host_list[@]}; do
		#echo $sudo_pass | sudo -S scp -r /etc/munge/munge.key $host:~/ #chekc if necessary
		#echo $sudo_pass | sudo -S chmod 755 /home/$USER/munge.key
		#sleep 3 #wait for nfs update
		#munge_comm="echo $sudo_pass | sudo -S cp munge.key /etc/munge; echo $sudo_pass | sudo -S chown munge:munge $munge_path/munge.key"
		#sshpass -p $sudo_pass ssh -t $host $munge_comm
	#done
	echo $sudo_pass | sudo -S chmod 755 /home/$USER/munge.key
	sleep 3
	munge_comm=" echo $sudo_pass | sudo -S cp munge.key /etc/munge; echo $sudo_pass | sudo -S chown munge:munge $munge_path/munge.key"
	parallel-ssh -i -h "$pssh_path" $munge_comm
fi

echo Now restarting services
#intiating/restarting the services to update the changes
comm1="echo $sudo_pass | sudo -S systemctl enable munge ; echo $sudo_pass | sudo -S systemctl start munge; echo $sudo_pass | sudo -S systemctl restart munge"
comm2="echo $sudo_pass | sudo -S systemctl enable slurmd; echo $sudo_pass | sudo -S systemctl start slurmd; echo $sudo_pass | sudo -S systemctl restart slurmd" 
comm="$comm1;$comm2"
parallel-ssh -i -h "$pssh_path" -t 0 "$comm"

echo $sudo_pass | sudo -S systemctl enable munge
echo $sudo_pass | sudo -S systemctl start munge
echo $sudo_pass | sudo -S systemctl restart munge
echo $sudo_pass | sudo -S systemctl enable slurmctld
echo $sudo_pass | sudo -S systemctl start slurmctld
echo $sudo_pass | sudo -S systemctl restart slurmctld
echo $sudo_pass | sudo -S systemctl enable slurmd
echo $sudo_pass | sudo -S systemctl start slurmd
echo $sudo_pass | sudo -S systemctl restart slurmd
echo $sudo_pass | sudo -S scontrol reconfigure

parallel-ssh -i -h "$pssh_path" -t 0 "$comm"
