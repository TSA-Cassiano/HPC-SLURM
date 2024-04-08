#################################################
#Author Tiago Cassiano 2023/1                   #
#this configures the first use of munge keys#
#################################################
hm=$(pwd)
read -s -p "Enter Password for sudo: " sudo_pass

#creating pool folder
echo $sudo_pass | sudo -S mkdir -p /var/spool/slurmctld
echo $sudo_pass | sudo -S chown slurm:slurm /var/spool/slurmctld/


sudo su -c 'cd /etc/munge/; create-munge-key'

