#################################################
#Author Tiago Cassiano 2023/1                   #
#this program updates slurm.conf on all machines#
#################################################
hm=$(pwd)
read -s -p "Enter Password for sudo: " sudo_pass
#https://sempreupdate.com.br/conheca-o-pssh-e-execute-um-unico-comando-em-varios-servidores-linux/
pssh_path=$hm/hosts.txt
parallel-ssh -i -h "$pssh_path" "slurmd -C"
