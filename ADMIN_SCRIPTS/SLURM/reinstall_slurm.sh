read -s -p "Enter Password for sudo: " sudo_pass
echo $sudo_pass | sudo -S apt-get purge --auto-remove slurm-wlm -y
echo $sudo_pass | sudo -S apt-get purge --auto-remove slurm-client -y
echo $sudo_pass | sudo -S apt-get purge --auto-remove munge -y


echo $sudo_pass | sudo -S rm -r slurm-llnl/
echo $sudo_pass | sudo -S rm -r slurmctld/ slurmd/
echo $sudo_pass | sudo -S apt autoremove -y
#echo $sudo_pass | sudo -S apt install slurm-wlm slurm-client munge
