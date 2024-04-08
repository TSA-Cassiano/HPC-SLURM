sudo_pass=onagrag

echo $sudo_pass | sudo -S mkdir -p /var/spool/slurmd
echo $sudo_pass | sudo -S mkdir -p /var/lib/slurm-llnl
echo $sudo_pass | sudo -S mkdir -p /var/lib/slurm-llnl/slurmd
echo $sudo_pass | sudo -S mkdir -p /var/lib/slurm-llnl/slurmctld
echo $sudo_pass | sudo -S mkdir -p /var/run/slurm-llnl 
echo $sudo_pass | sudo -S mkdir -p /var/log/slurm-llnl

echo $sudo_pass | sudo -S chmod -R 755 /var/spool/slurmd
echo $sudo_pass | sudo -S chmod -R 755 /var/lib/slurm-llnl/
echo $sudo_pass | sudo -S chmod -R 755 /var/run/slurm-llnl/ 
echo $sudo_pass | sudo -S chmod -R 755 /var/log/slurm-llnl/

echo $sudo_pass | sudo -S chown -R slurm:slurm /var/spool/slurmd
echo $sudo_pass | sudo -S chown -R slurm:slurm /var/lib/slurm-llnl/
echo $sudo_pass | sudo -S chown -R slurm:slurm /var/run/slurm-llnl/
echo $sudo_pass | sudo -S chown -R slurm:slurm /var/log/slurm-llnl/
