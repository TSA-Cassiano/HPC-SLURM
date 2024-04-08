
sudo_pass=onagrag
slurmdserv="/lib/systemd/system/slurmd.service"
oldline="PIDFile=/run/slurmd.pid"
newline="PIDFile=/var/run/slurm-llnl/slurmd.pid"

echo $sudo_pass | sudo -S sed -i "s|$oldline|$newline|g" $slurmdserv
