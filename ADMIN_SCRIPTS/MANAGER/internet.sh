#! /bin/bash
#################################################################
# This program sets up the internet configuration
# so that the slave nodes have access
# Author: Tiago Cassiano 2023/1
################################################################
hm=$(pwd) 
read -s -p "Enter Password for sudo: " sudo_pass
##############################################

inter=enp5s0
intra=enp4s0

#iptables rules
echo $sudo_pass | sudo -S iptables -A FORWARD -o $inter -i $intra -s 10.0.0.0/24 -m conntrack --ctstate NEW -j ACCEPT
echo $sudo_pass | sudo -S iptables -A FORWARD -m conntrack --ctstate ESTABLISHED,RELATED -j ACCEPT
echo $sudo_pass | sudo -S iptables -t nat -F POSTROUTING
echo $sudo_pass | sudo -S iptables -t nat -A POSTROUTING -o $inter -j MASQUERADE
#echo $sudo_pass | sudo -S iptables-save > /etc/iptables/rules.v4
echo $sudo_pass | sudo -S sed -i 's|#net.ipv4.ip_forward=1|net.ipv4.ip_forward=1|g' /etc/sysctl.conf 
echo $sudo_pass | sudo -S iptables-save | sudo tee /etc/iptables/rules.v4
echo $sudo_pass | sudo -S sh -c "echo 1 > /proc/sys/net/ipv4/ip_forward"

#enabling RC LOCAL
rcserv_text='[Unit]\n
 Description=/etc/rc.local Compatibility\n
 ConditionPathExists=/etc/rc.local\n
\n
[Service]\n
 Type=forking\n
 ExecStart=/etc/rc.local start\n
 TimeoutSec=0\n
 StandardOutput=tty\n
 RemainAfterExit=yes\n
 SysVStartPriority=99\n
\n
[Install]\n
 WantedBy=multi-user.target\n'

rcloc_text='
#!/bin/bash\n
exit 0\n
iptables-restore < /etc/iptables.sav\n
'
rm -f rc.local
rm -f rc-local.service
touch rc.local
touch rc-local.service
echo $sudo_pass | sudo -S echo -en $rcserv_text >> rc-local.service
echo $sudo_pass | sudo -S echo -en $rcloc_text >> rc.local
echo $sudo_pass | sudo -S cp "rc.local" /etc/
echo $sudo_pass | sudo -S cp "rc-local.service" /etc/systemd/system/

#echo $sudo_pass | sudo -S echo -en $rcserv_text >> /etc/systemd/system/rc-local.service
#echo $sudo_pass | sudo -S echo -en $rcloc_text >> /etc/rc.local


echo $sudo_pass | sudo -S chmod +x /etc/rc.local
echo $sudo_pass | sudo -S systemctl enable rc-local
echo $sudo_pass | sudo -S systemctl start rc-local.service


