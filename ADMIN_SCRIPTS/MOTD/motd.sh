read -s -p "Enter Password for sudo: " sudo_pass
file=motd
echo $sudo_pass | sudo -S rm  /etc/motd
echo $sudo_pass | sudo -S cp  $file /etc/
echo $sudo_pass| sudo systemctl restart sshd
