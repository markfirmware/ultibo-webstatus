#/bin/bash

#apt-get update
#apt-get -y dist-upgrade
apt-get -y install fail2ban git psmisc python-pip qemu-system-arm ufw
pip install requests
pip install circleclient
for i in 0 1 2 3 4 5
do
    ./set-ufw.sh $i
done
