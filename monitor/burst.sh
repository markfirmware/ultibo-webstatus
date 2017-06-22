.#/bin/bash

URL=http://163.172.178.31:9980  # Ultima Paris Xenial
URL=http://45.32.223.179:5570   # webstatusprogram Atlanta
URL=http://45.63.115.228:5570   # webstatusprogram Paris
URL=http://104.156.232.107:5570 # webstatusprogram Sydney
URL=http://108.61.117.135:5570  # webstatusprogram Amsterdam
URL=http://163.172.163.75:9980  # Ultima Paris Jessie
URL=http://45.79.200.166:5578   # webstatusprogram markfirmare Atlanta
URL=http://localhost:5588   # webstatusprogram markfirmare Atlanta

EPISODE=$(date +%H%M%S)
CURLTIMEOUT=60
BURSTCOUNT=$1
COUNTER=0
rm -f errors.txt
clear
while [[ $COUNTER -lt $BURSTCOUNT ]]
do
    FILE=threadlist.$EPISODE.$COUNTER
    curl -m $CURLTIMEOUT $URL/status > $FILE 2>> errors.txt &
#   sleep 0.1
    let COUNTER=COUNTER+1
done
time wait
grep 'curl.*28' errors.txt | wc
grep curl errors.txt | grep -v 28
