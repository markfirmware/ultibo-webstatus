#!/bin/bash

URL=http://163.172.178.31:9980  # Ultima Paris Xenial
URL=http://45.32.223.179:5570   # webstatusprogram Atlanta
URL=http://45.63.115.228:5570   # webstatusprogram Paris
URL=http://104.156.232.107:5570 # webstatusprogram Sydney
URL=http://108.61.117.135:5570  # webstatusprogram Amsterdam
URL=http://163.172.163.75:9980  # Ultima Paris Jessie
URL=http://45.79.200.166:5578   # webstatusprogram markfirmare Atlanta
URL=http://localhost:5588         # webstatusprogram markfirmare Atlanta

EPISODE=$(date +%H%M%S)
GETTIMEOUT=30
INTERVAL=$1
BURSTCOUNT=$2
COUNTER=0
rm -f wget.log.*
rm -f wget.file.*
echo -n 'starting   ' $BURSTCOUNT ' ... '
while [[ $COUNTER -lt $BURSTCOUNT ]]
do
    FILE=wget.$EPISODE.$COUNTER
    wget --read-timeout=$GETTIMEOUT -a wget.log.$COUNTER -O wget.file.$COUNTER $URL/status &
    sleep $INTERVAL
    let COUNTER=COUNTER+1
done
echo started
wait
grep "awaiting response" wget.log.* | grep -iv "200 OK"
