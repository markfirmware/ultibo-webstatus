#/bin/bash

URL=http://163.172.178.31:9980  # Ultima Paris Xenial
URL=http://45.32.223.179:5570   # webstatusprogram Atlanta
URL=http://45.63.115.228:5570   # webstatusprogram Paris
URL=http://104.156.232.107:5570 # webstatusprogram Sydney
URL=http://108.61.117.135:5570  # webstatusprogram Amsterdam
URL=http://163.172.163.75:9980  # Ultima Paris Jessie
URL=http://45.79.200.166:5588   # webstatusprogram markfirmare Atlanta
URL=http://localhost:5588       # lazarus qemu localhost

CURLTIMEOUT=20
INTERVALSECONDS=0.1
COUNTER=0
while true
do
    curl -s -m $CURLTIMEOUT $URL/status/about > /dev/null &
    COUNTER=$((COUNTER+1))
    echo $COUNTER
    sleep $INTERVALSECONDS
done
