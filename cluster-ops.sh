#/bin/bash

function stopall {
    killall -w -g $1
}

case $1 in
start)
    for i in 0 1 2 3 4 5 6 8 9
    do
        ./run-webstatus.py $i | tee log-$i.txt &
    done
    ;;
stop)
    stopall qemu-system-arm
    ;;
esac

ps aux | egrep '(run-webstatus|qemu-system-arm)' | grep -v grep | sort
