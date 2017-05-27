#/bin/bash

function stopall {
    killall -w -g $1
}

case $1 in
stop)
    stopall qemu-system-arm
    ;;
restart)
    stopall qemu-system-arm
    for i in 0 1 2 3 4 5 6 7 8 9
    do
        ./run-webstatus.py $i | tee log-$i.txt &
    done
    ;;
esac

ps aux | egrep '(run-webstatus|qemu-system-arm)' | grep -v grep | sort
