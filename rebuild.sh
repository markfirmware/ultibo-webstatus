#!/bin/bash

function log {
    echo $* | tee -a $LOG
}

function ultibo-bash-quotation {
    if [ "$(which $1)" != "" ]
    then
        echo -n $*
    else
        local DOCKER_IMAGE=markfirmware/ultibo-1.3.331-x64-1.0.0
        echo -en "docker run --rm -i -v $(pwd):/workdir --entrypoint /bin/bash $DOCKER_IMAGE -c \"$*\""
    fi
}

function ultibo-bash {
    eval $(ultibo-bash-quotation $*)
}

function unix_line_endings {
    tr -d \\r < $1 > tmp && \
    mv tmp $1
}

function convert-frames {
    ls frame*-1920x1080x3.fb > /dev/null 2>&1
    if [[ $? -eq 0 ]]
    then
        for frame in frame*-1920x1080x3.fb
        do
            ultibo-bash convert -size 1920x1080 -depth 8 rgb:$frame ${frame%.fb}.png && \
            rm $frame
        done
    fi
}

function test-qemu-controller {
    echo .... running qemu
    local RESTORE_PWD=$(pwd)
    echo $RESTORE_PWD
    cd $ARTIFACTS/QEMUVPB
    pwd
    time python $RESTORE_PWD/run-qemu
    if [[ $? -ne 0 ]]; then log fail: $?; fi
    cd test

    for textfile in *.txt
    do
        unix_line_endings $textfile
    done
    sed -i 's/.\x1b.*\x1b\[D//' qemumonitor.txt
    sed -i 's/\x1b\[K//' qemumonitor.txt
    ls screen*.ppm > /dev/null 2>&1
    if [[ $? -eq 0 ]]
    then
        for screen in screen*.ppm
        do
            ultibo-bash convert $screen ${screen%.ppm}.png && \
            rm $screen
        done
    fi
    convert-frames

    file *

#   grep -i error applog.txt	
#   local EXIT_STATUS=$?

    cd $RESTORE_PWD
#   if [[ EXIT_STATUS == 0 ]]; then log fail: $?; fi
}

function build-ultibo-webstatus {
    build-as QEMUVPB src
}

function build-as {
    local CONTROLLER=$1
    local SRC_FOLDER=$2
    local LPR_FILE=
    if [[ -d $SRC_FOLDER ]]
    then
        ls $SRC_FOLDER/*.lpr > /dev/null 2>&1
        if [[ $? -eq 0 ]]
        then
            LPR_FILE=$SRC_FOLDER/*.lpr
        fi
        if [[ $LPR_FILE != "" ]]
        then
            case $CONTROLLER in
                QEMUVPB)
                    build-lpr $SRC_FOLDER $LPR_FILE $CONTROLLER CONTROLLER_QEMUVPB "-CpARMV7A -WpQEMUVPB" qemuvpb.cfg ;;
                RPI)
                    build-lpr $SRC_FOLDER $LPR_FILE $CONTROLLER CONTROLLER_RPI_INCLUDING_RPI0 "-CpARMV6 -WpRPIB" rpi.cfg ;;
                RPI2)
                    build-lpr $SRC_FOLDER $LPR_FILE $CONTROLLER CONTROLLER_RPI2_INCLUDING_RPI3 "-CpARMV7A -WpRPI2B" rpi2.cfg ;;
                RPI3)
                    build-lpr $SRC_FOLDER $LPR_FILE $CONTROLLER CONTROLLER_RPI3 "-CpARMV7A -WpRPI3B" rpi3.cfg ;;
            esac
        fi
    fi
}

function build-lpr {
    local SRC_FOLDER=$1
    local LPR_FILE=$2
    local CONTROLLER=$3
    local CONTROLLER_SYMBOL=$4
    local CONTROLLER_COMPILER_OPTIONS=$5
    local CFG_NAME=$6
    local INCLUDES="-Fi/root/ultibo/core/fpc/source/packages/fv/src -Fi/root/ultibo/core/fpc/source/rtl/ultibo/core"
    log .... building $LPR_FILE $CONTROLLER
    mkdir -p $ARTIFACTS/$CONTROLLER $OBJ/$CONTROLLER && \
    ultibo-bash fpc \
     -d$CONTROLLER_SYMBOL \
     -l- \
     -v0ewn \
     -B \
     -Tultibo \
     -O2 \
     -Parm \
     -Mdelphi \
     -FuSource \
     -Fugh/ultibohub/Asphyre/Source \
     -FE$OBJ/$CONTROLLER \
     $INCLUDES \
     $CONTROLLER_COMPILER_OPTIONS \
     @/root/ultibo/core/fpc/bin/$CFG_NAME \
     $LPR_FILE |& tee -a $LOG && \
\
    mv kernel* $ARTIFACTS/$CONTROLLER
    if [[ $? -ne 0 ]]; then log fail: $?; fi
}

function create-build-summary {
    cat $LOG | egrep -i '(fail|error|warning|note):' | sort | uniq > $ERRORS
    log
    log Summary:
    log
    cat $ERRORS | tee -a $LOG
    log
    log $(wc $ERRORS)
    if [[ -s $ERRORS ]]
    then
        exit 1
    fi
}

ARTIFACTS=artifacts
OBJ=obj
SCREEN_NUMBER=1
ERRORS=build-errors.txt
LOG=$ARTIFACTS/build.log
rm -rf $ARTIFACTS $OBJ
mkdir -p $ARTIFACTS $OBJ

build-ultibo-webstatus

create-build-summary
