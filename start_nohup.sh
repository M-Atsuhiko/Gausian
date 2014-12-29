#! /bin/bash
#author: Atsuhiko Murakami

SERVER=$1

if ! [ -n "${SERVER}" ]
then
    echo "ERROR: input server name"
    exit 0
fi

WORK_DIR="/home/murakami/workspace/"
WORK_PROG_NAME="main.sh"
COPY_WHAT="./*"

# command
SCP="scp"
SSH="ssh"
NOHUP="nohup"

eval ${SCP} ${COPY_WHAT} ${SERVER}:${WORK_DIR}
eval ${SSH} ${SERVER}
