#! /bin/bash

SELF=`readlink -f "$0"`
WORKDIR=`dirname "$SELF"`

cd "$WORKDIR"


running=`ps -ef | grep epc_socks5 | wc -l`
if [[ running -ge 2 ]]
then
    echo "alreay running"
    exit -1
else
    if [ "$1" = "-d" ]; then
        erl -pa _build/default/lib/*/ebin -config client +K true -s epc_socks5_app start -detached
        echo "epc_socks5 client startd!"
    else
        erl -pa _build/default/lib/*/ebin -config client +K true -s epc_socks5_app start
    fi
fi

exit 0

