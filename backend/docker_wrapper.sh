#!/bin/bash

trap 'kill -SIGINT $(jobs -p)' SIGTERM
trap 'kill -SIGINT $(jobs -p)' SIGINT

nice -n 19 dockerd &>/dev/null &
npm start &

wait -n
exit $?