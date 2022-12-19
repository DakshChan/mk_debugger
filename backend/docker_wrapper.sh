#!/bin/bash

nice -n 19 dockerd &>/dev/null &
npm start &
wait -n
exit $?