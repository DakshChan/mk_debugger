#!/bin/bash

dockerd &>/dev/null &
npm start &
wait -n
exit $?