#!/bin/bash

# shellcheck disable=SC2046
./target-sender.exe "$@" $(cat params.extra) --seed "$3"
