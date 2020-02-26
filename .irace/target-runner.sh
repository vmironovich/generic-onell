#!/bin/bash

#DEFAULT="--algorithm oll --lambda-tuning fixed --lambda 8 --mutation-strength-alg shift --crossover-strength-alg shift --crossover-strength-base distance --good-mutant-strategy do-not-sample --popsize-rounding probabilistic"
DEFAULT="--average-over 25 --algorithm oll --lambda-tuning fixed --lambda 8 --mutation-strength-alg standard --crossover-strength-alg standard --crossover-strength-base lambda --good-mutant-strategy ignore --popsize-rounding round-up"

./target-sender.exe --port 8806 "$@" $DEFAULT --seed $3
