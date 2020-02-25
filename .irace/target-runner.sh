#!/bin/bash

DEFAULT="--algorithm oll --lambda-tuning linear --mutation-strength-alg shift --crossover-strength-alg shift --crossover-strength-base distance --good-mutant-strategy do-not-sample --popsize-rounding probabilistic"

echo "$@ $DEFAULT --seed $3" >> ~/irace.in

Z=""
while [[ "$Z" == "" ]]; do
    Z=`head -n 1 ~/irace.out`
done
echo $Z
