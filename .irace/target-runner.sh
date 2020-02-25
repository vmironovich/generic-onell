#!/bin/bash

SCALA_LIBRARY=~/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.13.1.jar

#DEFAULT="--algorithm oll --lambda-tuning fixed --lambda 8 --mutation-strength-alg shift --crossover-strength-alg shift --crossover-strength-base distance --good-mutant-strategy do-not-sample --popsize-rounding probabilistic"
DEFAULT="--average-over 25 --algorithm oll --lambda-tuning fixed --lambda 8 --mutation-strength-alg standard --crossover-strength-alg standard --crossover-strength-base lambda --good-mutant-strategy ignore --popsize-rounding round-up"

java -cp ../target/scala-2.13/classes:$SCALA_LIBRARY ru.ifmo.onell.Main irace "$@" $DEFAULT --seed $3
