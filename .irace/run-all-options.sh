#!/bin/bash

COMMON_OPTIONS="--port 8806 --average-over 25 --algorithm oll"
IRACE_EXEC=~/R/x86_64-pc-linux-gnu-library/3.4/irace/bin/irace
SCENARIO="$2"

for LAMBDA_TUNING in $1; do
    if [[ "$LAMBDA_TUNING" == "fixed" ]]; then
        LAMBDA_ADDITIONAL="--lambda 8"
    else
        LAMBDA_ADDITIONAL=""
    fi
    for MUTATION_STRENGTH in standard shift resampling; do
        for CROSSOVER_STRENGTH in standard shift resampling; do
            for CROSSOVER_BASE in lambda distance; do
                for MUTANT_STRATEGY in ignore skip-crossover do-not-count do-not-sample; do
                    for POPSIZE_ROUNDING in round-up round-down probabilistic; do
                        MEANINGFUL_OPTIONS="--lambda-tuning $LAMBDA_TUNING $LAMBDA_ADDITIONAL --mutation-strength-alg $MUTATION_STRENGTH --crossover-strength-alg $CROSSOVER_STRENGTH --crossover-strength-base $CROSSOVER_BASE --good-mutant-strategy $MUTANT_STRATEGY --popsize-rounding $POPSIZE_ROUNDING"
                        ALL_OPTIONS="$COMMON_OPTIONS $MEANINGFUL_OPTIONS"
                        export ALL_OPTIONS
                        echo "$MEANINGFUL_OPTIONS"
                        $IRACE_EXEC --scenario $SCENARIO | tail -n 4
                        echo ""
                    done
                done
            done
        done
    done
done
