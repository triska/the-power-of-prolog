#!/bin/bash

trap "exit" INT

bench () {
    echo % start: `date`
    for goal in sat sats taut; do
        swipl -O -f $1.pl -g "bench(swi, $1, $2, $goal)"
    done
    echo % end: `date`
}

. instances
