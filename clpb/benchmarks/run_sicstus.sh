#!/bin/bash

trap "exit" INT

SICS='/usr/local/sicstus4.3.2/bin/sicstus --noinfo --nologo'
bench () {
    echo % start: `date`
    for goal in sat sats taut; do
        $SICS -l $1.pl --goal "bench(sicstus, $1, $2, $goal)."
    done
    echo % end: `date`
}

. instances
