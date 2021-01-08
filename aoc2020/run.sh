#!/bin/bash

proj_abspath="$(realpath $(dirname ${BASH_SOURCE[0]}))"
run_day=$1

if [ -z "$2" ]; then
    run_input="$proj_abspath/$run_day/input.txt"
else
    run_input=${2}
fi

(cd "$proj_abspath"; lein run -m "$run_day.main/-main" "$run_input")
