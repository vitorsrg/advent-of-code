#!/bin/bash

proj_abspath="$(realpath $(dirname ${BASH_SOURCE[0]}))"
run_day=$1
# run_part=${2:-"-main"}

(cd "$proj_abspath"; lein run -m "$run_day.main/-main" "./$run_day/input.txt")
