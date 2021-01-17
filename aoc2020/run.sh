#!/bin/bash

proj_abspath="$(realpath $(dirname ${BASH_SOURCE[0]}))"
run_day=$1

(cd "$proj_abspath"; lein run -m "$run_day.main/-main" "${@:2}")
