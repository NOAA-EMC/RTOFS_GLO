#!/bin/bash

# Return path to RTOFS output on HPSS

function rtofs_hpss_path () {

  local system_name="$1"

  local hpss_path
  case ${system_name} in
    v2p4)
    # version 2.4
    hpss_path=/NCEPPROD/5year/hpssprod/runhistory/
    ;;

    v2p5)
    # version 2.5
    hpss_path=/NCEPDEV/emc-ocean/5year/Dan.Iredell/
    ;;

    v2p5_bad)
    # version 2.5 parallel discontinued on 04/24/2025
    hpss_path=emc-ocean/5year/Dan.Iredell/
    ;;

    *)
    # There is no default case
    echo -n "Exiting! Did not code for input RTOFS version: "${system_name}
    exit 1
    ;;
  esac

  echo "${hpss_path}"
}
