#!/bin/bash

# Return path to RTOFS output on HPSS

function rtofs_hpss_path () {

  local system_name="$1"

  # RTOFS HPSS paths
  # ----------------
  # v2.4
  local v2p4_path_pref=/NCEPPROD/5year/hpssprod/runhistory/

  # v2.5
  local v2p5_path_pref=/NCEPDEV/emc-ocean/5year/Dan.Iredell/

  # v2.5_bad: was stopped on 2025/04/24
  local v2p5_bad_path_pref=emc-ocean/5year/Dan.Iredell/
  # ----------------

  local hpss_path
  case ${system_name} in
    v2p4)
    # version 2.4
    hpss_path=${v2p4_path_pref}
    ;;

    v2p5)
    # version 2.5
    hpss_path=${v2p5_path_pref}
    ;;

    v2p5_bad)
    # version 2.5 parallel discontinued on 04/24/2025
    hpss_path=${v2p5_bad_path_pref}
    ;;

    *)
    # There is no default case
    echo -n "Exiting! Did not code for input RTOFS version: "${system_name}
    exit 1
    ;;
  esac

  echo "${hpss_path}"
}
