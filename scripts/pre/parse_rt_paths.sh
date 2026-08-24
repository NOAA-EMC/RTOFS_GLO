#!/bin/bash

# Ensure required variables are set before proceeding
if [[ -z "${UFSsrc}" || -z "${MACHINE_ID}" ]]; then
    echo "ERROR: UFSsrc and MACHINE_ID must be set before sourcing this script."
    return 1 2>/dev/null || exit 1
fi

DISKNM=""
in_machine_block=false

# Parse DISKNM for the specific machine
while IFS= read -r line; do
    line="${line#"${line%%[![:space:]]*}"}" # Trim leading whitespace
    
    # Did we hit the case block for our machine? (e.g. "wcoss2|acorn)")
    if [[ "$line" == *"${MACHINE_ID}"*")" ]]; then
        in_machine_block=true
    fi
    
    # Grab DISKNM if we are inside the block
    if [[ "$in_machine_block" == true && "$line" == DISKNM=* ]]; then
        DISKNM="${line#DISKNM=}"
        DISKNM="${DISKNM%\"}" # Remove trailing quote
        DISKNM="${DISKNM#\"}" # Remove leading quote
        break
    fi
    
    # Stop parsing if we hit the end of the case block
    if [[ "$in_machine_block" == true && "$line" == ";;" ]]; then
        break
    fi
done < "${UFSsrc}/tests/rt.sh"

# Extract INPUTDATA_ROOT string and replace ${DISKNM} with the actual path
INPUT_RAW=$(grep "^INPUTDATA_ROOT=" "${UFSsrc}/tests/rt.sh" | head -n 1)
RAW_PATH="${INPUT_RAW##*:-}"         # Strips everything up to the ':-'
RAW_PATH="${RAW_PATH%\}}"            # Removes the trailing '}'
export INPUTDATA_ROOT="${RAW_PATH/\$\{DISKNM\}/$DISKNM}"
export DISKNM
