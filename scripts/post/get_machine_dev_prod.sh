#!/bin/bash

# 1. Source the machine detection script
source ./detect_machine.sh

# 2. Initialize variables
host_env="unknown"
host_name="nobody"

# 3. WCOSS2-Specific Logic (Cactus, Dogwood, Acorn)
if [[ "${MACHINE_ID}" == "wcoss2" || "${MACHINE_ID}" == "acorn" ]]; then
    
    # Identify the specific cluster
    h=$(hostname | cut -c1-1)
    case "$h" in
        c) host_name="cactus" ;;
        d) host_name="dogwood" ;;
        a) host_name="acorn"  ;; # Added Acorn support
        *) host_name="nobody" ;;
    esac

    # 4. Check for Production/Development status
    # Standard location for the prodmachinefile on WCOSS2
    PROD_CFG="/lfs/h1/ops/prod/config/prodmachinefile"
    
    if [[ -s "$PROD_CFG" ]]; then
        hprod=$(grep primary "$PROD_CFG" | cut -d: -f2)
        hdev=$(grep backup "$PROD_CFG" | cut -d: -f2)

        if [[ "$host_name" == "$hprod" ]]; then
            host_env="prod"
        elif [[ "$host_name" == "$hdev" ]]; then
            host_env="dev"
        elif [[ "$host_name" == "acorn" ]]; then
            # Acorn is strictly a development/test system
            host_env="dev"
        else
            host_env="unknown"
        fi
    else
        # Fallback if the config file is inaccessible
        echo "Warning: $PROD_CFG not found. Defaulting host_env to dev."
        host_env="dev"
    fi

else
    # Logic for Ursa, Hera, etc.
    host_name="${MACHINE_ID}"
    #host_env="dev"
fi

# Export variables for downstream scripts
export host_env
export host_name

echo ">>> Machine: ${host_name} | Environment: ${host_env}"
