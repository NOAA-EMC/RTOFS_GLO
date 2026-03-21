# What's here?
  - Scripts that set up environment variables for what's in:
    - `/pre`
    - `/post`

  - Utitities that are common to the above.
    - None (currently).
  


# Brief Description:

| File name | Brief description | Notes |
| :--       | --: | :-- |
| `detect_machine.sh` | sets up environment variable: `$MACHINE_ID` | Has been taken from the [UFS-weather-model](https://github.com/ufs-community/ufs-weather-model/blob/develop/tests/detect_machine.sh). At some stage, the version from a clone of the UFS should be used. |
| `get_machine_dev_prod.sh` | set up env variables: `$host_name` and `$host_env` | Uses `detect_machine.sh` |
| | |

# Example usage:

- `source ./detect_machine.sh` 

   Note: no arguments are input

- `source get_machine_dev_prod.sh`

   Note: no arguments are input

- 
