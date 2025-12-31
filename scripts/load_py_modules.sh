#!/bin/bash

export py_mod="ve/evs/2.0_py312"

module use /apps/dev/modulefiles/
module load ${py_mod}
module list
