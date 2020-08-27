#module load NetCDF
#module load bufr
#module load w3nco
#module load w3emc
module purge
module load EnvVars/1.0.3
module load ips/18.0.1.163
module load impi/18.0.1
module load prod_util/1.1.3
module load prod_envir/1.0.3
module load NetCDF/4.5.0
module load bufr/11.3.0
module load bufr_dumplist/2.1.0
module load w3nco/2.0.6
module load w3emc/2.3.0
module load mmab/3.5.0


make ssmis_tol2 -f makefile_ssmisu
