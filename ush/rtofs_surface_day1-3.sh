#!/bin/sh
################################################################################
# /opc/save/grtofs/grtofs_surface_sst.sh
# Get Global RTOFS surface NetCDF files from production and create SST file
# for days 1-3 for the U.S. Coast Guard and post to ftp.
#
# Input Files:
#          /com/rtofs/prod/rtofs.${yyyymmdd}/rtofs_glo_2ds_${fcast}_1hrly_prog.nc for
#          fcast = 00 through 72 hours at 3 hour time-step
#
# B.Daniels/OPC         05/10/13 modified file to work on WCOSS
#################################################################################

####
# log
#####

cd ${DATA_opc}

yyyymmdd=${PDY}

/bin/rm -f *.nc *.nc

for fcast in f000 f003 f006 f009 f012 f015 f018 f021 f024 f027 f030 f033 f036 f039 f042 f045 f048 f051 f054 f057 f060 f063 f066 f069 f072; do

ln -sf  ${COMOUT}/rtofs_glo_2ds_${fcast}_prog.nc ${DATA_opc}/.

${cdo_r} splitname rtofs_glo_2ds_${fcast}_prog.nc rtofs_glo_2ds_${fcast}_1hrly_
done

${cdo_r} mergetime rtofs_glo_2ds_f000_1hrly_sst.nc rtofs_glo_2ds_f003_1hrly_sst.nc rtofs_glo_2ds_f006_1hrly_sst.nc rtofs_glo_2ds_f009_1hrly_sst.nc rtofs_glo_2ds_f012_1hrly_sst.nc rtofs_glo_2ds_f015_1hrly_sst.nc rtofs_glo_2ds_f018_1hrly_sst.nc rtofs_glo_2ds_f021_1hrly_sst.nc rtofs_glo_2ds_f024_1hrly_sst.nc rtofs_glo_2ds_1hrly_sst_${yyyymmdd}_day1.nc  


${cdo_r} mergetime rtofs_glo_2ds_f024_1hrly_sst.nc rtofs_glo_2ds_f027_1hrly_sst.nc rtofs_glo_2ds_f030_1hrly_sst.nc rtofs_glo_2ds_f033_1hrly_sst.nc rtofs_glo_2ds_f036_1hrly_sst.nc rtofs_glo_2ds_f039_1hrly_sst.nc rtofs_glo_2ds_f042_1hrly_sst.nc rtofs_glo_2ds_f045_1hrly_sst.nc rtofs_glo_2ds_f048_1hrly_sst.nc rtofs_glo_2ds_1hrly_sst_${yyyymmdd}_day2.nc

${cdo_r} mergetime rtofs_glo_2ds_f048_1hrly_sst.nc rtofs_glo_2ds_f051_1hrly_sst.nc rtofs_glo_2ds_f054_1hrly_sst.nc rtofs_glo_2ds_f057_1hrly_sst.nc rtofs_glo_2ds_f060_1hrly_sst.nc rtofs_glo_2ds_f063_1hrly_sst.nc rtofs_glo_2ds_f066_1hrly_sst.nc rtofs_glo_2ds_f069_1hrly_sst.nc rtofs_glo_2ds_f072_1hrly_sst.nc rtofs_glo_2ds_1hrly_sst_${yyyymmdd}_day3.nc 


gtar -cvf grtofs_sst_${yyyymmdd}.tar rtofs_glo_2ds_1hrly_sst_${yyyymmdd}_day1.nc rtofs_glo_2ds_1hrly_sst_${yyyymmdd}_day2.nc rtofs_glo_2ds_1hrly_sst_${yyyymmdd}_day3.nc

gzip grtofs_sst_${yyyymmdd}.tar

############

${cdo_r} mergetime rtofs_glo_2ds_f000_1hrly_u_velocity.nc rtofs_glo_2ds_f003_1hrly_u_velocity.nc rtofs_glo_2ds_f006_1hrly_u_velocity.nc rtofs_glo_2ds_f009_1hrly_u_velocity.nc rtofs_glo_2ds_f012_1hrly_u_velocity.nc rtofs_glo_2ds_f015_1hrly_u_velocity.nc rtofs_glo_2ds_f018_1hrly_u_velocity.nc rtofs_glo_2ds_f021_1hrly_u_velocity.nc rtofs_glo_2ds_f024_1hrly_u_velocity.nc rtofs_glo_2ds_1hrly_u_velocity_${yyyymmdd}_day1.nc

${cdo_r} mergetime rtofs_glo_2ds_f024_1hrly_u_velocity.nc rtofs_glo_2ds_f027_1hrly_u_velocity.nc rtofs_glo_2ds_f030_1hrly_u_velocity.nc rtofs_glo_2ds_f033_1hrly_u_velocity.nc rtofs_glo_2ds_f036_1hrly_u_velocity.nc rtofs_glo_2ds_f039_1hrly_u_velocity.nc rtofs_glo_2ds_f042_1hrly_u_velocity.nc rtofs_glo_2ds_f045_1hrly_u_velocity.nc rtofs_glo_2ds_f048_1hrly_u_velocity.nc rtofs_glo_2ds_1hrly_u_velocity_${yyyymmdd}_day2.nc

${cdo_r} mergetime rtofs_glo_2ds_f048_1hrly_u_velocity.nc rtofs_glo_2ds_f051_1hrly_u_velocity.nc rtofs_glo_2ds_f054_1hrly_u_velocity.nc rtofs_glo_2ds_f057_1hrly_u_velocity.nc rtofs_glo_2ds_f060_1hrly_u_velocity.nc rtofs_glo_2ds_f063_1hrly_u_velocity.nc rtofs_glo_2ds_f066_1hrly_u_velocity.nc rtofs_glo_2ds_f069_1hrly_u_velocity.nc rtofs_glo_2ds_f072_1hrly_u_velocity.nc rtofs_glo_2ds_1hrly_u_velocity_${yyyymmdd}_day3.nc 

${cdo_r} mergetime rtofs_glo_2ds_f000_1hrly_v_velocity.nc rtofs_glo_2ds_f003_1hrly_v_velocity.nc rtofs_glo_2ds_f006_1hrly_v_velocity.nc rtofs_glo_2ds_f009_1hrly_v_velocity.nc rtofs_glo_2ds_f012_1hrly_v_velocity.nc rtofs_glo_2ds_f015_1hrly_v_velocity.nc rtofs_glo_2ds_f018_1hrly_v_velocity.nc rtofs_glo_2ds_f021_1hrly_v_velocity.nc rtofs_glo_2ds_f024_1hrly_v_velocity.nc rtofs_glo_2ds_1hrly_v_velocity_${yyyymmdd}_day1.nc 

${cdo_r} mergetime rtofs_glo_2ds_f024_1hrly_v_velocity.nc rtofs_glo_2ds_f027_1hrly_v_velocity.nc rtofs_glo_2ds_f030_1hrly_v_velocity.nc rtofs_glo_2ds_f033_1hrly_v_velocity.nc rtofs_glo_2ds_f036_1hrly_v_velocity.nc rtofs_glo_2ds_f039_1hrly_v_velocity.nc rtofs_glo_2ds_f042_1hrly_v_velocity.nc rtofs_glo_2ds_f045_1hrly_v_velocity.nc rtofs_glo_2ds_f048_1hrly_v_velocity.nc rtofs_glo_2ds_1hrly_v_velocity_${yyyymmdd}_day2.nc 

${cdo_r} mergetime rtofs_glo_2ds_f048_1hrly_v_velocity.nc rtofs_glo_2ds_f051_1hrly_v_velocity.nc rtofs_glo_2ds_f054_1hrly_v_velocity.nc rtofs_glo_2ds_f057_1hrly_v_velocity.nc rtofs_glo_2ds_f060_1hrly_v_velocity.nc rtofs_glo_2ds_f063_1hrly_v_velocity.nc rtofs_glo_2ds_f066_1hrly_v_velocity.nc rtofs_glo_2ds_f069_1hrly_v_velocity.nc rtofs_glo_2ds_f072_1hrly_v_velocity.nc rtofs_glo_2ds_1hrly_v_velocity_${yyyymmdd}_day3.nc 

${cdo_r} merge rtofs_glo_2ds_1hrly_u_velocity_${yyyymmdd}_day1.nc rtofs_glo_2ds_1hrly_v_velocity_${yyyymmdd}_day1.nc rtofs_glo_2ds_1hrly_uv_${yyyymmdd}_day1.nc

${cdo_r} merge rtofs_glo_2ds_1hrly_u_velocity_${yyyymmdd}_day2.nc rtofs_glo_2ds_1hrly_v_velocity_${yyyymmdd}_day2.nc rtofs_glo_2ds_1hrly_uv_${yyyymmdd}_day2.nc

${cdo_r} merge rtofs_glo_2ds_1hrly_u_velocity_${yyyymmdd}_day3.nc rtofs_glo_2ds_1hrly_v_velocity_${yyyymmdd}_day3.nc rtofs_glo_2ds_1hrly_uv_${yyyymmdd}_day3.nc  

gtar -cvf grtofs_uv_${yyyymmdd}.tar rtofs_glo_2ds_1hrly_uv_${yyyymmdd}_day1.nc rtofs_glo_2ds_1hrly_uv_${yyyymmdd}_day2.nc rtofs_glo_2ds_1hrly_uv_${yyyymmdd}_day3.nc
gzip grtofs_uv_${yyyymmdd}.tar 

