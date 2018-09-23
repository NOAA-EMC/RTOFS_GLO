#!/bin/sh
################################################################################
# /opc/save/grtofs/grtofs_surface_sst.sh
# Get Global RTOFS surface NetCDF files from production and create SST file
# for days 1-3 for the U.S. Coast Guard and post to ftp.
#
# Input Files:
#          /com/rtofs/prod/rtofs.${yyyymmdd}/rtofs_glo_2ds_${fcast}_3hrly_prog.nc for
#          fcast = 00 through 72 hours at 3 hour time-step
#
# B.Daniels/OPC         05/10/13 modified file to work on WCOSS
#################################################################################

####
# log
#####

cd ${DATA_opc}

yyyymmdd=${PDY}

/bin/rm -f *.nc

for fcast in f000 f003 f006 f009 f012 f015 f018 f021 f024 f027 f030 f033 f036 f039 f042 f045 f048 f051 f054 f057 f060 f063 f066 f069 f072; do

ln -sf  ${COMOUT}/rtofs_glo_2ds_${fcast}_3hrly_prog.nc ${DATA_opc}/.

${cdo_r} splitname rtofs_glo_2ds_${fcast}_3hrly_prog.nc rtofs_glo_2ds_${fcast}_3hrly_
done

${cdo_r} mergetime rtofs_glo_2ds_f000_3hrly_sst.nc rtofs_glo_2ds_f003_3hrly_sst.nc rtofs_glo_2ds_f006_3hrly_sst.nc rtofs_glo_2ds_f009_3hrly_sst.nc rtofs_glo_2ds_f012_3hrly_sst.nc rtofs_glo_2ds_f015_3hrly_sst.nc rtofs_glo_2ds_f018_3hrly_sst.nc rtofs_glo_2ds_f021_3hrly_sst.nc rtofs_glo_2ds_f024_3hrly_sst.nc rtofs_glo_2ds_3hrly_sst_${yyyymmdd}_day1.nc  


${cdo_r} mergetime rtofs_glo_2ds_f024_3hrly_sst.nc rtofs_glo_2ds_f027_3hrly_sst.nc rtofs_glo_2ds_f030_3hrly_sst.nc rtofs_glo_2ds_f033_3hrly_sst.nc rtofs_glo_2ds_f036_3hrly_sst.nc rtofs_glo_2ds_f039_3hrly_sst.nc rtofs_glo_2ds_f042_3hrly_sst.nc rtofs_glo_2ds_f045_3hrly_sst.nc rtofs_glo_2ds_f048_3hrly_sst.nc rtofs_glo_2ds_3hrly_sst_${yyyymmdd}_day2.nc 

${cdo_r} mergetime rtofs_glo_2ds_f048_3hrly_sst.nc rtofs_glo_2ds_f051_3hrly_sst.nc rtofs_glo_2ds_f054_3hrly_sst.nc rtofs_glo_2ds_f057_3hrly_sst.nc rtofs_glo_2ds_f060_3hrly_sst.nc rtofs_glo_2ds_f063_3hrly_sst.nc rtofs_glo_2ds_f066_3hrly_sst.nc rtofs_glo_2ds_f069_3hrly_sst.nc rtofs_glo_2ds_f072_3hrly_sst.nc rtofs_glo_2ds_3hrly_sst_${yyyymmdd}_day3.nc 


gtar -cvf grtofs_sst_${yyyymmdd}.tar rtofs_glo_2ds_3hrly_sst_${yyyymmdd}_day1.nc rtofs_glo_2ds_3hrly_sst_${yyyymmdd}_day2.nc rtofs_glo_2ds_3hrly_sst_${yyyymmdd}_day3.nc

gzip grtofs_sst_${yyyymmdd}.tar

############

${cdo_r} mergetime rtofs_glo_2ds_f000_3hrly_u_velocity.nc rtofs_glo_2ds_f003_3hrly_u_velocity.nc rtofs_glo_2ds_f006_3hrly_u_velocity.nc rtofs_glo_2ds_f009_3hrly_u_velocity.nc rtofs_glo_2ds_f012_3hrly_u_velocity.nc rtofs_glo_2ds_f015_3hrly_u_velocity.nc rtofs_glo_2ds_f018_3hrly_u_velocity.nc rtofs_glo_2ds_f021_3hrly_u_velocity.nc rtofs_glo_2ds_f024_3hrly_u_velocity.nc rtofs_glo_2ds_3hrly_u_velocity_${yyyymmdd}_day1.nc 

${cdo_r} mergetime rtofs_glo_2ds_f024_3hrly_u_velocity.nc rtofs_glo_2ds_f027_3hrly_u_velocity.nc rtofs_glo_2ds_f030_3hrly_u_velocity.nc rtofs_glo_2ds_f033_3hrly_u_velocity.nc rtofs_glo_2ds_f036_3hrly_u_velocity.nc rtofs_glo_2ds_f039_3hrly_u_velocity.nc rtofs_glo_2ds_f042_3hrly_u_velocity.nc rtofs_glo_2ds_f045_3hrly_u_velocity.nc rtofs_glo_2ds_f048_3hrly_u_velocity.nc rtofs_glo_2ds_3hrly_u_velocity_${yyyymmdd}_day2.nc 

${cdo_r} mergetime rtofs_glo_2ds_f048_3hrly_u_velocity.nc rtofs_glo_2ds_f051_3hrly_u_velocity.nc rtofs_glo_2ds_f054_3hrly_u_velocity.nc rtofs_glo_2ds_f057_3hrly_u_velocity.nc rtofs_glo_2ds_f060_3hrly_u_velocity.nc rtofs_glo_2ds_f063_3hrly_u_velocity.nc rtofs_glo_2ds_f066_3hrly_u_velocity.nc rtofs_glo_2ds_f069_3hrly_u_velocity.nc rtofs_glo_2ds_f072_3hrly_u_velocity.nc rtofs_glo_2ds_3hrly_u_velocity_${yyyymmdd}_day3.nc 

${cdo_r} mergetime rtofs_glo_2ds_f000_3hrly_v_velocity.nc rtofs_glo_2ds_f003_3hrly_v_velocity.nc rtofs_glo_2ds_f006_3hrly_v_velocity.nc rtofs_glo_2ds_f009_3hrly_v_velocity.nc rtofs_glo_2ds_f012_3hrly_v_velocity.nc rtofs_glo_2ds_f015_3hrly_v_velocity.nc rtofs_glo_2ds_f018_3hrly_v_velocity.nc rtofs_glo_2ds_f021_3hrly_v_velocity.nc rtofs_glo_2ds_f024_3hrly_v_velocity.nc rtofs_glo_2ds_3hrly_v_velocity_${yyyymmdd}_day1.nc 

${cdo_r} mergetime rtofs_glo_2ds_f024_3hrly_v_velocity.nc rtofs_glo_2ds_f027_3hrly_v_velocity.nc rtofs_glo_2ds_f030_3hrly_v_velocity.nc rtofs_glo_2ds_f033_3hrly_v_velocity.nc rtofs_glo_2ds_f036_3hrly_v_velocity.nc rtofs_glo_2ds_f039_3hrly_v_velocity.nc rtofs_glo_2ds_f042_3hrly_v_velocity.nc rtofs_glo_2ds_f045_3hrly_v_velocity.nc rtofs_glo_2ds_f048_3hrly_v_velocity.nc rtofs_glo_2ds_3hrly_v_velocity_${yyyymmdd}_day2.nc 

${cdo_r} mergetime rtofs_glo_2ds_f048_3hrly_v_velocity.nc rtofs_glo_2ds_f051_3hrly_v_velocity.nc rtofs_glo_2ds_f054_3hrly_v_velocity.nc rtofs_glo_2ds_f057_3hrly_v_velocity.nc rtofs_glo_2ds_f060_3hrly_v_velocity.nc rtofs_glo_2ds_f063_3hrly_v_velocity.nc rtofs_glo_2ds_f066_3hrly_v_velocity.nc rtofs_glo_2ds_f069_3hrly_v_velocity.nc rtofs_glo_2ds_f072_3hrly_v_velocity.nc rtofs_glo_2ds_3hrly_v_velocity_${yyyymmdd}_day3.nc 

${cdo_r} merge rtofs_glo_2ds_3hrly_u_velocity_${yyyymmdd}_day1.nc rtofs_glo_2ds_3hrly_v_velocity_${yyyymmdd}_day1.nc rtofs_glo_2ds_3hrly_uv_${yyyymmdd}_day1.nc 

${cdo_r} merge rtofs_glo_2ds_3hrly_u_velocity_${yyyymmdd}_day2.nc rtofs_glo_2ds_3hrly_v_velocity_${yyyymmdd}_day2.nc rtofs_glo_2ds_3hrly_uv_${yyyymmdd}_day2.nc

${cdo_r} merge rtofs_glo_2ds_3hrly_u_velocity_${yyyymmdd}_day3.nc rtofs_glo_2ds_3hrly_v_velocity_${yyyymmdd}_day3.nc rtofs_glo_2ds_3hrly_uv_${yyyymmdd}_day3.nc  

gtar -cvf grtofs_uv_${yyyymmdd}.tar rtofs_glo_2ds_3hrly_uv_${yyyymmdd}_day1.nc rtofs_glo_2ds_3hrly_uv_${yyyymmdd}_day2.nc rtofs_glo_2ds_3hrly_uv_${yyyymmdd}_day3.nc
gzip grtofs_uv_${yyyymmdd}.tar 

