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

/bin/rm -f *.nc *.nc4

for fcast in f072 f075 f078 f081 f084 f087 f090 f093 f096 f099 f102 f105 f108 f111 f114 f117 f120
do
ln -sf ${COMOUT}/rtofs_glo_2ds_${fcast}_prog.nc ${DATA_opc}/.

${cdo_r} splitname rtofs_glo_2ds_${fcast}_prog.nc rtofs_glo_2ds_${fcast}_3hrly_
done

${cdo_r} mergetime rtofs_glo_2ds_f072_3hrly_sst.nc4 rtofs_glo_2ds_f075_3hrly_sst.nc4 rtofs_glo_2ds_f078_3hrly_sst.nc4 rtofs_glo_2ds_f081_3hrly_sst.nc4 rtofs_glo_2ds_f084_3hrly_sst.nc4 rtofs_glo_2ds_f087_3hrly_sst.nc4 rtofs_glo_2ds_f090_3hrly_sst.nc4 rtofs_glo_2ds_f093_3hrly_sst.nc4 rtofs_glo_2ds_f096_3hrly_sst.nc4 rtofs_glo_2ds_3hrly_sst_${yyyymmdd}_day4.nc

${cdo_r} mergetime rtofs_glo_2ds_f096_3hrly_sst.nc4 rtofs_glo_2ds_f099_3hrly_sst.nc4 rtofs_glo_2ds_f102_3hrly_sst.nc4 rtofs_glo_2ds_f105_3hrly_sst.nc4 rtofs_glo_2ds_f108_3hrly_sst.nc4 rtofs_glo_2ds_f111_3hrly_sst.nc4 rtofs_glo_2ds_f114_3hrly_sst.nc4 rtofs_glo_2ds_f117_3hrly_sst.nc4 rtofs_glo_2ds_f120_3hrly_sst.nc4 rtofs_glo_2ds_3hrly_sst_${yyyymmdd}_day5.nc 

mv rtofs_glo_2ds_3hrly_sst_${yyyymmdd}_day4.nc grtofs_sst_${yyyymmdd}_day4.nc
mv rtofs_glo_2ds_3hrly_sst_${yyyymmdd}_day5.nc grtofs_sst_${yyyymmdd}_day5.nc
gzip grtofs_sst_${yyyymmdd}_day4.nc
gzip grtofs_sst_${yyyymmdd}_day5.nc


${cdo_r} mergetime rtofs_glo_2ds_f072_3hrly_u_velocity.nc4 rtofs_glo_2ds_f075_3hrly_u_velocity.nc4 rtofs_glo_2ds_f078_3hrly_u_velocity.nc4 rtofs_glo_2ds_f081_3hrly_u_velocity.nc4 rtofs_glo_2ds_f084_3hrly_u_velocity.nc4 rtofs_glo_2ds_f087_3hrly_u_velocity.nc4 rtofs_glo_2ds_f090_3hrly_u_velocity.nc4 rtofs_glo_2ds_f093_3hrly_u_velocity.nc4 rtofs_glo_2ds_f096_3hrly_u_velocity.nc4 rtofs_glo_2ds_3hrly_u_velocity_${yyyymmdd}_day4.nc

${cdo_r} mergetime rtofs_glo_2ds_f072_3hrly_v_velocity.nc4 rtofs_glo_2ds_f075_3hrly_v_velocity.nc4 rtofs_glo_2ds_f078_3hrly_v_velocity.nc4 rtofs_glo_2ds_f081_3hrly_v_velocity.nc4 rtofs_glo_2ds_f084_3hrly_v_velocity.nc4 rtofs_glo_2ds_f087_3hrly_v_velocity.nc4 rtofs_glo_2ds_f090_3hrly_v_velocity.nc4 rtofs_glo_2ds_f093_3hrly_v_velocity.nc4 rtofs_glo_2ds_f096_3hrly_v_velocity.nc4 rtofs_glo_2ds_3hrly_v_velocity_${yyyymmdd}_day4.nc 

${cdo_r} mergetime rtofs_glo_2ds_f096_3hrly_u_velocity.nc4 rtofs_glo_2ds_f099_3hrly_u_velocity.nc4 rtofs_glo_2ds_f102_3hrly_u_velocity.nc4 rtofs_glo_2ds_f105_3hrly_u_velocity.nc4 rtofs_glo_2ds_f108_3hrly_u_velocity.nc4 rtofs_glo_2ds_f111_3hrly_u_velocity.nc4 rtofs_glo_2ds_f114_3hrly_u_velocity.nc4 rtofs_glo_2ds_f117_3hrly_u_velocity.nc4 rtofs_glo_2ds_f120_3hrly_u_velocity.nc4 rtofs_glo_2ds_3hrly_u_velocity_${yyyymmdd}_day5.nc 

${cdo_r} mergetime rtofs_glo_2ds_f096_3hrly_v_velocity.nc4 rtofs_glo_2ds_f099_3hrly_v_velocity.nc4 rtofs_glo_2ds_f102_3hrly_v_velocity.nc4 rtofs_glo_2ds_f105_3hrly_v_velocity.nc4 rtofs_glo_2ds_f108_3hrly_v_velocity.nc4 rtofs_glo_2ds_f111_3hrly_v_velocity.nc4 rtofs_glo_2ds_f114_3hrly_v_velocity.nc4 rtofs_glo_2ds_f117_3hrly_v_velocity.nc4 rtofs_glo_2ds_f120_3hrly_v_velocity.nc4 rtofs_glo_2ds_3hrly_v_velocity_${yyyymmdd}_day5.nc 

${cdo_r} merge rtofs_glo_2ds_3hrly_u_velocity_${yyyymmdd}_day4.nc rtofs_glo_2ds_3hrly_v_velocity_${yyyymmdd}_day4.nc grtofs_uv_${yyyymmdd}_day4.nc

${cdo_r} merge rtofs_glo_2ds_3hrly_u_velocity_${yyyymmdd}_day5.nc rtofs_glo_2ds_3hrly_v_velocity_${yyyymmdd}_day5.nc grtofs_uv_${yyyymmdd}_day5.nc


gzip grtofs_uv_${yyyymmdd}_day4.nc
gzip grtofs_uv_${yyyymmdd}_day5.nc
