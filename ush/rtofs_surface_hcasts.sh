#!/bin/sh

################################################################################
# rtofs_surface_hcasts.sh
# from
# /opc/save/grtofs/grtofs_surface_sst.sh
# Get Global RTOFS surface NetCDF files from production and create SST file
# for days 1-3 for the U.S. Coast Guard and post to ftp.
#
# Input Files:
#          /com/rtofs/prod/rtofs.${yyyymmdd}/rtofs_glo_2ds_${fcast}_1hrly_prog.nc for
#          fcast = 00 through 72 hours at 3 hour time-step
#
# B.Daniels/OPC         05/10/13 modified file to work on WCOSS
# B.Rajan/EMC           05/05/15 modified file to work as part of RTOFS
##############################################################################
set -xa
echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)

cd ${DATA_opc}

yyyymmdd=${PDY}

/bin/rm -rf *.nc *.nc

for fcast in n000 n003 n006 n009 n012 n015 n018 n021 n024; do
ln -sf  ${COMOUT}/rtofs_glo_2ds_${fcast}_prog.nc ${DATA_opc}/.
${cdo_r} splitname rtofs_glo_2ds_${fcast}_prog.nc rtofs_glo_2ds_${fcast}_1hrly_
done


${cdo_r} mergetime rtofs_glo_2ds_n000_1hrly_sst.nc rtofs_glo_2ds_n003_1hrly_sst.nc rtofs_glo_2ds_n006_1hrly_sst.nc rtofs_glo_2ds_n009_1hrly_sst.nc rtofs_glo_2ds_n012_1hrly_sst.nc rtofs_glo_2ds_n015_1hrly_sst.nc rtofs_glo_2ds_n018_1hrly_sst.nc rtofs_glo_2ds_n021_1hrly_sst.nc rtofs_glo_2ds_n024_1hrly_sst.nc rtofs_glo_2ds_1hrly_sst_${yyyymmdd}_hcasts.nc

mv rtofs_glo_2ds_1hrly_sst_${yyyymmdd}_hcasts.nc grtofs_sst_${yyyymmdd}_hcasts.nc
gzip grtofs_sst_${yyyymmdd}_hcasts.nc


${cdo_r} mergetime rtofs_glo_2ds_n000_1hrly_u_velocity.nc rtofs_glo_2ds_n003_1hrly_u_velocity.nc rtofs_glo_2ds_n006_1hrly_u_velocity.nc rtofs_glo_2ds_n009_1hrly_u_velocity.nc rtofs_glo_2ds_n012_1hrly_u_velocity.nc rtofs_glo_2ds_n015_1hrly_u_velocity.nc rtofs_glo_2ds_n018_1hrly_u_velocity.nc rtofs_glo_2ds_n021_1hrly_u_velocity.nc rtofs_glo_2ds_n024_1hrly_u_velocity.nc rtofs_glo_2ds_1hrly_u_velocity_${yyyymmdd}_hcasts.nc

${cdo_r} mergetime rtofs_glo_2ds_n000_1hrly_v_velocity.nc rtofs_glo_2ds_n003_1hrly_v_velocity.nc rtofs_glo_2ds_n006_1hrly_v_velocity.nc rtofs_glo_2ds_n009_1hrly_v_velocity.nc rtofs_glo_2ds_n012_1hrly_v_velocity.nc rtofs_glo_2ds_n015_1hrly_v_velocity.nc rtofs_glo_2ds_n018_1hrly_v_velocity.nc rtofs_glo_2ds_n021_1hrly_v_velocity.nc rtofs_glo_2ds_n024_1hrly_v_velocity.nc rtofs_glo_2ds_1hrly_v_velocity_${yyyymmdd}_hcasts.nc

${cdo_r} merge rtofs_glo_2ds_1hrly_u_velocity_${yyyymmdd}_hcasts.nc rtofs_glo_2ds_1hrly_v_velocity_${yyyymmdd}_hcasts.nc rtofs_glo_2ds_1hrly_uv_${yyyymmdd}_hcasts.nc

mv rtofs_glo_2ds_1hrly_uv_${yyyymmdd}_hcasts.nc grtofs_uv_${yyyymmdd}_hcasts.nc

gzip grtofs_uv_${yyyymmdd}_hcasts.nc

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)

