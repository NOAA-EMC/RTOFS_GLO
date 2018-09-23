#!/bin/sh

################################################################################
# rtofs_surface_hcasts.sh
# from
# /opc/save/grtofs/grtofs_surface_sst.sh
# Get Global RTOFS surface NetCDF files from production and create SST file
# for days 1-3 for the U.S. Coast Guard and post to ftp.
#
# Input Files:
#          /com/rtofs/prod/rtofs.${yyyymmdd}/rtofs_glo_2ds_${fcast}_3hrly_prog.nc for
#          fcast = 00 through 72 hours at 3 hour time-step
#
# B.Daniels/OPC         05/10/13 modified file to work on WCOSS
# B.Rajan/EMC           05/05/15 modified file to work as part of RTOFS
##############################################################################
set -xa
echo "*** Started script $0 on hostname "`hostname`' at time '`date`

cd ${DATA_opc}

yyyymmdd=${PDY}

/bin/rm -rf *nc

for fcast in n024 n027 n030 n033 n036 n039 n042 n045 n048; do
ln -sf  ${COMOUT}/rtofs_glo_2ds_${fcast}_3hrly_prog.nc ${DATA_opc}/.
${cdo_r} splitname rtofs_glo_2ds_${fcast}_3hrly_prog.nc rtofs_glo_2ds_${fcast}_3hrly_
done


${cdo_r} mergetime rtofs_glo_2ds_n024_3hrly_sst.nc rtofs_glo_2ds_n027_3hrly_sst.nc rtofs_glo_2ds_n030_3hrly_sst.nc rtofs_glo_2ds_n033_3hrly_sst.nc rtofs_glo_2ds_n036_3hrly_sst.nc rtofs_glo_2ds_n039_3hrly_sst.nc rtofs_glo_2ds_n042_3hrly_sst.nc rtofs_glo_2ds_n045_3hrly_sst.nc rtofs_glo_2ds_n048_3hrly_sst.nc rtofs_glo_2ds_3hrly_sst_${yyyymmdd}_hcasts.nc 

mv rtofs_glo_2ds_3hrly_sst_${yyyymmdd}_hcasts.nc grtofs_sst_${yyyymmdd}_hcasts.nc
gzip grtofs_sst_${yyyymmdd}_hcasts.nc


${cdo_r} mergetime rtofs_glo_2ds_n024_3hrly_u_velocity.nc rtofs_glo_2ds_n027_3hrly_u_velocity.nc rtofs_glo_2ds_n030_3hrly_u_velocity.nc rtofs_glo_2ds_n033_3hrly_u_velocity.nc rtofs_glo_2ds_n036_3hrly_u_velocity.nc rtofs_glo_2ds_n039_3hrly_u_velocity.nc rtofs_glo_2ds_n042_3hrly_u_velocity.nc rtofs_glo_2ds_n045_3hrly_u_velocity.nc rtofs_glo_2ds_n048_3hrly_u_velocity.nc rtofs_glo_2ds_3hrly_u_velocity_${yyyymmdd}_hcasts.nc

${cdo_r} mergetime rtofs_glo_2ds_n024_3hrly_v_velocity.nc rtofs_glo_2ds_n027_3hrly_v_velocity.nc rtofs_glo_2ds_n030_3hrly_v_velocity.nc rtofs_glo_2ds_n033_3hrly_v_velocity.nc rtofs_glo_2ds_n036_3hrly_v_velocity.nc rtofs_glo_2ds_n039_3hrly_v_velocity.nc rtofs_glo_2ds_n042_3hrly_v_velocity.nc rtofs_glo_2ds_n045_3hrly_v_velocity.nc rtofs_glo_2ds_n048_3hrly_v_velocity.nc rtofs_glo_2ds_3hrly_v_velocity_${yyyymmdd}_hcasts.nc

${cdo_r} merge rtofs_glo_2ds_3hrly_u_velocity_${yyyymmdd}_hcasts.nc rtofs_glo_2ds_3hrly_v_velocity_${yyyymmdd}_hcasts.nc rtofs_glo_2ds_3hrly_uv_${yyyymmdd}_hcasts.nc

mv rtofs_glo_2ds_3hrly_uv_${yyyymmdd}_hcasts.nc grtofs_uv_${yyyymmdd}_hcasts.nc

gzip grtofs_uv_${yyyymmdd}_hcasts.nc



echo "*** Finished script $0 on hostname "`hostname`' at time '`date`

