#!/bin/sh

module load intel/19.1.3.304 PrgEnv-intel/8.3.3 netcdf/4.7.4
module list

# -- Generic inputs that apply to ALL types
rtofs_version=v2.5
rtofs_date=20251217
o_date=20251214

inPath=/lfs/h1/ops/prod/com/rtofs/${rtofs_version}/rtofs.${rtofs_date}/ncoda/ocnqc/
oPath=/lfs/h2/emc/couple/noscrub/santha.akella/qc_decode/

exec_path=/lfs/h2/emc/ptmp/santha.akella/RTOFS_GLO/sorc/rtofs_da.fd/observations/
exec=${exec_path}/./read_binary_qc_obs.x
# --

echo " "
echo "Converting binary QC files to netcdf format..."

# -- SST 
# abuse of naming; mixed up instrument/satellite
oType=sst
sats=("amsr" "goes" "himawari" "msg")
hours=({00..18..06})
#for sat in "${sats[@]}"; do
#  for hour in "${hours[@]}"; do
#    inFile=${inPath}/${sat}/${o_date}${hour}.${sat}
#    oFile=${o_date}${hour}.${sat}.${oType}.nc
#    echo "[${inFile}] to [${oFile}]"
##   ${exec} ${inFile} ${oType} ${oPath} ${oFile}
# done
#done

# -- METOP and VIIRS
sats=("metop" "viirs")
hours=({00..21..03})
satNames=("jpss" "npp") # Used if VIIRS

#for sat in "${sats[@]}"; do
#  for hour in "${hours[@]}"; do
#
#   if [[ "$sat" == "metop" ]]; then
#     inFile=${inPath}/${sat}/${o_date}${hour}.${sat}
#     oFile=${o_date}${hour}.${sat}.${oType}.nc
#     echo "[${inFile}] to [${oFile}]"
#     ${exec} ${inFile} ${oType} ${oPath} ${oFile}
#   fi

#   if [[ "$sat" == "viirs" ]]; then
#     echo " "
#     for satName in "${satNames[@]}"; do
#       inFile=${inPath}/${sat}/${o_date}${hour}.${satName}
#       oFile=${o_date}${hour}.${sat}.${satName}.${oType}.nc
#       echo "[${inFile}] to [${oFile}]"
#       ${exec} ${inFile} ${oType} ${oPath} ${oFile}
#     done
#   fi
# done
#done
echo " "

# -- Ice; dates always have 00UTC
oType=ice
hour=00
instruments=("amsr" "ssmi")
#for instrument in "${instruments[@]}"; do
#  inFile=${inPath}/${oType}/${o_date}${hour}.${instrument}
#  oFile=${o_date}.${instrument}.${oType}.nc
#  echo "[${inFile}] to [${oFile}]"
#  ${exec} ${inFile} ${oType} ${oPath} ${oFile}
#done

echo " "

# -- SSH; dates always have 00UTC
oType=ssh
hour=00
#inFile=${inPath}/${oType}/${o_date}${hour}.${oType}
#oFile=${o_date}.${oType}.nc
#echo "[${inFile}] to [${oFile}]"
#${exec} ${inFile} ${oType} ${oPath} ${oFile}
echo " "

# -- SSS; dates always have 00UTC
hour=00

oType=sss
#inFile=${inPath}/${oType}/${o_date}${hour}.${oType}
#oFile=${o_date}.${oType}.nc
#echo "[${inFile}] to [${oFile}]"
#${exec} ${inFile} ${oType} ${oPath} ${oFile}
echo " "

# -- SSS/Argo near-surface SSS and sat match-up database
oType=mdb  # abuse of name and/or type!
# `mdb` files are -3 days delayed, check if it (they) exist
#if [[ "$oType" == "mdb" ]]; then
#   inFile=${inPath}/sss/${o_date}${hour}.${oType}
#   if [ -f "${inFile}" ]; then
#     oFile=${o_date}.${oType}.sss.nc
#     echo "[${inFile}] to [${oFile}]"
#      ${exec} ${inFile} ${oType} ${oPath} ${oFile}
#    else
#      echo "MDB file not (yet) present for: " ${o_date}${hour}
#      continue
#    fi
#fi
echo " "

# -- Velocity; dates always have 00UTC
hour=00

oType=velocity
#inFile=${inPath}/${oType}/${o_date}${hour}.${oType}
#oFile=${o_date}.${oType}.nc
#echo "[${inFile}] to [${oFile}]"
#${exec} ${inFile} ${oType} ${oPath} ${oFile}
echo " "

# -- sfc (surface); dates always have 00UTC
hour=00

oType=sfc
#inFile=${inPath}/${oType}/${o_date}${hour}.${oType}
#oFile=${o_date}.${oType}.nc
#echo "[${inFile}] to [${oFile}]"
#${exec} ${inFile} ${oType} ${oPath} ${oFile}
echo " "

# -- profile; dates always have 00UTC
hour=00

oType=profile
inFile=${inPath}/${oType}/${o_date}${hour}.${oType}
oFile=${o_date}.${oType}.nc
echo "[${inFile}] to [${oFile}]"
${exec} ${inFile} ${oType} ${oPath} ${oFile}
echo " "

echo "Done."
