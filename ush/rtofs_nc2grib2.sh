#!/bin/ksh
#
###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         rtofs_nc2grib2.sh                                      #
# Script description:  rtofs_nc2grib2.sh <region>                             #
#                                                                             #
# Authors: Bhavani Rajan & Ilya Rivin  Org: NP23         Date: 2013-08-20     #
#                                                                             #
# Abstract: This script creates the hourly grib2 files from global nc files   #
#           for 11 sub regions (except after 72 hours the forecast files are  #
#           3 hourlies) . The regions are:                                    #
#           alaska arctic bering guam gulf_alaska honolulu hudson_baffin      #
#           samoa trop_paci_lowres west_atl west_conus                        #
#                                                                             #
#                                                                             #
# Sub-scripts called:                                                         #
#                                                                             #
# Executables called:                                                         #
#                    rtofs_nc2grb2                                            #
#                                                                             #
#                                                                             #
# Imported variables:                                                         #
#                    RUN_MODE                                                 #
#                    fcstdays_before_thisstep                                 #
#                    modID                                                    #
#                    PDY                                                      #
#                    fhr                                                      #
#                    cdo_r                                                    #
#                    FIXrtofs                                                 #
#                    PARMrtofs                                                #
#                    mode                                                     #
#                    DATA                                                     #
#                                                                             #
# July 2020 : Modified to include ice_coverage and ice_thickness for 3        #          
#             regions: alaska, arctic and bering                              #
# Script history log:                                                         #
# XXXX-XX-XXX  Joe Dow                                                        #
#                                                                             #
###############################################################################

#set -xeu
set -xu

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)

typeset -Z3 fhr
typeset -Z3 intvl_hrly

# Declare directories here
region=$1
regdir=${DATA}/${region}
mkdir -p $region

dhr=024

# Here hours of ouput are hardwired. For the forecast,
# output is hourly for 0-72 hrs and 3-hourly afterwards.

if [ ${fcstdays_before_thisstep} -ge 3 ]
then
   intvl_hrly=${intvl_3hrly}
   fhr=$(expr ${fcstdays_before_thisstep} \* 24 \+ 3)
else
   intvl_hrly=${intvl_1hrly}
fi
fhr=$(expr ${fcstdays_before_thisstep} \* 24 \+ ${intvl_hrly})
nhr=$(expr \( ${fcstdays_before_thisstep} \+ ${fcstdays} \) \* 24)
echo "fhr=$fhr nhr=$nhr"

export year=$(echo $PDY | cut -c1-4)
export mycyc=${mycyc:-00}
export month=$(echo $PDY | cut -c5-6)
export day=$(echo $PDY | cut -c7-8)

if [ ${mode} = 'tm' ]; then
export gen_pro=14
fi
if [ ${mode} = 'f' ]; then
export gen_pro=2
fi

cd $regdir

test -f ${region}.out && rm ${region}.out
touch nc.out ${region}.out

# Set up for infiles here: get the lat0 lat1 lon0 lon1 dlat and dlon from des files
xx=$(grep -i "xsize" ${FIXrtofs}/${RUN}_grid_${region}.des | cut -f2 -d "=" )
yy=$(grep -i "ysize" ${FIXrtofs}/${RUN}_grid_${region}.des | cut -f2 -d "=" )
x0=$(grep -i "xfirst" ${FIXrtofs}/${RUN}_grid_${region}.des | cut -f2 -d "=" )
y0=$(grep -i "yfirst" ${FIXrtofs}/${RUN}_grid_${region}.des | cut -f2 -d "=" )
xinc=$(grep -i "xinc" ${FIXrtofs}/${RUN}_grid_${region}.des | cut -f2 -d "=" )
yinc=$(grep -i "yinc" ${FIXrtofs}/${RUN}_grid_${region}.des | cut -f2 -d "=" )

# generate weights file for this region
$cdo_r genbil,${FIXrtofs}/${RUN}_grid_${region}.des $DATA/${RUN}_${modID}_2ds.${mode}${fhr}.nc ${RUN}_${region}_weights.nc
if [ ${region} = 'alaska'  -o  ${region} = 'bering'  -o  ${region} = 'arctic' ]
then
  $cdo_r genbil,${FIXrtofs}/${RUN}_grid_${region}.des $DATA/${RUN}_${modID}_2ds.${mode}${fhr}.ice.nc ${RUN}_${region}_icthknss_weights.nc
fi

# loop over forecast hours
while [ $fhr -le $nhr ]
do
  ahr=$fhr
  # if we go in reverse order for tm files, then parm file needs to do the same
  if [ ${mode} = 'tm' ];then let ahr=24-fhr;fi
  ahr=$(printf "%03d\n" $ahr)
  if [[ ${mode} = 'f' || $ahr -ne 0 ]] # skip last tm file
  then
  #echo xx yy day year month fcsthr cyc param# category# x0 y0 xinc yinc
  # create infiles for each region below:
  echo $xx $yy  $day $year $month $ahr $mycyc 0 3 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_sst_${region}
  echo $xx $yy  $day $year $month $ahr $mycyc 3 4 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_sss_${region}
  echo $xx $yy  $day $year $month $ahr $mycyc 2 1 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_u_velocity_${region}
  echo $xx $yy  $day $year $month $ahr $mycyc 3 1 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_v_velocity_${region}
  echo $xx $yy  $day $year $month $ahr $mycyc 195 3 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_ssh_${region}
  echo $xx $yy  $day $year $month $ahr $mycyc 194 1 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_ubaro_${region}
  echo $xx $yy  $day $year $month $ahr $mycyc 195 1 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_vbaro_${region}

  # Split the netCDF file into components
  $cdo_r splitname $DATA/${RUN}_${modID}_2ds.${mode}${ahr}.nc ${RUN}_${modID}_2ds.${mode}${ahr}.
#### Added for ice
  if [ ${region} = 'alaska'  -o  ${region} = 'bering'  -o  ${region} = 'arctic' ]; then
    echo $xx $yy  $day $year $month $ahr $mycyc  0 2 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_ice_coverage_${region}
    echo $xx $yy  $day $year $month $ahr $mycyc  1 2 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_ice_thickness_${region}
    $cdo_r splitname $DATA/${RUN}_${modID}_2ds.${mode}${ahr}.ice.nc ${RUN}_${modID}_2ds.${mode}${ahr}.ice_
  fi

  # Some housekeeping
  touch ${RUN}_${modID}_${mode}_temp_${region}_std.grb2

  # Interpolate the global grid netCDF to latlon grid asc file

  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds.${mode}${ahr}.sst.nc sst_std_${region}.nc
  $cdo_r outputf,%8.4f,1 sst_std_${region}.nc > sst_std_${region}.asc

  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds.${mode}${ahr}.sss.nc sss_std_${region}.nc
  $cdo_r outputf,%8.4f,1 sss_std_${region}.nc > sss_std_${region}.asc

  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds.${mode}${ahr}.u_velocity.nc u_velocity_std_${region}.nc
  $cdo_r outputf,%8.4f,1 u_velocity_std_${region}.nc > u_velocity_std_${region}.asc

  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds.${mode}${ahr}.v_velocity.nc v_velocity_std_${region}.nc
  $cdo_r outputf,%8.4f,1 v_velocity_std_${region}.nc > v_velocity_std_${region}.asc

  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds.${mode}${ahr}.ssh.nc ssh_std_${region}.nc
  $cdo_r outputf,%8.4f,1 ssh_std_${region}.nc > ssh_std_${region}.asc

  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds.${mode}${ahr}.u_barotropic_velocity.nc ubaro_std_${region}.nc
  $cdo_r outputf,%8.4f,1 ubaro_std_${region}.nc > ubaro_std_${region}.asc

  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds.${mode}${ahr}.v_barotropic_velocity.nc vbaro_std_${region}.nc
  $cdo_r outputf,%8.4f,1 vbaro_std_${region}.nc > vbaro_std_${region}.asc
####
### Added for ice
if [ ${region} = 'alaska'  -o  ${region} = 'bering'  -o  ${region} = 'arctic' ]; then
  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds.${mode}${ahr}.ice_ice_coverage.nc ice_coverage_std_${region}.nc
  $cdo_r outputf,%8.4f,1 ice_coverage_std_${region}.nc > ice_coverage_std_${region}.asc

## Using different mask file for thickness as the source mask has too many missing values
  $cdo_r -L -R remap,${FIXrtofs}/${RUN}_grid_${region}.des,${RUN}_${region}_icthknss_weights.nc -setmisstoc,0 ${RUN}_${modID}_2ds.${mode}${ahr}.ice_ice_thickness.nc ice_thickness_std_${region}.nc
  $cdo_r outputf,%8.4f,1 ice_thickness_std_${region}.nc > ice_thickness_std_${region}.asc
fi
  #************
  # Pack the variables in GRIB 
### Added for ice for 3 regions
 if [ ${region} = 'alaska'  -o  ${region} = 'bering'  -o  ${region} = 'arctic' ]; then
    VARLIST="sst sss u_velocity v_velocity ssh ice_thickness ice_coverage ubaro vbaro"
 else
    VARLIST="sst sss u_velocity v_velocity ssh ubaro vbaro"
 fi
  for var in $VARLIST
  do
    echo ${var}_std.asc var_std_${region}.asc
    test -f fort.50 && rm -f fort.50
    test -f fort.20 && rm -f fort.20
    test -f fort.30 && rm -f fort.30
    ln -s ${regdir}/${var}_std_${region}.asc fort.20
    echo ${var} > var_name
    ln -s var_name fort.30

    export XLFUNIT_50=${regdir}/${var}_${ahr}_std_${region}.grb2

    $EXECrtofs/${RUN}_nc2grb2 < ${regdir}/infile_${var}_${region}  > nc2grb.ft06_${region} 2>> nc2grb.err_${region}
    # export err=$?; err_chk
    cat ${regdir}/${var}_${ahr}_std_${region}.grb2 >> ${RUN}_${modID}_${mode}_temp_${region}_std.grb2
  done # var
  fi
  fhr=$(expr $fhr + $intvl_hrly)
done # fhr

fhr=$(expr $fhr - $intvl_hrly)
ahr=$fhr
# keep fhr for tm (results in file being tm024)
#if [ ${mode} = 'tm' ];then let ahr=24-fhr;fi
ahr=$(printf "%03d\n" $ahr)
cp ${RUN}_${modID}_${mode}_temp_${region}_std.grb2 ${RUN}_${modID}.t${mycyc}z.${mode}${ahr}.${region}_std.grib2

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)
