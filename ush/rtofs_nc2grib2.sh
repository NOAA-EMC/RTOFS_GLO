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

if [ ${mode} = 'n' ]; then
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

# create infiles for each region below:

while [ $fhr -le $nhr ]
do

  #echo xx yy day year month fcsthr cyc param# category# x0 y0 xinc yinc
  echo $xx $yy  $day $year $month $fhr $mycyc 0 3 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_sst_${region}
  echo $xx $yy  $day $year $month $fhr $mycyc 3 4 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_sss_${region}
  echo $xx $yy  $day $year $month $fhr $mycyc 2 1 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_u_velocity_${region}
  echo $xx $yy  $day $year $month $fhr $mycyc 3 1 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_v_velocity_${region}
  echo $xx $yy  $day $year $month $fhr $mycyc 195 3 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_ssh_${region}
  echo $xx $yy  $day $year $month $fhr $mycyc 194 1 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_ubaro_${region}
  echo $xx $yy  $day $year $month $fhr $mycyc 195 1 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_vbaro_${region}


  # Split the netCDF file into components
    $cdo_r splitname $DATA/${RUN}_${modID}_2ds_${mode}${fhr}_diag.nc ${RUN}_${modID}_2ds_${mode}${fhr}_diag_
    $cdo_r splitname $DATA/${RUN}_${modID}_2ds_${mode}${fhr}_prog.nc ${RUN}_${modID}_2ds_${mode}${fhr}_prog_
#### Added for ice
    if [ ${region} = 'alaska'  -o  ${region} = 'bering'  -o  ${region} = 'arctic' ]; then
  echo $xx $yy  $day $year $month $fhr $mycyc  0 2 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_ice_coverage_${region}
  echo $xx $yy  $day $year $month $fhr $mycyc  1 2 $x0 $y0 $xinc $yinc 0 $gen_pro > infile_ice_thickness_${region}
      $cdo_r splitname $DATA/${RUN}_${modID}_2ds_${mode}${fhr}_ice.nc ${RUN}_${modID}_2ds_${mode}${fhr}_ice_
    fi

  # Some housekeeping
  touch ${RUN}_${modID}_${mode}_temp_${region}_std.grb2

  # Interpolate the global grid netCDF to latlon grid asc file


  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${FIXrtofs}/${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds_${mode}${fhr}_prog_sst.nc sst_std_${region}.nc
  $cdo_r outputf,%8.4f,1 sst_std_${region}.nc > sst_std_${region}.asc
  test -f ${RUN}_${modID}_2ds_${mode}${fhr}_prog_sst.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_prog_sst.nc

  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${FIXrtofs}/${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds_${mode}${fhr}_prog_sss.nc sss_std_${region}.nc
  $cdo_r outputf,%8.4f,1 sss_std_${region}.nc > sss_std_${region}.asc
  test -f ${RUN}_${modID}_2ds_${mode}${fhr}_prog_sss.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_prog_sss.nc

  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${FIXrtofs}/${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds_${mode}${fhr}_prog_u_velocity.nc u_velocity_std_${region}.nc
  $cdo_r outputf,%8.4f,1 u_velocity_std_${region}.nc > u_velocity_std_${region}.asc
  test -f ${RUN}_${modID}_2ds_${mode}${fhr}_prog_u_velocity.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_prog_u_velocity.nc

  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${FIXrtofs}/${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds_${mode}${fhr}_prog_v_velocity.nc v_velocity_std_${region}.nc
  $cdo_r outputf,%8.4f,1 v_velocity_std_${region}.nc > v_velocity_std_${region}.asc
  test -f ${RUN}_${modID}_2ds_${mode}${fhr}_prog_v_velocity.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_prog_v_velocity.nc

  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${FIXrtofs}/${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds_${mode}${fhr}_diag_ssh.nc ssh_std_${region}.nc
  $cdo_r outputf,%8.4f,1 ssh_std_${region}.nc > ssh_std_${region}.asc
  test -f ${RUN}_${modID}_2ds_${mode}${fhr}_diag_ssh.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_diag_ssh.nc

  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${FIXrtofs}/${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds_${mode}${fhr}_diag_u_barotropic_velocity.nc ubaro_std_${region}.nc
  $cdo_r outputf,%8.4f,1 ubaro_std_${region}.nc > ubaro_std_${region}.asc
  test -f ${RUN}_${modID}_2ds_${mode}${fhr}_diag_u_barotropic_velocity.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_diag_u_barotropic_velocity.nc

  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${FIXrtofs}/${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds_${mode}${fhr}_diag_v_barotropic_velocity.nc vbaro_std_${region}.nc
  $cdo_r outputf,%8.4f,1 vbaro_std_${region}.nc > vbaro_std_${region}.asc
  test -f ${RUN}_${modID}_2ds_${mode}${fhr}_diag_v_barotropic_velocity.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_diag_v_barotropic_velocity.nc
####
### Added for ice
if [ ${region} = 'alaska'  -o  ${region} = 'bering'  -o  ${region} = 'arctic' ]; then
  $cdo_r -remap,${FIXrtofs}/${RUN}_grid_${region}.des,${FIXrtofs}/${RUN}_${region}_weights.nc ${RUN}_${modID}_2ds_${mode}${fhr}_ice_ice_coverage.nc ice_coverage_std_${region}.nc
  $cdo_r outputf,%8.4f,1 ice_coverage_std_${region}.nc > ice_coverage_std_${region}.asc
  test -f ${RUN}_${modID}_2ds_${mode}${fhr}_ice_ice_coverage.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_ice_ice_coverage.nc

## Using different mask file for thickness as the source mask has too many missing values
## Create the weights file using:
##cdo_r -R genbil,rtofs_grid_arctic.des -setmisstoc,0 rtofs_glo*ice_thickness.nc rtofs_arctic_icthknss_weights.nc
  $cdo_r -L -R remap,${FIXrtofs}/${RUN}_grid_${region}.des,${FIXrtofs}/${RUN}_${region}_icthknss_weights.nc -setmisstoc,0 ${RUN}_${modID}_2ds_${mode}${fhr}_ice_ice_thickness.nc ice_thickness_std_${region}.nc
  $cdo_r outputf,%8.4f,1 ice_thickness_std_${region}.nc > ice_thickness_std_${region}.asc
  test -f ${RUN}_${modID}_2ds_${mode}${fhr}_ice_ice_thickness.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_ice_ice_thickness.nc
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

    export XLFUNIT_50=${regdir}/${var}_${fhr}_std_${region}.grb2

    $EXECrtofs/${RUN}_nc2grb2 < ${regdir}/infile_${var}_${region}  > nc2grb.ft06_${region} 2>> nc2grb.err_${region}
    # export err=$?; err_chk
    cat ${regdir}/${var}_${fhr}_std_${region}.grb2 >> ${RUN}_${modID}_${mode}_temp_${region}_std.grb2
    test -f ${regdir}/${var}_${fhr}_std_${region}.grb2 && rm ${regdir}/${var}_${fhr}_std_${region}.grb2

    if [ ${var} = 'vbaro' ]; then

      if [ ${fhr} -eq ${dhr} ]; then
        if [ ${RUN_MODE} = 'analysis' ]; then
        cp ${RUN}_${modID}_${mode}_temp_${region}_std.grb2 ${RUN}_${modID}.t${mycyc}z.${mode}${dhr}_${region}_std.grb2
        test -f ${RUN}_${modID}_${mode}_temp_${region}_std.grb2 && rm ${RUN}_${modID}_${mode}_temp_${region}_std.grb2
        echo $dhr from dhr loop >> ${region}.out
      fi
     fi

      if [ ${fhr} -eq ${nhr} ]; then
       if [ ${RUN_MODE} = 'forecast' ]; then
        cp ${RUN}_${modID}_${mode}_temp_${region}_std.grb2 ${RUN}_${modID}.t${mycyc}z.${mode}${fhr}_${region}_std.grb2
        test -f ${RUN}_${modID}_${mode}_temp_${region}_std.grb2 && rm ${RUN}_${modID}_${mode}_temp_${region}_std.grb2
      echo $nhr from nhr loop >> ${region}.out
      fi
     fi
    fi
  done
  fhr=$(expr $fhr + $intvl_hrly)
done

# More cleaning up and housekeeping 

test -f ${RUN}_${modID}_2ds_${mode}${fhr}_prog_sst.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_prog_sst.nc
test -f ${RUN}_${modID}_2ds_${mode}${fhr}_prog_sss.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_prog_sss.nc
test -f ${RUN}_${modID}_2ds_${mode}${fhr}_diag_ssh.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_diag_ssh.nc
test -f ${RUN}_${modID}_2ds_${mode}${fhr}_prog_u_velocity.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_prog_u_velocity.nc
test -f ${RUN}_${modID}_2ds_${mode}${fhr}_prog_v_velocity.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_prog_v_velocity.nc
test -f ${RUN}_${modID}_2ds_${mode}${fhr}_diag_u_barotropic.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_diag_u_barotropic.nc
test -f ${RUN}_${modID}_2ds_${mode}${fhr}_diag_v_barotropic.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_diag_v_barotropic.nc
test -f ${RUN}_${modID}_2ds_${mode}${fhr}_prog_layer_density.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_prog_layer_density.nc
test -f ${RUN}_${modID}_2ds_${mode}${fhr}_diag_ice_coverage.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_diag_ice_coverage.nc
test -f ${RUN}_${modID}_2ds_${mode}${fhr}_diag_ice_thickness.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_diag_ice_thickness.nc
test -f ${RUN}_${modID}_2ds_${mode}${fhr}_diag_mixed_layer_thickness.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_diag_mixed_layer_thickness.nc
test -f ${RUN}_${modID}_2ds_${mode}${fhr}_diag_surface_boundary_layer_thickness.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_diag_surface_boundary_layer_thickness.nc
test -f ${RUN}_${modID}_2ds_${mode}${fhr}_ice_ice_coverage.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_ice_ice_coverage.nc
test -f ${RUN}_${modID}_2ds_${mode}${fhr}_ice_ice_thickness.nc && rm ${RUN}_${modID}_2ds_${mode}${fhr}_ice_ice_thickness.nc

#done

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)
