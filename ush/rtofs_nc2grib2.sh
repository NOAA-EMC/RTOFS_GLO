#!/bin/ksh
set -xeu

echo "*** Started script $0 on hostname "`hostname`' at time '`date`

typeset -Z3 fhr
typeset -Z3 intvl_hrly

# Declare directories here
region=$1
regdir=${DATA}/${region}
mkdir -p $region

fhr=00
dhr=024

# Here hours of ouput are hardwired. For the forecast,
# output is hourly for 0-72 hrs and 3-hourly afterwards.

if [ ${RUN_MODE} = 'analysis' ]
then
   nhr=48
else
   nhr=72
   if [ ${fcstdays_before_thisstep} -eq 3 ]
   then
      fhr=72
      nhr=144
   fi
fi

echo $fhr $nhr

if [ ${fcstdays_before_thisstep} -eq 3 ]
then
   intvl_hrly=$intvl_3hrly
else
   intvl_hrly=$intvl_1hrly
fi

export year=`echo $PDY | cut -c1-4`
export mycyc=${mycyc:-00}
export month=`echo $PDY | cut -c5-6`
export day=`echo $PDY | cut -c7-8`


cd $regdir

test -f ${region}.out && rm ${region}.out
touch nc.out ${region}.out

# Set up for infiles here
xx=`grep -i "xsize" ${PARMrtofs}/grid_${region}.des | cut -f2 -d "=" `
yy=`grep -i "ysize" ${PARMrtofs}/grid_${region}.des | cut -f2 -d "=" `
x0=`grep -i "xfirst" ${PARMrtofs}/grid_${region}.des | cut -f2 -d "=" `
y0=`grep -i "yfirst" ${PARMrtofs}/grid_${region}.des | cut -f2 -d "=" `
xinc=`grep -i "xinc" ${PARMrtofs}/grid_${region}.des | cut -f2 -d "=" `
yinc=`grep -i "yinc" ${PARMrtofs}/grid_${region}.des | cut -f2 -d "=" `


while [ $fhr -le $nhr ]
do
  fhr=`expr $fhr + $intvl_hrly`

  #echo xx yy day year month fcsthr cyc table# param# x0 y0 xinc yinc
  echo $xx $yy  $day $year $month $fhr $mycyc 0 3 $x0 $y0 $xinc $yinc 0 > infile_sst_${region}
  echo $xx $yy  $day $year $month $fhr $mycyc 3 4 $x0 $y0 $xinc $yinc 0 > infile_sss_${region}
  echo $xx $yy  $day $year $month $fhr $mycyc 2 1 $x0 $y0 $xinc $yinc 0 > infile_u_velocity_${region}
  echo $xx $yy  $day $year $month $fhr $mycyc 3 1 $x0 $y0 $xinc $yinc 0 > infile_v_velocity_${region}
  echo $xx $yy  $day $year $month $fhr $mycyc 195 3 $x0 $y0 $xinc $yinc 0 > infile_ssh_${region}
  echo $xx $yy  $day $year $month $fhr $mycyc 194 1 $x0 $y0 $xinc $yinc 0 > infile_ubaro_${region}
  echo $xx $yy  $day $year $month $fhr $mycyc 195 1 $x0 $y0 $xinc $yinc 0 > infile_vbaro_${region}


  # Split the netCDF file into components
  if [ ${fcstdays_before_thisstep} -eq 3 ]
  then
    $cdo_r splitname $DATA/${RUN}_${modID}_2ds_${mode}${fhr}_3hrly_diag.nc ${RUN}_${modID}_2ds_${mode}${fhr}_diag_
    $cdo_r splitname $DATA/${RUN}_${modID}_2ds_${mode}${fhr}_3hrly_prog.nc ${RUN}_${modID}_2ds_${mode}${fhr}_prog_
  else
    $cdo_r splitname $DATA/${RUN}_${modID}_2ds_${mode}${fhr}_1hrly_diag.nc ${RUN}_${modID}_2ds_${mode}${fhr}_diag_
    $cdo_r splitname $DATA/${RUN}_${modID}_2ds_${mode}${fhr}_1hrly_prog.nc ${RUN}_${modID}_2ds_${mode}${fhr}_prog_
  fi

  # Some housekeeping
  touch rtofs_glo_${mode}_temp_${region}_std.grb2
  touch sst_std.nc sss_std.asc u_velocity_std.nc v_velocity_std.nc ssh_std.nc ubaro_std.asc vbaro_std.asc
  test -f sst_std.nc && rm sst_std.nc
  test -f sss_std.asc && rm sss_std.asc
  test -f u_velocity_std.nc && rm u_velocity_std.nc
  test -f v_velocity_std.nc && rm v_velocity_std.nc
  test -f ssh_std.nc && rm ssh_std.nc
  test -f ubaro_std.asc && rm ubaro_std.asc
  test -f vbaro_std.asc && rm vbaro_std.asc

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

  #************
  # Pack the variables in GRIB 
  for var in sst sss u_velocity v_velocity ssh ubaro vbaro
  do

    echo ${var}_std.asc var_std_${region}.asc
    test -f fort.50 && rm -f fort.50
    test -f fort.20 && rm -f fort.20
    ln -s ${regdir}/${var}_std_${region}.asc fort.20

    export XLFUNIT_50=${regdir}/${var}_${fhr}_std_${region}.grb2

    $EXECrtofs/${RUN}_nc2grib < ${regdir}/infile_${var}_${region}  > nc2grb.ft06_${region} 2>> nc2grb.err_${region}
    export err=$?; err_chk
    cat ${regdir}/${var}_${fhr}_std_${region}.grb2 >> rtofs_glo_${mode}_temp_${region}_std.grb2
    test -f ${regdir}/${var}_${fhr}_std_${region}.grb2 && rm ${regdir}/${var}_${fhr}_std_${region}.grb2

    if [ ${var} = 'vbaro' ]; then

      if [ ${fhr} -eq ${dhr} ]; then
        cp rtofs_glo_${mode}_temp_${region}_std.grb2 rtofs_glo.t${cycle}z.${mode}${dhr}_${region}_std.grb2 
        test -f rtofs_glo_${mode}_temp_${region}_std.grb2 && rm rtofs_glo_${mode}_temp_${region}_std.grb2
        echo $dhr from dhr loop >> ${region}.out
        if [ ${RUN_MODE} = 'forecast' ]; then
          dhr=048
        fi
      fi

      if [ ${fhr} -eq ${nhr} ]; then
        cp rtofs_glo_${mode}_temp_${region}_std.grb2 rtofs_glo.t${cycle}z.${mode}${ENDHOUR}_${region}_std.grb2 
        test -f rtofs_glo_${mode}_temp_${region}_std.grb2 && rm rtofs_glo_${mode}_temp_${region}_std.grb2
        echo $nhr from nhr loop >> ${region}.out
      fi

    fi
  done
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

#done

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
