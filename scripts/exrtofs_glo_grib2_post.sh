#!/bin/ksh

###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_grib2_post.sh                              #
# Script description:                                                         #
#                                                                             #
# Authors: Bhavani Rajan & Ilya Rivin  Org: NP23         Date: 2011-07-20     #
#                                                                             #
# Abstract: This is the post_grib for RTOFS_GLO                               #
#           Writes out hourly netcdf files for nowcast and 1-72 hr forecast   #
#           and 3-hrly from 72-144 hrs Surface forecast.                      #
#           Also packs the netcdf files in GRIB2 format for 11 Regions        #
#           These are surface only (2d) diag and prog and ice files           #
#           Additonally puts out netcdf files for OPC                         #
#                                                                             #
# Sub-scripts called:                                                         # 
#                    rtofs_glo2d_3hrly.sh                                     #
#                    rtofs_glo2d_1hrly.sh                                     #
#                    create_regions_mpmd_weights.sh                           #
#                                                                             # 
# Imported variables:                                                         #
#                    RUN                                                      #
#                    modID                                                    #
#                    PARMrtofs                                                #
#                    USHrtofs                                                 #
#                    DATA                                                     #
#                    COMIN                                                    #
#                    COMOUT                                                   #
#                    RUN_MODE                                                 #  
#                    SENDCOM                                                  #
#                    PDY                                                      #  
#                    mycyc                                                    #
#                    fcstdays                                                 #
#                    inputgrid                                                #
#                                                                             #
#                                                                             #
# Script history log:                                                         #
# XXXX-XX-XXX  Joe Dow                                                        #
#                                                                             #
###############################################################################

ALERT_MAYBE()
# alert using subtype $1
# if file $2 is on "double_alerts" list, alert only if it was not alerted before
{
	      if grep $2 ${PARMrtofs}/rtofs_glo.double_alerts ; then
		  if [ -f $COMOUT/$2.alerted ] ; then
		      rm $COMOUT/$2.alerted
		  else
		      > $COMOUT/$2.alerted
		      $DBNROOT/bin/dbn_alert MODEL $1 $job $COMOUT/$2
		  fi
	      else
		  $DBNROOT/bin/dbn_alert MODEL $1 $job $COMOUT/$2
	      fi
}

set -xa

export PS4='$SECONDS + '

cd $DATA

###
### NOTE: Move copying to forecast step
###

msg="RTOFS_GLO_GRIB_POST JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

typeset -Z3 fhr
typeset -Z3 fhr0
typeset -Z3 intvl_daily
typeset -Z3 intvl_3hrly
typeset -Z3 intvl_1hrly
typeset -Z3 hr_daily
typeset -Z3 hr_2d_1hrly
typeset -Z3 hr_2d_3hrly
typeset -Z3 ENDHOUR
typeset -Z3 firsthr
typeset -Z3 lasthr



# Define the end forecast hour here:

export fcstdays=${fcstdays:-1}
if [ ${RUN_MODE} = 'analysis' ]
then
  export startdate=${startdate:-${PDYm1}}
fi
if [ ${RUN_MODE} = 'forecast' ]
then
  export startdate=${startdate:-${PDY}}
fi
  export enddate=$($NDATE $(expr $fcstdays \* 24 ) ${startdate}${mycyc} | cut -c1-8)
  export ENDHOUR=$(expr \( $fcstdays \+ ${fcstdays_before_thisstep} \) \* 24)

# define what functions to do (default to operational settings)
export run_parallel=${run_parallel:-NO}
export grib_1hrly=${grib_1hrly:-YES}
export for_opc=${for_opc:-YES}
export intvl_1hrly=${intvl_1hrly:-1}
export intvl_3hrly=${intvl_3hrly:-3}
export intvl_daily=${intvl_daily:-24}
export no_procs=${NPROCS:-4} # no of processors used in poe

if [ ${RUN_MODE} = 'analysis' ]
then
  export surface_1hrly=${surface_1hrly:-YES}
  export surface_3hrly=${surface_3hrly:-NO}
  export opc_script=rtofs_surface_hcasts.sh
fi
if [ ${RUN_MODE} = 'forecast' ]
then
  if [ ${RUN_MODE} = 'forecast' ] && [ ${fcstdays_before_thisstep} -ge 3 ]
  then
     export surface_3hrly=${surface_3hrly:-YES}
     export surface_1hrly=${surface_1hrly:-NO}
     export opc_script=rtofs_surface_day4-5.sh
  else
     export surface_3hrly=${surface_3hrly:-NO}
     export surface_1hrly=${surface_1hrly:-YES}
     export opc_script=rtofs_surface_day1-3.sh
  fi
fi

echo $surface_1hrly $surface_3hrly

# Waiting time (in 10 sec)
icnt_max=180

# Define the Input files:
export DEPTHFILEa=${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.depth.a
export DEPTHFILEb=${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.depth.b
export GRIDFILEa=${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.grid.a
export GRIDFILEb=${FIXrtofs}/${RUN}_${modID}.${inputgrid}.regional.grid.b
                          
# Copy in the Fix files:
cp -f -p $GRIDFILEa  ${DATA}/regional.grid.a
cp -f -p $GRIDFILEb  ${DATA}/regional.grid.b
cp -f -p $DEPTHFILEa ${DATA}/regional.depth.a
cp -f -p $DEPTHFILEb ${DATA}/regional.depth.b

# Copy in the Parm Files:
cp -f -p ${PARMrtofs}/${RUN}_${modID}.${inputgrid}.archv2ncdf2d.in ${DATA}/archv2ncdf2d.in

fhr=$(expr ${fcstdays_before_thisstep} \* 24)
if [ ${RUN_MODE} = 'analysis' ]
then
  export mode=n
  analhrs=$(expr $analdays \* 24) 
fi
if [ ${RUN_MODE} = 'forecast' ]
then
  export mode=f
fi
export fhr0=$fhr
export hr_daily=$fhr0
export hr_2d_3hrly=$fhr0
export hr_2d_1hrly=$fhr0


echo fhr $fhr ENDHOUR $ENDHOUR
# Output NC header information for surface AND volume files

export CDF_TITLE='HYCOM ATLb2.00'
export CDF_INST="National Centers for Environmental Prediction"

while [ $fhr -le $ENDHOUR ]
do
  # Some reverse engineering
  if [ $fhr -le 100 ] 
  then
    typeset -Z2 fhr2
    fhr2=$fhr
  else
    typeset -Z3 fhr2
    fhr2=$fhr
  fi
  if [ ${RUN_MODE} = 'forecast' ]
  then
    arfile_tplate=${RUN}_${modID}.t${mycyc}z.${mode}${fhr2}.archs
    arefile_tplate=${RUN}_${modID}.t${mycyc}z.${mode}${fhr2}.arche
  fi
  if [ ${RUN_MODE} = 'analysis' ]
  then
    typeset -Z2 fhr3
    fhr3=$(expr $analhrs - $fhr2)
    if [ $fhr3 -eq -0 ]
    then
      chr=00
    else
      chr='-'${fhr3}
    fi
    arfile_tplate=${RUN}_${modID}.t${mycyc}z.${mode}${chr}.archs
    arefile_tplate=${RUN}_${modID}.t${mycyc}z.${mode}${chr}.arche
  fi
#*********************************
  if [ $run_parallel = 'YES' ]; then
    # wait for the new forecast output to be available:
    icnt=1
    if [ ! -f ${arfile_tplate}.a ]; then
      break
      print Sleeping $icnt times while waiting for ${arfile_tplate}.a
      sleep 10
      icnt=$((icnt + 1))
      if [ $icnt -ge $icnt_max ]; then
       echo Post timed out arfile_tplate unavailable after $icnt_max iterations
        echo "NOTdone" >${RUN}_${modID}.t${mycyc}z.nav.log
        export err=2; err_chk
      fi
    fi
  fi # end loop for run_parallel
#*********************************
# link current archive to the working directory
  rm -rf archv.a archv.b > /dev/null

  if [ $fhr -eq 00 -a ${RUN_MODE} = 'forecast' ] ;then
# n00:48hr  is same as f00 forecaast
     ln -s -f $COMIN/${RUN}_${modID}.t${mycyc}z.n00.archs.a archv.a
     ln -s -f $COMIN/${RUN}_${modID}.t${mycyc}z.n00.archs.b archv.b
     ln -s -f $COMIN/${RUN}_${modID}.t${mycyc}z.n00.arche.b arche.b
     ln -s -f $COMIN/${RUN}_${modID}.t${mycyc}z.n00.arche.a arche.a
  else
   if [ -s $COMIN/${arfile_tplate}.a ]; then
    ln -s -f $COMIN/${arfile_tplate}.a archv.a
    ln -s -f $COMIN/${arfile_tplate}.b archv.b
    ln -s -f $COMIN/${arefile_tplate}.a arche.a
    ln -s -f $COMIN/${arefile_tplate}.b arche.b
   else
     if [ $fhr -eq 00 ]; then
# This is for n00 or n-24 nowcast 
     ln -s -f $COMIN/${RUN}_${modID}.t${mycyc}z.n-24.archs.a archv.a
     ln -s -f $COMIN/${RUN}_${modID}.t${mycyc}z.n-24.archs.b archv.b
   else
    echo "Missing archs file $COMIN/${arfile_tplate}." >>${RUN}_${modID}.t${mycyc}z.nav.log
    echo "NOTdone due to missing archs file" >>${RUN}_${modID}.t${mycyc}z.nav.log
    export err=1; err_chk
  fi
     if [ $fhr -eq 00 ]; then
# This is for n00 or n-24 nowcast 
     ln -s -f $COMIN/${RUN}_${modID}.t${mycyc}z.n-24.arche.a arche.a
     ln -s -f $COMIN/${RUN}_${modID}.t${mycyc}z.n-24.arche.b arche.b
   else
    echo "Missing archs file $COMIN/${arefile_tplate}." >>${RUN}_${modID}.t${mycyc}z.nav.log
    echo "NOTdone due to missing arche file" >>${RUN}_${modID}.t${mycyc}z.nav.log
    export err=1; err_chk
  fi
 fi
fi
  missing=no
  for fn in regional.depth.a regional.depth.b regional.grid.a regional.grid.b archv.a archv.b archv2ncdf2d.in
  do
    if [ ! -f $fn ]
    then
      missing=yes
      echo "Missing file $fn, will not be able to run" >> ${RUN}_${modID}.t${mycyc}z.nav.log
    fi
  done
  if [ $missing = 'yes' ]
  then
    echo Cannot run due to missing files.
    echo "NOTdone" >>${RUN}_${modID}.t${mycyc}z.nav.log
    export err=1; err_chk
    exit
  fi

  # Now for the hourly runs: 1 hourly and 3 hourly and daily for the surface files  
  #
  # 1 hourly for surface files
  if [ $fhr -eq $hr_2d_1hrly ]; then
    hr_2d_1hrly=$(expr $hr_2d_1hrly + $intvl_1hrly)
    if [ $surface_1hrly = 'YES' ]
    then 
      ksh ${USHrtofs}/${RUN}_glo2d.sh
      ksh ${USHrtofs}/${RUN}_glo2d_ice.sh
      if [ $SENDCOM = 'YES' ]
      then
        for ftype in diag prog ice
        do
          cfile=${RUN}_${modID}_2ds_${mode}${fhr}_${ftype}.nc
          if [ -x cpfs ]   # rc=1 means cpfs not found
          then
            cp -f -p $cfile  $COMOUT
          else
            cpfs $cfile  $COMOUT
          fi
          if [ $SENDDBN = 'YES' ]
          then
#	      $DBNROOT/bin/dbn_alert MODEL RTOFS_GLO_NETCDF $job $COMOUT/$cfile
	      ALERT_MAYBE RTOFS_GLO_NETCDF  $cfile
          else
            msg="File $COMOUT/$cfile not posted to db_net."
            postmsg "$msg"
          fi
        done
      fi
    fi # end of sfc loop
 fi

 # 3 hourly for surface files
  if [ $fhr -eq $hr_2d_3hrly ]; then
    hr_2d_3hrly=$(expr $hr_2d_3hrly + $intvl_3hrly)
    if [ $surface_3hrly = 'YES' ]
    then
      ksh ${USHrtofs}/${RUN}_glo2d.sh
      ksh ${USHrtofs}/${RUN}_glo2d_ice.sh
      if [ $SENDCOM = 'YES' ]
      then
        for ftype in diag prog ice
        do
          cfile=${RUN}_${modID}_2ds_${mode}${fhr}_${ftype}.nc
          if [ -x cpfs ]   # rc=1 means cpfs not found
          then
            cp -f -p $cfile  $COMOUT
          else
            cpfs $cfile  $COMOUT
          fi
          if [ $SENDDBN = 'YES' ]
          then
#              $DBNROOT/bin/dbn_alert MODEL RTOFS_GLO_NETCDF $job $COMOUT/$cfile
	      ALERT_MAYBE RTOFS_GLO_NETCDF  $cfile
          else
            msg="File $COMOUT/$cfile not posted to db_net."
            postmsg "$msg"
          fi
        done
      fi
     fi # end of sfc loop
 fi
if [ ${RUN_MODE} = 'forecast' ] && [ ${fcstdays_before_thisstep} -ge 3 ]
then
   fhr=$(expr $fhr + $intvl_3hrly)
else
   fhr=$(expr $fhr + $intvl_1hrly)
fi

done
## For OPC
## For analysis
   if [ ${RUN_MODE} = 'analysis' ] && [ ${ENDHOUR} -eq 24 ]; then
    if [ $for_opc = 'YES' ]
     then
      echo ${USHrtofs}/${opc_script}
      sh ${USHrtofs}/${opc_script}
     if [ $SENDCOM = 'YES' ]
     then
	 for cfile in $(ls -C1 ${DATA_opc}/gr*gz)
	 do
	     cfile_new=${cfile/grtofs/rtofs_glo}
	     mv $cfile $cfile_new
             if [ -x cpfs ]   # rc=1 means cpfs not found
             then
               cp -f -p $cfile_new  $COMOUT
             else
               cpfs $cfile_new  $COMOUT
             fi
	     cname=$(basename $cfile_new)
             if [ $SENDDBN = 'YES' ]
             then
		 $DBNROOT/bin/dbn_alert MODEL RTOFS_GLO_NETCDFGZ $job $COMOUT/$cname
             else
		 msg="File $COMOUT/$cname not posted to db_net."
		 postmsg "$msg"
             fi
         done
      fi ## SENDCOM
    fi ## for_opc
    fi ## analysis


   if [ ${RUN_MODE} = 'forecast' ] && [ ${ENDHOUR} -eq 72 -o  ${ENDHOUR} -eq 144 ]; then
    if [ $for_opc = 'YES' ]
     then
      echo ${USHrtofs}/${opc_script}
      sh ${USHrtofs}/${opc_script}
     if [ $SENDCOM = 'YES' ]
     then
	 for cfile in $(ls -C1 ${DATA_opc}/gr*gz)
	 do
	     cfile_new=${cfile/grtofs/rtofs_glo}
	     mv $cfile $cfile_new
             if [ -x cpfs ]   # rc=1 means cpfs not found
             then
               cp -f -p $cfile_new  $COMOUT
             else
               cpfs $cfile_new  $COMOUT
             fi
	     cname=$(basename $cfile_new)
             if [ $SENDDBN = 'YES' ]
             then
		 $DBNROOT/bin/dbn_alert MODEL RTOFS_GLO_NETCDFGZ $job $COMOUT/$cname
             else
		 msg="File $COMOUT/$cname not posted to db_net."
		 postmsg "$msg"
             fi
         done
      fi ## SENDCOM
    fi ## for_opc
    fi ## forecast



# If you want to pack to grib then
    if [ $grib_1hrly = 'YES' ]
    then
      ksh ${USHrtofs}/${RUN}_create_regions_mpmd_weights.sh
    fi
if [ $SENDCOM = 'YES' ]
        then
# Copy them to grib output dir
## Adds the GRIB Header file to the GRIB2 files
    for ftype in alaska arctic bering guam gulf_alaska honolulu hudson_baffin samoa trop_paci_lowres west_atl west_conus
    do
      for cfile in $(ls -C1 $DATA/$ftype/${RUN}_${modID}.t${mycyc}z.${mode}*_${ftype}_std.grb2)
      do
          cname=$(basename $cfile)
          if [ -x cpfs ]   # rc=1 means cpfs not found
          then
            cp -f -p $cfile  ${COMOUT}/.
          else
            cpfs $cfile  ${COMOUT}/.
          fi
          file=$(echo $cfile |awk -F/ '{print $5}')
          fhour=$(echo $cname | cut -c17-19)

          ####################################
          # Processing GRIB2 RTOFS for AWIPS
          ####################################
          pgm=tocgrib2
          export pgm;. prep_step
          startmsg

          export FORT11=$ftype/${RUN}_${modID}.t${mycyc}z.${mode}${fhour}_${ftype}_std.grb2
          export FORT31=" "
          export FORT51=grib2_${RUN}_${modID}.t${mycyc}z.${mode}${fhour}_${ftype}_std
          $TOCGRIB2 < $PARMrtofs/grib2_rtofs_glo_${mode}${fhour}_${ftype}_std

          err=$?;export err ;err_chk
          echo " error from tocgrib2=",$err

          if [ $SENDCOM = "YES" ] ; then
          ##############################
          # Post Files to COMOUTwmo
          ##############################

             cp  grib2_${RUN}_${modID}.t${mycyc}z.${mode}${fhour}_${ftype}_std  $COMOUTwmo/grib2_${RUN}_${modID}.t${mycyc}z.${mode}${fhour}_${ftype}_std

          fi

          if [ $SENDDBN = 'YES' ]
          then
            $DBNROOT/bin/dbn_alert MODEL RTOFS_GLO_REG_GB2 $job $COMOUT/$cname

            ##########################
            # Distribute Data to NCF
            #########################
            #
            #    Distribute Data to TOC (AWIPS)
            #
            if [ $SENDDBN_NTC = YES ]; then
                $DBNROOT/bin/dbn_alert NTC_LOW $NET $job  $COMOUTwmo/grib2_${RUN}_${modID}.t${mycyc}z.${mode}${fhour}_${ftype}_std
            echo " "
            fi
          else
            msg="File $COMOUT/$cname not posted to db_net."
            postmsg "$msg"
          fi
       done
     done
fi

echo "done" >$COMOUT/${RUN}_${modID}.t${mycyc}z.nav.log

#################################################
msg='THE RTOFS_GLO_GRIB_POST JOB HAS ENDED NORMALLY.'
postmsg "$msg"
################## END OF SCRIPT #######################
