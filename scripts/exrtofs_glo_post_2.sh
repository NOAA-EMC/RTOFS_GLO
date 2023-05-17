#!/bin/ksh

###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_post_2.sh                                  #
# Script description:                                                         #
#                                                                             #
# Authors: Bhavani Rajan & Ilya Rivin  Org: NP23         Date: 2011-07-20     #
#                                                                             #
# Abstract: This is the post_grib for RTOFS_GLO                               #
#           Writes out 3hrly netcdf files for hrs 144-192                     #
#                                                                             #
# Sub-scripts called:                                                         # 
#                    rtofs_glo2d_3hrly.sh                                     #
#                                                                             # 
# Imported variables:                                                         #
#                    RUN                                                      #
#                    modID                                                    #
#                    PARMrtofs                                                #
#                    USHrtofs                                                 #
#                    DATA                                                     #
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

procstatus=0

typeset -Z3 fhr
typeset -Z3 fhr0
typeset -Z3 intvl_daily
typeset -Z3 intvl_3hrly
typeset -Z3 hr_daily
typeset -Z3 hr_2d_3hrly
typeset -Z3 ENDHOUR
typeset -Z3 firsthr
typeset -Z3 lasthr



# Define the end forecast hour here:

export fcstdays=${fcstdays:-1}
if [ ${RUN_MODE} = 'forecast' ]
then
  export startdate=${startdate:-${PDY}}
fi
  export enddate=$($NDATE $(expr $fcstdays \* 24 ) ${startdate}${mycyc} | cut -c1-8)
  export ENDHOUR=$(expr \( $fcstdays \+ ${fcstdays_before_thisstep} \) \* 24)

# define what functions to do (default to operational settings)
export running_realtime=${running_realtime:-NO}
export run_parallel=${run_parallel:-NO}
export intvl_3hrly=${intvl_3hrly:-3}
export intvl_daily=${intvl_daily:-24}
export no_procs=${NPROCS:-4} # no of processors used in poe

export surface_3hrly=${surface_3hrly:-YES}

echo $surface_3hrly

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
if [ ${RUN_MODE} = 'forecast' ]
then
  export mode=f
fi
export fhr0=$fhr
export hr_daily=$fhr0
export hr_2d_3hrly=$fhr0


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
     ln -s -f $COMOUT/${RUN}_${modID}.t${mycyc}z.n00.archs.a archv.a
     ln -s -f $COMOUT/${RUN}_${modID}.t${mycyc}z.n00.archs.b archv.b
     ln -s -f $COMOUT/${RUN}_${modID}.t${mycyc}z.n00.arche.b arche.b
     ln -s -f $COMOUT/${RUN}_${modID}.t${mycyc}z.n00.arche.a arche.a
  else
   if [ -s $COMOUT/${arfile_tplate}.a ]; then
    ln -s -f $COMOUT/${arfile_tplate}.a archv.a
    ln -s -f $COMOUT/${arfile_tplate}.b archv.b
    ln -s -f $COMOUT/${arefile_tplate}.a arche.a
    ln -s -f $COMOUT/${arefile_tplate}.b arche.b
   else
     if [ $fhr -eq 00 ]; then
# This is for n00 or n-24 nowcast 
     ln -s -f $COMOUT/${RUN}_${modID}.t${mycyc}z.n-24.archs.a archv.a
     ln -s -f $COMOUT/${RUN}_${modID}.t${mycyc}z.n-24.archs.b archv.b
   else
    echo Missing archs file $COMOUT/${arfile_tplate}. >>${RUN}_${modID}.t${mycyc}z.nav.log
    echo "NOTdone due to missing archs file" >>${RUN}_${modID}.t${mycyc}z.nav.log
    export err=1; err_chk
  fi
     if [ $fhr -eq 00 ]; then
# This is for n00 or n-24 nowcast
     ln -s -f $COMOUT/${RUN}_${modID}.t${mycyc}z.n-24.arche.a arche.a
     ln -s -f $COMOUT/${RUN}_${modID}.t${mycyc}z.n-24.arche.b arche.b
   else
    echo Missing archs file $COMOUT/${arefile_tplate}. >>${RUN}_${modID}.t${mycyc}z.nav.log
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
      echo Missing file $fn, will not be able to run >> ${RUN}_${modID}.t${mycyc}z.nav.log
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
  # 3 hourly for surface files

 # 1 hourly for surface files
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
          cp -f -p $cfile  $COMOUT/.
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
   fhr=$(expr $fhr + $intvl_3hrly)

done
      
  echo "done" >$COMOUT/${RUN}_${modID}.t${mycyc}z.nav.log
  msg='THE RTOFS_GLO_GRIB_POST JOB HAS ENDED NORMALLY.'
  postmsg "$msg"

#################################################
msg='THE RTOFS_GLO_GRI_POST JOB HAS ENDED NORMALLY.'
postmsg "$msg"

################## END OF SCRIPT #######################
