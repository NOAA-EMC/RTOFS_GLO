#!/bin/sh

###############################################################################
####  UNIX Script Documentation Block                                         #
#                                                                             #
# Script name:         exrtofs_glo_post.sh                                    #
# Script description:                                                         #
#                                                                             #
# Authors: Bhavani Rajan & Ilya Rivin  Org: NP23         Date: 2011-07-20     #
#                                                                             #
# Abstract: This is the post-processing script for RTOFS_GLO for Volume data  # 
#                                                                             #
# Sub-scripts called:                                                         # 
#                    rtofs_glo3z_daily.sh                                     #
#                    rtofs_glo3z_6hrly.sh                                     #
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

set -xa

export PS4='$SECONDS + '

cd $DATA

###
### NOTE: Move copying to forecast step
###

msg="RTOFS_GLO_POST JOB has begun on $(hostname) at $(date)"
postmsg "$msg"

typeset -Z3 fhr
typeset -Z3 fhr0
typeset -Z3 intvl_daily
typeset -Z3 intvl_6hrly
typeset -Z3 hr_daily
typeset -Z3 hr_3z_6hrly
typeset -Z3 ENDHOUR

# Define the end forecast hour here:

if [ ${RUN_MODE} = 'analysis' ]
then
  export fcstdays=${fcstdays:-2}
  export enddate=${analysis_end:-$PDY}
  export startdate=$($NDATE -$(expr $fcstdays \* 24 ) ${enddate}'00' | cut -c1-8)
  export ENDHOUR=$(expr $fcstdays \* 24)
fi
if [ ${RUN_MODE} = 'forecast' ]
then
  export fcstdays=${fcstdays:-1}
  export startdate=${startdate:-${PDY}}
  export enddate=$($NDATE $(expr $fcstdays \* 24 ) ${startdate}${mycyc} | cut -c1-8)
  export ENDHOUR=$(expr \( $fcstdays \+ ${fcstdays_before_thisstep} \) \* 24)
fi

# define what functions to do (default to operational settings)
export run_parallel=${run_parallel:-NO}
export volume_3z_daily=${volume_3z_daily:-YES}
export volume_3z_6hrly=${volume_3z_6hrly:-YES}
export intvl_daily=${intvl_daily:-24}
export intvl_6hrly=${intvl_6hrly:-6}
export no_procs=${NPROCS:-4} # no of processors used in poe


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


if [ ${RUN_MODE} = 'analysis' ]
then
  fhr=00
  export mode=n
  analhrs=$(expr $fcstdays \* 24) 
fi
if [ ${RUN_MODE} = 'forecast' ]
then
  fhr=$(expr ${fcstdays_before_thisstep} \* 24)
  export mode=f
fi
export fhr0=$fhr
fhr=$(expr $fhr + 6)

# srtarting hours
export hr_daily=$(expr $fhr0 \+ 24)
export hr_3z_6hrly=$(expr $fhr0 \+ 6)


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
    arfile_tplate=${RUN}_${modID}.t${mycyc}z.${mode}${fhr2}.archv
  fi
  if [ ${RUN_MODE} = 'analysis' ]
  then
    typeset -Z2 fhr3
    fhr3=$(expr $analhrs - $fhr2)
    if [ $fhr -eq $ENDHOUR ]; then
       arfile_tplate=${RUN}_${modID}.t${mycyc}z.${mode}00.archv
    else
       arfile_tplate=${RUN}_${modID}.t${mycyc}z.${mode}-${fhr3}.archv
    fi
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
      if [ $icnt -ge $icnt_max ]
      then
        echo Post timed out, arfile_tplate not available after $icnt_max iterations.
        echo "NOTdone" >${RUN}_${modID}.t${mycyc}z.nav.log
        export err=2; err_chk
      fi
    fi

  fi # end loop for run_parallel
  #*********************************
  # link current archive to the working directory
  rm -rf archv.a archv.b > /dev/null

  if [ -s $COMIN/${arfile_tplate}.a ]; then
    ln -s -f $COMIN/${arfile_tplate}.a archv.a
    ln -s -f $COMIN/${arfile_tplate}.b archv.b
  else
    echo Missing archv file $COMIN/${arfile_tplate}.
    echo "NOTdone due to missing archv file" >${RUN}_${modID}.t${mycyc}z.nav.log
    export err=1; err_chk  
  fi
  missing=no
  for fn in regional.depth.a regional.depth.b regional.grid.a regional.grid.b archv.a archv.b 
  do
    if [ ! -f $fn ]
    then
      missing=yes
      echo Missing file $fn, will not be able to run
    fi
  done
  if [ $missing = 'yes' ]
  then
    echo Cannot run due to missing files.
    echo "NOTdone" >${RUN}_${modID}.t${mycyc}z.nav.log
    export err=1; err_chk
    exit
  fi

#*********************************************************************
  if [ $fhr -eq $hr_daily ]
  then
    hr_daily=$(expr $hr_daily + $intvl_daily)
###
    if [ $volume_3z_daily = 'YES' ]
    then
      ${USHrtofs}/${RUN}_glo3z_daily.sh
      if [ $SENDCOM = 'YES' ]
      then
        for fld in 3zuio 3zvio 3ztio 3zsio
        do
          cfile=${RUN}_${modID}_3dz_${mode}${fhr}_daily_${fld}.nc
          cp -f -p $cfile $COMOUT/.
          if [ $SENDDBN = YES ]
          then
            $DBNROOT/bin/dbn_alert MODEL RTOFS_GLO_NETCDF $job $COMOUT/$cfile
          else
            msg="File $COMOUT/$cfile not posted to db_net."
            postmsg "$msg"
          fi
        done
      fi
    fi
  fi # End of Daily loop
  # Daily runs end here
#
  # 6 hourlies for volume files in 3 regions
  #
  if [ $fhr -eq $hr_3z_6hrly ]; then
    hr_3z_6hrly=$(expr $hr_3z_6hrly + $intvl_6hrly)
    if [ $volume_3z_6hrly = 'YES' ]
    then
      cmdtype='poe'
      test -f cmdfile && rm -f cmdfile cmdfile.*
      touch cmdfile
      for reg in hvr_US_east hvr_US_west hvr_alaska
      do
       echo "${USHrtofs}/${RUN}_glo3z_6hrly.sh $reg > $reg.$fhr.out 2>&1" >> cmdfile
      done
      chmod +x cmdfile
      #mpirun.lsf cfp cmdfile > mpirun_6hrly.out
      #mpirun cfp cmdfile > mpirun_6hrly.out
      mpiexec -np $NPROCS --cpu-bind verbose,core cfp cmdfile
      export err=$?; err_chk
## Copy all the regions to com
      for reg in hvr_US_east hvr_US_west hvr_alaska
      do
        if [ $SENDCOM = 'YES' ]
        then
          cfile=${RUN}_${modID}_3dz_${mode}${fhr}_6hrly_${reg}.nc
          cp -f -p $cfile  $COMOUT/.
          if [ $SENDDBN = YES ]
          then
            $DBNROOT/bin/dbn_alert MODEL RTOFS_GLO_NETCDF $job $COMOUT/$cfile
          else
            msg="File $COMOUT/$cfile not posted to db_net."
            postmsg "$msg"
          fi
        fi
    done # regional loop
    fi # 6hrly loop 
  fi # volume 3z loop

  fhr=$(expr $fhr + $intvl_6hrly) 
done

echo "done" >$COMOUT/${RUN}_${modID}.t${mycyc}z.nav.log

#################################################
msg='THE RTOFS_GLO_POST JOB HAS ENDED NORMALLY.'
postmsg "$msg"
################## END OF SCRIPT #######################
