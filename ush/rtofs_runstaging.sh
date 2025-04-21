#!/bin/sh
#########################################################################
# Usage: rtofs_runstaging.sh                                            #
#                                                                       #
# Description: Staging input files for analysis or forecast runs        #
#                                                                       #
# History:                                                              #
#    07-30-2010  Ilya Rivin                                             #
#########################################################################
set -xa

echo "*** Started script $0 on hostname "$(hostname)' at time '$(date)
export PS4='$SECONDS + '

cd $DATA
  
# --------------------------------------------------------------------------- #
# 1  Set parameters depending on run mode
if [ ${RUN_MODE} = "analysis" ]
then
  runmode='anal'
  runstep=${runmode}
  runname='ANALYSIS'
else
  runmode='fcst'
  runstep=${runmode}${stepnum}
  runname='FORECAST'
fi 


# --------------------------------------------------------------------------- #
# 1  Set up the start time and end time for the forecast
  sday=$($USHrtofs/rtofs_date_normal2hycom.sh $startdate)
  eday=$($USHrtofs/rtofs_date_normal2hycom.sh $enddate)
  echo "  $sday $eday false false  " > limits

#
# 2.a Get topography (depth, grid and mask)
  for fil in depth grid
  do
    for type in a b
    do
      if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.regional.${fil}.${type} ]
      then
        ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.regional.${fil}.${type} regional.${fil}.${type}
        echo "regional.${fil}.${type} copied" 
      else
        $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Topography files" \
          "${runmode}: NO FILE for regional.${fil}.${type}" 2
      fi
    done
  done
 # Get CICE reional file
  if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.regional.cice.r ]
  then
    ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.regional.cice.r regional.cice.r
    echo "regional.cice.r copied" 
  else
    $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Topography files" \
     "${runmode}: NO FILE for regional.cice.r" 2
  fi
 

# 2.b Get relax files

  for bunch in "xx=int yy=intf" "xx=tem yy=temp" "xx=sal yy=saln" "xx=ssh yy=ssh" \
               "xx=sss yy=sssrmx" "xx=rmu yy=rmu" 
  do
    eval $bunch
    for type in a b
    do
      if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.relax_${xx}.${type} ]
      then
        ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.relax_${xx}.${type} relax.${yy}.${type}
        echo "relax.${yy}.${type} copied" 
      else
        $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Relaxation File" \
          "${runmode}: NO FILE for relax.${yy}.${type}" 2
      fi
    done
  done
  Cb=cb_11_10mm
  for type in a b
  do
    if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.${Cb}.${type} ]
    then
      ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.${Cb}.${type} cb.${type}
    else
      $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Drag Relaxation File" \
        "${runmode}: NO FILE for cb.${type}" 2
    fi
  done

# 2.c Get fix files

  for fil in iso.sigma veldf2 veldf4 surtmp4
  do
    for type in a b
    do
      if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.${fil}.${type} ]
      then
        ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.${fil}.${type} ${fil}.${type} 
        echo "${fil}.${type} copied" 
       else
        $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Fix File" \
          "${runmode}: NO FILE for ${fil}.${type}" 2
      fi
    done
  done
  if [ ${DBL_THKDF} = 'YES' ]
  then
    fil=thkdf4_double
  else
    fil=thkdf4
  fi
  for type in a b
  do
    if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.${fil}.${type} ]
    then
      ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.${fil}.${type} thkdf4.${type} 
      echo "${fil}.${type} copied" 
     else
      $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Fix File" \
        "${runmode}: NO FILE for thkdf4.${type}" 2
    fi
  done
  # Get CICE fixed files
   
  for fil in 'cice.prec_lanl_12.r'  'cice.rhoa_ncar85-88_12.r'
  do
    if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.${fil} ]
    then
      ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.${fil} ${fil}
      echo "${fil} copied" 
     else
      $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Fix File" \
        "${runmode}: NO FILE for ${fil}" 2
    fi
  done


# 2.d Get fix forcing file

  #dbgz 201213 
  #for fil in 
  for fil in chl rivers offlux 
  do
    for type in a b
    do
      if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.forcing.${fil}.${type} ]
      then
        ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.forcing.${fil}.${type} forcing.${fil}.${type}
        echo "forcing.${fil}.${type} copied" 
       else
        $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Fix Forcing File" \
          "${runmode}: NO FILE for forcing.${fil}.${type}" 2
      fi
    done
  done

# 2.e Get drift files

  for type in a b
  do
    if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.tbaric.${type} ]
    then
       ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.tbaric.${type} tbaric.${type}
       echo "tbaric.${type} copied" 
    else
        $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Drift File" \
          "${runmode}: NO FILE for tbaric.${type}"
    fi
  done
  
# 2.f Get initialization files 
  for fil in blkdat patch archs ####  ports tracer tidalports_p tidalports_vel
  do
    mode=''
    if [ $fil = "blkdat" ] ; then mode=".${runmode}" ; fi
    if [ -s $PARMrtofs/${RUN}_${modID}.${inputgrid}${mode}.${fil}.input ]
    then
      ln -s -f $PARMrtofs/${RUN}_${modID}.${inputgrid}${mode}.${fil}.input ${fil}.input 
      echo "${fil}.input copied" 
    else
      $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Initialization File" \
        "${runmode}: NO FILE for ${fil}.input" 2
    fi
  done
    if [ -s $PARMrtofs/${RUN}_${modID}.${inputgrid}.${runmode}.ice_in ]
    then
      ln -s -f $PARMrtofs/${RUN}_${modID}.${inputgrid}.${runmode}.ice_in ice_in
      echo "ice_in copied" 
    else
      $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Initialization File" \
        "${runmode}: NO FILE for ice_in" 2
    fi

# 3. Get Forcing files

  for fil in airtmp glbrad lwdflx precip presur radflx shwflx surtmp vapmix wndewd wndnwd wndspd 
  do
    for type in a b
    do
      FORCEfile=${COMIN}/${RUN}_${modID}.${runstep}.t${mycyc}z.forcing.${fil}.${type}
      if [ -s $FORCEfile ]
      then
        ln -s -f $FORCEfile forcing.${fil}.${type}
        echo "forcing.${fil}.${type} copied" 
      else
        $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Fix Forcing File" \
          "${runstep}: NO FILE for forcing.${fil}.${type}" 2
      fi
    done
  done
  for fil in airtmp glbrad lwdflx vapmix wndewd wndnwd
  do
    for type in r B
    do
      FORCEfile=${COMIN}/${RUN}_${modID}.${runstep}.t${mycyc}z.cice.${fil}.${type}
      if [ -s $FORCEfile ]
      then
        ln -s -f $FORCEfile cice.${fil}.${type}
        echo "cice.${fil}.${type} copied" 
      else
        $USHrtofs/${RUN}_abort.sh "FATAL ERROR: $job Missing Fix Forcing File" \
          "${runstep}: NO FILE for cice.${fil}.${type}" 2
      fi
    done
  done

echo "*** Finished script $0 on hostname "$(hostname)' at time '$(date)
