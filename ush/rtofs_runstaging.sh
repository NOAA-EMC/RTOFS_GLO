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

echo "*** Started script $0 on hostname "`hostname`' at time '`date`
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
  sday=`$utilscript/date_normal2hycom.sh $startdate$mycyc`
  eday=`$utilscript/date_normal2hycom.sh $enddate$mycyc`
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
        $USHrtofs/${RUN}_abort.sh "Missing Topography files" \
          "ABNORMAL EXIT ${runmode}: NO FILE for regional.${fil}.${type}" 2
      fi
    done
  done

# 2.b Get relax files

  for bunch in "xx=int yy=intf" "xx=tem yy=temp" "xx=sal yy=saln" "xx=ssh yy=ssh"
  do
    eval $bunch
    for type in a b
    do
      if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.relax_${xx}.${type} ]
      then
        ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.relax_${xx}.${type} relax.${yy}.${type}
        echo "relax.${yy}.${type} copied" 
      else
        $USHrtofs/${RUN}_abort.sh "Missing Relaxation File" \
          "ABNORMAL EXIT ${runmode}: NO FILE for relax.${yy}.${type}" 2
      fi
    done
  done

# 2.c Get fix files

  for fil in iso.sigma thkdf4 veldf2 veldf4
  do
    for type in a b
    do
      if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.${fil}.${type} ]
      then
        ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.${fil}.${type} ${fil}.${type} 
        echo "${fil}.${type} copied" 
       else
        $USHrtofs/${RUN}_abort.sh "Missing Fix File" \
          "ABNORMAL EXIT ${runmode}: NO FILE for ${fil}.${type}" 2
      fi
    done
  done


# 2.d Get fix forcing file

  #dbgz 201213 
  #for fil in 
  for fil in kpar offlux rivers
  do
    for type in a b
    do
      if [ -s $FIXrtofs/${RUN}_${modID}.${inputgrid}.forcing.${fil}.${type} ]
      then
        ln -s -f $FIXrtofs/${RUN}_${modID}.${inputgrid}.forcing.${fil}.${type} forcing.${fil}.${type}
        echo "forcing.${fil}.${type} copied" 
       else
        $USHrtofs/${RUN}_abort.sh "Missing Fix Forcing File" \
          "ABNORMAL EXIT ${runmode}: NO FILE for forcing.${fil}.${type}" 2
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
        $USHrtofs/${RUN}_abort.sh "Missing Drift File" \
          "ABNORMAL EXIT ${runmode}: NO FILE for tbaric.${type}"
    fi
  done
  
# 2.f Get initialization files 
  for fil in blkdat ports tracer patch  #### tidalports_p tidalports_vel
  do
    mode=''
    if [ $fil = "blkdat" ] ; then mode=".${runmode}" ; fi
    if [ -s $PARMrtofs/${RUN}_${modID}.${inputgrid}${mode}.${fil}.input ]
    then
      ln -s -f $PARMrtofs/${RUN}_${modID}.${inputgrid}${mode}.${fil}.input ${fil}.input 
      echo "${fil}.input copied" 
    else
      $USHrtofs/${RUN}_abort.sh "Missing Initialization File" \
        "ABNORMAL EXIT ${runmode}: NO FILE for ${fil}.input" 2
    fi
  done

# 3. Get Forcing files

  for fil in airtmp precip presur radflx shwflx surtmp tauewd taunwd vapmix wndspd
  do
    for type in a b
    do
      FORCEfile=${COMIN}/${RUN}_${modID}.${runstep}.t${mycyc}z.forcing.${fil}.${type}
      if [ -s $FORCEfile ]
      then
        ln -s -f $FORCEfile forcing.${fil}.${type}
        echo "forcing.${fil}.${type} copied" 
      else
        $USHrtofs/${RUN}_abort.sh "Missing Fix Forcing File" \
          "ABNORMAL EXIT ${runstep}: NO FILE for forcing.${fil}.${type}" 2
      fi
    done
  done

echo "*** Finished script $0 on hostname "`hostname`' at time '`date`
