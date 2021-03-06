#!/bin/sh

###############################################################################
######################  UNIX Script Documentation Block  ######################
#                                                                             #
# Script name:         exrtofs_glo_gzip.sh                                    #
# Script description:                                                         #
#                                                                             #
# Authors: Floyd Fayton Org: PMB         Date: 2013-04-24                     #
#                                                                             #
# Abstract: This is the gzip script for RTOFS_GLO                             #
#                                                                             #
# Imported variables:                                                         #
#                    RUN                                                      #
#                    modID                                                    #
#                    DATA                                                     #
#                    COMIN                                                    #
#                    COMOUT                                                   #
#                    CYC                                                      #
#                                                                             #
# Script history log:                                                         #
# 2019-12-19  DCS - updated for Dell-p3 and added COMIN/COMOUT                #
#                                                                             #
###############################################################################

set -xa

export PS4='$SECONDS + '

cd $DATA

###
### NOTE: GZIP files and alert_dbn
###

msg="$job JOB has begun on `hostname` at `date`"
postmsg "$jlogfile" "$msg"

filepre=${RUN}_${modID}.t00z

> $DATA/poescript

case $CYC in 
    00)
# sort files from largest to smallest for cfp efficiency
	for file in `echo ${COMIN}/${filepre}.n*.restart.a;  echo ${COMIN}/${filepre}.n*.restart_cice;  echo ${COMIN}/${filepre}.n-{06..24..6}.archv.a;  echo ${COMIN}/${filepre}.n00.archv.a;  echo ${COMIN}/${filepre}.n00.arche.a;  echo ${COMIN}/${filepre}.n-{01..24}.arche.a;  echo ${COMIN}/${filepre}.n00.archs.a;  echo ${COMIN}/${filepre}.n-{01..24}.archs.a`
	do
	    if [ -s $file ]; then
		filename=`basename $file`
		echo "tar zcf ${COMOUT:?}/$filename.tgz -C ${COMIN} $filename " >> $DATA/poescript
	    else
		echo " $file missing, exiting now."
		export err=1;err_chk
	    fi
	done
	chmod 775 $DATA/poescript
	mpirun cfp $DATA/poescript
	export err=$?; err_chk
# send smaller files earlier
	for tgzfile in `echo ${COMOUT}/${filepre}.n00.archs.a.tgz;  echo ${COMOUT}/${filepre}.n-{01..24}.archs.a.tgz;  echo ${COMOUT}/${filepre}.n00.archv.a.tgz;  echo ${COMOUT}/${filepre}.n-{06..24..6}.archv.a.tgz;  echo ${COMOUT}/${filepre}.n00.restart_cice.tgz;  echo ${COMOUT}/${filepre}.n00.restart.a.tgz`  
	do
	    if [ -s $tgzfile ]; then
		if [ $SENDDBN = YES ] ; then
		    if [ `basename $tgzfile` == 'rtofs_glo.t00z.n00.restart.a.tgz' ]; then
			${DBNROOT}/bin/dbn_alert MODEL RTOFS_GLO_NRESTARTA_TGZ $job $tgzfile
			${DBNROOT}/bin/dbn_alert MODEL RTOFS_GLO_NRESTARTB $job ${tgzfile%.a.*}.b
		    elif [ `basename $tgzfile` == 'rtofs_glo.t00z.n00.restart_cice.tgz' ]; then
			${DBNROOT}/bin/dbn_alert MODEL RTOFS_GLO_NRESTARTCICE_TGZ $job $tgzfile
		    elif  echo $tgzfile | grep -q archv ; then
			${DBNROOT}/bin/dbn_alert MODEL RTOFS_GLO_NARCHVA_TGZ $job $tgzfile
			${DBNROOT}/bin/dbn_alert MODEL RTOFS_GLO_NARCHVB $job ${tgzfile%.a.*}.b
		    else
			${DBNROOT}/bin/dbn_alert MODEL RTOFS_GLO_NARCHSA_TGZ $job $tgzfile
			${DBNROOT}/bin/dbn_alert MODEL RTOFS_GLO_NARCHSB $job ${tgzfile%.a.*}.b
		    fi    
		fi
	    else
		echo " $tgzfile was not produced, exiting now."
		export err=1;err_chk
	    fi
	done
	;;
    06)
# sort files from largest to smallest for cfp efficiency
	for file in `echo  ${COMIN}/${filepre}.f{06..96..6}.archv.a;  echo  ${COMIN}/${filepre}.f{01..96}.archs.a; echo  ${COMIN}/${filepre}.f{00..96}.arche.a `
	do
	    if [ -s $file ]; then
		filename=`basename $file`
		echo "tar zcf ${COMOUT:?}/$filename.tgz -C ${COMIN} $filename " >> $DATA/poescript
	    else
		echo " $file missing, exiting now."
		export err=1;err_chk
	    fi
	done
	chmod 775 $DATA/poescript
	mpirun cfp $DATA/poescript
	export err=$?; err_chk
# send smaller files earlier
	for tgzfile in `echo ${COMOUT}/${filepre}.f{01..96}.archs.a.tgz;  echo ${COMOUT}/${filepre}.f{06..96..6}.archv.a.tgz`
	do
	    if [ -s $tgzfile ]; then
		if [ $SENDDBN = YES ] ; then
		    if  echo $tgzfile | grep -q archv ; then
			${DBNROOT}/bin/dbn_alert MODEL RTOFS_GLO_FARCHVA_TGZ $job $tgzfile
			${DBNROOT}/bin/dbn_alert MODEL RTOFS_GLO_FARCHVB $job ${tgzfile%.a.*}.b
		    else
			${DBNROOT}/bin/dbn_alert MODEL RTOFS_GLO_FARCHSA_TGZ $job $tgzfile
			${DBNROOT}/bin/dbn_alert MODEL RTOFS_GLO_FARCHSB $job ${tgzfile%.a.*}.b
		    fi
		fi
	    else
		echo " $tgzfile was not produced, exiting now."
		export err=1;err_chk
	    fi
	done
	;;
    12)
# sort files from largest to smallest for cfp efficiency
	for file in `echo  ${COMIN}/${filepre}.f{102..192..6}.archv.a;  echo ${COMIN}/${filepre}.f{97..192}.archs.a; echo ${COMIN}/${filepre}.f{97..192}.arche.a   `
	do
	    if [ -s $file ]; then
		filename=`basename $file`
		echo "tar zcf ${COMOUT:?}/$filename.tgz -C ${COMIN} $filename " >> $DATA/poescript
	    else
		echo " $file missing, exiting now."
		export err=1;err_chk
	    fi
	done
	chmod 775 $DATA/poescript
	mpirun cfp $DATA/poescript
	export err=$?; err_chk
# send smaller files earlier
	for tgzfile in `echo ${COMOUT}/${filepre}.f{97..192}.archs.a.tgz;  echo ${COMOUT}/${filepre}.f{102..196..6}.archv.a.tgz`
	do
	    if [ -s $tgzfile ]; then
		if [ $SENDDBN = YES ] ; then
		    if  echo $tgzfile | grep -q archv ; then
			${DBNROOT}/bin/dbn_alert MODEL RTOFS_GLO_FARCHVA_TGZ $job $tgzfile
			${DBNROOT}/bin/dbn_alert MODEL RTOFS_GLO_FARCHVB $job ${tgzfile%.a.*}.b
		    else
			${DBNROOT}/bin/dbn_alert MODEL RTOFS_GLO_FARCHSA_TGZ $job $tgzfile
			${DBNROOT}/bin/dbn_alert MODEL RTOFS_GLO_FARCHSB $job ${tgzfile%.a.*}.b
		    fi
		fi
	    else
		echo " $tgzfile was not produced, exiting now."
		export err=1;err_chk
	    fi
	done
	;;
    *)
  echo "Cycle not recognized RTOFS GZIP JOB, NO GZIPPED FILES ALERTED"
;;
esac

#################################################
msg='THE $job JOB HAS ENDED NORMALLY.'
postmsg "$jlogfile" "$msg"
################## END OF SCRIPT #######################
