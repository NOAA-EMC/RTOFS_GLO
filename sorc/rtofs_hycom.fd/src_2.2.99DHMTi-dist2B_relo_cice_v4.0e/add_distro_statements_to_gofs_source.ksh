#!/bin/ksh

# Rowley, Dec 2013;  Wallcraft, May 2017;

displain() {
cat << END
  #
  DISTRIBUTION STATEMENT B: Distribution authorized to U.S. Government
  agencies based upon the reasons of possible Premature Distribution
  and the possibility of containing Software Documentation as listed
  on Table 1 of DoD Instruction 5230.24, Distribution Statements on
  Technical Documents, of 23 August 2012. Other requests for this
  document shall be made to Dr. Ruth H. Preller, Superintendent,
  Oceanography Division, U.S. Naval Research Laboratory, DEPARTMENT
  OF THE NAVY, John C. Stennis Space Center, MS 39529-5004; (228)
  688-4670 (voice); ruth.preller@nrlssc.navy.mil (e-mail).
  #
END
}

dis_c() {
cat << END
/*
  #
  DISTRIBUTION STATEMENT B: Distribution authorized to U.S. Government
  agencies based upon the reasons of possible Premature Distribution
  and the possibility of containing Software Documentation as listed
  on Table 1 of DoD Instruction 5230.24, Distribution Statements on
  Technical Documents, of 23 August 2012. Other requests for this
  document shall be made to Dr. Ruth H. Preller, Superintendent,
  Oceanography Division, U.S. Naval Research Laboratory, DEPARTMENT
  OF THE NAVY, John C. Stennis Space Center, MS 39529-5004; (228)
  688-4670 (voice); ruth.preller@nrlssc.navy.mil (e-mail).
  #
*/
END
}

premake() {
  displain | sed 's/^ /#/' > .tmpfile
  cat $1 >> .tmpfile
  mv .tmpfile $1
}

prec() {
  dis_c   > .tmpfile
  cat $1 >> .tmpfile
  mv .tmpfile $1
}

prefortran() {
  displain | sed 's/^ /c/' > .tmpfile
  cat $1 >> .tmpfile
  mv .tmpfile $1
}

pref90() {
  displain | sed 's/^ /!/' > .tmpfile
  cat $1 >> .tmpfile
  mv .tmpfile $1
}

check_one() {
  var=`grep -l 'DISTRIBUTION' $1`
  if [[ $var = "" ]]; then
    return 1
  else
    return 0
  fi
}

check_missing() {
  missing=`find . -type f -exec grep -L 'DISTRIBUTION' {} \;`
  nmissing=`echo $missing | wc -w`
  return $nmissing
}

print "This script adds DOD distro statement:"
displain
print "to GOFS 3.1 source codes."

if [[ -e comp_ice.csh && -d bld && -d hycom ]]; then
  print "This appears to be a GOFS 3.1 source directory..."
else
  print "WARNING: This does not appear to be an GOFS 3.1 source directory..."
fi

var=N;
print "Continue to add the distribution statement to source code"
print -n "under this directory? (y/N):"; read var; print "";

if [[ $var != "y" && $var != "Y" ]]; then
  print "Quitting..."
  exit 0;
fi

for f in `find . -name \*.csh -print`; do
  check_one $f || (print $f; premake $f)
done

for f in `find . -name Macros.\* -print`; do
  check_one $f || (print $f; premake $f)
done

for f in `find . -name README\* -print`; do
  check_one $f || (print $f; premake $f)
done

for f in `find . -name \*_cice -print`; do
  check_one $f || (print $f; premake $f)
done

for f in `find . -name Makefile -print`; do
  check_one $f || (print $f; premake $f)
done

for f in `find . -name \*.c -print`; do
  check_one $f || (print $f; prec $f)
done

for f in `find . -name \*.[Ff]90 -print`; do
  check_one $f || (print $f; pref90 $f)
done

for f in `find . -name \*.[Ff] -print`; do
  check_one $f || (print $f; prefortran $f)
done

for f in `find . -name \*.h* -print`; do
  check_one $f || (print $f; prefortran $f)
done

check_missing
nmiss=$?
if [[ $nmiss -ne 0 ]]; then
  print "WARNING: $nmiss files appear not to have the statement"
  find . -type f -exec grep -L 'DISTRIBUTION' {} \;
  exit 1
fi

exit 0
