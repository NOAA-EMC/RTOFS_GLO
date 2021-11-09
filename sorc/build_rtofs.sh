#!/bin/sh
# top level script to build / install / clean RTOFS binaries

if [ $# -lt 1 ] 
then
  whattodo=compile
else
  whattodo=$1
fi

case $whattodo in
  compile)
    echo
    echo Building rtofs_code.fd
    echo
    cd ./rtofs_code.fd;./build_code.sh;cd ..

    echo
    echo Building rtofs_hycom.fd
    echo
    cd ./rtofs_hycom.fd;./build_hycom.sh esmf ;cd ..
    cd ./rtofs_hycom.fd;./build_hycom.sh;cd ..

    echo
    echo Building rtofs_ncoda.fd
    echo
    cd ./rtofs_ncoda.fd;./build_ncoda.sh;cd ..
    ;;

  install)
    echo
    echo Running option install
    echo
    cd rtofs_code.fd;make install;cd ..
    cd rtofs_hycom.fd;./build_hycom.sh install;cd ..
    cd rtofs_ncoda.fd;make install;cd ..
    ;;

  clean)
    echo
    echo Running option clean
    echo
    cd rtofs_code.fd;make clean;cd ..
    cd rtofs_hycom.fd;./build_hycom.sh clean;cd ..
    cd rtofs_ncoda.fd;make clean;cd ..
    ;;

  *)
    echo
    echo Usage $0 \[clean\|install\|compile\]
    echo
    ;;
esac

