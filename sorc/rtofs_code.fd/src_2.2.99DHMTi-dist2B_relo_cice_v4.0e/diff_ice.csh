#! /bin/csh -f
#
set echo
#
# --- diff source code with another HYCOM_CICE directory
#
setenv HC ../theia/src_2.2.99DHMTi-900-dist2B_relo_cice_v4.0e
#
foreach f ( comp_ice.csh */*.F90 */*/*.F90 )
  C "*****     *****     *****     *****     *****     *****     *****"
  diff -ibw $f ${HC}/$f
end
#
foreach f ( config/*_cice hycom/*.[Ffh] )
  C "*****     *****     *****     *****     *****     *****     *****"
  diff -ibw $f ${HC}/$f
end
##
## --- if just added STOKES
##
#foreach n ( barotp cnuity diapfl momtum mxkprf )
#  echo "*****     *****     *****     *****     *****     *****     *****"
#  diff -ibw hycom/${n}.F ${HC}/hycom/${n}.f
#end
