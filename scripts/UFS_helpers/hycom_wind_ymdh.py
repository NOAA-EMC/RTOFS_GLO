#!/usr/bin/env python3

import numpy as np

"""
To convert time stamp in archive to yyyy/mm/dd:hh
a translation of
https://github.com/HYCOM/HYCOM-tools/blob/master/bin/hycom_wind_ymdh.f
"""

def hycom_wind_ymdh(dtime, yrflag=3):

  if yrflag !=3:
    raise ValueError("yrflag must be 3")

  [iyear, month, iday, ihour] = fordate(dtime)

  return iyear, month, iday, ihour
# --

def fordate(dtime):

  month0=np.asarray([[1,  31,  61,  91, 121, 151, 181, 211, 241, 271, 301, 331, 361],
               [1,  32,  60,  91, 121, 152, 182, 213, 244, 274, 305, 335, 366],
               [1,  32,  61,  92, 122, 153, 183, 214, 245, 275, 306, 336, 367]])

  [iyear, jday, ihour] = forday(dtime)
  #print(f"fordate: dtime, iyear, jday, ihour", dtime, iyear, jday, ihour)

  if (np.mod(iyear, 4) == 0):
    k = 3  #leap year
  else:
    k = 2  #standard year
  #print(f"fordate: k", k)

  for m in range(0, 12):
    #print(jday, month0[k-1, m], month0[k-1, m+1])
    if ((jday >= month0[k-1, m]) and (jday < month0[k-1, m+1])):
      month = m+1
      iday  = jday - month0[k-1, m] + 1
  #print(f"fordate: month, iday", month, iday)

  return iyear, month, iday, ihour
# --

def forday(dtime):
  # model day is calendar days since 01/01/1901
  #print(f"forday INPUT dtime: ", dtime)

  iyr   = int((dtime-1.)/365.25)
  nleap = int(iyr/4.)
  dtim1 = 365.0*iyr + nleap + 1.
  day   = dtime - dtim1 + 1.
  #print(f"forday: iyr, nleap, dtim1, day\n", iyr, nleap, dtim1, day)

  if dtim1 > dtime:
    iyr = iyr - 1
  elif day >= 367.:
    iyr = iyr + 1
  elif ((day >= 366.0) and (np.mod(iyr,4) != 3)):
    iyr = iyr + 1
  nleap = int(iyr/4.)
  dtim1 = 365.0*iyr + nleap + 1.
  #print(f"forday: iyr, nleap, dtim1", iyr, nleap, dtim1)

  iyear =  int(1901 + iyr)
  iday  =  int(dtime - dtim1 + 1.)
  ihour = int( (dtime - dtim1 + 1.001 - iday)*24.)
  #print(f"forday: iyear, iday, ihour:", iyear, iday, ihour)

  return iyear, iday, ihour
