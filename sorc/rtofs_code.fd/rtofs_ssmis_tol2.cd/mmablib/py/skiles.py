import numpy as np
from math import *

import const
from latpt import *
nmtokm = 1.852

#better to give skpt a latpt (has-a)
class skpt:

  def __init__(self, lat = 0., lon = 0., wmoid = 0):
    self.lat = lat
    self.lon = lon
    self.wmoid  = int(wmoid)

  def show(self):
    print("skiles point ",self.wmoid, self.lon, self.lat)


class skfcst:

   def __init__(self, wmoid=0, lon=0., lat=0., wdir=0., dist=0.):
      self.wmoid   = wmoid
      self.lon  = float(lon)
      self.lat  = float(lat)
      self.wdir  = float(wdir)
      self.dist = float(dist)

   def show(self):
      print(self.wmoid, self.lon, self.lat, self.wdir, self.dist)

class skiles2_points :

  def __init__(self, fname='seaice_forecast.points'):
    self.model = []
    #
    fin = open(fname)
    count = 0
    x     = skpt()
    self.model.append(x)
    self.model[count] = skpt()
    count = 1
    for line in fin:
      words = line.split()
      wmoid  = words[0]
      lat = float(words[1])
      lon = float(words[2])
      self.model.append(x)
      self.model[count] = skpt(lat, lon, wmoid)
      #self.model[count].show()
      count += 1
    fin.close()
  
class skiles2_forecast :
  days    =  int(16)
  header  =  int( 5)
  nskiles =  int(207)

  def __init__(self, fname):
    self.refpts = skiles2_points()
    self.fcst = []
    self.npoints = int(0)
    
    fin = open(fname)
    j   = int(0)
    day = int(0)
    npts_fcst=int(0)
    blank_count = int(0)
    
    x = skfcst()

    for line in fin:

      if ( j < skiles2_forecast.header or j == (207 + 5) ):
        npts_fcst += 1
      else:
        if ( (j < 207+5)):
          words = line.split()
          wmoid = int(words[0])
          lon = self.refpts.model[wmoid].lon
          lat = self.refpts.model[wmoid].lat
          wdir = float(words[1])
          dist = float(words[2])*nmtokm
        else:
          if ( ( j > 212 )):
            if (len(line) < 5): 
              blank_count += 1
            else:
              words = line.split()
              wmoid = int(words[0])
              lon = float(words[1])
              lat = float(words[2])
              wdir = float(words[3])
              dist = float(words[4])*nmtokm

        if (blank_count == 0):
          self.fcst.append(x)
          self.fcst[j+int(day*self.npoints)-npts_fcst] = skfcst(wmoid, lon, lat, wdir, dist)
        elif (blank_count == 2):
          j = -1
          blank_count = 0
          npts_fcst = 0
          day += 1

      j += 1
      if (day != 0 and j == 0) :
        self.npoints = len(self.fcst) / day
# loop back to process next line.  
#  n.b. if j = 0, we've reached the end of a day and now know how many 
#    points are in the forecast
# all 16 days are in the one long vector

  def nearest(self, lat, lon, tolerance = 50) :
     dist = 1.e6
     j = int(-1)
     ref = latpt(lat,lon)
     for i in range (int(0), int(self.npoints) ) :
       x = latpt(self.fcst[i].lat, self.fcst[i].lon)
       tmp = ref.distance(x)
       if (tmp < dist):
         dist = tmp
         j = i
     #print("dist ",j,dist,lat,lon)
     if (dist < tolerance) :
       return j
     else:
       return -1
