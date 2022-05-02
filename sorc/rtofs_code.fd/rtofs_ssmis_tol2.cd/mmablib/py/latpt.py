from math import *

from const import *
#Robert Grumbine
#1 June 2018

class latpt:

  def __init__(self,lat = 0., lon = 0.):
    self.lat = float(lat)
    self.lon = float(lon)

  def show(self):
    print (str(self.lon),str(self.lat))

  def distance(self, x ):
### These are in the const.py
#    earth_radius = 6371.2 #km
#    rpdg = 3.1416 / 180.
##
    ab = (90.-self.lat)*const.rpdg
    ac = (90.-x.lat)   *const.rpdg
    bc = abs(self.lon - x.lon)*const.rpdg

    arg = cos(ab)*cos(ac)+sin(ab)*sin(ac)*cos(bc)
    if (arg > 1):
      arg = 1
    if (arg < -1):
      arg = -1

    return const.earth_radius * acos(arg)

#Direction in degrees, distance in km (const.earth_radius)
  def bearing_from(self, distance, direction) :
     final      = latpt()
     R          = const.earth_radius
     direction *= const.rpdg  #beware conventions on direction
     lat1 = self.lat*const.rpdg
     lon1 = self.lon*const.rpdg
     lat2 = asin( sin(lat1)*cos(distance/R) + cos(lat1)*sin(distance/R)*cos(direction))
     lon2 = lon1 + atan2(sin(direction)*sin(distance/R)*cos(lat1), cos(distance/R)-sin(lat1)*sin(lat2))
     final.lat = lat2 / const.rpdg
     final.lon = lon2 / const.rpdg
     return final
  
