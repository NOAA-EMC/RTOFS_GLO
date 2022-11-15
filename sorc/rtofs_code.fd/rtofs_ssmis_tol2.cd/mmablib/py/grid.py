import numpy as np
import const
#Robert Grumbine
#1 June 2018

from ijpt import *
from latpt import *

def ok(x, loc):
  return (loc.i >= 0 and loc.i < x.shape[0] and
          loc.j >= 0 and loc.j < x.shape[1] )

#Includes only the mapping, not the data
#############################################################
class psgrid:

  def locate(self, i, j, z):
    z.lat = i
    z.lon = j


class llgrid:

  def locate(self, i, j, z):
    z.lat = self.firstlat + j*self.dlat
    z.lon = self.firstlon + i*self.dlon
    while (z.lon > 360.):
      z.lon -= 360.

#  def locate(self, i, j):
#    z = latpt()
#    z.lat = self.firstlat + j*self.dlat
#    z.lon = self.firstlon + i*self.dlon
#    while (z.lon > 360.):
#      z.lon -= 360.
#    return z

#############################################################

class global_5min(llgrid):

  def __init__(self, nx = 12*360, ny = 12*180):
    self.dlat = -1./12.
    self.dlon =  1./12.
    self.firstlat = 90 + self.dlat/2. 
    self.firstlon = self.dlon / 2.
    self.nx = nx
    self.ny = ny

    
class global_halfdeg(llgrid):

  def __init__(self, nx = 720, ny = 360):
    self.dlat = -0.5
    self.dlon =  0.5
    self.firstlat = 90 + self.dlat/2. 
    self.firstlon = self.dlon / 2.
    self.nx = nx
    self.ny = ny

class global_nthdeg(llgrid):

  def __init__(self, n = 1.0):
    self.dlat = -1./float(n)
    self.dlon =  1./float(n)
    self.firstlat = 90 + self.dlat/2.
    self.firstlon = self.dlon / 2.
    self.nx = 360*n
    self.ny = 180*n

    
