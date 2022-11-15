def midpoint(x, y, z):
#Robert Grumbine
#1 June 2018
  z.i = (x.i + y.i)/2.0
  z.j = (x.j + y.j)/2.0
  return z
  

class ijpt:

  def __init__(self,i = 0, j = 0):
    self.i = i
    self.j = j

  def show(self):
    print (self.i,self.j)

  def im(self):
    x = ijpt()
    x.i = self.i - 1
    x.j = self.j
    return x
  def ip(self):
    x = ijpt()
    x.i = self.i + 1
    x.j = self.j
    return x
  def jm(self):
    x = ijpt()
    x.i = self.i
    x.j = self.j - 1
    return x
  def jp(self):
    x = ijpt()
    x.i = self.i
    x.j = self.j + 1
    return x

