#LINUX
CC=g++
OPTS=-O2 -DLINUX -Wall -I/usr1/rmg3/checkout/mmablib/include
LIBS=~/usrlocal/lib/libombf_4.a

#IBM
CC=ncepxlC
OPTS=-O2 -DIBM -I../../lib/sorc/omb/include
LIBS=-L../../lib/ -lombf_4

#--------- Should be no changes below here ------------

all : rtofs_gsf 

rtofs_gsf : rtofs_gsf.C
	$(CC) $(OPTS) rtofs_gsf.C -o rtofs_gsf $(LIBS)
