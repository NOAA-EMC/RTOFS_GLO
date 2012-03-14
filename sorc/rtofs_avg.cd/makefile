#LINUX
CC=g++
OPTS=-O2 -DLINUX -Wall -I/usr1/rmg3/checkout/mmablib/include
LIBS=~/usrlocal/lib/libombf_4.a

#IBM
CC=ncepxlC
OPTS=-O2 -DIBM -I../../lib/sorc/omb/include
LIBS=-L../../lib/ -lombf_4

#----------- Should be no changes below here -------------

all : rtofs_avg

rtofs_avg : rtofs_avg.C
	$(CC) $(OPTS) rtofs_avg.C -o rtofs_avg $(LIBS)
 
