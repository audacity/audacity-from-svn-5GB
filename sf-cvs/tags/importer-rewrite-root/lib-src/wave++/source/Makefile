# Makefile for the whole library 

all: libw.a

OBJS = common.o Interval.o ArrayTree.o Hedge.o Utility.o InOutUtil.o \
	WavePacket.o WaveTrans.o ConvDec.o QMF.o fft.o Gabor.o \
	Partition.o ShiftGaborMP.o FFTGaborMP.o

libw.a: $(OBJS)
	ar rcvs $@ $(OBJS)
	ranlib $@

clean: 
	rm -f *.o *~ libw.a


# Implicit rule for compiling .cc files

%.o: %.cc
	$(CC) -c $(CFLAGS) $< -o $@


# Compiler settings

CC = g++
CFLAGS = -O2 -Wall


# File dependencies

common.o: common.h

Interval.o: Interval.h

ArrayTree.o: Interval.h ArrayTree.h

Hedge.o: Interval.h Hedge.h

InOutUtil.o: Interval.h Hedge.h ArrayTree.h InOutUtil.h

Utility.o: Interval.h Hedge.h ArrayTree.h BinTree.h Utility.h

QMF.o: common.h QMF.h

ConvDec.o: Interval.h QMF.h ConvDec.h

WavePacket.o: Interval.h ArrayTree.h ConvDec.h QMF.h WavePacket.h

WaveTrans.o: Interval.h ConvDec.h QMF.h WaveTrans.h

fft.o: common.h fft.h

Gabor.o: Gabor.h Interval.h

Partition.o: Interval.h Partition.h

ShiftGaborMP.o: ShiftGaborMP.h Partition.h  Gabor.h

FFTGaborMP.o: FFTGaborMP.h fft.h Gabor.h
