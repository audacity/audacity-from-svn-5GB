# Make PortAudio Library for Linux

DEST = ../../../lib/linux/lib

CC = gcc

CFLAGS = -g -I../pa_common
PASRC = ../pa_common/pa_lib.c pa_linux_oss.c
PAINC = ../pa_common/portaudio.h
PAOBJ = ../pa_common/pa_lib.o pa_linux_oss.o

portaudio.a: $(PAOBJ)
	ar ruv portaudio.a $(PAOBJ)
	ranlib portaudio.a
	cp portaudio.a $(DEST)

%.o: %.c $(PAINC) Makefile
	$(CC) -c $(CFLAGS) $< -o $@
