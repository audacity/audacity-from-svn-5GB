# Make PortAudio Library for Unix/OSS

CC = gcc

CFLAGS = -g -O -ansi -I../pa_common
PASRC = ../pa_common/pa_lib.c pa_unix_oss.c
PAINC = ../pa_common/portaudio.h
PAOBJ = ../pa_common/pa_lib.o pa_unix_oss.o

portaudio.a: $(PAOBJ)
	ar ruv portaudio.a $(PAOBJ)
	ranlib portaudio.a
#	cp portaudio.a $(DEST)

clean:
	rm -f portaudio.a *.o

%.o: %.c $(PAINC) Makefile
	$(CC) -c $(CFLAGS) $< -o $@
