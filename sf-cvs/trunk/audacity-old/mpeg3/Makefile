include global_config
CFLAGS += 
export CFLAGS

OBJS = \
	bitstream.o \
	libmpeg3.o \
	mpeg3atrack.o \
	mpeg3css.o \
	mpeg3demux.o \
	mpeg3ifo.o \
	mpeg3io.o \
	mpeg3title.o \
	mpeg3toc2.o \
	mpeg3vtrack.o

AUDIOOBJS = \
	audio/ac3.o \
	audio/bit_allocation.o \
	audio/dct.o \
	audio/exponents.o \
	audio/header.o \
	audio/layer2.o \
	audio/layer3.o \
	audio/mantissa.o \
	audio/mpeg3audio.o \
	audio/pcm.o \
	audio/synthesizers.o \
	audio/tables.o

VIDEOOBJS = \
	video/getpicture.o \
	video/headers.o \
	video/idct.o \
	video/macroblocks.o \
	video/mmxtest.o \
	video/motion.o \
	video/mpeg3video.o \
	video/output.o \
	video/reconstruct.o \
	video/seek.o \
	video/slice.o \
	video/vlc.o

OUTPUT = libmpeg3.a
UTILS = dump mpeg3cat mpeg3toc mpeg3split
LIBS = -lm -lpthread

all: $(OUTPUT) util

$(OUTPUT): $(OBJS)
	$(MAKE) -C audio
	$(MAKE) -C video
	ar rcs $(OUTPUT) $(OBJS) $(MMXOBJS) $(VIDEOOBJS) $(AUDIOOBJS) $(IFOOBJS)

dump: dump.o $(OUTPUT)
	$(CC) -o dump dump.o $(OUTPUT) $(LIBS)

mpeg3cat: mpeg3cat.o $(OUTPUT)
	$(CC) -o mpeg3cat mpeg3cat.o $(OUTPUT) $(LIBS)

mpeg3toc: mpeg3toc.o $(OUTPUT)
	$(CC) -o mpeg3toc mpeg3toc.o $(OUTPUT) $(LIBS)

mpeg3split: mpeg3split.o $(OUTPUT)
	$(CC) -o mpeg3split mpeg3split.o $(OUTPUT) $(LIBS)

util: $(UTILS)

.c.o:
	$(CC) -c `./c_flags` $*.c

clean:
	make -C audio clean
	make -C video clean
	rm -f core *.o $(OUTPUT)
	rm -f $(UTILS)

backup: clean
	cd .. && \
	tar zcf libmpeg3.tar.gz libmpeg3

wc:
	cat *.c *.h audio/*.c audio/*.h video/*.c video/*.h | wc
