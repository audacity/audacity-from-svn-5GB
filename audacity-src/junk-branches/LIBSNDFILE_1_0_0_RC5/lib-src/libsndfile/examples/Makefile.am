# This file was automatically generated
# DO NOT EDIT!

bin_PROGRAMS = sndfile-info sndfile-play

noinst_PROGRAMS = sfconvert wav32_aiff24 make_sine sfhexdump sfprocess \
		sfdump list_formats sndfile2oct generate

# This is the BeOS version of sndfile-play. It needs to be compiled with the C++
# compiler.
EXTRA_DIST = sndfile-play-beos.cpp

OS_SPECIFIC_INCLUDES = @OS_SPECIFIC_INCLUDES@
OS_SPECIFIC_LINKS = @OS_SPECIFIC_LINKS@

SNDFILEDIR =../src
INCLUDES = -I$(srcdir)/$(SNDFILEDIR) $(OS_SPECIFIC_INCLUDES)

sndfile_info_SOURCES = sndfile-info.c 
sndfile_info_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

sndfile_play_SOURCES = sndfile-play.c 
sndfile_play_LDADD = $(SNDFILEDIR)/libsndfile.la -lm $(OS_SPECIFIC_LINKS)

sfconvert_SOURCES = sfconvert.c 
sfconvert_LDADD = $(SNDFILEDIR)/libsndfile.la -lm
 
sndfile2oct_SOURCES = sndfile2oct.c 
sndfile2oct_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

wav32_aiff24_SOURCES = wav32_aiff24.c 
wav32_aiff24_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

make_sine_SOURCES = make_sine.c 
make_sine_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

sfhexdump_SOURCES = sfhexdump.c 
sfhexdump_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

sfprocess_SOURCES = sfprocess.c 
sfprocess_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

sfdump_SOURCES = sfdump.c 
sfdump_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

list_formats_SOURCES = list_formats.c 
list_formats_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

generate_SOURCES = generate.c 
generate_LDADD = $(SNDFILEDIR)/libsndfile.la -lm


