noinst_PROGRAMS = sfversion floating_point_test write_read_test \
	lossy_comp_test error_test ulaw_test alaw_test dwvw_test \
	peak_chunk_test command_test stdin_test stdout_test stdio_test \
	pcm_test headerless_test pipe_test benchmark update_header_test \
	fix_this
	
SNDFILEDIR =../src
INCLUDES = -I$(srcdir)/$(SNDFILEDIR)
noinst_HEADERS = dft_cmp.h utils.h

autogen_sources = write_read_test.tpl write_read_test.def	\
					pcm_test.tpl pcm_test.def	\
					utils.tpl utils.def						\
					command_test.tpl command_test.def		\
					benchmark.tpl benchmark.def	

EXTRA_DIST = $(autogen_sources)

#===============================================================================

sfversion_SOURCES = sfversion.c 
sfversion_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

write_read_test_SOURCES = utils.c write_read_test.c
write_read_test_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

lossy_comp_test_SOURCES = utils.c lossy_comp_test.c
lossy_comp_test_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

fix_this_SOURCES = utils.c fix_this.c 
fix_this_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

error_test_SOURCES = error_test.c 
error_test_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

ulaw_test_SOURCES = utils.c ulaw_test.c
ulaw_test_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

alaw_test_SOURCES = utils.c alaw_test.c
alaw_test_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

dwvw_test_SOURCES = utils.c dwvw_test.c
dwvw_test_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

floating_point_test_SOURCES = utils.c dft_cmp.c floating_point_test.c
floating_point_test_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

peak_chunk_test_SOURCES = peak_chunk_test.c 
peak_chunk_test_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

command_test_SOURCES = command_test.c utils.c
command_test_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

pcm_test_SOURCES = utils.c pcm_test.c
pcm_test_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

headerless_test_SOURCES = utils.c headerless_test.c
headerless_test_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

stdin_test_SOURCES = stdin_test.c 
stdin_test_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

stdout_test_SOURCES = stdout_test.c 
stdout_test_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

stdio_test_SOURCES = stdio_test.c 
stdio_test_LDADD = 

pipe_test_SOURCES = pipe_test.c 
pipe_test_LDADD = 

benchmark_SOURCES = benchmark.c 
benchmark_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

update_header_test_SOURCES = update_header_test.c utils.c 
update_header_test_LDADD = $(SNDFILEDIR)/libsndfile.la -lm

#===============================================================================

write_read_test.c: write_read_test.def write_read_test.tpl
	autogen --writable --source-time write_read_test.def
	
pcm_test.c: pcm_test.def pcm_test.tpl
	autogen --writable --source-time pcm_test.def
	
utils.c: utils.def utils.tpl
	autogen --writable --source-time utils.def

command_test.c: command_test.def command_test.tpl
	autogen --writable --source-time command_test.def
	
benchmark.c: benchmark.def benchmark.tpl
	autogen --writable --source-time benchmark.def

#===============================================================================

check: generic-tests ulaw_test alaw_test floating_point_test \
		wav-tests aiff-tests au-tests raw-tests paf-tests svx-tests nist-tests \
		ircam-tests voc-tests w64-tests stdio-tests

generic-tests : error_test ulaw_test alaw_test command_test floating_point_test pcm_test
	./error_test
	./pcm_test
	./ulaw_test
	./alaw_test
	./dwvw_test
	./command_test ver
	./command_test norm
	./command_test format
	./command_test peak
	./command_test trunc
	./floating_point_test
	./headerless_test
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion` passed common tests."
	@echo "----------------------------------------------------------------------"

wav-tests: write_read_test lossy_comp_test peak_chunk_test update_header_test
	./write_read_test wav
	./lossy_comp_test wav_ima
	./lossy_comp_test wav_msadpcm
	./lossy_comp_test wav_ulaw
	./lossy_comp_test wav_alaw
	./lossy_comp_test wav_gsm610
	./peak_chunk_test wav
	./update_header_test wav
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion` passed tests on WAV files."
	@echo "----------------------------------------------------------------------"

aiff-tests: write_read_test lossy_comp_test peak_chunk_test update_header_test
	./write_read_test aiff
	./lossy_comp_test aiff_ulaw
	./lossy_comp_test aiff_alaw
	./lossy_comp_test aiff_gsm610
	./peak_chunk_test aiff
	./update_header_test aiff
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion` passed tests on AIFF files."
	@echo "----------------------------------------------------------------------"

au-tests: write_read_test lossy_comp_test update_header_test
	./write_read_test au
	./lossy_comp_test au_ulaw
	./lossy_comp_test au_alaw
	./lossy_comp_test au_g721
	./lossy_comp_test au_g723
	./update_header_test au
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion` passed tests on AU files."
	@echo "----------------------------------------------------------------------"

raw-tests: write_read_test lossy_comp_test
	./write_read_test raw
	./lossy_comp_test raw_ulaw
	./lossy_comp_test raw_alaw
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion` passed tests on RAW (header-less) files."
	@echo "----------------------------------------------------------------------"

paf-tests: write_read_test
	./write_read_test paf
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion` passed tests on PAF files."
	@echo "----------------------------------------------------------------------"

svx-tests: write_read_test update_header_test
	./write_read_test svx
	./update_header_test svx
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion` passed tests on SVX files."
	@echo "----------------------------------------------------------------------"

nist-tests: write_read_test lossy_comp_test
	./write_read_test nist
	./lossy_comp_test nist_ulaw
	./lossy_comp_test nist_alaw
	./update_header_test nist
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion` passed tests on NIST files."
	@echo "----------------------------------------------------------------------"

ircam-tests: write_read_test lossy_comp_test
	./write_read_test ircam
	./lossy_comp_test ircam_ulaw
	./lossy_comp_test ircam_alaw
	./update_header_test ircam
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion` passed tests on IRCAM files."
	@echo "----------------------------------------------------------------------"

voc-tests: write_read_test lossy_comp_test
	./write_read_test voc
	./lossy_comp_test voc_ulaw
	./lossy_comp_test voc_alaw
	./update_header_test voc
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion` passed tests on VOC files."
	@echo "----------------------------------------------------------------------"

w64-tests: write_read_test lossy_comp_test
	./write_read_test w64
	./lossy_comp_test w64_ima
	./lossy_comp_test w64_msadpcm
	./lossy_comp_test w64_ulaw
	./lossy_comp_test w64_alaw
	./lossy_comp_test w64_gsm610
	./update_header_test w64
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion` passed tests on W64 files."
	@echo "----------------------------------------------------------------------"

stdio-tests: stdio_test stdin_test stdout_test pipe_test
	./stdio_test
	./pipe_test
	@echo "----------------------------------------------------------------------"
	@echo "  `./sfversion` passed stdio and pipe tests."
	@echo "----------------------------------------------------------------------"


