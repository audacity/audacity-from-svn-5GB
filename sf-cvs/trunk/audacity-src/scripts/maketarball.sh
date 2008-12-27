#!/bin/bash

# Copyright 2003, 2004, 2005 Dominic Mazzoni and Matt Brubeck
# Distributed under the GNU General Public License 2.0.
# See the file LICENSE.txt for details.
# Re-written in Bash by Richard Ash 2006

function myrmrvf {
	# a replacement for rm -rvf that has it's output controlled
	# by the value of the first argument
	# setting it to 1 makes it verbose, to anything else makes it quiet
	if [ $1 -eq 1 ]
	then	#verbose mode
		shift
		echo "rm -rf $*"
		rm -rf $*
	else
		# quietly
		shift
		rm -rf $*
	fi
	}

function myrmvf {
	# a replacement for rm -vf that has it's output controlled
	# by the value of the first argument
	# setting it to 1 makes it verbose, to anything else makes it quiet
	if [ $1 -eq 1 ]
	then	#verbose mode
		shift
		echo "rm -f $*"
		rm -f $*
	else
		# quietly
		shift
		rm -f $*
	fi
	}

function myfindrm {
	# search the file tree removing files that match the specified pattern in
	# the second argument, with output controlled by the value of the first
	# argument.
	# setting it to 1 makes it verbose, to anything else makes it quiet
	if [ $1 -eq 1 ]; then
		find . -name "$2" -print -delete
	else
		find . -name "$2" -delete
	fi
}

function updsln  {
	# removes unwanted projects from the Windows solution
	# setting it to 1 makes it verbose, to anything else makes it quiet
	if [ $1 -eq 1 ]
	then	#verbose mode
		shift
		echo "sed -e '/$*/,/EndProject/d' win/audacity.sln >win/audacity.sln.new"
		sed -e "/$*/,/EndProject/d" win/audacity.sln >win/audacity.sln.new
		echo "mv win/audacity.sln.new win/audacity.sln"
      mv win/audacity.sln.new win/audacity.sln
	else
		# quietly
		shift
		sed -e "/$*/,/EndProject/d" win/audacity.sln >win/audacity.sln.new
      mv win/audacity.sln.new win/audacity.sln
	fi
	}

echo "Maketarball 2.0.0 -- make an Audacity distribution tarball"

# check number of arguments, if not one then print a usage message
if [ $# == 1 ]; then
	:
	# carry on, looks like they know what they are doing
else
    echo "Script to make directory trees for audacity source tarballs"
    echo "Usage: $0 <mode>"
	echo "Where mode is either \"quiet\" or \"verbose\""
	exit 1
fi

if [ "$1" = "quiet" ]; then
		mode=0
elif [ "$1" = "verbose" ]; then
		mode=1
else
	echo "The argument to $0 must be either \"quiet\" or \"verbose\""
	exit 1
fi

if [ -f "src/Audacity.h" ]
then :
	# a null body - we will just carry straight on 
else
	echo "$0 must be run from top-level audacity directory"
	exit 1
fi

# find version number from C header file
major_version=`awk '/^#define+ AUDACITY_VERSION / {print $3}' src/Audacity.h`
minor_version=`awk '/^#define+ AUDACITY_RELEASE / {print $3}' src/Audacity.h`
micro_version=`awk '/^#define+ AUDACITY_REVISION / {print $3}' src/Audacity.h`
version_suffix=`awk '/^#define+ AUDACITY_SUFFIX / {split($0,subs,"\""); print(subs[2]) }' src/Audacity.h`

version="${major_version}.${minor_version}.${micro_version}${version_suffix}"
echo "version set to ${version}"

printf "making copy of source directory... "
cp -r . "../audacity-src-${version}"
printf "Done\n"
cd "../audacity-src-${version}"

printf "Done\n"

printf "making distclean... "
if [ $mode -eq 1 ]; then
	make distclean;
else
	make distclean 2>/dev/null > /dev/null;
fi
printf "Done\n"

printf "removing CVS directories... ";
find . -depth -name 'CVS' -execdir rm -rf '{}' ';'
# -depth is needed to avoid find trying to examine directories it has just
# deleted.
# The sort of quotes used is critical!
myfindrm $mode ".cvsignore"
printf "Done\n"

printf "removing vim / emacs temp files... ";
myfindrm $mode "*~"
printf "\nremoving CVS conflict files... ";
myfindrm $mode ".#*"
printf "Done\n"

printf "removing executable and other intermediate files... ";
myrmvf $mode audacity src/.depend src/.gchdepend
myfindrm $mode config.status
myfindrm $mode config.log
myfindrm $mode Makefile
myfindrm $mode config.cache
find . -depth -name 'autom4te.cache' -execdir rm -rf '{}' ';'
myfindrm $mode aclocal.m4
printf "Done\n"

printf "removing executable bit from source files... ";
if [ $mode -eq 1 ]; then
	find . -name '*.cpp' -executable -execdir chmod ugo-x '{}' ';' -print
	find . -name '*.h' -executable -execdir chmod ugo-x '{}' ';' -print
else
	find . -name '*.cpp' -executable -execdir chmod ugo-x '{}' ';'
	find . -name '*.h' -executable -execdir chmod ugo-x '{}' ';'
fi
printf "Done\n"

printf "removing orphaned symlinks in lib-src/ ... ";
myrmvf $mode lib-src/*.a 
printf "Done\n"

printf "removing bugs and todo lists ... ";
myrmvf $mode bugs.txt todo.txt
printf "Done\n"

printf "removing scripts and tests ... ";
myrmrvf $mode scripts tests
printf "Done\n"

printf "removing unused libraries from CVS tree ..."
myrmrvf $mode lib-src/allegro;
myrmrvf $mode lib-src/iAVC lib-src/id3lib lib-src/libscorealign;
myrmrvf $mode lib-src/portaudio lib-src/portburn lib-src/rtaudio; 
myrmrvf $mode lib-src/wave++;

printf "Done\n"

printf "removing libraries that should be installed locally..."
myrmrvf $mode lib-src/expat lib-src/libflac lib-src/libid3tag; 
myrmrvf $mode lib-src/liblrdf lib-src/libmad lib-src/libogg lib-src/libraptor;
myrmrvf $mode lib-src/libsamplerate lib-src/libsndfile;
myrmrvf $mode lib-src/libvorbis lib-src/redland lib-src/slv2 lib-src/soundtouch;
myrmrvf $mode lib-src/twolame;
printf "Done\n"

printf "removing qa ... ";
myrmrvf $mode qa 
printf "Done\n"

printf "removing doxygen output files ... ";
myrmrvf $mode dox 
printf "Done\n"

printf "removing unused portaudio-v19 directories ... ";
myrmrvf $mode lib-src/portaudio-v19/docs
myrmrvf $mode lib-src/portaudio-v19/pa_asio
myrmrvf $mode lib-src/portaudio-v19/pa_sgi
myrmrvf $mode lib-src/portaudio-v19/pa_mac_sm
myrmrvf $mode lib-src/portaudio-v19/test
myrmrvf $mode lib-src/portaudio-v19/testcvs
printf "Done\n"

printf "removing wxstd locale files (since they come with wxWidgets)\n";
myrmrvf $mode locale/wxstd

printf "removing Nyquist plug-ins that are just for show ... "
myrmvf $mode plug-ins/analyze.ny plug-ins/fadein.ny plug-ins/fadeout.ny
myrmvf $mode plug-ins/undcbias.ny
printf "Done\n"

printf "Giving VC++ project/workspace files DOS line endings ... "
if [ $mode -eq 1 ]; then
	for file in `find . \( -name '*.ds?' -print \) -or  \( -name '*.vcproj' -print \) -or \( -name '*.sln' -print \)`
	do
		unix2dos "$file" 
	done
else
	for file in `find . \( -name '*.ds?' -print \) -or  \( -name '*.vcproj' -print \) -or \( -name '*.sln' -print \)`
	do
		unix2dos "$file" > /dev/null 2>/dev/null
	done
fi

printf "Done\n"

printf "Changing Windows header so that it doesn't try to build with\n";
printf "support for optional libraries by default.\n";

echo "" >> "win/configwin.h"
echo "// The Audacity source tarball does NOT come with" >> "win/configwin.h"
echo "// any optional libraries." >> "win/configwin.h"
echo "" >> "win/configwin.h"
echo "// Delete the following lines if you install them manually." >> "win/configwin.h"
echo "" >> "win/configwin.h"
echo "#undef MP3SUPPORT" >> "win/configwin.h"
echo "#undef USE_FFMPEG" >> "win/configwin.h"
echo "#undef USE_LIBFLAC" >> "win/configwin.h"
echo "#undef USE_LIBID3TAG" >> "win/configwin.h"
echo "#undef USE_LIBLRDF" >> "win/configwin.h"
echo "#undef USE_LIBMAD" >> "win/configwin.h"
echo "#undef USE_LIBSAMPLERATE" >> "win/configwin.h"
echo "#undef USE_LIBTWOLAME" >> "win/configwin.h"
echo "#undef USE_LIBVORBIS" >> "win/configwin.h"
echo "#undef USE_SLV2" >> "win/configwin.h"
echo "#undef USE_SOUNDTOUCH" >> "win/configwin.h"
echo "#undef EXPERIMENTAL_SCOREALIGN" >> "win/configwin.h"


printf "removing unwanted projects from VC++ solution\n"
updsln $mode libflac
updsln $mode libflac++
updsln $mode libid3tag
updsln $mode libmad
updsln $mode liblrdf
updsln $mode librdf
updsln $mode libsamplerate
updsln $mode twolame
updsln $mode libvorbis
updsln $mode libogg
updsln $mode raptor
updsln $mode rasqal
updsln $mode slv2
updsln $mode soundtouch
updsln $mode libscorealign

printf "Done\n"
