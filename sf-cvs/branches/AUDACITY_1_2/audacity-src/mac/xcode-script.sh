#!/bin/sh
echo "This script should be run once after building all of the libraries"
echo "in lib-src, but before trying to compiling Audacity with Xcode."
ln -fs ../lib-src/allegro/allegro.a ../lib-src/allegro/liballegro.a
ln -fs ../lib-src/expat/expat.a ../lib-src/expat/libexpat.a
ln -fs ../lib-src/soundtouch/soundtouch.a ../lib-src/soundtouch/libsoundtouch.a
ln -fs ../lib-src/portaudio/portaudio.a ../lib-src/portaudio/libportaudio.a
ln -fs ../lib-src/portmixer/portmixer.a ../lib-src/portmixer/libportmixer.a
echo "Done."
