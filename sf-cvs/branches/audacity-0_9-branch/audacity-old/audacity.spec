Summary: Audio Editor
Name: audacity
Version: 1.0.0
Release: 2
URL: http://audacity.sourceforge.net/
License: GPL
Group: Audio
Source0: audacity-1.0.0.tgz
BuildRoot: /tmp/rpm-root
Requires: wxGTK >= 2.2.9
Requires: libogg >= 1.0rc3
Requires: libvorbis >= 1.0rc3
BuildRequires: wxGTK-devel >= 2.2.9
BuildRequires: libogg-devel >= 1.0rc3
BuildRequires: libvorbis-devel >= 1.0rc3

%description
Audacity is a free audio editor. You can record sounds, play sounds,
import and export WAV, AIFF, MP3, and Ogg Vorbis files, and more.  Use
it to edit your sounds using Cut, Copy and Paste (with unlimited
Undo), mix tracks together, or apply effects to your recordings. It
also has a built-in amplitude envelope editor, a customizable
spectrogram mode and a frequency analysis window for audio analysis
applications. Built-in effects include Bass Boost, Wahwah, and Noise
Removal.

%prep

%setup -q

%build
./configure --with-id3 --with-oss --with-xaudio --with-vorbis --prefix=$RPM_BUILD_ROOT/usr
make
strip audacity

%install
make install

%clean
rm -rf $RPM_BUILD_ROOT

%files 
%defattr(-, root, root)
/usr/bin/audacity
/usr/share/doc/audacity/README.txt
/usr/share/doc/audacity/LICENSE.txt
/usr/share/doc/audacity/audacity-help.htb




