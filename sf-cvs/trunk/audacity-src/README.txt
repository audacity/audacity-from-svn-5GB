Audacity: A Free, Cross-Platform Digital Audio Editor

Version 1.1.0 (under development)
For changelog, see the bottom of this document.

http://audacity.sourceforge.net/

Primary author:

  Dominic Mazzoni <dominic@minorninth.com>

Authors:

  Dominic Mazzoni <dominic@minorninth.com>
  Roger Dannenberg <rbd+@cs.cmu.edu>
  Jason Cohen <cohen3+@andrew.cmu.edu>
  Robert Leidle <rfl+@andrew.cmu.edu>
  Mark Tomlinson <marktoml@hotmail.com>
  Joshua Haberman <joshua@haberman.com>
  Nasca Octavian Paul <paulnasca@email.ro> or <paulnasca@yahoo.com>
  Logan Lewis <proxima@proxc.com>
  Matt Brubeck <mbrubeck@cs.hmc.edu>
  Mark Phillips <mitb@totaldeath.com>
  Tony Oetzmann <airon@epost.de>
  Brian Gunlogson <bmg300@users.sf.net>
  Shane Mueller <nestify@users.sf.net>

Icons and logo:

  Harvey Lubin <agrapha@agrapha.com>
  http://www.agrapha.com/

Aqua/MacOS graphics:

  Tom Woodhams <tom@imaginemedia.co.uk>

-------------------------------------------------------------

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program (in a file called LICENSE.txt); if not, go
to http://www.gnu.org/copyleft/gpl.html or write to 

  Free Software Foundation, Inc.
  59 Temple Place - Suite 330
  Boston, MA 02111-1307 USA

-------------------------------------------------------------

Source code to this program is always available; for more
information visit our website at:

  http://audacity.sourceforge.net/

This program uses wxWindows, a cross-platform GUI toolkit.  To
compile this program, you will need to download wxWindows from:

  http://www.wxwindows.org/

To compile on Linux and other Unix systems, simply run:

  ./configure
  make
  make install  # as root
  
If you want to do any development, you might want to generate
a configure cache and header dependencies:

  ./configure -C
  make dep

For more information on compilation on other platforms (VC++
for Windows and CodeWarrior for Mac are supported) please
email audacity-devel@lists.sourceforge.net

-------------------------------------------------------------

Known issues/problems:

* Windows: The floating tool palette does not minimize when
  minimizing a project window.  As a workaround, you can
  just keep the tool palette docked with the project window.

* Linux: Full duplex (play one track while recording another)
  does not seem to work, even if your sound card supports it.
  This is likely a problem with the way we are using OSS.

* Linux: Some users report choppy audio.  This is likely a
  problem with the way we are using OSS.

* Linux: Sound cards that only deal with more than two
  channels are not yet supported.

-------------------------------------------------------------

Major new features in Audacity 1.1.0:

  * Automatic real-time resampling (using linear
      interpolation)
  * 24-bit and 32-bit sample formats
  * New toolbar buttons and a new Edit toolbar
  * Autoscroll while playing or recording
  * LADSPA effects plugins on Linux / Unix
  * Ogg Vorbis exporting
  * Export to command-line programs on Unix
  * Many user interface improvements

New libraries in Audacity 1.1:

  * libmad for fast MP3 importing
  * libid3tag for editing MP3 file information
  * libsndfile to read and write more audio file formats
  * PortAudio for cross-platform audio playing and recording
