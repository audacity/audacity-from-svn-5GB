Audacity: A Free, Cross-Platform Digital Audio Editor

Version 1.2.0
For changelog, see the bottom of this document.

WWW:   http://audacity.sourceforge.net/

Email: audacity-help@lists.sourceforge.net

Lead Developers:

  Dominic Mazzoni
  Joshua Haberman
  Matt Brubeck

Developers:

  Brian Gunlogson
  Shane Mueller
  Vaughan Johnson
  Greg Mekkes
  Vince Busam
  Augustus Saunders  
  Tony Oetzmann
  Paul Nasca
  Roger Dannenberg

Other Contributors:

  Dave Beydler
  Jason Cohen
  Steve Harris
  Daniil Kolpakov
  Robert Leidle
  Logan Lewis
  Jason Pepas
  Mark Phillips
  Jonathan Ryshpan
  Patrick Shirkey
  Mark Tomlinson
  David Topper
  Rudy Trubitt

The Audacity Logo:

  Harvey Lubin
  http://www.agrapha.com/

Special Thanks:

  The wxWindows Team
  The Ogg Vorbis Team
  Rob Leslie (libmad)
  Ross Bencina and Phil Burk (PortAudio)
  Erik de Castro Lopo (libsndfile)
  Verilogix, Inc.

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

Audacity is built upon other free libraries; some of
these libraries may have come with Audacity in the lib-src
directory.  Others you are expected to install first if
you want Audacity to have certain capabilities.  Most
of these libraries are not distributed under the terms
of the GPL, but rather some other free, GPL-compatible
license.  Specifically:

  wxWindows: LGPL
    Cross-platform GUI library - must be downloaded and
    compiled separately.

  expat: BSD-like license.
    Provides XML parsing.  Included with Audacity

  iAVC: LGPL
    Part of the code to the AVC Compressor effect.
    Included with Audacity.

  libid3tag: GPL
    Reads/writes ID3 tags in MP3 files.  Optional
    separate download as part of libmad.

  libmad: GPL
    Decodes MP3 files.  Optional separate download.

  libnyquist: BSD-like license.
    Functional language for manipulating audio; available
    within Audacity for effects processing.

  libogg: BSD-like license.
    Optional separate download, along with libvorbis.

  libsndfile: LGPL
    Reads and writes uncompressed PCM audio files.
    Included with Audacity.

  libvorbis: BSD-like license.
    Decodes and encodes Ogg Vorbis files.  Optional
    separate download.

For more information, see the documentation inside
each library's source code directory.

-------------------------------------------------------------------------
Additional copyright information:
-------------------------------------------------------------------------

Nyquist

Copyright (c) 2000-2002, by Roger B. Dannenberg
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

Redistributions of source code must retain the copyright notice, the
list of conditions, and the disclaimer, all three of which appear below under
"COPYRIGHT AND LICENSE INFORMATION FOR XLISP."

Redistributions in binary form must reproduce the above copyright notice, this
list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

Redistributions in binary form must reproduce the copyright notice, the
list of conditions, and the disclaimer, all three of which appear below under
"COPYRIGHT AND LICENSE INFORMATION FOR XLISP," in the documentation and/or
other materials provided with the distribution.

Neither the name of Roger B. Dannenberg, Carnegie Mellon University, nor the 
names of any contributors may be used to endorse or promote products derived 
from this software without specific prior written permission.

COPYRIGHT AND LICENSE INFORMATION FOR XLISP (part of Nyquist):

Copyright (c) 1984-2002, by David Michael Betz
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer. 

Redistributions in binary form must reproduce the above copyright notice, this
list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

Neither the name of David Michael Betz nor the names of any contributors may be
used to endorse or promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-------------------------------------------------------------

Compilation instructions:

First you must download wxWindows from:

  http://www.wxwindows.org/

If you install the RPM, make sure you install the devel RPM
as well, otherwise, you won't be able to compile Audacity
from source.

To compile on Linux, Mac OS X, and other Unix systems,
simply execute these commands:

  ./configure
  make
  make install  # as root

To see compile-time options you can set, you can type
"./configure --help".
  
If you want to do any development, you might want to generate
a configure cache and header dependencies:

  ./configure -C
  make dep

To compile on Windows using MSVC++, please follow the
instructions found in compile.txt in the "win" subdirectory.

For more information on compilation (CodeWarrior for Mac is
also supported) please email audacity-help@lists.sourceforge.net

-------------------------------------------------------------

Known issues/problems:

* Windows: The floating tool palette does not minimize when
  minimizing a project window.  As a workaround, you can
  just keep the tool palette docked with the project window.

-------------------------------------------------------------

New features in Audacity 1.2.0:
  * User Interface
    - Vertical zooming of tracks.  Click or drag on a track's
      ruler to zoom in.  Shift-click or right-click to zoom in.
    - Improved look and placement of toolbars
    - Complete implementation of editable keyboard shortcuts.
      You can optionally save/load a keyboard layout to share
      with friends.
    - Type "Z" to snap the selection to the nearest zero-crossings.
	 - Support for opening multiple files from the same dialog.
    - Support for using a text file to specify a list of audio files
	   to open with offsets.

  * Effects
    - Repeat Last Effect
    - Improved VST plug-in support
    - Most effects now have a Preview button
    - Compressor (Dynamic Range Compressor)
    - Change Pitch (without changing tempo) - automatically
      determines the starting fundamental frequency of the
      area you have selected
    - Change Tempo (without changing pitch)
    - Change Speed (changing both pitch and tempo)
    - Repeat effect - useful for creating loops

  * Audio I/O
    - Type "1" to hear a 1-second preview of the audio surrounding
      the cursor
    - Looped-play mode - hold down Shift while clicking Play, or
      type "L".

  * File I/O
    - Audacity 1.2.0 opens project files from all previous versions
      of Audacity from 0.98 through 1.1.3.

  * Many more bug fixes and new features

New features in Audacity 1.1.3:
  * User Interface
    - New Mixer toolbar allows you to control the output
      volume, input volume, and input source directly
      from Audacity.
    - Every track now has its own gain and pan controls.

  * File I/O
    - Uses improved project file format.  (Unfortunately reading
      previous formats, including 1.1.1, is not supported.)
    - Block files (stored in Audacity project directories) now
      use the standard AU format.  Though some Audacity
      meta-information is in these files, they can now be
      read by many other popular audio programs as well.
    - Fixed some bugs relating to reading/writing audio
      files with more than 16 bits per sample.
    - Import RAW is functional again, with a simpler GUI
      but support for far more file formats.  The
      autodetection algorithms are much more accurate than
      in 1.0.

  * Audio I/O
    - Completely rewritten audio I/O, with lower latency
      and minimal chance of buffer underruns while
      recording.

  * Resampling
    - Using high quality resampling algorithms, with the
      option of better quality for mixing than for real-time
      playback

    - Preliminary support for Time Tracks, for changing
      playback speed over time.

  * Many more bug fixes and new features

New features in Audacity 1.1.2:
  * User Interface
    - Fixed bug in Windows version, for track menu commands 
	  "Name..." and "Split Stereo Track"/"Make Stereo Track".
  * Effects
    - Nyquist support on Windows (supports plug-ins written 
	  in Nyquist, an interpreted functional language based 
	  on Lisp).

New features in Audacity 1.1.1:

  * User Interface
    - Tooltips appear in Statusbar.
    - Vertical cursor follows play/record
    - Pause button
    - Drawing tool (with three different modes)
    - Vertical Resizing of stereo tracks is more fun.
    - Adjust selection by click-dragging selection boundary
    - Toolbar button context-sensitive enabling/disabling
    - Better zooming functionality (centers region)
    - Multiple ways to display the cursor position and selection
    - Snap-to selection mode
    - Drag tracks up and down
    - Align and group align functions
    - Cursor save/restore
    - Working history window
  * Effects
    - Effects broken down into three menus: Generate, Effect, and
      Analyze
    - Generate menu lets you generate silence, noise, or a tone
    - Nyquist support (supports plug-ins written in Nyquist,
      an interpreted functional language based on Lisp)
  * Localization
    - Improved localization support
    - More languages available
    - Language selection dialog on startup
  * Mac OS X
    - Support for more audio hardware
    - Support for full-duplex (play while recording)
    - Support for MP3 exporting using LameLib Carbon
  * Unix
    - Audacity now has a man page (it describes command-line
      options and how to set the search path)
  * File Formats
    - Uses libsndfile 1.0, which fixes some bugs and
      improves performance
  * Searching for Files:
    - On Windows and Mac OS, Audacity now looks for
      translations in the "Languages" folder and all plug-ins
      in the "Plug-ins" folder, relative to the program.
    - On Unix, Audacity looks for translations in
      <prefix>/share/locale and looks for everything else
      in <prefix>/share/audacity and also in any paths in
      the AUDACITY_PATH environment variable

New features in Audacity 1.1.0:

  * Core audio processing:
    - Support for 24-bit and 32-bit sample formats
    - Automatic real-time resampling (using linear
        interpolation)
  * Effects:
    - Support LADSPA plugins on Linux / Unix
  * File formats:
    - New XML-based Audacity project format
    - Full Ogg Vorbis support now (importing and exporting)
    - Export to any command-line programs on Unix
    - Support for reading and writing many more types of
        uncompressed audio files, including ADPCM WAV files.
  * Toolbars
    - New toolbar drawing code; automatically adopts your
        operating system's colors
    - New toolbar buttons (Skip to Start, Skip to End)
    - New Edit toolbar
    - Toolbar buttons disable when they're not available
  * User Interface
    - Fully customizable keyboard commands
    - Autoscroll while playing or recording
    - New Ruler, used in main view and in
        FFT Filter effect
    - The waveform now displays the average value in a lighter
        color inside the peak values
  * Localization
    - Audacity can now be localized to different foreign
      languages.

New libraries in Audacity 1.1:

  * libmad for fast MP3 importing
  * libid3tag for editing MP3 file information
  * libsndfile to read and write more audio file formats
  * PortAudio for cross-platform audio playing and recording
