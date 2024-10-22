
Audacity(R): A Free, Cross-Platform Digital Audio Editor
  WWW: http://audacity.sourceforge.net/

We welcome feedback on Audacity, suggestions for new or
improved features, bug reports and patches at:
  feedback@audacityteam.org

Personal support with Audacity is not provided by e-mail,
but on our Forum:
  http://audacityteam.org/forum/

Audacity is copyright (c) 1999-2009 by Audacity Team.
This copyright notice applies to all documents in the
Audacity source code archive, except as otherwise noted
(mostly in the lib-src subdirectories).

The documentation for Audacity is licensed under the Creative Commons license
http://creativecommons.org/licenses/by/3.0/legalcode

"Audacity" is a registered trademark of Dominic Mazzoni.

Version 1.3.8 Beta

Contents of this README:

1.  Licensing
2.  Changes in version 1.3.8 Beta
3.  Known Issues at Release
4.  Source Code, Libraries and Additional Copyright Information
5.  Compilation Instructions
6.  Previous Changes going back to version 1.1.0

--------------------------------------------------------------------------------

1. Licensing

This program is free software; you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation; either version 2 of the License, or (at your
option) any later version. The program source code is also freely
available as per Section 4 of this README.

This program is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
License for more details.

You should have received a copy of the GNU General Public License
along with this program (in a file called LICENSE.txt); if not, go
to http://www.gnu.org/copyleft/gpl.html or write to

  Free Software Foundation, Inc.
  59 Temple Place - Suite 330
  Boston, MA 02111-1307 USA

--------------------------------------------------------------------------------

2. Changes in version 1.3.8 Beta: 

New Features:
           * Effects and Analysis:
              * VST Effects now display GUI by default  
              * Faster Equalization and Noise Removal; improved 
                 Truncate Silence and Click Track
              * Chains applied to files now clear temporary data after 
                 processing each file 
              * Updated Nyquist implementation with support for SAL 
                 syntax and improved memory management 
              * Plot Spectrum now analyzes up to 237.8 seconds of audio,
                 with separate windows for each project and improved 
                 display; new preferences for Spectrograms 
              * Contrast Analysis tool now modeless for easier use    
           * Interface:
              * Draft Manual/Quick Help included in Windows and Mac 
                 installers 
              * New "Mixer Board" view with per-track VU meters 
              * Mute, solo, gain, pan and track height saved in projects
              * More compact Preferences window with easier-to-use Keyboard
                 tab and new toolbars shortcuts   
              * New Screenshot Tools and improved screen reader support  
           * Other:
              * Record more than 16 channels (hardware/drivers permitting)  
              * Improved support for non-mmap ALSA devices such as PulseAudio 
              * 32-bit float data over 0 dB now handled without clipping
              * "Stop" option when importing preserves already imported data
              * AMR NB export now supported if the optional FFmpeg library
                 is installed
              * Faster waveform drawing and better response in multi-track 
                 projects 

Bug fixes for:
              * Export Multiple: failed with no export or warning if empty 
                 label encountered; silenced project and exported files if
                 overwriting imported WAV files without copying the data in   
              * Metadata Editor hidden if it was on a now unavailable second
                 monitor
              * Misaligned audio after "Split New" or Noise Removal effect
              * Incorrect label movement and paste with linked audio and label 
                 tracks      
              * Equalization, Cut Preview and Advanced Mixing Options dialogue
              * (Linux) Mixer Toolbar should now adjust levels and select input
                 sources properly  
              * "Audio cache" preference caused crashes - data is now only
                 cached in memory if available RAM is above a level defined 
                 in preferences 
              * Various other crashes


--------------------------------------------------------------------------------

3. Known Issues at Release

Please also check:
  http://wiki.audacityteam.org/index.php?title=Known_Issues

for details of any issues that have been identified after release of
this version.

 * Some VST plug-ins may have two instances if vst-bridge present.

 * Imports:
    * Non-MP3 files imported via drag or Recent Files will crash
       or freeze when filter in file open window is set to MP3.
    * (Windows) The Audacity 1.3.8 executable cannot be added to 
        the Explorer "Open With" menu if you have another version
        of Audacity also called "audacity.exe". You can set the 
        file association to always use Audacity 1.3.8 to open the 
        required file type, but it will not appear in the list 
        accessed by the "Open With" context menu item.    
              
 * Exports:
    * Metadata Editor appears before the Export window when exporting 
       to any format. Click OK in Metadata Editor to proceed to the 
       Export window. If you do not use metadata, you can go to the 
       Import / Export tab of Preferences and uncheck "Show Metadata
       Editor prior to export step". 
    * WAVEX (Microsoft) headers: GSM 6.10 files cannot be exported, and
       U-Law/A-Law files may not be playable
    * M4A: exports at rates below 44100 Hz have incorrect sample rates,
       and 38000 Hz exports may not play properly (FFmpeg bugs); M4A
       renamed to MOV will not play on Windows in iTunes or QuickTime
    * Muting specific time-shifted mono tracks produces audio at wrong 
       point on timeline in exported file if muted tracks are to left 
       of unmuted.

 * It is currently possible to attempt simultaneous imports or exports
    by using shortcuts (or File > New on Mac): Audacity is not yet
    capable of running these simultaneous operations safely, and
    attempting this may crash your project.

 * Genre WAV info tag (IGNR) not supported due to limitation in
    libsndfile. 

 * Effects and Analysis:
    * Nyquist effects, SoundTouch, SBSMS and Generators join separate 
       clips together.
    * Equalization used in a chain applies the parameters last used 
       via the Effect menu instead of those stored in chain. 
    * LADSPA Multiband EQ (optional download) may not be visible in 
       the Effect menu, or may crash in use.
    * Compressor produces shorter attack/decay times than those specified 
       in the dialogue. 
   
 * Audio generated or pasted when cursor is in white space overwrites
    instead of inserts.

 * If playback scrolls, cursor jumps to start of scroll on stop, hiding
    previously visible audio preceding the playback position.  

 * If shortcut for "Add label at playback position" is an unmodified
    character, confirming the second label with ENTER places that 
    character in a label at the start cursor.

 * Preferences window: OK button does not respond to ENTER when a tab 
    is selected in the left-hand panel.

 * At 120 DPI, importing a second stereo track causes the pre-existing 
    stereo track above to split.

 * On-Demand:
    * does not work if using the optional FFmpeg importer (that is, if
       "FFmpeg-compatible files" set in the import dialogue)
    * when importing a mixture of uncompressed and compressed files,
       the compressed files intentionally load first to minimise UI
       blockage, unless File > Open is used so that the files import 
       into separate projects. This distinction is not yet observed on 
       Windows. On all platforms, multiple files in one project will 
       not necessarily be sorted in file name order after import.  

 * Mixer Board:
    * The slider maximum gain is +6 dB, and its tooltip will not show 
       more than this maximum even if the gain applied on the Track Panel
       slider is higher.     
    * Selecting a track by clicking the Track Panel does not select it on
       the Mixer Board. 
    * The meter range does not reflect a change in the dB range meter 
       preferences until restart. 

 * Not all menu items are correctly enabled when the preference:
    "Select all audio in project, if none selected" is checked

 * A few interface elements do not change correctly after a language 
    change until restart.

 * Calculation of "disk space remains for recording (time)" incorrect
    when recording in 24-bit quality. You may record for 50% longer
    than the indicated time.

 * Pressing Play (but not Space) in a second project when another is
    already playing stops playback of the first project

 * Projects created by Audacity 1.1.x or earlier are no longer
    supported. Export each project track as WAV using the appropriate
    legacy version of Audacity, then import the WAV files into current
    Audacity

 * Audacity can import, display and cut/copy/paste MIDI files, then
    export them, but they cannot be played; undoing an edit with a MIDI
    track open causes the MIDI data to be lost in Windows builds

 * Audacity requires restart to detect audio devices plugged in while 
    it is running.  

 * Intermittently occurring bugs: Please write to feedback@audacityteam.org   
   if you experience any of these known but not fully understood 
   issues, giving us steps to reproduce them so they can be fixed:
 
    * Projects do not reopen properly with "orphaned" or "missing"
       blockfiles or "duplicate attribute" errors
    * Projects do not re-open properly from the Automatic 
       Crash Recovery dialogue
    * Projects crash when applying repeated effects towards
       the end of audio tracks 
    * AIFF files import as noise    
    * (Windows XP, reported on) Clicks during recording 
    * (Windows) Timer Record unreliable with recordings starting
       before and ending after midnight
    * (Linux) After opening a sufficiently long audio file, opening a 
       second file of any size leads to locked GUI/console messages 
       until first file completes play

 * (Windows) On Vista, and on XP with some USB microphones, the system 
    mixer level sliders and the Audacity Mixer Toolbar level sliders 
    act independently. The achieved recorded level only matches the
    level indicated on the Recording VU meter if the Audacity input 
    slider is at 100%. 

 * (Windows) On Vista, input sources such as microphone and line-in 
    must be selected in the Audacity Audio I/O Preferences, not the 
    Mixer Toolbar input selector.   

 * (Windows) Timer Record cannot maintain scheduled duration if system 
    clock changes.

 * (Windows) Audacity is incompatible with some professional sound
    cards and may crash if one of these cards is the default when you
    open Audacity: as a workaround, make a different sound card your
    default when using Audacity, but please let us know if this affects
    you so we can track down and solve the problem.

 * (Mac OS X) Very occasionally, users may find that recording
    causes "error opening sound device", or that after running
    Audacity, other media players don't produce any sound, or crash:
    to resolve this, set up your sound device in Apple Audio MIDI Setup
    to work in stereo, 16-bit, with a sample rate of 44100 Hz or
    48000 Hz, and set the sample format and rate identically in
    Audacity. More help at:
      http://audacityteam.org/forum/viewtopic.php?f=17&t=5064

 * (Mac OS X) If using Audacity when the "Hear" audio plug-in is
    running (or has been since boot), there will be excessive memory
    usage which could cause a crash: appears to be due to buggy
    memory allocation in "Hear".

 * (Linux) Undoing or redoing a label edit may cause a crash if some 
    character strings are common to all labels.    

 * (Linux) Effects and other dialogues do not have focus on opening. 
    This is a bug in wxGTK for which there is no current workaround. 
    Click in the dialogue to navigate it and change parameters.    

 * (Linux) If Portmixer uses emulated playback volume rather than 
    native, the Audacity output slider will affect the VU playback 
    meters and these may not then reflect the actual volume level 
    of the waveform. 

 * (Linux) Audacity will require a FFmpeg package or build later than
    0.5 in order for optional FFmpeg import/export features to work.    

 * (Linux) Audacity now supports interfacing with JACK, but this has
    not been tested, and has a number of known reliability and usability
    issues: patches to improve both will be welcomed

Also note the Windows installer will not replace 1.2.x installations,
but will install alongside them. 


--------------------------------------------------------------------------------

4.  Source Code, Libraries and Additional Copyright Information

Source code to this program is always available; for more information visit
our web site at:

  http://audacity.sourceforge.net/download/

Audacity is built upon other free libraries; some of these libraries may have
come with Audacity in the lib-src directory.  Others you are expected to install
first if you want Audacity to have certain capabilities.  Most of these libraries
are not distributed under the terms of the GPL, but rather some other free,
GPL-compatible license.  Specifically:

  wxWidgets: wxWindows license (based on LGPL)
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

  libflac: Xiph.Org BSD-like licence (the parts we use)
    Decodes and Encodes Free Lossless Audio Codec files. Optional separate
    download.

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

  libvamp: new-style BSD
    Plug-in interface and support library for audio analysis plug-ins.
	Included with Audacity.

  libvorbis: BSD-like license.
    Decodes and encodes Ogg Vorbis files.  Optional
    separate download.

  portsmf: BSD-like license.
    library for reading and writing midi files. Included with Audacity

  sbsms: GPL v2
    Pitch and tempo changing library. Included in Audacity

  SoundTouch: LGPL
    Changes tempo without changing pitch and vice versa.
    Included in audacity

  Twolame: LGPL
    Encodes MPEG I layer 2 audio (used in DVDs and Radio). Optional separate
    download.

For more information, see the documentation inside each library's
source code directory.

--------------------------------------------------------------------------------
Additional copyright information:
--------------------------------------------------------------------------------

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
"COPYRIGHT AND LICENSE INFORMATION FOR XLISP," in the documentation
and/or other materials provided with the distribution.

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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND
CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


--------------------------------------------------------------------------------

5. Compilation instructions

First you must download wxWidgets. Audacity 1.3.8 and later requires
wxWidgets 2.8.10 from:

  http://www.wxWidgets.org/

If you install the RPM, make sure you install the devel RPM as well, otherwise you
won't be able to compile Audacity from source.

To compile on Linux, Mac OS X, and other Unix systems, simply execute these
commands:

  ./configure
  make
  make install  # as root

To see compile-time options you can set, you can type
"./configure --help".

If you want to do any development, you might want to generate a configure cache
and header dependencies:

  ./configure -C
  make dep

To compile on Windows using MSVC++, please follow the instructions found in
compile.txt in the "win" subdirectory.

To compile using Xcode on Mac OS X, see the instructions in mac/compile.txt.

For more information on compilation, please visit:

  http://audacityteam.org/wiki/index.php?title=Developer_Guide#Platform_Specific_Guides

or e-mail us at:

  audacity-devel@lists.sourceforge.net

--------------------------------------------------------------------------------

6.  Previous Changes going back to version 1.1.0

Changes in version 1.3.7 Beta:

Cross-platform Bug Fixes:
        * Muting/soloing caused incorrect channel results in exported
           stereo files
        * Noise Removal and all Nyquist effects pasted the original
           unmodified audio at the end of the modified region
        * Noise Removal inserted a tail of low level noise at the end
           of the modified region
        * Nyquist and Compressor plug-ins did not display moving bars
           in progress dialogue and over-estimated "Remaining Time"
        * Cancelling Nyquist effects deleted unprocessed audio
        * Change Speed and Change Tempo failed to modify the original
           selection length
        * Cut lines invisible
        * Fixed various bugs importing multi-stream files via FFmpeg
        * File > Export as WAV could be corrupted if overwriting
           an imported WAV read direct from the file
        * Export multiple "Other uncompressed files" choice always
           produced 16-bit PCM audio irrespective of chosen options.
        * MP3 export usually produced a 128 kbps constant bit rate file
           irrespective of chosen options; reported length often
           incorrect
        * MP3 ID3 Genre tag misread on import if the genre list in
           Metadata Editor was opened and saved
        * Exported metadata tags in MP3, OGG and FLAC often not seen by
           player software - now substantially improved
        * WMA exports (via FFmpeg)corrupted if metadata tags included
        * Some multi-channel recording devices that previously recorded
           more than two channels no longer did so
        * Generated audio did not fit in window
        * No warning was given when saving an empty project
        * Beep on completing longer process did not work on many
           systems
        * fixed crashes importing lists of files (.LOF), in Meter Toolbar
           and Change Speed

Platform-specific Bug Fixes:
        * Windows Vista: crash opening Preferences with no sound
           devices enabled and connected
        * Mac OS X and Linux:
           * Spurious clipping at start of playback
           * Labels did not accept certain legal characters
           * Shortcuts did not work after running effects
           * Project Rate did not change to respect rate of first
              imported file if that rate was unsupported
        * Mac OS X only:
           * Crash resizing project window
           * Menu items became inactive or visibly corrupted
           * File > Open dialogue did not always work on OS X 10.4
           * Impossible to set independent Command and Control
              shortcuts that shared the same key
           * Freeze importing uncompressed files via On-Demand
              (please report any remaining instances of this to:
               feedback@audacityteam.org)
           * Portable settings were not picked up, instead settings
              were taken from the default location
           * Fixed unavailability of FFmpeg installer

New Features:
           * F11 Full Screen mode
           * High-quality "Sliding Time Scale/Pitch Shift" effect
           * Audio Contrast Analyzer for testing audio on the
              internet for WCAG2 accessibility compliance.
           * Windows: sound devices can now be opened using the
              more efficient DirectSound API

Other changes:
           * Latency correction should be improved for many users
              by employing a fixed rather than variable correction
           * Grouping of Effects into categories turned off until
              a way is added for users to do so themselves
           * Numerous minor interface improvements such as Metadata
              Editor navigation, new "hh:mm:ss + hundredths"
              selection format
           * Note: Windows users wanting to export MP3 files will
              require the latest version of the LAME encoder from
              http://lame.buanzo.com.ar/


Changes in 1.3.6 Beta (since 1.3.6a6 Alpha):

Interface:
        * "Save Compressed Copy of Project" saves in much smaller .OGG
           format to facilitate online transmission of projects
        * Improved MIDI import and export routines, and clearer color
           for selection region
        * Default temporary directory on Mac now accessible in Finder

Import / Export:
	* Stability improvements in on-demand loading
        * FFmpeg: support for latest version of library, improved
           version checks and error messages, stability improvements
           in custom exporter

Bug Fixes:
        * Crash in "Get Noise Profile" step of Noise Removal at project
           rates below 20480 Hz.
        * Underestimation of peak level in tracks with a small number
           of different peaks
        * Truncate Silence could result in repeated or lost audio if
           applied to the whole of a track
        * Other interface, generating, exporting and platform-specific
           fixes

Compilation:
        * Added autoconf macro archive to CVS, enabling *.nix users
           without this archive to build --with -MIDI


Changes in 1.3.6a6:

Interface:
        * Note Track now supports export as a MIDI file
        * Linked audio and label tracks: improved support when source
           and target number of tracks differ and when cross-pasting
           different track types

Import / Export:
        * On-demand now supports project saving during summarising;
           reverts to stripey background; fixed some crashes due to
           threading issues
        * Exports: Single AAC filter (M4A LC profile) with quality
           settings slider; removed FFmpeg formats already supported
           by Audacity; added explicit GSM 6.10 (WAV) filter; current
           project rate now used for all exports, with check for
           format-invalid rates; improvements to metadata support

Effects:
        * LV2 plug-ins: added support (OS X and Linux only) for using
           synths as tone generators, scale point labels, grouped
           controls and i18n translations


Changes in 1.3.6a5:

Interface:
        * Note Track now builds on Windows
        * Fixes/improvements for linked audio and label tracks (one
           desynchronisation bug remains when pasting audio into a
           greater number of tracks than were copied); now supports
           label shifting when changing pitch and tempo
        * Added full label pasting support: now possible to paste
           multiple labels, region labels and labels with audio, and
           correct label text is now pasted

Import / Export:
        * Added full "on-demand" support (now with minimum file
           length of 30 seconds): clicking moves summary calculation
           point; supports split and merge of stereo tracks;
           incompletely summarised tracks resume summary calculation
           automatically; text-based Status Bar progress indication and
           animated dummy waveform instead of embedded progress bar


Effects:
        * Fixed a bug where previewing Equalization curves more
           than once previewed the unmodified audio
        * Improvements to DTMF generator

Miscellaneous:
        * Improved support for working with audio in excess of 2^31
           samples (about 13.5 hours at 44100 Hz); some accessibility
           improvements


Changes in 1.3.6a4:

Interface:
        * New Preference: Default View Mode, to choose type of
           waveform, spectrum or pitch view for new tracks
        * Note Track: experimental support is now enabled by defining
           USE_MIDI in config*, but does not build out-of-the-box
           on Windows
        * Bug fixes for linked audio and label tracks; now supports
           label shifting when changing speed and generating tones

Import / Export:
        * Improvements/fixes for AAC exports including new M4A
           filter for compatibility with iTunes; RealAudio export
           support removed - FFmpeg does not support this properly
        * Improved refresh of on-demand loading; fixed a phantom
           on-demand progress bar when time-shifting clips

Effects:
        * Experimental support for LV2 plug-in architecture on Linux
           and Mac, but operation may be buggy; no LV2 support yet on
           Windows, because the required slv2 library currently does
           not build


Changes in 1.3.6a3:

Import / Export:
        * Experimental support for exporting a much wider range
           of proprietary audio formats via FFmpeg
        * "On-demand" immediate loading of imported PCM WAV or
           AIFF files now has experimental "progress bar" embedded in
           the waveform until fully loaded

Interface:
        * Note Track: experimental support for cut, copy and paste
           using Edit Toolbar; currently not available for Linux, where
           EXPERIMENTAL_NOTE_TRACK must be undefined in order
           to build
        * New Transport menu for alternative access to play and record
           commands and some recording preferences
        * Audio tracks are now linked to label tracks by being positioned
           above a label track, if linkage is enabled in the Tracks menu


Changes in 1.3.6a2:

Import / Export:
        * Experimental support for importing a much wider range
           of audio formats via FFmpeg: support has to be enabled
           in *config when building and requires FFmpeg libraries
        * Experimental support for "on-demand" immediate loading
           of imported PCM WAV or AIFF files (full waveform continues
           to load while you play or edit).

Effects:
        * Built-in plug-ins now grouped into related hierarchical
           categories

Interface:
        * New Debug Log window available in all builds
        * Experimental support for linking a label track with any
           number of audio tracks so that labels shift with cuts and
           inserts in the audio track
        * Default theme now reverted to that of 1.3.5
        * Recording channels preference now defaults to stereo

Miscellaneous:
        * Bug fixes for shortcut availability/tab order in Selection Bar,
           and for window focus issues when previewing effects
        * Improvements in escaping and navigating fields in dialogs,
           and in stability when screen readers are used


Changes in 1.3.6a1:

Interface:
	* Further improvements to menu navigation and wordings.
	* All file dialogs are now resizable, and support "Places"
	   sidebar on Windows 2000 or later.
	* Preferences:
	        * New "Theme" preference for modifying interface
	           colours and images, with experimental new default
	           colour scheme.
	        * New "Smart Recording" preference automatically pauses
	           recordings that fall below a pre-defined input level.

Compilation:
        * Simplified modular builds for Windows, removing
           static-linked configurations.
        * New shared configurations on Mac to support modular
           builds, and all builds are now Unicode.

Miscellaneous:
        * Default auto save interval reduced to 2 minutes.
        * Bug fixes to correct project rate initialisation on Linux, and
           file importing issues on PPC Macs.


Changes in 1.3.5:

Recording  / Playback:
	* Several bugs fixed so that latency correction should be better, and more
	   devices work correctly. Problems with invalid sample rates under Linux
	   should be much rarer.
	* Newer version of Portaudio library.
	* New feature to record onto the end of an existing track
	   (hold Shift while clicking Record).

Import / Export:
	* Updated versions of Libogg, Libvorbis, Libflac, Libsndfile and Twolame
	   libraries.
	* Handling of unsupported file formats more informative.
	* Handling of file names with slashes on OS X improved. New dialog
	   allows replacement of illegal file name characters on all platforms.

Interface:
	* Improved scaling and layout for rulers and VU meters.
	* Envelope fixes/improvements including full control of undo/redo.
	* New keyboard shortcuts and improved menu navigation.
	* Preferences: More intuitive tab arrangement. New options for
	   mute/solo and Metadata Editor behavior. Language can now be
	   changed without restart.
	* Expanded Build Information tab.

Effects:
	* New Vocal Remover plug-in, improvements for Generate effects.

Compilation:
	* Fixes when building Audacity with libraries disabled.
	* Improvements to make Mac and Solaris builds easier.

Security:
	* Full fix for issue CVE-2007-6061 on systems where temporary directories
	   can be changed by other users (thanks to Michael Schwendt).

Miscellaneous:
	* Updated translations for many locales.
	* Several stability improvements.


Changes in 1.3.4:

New Features
	* New Welcome Screen with introduction to Audacity
	* Enhanced Windows Shell integration, so Audacity shows up in lots of
		Windows places such as "Open With".
	* New keyboard command: 'Mix and Render to New Track'
		(bound to Ctrl+Shift+M).
	* New keyboard shortcut: "Shift-A" starts playback when stopped,
		or performs "Stop and Select" when playing.
	* Added support for VAMP audio analysis plug-ins.
	* Solo button solos only one track at a time, and a track cannot be both
		mute and solo.

Interface:
	* Keyboard shortcuts for making short/long jumps along the timeline.
	* Added 'Snap To' in the Selection Bar.
	* Made keyboard navigation easier when multiple menu items with the
		same first letter exist.
	* Enhanced interface for label editing.
	* Layout of OK/Cancel buttons consistency improved.
	* Preferences:
		* "Select all audio in project, if none selected" (on by default)
		* "Beep on completion of longer activities" (system bell, not
			main output).
		* Other preferences cleaned up and explanations improved.
	* Envelopes: Many fixes when copying / pasting / repeating.
	* Many translation updates.
	* Track height fixed in several cases.
	* CleanSpeech mode switching without closing and re-opening fixed.

Opening/saving formats:
	* Metadata editor added for OGG, FLAC and WAV/AIFF exports, and
		general improvements in this area.
	* Import of metadata improved.
	* Muted tracks are no longer audible in the exported mix.

Effects:
	* Truncate Silence: support for multiple and stereo tracks.
	* Dtmf Generator:
		* added support for keypad letters
		* added an amplitude control.
	* Compressor: variable decay time added.
	* Equalization:
		* Clicks at start / end prevented
		* Improvements to saved curves being found
		* Preview works correctly
	* 'Merge' command appears in Undo history.
	* Clipping detected more reliably.
	* Nyquist plug-ins reviewed and enhanced.
	* Better (and more) progress bars.
	* Cancelling effect always restores previous audio.
	* Several improvement to effects in batch mode.

Recording / Playback:
	* Improvements to latency correction.
	* Updated version of portaudio-v19 library.

Note that Help is no longer built in, but accessible on the Web via links
in Audacity.


Changes in 1.3.3:

Opening/saving formats:
   * Import
      * Import of audio from QuickTime (mov, aac, m4a) files is now
        supported on OS X.
      * Broadcast Wave Format (BWF) wave files can now be imported.
   * Export
      * Metadata can be added to OGG files
      * Improved export option selection
      * Additional export options added to MP3 and FLAC file formats
      * Command line exporter now supported on Windows and OS X

Effects:
   * EQ effect
      * Responsiveness improved.
      * Several enhancements added.
      * Batch support added.
   * New Auto Duck effect
   * Added previewing to AudioUnit effects
   * Much improved Noise Removal effect
   * Effects previewing can now be canceled
   * New DTMF Tone Generator effect
   * Additional options available in Noise effect
   * Improved the Tone Generation effects

Other features:
   * Major speed improvement in Spectrogram rendering
   * Increased support for drag and drop on OS X
   * Support added for building against wxWidgets 2.8
   * Support opening multiple Audacity Projects at once from Explorer on
     Windows
   * Improved main window sliders
   * New support for snapping while selecting and sliding
   * Improved track focus handling and visual feedback
   * Speed improvements and handling of resizing/zooming in tracks
   * Spectrum view can now be zoomed.
   * New internal file cache to improve handling of project files over
     networks

Also:
   * Many improvements to language specific translations
   * Numerous stability improvements


Changes in 1.3.1 and 1.3.2:

o Improved accessibility for the visually impaired
      + Improvements for screen readers, accessibility of
        tracks, and hot keys
o Usability improvements
      + New selection bar
      + New features for label tracks
      + Improved toolbar docking flexibility
      + Menu renaming and reorganization
      + Selection, ruler, and playback control improvements
o Auto-save and automatic crash recovery
o Many bug fixes and stability improvements
o Major improvements to some built-in effects (Repair, Equalization)
  and fixes to others
o New features and bug fixes for Nyquist
o Restructured Preferences dialog
o Improved batch processing
o File format export improvements
o Timer recording
o Intel Mac support


Changes in 1.3.0:

   New features
   The new features in Audacity 1.3 have been grouped into the
   following six major categories.

   1. Collapse/Expand Tracks
      In Audacity 1.3, every track has an upward-pointing triangle at
      the bottom of the label area on the left side of the track.

   2. Multiple clips per track
      In Audacity 1.2, there is one audio 'clip' per track. There is
      no easy way to time-shift part of a track without moving the
      rest. In Audacity 1.3, you can split a single track into multiple
      clips. You can move these clips around between different tracks,
      making it easy to construct complex compositions out of hundreds
      of smaller audio samples.

   3. Selection Bar
      In Audacity 1.2, the current selection is contained in a
      status bar at the bottom of the window. In Audacity 1.3,
      this is replaced by a fully functional Selection Bar, which
      displays and controls the current selection (your choice of
      Start and End, or Start and Length), and the current audio
      position. The selection bar is fully editable - just click
      in any field and type to change the current selection precisely.
      In addition, many formatting options allow you to view times in
      different units, such as samples, CD frames, or NTSC video frames.

   4. Improved Label Tracks
      Label Tracks are Audacity's way for you to create markings 3
      and annotations within your project. In Audacity 1.3, Label
      Tracks are much improved, with support for overlapping labels,
      and support for modifying both the left and right edge of the
      label region just by clicking and dragging.

   5. QuickTime and Audio Units on Mac OS X
      On Mac OS X, Audacity can now import and audio file supported
      by Apple's QuickTime technology. This includes .MOV and .MP4
      (AAC) files. Unfortunately encrypted audio files (such as
      those from the iTunes Music Store) cannot be imported directly
      into Audacity - Apple does not allow this to be done easily
      because it would be too easy to circumvent the encryption this way.

      Also on Mac OS X, Audacity now supports Audio Unit plug-ins.
      Audacity searches for Audio Units in the usual location, in the
      system or user's Library folder.

   6. Other features
      Better performance with large projects

      Project integrity check on open

      Transcription toolbar

      Upload

      Batch

      Cut lines

      CleanSpeech


Changes in 1.2.4:

  * The File menu now includes a list of recent files.

  * The "Generate Silence" effect now prompts for a length.

  * Audacity is now built with Vorbis 1.1, which features better encoding
    quality and file compression.

  * Dragging sound files into the Audacity window now works on Mac OS X
    and Linux, as well as Windows.  (Before, it worked only on Windows.)

  * The "View History" window can now discard old undo levels to save disk
    space on Windows.  (This previously worked only on Linux and Mac.)

  * "Preferences" command is now in Edit menu.

  * "Plot Spectrum" command is now in Analyze menu.

  * Opening a project file saved by a later version of Audacity displays
    an intelligent error message.  Also, trying to import a project file
    (instead of open it) displays an intelligent error message.

  * Audacity now compiles in Visual C++ .NET 2003.

  * Other minor bug fixes.

  * New or updated translations: Arabic (ar), Czech (cs), Finnish (fi),
    Hungarian (hu), Japanese (ja), Norwegian (nb), Slovenian (sl),
    Simplified Chinese (zh_CN), Traditional Chinese (zh_TW).


Changes in 1.2.3:

  * Fixed a bug that caused recording to stop or display incorrectly
    after about 50 minutes on some Windows systems.  (This was partly
    fixed in Audacity 1.2.2, but still didn't work on some systems.)

  * The Change Pitch and Change Tempo effects have been upgraded to
    use a new version of the SoundTouch library by Olli Parviainen,
    with better speed and higher quality.

  * libsndfile has been upgraded to version 1.0.11.

  * Fixed a bug that caused the program to run slowly when using the
    Envelope tool.

  * Shift-clicking on a mute or solo button now un-mutes (or un-solos)
    all other tracks.

  * Nyquist plug-ins can now accept strings as input.  Also, a "Debug"
    button has been added to Nyquist effect dialogs, which allows you
    to see all of the output produced by Nyquist, for aid in debugging.

  * When the audio file referenced ("aliased") by an Audacity project is
    missing, Audacity will now always play silence.  Before, Audacity
    would sometimes repeat the most recent audio that was played previously.

  * VU Meters will now always reset when audio I/O has stopped.

  * Fixed a major Mac-only bug that was causing Audacity to crash at seemingly
    random times, but especially during audio playback and recording.

  * New or updated translations: Italian (it), Hungarian (hu),
    Ukrainian (uk), Spanish (es). Polish (pl), Simplified Chinese (zh),
    Norsk-Bokmal (nb), French (fr).


Changes in 1.2.2:

  * VU Meters added for both playback and recording.  Click on
    the recording meter to monitor the input without recording.

  * Export Multiple - new feature that lets you export multiple
    files at once, either by track, or split based on labels.

  * Attempt to automatically correct latency in full-duplex recordings.
    (This does not work perfectly, and is not yet supported on all
    systems.  It will improve in future versions.)

  * Fixed a serious bug that could cause data loss when you save and
    then reload and re-edit an Audacity project containing repeated
    or duplicate data.

  * MP3 tags dialog will only pop up the first time you export as
    MP3; after that it will not pop up again as long as you have
    filled in at least one tag.

  * You can now add a label at the current playback position - in
    the Project menu, with a shortcut of Ctrl+M.

  * Clicking on a label now selects all of the tracks, making it
    easier to use the label track to recall selections.

  * Windows: Fixed a crash in the Time Track "Set Rate" command.

  * Fixed a bug that caused problems with recordings over 45 minutes
    on some Windows systems.

  * Mac OS X: Improved support for the Griffin iMic by fixing a bug
    that was causing it to always record in mono instead of stereo.

  * Added support for Software Playthrough (listen to what you're
    recording while recording it, or while monitoring using a VU
    meter) - this makes it possible, for example, to record using one
    audio device while listening to it play through a separate device.

  * Unix/Linux: Fixed freeze caused by captured mouse when audio
    device hangs.  (Audacity may not respond, but it will no longer
    freeze all of X.)

  * Fixed a cosmetic bug that caused improper waveform display if
    you tried to open an Audacity project saved on a different
    platform (e.g., copying a project from a Mac to a PC).

  * Fixed bug that could cause instability when pasting, splitting,
    or duplicating a label track.

  * You can now change the font of a label track by choosing "Font..."
    from the label track's pop-up menu.

  * Basic printing support has been added.  Currently it scales the
    entire project to fit exactly on one page.  Try printing in
    landscape orientation for best results.

  * Mac OS X and Windows: Audacity ships with a newer version (1.0.1)
    of the Ogg Vorbis encoder.  Vorbis compression will now have higher
    quality and smaller file sizes.

  * Fix a bug that occasionally caused crashes when applying effects
    to split tracks.

  * Zoom In / Zoom Out now properly disable when they're not available.

  * Fixed disk memory leak in Preview

  * Other minor bug fixes and performance improvements.


Changes in 1.2.1:

  * The following translations have been added or updated:  Finnish,
    French, Hungarian, Italian, Japanese, Norwegian, Polish, Russian.

  * Fix a bug that could cause data to be lost when pasting audio
    from one project into another, after the first project has been
    saved and closed.

  * Fix a possible crash when opening or resizing the Equalization
    window, especially when using large system fonts.

  * Don't allow percentages less than -100% in Change Pitch/Speed/Tempo
    effects (fixes a possible crash).

  * Fix a crash when the temporary directory is not available on startup.

  * Correctly load ID3 tags saved in Audacity project files.

  * On Linux and OS X, store lockfiles in the temp directory instead of
    the user's home directory.  This fixes problems in lab environments
    where users have restricted or network-mounted home directories.

  * Fix a bug that prevented Nyquist effects from running when certain
    regional settings were activated.

  * Fix a bug in the Quick Mix command that could cause old temporary
    files to not be deleted.

  * Linux: Fix endianness problems in playback on PowerPC.

  * Linux: Fix compilation problem in Nyquist on MIPS.

  * Linux: Include a more recent PortAudio v19 snapshot (fixes compilation
    problems when building with the --with-portaudio=v19 option).

  * Two new Nyquist plug-ins: "Cross Fade In" and "Cross Fade Out."

  * Other minor bug-fixes.


Changes in 1.2.0:

  * New cross-fade effects.

  * Fix problem where samples were drawn in the wrong position
    when zoomed all the way in.  This caused the drawing tool
    to move a different sample than the one under the cursor.

  * Don't use id3v2.4 tags, which are not yet supported by
    most players.  (This was fixed in 1.2.0-pre2, but appeared
    again by accident in 1.2.0-pre3.)

  * Correctly display translated messages in the status bar.

  * When the cursor is on-screen, the Zoom In button now zooms
    to the area around the cursor.

  * Mac OS X: Fixed audio problems on the Apple PowerMac G5.

  * Linux/ALSA: Work around a bug in ALSA's OSS emulation that
    caused Audacity's playback cursor to move too quickly.

  * Microsoft Windows: The Audacity source code should now
    compile out of the box on Windows.

  * Many new/updated translations.


Changes in 1.2.0-pre4:

  * Fixed problems that could occur when importing certain
    non-seekable PCM audio files, such as GSM610.

  * Fixed bug that was causing the samples to shift off-screen
    horizontally when zoomed in very far and the track had a
    time-shift offset.

  * Fixed bugs in the new resampler that added noise to resampled
    audio on some systems. If you experienced noise when exporting
    to a WAV, MP3 or OGG file you may have been bitten by this bug.

  * Fixed bug that led to occasional crashes when using the
    time-shift tool in conjunction with high zoom factors.

  * Dithering is now only applied on export when it is really
    necessary (e.g. when converting float samples to 16-bit).

  * Files that only contain mono tracks are now automatically
    exported to stereo files when they contain tracks which are
    panned to the left or the right.

  * The Delete key can now be used to delete the current selection,
    in addition to the Backspace key.

  * Fixed bug where Audacity didn't ask whether to save
    changes if you close the project or exit while recording.

  * Mac OS X: Supports Playthrough (listen to what you're recording
    while recording it) if your hardware device supports it.

  * Mac OS X: Audacity is now a package (you can right-click on
    Audacity.app and select 'Show Package Contents').  Launch time
    has improved significantly.

  * MS Windows: Fixed problem that caused Windows XP to use
    the short name of a file ("TESTFI~1.AUP"), which led to
    problems when the file was later opened again using the
    long file name.

  * MS Windows: Fixed bug that caused file exports to fail
    if the destination directory was the root folder of a
    Windows drive.

  * MS Windows: Audacity's application information which
    is written to the Windows registry now always contains
    the full path to the executable.

  * MS Windows: Fixed problems in trying to set the Windows
    registry as non-admin user, for file-type associations.

  * Make sure the "Save" command is enabled after changing
    gain and pan sliders.

  * Updated translations.  Added translator credits to the
    "About" window in localized versions.


Changes in 1.2.0-pre3:

  * Fixed bug where Export is grayed out when nothing is
    selected.

  * Fixed crash caused by opening Audacity on a computer with
    a high-end sound card with no mixer support.

  * Fixed crash in Import Raw.

  * Fixed New Stereo Track.

  * Cosmetic fixes for Mac OS X.

  * Support for the VST Enabler on Windows added.

  * Fixed crash if you close Audacity while the Preferences
    dialog is open.

  * Fixed duplicate-character bug in Mac OS X Label Tracks.

  * The recording level control on Linux now adjusts the IGAIN,
    rather than the playthrough level of the recording source.

  * Fixed bug that caused corruption to 16-bit stereo recordings.

  * Fixed bug that caused data loss if you deleted all tracks in
    a saved project and then open a new file into the same window.

  * Added support for alternate audio button order (in Interface
    preferences)

  * Added preliminary support for wxX11

  * Added fully transparent Windows XP icon

  * Fixed crash if you try to record (or play) and no audio
    devices exist, or if the audio device doesn't support the
    mode you selected.

  * Audacity no longer sets the process priority to high while
    recording on Windows.  Users can still do this manually
    using the Task Manager.

  * Fixed bug that caused last ~100 ms of the selection to get
    cut off on Windows.

  * Fixed FFT Filter and Equalization effects dialogs.

  * Fixed bugs in Unix build system (DESTDIR in locale directory,
    choosing libsamplerate instead of libresample)

  * Support for LADSPA plug-ins on Windows added, and
    three open source LADSPA plug-ins ported to Windows
    (GVerb reverb, SC4 compressor, and Hard Limiter)


Changes in 1.2.0-pre2:

  * Online help completed.  The full manual is nearly complete
    and will be posted to the web site for online browsing shortly.

  * Audacity will no longer let you do unsafe editing operations
    while playing or recording.  This eliminates many potential
    crashes.

  * Fixed ability to cancel Quit button.

  * New resampling library, with no restrictions on the maximum or
    minimum rate of resampling.

  * Audacity now supports LADSPA plug-ins on all platforms, and
    supports VST plug-ins through an optional LADSPA plug-in
    called the "VST Enabler", which you can download separately.
    Because of licensing issues, Audacity cannot be distributed
    with VST support built-in.

  * Mac OS X keyboard shortcut problems have been fixed.

  * Mac OS X audio muting problems have been fixed.

  * Mac OS X playback/recording cursor sync problems have been fixed.

  * Silence now displays a straight line again, instead of nothing.

  * Added a vertical ruler to the Waveform dB display.

  * Fixed crash in Change Pitch.

  * You can now Paste if nothing is selected.

  * Canceling an Import operation doesn't cause an extra error
    dialog to appear.

  * Audacity now handles filenames with international characters
    correctly.

  * Now outputs ID3v2.3 tags (instead of ID3v2.4), to be
    compatible with more MP3 players.

  * Minor improvements to build system on Unix systems.


New features in Audacity 1.2:
  * User Interface
    - Vertical zooming of tracks.
    - Improved look and placement of toolbars.
    - New custom mouse cursors.
    - Complete implementation of editable keyboard shortcuts.
    - Find zero-crossings.
    - Mouse wheel can be used to zoom in and out.
    - Multi-Tool mode.
    - Amplify using envelope.
    - Labels can store selections (like Audacity 1.0.0).

  * Effects
    - Repeat Last Effect command
    - Improved VST plug-in support
    - Most effects now have a Preview button
    - Compressor (Dynamic Range Compressor)
    - Change Pitch (without changing tempo)
    - Change Tempo (without changing pitch)
    - Change Speed (changing both pitch and tempo)
    - Repeat (useful for creating loops)
    - Normalize (adjust volume and DC bias)

  * Audio I/O
    - 1-second preview command.
    - Looped play.

  * File I/O
    - Audacity 1.2.0 opens project files from all previous versions
      of Audacity from 0.98 through 1.1.3.
    - Open multiple files from the same dialog.
    - Use a text file to specify a list of audio files to open with offsets.

  * Updated user manual

  * Bug fixes
    - Project files with special characters are no longer invalid.
    - "Scratchy" noises caused by bad clipping are fixed.
    - Audacity no longer exports invalid Ogg files, and does not cut off the
      last few seconds of exported Ogg files.
    - Mono MP3 files now export at the correct speed.
    - Many incorrect results from the Envelope tool have been fixed.
    - The "Export Labels" command now overwrites existing files correctly.
    - The "Plot Spectrum" window displays the correct octave numbers for
      notes.
    - Several memory leaks are fixed.


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
    - Tooltips appear in Status Bar.
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
    - Support LADSPA plug-ins on Linux / Unix
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
