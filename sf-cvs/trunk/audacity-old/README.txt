Audacity: A Free, Cross-Platform Digital Audio Editor

Version 0.94: January 15, 2001

http://www.cs.cmu.edu/~music/audacity/

Primary author:

  Dominic Mazzoni <dmazzoni+@cs.cmu.edu>

Authors:

  Dominic Mazzoni <dmazzoni+@cs.cmu.edu>
  Roger Dannenberg <rbd+@cs.cmu.edu>
  Jason Cohen <cohen3+@andrew.cmu.edu>
  Robert Leidle <rfl+@andrew.cmu.edu>
  Mark Tomlinson <marktoml@hotmail.com>
  Joshua Haberman <joshua@haberman.com>

For changelog, see the bottom of this document

-------------------------------------------------------------

With the exception of the Xaudio library for importing MP3 files,
this program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

The Xaudio library is commercial software and has been licensed
for use in this program.  For more information, see their
website at http://www.xaudio.com/

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this program (in a file called LICENSE.txt); if not,
go to http://www.gnu.org/copyleft/gpl.html or write to the Free
Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

-------------------------------------------------------------

Source code to this program is always available; for more
information visit our website at:

  http://www.cs.cmu.edu/~music/audacity/

This program uses wxWindows, a cross-platform GUI toolkit.  To
compile this program, you will need to download wxWindows from:

  http://www.wxwindows.org/

To compile under Linux, simply run:

  ./configure
  make

If you want to do any development, you might want to generate
dependencies:

  make dep

For more information on compilation on other platforms (VC++
for Windows and CodeWarrior for Mac are supported) please
email Dominic Mazzoni <dmazzoni+@cs.cmu.edu>.

-------------------------------------------------------------

Known problems:

* Windows: The floating tool palette does not minimize when
  minimizing a project window.  As a workaround, you can
  just keep the tool palette docked with the project window.

-------------------------------------------------------------

Changes in 0.94:

* Preferences dialog (Joshua Haberman)

* OGG Vorbis import (Joshua Haberman)

* Silence, Insert Silence commands

* Split and Duplicate commands

* Mac OS X support

* Supports recording on Mac OS 8 and 9

* Many bug fixes

Changes in 0.93:

* Displays playback/recording position indicator

* Keeps track of some preferences

* Supports arbitrary project sample rate

* Mac: opens documents from the Finder

* Floating tool palette is now dockable
  (and docked by default)

* Fixed bugs in handling multiple open projects

* Supports recording (Windows, Linux)

* Frequency Window displays note names (i.e. C4, G#5)

* Many bug fixes for effects, including VST plug-in effects

Changes in 0.92:

* Added Frequency Plot window and improved Spectrum display

* Fixed bug in File:Open when the file to be opened was
  actually a large WAV file

Changes in 0.91:

* Uses xaudio library to import mp3 files

* Zoom menu

Changes in 0.9:

* New floating tool palette with four tools (selection,
  sliding, zooming, and envelope editing) plus play and
  stop buttons

* Playback now mixes tracks, and you can work with the
  document while listening.  The stop button works.

* Rewritten file handling functions.  The main view
  is no longer dependent on the wxWindows DocView
  classes, so we can handle files ourselves.  The
  project file format is now text-based for easy
  debugging.  Eventually it will probably move to XML.

* Improved handling of wave tracks: as before, the data
  is stored in blocks, but now, the blocks are correctly
  limited to betweek n and 2n bytes each (for some n),
  which guarantees editing operations always take the
  same amount of time, while also ensuring that projects
  don't get more fragmented over time.

* Rewritten user interface code.  The shades of gray
  are taken from the OS, and the project window has been
  redesigned to have more consistent layout across all
  platforms.

* Selecting "Open" now does the smart thing, opening a
  project if you give it a project, or importing a WAV
  file if you give it that.

* Flashing cursor indicates the current editing position

* Much improved ruler - besides looking nicer, the ruler
  now displays the selection and the cursor.

* The zoom tool centers on the cursor so you can zoom
  into wherever you are.

