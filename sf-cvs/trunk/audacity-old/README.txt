Audacity: A Free, Cross-Platform Digital Audio Editor

Version 0.9 (pre): September 5, 2000

http://www.cs.cmu.edu/~music/audacity/

Primary author:

  Dominic Mazzoni <dmazzoni+@cs.cmu.edu>

Authors:

  Dominic Mazzoni <dmazzoni+@cs.cmu.edu>
  Roger Dannenberg <rbd+@cs.cmu.edu>
  Jason Cohen <cohen3+@andrew.cmu.edu>
  Robert Leidle <rfl+@andrew.cmu.edu>
  Mark Tomlinson <marktoml@hotmail.com>

For changelog, see the bottom of this document

-------------------------------------------------------------

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public
License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This library is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public
License along with this library (in a file called COPYING.txt); if not,
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

Changes from 0.8 to 0.9:

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

* Rewritten user interface code.  The shades of gray
  are taken from the OS, and the project window has been
  redesigned to have more consistent layout across all
  platforms.

* Selecting "Open" now does the smart thing, opening a
  project if you give it a project, or importing a WAV
  file if you give it that.

* Flashing cursor indicates the current editing position

* Much improved ruler - besides looking nicer, the ruler
  now displays the selection and the cursor

* The zoom tool centers on the cursor so you can zoom
  into wherever you are
