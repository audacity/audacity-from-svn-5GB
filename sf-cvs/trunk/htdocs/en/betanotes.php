<?php BoxTop("$releaseNotesStr $betaVersion"); ?>

<pre>
Known issues in Audacity 1.1.3:
  - It places a file called "Audacity-Commands.xml" in your home directory
    or somewhere else on your computer.  This file is necessary for
    Audacity 1.1.3 to run.  We apologize for cluttering up your computer
    and promise to put the file in a nicer place in the next version.

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

Known bugs in Audacity 1.1.1:

  * Do not run the Benchmark command on Windows - it may crash.
    It is not a useful feature for most users - it is intended
    for developers to judge the speed of the program on different
    computers and operating systems.

  * Mac OS X: some audio may be lost if you hold the mouse button
    for too long over a menu while recording.  Try to leave Audacity
    alone while recording for best results.

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
</pre>

<?php BoxBottom(); ?>
