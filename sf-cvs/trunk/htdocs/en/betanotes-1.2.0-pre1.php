<pre>
Changes in 1.2.0-pre2:

  * Online help completed.  The full manual is nearly complete
    and will be posted to the website for online browsing shortly.

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

  * Minor improvements to build system on Unix systems.

New features in Audacity 1.2.0-pre1:

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
    - Repeat Last Effect command.
    - Improved VST plug-in support.
    - Most effects now have a Preview button.
    - Compressor (Dynamic Range Compressor).
    - Change Pitch (without changing tempo).
    - Change Tempo (without changing pitch).
    - Change Speed (changing both pitch and tempo).
    - Repeat (useful for creating loops).
    - Normalize (adjust volume and DC bias).

  * Audio I/O
    - 1-second preview command.
    - Looped play.

  * File I/O
    - Audacity 1.2.0 opens project files from all previous versions
      of Audacity from 0.98 through 1.1.3.
    - Open multiple files from the same dialog.
    - Use a text file to specify a list of audio files to open with
      offsets.

  * Updated user manual.


Bug fixes in Audacity 1.2.0-pre1

  * Project files with special characters are no longer invalid.

  * "Scratchy" noises caused by bad clipping are fixed.

  * Audacity no longer exports invalid Ogg files, and does not cut off
    the last few seconds of exported Ogg files.

  * Mono MP3 files now export at the correct speed.

  * Many incorrect results from the Envelope tool have been fixed.

  * The "Export Labels" command now overwrites existing files correctly.
  
  * The "Plot Spectrum" window displays correct octave numbers for notes.

  * Several memory leaks are fixed.

  * Many other minor bugfixes.
</pre>
