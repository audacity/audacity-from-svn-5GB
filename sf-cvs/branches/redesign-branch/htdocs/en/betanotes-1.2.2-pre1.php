<pre>
Changes in 1.2.2-pre1:

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

  * Other minor bug fixes and performance improvements.

</pre>
