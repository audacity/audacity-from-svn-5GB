<pre>
Changes in 1.2.0-pre4:

  * Fixed problems that could occur when importing certain
    non-seekable PCM audio files, such as GSM610.

  * Fixed bug that was causing the samples to shift off-screen
    horizonally when zoomed in very far and the track had a
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

  * Updated translations.  Added translator credits to localized
    versions of Audacity.

</pre>
