<pre>
Changes in 1.2.1:

  * Fix a possible crash when opening or resizing the Equalization
    window, especially when using large system fonts.

  * Don't allow numbers less than -100% in Change Pitch/Speed/Tempo
    effects.  (Fixes a possible crash.)

  * Correctly load ID3 tags saved in Audacity project files.

  * On Linux and OS X, store lockfiles in the temp directory instead of
    the user's home directory.  This fixes problems in computer labs
    where users have restricted or network-mounted home directories.

  * Fix a problem that caused Nyquist effects to fail when using certain
    regional settings.

  * Linux: Include a newer PortAudio v19 snapshot.  (Fixes build
    problems when using the experimental --with-portaudio=v19 option.)

  * Linux: Fix endianness bug in playback on PowerPC.

  * Linux: Fix compilation problems in Nyquist and SoundTouch.

  * Updated translations.

</pre>
