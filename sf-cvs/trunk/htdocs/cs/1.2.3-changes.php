<h2>Zmìny v Audacity 1.2.3</h2>
<ul>
<li><p>
		Windows: Fixed a bug that caused recording to stop or display incorrectly
		after about 50 minutes, on some Windows systems.  (This was partly
    fixed in Audacity 1.2.2, but still didn't work on some systems.)
</p></li>
<li><p>
		Mac OS X: Fixed a major bug that caused Audacity to crash at
		seemingly-random times on Mac systems, especially during playback or
		recording.
</p></li>
<li><p>
    The Change Pitch and Change Tempo effects have been upgraded to
    use a new version of the SoundTouch library by Olli Parviainen,
    with better speed and higher quality.
</p></li>
<li><p>
    libsndfile has been upgraded to version 1.0.11.  Audacity can now
		import Sound Designer II (SDII) files.
</p></li>
<li><p>
    Fixed a bug that caused the program to run slowly when using the
    Envelope tool.
</p></li>
<li><p>
    Shift-clicking on a mute or solo button now un-mutes (or un-solos)
    all other tracks.
</p></li>
<li><p>
    Nyquist plug-ins can now accept strings as input.  Also, a "Debug"
    button has been added to Nyquist effect dialogs, which allows you
    to see all of the output produced by Nyquist, for aid in debugging.
</p></li>
<li><p>
    When the audio file referenced ("aliased") by an Audacity project is
    missing, Audacity will now always play silence.  Before, Audacity
    would sometimes repeat the most recent audio that was played previously.
</p></li>
<li><p>
    VU Meters will now always reset when audio I/O has stopped.
</p></li>
<li><p>
    New or updated translations: Italian (it), Hungarian (hu),
    Ukrainian (uk), Spanish (es). Polish (pl), Simplified Chinese (zh),
    Norsk-Bokmal (nb), French (fr), Russian (ru).
</p></li>
</ul>
