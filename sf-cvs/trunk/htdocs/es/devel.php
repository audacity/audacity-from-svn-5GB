<?php BoxTop($develNewsStr); ?>

<p>
Esta pagina va a convertirse en un lugar donde podrás ver en que andan todos los desarrolladores.  Mientras tanto, aquí encontraras un poco de información sobre Audacity 1.1, que es la versión que actualmente esta en "beta".
</p>

<p>
<b>New features in version 1.1:</b>
<pre>
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
</pre>
</p>

<p>
<b>Table of libraries we depend on (version 1.1.0 and beyond):</b>
<table border=0 cellpadding=8 cellspacing=2>
<tr>
<th>Library</th>
<th>Purpose</th>
</tr>
<tr>
<td bgcolor="#ccccff"
><a href="http://wxwidgets.org/">wxWidgets</a>
<td bgcolor="#ccccff"
>This is a cross-platform library which allows our GUI (menus, buttons,
    windows, drawing, etc.) to run natively on Mac, Windows, and Unix
    systems.  wxWidgets also provides other useful C++ classes, and
    Audacity is 100% dependent on this library.  We highly recommend
    using this library if you want to do cross-platform development.
</tr>

<tr>
<td bgcolor="#ccccff"
><a href="http://www.mars.org/home/rob/proj/mpeg/">libmad</a>
<td bgcolor="#ccccff"
><p>
    MAD stands for MPEG Audio Decoder.  This is the one of the
    few MP3 decoders which is free (GPL), and the only one we
    are aware of which uses only integer math and is capable of
    producing 24-bit output (which we don't yet take advantage of).
    Even though MP3 files are normally created from 16-bit input,
    having the MP3 decoder produce 24-bit output allows us to use
    our own dither, leading to potentially higher audio quality,
    or at least better user control.  This library is very fast
    (even compared to xaudio, which we were using before) and
    very stable.  It was written by Rob Leslie.</p>
    <p>This library is only required if you want to import MP3 files</p>
</tr>
<tr>
<td bgcolor="#ccccff"
><a href="http://www.mars.org/home/rob/proj/mpeg/">libid3tag</a></td>
<td bgcolor="#ccccff"
><p>
    This library was also written by Rob Leslie (the author of libmad,
    above) and is a nice, simple library to read and write ID3 tags
    in MP3 files.</p>
    <p>This library is optional; if it is linked in, then the user will
    be presented with a Tags dialog when exporting MP3 files, and
    the tags will be imported when importing an MP3 file.
    Note that libid3tag is not distributed separately; it is
    included as part of MAD.</p>
</td>
</tr>
<tr>
<td bgcolor="#ccccff"
><a href="http://www.xiph.org/ogg/vorbis/"
>libogg<br>libvorbis<br>libvorbisfile</a>
</td>

<td bgcolor="#ccccff"
>
<p>Ogg Vorbis is a both a free audio compression format and a library
which encodes and decodes files in this format.  It is intended to
be a replacement for MP3, and many people feel that it equals and
sometimes surpasses the MP3 format in both quality and size.
The latest (beta) version of Audacity both imports and exports
Ogg Vorbis files.
</p>
</td>
</tr>
<tr>
<td bgcolor="#ccccff"
>
<a href="http://www.portaudio.com">portaudio</a>
</td>
<td bgcolor="#ccccff"
>
This library allows us to do audio I/O on multiple platforms using
a common API.  It hides the differences in audio I/O implementations
between different platforms and generally provides very good
performance.  The native audio code is stable for Windows (MME and DirectX),
Unix/OSS, and MacOS 9, and ports for MacOS X, Linux/ALSA, and Linux/aRts
are in progress.
</td>
</tr>
<tr>
<td bgcolor="#ccccff"
><a href="http://www.zip.com.au/~erikd/libsndfile/">libsndfile</a></td>

<td bgcolor="#ccccff"
>
This is the new library we're using to read and write audio files
such as WAV, AIFF, and AU.  It handles simple compression like ADPCM,
but not lossy-compressed files.
</td>
</tr>
</table>
</p>


<?php BoxBottom(); ?>

