<?php
/*
 * Copyright 2004 -11 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "features";
  $pageTitle = _("Features");
  include "../include/header.inc.php";

  echo "<h2>$pageTitle</h2>";
  echo "<p>"._('This is a list of features in Audacity, the free audio editor.  For more information on how to use these features, go to the <a href="../help/">help pages</a>.')."</p>";

  echo _('<h3>Recording</h3>
<p>Audacity can record live audio through a microphone or mixer, or digitize recordings from cassette tapes, vinyl records, or minidiscs.  With some sound cards, it can also capture streaming audio.</p>
<ul>
  <li>Record from microphone, line input, or other sources.</li>
  <li>Dub over existing tracks to create multi-track recordings.</li>
  <li>Record up to 16 channels at once (requires multi-channel hardware).</li>
  <li>Level meters can monitor volume levels before, during, and after recording.</li>
</ul>');

  echo _('<h3>Import and Export</h3>
<p>Import sound files, edit them, and combine them with other files or new recordings.  Export your recordings in several common file formats.</p>
<ul>
  <li>Import and export WAV, AIFF, AU, and <a href="http://vorbis.com">Ogg Vorbis</a> files.</li>
  <li>Import MPEG audio (including MP2 and MP3 files) with <a href="http://www.underbit.com/products/mad/">libmad</a>.</li>
  <li>Export MP3 files with the optional <a href="http://audacity.sourceforge.net/help/faq?s=install&i=lame-mp3">LAME encoder library</a>.</li>
  <li>Create WAV or AIFF files suitable for burning to CD.</li>
  <li>Import and export all file formats supported by <a href="http://www.mega-nerd.com/libsndfile/">libsndfile</a>.</li>
  <li>Open raw (headerless) audio files using the “Import Raw” command.</li>
  <li><strong>Note:</strong>  To import or export AC3, AMR(NB), M4A, WMA and other proprietary formats, use <a href="http://audacity.sourceforge.net/download/features-1.3-a">Audacity Beta</a> with the optional <a href="http://audacity.sourceforge.net/help/faq?s=files&i=wma-proprietary">FFmpeg library</a>.</li>
</ul>');

  echo _('<h3>Editing</h3>
<ul>
  <li>Easy editing with Cut, Copy, Paste, and Delete.</li>
  <li>Use unlimited Undo (and Redo) to go back any number of steps.</li>
  <li>Very fast editing of large files.</li>
  <li>Edit and mix an unlimited number of tracks.</li>
  <li>Use the Drawing tool to alter individual sample points.</li>
  <li>Fade the volume up or down smoothly with the Envelope tool.</li>
</ul>');

  echo _('<h3>Effects</h3>
<ul>
  <li>Change the pitch without altering the tempo, or vice-versa.</li>
  <li>Remove static, hiss, hum, or other constant background noises.</li>
  <li>Alter frequencies with Equalization, FFT Filter, and Bass Boost effects.</li>
  <li>Adjust volumes with Compressor, Amplify, and Normalize effects.
  <li>Other built-in effects include:
    <ul>
      <li>Echo</li>
      <li>Phaser</li>
      <li>Wahwah</li>
      <li>Reverse</li>
    </ul>
  </li>
</ul>');

  echo _('<h3>Sound Quality</h3>
<ul>
  <li>Record and edit 16-bit, 24-bit, and 32-bit (floating point) samples.</li>
  <li>Record at up to 96 kHz.</li>
  <li>Sample rates and formats are converted using high-quality resampling and dithering.</li>
  <li>Mix tracks with different sample rates or formats, and Audacity will convert them automatically in realtime.</li>
</ul>');

  echo _('<h3>Plug-Ins</h3>
<ul>
  <li>Add new effects with <a href="http://www.ladspa.org/">LADSPA plug-ins</a>.</li>
  <li>Audacity includes some sample plug-ins by <a href="http://plugin.org.uk/">Steve Harris</a>.</li>
  <li>Load VST plug-ins for Windows and Mac (please read this <a href="../help/faq?s=install&i=vst-enabler">Frequently Asked Question</a>).</li>
  <li>Write new effects with the built-in <a href="../help/nyquist">Nyquist</a> programming language.</li>
</ul>');

  echo _('<h3>Analysis</h3>
<ul>
  <li>Spectrogram mode for visualizing frequencies.</li>
  <li>“Plot Spectrum” command for detailed frequency analysis.</li>
</ul>');

  echo _('<h3>Free and Cross-Platform</h3>
<ul>
  <li>Licensed under the <a href="license">GNU General Public License (GPL)</a>.</li>
  <li>Runs on Mac OS X, Windows, and GNU/Linux.</li>
</ul>');

  include "../include/footer.inc.php";
?>
