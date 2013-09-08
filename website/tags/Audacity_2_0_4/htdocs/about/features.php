<?php
/*
 * Copyright 2004 - 13 Matt Brubeck, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "features";
  $pageTitle = _("Features");
  include "../include/header.inc.php";

  echo "<h2>$pageTitle</h2>";
  echo "<p>"._('This is a list of features in Audacity, the free, open source, cross-platform audio editor.  For full information on how to use these features, go to the <a href="../help/">help pages</a>.')."</p>";

  echo _('<h3 id="record">Recording</h3>
<p>Audacity can record live audio through a microphone or mixer, or <a href="http://manual.audacityteam.org/o/man/tutorial_copying_tapes_lps_or_minidiscs_to_cd.html">digitize recordings from cassette tapes, records or minidiscs</a>. With some sound cards, and on any Windows Vista, Windows 7 or Windows 8 machine, Audacity can also capture <a href="http://manual.audacityteam.org/o/man/tutorial_recording_audio_playing_on_the_computer.html">streaming audio</a>.</p>
<ul>
  <li><a href="http://manual.audacityteam.org/o/man/device_toolbar.html">Device Toolbar</a> manages multiple input and output devices.</li>  
  <li><a href="http://manual.audacityteam.org/o/man/meter_toolbar.html">Level meters</a> can monitor volume levels before, during and after recording. <a href="http://manual.audacityteam.org/o/man/glossary.html#clipping">Clipping</a> can be displayed in the waveform or in a label track.</li>
  <li>Record from microphone, line input, USB/Firewire devices and others.</li>
  <li><a href="http://manual.audacityteam.org/o/man/tutorial_recording_computer_playback_on_windows.html">Record computer playback on Windows Vista and later</a> by choosing "Windows WASAPI" host in Device Toolbar then a "loopback" input.</li>
  <li>Timer Record and Sound Activated Recording features.</li>
  <li>Dub over existing tracks to create <a href="http://manual.audacityteam.org/o/man/faq_recording_how_to_s.html#overdub">multi-track recordings</a>.</li>
  <li>Record at very low <a href="http://manual.audacityteam.org/o/man/glossary.html#latency">latencies</a> on supported devices on Windows by choosing "Windows WDM-KS" host and a low "Audio to Buffer" setting in <a href="http://manual.audacityteam.org/o/man/recording_preferences.html">Recording Preferences</a>. On Linux, you can obtain comparable latencies by using Audacity  with <a href="http://wiki.audacityteam.org/wiki/Linux_Issues#JACK">JACK</a>.  
  <li>Record at sample rates up to 192,000 Hz (subject to appropriate hardware and host selection). Up to 384,000 Hz is supported for appropriate high-resolution devices (note that Windows DirectSound host is limited to 200,000 Hz).</li> 
  <li>Record at 24-bit depth on Windows (using Windows WDM-KS or Windows WASAPI host), Mac OS X or Linux (using ALSA or JACK  host).
  <li>Record <a href="http://wiki.audacityteam.org/wiki/Multichannel_Recording">multiple channels</a> at once (subject to appropriate hardware).</li>
</ul>');

  echo _('<h3 id="imp_exp">Import and Export</h3>
<p>Import sound files, edit them, and combine them with other files or new recordings.  Export your recordings in many different file formats, including <a href="http://manual.audacityteam.org/o/man/export_multiple.html">multiple files at once</a>.</p>
<ul>
  <li>Import and export <b>WAV</b>, <b>AIFF</b>, <b>AU</b>, <b>FLAC</b> and <a href="http://vorbis.com">Ogg Vorbis</a> files.</li>
  <li>Fast "On-Demand" import of WAV or AIFF files (letting you start work with the files almost immediately) if read directly from source.</li>
  <li>Import and export all formats supported by <a href="http://www.mega-nerd.com/libsndfile/">libsndfile</a> such as <b>GSM 6.10</b>, <b>32-bit and 64-bit float WAV</b> and <b>U/A-Law</b>.</li>
  <li>Import MPEG audio (including <b>MP2</b> and <b>MP3</b> files) using <a href="http://www.underbit.com/products/mad/">libmad</a>.</li>
  <li>Import raw (headerless) audio files using the "Import Raw" command.</li>
  <li>Create WAV or AIFF files suitable for burning to <a href="http://manual.audacityteam.org/o/man/burning_music_files_to_a_cd.html">audio CD</a>.</li>
  <li>Export MP3 files with the optional <a href="http://manual.audacityteam.org/o/man/faq_installation_and_plug_ins.html#lame">LAME encoder library</a>.</li>
  <li>Import and export <b>AC3</b>, <b>M4A/M4R (AAC)</b> and <b>WMA</b> with the optional <a href="http://manual.audacityteam.org/o/man/faq_installation_and_plug_ins.html#ffdown">FFmpeg library</a> (this also supports import of audio from video files).</li>
</ul>');

  echo _('<h3>Sound Quality</h3>
<ul>
  <li>Supports 16-bit, 24-bit and 32-bit (floating point) samples (the latter preserves samples in excess of full scale).</li>
  <li>Sample rates and formats are converted using high-quality resampling and dithering.</li>
  <li>Tracks with different sample rates or formats are converted automatically in real time.</li>
</ul>');

  echo _('<h3 id="edit">Editing</h3>
<ul>
  <li>Easy editing with Cut, Copy, Paste and Delete.</li>
  <li>Unlimited sequential Undo (and Redo) to go back any number of steps.</li>
  <li>Edit and mix large numbers of tracks.</li>
  <li>Multiple <a href="http://manual.audacityteam.org/help/manual/man/audacity_tracks_and_clips.html">clips</a> are allowed per track.</li>
  <li><a href="http://manual.audacityteam.org/o/man/label_tracks.html">Label tracks</a> with selectable <a href="http://manual.audacityteam.org/o/man/sync-locked_track_groups.html">Sync-Lock Tracks</a> feature for keeping tracks and labels synchronized.</li>
  <li>Draw Tool to alter individual sample points.</li>
  <li><a href="http://manual.audacityteam.org/help/manual/man/envelope_tool.html">Envelope Tool</a> to fade the volume up or down smoothly.</li>
  <li>Automatic Crash Recovery in the event of abnormal program termination.</li>  
</ul>');

  echo _('<h3><a href="http://manual.audacityteam.org/help/manual/man/accessibility.html">Accessibility</a></h3>
<ul>
  <li> Tracks and selections can be <a href="http://manual.audacityteam.org/help/manual/man/audacity_selection.html">fully manipulated using the keyboard</a>.</li>
  <li>Large range of <a href="http://manual.audacityteam.org/help/manual/man/keyboard_shortcut_reference.html">keyboard shortcuts</a>. </li>
  <li>Excellent support for JAWS, <a href="http://www.nvda-project.org/">NVDA</a> and other screen readers on Windows, and for <a href="http://www.apple.com/accessibility/voiceover/">VoiceOver</a> on Mac.</li>   
</ul>');

  echo _('<h3 id="effects">Effects</h3>
<ul>
  <li>Change the pitch without altering the tempo (or vice-versa).</li>
  <li>Remove static, hiss, hum or other constant background noises.</li>
  <li>Alter frequencies with Equalization, Bass and Treble, High/Low Pass and Notch Filter effects.</li>
  <li>Adjust volume with Compressor, Amplify, Normalize, Fade In/Fade Out and Adjustable Fade effects.
  <li><a href="http://manual.audacityteam.org/help/manual/man/vocal_remover.html">Remove Vocals</a> from suitable stereo tracks.
  <li>Create voice-overs for podcasts or DJ sets using <a href="http://manual.audacityteam.org/help/manual/man/auto_duck.html">Auto Duck</a> effect.
  <li>Other built-in effects include:
    <ul>
      <li>Echo</li>
      <li>Paulstretch (extreme stretch)</li>
      <li>Phaser</li>
      <li>Reverb</li>
      <li>Reverse</li>
      <li>Truncate Silence</li>
      <li>Wahwah</li> 
    </ul>
  <li>Run "Chains" of effects on a project or multiple files in <a href="http://manual.audacityteam.org/o/man/chains_for_batch_processing_and_effects_automation.html">Batch Processing</a> mode. 
</ul>');

  echo _('<h3><a href="../download/plugins">Plug-ins</a></h3>
<ul>
  <li>Support for <a href="http://www.ladspa.org/">LADSPA</a>, <a href="http://wiki.audacityteam.org/wiki/Download_Nyquist_Plug-ins">Nyquist</a>, <a href="http://manual.audacityteam.org/o/man/faq_installation_and_plug_ins.html#vst_install">VST</a> and <a href="http://manual.audacityteam.org/o/man/effect_menu.html#Audio_Unit_Effects">Audio Unit</a> effect plug-ins.</li>
  <li>Effects written in the <a href="../help/nyquist">Nyquist programming language</a> can be easily modified in a text editor - or you can even write your own plug-in.</li>
</ul>');

  echo _('<h3>Analysis</h3>
<ul>
  <li>Spectrogram view modes for visualizing frequencies.</li>
  <li>"Plot Spectrum" command for detailed frequency analysis.</li>
  <li>"Sample Data Export" for exporting a file containing amplitude values for each sample in the selection.</li> 
  <li>Contrast Analysis for analyzing average rms volume differences between foreground speech and background music.</li>
  <li>Support for adding <a href="http://manual.audacityteam.org/help/manual/man/analyze_menu.html#vamp">VAMP</a> analysis plug-ins.</a>
</ul>');

  echo _('<h3>Free and Cross-Platform</h3>
<ul>
  <li>Licensed under the <a href="license">GNU General Public License (GPL)</a>.</li>
  <li>Runs on Windows, Mac OS X and GNU/Linux.</li>
</ul>');

  include "../include/footer.inc.php";
?>
