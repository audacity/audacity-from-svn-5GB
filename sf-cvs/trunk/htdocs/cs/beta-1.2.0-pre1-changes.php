<h2>Zmìny od vydání Audacity 1.0</h2>

Tato sekce uvádí seznam hlavních zmìn od vydání naší poslední stabilní verze Audacity 1.0.0. Uživatelé našich beta verzí by se mìli podívat na <a href="<?php print "betanotes.php$langQueryStr"; ?>">zmìny od Audacity 1.1.3</a>.

<h3>Profesionální zvuková kvalita</h3>

<ul>
<li>
Audacity nyní umí nahrávat a upravovat 24 a 32 bitové vzorky (pohyblivá øadová èárka). V jednom projektu mohou být stopy s rozdílnými vzorkovacími frekvencemi i formáty, pøièemž pokud je tøeba, Audacity je zkonvertuje v reálném èase. Pro všechny konverze je použito vysoce kvalitní pøevzorkování a dithering. Pro pøevzorkování se nyní používá algoritmus z knihovny <a href="http://www.mega-nerd.com/SRC/">SRC</a> od Erika de Castra Lopa.
</li>

<li>
Vstup a výstup zvuku v Audacity byl vylepšen. Audacity nyní mùže nahrávat více než dva kanály najednou. Kvùli snížení možnosti podteèení vyrovnávací pamìti byla snížena latence.
</li>
</ul>


<h3>Efekty</h3>

<ul>
<li>
Three new effects change the pitch and tempo of a track:
 <ul>
  <li>Change Pitch raises or lowers the tone of a selection, without
  affecting the speed.</li>
  <li>Change Tempo makes the selection play faster or slower, without
  altering the pitch.</li>
  <li>Change Speed alters both the playback speed and the pitch, just
  like changing the speed of a turntable or tape player.</li>
 </ul>
</li>

<li>
Most effects now include a "Preview" button, which allows you to try
different settings without closing the effect window.  A new command
allows you to repeat the last effect without re-opening the window.
</li>

<li>
Other new effects include:
 <ul>
  <li>Compressor, for dynamic range compression.</li>
  <li>Repeat, to loop samples.</li>
  <li>Normalize, to adjust volume and correct DC bias.</li>
 </ul>
</li>
</ul>


<h3>Nové editaèní vlastnosti</h3>

<ul>
<li>
The Envelope tool, used for smoothly fading tracks in and out, can now be
used to make tracks louder than their original volume as well as quieter.
</li>

<li>
The new "Time track" feature is similar to the volume envelope, but
instead changes the playback speed smoothly as a track plays.
</li>

<li>
Each track now has its own Gain and Pan controls, for easier mixing.
</li>

<li>
Audacity can find zero-crossings, to help create smooth cuts and loops.
Press "Z" to move selection edges to the nearest zero-crossings.
</li>
</ul>


<h3>Pluginy</h3>

<ul>
<li>
On Linux, Audacity can now load <a href="http://www.ladspa.org/">LADSPA</a>
plugins.
</li>

<li>
Audacity 1.2 features a digital signal processing language called
<a href="nyquist.php">Nyquist</a>, which allows users to program new effects
in a LISP-like language.
</li>
</ul>


<h3>Import a export souborù</h3>

<ul>
<li>
Audacity 1.2 project files use a new XML file format.  Audacity 1.2 will
automatically open and convert project files from earlier releases.
</li>

<li>
Audacity 1.2 uses <a href="http://www.underbit.com/products/mad/">libmad</a>
for much faster decoding of MP3 files.  Erik de Castro Lopo's
<a href="http://www.zip.com.au/~erikd/libsndfile/">libsndfile</a> provides
improved compatibility with many uncompressed audio file formats.
</li>

<li>
The latest version of the <a href="http://www.vorbis.com/">Vorbis</a>
libraries are used, for higher-quality Ogg Vorbis encoding.
</li>

<li>
The import and open dialogs now allow Audacity users to select several
audio files at once, and open them into a single project.  A new "LOF"
file format provides a way for Audacity to open a group of files with
offsets listed in a text file.
</li>
</ul>


<h3>Vylepšené uživatelské rozhraní</h3>

<ul>
<li>
New Edit and Mixer toolbars for fast access to common functions.
</li>

<li>
The new Drawing tool allows adjustment of individual samples, when fully
zoomed in.  The new Multi-Tool mode gives quick access to different editing
functions without needing to switch tools.
</li>

<li>
Many new keyboard commands have been added, and keyboard shortcuts can
now be customized.
</li>

<li>
New commands:
 <ul>
  <li>Looped play.  Type "L", or hold down shift when clicking Play.</li>
  <li>Type "1" to play a 1-second preview of the audio around the cursor.</li>
 </ul>
</li>

<li>
The mouse wheel can be used to zoom in and out.
</li>

<li>
Tracks can now be zoomed vertically by clicking or dragging in the
vertical rulers.  Shift-click or right-click to zoom out.  
</li>

<li>
The ruler and status bar can now display time in several different
formats, including seconds, samples, or video frames.
</li>

<li>
Audacity's interface can now be translated into languages other than
English.  You can volunteer to help <a href="translation/">translate
Audacity</a> into your native language.
</li>
</ul>

