<?php
/*
 * Copyright 2005-2008 Dominic Mazzoni, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "features-1.3-a";
  $pageTitle = _("New features in Audacity 1.3");
  include "../include/header.inc.php";
  include "../latest/versions.inc.php";
?>

<h2><?=$pageTitle?></h2>

<h3><?=_('Beta version')?></h3>
<p>
<?=_('Audacity 1.3 Beta is our new, not quite finished, development version which will be the basis of our next "stable" version.')?></p>

<p><?=_('
 <ul>
  <li>Contains dozens of new, exciting features. Very occasionally, these might need final polishing or not be retained in later versions.</li>
  <li>Occasionally, a feature might not work as it did before, or might be temporarily disabled.</li>
  <li>Some parts of the program are not yet documented or translated into different languages.</li>
 </ul>
')?></p>

<h3 id="details"><?=_("New in Audacity 1.3.7")?></h3>

<dl>
	<dt> Improvements and new features </dt>
	      <dd>   <ul>   
                      <li>F11 Full Screen mode</li>
                      <li>High-quality "Sliding Time Scale/Pitch Shift" effect</li>
                      <li>"Audio Contrast Analyzer" for testing audio on the
                           internet for
                           <a href="http://www.eramp.com/WCAG_2_audio_contrast_tool_help.htm">
                           WCAG2 accessibility compliance</a></li>
                      <li>On Windows, sound devices can now be opened using the more
                           efficient DirectSound API</li>
                      <li>As a first step towards improving latency correction, a fixed
                           rather than variable correction is now applied, customizable
                           in Preferences                    
                      <li>Numerous minor interface improvements such as Metadata
                           Editor navigation, new "hh:mm:ss + hundredths" selection format
                </ul>
         </dd>

	<dt> Numerous usability and stability fixes </dt>
	      <dd> Cross-platform Bug Fixes:
                <ul>
                     <li>Muting/soloing caused incorrect channel results in exported
                          stereo files</li>
                     <li>Nyquist effects: pasted unmodified audio into the result,
                          no progress bar, truncation of processed audio on
                          cancel</li> 
                     <li>Noise Removal: pasted unmodified audio into the result, 
                          unwanted filtering tail</li>
                     <li>Uncompressed exports:</li>
                            <ul>
                                  <li>Export as WAV could be corrupted if
                                       overwriting to same file</li>
                                  <li>Export multiple to uncompressed
                                       formats only produced 16-bit WAV</li>
                            </ul> 
                     <li>Compressed exports:</li>
                            <ul>
                                  <li>MP3 exports now produce correct bit rate
                                       mode, quality and length, with improved 
                                       metadata visibility in player applications 
                                       (Windows users will require the
                                       <a href="http://lame.buanzo.com.ar/">
                                       latest version of the LAME encoder</a>) 
                                  <li>WMA exports containing metadata are now correct</li> 
                            </ul>
                     <li>Restored support for multi-channel recording devices that
                          can record more than two channels using Audacity</li>
                </ul>
         </dd>

         <dd>Platform-specific Bug Fixes:
                <ul>
                     <li><i>Windows Vista:</i> fixed crash opening Preferences with no sound
                          devices enabled and connected </li>
                     <li><i>Mac OS X and Linux:</i></li>
                         <ul>      
                                <li>fixes for spurious clipping, label typing, no shortcuts after
                                     running effects</li>                           
                                <li>project rate now always changes to respect that of first 
                                   imported file</li>
                         </ul>
                     <li><i>Mac OS X only:</i></li>
                         <ul>
                                <li>fixes for inactive or corrupted menus, hidden dialogues, 
                                     portable settings not detected, and inability to set 
                                     independent Command and Control shortcuts</li>
                                <li>FFmpeg installer now available</li>
                         </ul> 
                </ul>
         </dd>    
   </dt>    
</dl>

<p>
Please see the included README.txt for full details of changes, and for issues known at time of Release.
</p>


<h3><?=_("New in Audacity 1.3.6")?></h3>

<dl>
	<dt> Major new capabilities </dt>
         <dd>   <ul>
                      <li><b>FFmpeg support</b> (downloadable separately) permits import and
                             export of a much wider range of file formats, including WMA, 
                             M4A and AC3, plus import of audio from video files</li>
                      <li><b>On-demand loading</b> of uncompressed files eliminates the 
                             wait before files can be played or edited</li> 
                      <li><b>Linked audio and label tracks</b> allow labels to move with
                             their corresponding audio when cutting, pasting or changing
                             speed or tempo</li>
         </ul>   </dd>
   </dt>

   <dt> Other features </dt>
         <dd>   <ul>
                       <li>Hierarchical plug-in grouping for built-in plug-ins</li>
                       <li>Sound activated recording</li>
                       <li>Ability to save smaller, compressed project files</li>
                       <li>MIDI files can now be imported, cut-and-paste edited, then exported</li>
         </ul>   </dd>      
   </dt>

   <dt> Miscellaneous </dt>
         <dd>   <ul>
                       <li>Transport menu for easy access to frequently used
                           recording/playback commands and preferences</li>       
                       <li>Default View Mode Preference to choose Waveform, 
                           Spectrum or Pitch view</li>
                       <li>Several bug fixes</li>
         </ul>  </dd>         
   </dt>
</dl>


<h3><?=_("New in Audacity 1.3.5")?></h3>

<dl>
	<dt> Recording / Playback </dt>
	      <dd>   <ul>
                           <li>Several bugs fixed so that latency correction should be better, 
                               and more devices work correctly.</li>
                           <li>Problems with invalid sample rates under Linux should be much rarer.</li>
                           <li>Newer version of Portaudio library.</li>
                           <li>New feature to record onto the end of an existing track
                               (hold Shift while clicking Record).</li>
             </dd>   </ul>
       </dt>

	<dt> Import / Export </dt>
	      <dd>   <ul>
                           <li>Updated versions of Libogg, Libvorbis, Libflac, Libsndfile and Twolame
                               libraries.
                           <li>Handling of unsupported file formats more informative.</li>
                           <li>Handling of file names with slashes on OS X improved.</li>
                           <li>New dialog allows replacement of illegal file name characters on all platforms.</li>
             </dd>   </ul>
       </dt>

	<dt> Interface </dt>
	      <dd>   <ul>
                           <li>Improved scaling and layout for rulers and VU meters.</li>
                           <li>Envelope fixes/improvements including full control of undo/redo.</li>
                           <li>New keyboard shortcuts and improved menu navigation.</li>
                           <li>Preferences:</li>
                               <ul>
                                    <li>More intuitive tab arrangement.</li> 
                                    <li>New options for mute/solo and Metadata Editor behavior.</li> 
                                    <li>Language can now be changed without restart.</li>
                               </ul>
			      <li>Expanded Build Information tab.</li>
           </dd>   </ul>
       </dt>

	<dt> Effects </dt>
	      <dd>   <ul>
                           <li>New Vocal Remover plug-in, improvements for Generate effects.</li>
             </dd>   </ul>
       </dt>

	<dt> Compilation </dt>
	      <dd>   <ul>
                           <li>Fixes when building Audacity with libraries disabled.</li>
                           <li>Improvements to make Mac and Solaris builds easier.</li>
             </dd>   </ul>
       </dt>

	<dt> Security </dt>
	      <dd>   <ul>
                           <li>Full fix for issue CVE-2007-6061 on systems where temporary directories
                                can be changed by other users (thanks to Michael Schwendt).</li>
             </dd>   </ul>
       </dt>


	<dt> Miscellaneous </dt>
	      <dd>   <ul>
                           <li>Updated translations for many locales.</li>
                           <li>Several stability improvements.</li> 
             </dd>   </ul>
       </dt>
</dl>


<h3><?=_("New in Audacity 1.3.4")?></h3>
<dl>
  <dt></dt>
  <dd>
    <!-- indent cheat -->
    <dl>
      <dt> New features </dt>
      <dd>New Welcome Screen with introduction to Audacity.</dd>
      <dd>Enhanced Windows Shell integration, so Audacity shows up in lots of
          Windows places such as "Open With".</dd>
      <dd>New keyboard command: 'Mix and Render to New Track' (bound to Ctrl+Shift+M).</dd>
      <dd>New keyboard shortcut: "Shift-A" starts playback when stopped,
          or performs "Stop and Select" when playing.</dd>
      <dd>Added support for VAMP audio analysis plug-ins.</dd>
      <dd>Solo button solos only one track at a time, and a track cannot be both mute and solo.</dd>

      <dt> Interface </dt>
      <dd>Keyboard shortcuts for making short/long jumps along the timeline.</dd>
      <dd>Added 'Snap To' in the Selection Bar.</dd>
      <dd>Made keyboard navigation easier when multiple menu items with the same first letter exist.</dd>
      <dd>Enhanced interface for label editing.</dd>
      <dd>Layout of OK/Cancel buttons consistency improved.</dd>
      <dd>
        Preferences:
        <ul>
          <li>"Select all audio in project, if none selected" (on by default).</li>
          <li>"Beep on completion of longer activities" (system bell, not main output).</li>
          <li>Other preferences cleaned up and explanations improved.</li>
        </ul>
      </dd>
      <dd>Envelopes: Many fixes when copying / pasting / repeating.</dd>
      <dd>Many translation updates.</dd>
      <dd>Track height fixed in several cases.</dd>
      <dd>CleanSpeech mode switching without closing and re-opening fixed.</dd>

      <dt> Opening/Saving Formats </dt>
      <dd>Metadata editor added for OGG, FLAC and WAV/AIFF exports, and general improvements.</dd>
      <dd>Metadata import improved.</dd>
      <dd>Muted tracks are no longer audible in the exported mix.</dd>

      <dt> Effects </dt>
      <dd>Truncate Silence: support for multiple and stereo tracks.</dd>
      <dd>
        Dtmf Generator:
        <ul>
          <li>Added support for keypad letters.</li>
          <li>Added an amplitude control.</li>
        </ul>
      </dd>
      <dd>Compressor: variable decay time added.</dd>
      <dd>
        Equalization:
        <ul>
          <li>Clicks at start / end prevented.</li>
          <li>Improvements to saved curves being found.</li>
          <li>Preview works correctly.</li>
        </ul>
      </dd>
      <dd>'Merge' command appears in Undo history.</dd>
      <dd>Clipping detected more reliably.</dd>
      <dd>Nyquist plug-ins reviewed and enhanced.</dd>
      <dd>Better (and more) progress bars.</dd>
      <dd>Canceling effect always restores previous audio.</dd>
      <dd>Several improvement to effects in batch mode.</dd>

      <dt> Recording/Playback </dt>
      <dd>Improvements to latency correction.</dd>
      <dd>Updated version of portaudio-v19 library.</dd>

      <dt>
        Note that Help is no longer built in, but accessible on the Web via links in Audacity.
      </dt>
    </dl>
  </dd>
</dl>

<h3 id="olderdetails"><?=_("New features in Audacity 1.3.3")?></h3>
<dl>
  <dt></dt>
  <dd>
    <!-- indent cheat -->
    <dl>
      <dt> Opening/saving formats </dt>
      <dd>
        Import
        <ul>
          <li>Import of audio from QuickTime (mov, aac, m4a) files now supported on OS X.</li>
          <li>Broadcast Wave Format (BWF) wave files can now be imported.</li>
        </ul>
      </dd>
      <dd>
        Export
        <ul>
          <li>Metadata can be added to OGG files.</li>
          <li>Improved Export option selection.</li>
          <li>Additional export options added to MP3 and FLAC file formats.</li>
          <li>Command line exporter now supported on Windows and OS X.</li>
        </ul>
      </dd>
      <dt> Effects </dt>
      <dd>
        EQ effect
        <ul>
          <li>Responsiveness improved.</li>
          <li>Several enhancements added.</li>
          <li>Batch support added.</li>
        </ul>
      </dd>
      <dd> New Auto Duck effect </dd>
      <dd> Added previewing to AudioUnit effects. </dd>
      <dd> Much improved Noise Removal effect </dd>
      <dd> Effects previewing can now be canceled. </dd>
      <dd> New DTMF Tone Generator effect </dd>
      <dd> Additional options available in Noise effect. </dd>
      <dd> Improved the Tone Generation effects. </dd>
      <dt> Other features </dt>
      <dd> New built-in screen capture utility </dd>
      <dd> Major speed improvement in Spectrogram rendering </dd>
      <dd> Increased support for drag and drop on OS X. </dd>
      <dd> Support added for building against wxWidgets 2.8.x. </dd>
      <dd> Can now open multiple Audacity Projects at once from Explorer on Windows. </dd>
      <dd> Improved main window sliders. </dd>
      <dd> New support for snapping while selecting and sliding </dd>
      <dd> Improved track focus handling and visual feedback. </dd>
      <dd> Speed improvements and handling of resizing/zooming in tracks </dd>
      <dd> Spectrum view can now be zoomed. </dd>
      <dd> New internal file cache to improve handling of project files over networks </dd>
      <dt> Also </dt>
      <dd> Many improvements to language specific translations </dd>
      <dd> Numerous stability improvements </dd>
    </dl>
  </dd>
</dl>

<h3><?=_("New features in Audacity 1.3.1 and 1.3.2")?></h3>

<ol>
<li><a href="features-1.3-h.php"><?=_("Improved Toolbar Docking")?></a></li>
<li><a href="features-1.3-i.php"><?=_("Track focus for improved keyboard navigation")?></a></li>
<li><a href="features-1.3-j.php"><?=_("Repair and Equalization effects")?></a></li>
<li><a href="features-1.3-k.php"><?=_("Timer Recording")?></a></li>
<li><a href="features-1.3-l.php"><?=_("Project saving and recovery")?></a></li>
<li><a href="features-1.3-d.php"><?=_("Selection Bar")?></a></li>
<li><a href="features-1.3-f.php"><?=_("Mac OS X features")?></a></li>
</ol>

<h3><?=_("New features in Audacity 1.3.0")?></h3>

<ol>
<li><a href="features-1.3-b.php"><?=_("Collapse/Expand Tracks")?></a></li>
<li><a href="features-1.3-c.php"><?=_("Multiple clips per track")?></a></li>
<li><a href="features-1.3-e.php"><?=_("Improved Label Tracks")?></a></li>
<li><a href="features-1.3-g.php"><?=_("Other features")?></a></li>
</ol>

<?php
  include "../include/footer.inc.php";
?>