<?php
/*
 * Copyright 2003 Dominic Mazzoni
 * Copyright 2005 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "faq";

  if ($_REQUEST["section"])
    $faqSectionId = $_REQUEST["section"];
  else
    $faqSectionId = $_REQUEST["s"];

  if ($_REQUEST["item"])
    $itemId = $_REQUEST["item"];
  else
    $itemId = $_REQUEST["i"];

  $faqSections = array(
    "general" => array(
      _("About Audacity"),
      array(
        "free" => array(
          _("Is Audacity really free?  Why?"),
          _("<p>Yes, Audacity is <a href=\"../about/\">completely free, open source software</a>.  You are free to use this program for personal or commercial purposes.  You are also free to give it away, sell it, or modify it for your own use, under the terms of the <a href=\"../about/license\">GNU General Public License</a>.</p>
<p>The authors of Audacity decided to release it under the GPL for many reasons. Some of us do it out of generosity. Some of us do it for moral reasons, because we feel that all software should be free; others believe that there is a place for both free and proprietary software.</p>
<p>One reason Audacity is free is so that it will be more popular and useful.  Yet another reason is to encourage collaboration. Because of Audacity's free license, dozens of people around the world have contributed code, bug fixes, documentation, and graphics.</p>")
        ),
        "spyware" => array(
          _("Does Audacity contain any spyware or adware?"),
          _("<p>No.  Audacity does not include any spyware or adware.  Audacity is <a href=\"../about/\">completely free and open source</a>.  It is developed by a not-for-profit volunteer group, and the source code is available for anyone to study or use.</p>")
        ),
        "copy-permission" => array(
          _("May I distribute copies of Audacity?"),
          _("<p>You can redistribute Audacity under the GNU General Public License, which gives you permission to modify, copy, and sell the program as long as you keep the same license and make the source code available.  For details, see our <a href=\"../about/license\">license information</a>.</p>")
        ),
        "ebay" => array(
          _("Someone is selling Audacity on eBay.  Is this legal?"),
          _("<p>It is legal to sell Audacity, as long as the seller makes the program and source code available under the <a href=\"../about/license\">GNU General Public License</a>.</p>
<p>Some vendors try to trick customers by selling the software under a different name.  If you bought a product and later found out it was Audacity, we encourage you to <b>ask for a refund or file a complaint</b> if you feel you did not get a fair deal.  We ask vendors to give proper credit to the Audacity project, and allow users to make an informed choice.</p>
<p><b>If you want to buy Audacity on CD</b>, please check out the vendors listed on our <a href=\"../download/buy\">purchase page</a>.</p>")
        ),
        "blind-users" => array(
          _("Does Audacity work with screen-reader programs for blind users?"),
          // i18n-hint: The link below is in English.
          _("<p>Audacity works with most screen-reader programs, but some of Audacity’s features are difficult or impossible to use without a mouse.  We are working on improving the program’s accessibility and keyboard shortcuts.  For more information, see <a href=\"http://audacityteam.org/wiki/index.pl?AudacityForBlindUsers\">Audacity for Blind Users</a>.</p>")
        ),
        "time-remaining" => array(
          _("What does “time remaining” mean?  Does Audacity expire?"),
          _("<p>Audacity does not expire.  During recording, it displays how long you can record before running out of space on your computer’s hard drive.  To record for a longer time, delete old files or save your project on a larger disk.</p>")
        ),
      )
    ),
    "install" => array(
      _("Installation and Plug-Ins"),
      array(
        "lame-mp3" => array(
          _("How do I download and install the LAME MP3 encoder?"),
          sprintf(_('<p>Because of software patents, we cannot distribute MP3 encoding software ourselves.  Follow these instructions to use the free LAME encoder to export MP3 files with Audacity.
<h3>Windows</h3>
<ol>
  <li>Go to the <a href="%s">LAME download page</a>.</li>
  <li>Click on any link from the list of identical "%s" links.</li>
  <li>When you have finished downloading LAME, unzip it and save the file <b>lame_enc.dll</b> anywhere on your computer.</li>
  <li>The first time you use the "Export as MP3" command, Audacity will ask you where lame_enc.dll is saved.</li>
</ol>
<h3>Mac OS 9 or X</h3>
<ol>
  <li>Go to the <a href="%s">LAME download page</a>.</li>
  <li>Download the version of LameLib for your operating system.</li>
  <li>When you have finished downloading, use Stuffit Expander to extract the files.  (This may happen automatically.)</li>
  <li>Save the file called "LameLib" anywhere on your computer.</li>
  <li>The first time you use the "Export as MP3" command, Audacity will ask you where LameLib is saved.</li>
</ol>'), "http://www-users.york.ac.uk/~raa110/audacity/lame.html", "lame-3.96.1", "http://spaghetticode.org/lame/")
        ),
        // TODO: Connect/replace this with documentation on plug-ins page?
        "vst-enabler" => array(
          _("How do I install VST plug-ins?"),
          sprintf(_("<ol>
<li><p>First, download and unzip the <a href=\"%s\">VST Enabler</a>.</li>
<li><p>Place the unzipped VST Enabler, along with your VST plug-ins, into the “Plug-ins” folder in the Audacity installation folder.  (On Mac OS X, this is usually in the Applications folder.  On Windows, it is usually under Program Files.)</li>
<li><p>The next time you start Audacity, your VST effects will appear at the bottom of the “Effect” menu.</li>
</ol>
<p>See also: <a href=\"faq?s=install&amp;i=vst-bugs\">Why do some VST plug-ins look wrong, or not work?</a></p>"), "http://audacityteam.org/vst/")
        ),
        "vst-bugs" => array(
          _("Why do some VST plug-ins look wrong, or not work?"),
          _("<p>The <a href=\"faq?s=install&amp;i=vst-enabler\">VST Enabler</a> is not yet able to display custom interfaces for plug-ins.  Instead, it uses generic controls for all VST plug-ins.  Because of this, many plug-ins will look or act differently than they do in other programs.</p>
<p>Other VST plug-ins may not work at all, because of bugs in the VST Enabler.  You can <a href\"../contact/\">contact us</a> to report plug-ins that do not work.</p>  
<p>Audacity also does not support VST instrument (VSTi) plug-ins.</p>")
        ),
        "download-problems" => array(
          _("What should I do if I have problems downloading or installing Audacity?"),
          _("<p>If you can't download Audacity, or if the downloaded setup file is corrupt, try downloading it from our <a href=\"http://sourceforge.net/project/showfiles.php?group_id=6235\">alternate servers</a>.</p>")
        ),
      ),
    ),
    "recording" => array(
      _("Recording"),
      array(
        "stereo" => array(
          _("How can I record in stereo?"),
          _("<p>To record in stereo, open the Audacity preferences.  In the “Audio I/O” section, change the number of recording channels from 1 (mono) to 2 (stereo).</p>")
        ),
        "records-tapes" => array(
          _("How do I record from vinyl records, cassette tapes, or minidiscs?"),
          _('<p>First, set Audacity to <a href="faq?s=recording&amp;i=stereo">record in stereo</a>.</p>
<p>Next, plug one end of a stereo cable into the “Line Out” or “Headphone” connector on your tape deck, minidisc player, or stereo system.  Plug the other end into your computer’s “Line In” connector.  If you do not have a cable that fits both of these connectors, you can find one at an electronics store.</p>
<p>Choose “Line In” as the input source on the Audacity toolbar, and press the Record button.  While Audacity is recording, start playing your tape or disc.  When you have captured the entire recording, press the Stop button.</p>
<p>Notes:</p>
<ul>
	<li><p>Do not plug stereo equipment into your computer’s “Microphone” port, which is designed for low-powered (“mic-level”) signals only.   Use the “Line In” port instead.</p></li>
	<li><p>Do not connect a turntable directly to your computer.  The signal from a turntable is distorted; it must be corrected by passing it through a phono pre-amp or a receiver with a “phono” input.</p></li>
</ul>
<p>See also: <a href="faq?s=files&amp;i=split">How to split a long recording into multiple files</a>.</p>').
          "<p>"._("See also:").' <a href="faq?s=files&amp;i=burn-cd">'._("How do I save my recording on an audio CD?")."</a></p>"
        ),
        "playthrough" => array(
          _("Why can’t I hear what I’m recording?"),
          _("<h3>Windows and Linux</h3>
<p>To monitor your recording, open your computer's volume control panel.  Turn up the playback volume and turn off the “mute” checkbox for your recording source (usually “microphone” or “line in”).</p>
<p>If this does not work, open the Audacity preferences.  In the “Audio I/O” section, turn on the “Software Playthrough” option.</p>
<h3>Mac OS X</h3>
<p>Open the Audacity preferences.  In the “Audio I/O” section, turn on the “playthrough” option.  “Hardware Playthrough” is best if it is available.  If it does not work, you can choose “Software Playthrough” instead.</p>
")
        ),
        "multi-track" => array(
          _("Can I play a track while recording a new one on top of it?"),
          _("<p>This is called multi-track recording.  It makes it possible to record harmonies with yourself, or add new instruments or vocals to an existing recording.  To do this in Audacity, follow these instructions:</p>
<ol>
  <li>Import or record the first track.</li>
  <li>Open the “Audio I/O” section of the Audacity preferences, and check the box labeled <b>Play other tracks while recording new one.</b></li>
  <li>Close the preferences and press the <b>Record</b> button.</li>
</ol>
<p>See also: <a href=\"faq?s=recording&amp;i=sync\">Why isn't my new track in sync with the previous ones?</a></p>")
        ),
        "sync" => array(
          _("Why isn’t my new track in sync with the previous ones?"),
          _("<p>When you make a <a href=\"faq?s=recording&amp;i=multi-track\">multi-track recording</a>, there is an unpredictable delay between playback and recording.  Audacity tries to correct for this automatically, but this doesn't yet work on all computers.</p>
<p>If a new track is not synchronized with the others, you can zoom in and use the Time Shift tool to drag it to the correct location.</p>")
        ),
        "streaming" => array(
          _("Can Audacity record RealAudio or other streaming audio?"),
          _("<h3>Windows and Linux</h3>
<p>With most Windows and Linux audio devices, it is possible to record whatever sound the computer is currently playing, including internet radio streams.</p>
<p>In the drop-down menu on Audacity's mixer toolbar, choose “Wave Out” or “Stereo Mix” as the input source.  (The exact name may be different, depending on your computer's sound drivers.)  When you press the Record button, Audacity will capture whatever sound is playing on your computer's speakers.</p>
<p>If this doesn't work on your computer, you can instead use a cable to connect your computer's “Line Out” (speaker) port to its “Line In” port, and use Audacity to record from Line In.</p>
<h3>Mac OS X</h3>
<p>Mac OS X users can capture streaming audio using a program like <a href=\"http://www.rogueamoeba.com/audiohijack/\">Audio Hijack</a> or <a href=\"http://www.ambrosiasw.com/utilities/wiretap/\">Wiretap Pro</a>.</p>
")
        ),
        "scheduled" => array(
          _("Can I set Audacity to record at a certain time?"),
          _("<p>Sorry, Audacity does not yet support scheduled recording.  We may add this feature to a future version of the program.</p>
<p>You can make Audacity <b>stop</b> recording after a certain time limit, by following these steps:</p>
<ol>
  <li>Turn on “Play existing tracks while recording” in the “Audio I/O” section of the preferences.</li>
  <li>Choose “New Audio Track” from the Project menu.</li>
  <li>Zoom out if necessary, then click and drag to select the amount of time you want to record.</li>
  <li>Start recording.  Audacity will stop recording automatically when it reaches the end of the selected area.</li>
</ol>")
        ),
      ),
    ),
    "files" => array(
      _("Opening and Saving Files"),
      array(
        "aup" => array(
          _("How do I open an Audacity project (AUP file) in another program?"),
          _("<p>Audacity project files are saved in a special format that only Audacity can open.  To open your project in another program or burn it to CD, you must open the “project.aup” file in Audacity, and use the Export commands (in the File menu) to save it in a standard format like WAV or AIFF.</p>")
        ),
        "data-folder" => array(
          _("Why does Audacity create a folder full of .au files when I save a project?"),
          _("<p>Audacity breaks long tracks into small pieces so it can edit them more efficiently.  When you save a project, Audacity stores all of the pieces in a folder with a name like “project_data.”</p>  <p>You do not need to open these files yourself.  Audacity will load them automatically when you open the “project.aup” file, which is saved in the same location as the “data” folder.</p>").
          "<p>"._("See also:").' <a href="faq?s=files&amp;i=aup">'._("How do I open an Audacity project (AUP file) in another program?")."</a></p>"
        ),
        "crash-recovery" => array(
          _("Audacity crashed!  Can I recover any unsaved data?"),
          // i18n-hint: Sorry, these links are in English only.  If you would
          // like to make more information available, please create a page on
          // the wiki for now.
          _("<p>You may be able to recover your project automatically with the <a href=\"http://www.mesw.de/audacity/recovery/\">Audacity Recovery Utility</a>.  Please see the <a href=\"http://audacityteam.org/wiki/index.pl?CrashRecovery\">Crash Recovery</a> page for more information.</p>")
        ),
        "wma-proprietary" => array(
          _("Can Audacity import file formats like WMA, AAC, FLAC, etc.?"),
          _("<p>Audacity <b>cannot</b> import or export files in <b>WMA, AAC, RealAudio, Shorten (SHN)</b>, or most other proprietary formats.  Because of licensing and patent restrictions, we are not allowed to add these formats to Audacity.  Future versions of Audacity might be able to support these formats using codecs installed in your operating system.</p>
<p>Some open formats are not yet supported by Audacity, including <b>Ogg Speex</b> and <b>FLAC</b>.  We hope to support these formats in future versions of Audacity.</p>
<p>For a list of supported formats, see the <a href=\"../about/features\">feature list</a>.</p>
<p>See also: <a href=\"faq?s=files&amp;i=midi\">Why can't I play MIDI files?</a></p>")
        ),
        "midi" => array(
          _("Why can’t I play MIDI files?"),
          _("<p>Sorry, Audacity cannot play, edit, or convert MIDI files.  It can only display them visually (for comparison with recorded sounds).  We might add MIDI editing to a future version of Audacity, but for now Audacity is focused on sampled audio (like WAV files).</p>")
        ),
        "import-cd" => array(
          _("How do I import a track from an audio CD?"),
          _("<p>Audacity cannot import a track directly from an audio CD.  You must use a separate program like <a href=\"http://cdexos.sourceforge.net/\">CDex</a> or <a href=\"http://www.apple.com/itunes/\">iTunes</a> to extract CD tracks into a format that Audacity can read, like WAV or AIFF.</p>")
        ),
        "burn-cd" => array(
          _("How do I save my recording on an audio CD?"),
          "<p>"._("After making a recording or editing a file in Audacity, follow these steps to save your work on an audio CD:")."</p>".
          _("<ol>
  <li>Use the “Export as WAV” or “Export as AIFF” command to save your Audacity recording in a sound file.</li>
  <li>Use any CD-recording software (iTunes or Nero, for example) to burn this file to a CD.</li>
</ol>
<p>To make a disc you can play in normal CD players, make sure to create a “music” or “audio” CD (not a “data” CD).  Use CD-R discs, because some players cannot read CD-RW.</p>
<p>Some CD software will burn only 16-bit, 44.1KHz stereo sound files.  If your CD recording software won't open your sound file, export the file again after choosing the following settings in Audacity:</p>
<ol>
  <li>At the bottom of the Audacity window, set the Project Rate to 44100 Hz.</li>
  <li>In the File Formats preferences, choose WAV (16-bit...) or AIFF (16-bit...).</li>
  <li>If your project does not already contain a stereo track, choose “New Stereo Track” from the Project menu.  (This will make Audacity export your recording as a stereo file.)</li>
</ol>
<p>See also: <a href=\"faq?s=files&amp;i=split\">How can I split a long recording into multiple tracks?</a></p>")
        ),
        "split" => array(
          _("How can I split a long recording into multiple files or CD tracks?"),
          _("<p>Follow these steps to create a separate file for each song or segment of a long recording.  This is particularly useful if you are creating a CD, since each file will appear as a separate track on the CD.</p>
<ol>
  <li>Click to place the cursor at the start of the first song.</li>
  <li>Choose “Add Label at Selection” from the Project menu.  If you wish, you can type the name of the song.</li>
  <li>Repeat steps 1 and 2 for each song.</li>
  <li>When you are finished, choose “Export Multiple” from the File menu.  When you click the “Export” button, Audacity will save each song as a separate file, using the format and location you choose.</li>
</ol>").
          "<p>"._("See also:").' <a href="faq?s=files&amp;i=burn-cd">'._("How do I save my recording on an audio CD?")."</a></p>"
        ),
      ),
    ),
    "editing" => array(
      _("Editing"),
      array(
        "menu-disabled" => array(
          _("Why can’t I use the effects or other menu items?"),
          _("<p>Some menu items are grayed out or disabled until they are ready for use.  Before choosing an effect, you must select the audio that you want to change.  To select audio, click and drag with the Selection tool to highlight it, or choose the “Select All” command from the Edit menu.</p>")
        ),
        "join" => array(
          _("How do I combine two files into one longer file?"),
          _("<p>Follow these steps to splice two files together:</p>
<ol>
  <li>Import both files into Audacity.</li>
  <li>Select the second one by clicking on its label (the area around the mute/solo buttons).</li>
  <li>Choose “Cut” from the Edit menu.</li>
  <li>Place the cursor by clicking in the first track, after the end of the audio.</li>
  <li>Choose “Paste” from the Edit menu.</li>
</ol>
<p>You can press the Play button to hear the result, and use the Export commands (in the File menu) to save it as a sound file.</p>")
        ),
        "mix" => array(
          _("How do I mix two tracks together?"),
          _("<p>To mix two files, just import both of them into Audacity.  They will appear in separate tracks, and will be mixed together when you press the Play button.  You can use the Time Shift tool to move them around so that they start at different times, or use the other editing commands to alter either of the tracks.</p>")
        ),
        "remove-vocals" => array(
          _("Can I remove the vocals from a recording to make a Karaoke track?"),
          _("<p>This is possible <b>only for certain stereo tracks</b>.  When the vocals are exactly the same on both stereo channels, you can remove them by “subtracting” one channel from the other.  This works for many studio recordings, where the vocal track is mixed exactly in the center.</p>
<p>To do this in Audacity:</p>
<ol>
  <li>Import your stereo file into Audacity.</li>
  <li>Open the track menu (click the arrow next to the track title), and choose “Split Stereo Track.”</li>
  <li>Select the lower track (the right channel) by clicking it in the area around the mute/solo buttons.</li>
  <li>Choose “Invert” from the Effects menu.</li>
  <li>Using the track menus, change each track to “Mono.”</li>
</ol>
<p>Press the Play button to hear the results.  If you are lucky, the voice will be gone but most of the other instruments will be unaffected, just like a karaoke track.  You can use the Export commands in the File menu to save the results.</p>
")
        ),
      ),
    ),
  );

  if ($faqSectionId != "" && $itemId != "") {
    // Print the requested item.
    $item = $faqSections[$faqSectionId][1][$itemId];

    $question = $item[0];
    $answer   = $item[1];

    $pageTitle = $question;
    include "../include/header.inc.php";

    echo "<h2>$question</h2>";
    echo $answer;

    echo "<h3><a href=\"faq\">"._("Other frequently asked questions...")."</a></h3>";
  }
  else {
    // Print the list of sections and questions.
    $pageTitle = _("Frequently Asked Questions");
    include "../include/header.inc.php";

    echo "<h2>$pageTitle</h2>";
    echo "<p>"._('These are some of the most common questions about Audacity.  If your question isn’t answered here, see the <a href="documentation">documentation</a>, or <a href="../contact/">contact us</a> for help.')."</p>";

    foreach ($faqSections as $faqSectionId => $section) {
      $sectionTitle = $section[0];
      $sectionItems = $section[1];

      echo "<h3 id=\"s$faqSectionId\">$sectionTitle</h3>";

      echo "<ol>";
      foreach ($sectionItems as $itemId => $item) {
        $question = $item[0];
        $answer   = $item[1];

        echo "<li><a href=\"faq?s=$faqSectionId&amp;i=$itemId\">$question</a></li>";
      }
      echo "</ol>";
    }
  }

  include "../include/footer.inc.php";
?>
