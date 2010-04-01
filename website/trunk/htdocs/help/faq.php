<?php
/*
 * Copyright 2003 Dominic Mazzoni
 * Copyright 2005 Matt Brubeck
 * Copyright 2007 - 10 Gale Andrews, Vaughan Johnson
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
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
      _("<div id=\"about\">About Audacity</div>"),
      array(
        "free" => array(
          _("Is Audacity really free?  Why?"),
          _("<p>Yes, Audacity is <a href=\"../about/\">completely free, open source software</a>.  You are free to use this program for any personal, commercial, or educational purposes, including installing it on as many different computers as you wish. You are also free to give it away, sell it, or modify it for your own use, under the terms of the <a href=\"../about/license\">GNU General Public License</a>.</p>
<p>The authors of Audacity decided to release it under the GPL for many reasons. Some of us do it out of generosity. Some of us do it for moral reasons, because we feel that all software should be free; others believe that there is a place for both free and proprietary software.</p>
<p>One reason Audacity is free is so that it will be more popular and useful.  Yet another reason is to encourage collaboration. Because of Audacity's free license, dozens of people around the world have contributed code, bug fixes, documentation, and graphics.</p>
<p>We welcome <a href=\"../community/donate\">donations</a> to support Audacity development.</p>")
        ),
        "spyware" => array(
          _("Does Audacity contain any spyware or adware?"),
          _("<p>No.  Audacity does not include any spyware or adware.  Audacity is <a href=\"../about/\">completely free and open source</a>.  It is developed by a not-for-profit volunteer group, and the source code is available for anyone to study or use.</p>")
        ),
        "copy-permission" => array(
          _("May I distribute copies of Audacity?"),
          _("<p>You can redistribute Audacity under the <a href=\"../about/license\">GNU General Public License</a>, which gives you permission to modify, copy, and sell the program as long as you keep the same license and make the source code available.  For details, see <a href=\"../about/license\">License, and Advice for Vendors</a>.</p>")
        ),
        "ebay" => array(
          _("Someone is selling Audacity on eBay.  Is this legal?"),
          // i18n-hint: the encoding inside the <a href> tag below
          // obscures the e-mail address from (at least some)
          // harvesting bots. Please ignore it, and restart
          // translation at "e-mail us".
          _("<p>It is legal to sell Audacity, as long as the seller makes the program and source code available under the <a href=\"../about/license\">GNU General Public License</a> (GPL).</p>
<p>Some vendors try to trick customers by selling unmodified Audacity software under a different name. If you bought a product and later found out it was Audacity, we encourage you to <b>ask for a refund or file a complaint</b> if you feel you did not get a fair deal.  We ask vendors to give proper credit to the Audacity project, and allow users to make an informed choice.</p>
<p>If you have concerns that Audacity (unmodified or otherwise) is being sold without a copy of the GPL, or without any offer to provide source code, please
          <a href=\"&#109;&#x61;&#105;&#x6c;&#x74;&#111;&#x3a;&#97;&#98;&#x75;&#115;&#x65;&#64;&#97;&#x75;&#100;&#97;&#x63;&#x69;&#116;&#x79;&#x74;&#101;&#97;&#x6d;&#x2e;&#x6f;&#114;&#x67;\">
          e-mail us</a> in confidence. </p>
<p><b>If you want to obtain Audacity on CD</b>, free ISO downloads and inexpensive CDs by post are available from suppliers on our <a href=\"../download/buy\">Audacity on CD page</a>.</p>")
        ),
        "64-bit" => array(
          _("Does Audacity run on 64-bit systems?"),
          _("<p>There is no 64-bit version of Audacity, but in principle it should run on 64-bit systems, subject to having appropriate drivers for the sound device. Sufficient RAM must be available to run Audacity smoothly and support the greater memory requirements of 64-bit systems compared to 32-bit.</p>    
<h3>Windows and Linux</h3> 
<p>On 64-bit versions of Windows XP, Vista and 7, and on 64-bit Linux, sound device drivers specific to the 64-bit operating system are essential. If upgrading from a 32-bit to 64-bit operating system, a 64-bit capable computer is required, and the sound device drivers must also be updated to a 64-bit version.</p> 
<p>Users upgrading from 32-bit Windows XP to 64-bit Windows Vista or 7 should make especially sure their computer is well above Audacity's minimum <a href=\"../download/windows#sysreq\">system requirements for Windows</a>.</p> 
<h3>Mac OS X</h3> 
<p>We strongly recommend the <a href=\"../download/beta_mac\">Beta</a> version of Audacity for 64-bit systems. OS X 10.5 supports both 64-bit and 32-bit processors equally well, with few if any issues with the supplied system drivers.</p>") 
        ),
        "blind-users" => array(
          _("Does Audacity work with screen-reader programs for blind users?"),
          // i18n-hint: The link below is in English.
          _("<p>Audacity works with most screen-reader programs, but some of Audacity’s features are difficult or impossible to use without a mouse.  We are working on improving the program’s accessibility and keyboard shortcuts.  For more information, see <a href=\"http://audacityteam.org/wiki/index.pl?AudacityForBlindUsers\">Audacity for Blind Users</a>.</p>")
        ),
        "time-remaining" => array(
          _("What does “time remaining” mean?  Does Audacity expire?"),
          _("<p>Audacity does not expire, but the time you can record for depends on the amount of disk space you have. The message you are seeing exists only in older versions of Audacity, so please update to the <a href=\"../download\">latest version</a>. In newer versions, the message says \"Disk space remains for recording xx hours and yy minutes\".</p>
<p>To get more recording time, delete your old files and folders (especially your old Audacity Project files and _data folders when you have finished with them), or use an alternative disk with more space on it. Or consider recording in mono instead of stereo (on the Audio I/O tab of Preferences) or in 16 bit instead of 32 bit (Quality tab of Preferences). Either of those choices will halve the disk space needed.</p>")
        ),
      )
    ),
    "install" => array(
      _("<div id=\"install\">Installation and Plug-Ins</div>"),
      array(
        "lame-mp3" => array(
          _("How do I download and install the LAME MP3 encoder?"),
          sprintf(_('<p>Because of software patents, we cannot distribute MP3 encoding software ourselves.  Follow these instructions to use the free LAME encoder to export MP3 files with Audacity.
<h3 id="win">Windows</h3>
<ol>
  <li>Go to the <a href="http://lame.buanzo.com.ar/">LAME download page</a>.</li>
  <li>Under "For Audacity on Windows", left-click the link "Lame v3.98.2 for Audacity on Windows.exe" and save the file anywhere on your computer. <b>Do not right-click the link to the .exe file.</b> </li> 
  <li>Double-click <b>"Lame_v3.98.2_for_Audacity_on_Windows.exe"</b> to launch it (you can safely ignore any warnings that the "publisher could not be verified").</li> 
  <li>Follow the "Setup" instructions to install LAME for Audacity. Do not change the offered destination location of "C:\Program Files\Lame for Audacity".</li>
  <li>The first time you use the "Export as MP3" command, Audacity will ask for the location of <b>"lame_enc.dll"</b>. Navigate to "C:\Program Files\Lame for Audacity", select "lame_enc.dll", then click "Open" and "OK".</li>
  <li>If you prefer the Zip option for the LAME download, save the zip file to anywhere on your computer, extract "lame_enc.dll" to any location, then show Audacity where to find it as in Step 5 above.</li>
  <li>In case of difficulty, please view our <a href="http://audacityteam.org/wiki/index.php?title=Lame_Installation#Windows_Instructions">more detailed instructions</a> on the <a href="http://audacityteam.org/wiki/">Audacity Wiki</a>.</li>
</ol>
<h3 id="mac">Mac OS 9 or X</h3>
<ol>
  <li>Go to the <a href="http://lame.buanzo.com.ar/">LAME download page</a>.</li>
  <li>Click to download either <b>"Lame Library v3.98.2 for Audacity on OSX.dmg"</b> or <b>"LameLib-Carbon.sit"</b>, according to your operating system.</li>
  <li>Double-click the .dmg to extract "Lame Library v3.98.2 for Audacity on OSX.pkg" to Finder, or use Stuffit to extract "LameLib" from the .sit (either of these may happen automatically).</li>
  <li>Double-click the .pkg to install <b>"libmp3lame.dylib"</b> in /usr/local/lib/audacity, or save <b>"LameLib"</b> anywhere on your computer.</li>
  <li>The first time you use the "Export as MP3" command, Audacity will ask for the location of "libmp3lame.dylib" or "LameLib". Navigate to that location, then open the file and click "OK".</li>
  <li>In case of difficulty, please view our <a href="http://audacityteam.org/wiki/index.php?title=Lame_Installation#Mac_Instructions">more detailed instructions</a> on the <a href="http://audacityteam.org/wiki/index.php">Audacity Wiki</a>.</li> 
</ol>
<h3 id="nix">Linux/Unix</h3>
Follow <a href="http://audacityteam.org/wiki/index.php?title=Lame_Installation#GNU.2FLinux.2FUnix_instructions">these instructions</a> on our <a href="http://audacityteam.org/wiki/">Wiki</a> to download and install a suitable LAME package from the internet.'),  "http://lame.buanzo.com.ar/", "http://lame.buanzo.com.ar/")
        ),
        "vst-enabler" => array(
          _("How do I install VST plug-ins?"),
          sprintf(_("<ol>
<li><p>First, download and unzip the <a href=\"%s\">VST Enabler</a>.</li>
<li><p>Place the unzipped VST Enabler, along with your VST plug-ins, into the “Plug-ins” folder in the Audacity installation folder.  (On Mac OS X, this is usually in the Applications folder.  On Windows, it is usually under Program Files.)</li>
<li><p>The next time you start Audacity, your VST effects will appear at the bottom of the “Effect” menu.</li>
</ol>
<p><b>Note:</b> Current Beta (1.3) versions of Audacity will display VST plug-ins automatically, without having to install the VST Enabler.</p>    
<p>See also: <a href=\"faq?s=install&amp;i=vst-bugs\">Why do some VST plug-ins look wrong, or not work?</a></p>"), "http://audacityteam.org/vst/")
        ),
        "vst-bugs" => array(
          _("Why do some VST plug-ins look wrong, or not work?"),
          _("<p>1.2 versions of Audacity require the <a href=\"faq?s=install&amp;i=vst-enabler\">VST Enabler</a> in order to display VST plug-ins. The Enabler can only display a generic, tabular interface for plug-ins, so they may look or act differently than they do in other programs, or not function at all.</p>
<p>Current Beta (1.3) versions of Audacity will by default display VST effect plug-ins with their full interface, without the need of an Enabler. If you experience any problems with VST plug-ins in Audacity Beta, please read this <a href=\"http://forum.audacityteam.org/viewtopic.php?f=37&t=11534&p=45667#p45667\">Forum topic</a>. You may <a href=\"../contact#feedback\">contact us</a> to report any difficulties not covered in that topic.</p>                
<p>Neither version of Audacity supports VST instrument (VSTi) plug-ins. </p>")
        ),
        "download-problems" => array(
          _("What should I do if I have problems downloading or installing Audacity?"),
          _("<p>If you can't download Audacity, or if the downloaded setup file is corrupt, try downloading it from our <a href=\"http://sourceforge.net/project/showfiles.php?group_id=6235\">alternate servers</a>.</p>")
        ),
      ),
    ),
    "recording" => array(
      _("<div id=\"record\">Recording</div>"),
      array(
        "stereo" => array(
          _("How can I record in stereo?"),
          _("<p>To record in stereo, open the Audacity preferences.  In the “Audio I/O” section, change the number of recording channels from 1 (mono) to 2 (stereo).</p>")
        ),
        "records-tapes" => array(
          _("How do I record from vinyl records, cassette tapes, or minidiscs?"),
          _('<p>First, set Audacity to <a href="faq?s=recording&amp;i=stereo">record in stereo</a>.</p>
<p>Next, plug one end of a stereo cable into the “Line Out” or “Headphone” connector on your tape deck, minidisc player, or stereo system.  Plug the other end into your computer’s “Line In” connector.  If you do not have a cable that fits both of these connectors, you can find one at an electronics store.</p>
<p>Choose “Line In” as the input source on the Audacity toolbar, or in the Audacity Preferences (Windows Vista and 7) or Apple Audio-MIDI Setup (OS X).</p>
<p>Then press the <b>Record</b> button.  While Audacity is recording, start playing your tape or disc.  When you have captured the entire recording, press the <b>Stop</b> button.</p>
<p>Vinyl or shellac records can also be recorded into Audacity with a special type of turntable that connects to the USB port of your computer. These turntables need to be set up differently to those that connect to your line-in port. For instructions on setting up USB turntables, see <a href="http://audacityteam.org/wiki/index.php?title=USB_turntables">USB_turntables</a> on the Audacity Wiki.</p>
<p>There is also a detailed tutorial, <a href="http://audacityteam.org/wiki/index.php?title=Transferring_tapes_and_records_to_computer_or_CD">Transferring_tapes_and_records_to_computer_or_CD</a>, on the Wiki. This will guide you through the steps from recording your records, cassettes or minidiscs to exporting as an audio file and burning to CD.</p>
<p>Notes:</p>
<ul>
	<li><p>Do not plug stereo equipment into your computer’s “Microphone” port, which is designed for low-powered (“mic-level”) signals only.   Use the “Line In” port instead.</p></li>
	<li><p>Do not connect a turntable directly to your computer.  The signal from a turntable is distorted; it must be corrected by passing it through a phono pre-amp or a receiver with a “phono” input.</p></li>
</ul>
<p>See also: <a href="faq?s=files&amp;i=split">How to split a long recording into multiple files</a>.</p>').
          "<p>"._("See also:").' <a href="faq?s=files&amp;i=burn-cd">'._("How do I save my recording on an audio CD?")."</a></p>"
        ),
        "mixer-toolbar-input" => array(
          _("Why doesn't the Mixer Toolbar input selector work properly, for example on Windows 7?"),
          _("<p>The Input selector may be greyed-out, lacking the expected choices, or apparently not recording from the indicated source.</p>
<p>The cause varies by operating system and sound card driver. For example, on Windows Vista or 7, you must use the \"Recording Device\" menu in the Audio I/O tab of Preferences to select input sources. For more help, see the <a href=\"http://audacityteam.org/wiki/index.php?title=Mixer_Toolbar_Issues\">Mixer Toolbar Issues</a> page on the <a href=\"http://audacityteam.org/wiki\">Audacity Wiki</a>.</p>
")
        ),
        "playthrough" => array(
          _("Why can’t I hear what I’m recording?"),
          _("<h3>Windows and Linux</h3>
<p>To monitor your recording, open your computer's volume control panel.  Turn up the playback volume and turn off the “mute” checkbox for your recording source (usually “microphone” or “line in”).</p>
<p>If this does not work, open the Audacity preferences.  In the “Audio I/O” section, turn on the “Software Playthrough” option.</p>
<h3>Mac OS X</h3>
<p>Open the Audacity preferences.  In the “Audio I/O” section, turn on the “playthrough” option.  “Hardware Playthrough” is best if it is available.  If it does not work, you can choose “Software Playthrough” instead.</p>
<h3>Monitoring while Recording from a Microphone</h3>
<p>Always use headphones when listening as you record from a microphone, to avoid the playthrough of the recording being picked up in the recording itself.</p>
")
        ),
        "multi-track" => array(
          _("Can I play a track while recording a new one on top of it?"),
          _("<p>This is called multi-track recording.  It makes it possible to record harmonies with yourself, or add new instruments or vocals to an existing recording.  To do this in Audacity, follow these instructions:</p>
<ol>
  <li>Select the correct recording input that you are plugged into (probably microphone or line-in). On <b>Windows or Linux</b>, select the input in the dropdown menu in Audacity's <a href=\"http://audacity.sourceforge.net/onlinehelp-1.2/toolbar_mixer.htm\">Mixer Toolbar</a> (on Windows Vista or 7, use the \"Recording Device\" dropdown in the Audacity Preferences instead). On <b>OS X</b>, Mixer Toolbar may only show \"Default Input\". If so, select the source on the Audio Devices tab of Apple Audio MIDI Setup.</li>
  <li>Import or record the first track.</li>
  <li>Open the \"Audio I/O\" tab of Audacity preferences, and check the box labeled <b>Play other tracks while recording new one.</b></li>
  <li>Close the preferences and press the <b>Record</b> button. If you are recording from a microphone, make sure you use headphones so that the microphone picks up only what you are recording.</li>
</ol>
<p>See also:
	<ul>
		<li><a href=\"faq?s=recording&amp;i=sync\">Why isn't my new track in sync with the previous ones?</a></li>
		<li><a href=\"faq?s=recording&amp;i=playthrough\">Why can't I hear what I am recording?</a></li>
		<li><a href=\"faq?s=recording&amp;i=mixer-toolbar-input\">Why doesn't the Mixer Toolbar input selector work properly, for example on Windows Vista?</a></li>
	</ul>
</p>")
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
<p>In the drop-down menu on Audacity's mixer toolbar, choose “Wave Out” or “Stereo Mix” as the input source.  (The exact name may be different, depending on your computer's sound drivers.)  When you press the <b>Record</b> button, Audacity will capture whatever sound is playing on your computer's speakers. Note that on Windows Vista or 7, the “stereo mix” option must be chosen in the Audio I/O tab of Preferences, in the “Recording Device” dropdown. On Windows, if you don't have a “Wave Out” or “Stereo Mix” option, or if it won't record, go to the system Control Panel and try to enable this option there. For instructions see: <a href=\"http://audacityteam.org/wiki/index.php?title=Mixer_Toolbar_Issues#Using_the_Control_Panel\">Using the Control Panel</a> on the Wiki.</p>
<p>If this doesn't work on your computer, you can instead use a cable to connect your computer's “Line Out” (speaker) port to its “Line In” port, and use Audacity to record from Line In.</p>
<h3 id=\"mac\">Mac OS X</h3>
<p>Mac OS X users can capture streaming audio using a program like <a href=\"http://www.cycling74.com/products/soundflower\">Soundflower</a> (free, open source), <a href=\"http://www.rogueamoeba.com/audiohijack/\">Audio Hijack</a> or <a href=\"http://www.ambrosiasw.com/utilities/wiretap/\">Wiretap Pro</a>. For more help see <a href=\"http://wiki.audacityteam.org/wiki/Recording_audio_playing_on_the_computer#Mac\">this page</a> on our <a href=\"http://wiki.audacityteam.org/wiki/\">Wiki</a>.</p>
")
        ),
        "scheduled" => array(
          _("Can I set Audacity to record at a certain time?"),
          _("<p>Audacity 1.2 does not support scheduled recording, but Audacity <a href=\"../download/features-1.3-a\">1.3 Beta</a> has a <a href=\"http://audacity.sourceforge.net/download/features-1.3-k.php\">Timer Record</a> feature to start and stop recording at a particular time. </p>
<p>In 1.2, you can make Audacity <b>stop</b> recording after a certain time limit, by following these steps:</p>
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
      _("<div id=\"opensave\">Opening and Saving Files</div>"),
      array(
        "aup" => array(
          _("How do I open an Audacity project (AUP file) in another program?"),
          _("<p>Audacity project files are saved in a special format that only Audacity can open. To open your project in another program or <a href=\"faq?s=files&amp;i=burn-cd\">burn it to CD</a>, firstly open the .aup file in Audacity if you have already saved one (if you saved it recently it will be in the File > Recent Files menu). Then use the <b>Export</b> commands further down the File menu to save the audio in a standard format like WAV or AIFF.</p>
<p>To learn more about working with Audacity projects, please see our <a href=\"http://wiki.audacityteam.org/index.php?title=File_Management_Tips\">File Management Tips</a> on the Audacity <a href=\"http://wiki.audacityteam.org/index.php\">Wiki</a>.</p>   
")
        ),
        "data-folder" => array(
          _("Why does Audacity create a folder full of .au files when I save a project?"),
          _("<p>Audacity breaks long tracks into small pieces so it can edit them more efficiently. When you save an .aup project file, Audacity stores all the pieces in a _data folder that has the same name as the .aup file. For example, \"song.aup\" will open the pieces inside the \"song_data\" folder.</p>  <p><b>You should not touch the .au files yourself, or move or rename the _data folder</b>. Simply open the .aup file, then Audacity will load the .au files in the correct sequence automatically.</p> <p>To learn more about working with Audacity projects, please see our <a href=\"http://wiki.audacityteam.org/index.php?title=File_Management_Tips\">File Management Tips</a> on the Audacity <a href=\"http://wiki.audacityteam.org/index.php\">Wiki</a>.</p>").
          "<p>"._("See also:").' <a href="faq?s=files&amp;i=aup">'._("How do I open an Audacity project (AUP file) in another program?")."</a></p>"
        ),
        "crash-recovery" => array(
          _("Audacity crashed!  Can I recover any unsaved data?"),
          // i18n-hint: Sorry, these links are in English only.  If you would
          // like to make more information available, please create a page on
          // the wiki for now.
          _("<p>You may be able to recover your project automatically with the <a href=\"http://www.mesw.de/audacity/recovery/\">Audacity Recovery Utility</a>.  Please see the <a href=\"http://audacityteam.org/wiki/index.pl?CrashRecovery\">Crash Recovery</a> page for more information.</p>
<p>Please note the .au files need to be numbered consecutively when input to the Crash Recovery Utility or errors will occur in the program. If this problem occurs you can open Audacity's temporary folder in your system File Manager (e.g., Windows Explorer) and try resorting the .au files by time modified and then batch renaming them using a numeric sequence whilst they are sorted by time modified. An arrangement looking something like this should work:</p>
<ul>
	<li>b001.au 15:56:02</li>
	<li>b002.au 15:56:02</li>
	<li>b003.au 15:56:10</li>
	<li>b004.au 15:56:10</li>
</ul>
<p>If your file manager does not have a suitable renaming tool, you should be able to obtain suitable free tools on the Internet.</p>
")
        ),
        "wma-proprietary" => array(
          _("Can Audacity import additional formats like M4A, WMA or FLAC?"),
          _("<p>Audacity 1.2 <b>cannot</b> import or export <b>AC3, AMR(NB), M4A, WMA</b> or most other proprietary file formats due to licensing and patent restrictions. However, the <a href=\"../download/features-1.3-a\">Beta 1.3</a> version of Audacity can import or export these and many additional formats, if you install the optional <a href=\"http://manual.audacityteam.org/index.php?title=FAQ:Installation_and_Plug-Ins#installffmpeg\">FFmpeg</a> library. FFmpeg also allows Audacity to import audio from video files. Note that on Mac, Audacity Beta has built-in support for M4A import without installing FFmpeg, using the QuickTime components supplied by the operating system.</p>
<p><b>Digital Rights Management (DRM) protected</b> files, such as those purchased from iTunes prior to 2009, <b>cannot</b> be imported into any version of Audacity. If you use Windows Media Player to extract (rip) a CD track to WMA, ensure the option to copy-protect the WMA file is turned off.</p>
<p><b>FLAC</b> import and export support is now built into Audacity Beta, but some other open formats such as <b>Ogg Speex</b> are not yet supported.</p>
<p><b>Workarounds for importing additional or protected formats:</b> To import other than the directly supported WAV, AIFF, AU, MP2/MP3 or OGG Vorbis formats, you can always convert the file to WAV or AIFF. As long as it's not a DRM-protected file, you can <a href=\"http://wiki.audacityteam.org/wiki/How_to_import_files_from_iTunes#convert\">convert with iTunes&reg;</a> or <a href=\"http://www.erightsoft.com/SUPER.html#Dnload\">SUPER</a>. For a DRM-protected file, first burn it to an audio CD in the application that is licensed to play it, then <a href=\"http://wiki.audacityteam.org/wiki/How_to_import_CDs\">rip the CD track</a> to WAV or AIFF. You can also record the protected file - see our FAQ for <a href=\"faq?s=recording&amp;i=streaming\"> recording streaming audio</a>.</p>
<p>For more detail on supported formats, see the <a href=\"../about/features\">feature list</a>.</p>
<p>See also: <a href=\"faq?s=files&amp;i=midi\">Why can't I play MIDI files?</a></p>
")
        ),
        "midi" => array(
          _("Why can’t I play MIDI files?"),
          _("<p>Sorry, Audacity cannot play, edit, or convert MIDI files.  It can only display them visually (for comparison with recorded sounds).  We might add MIDI editing to a future version of Audacity, but for now Audacity is focused on sampled audio (like WAV files).</p>")
        ),
        "import-cd" => array(
          _("How do I import a track from an audio CD?"),
          _("<p>Audacity cannot import a track directly from an audio CD.  You must use a separate program like <a href=\"http://cdexos.sourceforge.net/\">CDex</a> or <a href=\"http://www.apple.com/itunes/\">iTunes</a> to extract CD tracks into a format that Audacity can read, like WAV or AIFF.</p>
<p>On Mac OS X computers, CD tracks appear in Finder as AIFF files so can be imported directly into Audacity. For more help on importing audio from CD tracks on both Windows and OS X, see our Wiki help page on <a href=\"http://audacityteam.org/wiki/index.php?title=How_to_import_CDs\">How_to_import_CDs</a>.</p>
<p>See also: <a href=\"faq?s=files&amp;i=burn-cd\">How do I save my recording on an audio CD?</a></p>
")
        ),
        "burn-cd" => array(
          _("How do I save my recording on an audio CD?"),
          "<p>"._("After making a recording or editing a file in Audacity, follow these steps to save your work on an audio CD:")."</p>".
          _("<ol>
  <li>Use the “Export as WAV” or “Export as AIFF” command to save your Audacity recording in a sound file.</li>
  <li>Use any CD-recording software (iTunes or Nero, for example) to burn this file to a CD.</li>
</ol>
<p>To make a disc you can play in normal CD players, make sure to create a “music” or “audio” CD (not a “data” CD).  Use CD-R discs, because some players cannot read CD-RW. You can burn only 74 minutes or so onto an audio CD – this is a limitation of the audio CD format.</p>
<p>Some CD software will burn only 16-bit, 44.1KHz stereo sound files.  If your CD recording software won't open your sound file, export the file again after choosing the following settings in Audacity:</p>
<ol>
  <li>At the bottom of the Audacity window, set the Project Rate to 44100 Hz.</li>
  <li>In the File Formats preferences, choose WAV (16-bit...) or AIFF (16-bit...).</li>
  <li>If your project does not already contain a stereo track, choose “New Stereo Track” from the Project menu.  (This will make Audacity export your recording as a stereo file.)</li>
</ol>
<p>For helpful tips on CD burning with Windows Media Player and iTunes, and help on burning longer “data” CDs, see <a href=\"http://audacityteam.org/wiki/index.php?title=How_to_burn_CDs\">How_to_burn_CDs</a> on the Audacity Wiki.</p>
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
"<p>"._("A detailed, illustrated tutorial explaining this process is available on the ").' <a href="http://audacityteam.org/wiki/index.php?title=Splitting_recordings_into_separate_tracks">Splitting recordings into separate tracks</a>'._(" page on the Audacity Wiki.")."</p>".
"<p>"._("See also:").' <a href="faq?s=files&amp;i=burn-cd">'._("How do I save my recording on an audio CD?")."</a></p>"
        ),
      ),
    ),
    "editing" => array(
      _("<div id=\"editing\">Editing</div>"),
      array(
        "menu-disabled" => array(
          _("Why can’t I use the effects or other menu items?"),
          _("<p>Some menu items are grayed out or disabled until they are ready for use.  Before choosing an effect, you must select the audio that you want to change.  To select audio, click and drag with the Selection tool to highlight it, or choose the “Select All” command from the Edit menu.</p>
<p>Also, many menu items are disabled while playing, recording, or pausing a track. To enable them, press the yellow <b>Stop</b> button.</p>")
        ),
        "join" => array(
          _("How do I combine two files into one longer file?"),
          _("<p>Follow these steps to splice two files together:</p>
<ol>
  <li>Import both files into Audacity.</li>
  <li>Select the second one by clicking on its label (the area around the mute/solo buttons).</li>
  <li>Choose “Find Zero Crossings” from the Edit menu.</li>
  <li>Choose “Cut” from the Edit menu.</li>
  <li>Place the cursor by clicking in the first track, after the end of the audio.</li>
  <li>Choose “Paste” from the Edit menu.</li>
</ol>
<p>You can press the <b>Play</b> button to hear the result, and use the Export commands (in the File menu) to save it as a sound file.</p>")
        ),
        "mix" => array(
          _("How do I mix two tracks together?"),
          _("<p>To mix two files, just import both of them into Audacity.  They will appear in separate tracks, and will be mixed together when you press the <b>Play</b> button.  You can use the Time Shift tool to move them around so that they start at different times, or use the other editing commands to alter either of the tracks.</p>")
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
<p>Press the <b>Play</b> button to hear the results.  If you are lucky, the voice will be gone but most of the other instruments will be unaffected, just like a karaoke track.  You can use the Export commands in the File menu to save the results.</p>
<p>If the vocals are not exactly the same on both stereo channels, there are some other techniques or optional plugins you can try. Please see our <a href=\"http://audacityteam.org/wiki/index.php?title=Vocal_Removal\">Vocal_Removal</a> Wiki page for more details.</p>
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