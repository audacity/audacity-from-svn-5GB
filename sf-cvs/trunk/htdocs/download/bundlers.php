<?php
/*
 * Copyright 2004 Matt Brubeck
 * Copyright 2007, 2008  Vaughan Johnson, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "bundlers";
  $pageTitle = _("Vendors and Distributors of Audacity");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_('Audacity is free to download, but vendors are also free to bundle copies of the program with their products, or resell or distribute Audacity, thanks to the <a href="../about/license">GNU General Public License</a>. The following are legitimate, legal offerings. We are happy and proud that they chose to bundle Audacity with their products.')?></p>

<p><?=_('Some vendors, <a href="http://umixit.com/aboutus.html">Umixit</a>, <a href="http://www.thinklabsmedical.com">Thinklabs</a>, and <a href="http://audiotouch.nouturn.com/">Audiotouch</a>, have additionally supported Audacity by sponsoring custom versions. The UmixIt custom version adds a mixer board interface and karaoke window. The ThinkLabs version is customized for cardiographs from digital stethoscopes. The Audiotouch version is customized for ease of use for grade school children. Many of these improvements will become part of the standard Audacity releases.')?></p>

<p><?=_('To be considered for addition to this list, please first review our <a href="../about/license">License, and Advice for Vendors and Distributors</a> page. If you are distributing Audacity under those terms and would like to be on this list, please <a href="&#109;&#97;&#x69;&#108;&#x74;&#111;&#58;&#x76;&#97;&#117;&#103;&#x68;&#x61;&#x6e;&#x40;&#x61;&#x75;&#x64;&#97;&#99;&#x69;&#x74;&#x79;&#x74;&#x65;&#97;&#x6d;&#x2e;&#x6f;&#114;&#x67;&#x3f;&#115;&#117;&#x62;&#x6a;&#x65;&#99;&#x74;&#61;&#x52;&#101;&#113;&#x75;&#x65;&#115;&#x74;&#32;&#x66;&#x6f;&#114;&#32;&#x61;&#x64;&#x64;&#105;&#x74;&#x69;&#111;&#x6e;&#32;&#x74;&#111;&#32;&#x42;&#117;&#x6e;&#x64;&#x6c;&#x65;&#114;&#x73;&#x20;&#x6c;&#105;&#x73;&#116;">contact us</a>.')?></p>


<ul id="bundlers">
  <li>
    <a href="#Hardware Bundles">Hardware Bundles</a>
  </li>
  <li>
    <a href="#Interfaces">Interfaces</a>
  </li>
  <li>
    <a href="#Musical Instruments">Musical Instruments</a>
  </li>
  <li>
    <a href="#Playback Devices">Playback Devices</a>
  </li>
  <li>
    <a href="#Others">Others</a>
  </li>
</ul>


<a name="Hardware Bundles"></a>
<h3>Hardware Bundles</h3>
<table border=2 bordercolor=#E5E5E5 cellpadding=8 rules=all frame=box style="border-collapse: collapse">
  <tr>
    <th><?=_("Vendor")?></th>
    <th><?=_("Product")?></th>
    <th><?=_("Description")?></th>
  </tr>
  <tr>
 <td width="15%" valign="top">
      <a href="http://www.behringer.com">Behringer</a>
    </td>
    <td width="25%" valign="top">
      <a href="http://www.behringer.com/PODCASTUDIO-FIREWIRE">PODCASTUDIO FIREWIRE</a>
    </td>
    <td valign="top">
      <?=_("FireWire&reg; audio interface, 8-input mixer, headphones, studio microphone with stand, cables and software.")?></td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.behringer.com">Behringer</a>
    </td>
    <td valign="top">
      <a href="http://www.behringer.com/PODCASTUDIO-USB">PODCASTUDIO USB</a>
    </td>
    <td valign="top"><?=_("USB audio interface, 8-input mixer, headphones, studio microphone with stand, cables and software.")?></td>
  </tr>
  <tr>
    <td valign="top">
		  <a href="http://www.m-audio.com">M-Audio</a>
		</td>
		<td valign="top">
      <?=_("Podcast Factory is now discontinued, but generally available online from retailers.")?>
    </td>
		<td valign="top"><?=_("Hardware and software to record, edit and publish podcasts: 24-bit/48kHz audio interface, dynamic broadcast microphone with desk stand. NOTE: Currently Podcast Factory will not work with Windows XP Media Center Edition (XP Home and Pro are not affected). Audacity itself does work with Media Center Edition, subject to your computer having suitable sound device drivers specific to your hardware.")?></td>
  </tr>
  <tr>
		<td valign="top">
		  <a href="http://www.pinnaclesys.com">Pinnacle Systems</a>
		</td>
		<td valign="top">
		  <a href="http://www.pinnaclesys.com/PublicSite/uk/Products/Consumer+Products/Audio/M-Audio/Family?code=UKMaudio">Podcast Factory</a>
		</td>
    <td valign="top">
      <?=_("Hardware and software to record, edit and publish podcasts: 24-bit/48kHz audio interface, dynamic broadcast microphone with desk stand. NOTE: Currently Podcast Factory will not work with Windows XP Media Center Edition (XP Home and Pro are not affected). Audacity itself does work with Media Center Edition, subject to your computer having suitable sound device drivers specific to your hardware.")?>
    </td>
  </tr>
</table>


<a name="Interfaces"></a>
<h3>Interfaces</h3>
<table border=2 bordercolor=#E5E5E5 cellpadding=8 rules=all frame=box style="border-collapse: collapse">
  <tr>
    <th><?=_("Vendor")?></th>
    <th><?=_("Product")?></th>
    <th><?=_("Description")?></th>
  </tr>
  <tr>
    <td width="15%" valign="top">
      <a href="http://www.artproaudio.com">Applied Research and Technology</a>
    </td>
    <td width="25%" valign="top">
      <a href="http://www.artproaudio.com/products.asp?type=90&cat=13&id=128">USBPhonoPlus v2</a>
    </td>
    <td valign="top"><?=_("Interface for analog or digital audio source into computer via USB")?></td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.behringer.com">Behringer</a>
    </td>
    <td valign="top">
      <a href="http://www.behringer.com/FCA202">F-CONTROL AUDIO FCA202</a>
    </td>
    <td valign="top"><?=_("2 In/2 Out 24-Bit/96 kHz FireWire&reg; audio interface")?></td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.citronic.co.uk">Citronic</a>
    </td>
    <td valign="top">
      <a href="http://www.citronic.co.uk/product/product.php?s=128.515">AC-1USB Audio Capture Device</a>
    </td>
    <td valign="top"><?=_("Interface to connect turntable, mixer, or CD player to USB or analog")?></td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.diamondmm.com">Diamond Multimedia</a>
    </td>
    <td valign="top">
      <a href="http://www.diamondmm.com/XS51.php">XS51</a>
    </td>
    <td valign="top"><?=_("XtremeSound 5.1/16 bit Sound Card")?></td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.diamondmm.com">Diamond Multimedia</a>
    </td>
    <td valign="top">
      <a href="http://www.diamondmm.com/XS71.php">7.1/24 bit</a><br>
      <a href="http://www.diamondmm.com/XS71DDL.php">7.1/24 bit with Dolby DL</a>
    </td>
    <td valign="top">
      <?=_("XtremeSound 7.1/24 bit and 7.1/24 bit Dolby Digital Live&reg; Sound Cards")?>
    </td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.kam.co.uk">KAM</a>
    </td>
    <td valign="top">
      <a href="http://www.djsuperstore.co.uk/item/computer-hardware-software/645868-kam-usbh100-audio-capture-device-%C2%A349.99">USBH100</a>
    </td>
    <td valign="top">
      <?=_("Audio Capture Device")?>
    </td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.presonus.com">Presonus Audio Electronics</a>
    </td>
    <td valign="top">
      <a href="http://www.presonus.com/inspire1394.html">Inspire 1394</a>
    </td>
    <td valign="top">
      <?=_("FireWire&reg; recording system featuring 24-bit/96k analog to digital conversion rate, four simultaneous inputs, software control panel, audio recording and production software")?>
    </td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://terratec.com/">TerraTec Electronic</a>
    </td>
    <td valign="top">
      <a href="http://sounden.terratec.net/">DMX 6fire USB</a>
    </td>
    <td valign="top">
      <?=_("USB 2.0 external audio system for musicians, DJs and gamers, with: high end sound (24 Bit/192 kHz), 4 analog Inputs, 6 analog Outputs, XLR-Microphone-Input with gain control, Instrument-Input with gain control, Phono-Input with RIAA equaliser and gain control, Optical digital I/O, Coaxial digital digitaler I/O, MIDI interface, ASIO 2.0 support, and Stereo to 5.1 Surround Expander")?>
    </td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://westek.com">Westek</a>
    </td>
    <td valign="top">
      <a href="http://www.google.com/search?q=Westek+TeleTool+2000">Westek TeleTool 2000 PC/Telephone Recorder</a>
    </td>
    <td valign="top"><?=_("Record telephone conversations from phone to PC.")?></td>
  </tr>
</table>


<a name="Musical Instruments"></a>
<h3>Musical Instruments</h3>
<table border=2 bordercolor=#E5E5E5 cellpadding=8 rules=all frame=box style="border-collapse: collapse">
  <tr>
    <th><?=_("Vendor")?></th>
    <th><?=_("Product")?></th>
    <th><?=_("Description")?></th>
  </tr>
  <tr>
    <td width="15%" valign="top">
      <a href="http://www.behringer.com">Behringer</a>
    </td>
    <td width="25%" valign="top">
      <a href="http://www.behringer.com/UCG102">GUITAR LINK UCG102</a>
    </td>
    <td valign="top"><?=_("Guitar-to-USB interface with modeling amps and effects.")?></td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.behringer.com">Behringer</a>
    </td>
    <td valign="top">
      <a href="http://www.behringer.com/IAXE393">iAXE393</a><br>
      <a href="http://www.behringer.com/IAXE393-BK">iAXE393-BK</a><br>
      <a href="http://www.behringer.com/IAXE624-BD">iAXE624-BD CENTARI</a><br>
      <a href="http://www.behringer.com/IAXE624-BK">iAXE624-BK CENTARI</a><br>
      <a href="http://www.behringer.com/IAXE629-BKLS">iAXE629-BKLS METALIEN</a>
      </td>
      <td valign="top"><?=_("Various models of electric guitar with built-in USB port, modeling amps and effects.")?></td>
     </tr>
     <tr>
     <td valign="top">
      <a href="http://www.behringer.com">Behringer</a>
	     </td>
	     <td valign="top">
           <a href="http://www.behringer.com/UMA25S">U-CONTROL UMA25S</a>
    </td>
    <td valign="top"><?=_("Ultra-slim 25-Key USB/MIDI controller keyboard with internal audio interface, more than 100 software plug-ins and Ableton Live Lite 4 BEHRINGER Edition.")?></td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.behringer.com">Behringer</a>
    </td>
    <td valign="top">
      <a href="http://www.behringer.com/UMX25">U-CONTROL UMX25</a><br>
      <a href="http://www.behringer.com/UMX49">U-CONTROL UMX49</a><br>
      <a href="http://www.behringer.com/UMX61">U-CONTROL UMX61</a>
    </td>
    <td valign="top"><?=_("25-, 49- or 61-key USB/MIDI controller keyboard with internal audio interface, more than 100 software plug-ins and Ableton Live Lite 4 BEHRINGER Edition.")?></td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.behringer.com">Behringer</a>
    </td>
    <td valign="top">
      <a href="http://www.behringer.com/V-AMP2">V-AMP 2</a>
    </td>
    <td valign="top"><?=_("Virtual guitar amplifier with tube simulation and 24-bit effects.")?></td>
  </tr>
</table>


<a name="Playback Devices"></a>
<h3>Playback Devices</h3>
<table border=2 bordercolor=#E5E5E5 cellpadding=8 rules=all frame=box style="border-collapse: collapse">
  <tr>
    <th><?=_("Vendor")?></th>
    <th><?=_("Product")?></th>
    <th><?=_("Description")?></th>
  </tr>
  	  <tr>
  		<td valign="top" width=15%>
  		  <a href="http://www.ion-audio.com">Ion Audio</a>
  		</td>
  		<td valign="top" width=25%>
  		  <a href="http://www.ion-audio.com/ittusb.php">iTTUSB</a><br><a 
href="http://www.ion-audio.com/ionttusb05">iTTUSB05</a><br><a 
href="http://www.ion-audio.com/ionttusb10">iTTUSB10</a><br><a 
href="http://www.ion-audio.com/extras/miscfiles/turntable_comparison.php?tp=VE1HUj0xLHRpZD05MjUwLA%3D%3D">(Feature 
comparison)</a><br><br><a href="http://www.ion-audio.com/iptusb">iPTUSB battery operated</a>
  		</td>
  		<td valign="top"><?=_('USB turntables. NOTE: In January 2007, an <a href="http://select.nytimes.com/gst/abstract.html?res=F40E1EFA35540C728EDDA80894DF404482">article about these turntables</a> appeared in the New York Times, containing erroneous statements about limitations of Audacity. Despite what was stated, Audacity can record a whole LP, detect split points between LP tracks based on silences, and simultaneously export multiple files for each track. Ion have corrected their documentation, and are currently still bundling Audacity with the listed products. Please see our Wiki page on <a href="http://audacityteam.org/wiki/index.php?title=USB_turntables">USB turntables</a> for more information.')?></td>
  	  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.kam.co.uk">KAM</a>
    </td>
    <td valign="top">
      <a href="http://www.kam.co.uk/products/turntables/bx900usb.htm">BDX900 USB</a>
    </td>
    <td valign="top"><?=_("USB turntable")?></td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.lenco.de">Lenco</a>
    </td>
    <td valign="top">
      <a href="http://www.lenco.de/en/products/7/Turntable/444/TURNTABLE_WITH_USB_CONNECTION">L-3866 USB</a>
    </td>
    <td valign="top"><?=_("USB turntable")?></td>
  </tr>
	  </tr>
	  <tr>
		<td valign="top">
		  <a href="http://www.numark.com">Numark</a>
		</td>
		<td valign="top">
		  <a href="http://www.numark.com/ttusb">TTUSB</a>
		</td>
		<td valign="top"><?=_('USB turntables. NOTE: In January 2007, an <a href="http://select.nytimes.com/gst/abstract.html?res=F40E1EFA35540C728EDDA80894DF404482">article about these turntables</a> appeared in the New York Times, containing erroneous statements about limitations of Audacity. Despite what was stated, Audacity can record a whole LP, detect split points between LP tracks based on silences, and simultaneously export multiple files for each track. Numark have corrected their documentation, and are currently still bundling Audacity with the listed product. Please see our Wiki page on <a href="http://audacityteam.org/wiki/index.php?title=USB_turntables">USB turntables</a> for more information.')?></td>
	  </tr>
</table>


<a name="Others"></a>
<h3>Others</h3>
<table border=2 bordercolor=#E5E5E5 cellpadding=8 rules=all frame=box style="border-collapse: collapse">
  <tr>
  <th><?=_("Vendor")?></th>
  <th><?=_("Product")?></th>
  <th><?=_("Description")?></th>
  </tr>
  <tr>
    <td>
      <a href="http://audiotouch.nouturn.com/">Audiotouch</a>
    </td>
    <td width="25%" valign="top">
      <a href="http://audiotouch.nouturn.com/">Audiotouch</a>
    </td>
    <td valign="top">
      <?=_("Audiotouch is sound recording and playback software, designed for ease of use. It features a unique one-touch recording / saving feature, so you can quickly record multiple audio files directly to disk, without the need to interact with any save dialog boxes.")?>
    </td>
  </tr>
  <tr>
    <td width="15%" valign="top">
      <a href="http://www.myplanetwide.com/">Planetwide Games</a>
    </td>
    <td width="25%" valign="top">
      <a href="http://www.mycomicbookcreator.com">Comic Book Creator</a>
    </td>
    <td valign="top"><?=_("Comic Book Creator is software that allows users to create their own custom comic books with included clipart, backgrounds, word balloons, sound files, and other  assets.")?></td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.theopencd.org">TheOpenCD</a>
	  </td>
    <td valign="top">
      <a href="http://releases.theopencd.org/">OpenCD downloadable CD images</a>
    </td>
    <td valign="top"><?=_('TheOpenCD was a collection of high quality free and open source software (including Audacity) for Microsoft Windows. The project is now discontinued, but archived CD images are still available from the link to left. Products from TheOpenCD are now in use by alternative projects including <a href="http://ubuntu.com/">Linux Ubuntu.')?></td>
   </tr>
  <tr>
    <td valign="top">
      <a href="http://theopendisc.com/">TheOpenDisc</a>
    </td>
    <td valign="top"><a href="http://downloads.sourceforge.net/opendisc/OpenDisc-07.10.iso?use_mirror=osdn">OpenDisc fownloadable CD image</a></td>
    <td valign="top"><?=_('TheOpenDisc is a successor to TheOpenCD, a collection of high quality free and open source <a href="http://theopendisc.com/programs">software</a> (including Audacity) for Microsoft Windows.')?></td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.thinklabsmedical.com">ThinkLabs</a>
    </td>
    <td valign="top">
      <a href="http://www.thinklabsmedical.com/content/view/17/157/">ds32a digital stethoscope</a><br><br>
      <a href="http://www.thinklabsmedical.com/content/view/72/161/">Thinklabs Phonocardiography</a>
    </td>
    <td valign="top"><?=_('The ds32a digital stethoscope has "enhanced sound quality, with 50X amplification. Adjust volume for faint heart sounds, obese patients, noisy work environments". The waveforms and spectrograms can be examined in the Phonocardiography software for Microsoft Windows and Mac OS X, powered by Audacity.')?></td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.trinityaudiogroup.com">Trinity Audio Group</a>
    </td>
    <td valign="top">
      <a href="http://www.trinityaudiogroup.com/home.html">Trinity</a>
    </td>
    <td valign="top"><?=_('Portable recording studio or (DAW) digital audio workstation, featuring "walk and mix" technology.')?></td>
  </tr>
  <tr>
	<td valign="top">
	  <a href="http://umixit.com/aboutus.html">Umixit</a>
	</td>
	<td valign="top">
	  <a href="http://www.umixit.com/download.html">Umixit powered by Audacity</a>
	</td>
	<td valign="top"><?=_('<em>Umixit powered by Audacity<em> for Microsoft Windows and Mac OS X allows the fan to share the studio experience with an artist. Add a Umixit-enabled CD or song, then record your own vocals or instruments, create remixes, mash-ups, sing karaoke and more...')?></td>
  </tr>
  <tr>
    <td valign="top">
      <a href="http://www.forest.impress.co.jp">Windows Forest</a>
    </td>
    <td valign="top">
      <a href="http://www.forest.impress.co.jp/lib/pic/music/soundedit/audacity.html">Audacity</a>
    </td>
    <td valign="top"><?=_("Mirror site for Audacity downloads, from webzine about Windows software at Impress Watch Corporation, Tokyo, Japan.")?></td>
  </tr>
</table>

<?php
  include "../include/footer.inc.php";
?>
