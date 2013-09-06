<?php
/*
 * Copyright 2004 Matt Brubeck
 * Copyright 2007 - 2013  Vaughan Johnson, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "bundlers";
  $pageTitle = _("Vendors and Distributors of Audacity");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_('Audacity is free to download, but many vendors bundle copies of the program with their products, or resell or distribute Audacity, thanks to the <a href="../about/license">GNU General Public License</a>. The following are legitimate, legal offerings from companies that have worked with us. We are happy and proud that they chose to bundle Audacity with their products.')?></p>

<p><?=_('Some vendors, Umixit, <a href="http://www.thinklabsmedical.com">Thinklabs</a>, and <a href="http://audiotouch.com.au/">Audiotouch</a>, have additionally supported Audacity by sponsoring <a href="#others">custom versions</a>. The UmixIt custom version added a mixer board interface and karaoke window which are now incorporated into Audacity. The ThinkLabs version is customized for cardiographs from digital stethoscopes. Audacity_Voice is customized for integration with Audiotouch, for ease of use by students needing numerous recordings. Some of these improvements will be incorporated in future versions of Audacity.')?></p>

<p>
  <?php echo _('To be considered for addition to this list, please first review our <a href="../about/license">License, and Advice for Vendors and Distributors</a> page. If you are distributing Audacity under those terms ');?>
  <?php
  // i18n-hint: the encoding inside the <a href> tag between the  
  // two "echo" strings obscures the email address from (at least some) 
  // harvesting bots. Only the two strings above and below the
  // <a href> tag need to be translated.   
  echo _('and would like to be on this list, please ');?></a>
  <a href="&#109;&#97;&#x69;&#x6c;&#116;&#x6f;&#x3a;&#103;&#97;&#x6c;&#x65;&#64;&#x61;&#x75;&#100;&#97;&#x63;&#x69;&#116;&#x79;&#x74;&#101;&#x61;&#x6d;&#46;&#x6f;&#x72;&#103;&#63;&#x73;&#117;&#98;&#106;&#101;&#x63;&#x74;&#61;&#x52;&#x65;&#113;&#117;&#x65;&#x73;&#x74;&#x20;&#x66;&#111;&#114;&#x20;&#x61;&#100;&#x64;&#105;&#x74;&#x69;&#x6f;&#x6e;&#x20;&#116;&#x6f;&#32;&#66;&#x75;&#x6e;&#x64;&#108;&#x65;&#114;&#115;&#32;&#x4c;&#105;&#115;&#116;&#x20;">
  <?php
  echo _('contact us');?></a>.</p>

<ul id="bundlers">
  <li>
   <a href="#hardware"><?=_('Hardware Bundles')?></a>
  </li>
  <li>
   <a href="#interfaces"><?=_('Interfaces and Sound Cards')?></a>
  </li>
  <li>
    <a href="#instruments"><?=_('Musical Instruments')?></a>
  </li>
  <li>
    <a href="#playback"><?=_('Playback Devices')?></a>
  </li>
  <li>
    <a href="#cd"><?=_('Open Source CD Bundles')?></a>
  </li>
  <li>
    <a href="#others"><?=_('Others')?></a>
  </li>
</ul>

<h3 id="hardware"><?=_('Hardware Bundles')?></h3>
<table class="bundlers">
  <tr>
   <th><?=_("Vendor")?></th>
   <th><?=_("Product")?></th>
   <th><?=_("Description")?></th>
  </tr>
  <tr>
   <td class="col1">
   <a href="http://www.behringer.com">Behringer</a>
   </td>
   <td class="col2">
   <a href="http://www.behringer.com/EN/Products/PODCASTUDIO-FIREWIRE.aspx">PODCASTUDIO FIREWIRE</a>
   </td>
   <td class="col3">
   <?=_("FireWire&reg; audio interface with 8-input mixer, plus headphones, studio microphone with stand, cables and software.")?></td>
  </tr>
  <tr>
   <td>
   <a href="http://www.behringer.com">Behringer</a>
   </td>
   <td>
   <a href="http://www.behringer.com/EN/Products/PODCASTUDIO-USB.aspx">PODCASTUDIO USB</a>
   </td>
   <td><?=_("USB audio interface with 8-input mixer, plus headphones, studio microphone with stand, cables and software.")?></td>
  </tr>
  <tr>
   <td>
   <a href="http://www.m-audio.com">M-Audio</a>
   </td>
   <td>
    Podcast Factory
    </td>
	<td>
    <?=_("Now discontinued, but generally still available online from retailers in USA and Canada. Hardware and software to record, edit, and publish podcasts: 24-bit/48kHz audio interface, dynamic broadcast microphone with desk stand. NOTE: Currently Podcast Factory will not work with Windows XP Media Center Edition (XP Home and Pro are not affected). Audacity itself does work with Media Center Edition, subject to your computer having suitable sound device drivers specific to your hardware.")?>
   </td>
  </tr>
  <tr>
   <td>
   <a href="http://www.pinnaclesys.com/PublicSite/uk/Home/">Pinnacle Systems</a>
   </td>
   <td>
   <a href="http://www.pinnaclesys.com/PublicSite/uk/Products/Consumer+Products/Audio/M-Audio/Family?code=UKMaudio">Podcast Factory</a>
   </td>
   <td>
   <?=_("Available in Europe. Hardware and software to record, edit and publish podcasts: 24-bit/48kHz audio interface, dynamic broadcast microphone with desk stand. NOTE: Currently Podcast Factory will not work with Windows XP Media Center Edition (XP Home and Pro are not affected). Audacity itself does work with Media Center Edition, subject to your computer having suitable sound device drivers specific to your hardware.")?>
   </td>
  </tr>
  <tr>
   <td>
   <a href="http://www.twistedlincoln.com/">Twisted Lincoln, Inc.</a>
   </td>
   <td>
   <a href="http://www.twistedlincoln.com/shop/catalog/computers">Desktop and Laptop Computers</a>
   </td>
   <td>
   <?=_('Twisted Lincoln, Inc. sells reconditioned computers online which are preloaded with Audacity and other open source software from the company\'s <a href="#freedom">FreedomSampler&trade;</a> CD.')?>
   </td>
  </tr>
</table>

<h3 id="interfaces"><?=_('Interfaces and Sound Cards')?></h3>
<table class="bundlers">
  <tr>
   <th><?=_("Vendor")?></th>
   <th><?=_("Product")?></th>
   <th><?=_("Description")?></th>
  </tr>
  <tr>
   <td class="col1">
   <a href="http://www.artproaudio.com">Applied Research and Technology</a>
   </td>
   <td class="col2">
   <a href="http://artproaudio.com/discontinued_products/discontinued_products/product/usb_phono_plus_v2/">USBPhonoPlus v2</a>
   </td>
   <td class="col3"><?=_("USB Interface for transferring an analog or digital audio source into computer via USB.")?></td>
  </tr>
  <tr>
   <td>
   <a href="http://www.behringer.com">Behringer</a>
   </td>
   <td>
   <a href="http://www.behringer.com/EN/Products/FCA202.aspx">F-CONTROL AUDIO FCA202</a>
   </td>
   <td><?=_("2 In/2 Out 24-Bit/96 kHz FireWire&reg; audio interface.")?></td>
  </tr>
  <tr>
   <td>
   <a href="http://www.citronic.co.uk">Citronic</a>
   </td>
   <td>
   <a href="http://avsl-citronic.com/product/128.515UK">AC-1USB Audio Capture Device</a>
   </td>
   <td><?=_("USB interface to connect turntable, mixer or CD player to USB or analog.")?></td>
  </tr>
  <tr>
   <td>
   <a href="http://www.diamondmm.com">Diamond Multimedia</a>
   </td>
   <td>
   <a href="http://www.diamondmm.com/XS51.php">XS51</a>
   </td>
   <td><?=_("XtremeSound 5.1/16 bit Sound Card.")?></td>
  </tr>
  <tr>
   <td>
   <a href="http://www.diamondmm.com">Diamond Multimedia</a>
   </td>
   <td>
   <a href="http://www.diamondmm.com/XS71.php">7.1/24 bit</a><br>
   <a href="http://www.diamondmm.com/XS71DDL.php">7.1/24 bit with Dolby DL</a>
   </td>
   <td>
   <?=_("XtremeSound 7.1/24 bit and 7.1/24 bit Dolby Digital Live&reg; Sound Cards.")?>
   </td>
  </tr>
  <tr>
   <td>
   <a href="http://www.kam.co.uk">KAM</a>
   </td>
   <td>
   <a href="http://www.kam.co.uk/index.php?action=product&product_id=24">USBH100</a>
   </td>
   <td>
   <?=_("USB Audio Capture Device.")?>
   </td>
  </tr>
  <tr>
   <td>
   <a href="http://www.presonus.com">Presonus Audio Electronics</a>
   </td>
   <td>
   <a href="http://www.presonus.com/products/Inspire-1394">Inspire 1394</a>
   </td>
   <td>
   <?=_("Now discontinued FireWire&reg; recording system featuring 24-bit/96k analog to digital conversion rate, four simultaneous inputs, software control panel, audio recording and production software.")?>
   </td>
  </tr>
  <tr>
   <td>
   <a href="http://terratec.com/">TerraTec Electronic</a>
   </td>
   <td>
   <a href="http://www.terratec.net/en/products/DMX_6Fire_USB_2084.html">DMX 6fire USB</a>
   </td>
   <td>
   <?=_("USB 2.0 external audio system for musicians, DJs and gamers, with: high end sound (24 Bit/192 kHz), 4 analog Inputs, 6 analog Outputs, XLR-Microphone-Input with gain control, Instrument-Input with gain control, Phono-Input with RIAA equaliser and gain control, Optical digital I/O, Coaxial digital digitaler I/O, MIDI interface, ASIO 2.0 support, and Stereo to 5.1 Surround Expander.")?>
   </td>
  </tr>
  <tr>
   <td>
   <a href="http://westek.com">Westek</a>
   </td>
   <td>
   Westek Tele-Tool 2000 PC/Telephone Recorder
   </td>
   <td><?=_("Record conversations to PC from any phone with a modular handset.")?></td>
  </tr>
</table>

<h3 id="instruments"><?=_('Musical Instruments')?></h3>
<table class="bundlers">
  <tr>
   <th><?=_("Vendor")?></th>
   <th><?=_("Product")?></th>
   <th><?=_("Description")?></th>
  </tr>
  <tr>
   <td class="col1">
   <a href="http://www.behringer.com">Behringer</a>
   </td>
   <td class="col2">
   <a href="http://www.behringer.com/EN/Products/UCG102.aspx">GUITAR LINK UCG102</a>
   </td>
   <td class="col3"><?=_("Guitar-to-USB interface with modeling amps and effects.")?></td>
  </tr>
  <tr>
   <td>
   <a href="http://www.behringer.com">Behringer</a>
   </td>
   <td>
   <a href="http://www.behringer.com/EN/Products/IAXE393.aspx">iAXE393</a><br>
   <a href="http://www.behringer.com/EN/Products/IAXE624-BK.aspx">iAXE624-BK</a><br>
   <a href="http://www.behringer.com/EN/Products/IAXE629-BKLS.aspx">iAXE629-BKLS</a>
   </td>
   <td><?=_("Various models of electric guitar with built-in USB port, modeling amps and effects.")?></td>
  </tr>
  <tr>
   <td>
   <a href="http://www.behringer.com">Behringer</a>
   </td>
   <td>
   <a href="http://www.behringer.com/EN/Products/UMA25S.aspx">U-CONTROL UMA25S</a>
   </td>
   <td><?=_("Ultra-slim 25-Key USB/MIDI controller keyboard with internal audio interface, more than 100 software plug-ins and Ableton Live Lite 4 BEHRINGER Edition.")?></td>
  </tr>
  <tr>
   <td>
   <a href="http://www.behringer.com">Behringer</a>
   </td>
   <td>
   <a href="http://www.behringer.com/EN/Products/UMX25.aspx">U-CONTROL UMX25</a><br>
   <a href="http://www.behringer.com/EN/Products/UMX49.aspx">U-CONTROL UMX49</a><br>
   <a href="http://www.behringer.com/EN/Products/UMX61.aspx">U-CONTROL UMX61</a>
   </td>
   <td><?=_("25-, 49- or 61-key USB/MIDI controller keyboard with internal audio interface, more than 100 software plug-ins and Ableton Live Lite 4 BEHRINGER Edition.")?></td>
  </tr>
  <tr>
   <td>
   <a href="http://www.behringer.com">Behringer</a>
   </td>
   <td>
   <a href="http://www.behringer.com/EN/Products/V-AMP2.aspx">V-AMP 2</a>
   </td>
   <td><?=_("Virtual guitar amplifier with tube simulation and 24-bit effects.")?></td>
  </tr>
</table>

<h3 id="playback"><?=_('Playback Devices')?></h3>
<table class="bundlers">
  <tr>
   <th><?=_("Vendor")?></th>
   <th><?=_("Product")?></th>
   <th><?=_("Description")?></th>
  </tr>
  <tr>
   <td class="col1">
   <a href="http://www.area-powers.jp/">Area Co.Ltd.</a>
   </td>
   <td class="col2">
   <a href="http://www.u-lexnet.info/items/others/UL-TAPPfeature.html">UL-TAPP</a>
   </td>
   <td class="col3"><?=_("USB Cassette Player.")?></td>
  </tr>
  <tr>
   <td class="col1">
   <a href="http://www.goldsound.com.hk/index.php">Goldsound</a>
   </td>
   <td class="col2">
   <a href="http://www.goldsound.com.hk/product318672.html">TT-990PC</a>
   </td>
   <td class="col3"><?=_("USB Turntable.")?></td>
  </tr>
  <tr>
   <td>
   <a href="http://www.ion-audio.com">Ion Audio</a>
   </td>
   <td>
   USB <a href="http://www.ionaudio.com/products/audio-conversion/turntables">
   Turntables</a> and <a href="http://www.ionaudio.com/products/audio-conversion/cassette-players">
   Cassette Players</a>
   </td>
   <td><?=_('As one of the major manufacturers of USB playback equipment, Ion bundled Audacity with the listed products for several years, helping to establish the wide popularity of digitizing LPs and cassettes. NOTE: In January 2007, an <a href="http://select.nytimes.com/gst/abstract.html?res=F40E1EFA35540C728EDDA80894DF404482">article about these turntables</a> appeared in the New York Times, containing erroneous statements about limitations of Audacity. Ion later corrected their documentation to clarify that Audacity could record a whole LP, detect split points between LP tracks based on silences and simultaneously export multiple files for each track. See the Audacity Wiki page about <a href="http://wiki.audacityteam.org/wiki/USB_turntables">USB turntables</a> for more information.')?>
   </td>
  </tr>
  <tr>
   <td>
   <a href="http://www.kam.co.uk">KAM</a>
   </td>
   <td>
   <a href="http://www.kam.co.uk/index.php?action=product&product_id=20">BDX900 USB</a>
   </td>
   <td><?=_("USB Turntable.")?></td>
  </tr>
  <tr>
   <td>
   <a href="http://www.lenco.eu/index.php?lang=en">Lenco</a>
   </td>
   <td>
   <a href="http://www.lenco.eu/index.php?option=com_content&view=article&id=3%3Al-3866-usb&catid=1%3Aplatenspelers&Itemid=3&lang=en">L-3866 USB</a>
   </td>
   <td><?=_("USB Turntable.")?></td>
  </tr>
  <tr>
   <td>
   <a href="http://www.numark.com">Numark</a>
   </td>
   <td>
   <a href="http://www.numark.com/turntables3">USB Turntables</a>
   </td>
   <td><?=_('As one of the major manufacturers of USB turntables, Numark bundled Audacity with the listed products for several years, helping to establish the wide popularity of digitizing LPs. NOTE: In January 2007, an <a href="http://select.nytimes.com/gst/abstract.html?res=F40E1EFA35540C728EDDA80894DF404482">article about these turntables</a> appeared in the New York Times, containing erroneous statements about limitations of Audacity. Numark later corrected their documentation to clarify that Audacity could record a whole LP, detect split points between LP tracks based on silences and simultaneously export multiple files for each track.  See the Audacity Wiki page about <a href="http://wiki.audacityteam.org/wiki/USB_turntables">USB turntables</a> for more information.')?>
   </td>
  </tr>
</table>

<h3 id="cd"><?=_('Open Source CD Bundles')?></h3>
<table class="bundlers">
  <tr>
   <th><?=_("Vendor")?></th>
   <th><?=_("Product")?></th>
   <th><?=_("Description")?></th>
  </tr>
  <tr>
   <td id="freedom" class="col1">
   <a href="http://www.twistedlincoln.com/">Twisted Lincoln, Inc.</a>
   </td>
   <td class="col2">
   <a href="http://www.freedomsampler.com/">FreedomSampler&trade;</a>
   </td>
   <td class="col3">
   <?=_('FreedomSampler&trade; is a collection of free, open source software for Windows and Linux with full source code, allowing easy redistribution under the GNU General Public License. ISO images may be downloaded for free, or you can <a href="http://www.twistedlincoln.com/shop/catalog/products/freedomsampler">purchase</a> a copy to be shipped to you.')?>
   </td>
  </tr>
  <tr>
   <td class="col1">
   <a href="http://www.theopencd.org/">TheOpenCD.org</a>
   </td>
   <td class="col2">
   <a href="http://www.theopencd.org/">TheOpenCD</a>
   </td>
   <td class="col3">
   <?=_('TheOpenCD is a now-discontinued collection of high quality free and open source software for Windows. Currently, you can still <a href="http://releases.theopencd.org/">download archived CD images</a> or <a href="http://www.thelinuxshop.co.uk/catalog/product_info.php?products_id=189">purchase a CD online</a> for worldwide shipping.')?>
   </td>
  </tr>
  <tr>
   <td class="col1">
   <a href="http://theopendisc.com/">TheOpenDisc.com</a>
   </td>
   <td class="col2">
   <a href="http://theopendisc.com/">OpenDisc</a>
   </td>
   <td class="col3">
   <?=_('OpenDisc is a similar collection of high quality free and open source software for Windows. Currently, a downloadable <a href="http://downloads.sourceforge.net/opendisc/OpenDisc-07.10.iso?use_mirror=osdn">CD image</a> is available. The project hopes to make available a purchasable CD shortly.')?>
   </td>
  </tr>
  <tr>
   <td class="col1">
   <a href="http://www.valo-cd.net/">VALO-CD.net</a>
   </td>
   <td class="col2">
   <a href="http://www.valo-cd.net/get/">VALO-CD</a>
   </td>
   <td class="col3">
   <?=_('VALO-CD contains twenty-two open-source Windows programs on a single CD. You can download and burn the ISO image, or purchase the CD for worldwide shipping.')?>
   </td> 
  </tr>
</table>

<h3 id="others"><?=_('Others')?></h3>
<table class="bundlers">
  <tr>
   <th><?=_("Vendor")?></th>
   <th><?=_("Product")?></th>
   <th><?=_("Description")?></th>
  </tr>
  <tr>
   <td class="col1">
   <a href="http://audiotouch.com.au/">Audiotouch</a>
   </td>
   <td class="col2">
   <a href="http://audiotouch.com.au/">Audiotouch</a>
   </td>
   <td class="col3">
   <?=_("Audiotouch is sound recording and playback software, designed for ease of use. It features one-touch recording and saving, quickly recording multiple audio files directly to disk without the need to interact with save dialog boxes. Audacity_Voice is a customized version of Audacity which can be added to Audiotouch, offering simple editing with reduced menu dialogs.")?>
   </td>
  </tr>
  <tr>
   <td>
   <a href="http://www.myplanetwide.com/">Planetwide Games</a>
   </td>
   <td>
   <a href="http://www.mycomicbookcreator.com">Comic Book Creator</a>
   </td>
   <td><?=_("Software allowing users to create their own custom comic books with included clipart, backgrounds, word balloons, sound files and other  assets.")?></td>
  </tr>
  <tr>
   <td>
   <a href="http://www.thinklabsmedical.com">ThinkLabs</a>
   </td>
   <td>
   <a href="http://www.thinklabsmedical.com/software-download.html">Thinklabs Phonocardiography</a>
   </td>
   <td><?=_('The Thinklabs <a href="http://www.thinklabsmedical.com/electronic-stethoscope.html">ds32a digital stethoscope</a> has enhanced sound quality with 50x amplification. Volume can be adjusted for faint heart sounds, obese patients or noisy work environments. The stethoscope\'s waveforms and spectrograms can be analyzed in Thinklabs Phonocardiography (a customized version of Audacity for Microsoft Windows and Mac OS X).')?></td>
  </tr>
  <tr>
   <td>
   <a href="http://www.trinityaudiogroup.com">Trinity Audio Group</a>
   </td>
   <td>
   <a href="http://www.trinityaudiogroup.com/home.html">Trinity</a>
   </td>
   <td><?=_('Portable recording studio or digital audio workstation (DAW), featuring "walk and mix" technology.')?></td>
  </tr>
  <tr>
   <td>
   Umixit
   </td>
   <td>
   UmixIt powered by Audacity
   </td>
   <td>
   <?=_('A now discontinued, customized version of Audacity for Microsoft Windows and Mac OS X. Umixit-enabled multi-track CDs or songs were available, enabling the user to record their own vocals or instruments to create remixes, mash-ups, sing karaoke and more.')?>
   </td>
  </tr>
  <tr>
   <td>
   <a href="http://www.forest.impress.co.jp">Windows Forest</a>
   </td>
   <td>
   <a href="http://www.forest.impress.co.jp/lib/pic/music/soundedit/audacity.html">Audacity</a>
   </td>
   <td><?=_("Mirror site for Audacity downloads, from webzine about Windows software at Impress Watch Corporation, Tokyo, Japan.")?></td>
  </tr>
</table>

<?php
  include "../include/footer.inc.php";
?>