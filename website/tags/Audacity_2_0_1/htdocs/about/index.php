<?php
/*
 * Copyright 2004 Matt Brubeck
 * 2007-9 Gale Andrews, Vaughan Johnson. 
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "";
  $pageTitle = _("About Audacity");
  include "../include/header.inc.php";

  echo "<h2>$pageTitle</h2>";

  echo _('<p>Audacity is a free, easy-to-use and <a href="http://audacityteam.org/wiki/index.php?title=Changing_the_current_language">multilingual</a> audio editor and recorder for Windows, Mac OS X, GNU/Linux and other operating systems.  You can use Audacity to:</p>

<ul>
  <li>Record live audio.</li>
  <li>Convert tapes and records into digital recordings or CDs.</li>
  <li>Edit Ogg Vorbis, MP3, WAV or AIFF sound files.</li>
  <li>Cut, copy, splice or mix sounds together.</li>
  <li>Change the speed or pitch of a recording.</li>
  <li>And more! See the complete <a href="features">list of features</a>.</li>
</ul>');

  // i18n-hint: You may change the link addresses below, if there is a
  // version of the page in your language at a different location.
  //
  // For official translations of the phrase "free software", please see:
  //   http://www.fsf.org/philosophy/fs-translations.html
  //
  // Also check the translations of this web page:
  //   http://www.fsf.org/philosophy/free-sw.html
  echo _('<h3>About Free Software</h3>

<p>Audacity is free software, developed by a group of volunteers and distributed under the <a href="license">GNU General Public License (GPL)</a>.</p>

<p>Free software is not just free of cost (like &quot;free beer&quot;).  It is <b>free as in freedom</b> (like &quot;free speech&quot;).  Free software gives you the freedom to use a program, study how it works, improve it and share it with others.  For more information, visit the <a href="http://www.fsf.org/">Free Software Foundation</a>.</p>

<p>Programs like Audacity are also called <b>open source software</b>, because their source code is available for anyone to study or use.  There are thousands of other free and open source programs, including the <a href="http://www.mozilla.com/">Firefox</a> web browser, the <a href="http://www.openoffice.org/">OpenOffice.org</a> office suite and entire Linux-based operating systems such as <a href="http://www.ubuntulinux.org/">Ubuntu</a>.</p>

<p>We welcome <a href="../community/donate">donations</a> to support Audacity development. Anyone can <a href="http://audacityteam.org/wiki/index.php?title=Contribute">contribute</a> to Audacity by helping us with <a href="http://www.audacityteam.org/manual/index.php?title=Main_Page">documentation</a>, <a href="../community/translation">translations</a>, user <a href="http://audacityteam.org/forum/index.php">support</a> and by <a href="http://audacityteam.org/wiki/index.php?title=Nightly_Builds">testing<a/> our latest code.</p>');

echo _('<p><a href="http://www.ohloh.net/projects/59">Ohloh</a> has statistics on the value of Audacity development. The Ohloh &quot;badge&quot; at the bottom of each page on this site shows updated summary values. We set up the Ohloh pages about Audacity so the statistics show only the values of Audacity-specific development, excluding the third-party code libraries Audacity uses.</p>');

  echo _('<h3>Bundling, Reselling or Distributing Audacity</h3>

<p>Vendors are free to bundle Audacity with their products, or to sell or distribute copies of Audacity (see <a href="../download/bundlers">Vendors and Distributors of Audacity</a>) under the <a href="../about/license#license">GNU General Public License (GPL)</a>.</p>

<p>If you are interested in bundling, selling or distributing Audacity, please review our entire <a href="../about/license">License, and Advice for Vendors and Distributors</a> page.</p>');

  echo _('<h3>Sponsoring Audacity Development</h3>

<p>We are keen to hear from any companies or groups interested in <a href="http://audacityteam.org/sponsor.php">sponsoring<a/> Audacity development.</p>');  

  include "../include/footer.inc.php";
?>