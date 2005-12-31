<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "";
  $pageTitle = _("About Audacity");
  include "../include/header.inc.php";

  echo "<h2>$pageTitle</h2>";

  echo _('<p>Audacity is a free, easy-to-use audio editor and recorder for Windows, Mac OS X, GNU/Linux, and other operating systems.  You can use Audacity to:</p>

<ul>
  <li>Record live audio.</li>
  <li>Convert tapes and records into digital recordings or CDs.</li>
  <li>Edit Ogg Vorbis, MP3, and WAV sound files.</li>
  <li>Cut, copy, splice, and mix sounds together.</li>
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

<p>Free software is not just free of cost (like “free beer”).  It is <b>free as in freedom</b> (like “free speech”).  Free software gives you the freedom to use a program, study how it works, improve it, and share it with others.  For more information, visit the <a href="http://www.fsf.org/">Free Software Foundation</a>.</p>

<p>Programs like Audacity are also called <b>open source software</b>, because their source code is available for anyone to study or use.  There are thousands of other free and open source programs, including the <a href="http://www.mozilla.com/">Mozilla</a> web browser, the <a href="http://www.openoffice.org/">OpenOffice.org</a> office suite, and entire <a title="Ubuntu Linux" href="http://www.ubuntulinux.org/">Linux-based operating systems</a>.</p>');

  include "../include/footer.inc.php";
?>
