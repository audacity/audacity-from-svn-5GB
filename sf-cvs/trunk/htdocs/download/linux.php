<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "linux";
  $pageTitle = _("Linux/Unix");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_('The easiest way to install the free Audacity audio editor on GNU/Linux (and other Unix-like systems) is to download a package built for your operating system:')?></p>
<ul>
  <li><a href="http://www.altlinux.com/index.php?module=sisyphus&package=audacity">Alt Linux</a></li>
  <li><a href="http://packages.debian.org/cgi-bin/search_packages.pl?searchon=names&version=all&exact=1&keywords=audacity">Debian</a></li>
  <li><a href="http://packages.gentoo.org/packages/?category=media-sound;name=audacity">Gentoo</a></li>
  <li><a href="http://seerofsouls.com/rpm2006.html">Mandriva i586</a> and 
  <a href="http://seerofsouls.com/x86_64.html">Mandriva x86_64</a></li>
  <li><a href="ftp://ftp.ac.pld-linux.org/dists/ac/PLD/">PLD Linux</a></li>
  <li>Red Hat / Fedora Core
    <ul>
      <li><a href="http://fedoraproject.org/wiki/Extras/FedoraHOWTO">Fedora Extras</a></li>
      <li><a href="http://ccrma.stanford.edu/planetccrma/software/">Planet CCRMA</a></li>
    </ul>
  </li>
  <li><a href="http://packman.links2linux.org/?action=286">SuSE Linux</a></li>
  <li><a href="http://packages.ubuntu.com/audacity">Ubuntu Linux</a></li>
</ul>

<p><?=_('Linux and Unix users may also compile Audacity from <a href="source">source code</a>.')?></p>

<?php
  include "../include/footer.inc.php";
?>
