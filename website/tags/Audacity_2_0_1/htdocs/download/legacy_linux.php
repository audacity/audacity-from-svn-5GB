<?php
/*
 * Copyright 2004 Matt Brubeck
 * 2008 - 2012 Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "linux";
  $pageTitle = _("Legacy GNU/Linux");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_('Installation packages for Audacity on GNU/Linux and other Unix-like systems are often provided by individual distributions:')?></p>
<ul>
  <li><a href="http://rpmseek.com/rpm-pl/audacity.html?hl=com&cx=0%3A-%3A0%3A0%3A0%3A0%3A0&qArStr=0&qRtStr=0&qDnStr=48">Alt Linux</a></li>
  <li><a href="http://packages.debian.org/search?keywords=audacity&searchon=names&suite=stable&section=all">Debian</a></li>
  <li><a href="http://rpmseek.com/rpm-pl/audacity.html?hl=com&cx=0%3A-%3A0%3A0%3A0%3A0%3A0&qArStr=0&qRtStr=0&qDnStr=82">Fedora Core</a></li>
  <li><a href="http://rpmseek.com/rpm-pl/audacity.html?hl=com&cx=0%3A-%3A0%3A0%3A0%3A0%3A0&qArStr=0&qRtStr=0&qDnStr=98">Fedora Project</a></li>
  <li><a href="http://rpmseek.com/rpm-dl/audacity-1.2.4b-4.2006.SoS.i586.html?hl=com&cx=0:-:0:2750311:0:0:0">Mandriva i586</a></li>
  <li><a href="http://rpmseek.com/rpm-pl/audacity.html?hl=com&cx=0%3A-%3A0%3A0%3A0%3A0%3A0&qArStr=0&qRtStr=0&qDnStr=108">OpenSUSE</a></li>
  <li><a href="http://rpmseek.com/rpm-pl/audacity.html?hl=com&cx=0%3A-%3A0%3A0%3A0%3A0%3A0&qArStr=0&qRtStr=0&qDnStr=70">Red Hat</a></li>   
  <li><a href="http://rpmseek.com/rpm-pl/audacity.html?hl=com&cx=0%3A-%3A0%3A0%3A0%3A0%3A0&qArStr=0&qRtStr=0&qDnStr=11">SuSE</a> and <a href="http://rpmseek.com/rpm-pl/audacity.html?hl=com&cx=0%3A-%3A0%3A0%3A0%3A0%3A0&qArStr=0&qRtStr=0&qDnStr=44">packman (suse)</a></li>
  <li>Ubuntu: <a href="http://packages.ubuntu.com/audacity">packages.ubuntu</a> and <a href="http://rpmseek.com/rpm-pl/audacity.html?hl=com&cx=0%3A-%3A0%3A0%3A0%3A0%3A0&qArStr=0&qRtStr=0&qDnStr=109">rpm.seek</a> </li>
</ul>

<p><?=_('The above list is not comprehensive. If you don\'t see an up-to-date package for your own distribution, we recommend searching your distribution\'s web site for the latest information. Alternatively, you can compile Audacity from <a href="legacy_source">source code</a>.')?></p>

<h3 id="sysreq"><?=_("System Requirements")?></h3>
 <ul>
  <li><?=_("We recommend using the latest version of GNU/Linux from your distribution that is compatible with your hardware specifications. Audacity will run best with at least 64 MB RAM and a 300 MHz processor.")?></li>
 </ul>


<?php
  include "../include/footer.inc.php";
?>
