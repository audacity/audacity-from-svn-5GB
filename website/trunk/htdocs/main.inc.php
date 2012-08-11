<?php
/*
 * Copyright 2004 - 12 Matt Brubeck, Gale Andrews, Vaughan Johnson
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */

// Work around PHP bug on sf.net where include_path gets reset.
set_include_path(".".PATH_SEPARATOR."/usr/share/pear".PATH_SEPARATOR."/usr/share/php");
require_once "include/lang.inc.php";
localization_setup();

$sitePath = ".";
$sectionId = "none";

// marker indicating a menu item should be indented
$ind = "\t";

$siteNavItems = array(
  array(_("Home"), "", 0),
  array(_("About"), "about/",
    array(
      array(_("Features"), "features"),
      array(_("Screenshots"), "screenshots"),
      array(_("News"), "news"),
      array(_("License, and Advice for Vendors and Distributors"), "license"),
      array(_("Credits"), "credits"),
      array(_("Links"), "links"),
    )
  ),
  array(_("Download"), "download/",
    array(
      array(_("Audacity 2.0 series")."", ""),
      array($ind._("Windows"), "windows"),
      array($ind._("Mac"), "mac"),
      array($ind._("GNU/Linux"), "linux"),
      array($ind._("Source Code"), "source"),

      array(_("<A class=\"ext\" href=\"http://wiki.audacityteam.org/wiki/Release_Notes\" target=\"_blank\">Release Notes (Wiki)</a>")."", ""),
      array(_("Plug-Ins and Libraries"), "plugins"),
      array(_("Obtain a CD"), "buy"),
      array(_("Vendors and Distributors of Audacity"), "bundlers"),
    )
  ),
  array(_("Help"), "help/",
    array(
      array(_("FAQ"), "faq"),
      array(_("Documentation"), "documentation"),
      array(_("<A class=\"ext\" href=\"http://wiki.audacityteam.org/wiki/Category:Tutorial\" target=\"_blank\">More Tutorials (Wiki)</a>")."", ""),
      array(_("<A class=\"ext\" href=\"http://wiki.audacityteam.org/wiki/Category:Tips\" target=\"_blank\">Tips (Wiki)</a>")."", ""),
      array(_("Nyquist"), "nyquist"),
    )
  ),
  array(_("Contact Us"), "contact/",
    array(
      array(_("Mailing Lists"), "lists"),
      array(_("Privacy Policy"), "privacy"),
    )
  ),
  array(_("Get Involved"), "community/",
    array(
      array(_("Users"), "users"),
      array(_("Developers"), "developers"),
      array(_("Translators"), "translation"),
    )
  ),
  array(_("Donate"), "donate/", 0),
);
?>
