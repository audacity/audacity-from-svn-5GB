<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
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
      array(_("License"), "license"),
      array(_("Credits"), "credits"),
      array(_("Links"), "links"),
    )
  ),
  array(_("Download"), "download/",
    array(
      array(_("Stable").": 1.2.4", ""),
      array($ind._("Windows"), "windows"),
      array($ind._("Mac"), "mac"),
      array($ind._("Linux/Unix"), "linux"),
      array($ind._("Source Code"), "source"),
      array($ind._("Release Notes"), "release-notes"),
      array(_("Beta").": 1.3.0", ""),
      array($ind._("New Features in 1.3"), "features-1.3-a"),
      array($ind._("Windows"), "beta_windows"),
      array($ind._("Mac"), "beta_mac"),
      array($ind._("Linux/Unix"), "beta_linux"),
      array($ind._("Source Code"), "beta_source"),
      array(_("Plug-Ins"), "plugins"),
      array(_("Buy a CD"), "buy"),
    )
  ),
  array(_("Help"), "help/",
    array(
      array(_("FAQ"), "faq"),
      array(_("Documentation"), "documentation"),
      array(_("Tutorials"), "tutorials"),
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
      array(_("Translators"), "translation"),
      array(_("Developers"), "developers"),
      array(_("Donate"), "donate"),
    )
  ),
);
?>
