<?php
/*
 * Copyright 2003, 2004 Dominic Mazzoni
 * Copyright 2002, 2003, 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "translation";
  $pageTitle = _("Translators");
  include "../include/header.inc.php";
?>

<h2><?=$pageTitle?></h2>

<p><?=_('A group of volunteers is translating the free Audacity sound editor into several languages.  If you would like to help, please join the <a href="../contact/lists#translation">audacity-translation mailing list</a> and introduce yourself.')?></p>

<p><?=_('The translation effort is now focused on Audacity 1.3.3 and beyond.  Translations from previous versions of Audacity have been imported and need to be updated.')?></p>

<h3><?=_("Resources for Translators")?></h3>
<ul>
  <li><a href="../contact/lists#translation"><?=_("Mailing list and archives")?></a>.</li>
  <li><a href="../locale/audacity.pot"><?=_("Latest audacity.pot file")?></a>.</li>
  <li><a href="../locale/audacity_website.pot"><?=_("Latest audacity_website.pot file")?></a>.</li>
  <li><a href="http://wxwidgets.org/i18n.php"><?=_("wxWidgets i18n")?></a>.</li>
</ul>

<?php
  // i18n-hint: These instructions are for translators only, so you may
  // leave them untranslated if you like.
  printf(_('<h3>Translation Instructions</h3>

<p>To start a new translation, download POT file above.  Rename it "audacity.po" before you begin translating.  To edit an existing translation, download the PO file for your language:</p>
<ul>
  <li><a href="%s">Audacity translations</a></li>
  <li><a href="%s">Audacity web site translations</a></li>
</ul>

<p>These programs can create and edit PO files:</p>
<ul>
  <li><a href="http://poedit.sourceforge.net/">poEdit</a> for Windows and Unix.</li>
  <li><a href="http://i18n.kde.org/tools/kbabel/">KBabel</a> for KDE.</li>
  <li><a href="http://www.gnu.org/software/gettext/">GNU gettext</a> is standard on most Unix systems.  It includes a PO mode for the Emacs text editor.</li>
</ul>

<p>Send completed PO files to Dominic Mazzoni <a href="mailto:dominic@audacityteam.org">dominic@audacityteam.org</a>.</p>

<h3>Notes</h3>

<ul>
  <li><p>In poEdit, make sure to set the "charset" option correctly.  Otherwise, poEdit will not save any translations with non-English characters.</p>
  <li><p>Strings like <b>"&amp;File"</b> and <b>"New &amp;Audio Track"</b> are menu items.  The letter after the "&amp;" symbol is used as a keyboard shortcut.</p>
  <li><p><b>"Import Audio...\tCtrl+i"</b> is also a menu item.  The \t stands for a tab character, which separates the menu text from the keyboard command.</p>
  <li><p>You will find many strings like <b>"There were %%d buffer underruns, last near %%lf seconds."</b>  The %% signs mark places where numbers or names will be inserted into the string.  For example, this string might become "There were 4 buffer underruns, last near 3.01 seconds." If you change the order of the the %% markers in a string, add "1$" after the percent sign that used to be first, "2$" after the percent sign that used to be second, and so on.  For example:</p>
  <blockquote><p><b>"The buffer underrun near %%2$lf seconds was the last of %%1$d."</b></p></blockquote>
</ul>'), "http://audacity.cvs.sourceforge.net/audacity/audacity-src/locale/", "http://audacity.cvs.sourceforge.net/audacity/htdocs/locale/");

  include "../include/footer.inc.php";
?>
