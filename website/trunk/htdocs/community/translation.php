<?php
/*
 * Copyright 2002-2011 Dominic Mazzoni, Matt Brubeck, Gale Andrews
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "translation";
  $pageTitle = _("Translators");
  include "../include/header.inc.php";
  include "potdates.inc";
?>

<h2><?=$pageTitle?></h2>

<p><?php
  // i18n-hint: The two %s will be replaced by HTML tags to make the text
  // inbetween into a link to the mailing list page. They are thus invisible in
  // the final text.
printf(_('A group of volunteers is translating the free Audacity sound editor into many different languages. The translation effort is now focused exclusively on Audacity 1.3 Beta as we prepare it to be our next Stable Release. If you would like to help, please join the %saudacity-translation mailing list%s and introduce yourself. Please read the instructions below on how to start working on translations.'), '<a href="https://lists.sourceforge.net/lists/listinfo/audacity-translation">', '</a>'); ?></p>

<h3><?=_("Resources for Translators")?></h3>
<ul>
  <li><a href="https://lists.sourceforge.net/lists/listinfo/audacity-translation"><?=_("Mailing list and archives")?></a></li>
  <li><a href="http://wiki.audacityteam.org/index.php?title=Translating_Audacity"> <?=_("Translating Audacity</a> on the Audacity <a href=\"http://wiki.audacityteam.org/index.php\">Wiki</a>.")?></li>  
  <li><a href="../locale/audacity.pot"><?php
		// i18n-hint: The three numbers will be year, month, day. Remember you
		// can re-order the numbers if you want to - to get Day, Month, Year,
		// translate the values as %4$02d/%3$02d/%2$04d. The string (%s) inserts
		// a html tag to end the link, and is not visible.
	printf(_("Latest audacity.pot file%s (last updated %04d/%02d/%02d)"), 
		'</a>', $prog_year, $prog_month, $prog_day);
?></a></li>
  <li><a href="../locale/audacity_website.pot"><?php
		// i18n-hint: The three numbers will be year, month, day. Remember you
		// can re-order the numbers if you want to - to get Day, Month, Year,
		// translate the values as %4$02d/%3$02d/%2$04d. The string (%s) inserts
		// a html tag to end the link, and is not visible.
   printf(_("Latest audacity_website.pot file%s (last updated %04d/%02d/%02d)"),
		'</a>', $web_year, $web_month, $web_day);
?></a></li>
  <li><a href="http://wxwidgets.org/i18n.php"><?=_("wxWidgets i18n")?></a></li>
  <li><a href="http://manual.audacityteam.org/index.php?title=Main_Page"><?=_("Audacity Beta User Manual</a> - The Audacity Beta User Manual also requires translating. This is done online using Wiki software. To create an account and start translating, please contact")?> 
<a href="&#x6d;&#x61;&#105;&#108;&#x74;&#x6f;&#58;&#103;&#x61;&#x6c;&#101;&#x40;&#x61;&#x75;&#x64;&#x61;&#x63;&#105;&#x74;&#x79;&#x74;&#x65;&#97;&#109;&#46;&#111;&#114;&#x67;&#x3f;&#115;&#x75;&#x62;&#106;&#101;&#x63;&#x74;&#61;&#x4d;&#x61;&#x6e;&#x75;&#x61;&#x6c;&#95;&#87;&#105;&#107;&#105;&#95;&#97;&#x63;&#99;&#x6f;&#117;&#110;&#116;">Gale</a>.</li>  
</ul>

<?php
  // i18n-hint: These instructions are for translators only, so you may
  // leave them untranslated if you like.
   printf(_('<h3>Translation Instructions (software and web site)</h3>

<p>Normally there will already be a translation for your language of the Audacity software or web site, but often it needs to be updated for the latest changes. <b>To update an existing translation</b>, use the links below to download the .po file for your language:</p>
<ul>
  <li><a href="%s">Audacity translations</a></li>
  <li><a href="%s">Audacity web site translations</a></li>
</ul>

<p>After clicking either link above, click on the link for the .po file for your language. On the page you come to, scroll down to the blue "File info" panel on the right, then right-click or control-click over the "View raw file" link and "save target" or "save link as" to download the .po file.</p> 

<p><b>If there is no .po file for your language</b>, create one by downloading the latest "audacity.pot" or "audacity_website.pot" file under "Resources for Translators" above. (If the links only produce text in the browser, right-click or control-click and save the target or link). Rename the .pot to "audacity.po" (or "audacity_website.po" for web site translation) before starting your translation.</p>

<p><b>Programs for creating and editing .po files:</b></p>
<ul>
  <li><a href="http://poedit.sourceforge.net/">poEdit</a> for Windows and Unix.</li>
  <li><a href="http://userbase.kde.org/Lokalize">Lokalize</a> for KDE4</li>
  <li><a href="http://translate.sourceforge.net/wiki/virtaal/index">Virtaal</a></li>
  <li><a href="http://www.gnu.org/software/gettext/">GNU gettext</a> is standard on most Unix systems.  It includes a PO mode for the Emacs text editor.</li>
</ul>

<p><b>Basic usage of poEdit:</b><p>
<ul>
  <li>For a newly created .po file, set its properties by clicking <b>Catalog > Settings</b> and filling in the required information. Set the target language in the <b>Language</b> menu. Make sure to set the "charset" option correctly, otherwise poEdit will not save any translations with non-English characters. </li>
  <li>For an existing .po file, download "audacity.pot" or "audacity_website.pot" from "Resources for Translators" above. In poEdit, click <b>Catalog > Update from POT file</b> to update your .po file with the latest strings from the downloaded .pot file. Then translate the .po file (<b>not</b> the .pot file).</li>     
  <li>When you File > Save, poEdit saves the .po file, plus a .mo file for use in Audacity.</li>
</ul>

<p><b>To test a translation in Audacity:</b></p>
<ul>
  <li>Navigate to the "Languages" directory inside the Audacity installation directory, and open the directory with the same name as your .po file. If creating a new translation, create a directory in the "Languages" folder using the correct language code from <a href="http://www.poedit.net/translations.php">http://www.poedit.net/translations.php</a>. For example, the code for Tamil is "ta", so create "Audacity\Languages\ta".</li>
  <li>Rename the saved <b>.mo</b> file to "Audacity.mo", and paste it into the directory you opened or created.</li>
  <li>Open Audacity and in <b>Preferences: Interface</b>, choose your language and click OK. You should now see your translations. </li>
</ul>

<p><b>Further information:</b></p>
<ul>
  <li><b>To submit a translation</b>, please send the completed .po file to the audacity-translation mailing list. A member of Audacity Team will commit the file and send a message to the list confirming this.</li> 
  <li>Please read <a href="http://wiki.audacityteam.org/index.php?title=Translating_Audacity">Translating Audacity</a> for more tips on translating the Audacity source code, and to learn how the Audacity software and web site are translated.</li>
</ul>'), "http://code.google.com/p/audacity/source/browse/audacity-src/trunk/locale", "http://code.google.com/p/audacity/source/browse/website/trunk/htdocs/locale");

  include "../include/footer.inc.php";
?>
