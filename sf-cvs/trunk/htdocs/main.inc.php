<?php

require_once 'lang.inc.php';
require_once 'versions.inc.php';

$langLinkStr = phpLang_link;

// defaults

$charset =             "iso-8859-1";

$translatorName =      "(Your name here)";
$languageStr =         "English";

$metaDescription =     "Audacity, a free cross-platform digital audio editor";

$helpSectionStr =      "Help";

$aboutStr =            "What is Audacity?";
$screenshotsStr =      "Screenshots";
$helpStr =             "Online Help";
$faqStr =              "F.A.Q."; // Frequently Asked Questions
$tutorialsStr =        "Tutorials";
$linksStr =            "Links";
$contactUsStr =        "Contact Us";

$downloadSectionStr =  "Download";

$stableStr =           "Stable";
$betaStr =             "Beta";

$windowsStr =          "Windows";
$macStr =              "Mac OS 9/X";
$unixStr =             "Linux / Unix";
$betaVersionsStr =     "Beta Versions";
$pluginsStr =          "Plug-ins";

$mailingListStr =      "Get email notifications of new versions:";
$emailAddressStr =     "Email address:";
$addEmailStr =         "Add";
$removeEmailStr =      "Remove";
$privacyPolicyStr =    "Privacy Policy";

$communitySectionStr = "Community";

$usersListStr =        "Audacity-Users List";
$translationStr =      "Translation Project";
$donateTimeStr =       "Donate Time";
$donateMoneyStr =      "Donate Money";

$develSectionStr =     "Developers";
$develListStr =        "Audacity-Devel List";
$develNewsStr =        "Latest Devel News";
$sourceForgeStr =      "SourceForge Project";
$browseSourceStr =     "Browse Source Code";
$browseDoxygenStr =    "Browse Doxygen Documentation";
$creditsStr =          "Credits";

$sourceStr =           "Source Code";

$macOS9ReqStr =        "Requires Mac OS 8.6 or higher";
$macOSXReqStr =        "Requires Mac OS X 10.1 or higher";
$windowsReqStr =       "Requires Windows 98, ME, 2000, or XP";

$lameStr =             "LAME MP3 Encoder Library";
$lameStr2 =            "(You must download this for Audacity to create MP3 files.)";

$installerStr =        "Installer program";
$zipStr =              "ZIP file";
$sitStr =              "StuffIt archive";
$dmgStr =              "Mac OS X Disk Image";
$tarballStr =          "Source code tarball";
$rpmStr =              "Binary RPM";
$srpmStr =             "Source RPM";

$latestStableStr =     "Latest Stable Version:";
$latestBetaStr =       "Latest Beta Version:";

$releaseNotesStr =     "Release Notes";
$releaseNotesStr2 =    "Release Notes (what's new in this version?)";

$vstLinksHereStr =     "Download VST Plug-ins here:";

$docsStr =             "Download latest documentation";

// Span this on two lines so that the translation editor script
// doesn't count it as a language...
$translatorName =
  "";
//

include phpLang_current.'/main.inc.php';

function BoxTop($title)
{
	print '<table border="0" cellpadding="1" cellspacing="0" bgcolor="#000099" width="100%"><tr><td>';
	print '<table border="0" cellspacing="1" cellpadding="4" width="100%">';
	print '<tr><td align="center" bgcolor="#dddddd">';
	print '<font size="+1"><b>';
	print $title;
	print '<b></font>';
	print '</td></tr>';
	print '<tr><td align="left" bgcolor="#ffffff">';
}

function BoxBottom()
{
	print '</td></tr></table></td></tr></table>';
}

function IncludeFile($name)
{
	$f = phpLang_current.'/'.$name.'.php';
	if (file_exists("updates/$f")) {
		include "updates/$f";
	}
	else if (file_exists($f)) {
		include $f;
	}
	else {
		include "en/${name}.php";
	}
}

?>
