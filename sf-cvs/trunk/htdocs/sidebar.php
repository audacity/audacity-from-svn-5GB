<!-- sidebar table - a double table to give it a border -->
<table border="0" cellpadding="1" cellspacing="0" bgcolor="#000099"><tr><td>
<table border="0" cellspacing="1" cellpadding="4">

<tr><td align="center" bgcolor="#dddddd">
<font size="+1"><b><?php print $helpSectionStr; ?></b></font>
</td></tr>

<tr><td bgcolor="#ffffff">

<?php

print "<a href=about.php?$langLinkStr>$aboutStr</a>\n<br>\n";
print "<a href=screenshots.php?$langLinkStr>$screenshotsStr</a>\n<br>\n";
print "<a href=help.php?$langLinkStr>$helpStr</a>\n<br>\n";
print "<a href=faq.php?$langLinkStr>$faqStr</a>\n<br>\n";
print "<a href=tutorials.php?$langLinkStr>$tutorialsStr</a>\n<br>\n";
print "<a href=links.php?$langLinkStr>$linksStr</a>\n<br>\n";
print "<a href=contact.php?$langLinkStr>$contactUsStr</a>\n<br>\n";

?>

</td></tr>

<tr><td align="center" bgcolor="#dddddd">
<font size="+1"><b><?php print $downloadSectionStr; ?></b></font>
</td></tr>

<tr><td bgcolor="#ffffff">

<!--
<p>
<i><?php print "$stableVersion $stableStr / ".
	"$betaVersion $betaStr"; ?></i>
</p>
-->

<table border="0">

<tr>
<td><?php print "<a href=windows.php?$langLinkStr>"; ?><img src="images/windows.gif" border=0 width=32 height=22></a></td>
<td><font size="+1"><?php print "<a href=windows.php?$langLinkStr>$windowsStr "; ?>
</a></font></td>
</tr>

<tr>
<td>

<?php print "<a href=mac.php?$langLinkStr>"; ?><img src="images/macos.gif" border=0 width=26 height=26><img src="images/macosx.gif" border=0 width=19 height=28></a></td>
<td><font size="+1"><?php print "<a href=mac.php?$langLinkStr>$macStr"; ?>
</a></font></td>
</tr>

<tr>
<td><?php print "<a href=unix.php?$langLinkStr>"; ?><img src="images/linux.gif" border=0 width=22 height=26><img src="images/freebsd.gif" border=0 width=26 height=28></a></td>
<td><font size="+1"><?php print "<a href=unix.php?$langLinkStr>$unixStr"; ?>
</a></font></td>
</tr>

<tr>
<td><?php print "<a href=plugins.php?$langLinkStr>"; ?><img src="images/VST.gif" border=0 width=36 height=21></a></td>
<td><font size="+1"><?php print "<a href=plugins.php?$langLinkStr>$pluginsStr "; ?>
</a></font></td>
</tr>

</table>


<p>
<i><?php print $mailingListStr; ?></i>
<br>
<form method=post action=http://scripts.dreamhost.com/add_list.cgi>
<input type=hidden name=list value="audacity-announce">
<input type=hidden name=url 
value="http://audacity.sourceforge.net/list/subscribed.php">
<input type=hidden name=emailconfirmurl 
value="http://audacity.sourceforge.net/list/emailconfirm.php">
<input type=hidden name=unsuburl 
value="http://audacity.sourceforge.net/list/unsubscribed.php">
<input type=hidden name=alreadyonurl 
value="http://audacity.sourceforge.net/list/alreadyon.php">
<input type=hidden name=notonurl 
value="http://audacity.sourceforge.net/list/noton.php">
<input type=hidden name=invalidurl 
value="http://audacity.sourceforge.net/list/invalid.php">
<input type=hidden name=domain value="spaghetticode.org">
<input type=hidden name="emailit" value="1">

<font size="-1"><?php print $emailAddressStr; ?></font><br>
<input name="address"><br>

<?php
  print "<input type=submit name=submit value=\"$addEmailStr\">\n";
  print "<input type=submit name=unsub value=\"$removeEmailStr\">\n";
?>

<br>
<?php print "<a href=privacy.php?$langLinkStr><font size=-1>$privacyPolicyStr</font></a>"; ?>

</form>
</p>

<p>
<font size="+1">
<?php print "<a href=beta.php?$langLinkStr>$betaVersionsStr</a>"; ?>
</font>
</p>

</td></tr>

<tr><td align="center" bgcolor="#dddddd">
<font size="+1"><b><?php print $communitySectionStr; ?></b></font>
</td></tr>

<tr><td bgcolor="#ffffff">

<a href=http://lists.sourceforge.net/lists/listinfo/audacity-users>
<?php print $usersListStr; ?>
</a>
<br>
<a href=http://audacity.sourceforge.net/translation/>
<?php print $translationStr; ?>
</a>
<br>
<?php print "<a href=donatetime.php?$langLinkStr>"; ?>
<?php print $donateTimeStr; ?>
</a>
<br>
<?php print "<a href=donatemoney.php?$langLinkStr>"; ?>
<?php print $donateMoneyStr; ?>
</a>

</td></tr>

<tr><td align="center" bgcolor="#dddddd">
<font size="+1"><b><?php print $develSectionStr; ?></b></font>
</td></tr>

<tr><td bgcolor="#ffffff">

<a href=http://lists.sourceforge.net/lists/listinfo/audacity-devel>
<?php print $develListStr; ?>
</a>
<br>
<?php print "<a href=devel.php?$langLinkStr>"; ?>
<?php print $develNewsStr; ?>
</a>
<br>
<a href=http://sourceforge.net/projects/audacity/>
<?php print $sourceForgeStr; ?>
</a>
<br>
<a href=http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/audacity/audacity-src/>
<?php print $browseSourceStr; ?>
</a>
<br>
<a href=dox/html/index.html>Doxygen Class Docs</a>
<br>
<?php print "<a href=nyquist.php?$langLinkStr>"; ?>
Nyquist
</a>
<br>
<?php print "<a href=cvs.php?$langLinkStr>"; ?>
CVS
</a>
<br>
<?php print "<a href=credits.php?$langLinkStr>"; ?>
<?php print $creditsStr; ?>
</a>
<br>

<p>
<center>
<a href="http://sourceforge.net/projects/audacity/"
><img src="http://sourceforge.net/sflogo.php?group_id=6235&type=1"
  width="88" height="31" border="0"></a>
</center>
</p>

</td></tr>

<!-- end of the (double) sidebar table -->
</td></tr></table>
</td></tr></table>
