<?php BoxTop("$helpStr - Audacity 1.2.0"); ?>

<p>
Audacity 1.2 - kompletní manuál: 
[<a href="manual-1.2">prohlížení on-nline</a>]
[<a href="audacity-manual-1.2.zip">stažení (450k ZIP)</a>]
</p>

<p>
Audacity 1.2 on-line nápovìda:
[<a href="onlinehelp-1.2/contents.htm">prohlížení</a>]
</p>


<p>

Prohledávat dokumentaci:

<?php
/* Include the iSearch search form */

/* Relative reference to isearch directory */
$isearch_path = "isearch";

/* Set the width of the entry box in chars */
$isearch_searchFormWidth = 20;

/* Optional - set target frame for search results.
 * Default value is "_self".
 */
$isearch_resultFrame = "_self";

/* Optional - allow the user to select "all words" or "any words" to match.
 * Default value is False.
 */
$isearch_allowSetOperator = False;

/* Optional - add a "Search Internet" button to be displayed.
 * Default value is True.
 */
$isearch_allowSearchInternet = False;

/* Optional - Determine whether page containing this form is UTF-8 encoded.
 * Default value is True.
 */
$isearch_charsetUtf8 = "True";

include("$isearch_path/isearch_form.inc.php");
?>
</p>




<p>
<hr>
</p>

<p>
<?php print "<a href=tutorials.php?$langLinkStr>";
?>návody</a>
</p>

<p>
<?php print "<a href=faq.php?$langLinkStr>";
?>èasto kladené otázky (F.A.Q.)</a>
</p>

<p>
<a href="http://sourceforge.net/mailarchive/forum.php?forum_id=828"
>archiv e-mailové konference</a>
</p>

<p>
<a href="mailto:audacity-help@lists.sourceforge.net"
>audacity-help@lists.sourceforge.net</a>
</p>

<?php BoxBottom(); ?>

<p>

<?php BoxTop("$helpStr - Audacity 1.0.0"); ?>

<p>
<a href="docs/contents.html">Pøeètìte si dokumentaci on-line</a>
</p>

<p>
<?php print $docsStr; ?>:
<ul>
<li><a href="audacity-manual-1.0.0-A.zip">Anglicky</a>
<li><a href="audacity-manual-1.0.0-bg.zip">Bulharsky</a>
<li><a href="audacity-manual-1.0.0-es.zip">Španìlsky</a>
</ul>
</p>

<p>
<hr>
</p>

<p>
<?php print "<a href=tutorials.php?$langLinkStr>";
?>návody</a>
</p>

<p>
<?php print "<a href=faq.php?$langLinkStr>";
?>èasto kladené otázky (F.A.Q.)</a>
</p>

<p>
<a href="http://sourceforge.net/mailarchive/forum.php?forum_id=828"
>archiv e-mailové konference</a>
</p>

<p>
<a href="mailto:audacity-help@lists.sourceforge.net"
>audacity-help@lists.sourceforge.net</a>
</p>

<?php BoxBottom(); ?>

