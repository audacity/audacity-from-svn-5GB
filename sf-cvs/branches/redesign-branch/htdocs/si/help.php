<?php BoxTop($helpStr); ?>

<p>
<a href="docs/contents.html">Preberite dokumentacijo</a>
</p>

<p>
<?php print $docsStr; ?>:
<ul>
<li><a href="audacity-manual-1.0.0-A.zip">Angleško</a>
<li><a href="audacity-manual-1.0.0-bg.zip">Bulgarko</a>
</ul>
</p>

<p>
<?php print "<a href=tutorials.php?$langLinkStr>";
?>Pomoèniki</a>
</p>

<p>
<?php print "<a href=faq.php?$langLinkStr>";
?>Najpogosteje Zastavljena Vprašanja (N.Z.V.)</a>
</p>

<p>
<a href="http://sourceforge.net/mailarchive/forum.php?forum_id=828"
>Arhivi seznamov Audacity-Pomoèi</a>
</p>

<p>
<a href="mailto:audacity-help@lists.sourceforge.net"
>audacity-help@lists.sourceforge.net</a>
</p>

<?php BoxBottom(); ?>
