<?php BoxTop($helpStr); ?>

<p>
<a href="docs/contents.html">Brugerguide / Dokumentation online</a>
</p>

<p>
<?php print $docsStr; ?>:
<ul>
<li><a href="audacity-manual-1.0.0-A.zip">Engelsk</a>
<li><a href="audacity-manual-1.0.0-bg.zip">Bulgarsk</a>
<li><a href="audacity-manual-1.0.0-es.zip">Spank</a>
</ul>
</p>

<p>
<?php print "<a href=tutorials.php?$langLinkStr>";
?>Vejledninger</a>
</p>

<p>
<?php print "<a href=faq.php?$langLinkStr>";
?>Ofte Stillede Spørgsmål (F.A.Q.)</a>
</p>

<p>
<a href="http://sourceforge.net/mailarchive/forum.php?forum_id=828"
>Audacity-Help mailliste arkiv</a>
</p>

<p>
<a href="mailto:audacity-help@lists.sourceforge.net"
>audacity-help@lists.sourceforge.net</a>
</p>

<?php BoxBottom(); ?>
