<?php BoxTop("$helpStr - Audacity 1.2.0"); ?>

<p>
Audacity 1.2 manual (på engelsk): 
[<a href="manual-1.2">Læs online</a>]
[<a href="audacity-manual-1.2.zip">Hent fil (450k ZIP)</a>]
</p>

<p>
Audacity 1.2 Online Hjælp:
[<a href="onlinehelp-1.2/contents.htm">Læs online</a>]
</p>

<p>

Søg i Audacity dokumentationen:
<form method="post" action="/isearch/index.php" target="_self">

<table border="0" cellpadding="3" cellspacing="1">
  <tr>
    <td>

      <input maxLength="255" name="s" size="20" value=''>
      <input type="submit" value="Søg hjemmeside">
      <a TARGET="_blank" HREF="/isearch/help.php">Hjælp</a>
    </td>
  </tr>
</table>

<input type="hidden" name="action" value="search">
</form>
</p>

<p>
<hr>
</p>

<p>
<?php print "<a href=tutorials.php?$langLinkStr>";
?>Vejledninger</a>
</p>

<p>
<?php print "<a href=faq.php?$langLinkStr>";
?>Ofte Stillede Spørgsmål (O.S.S.)</a>
</p>

<p>
<a href="http://sourceforge.net/mailarchive/forum.php?forum_id=828"
>Arkiv af Audacity-Help postlisten (engelsk)</a>
</p>

<p>
<a href="mailto:audacity-help@lists.sourceforge.net"
>audacity-help@lists.sourceforge.net</a>
</p>

<?php BoxBottom(); ?>

<p>

<?php BoxTop("$helpStr - Audacity 1.0.0"); ?>

<p>
<a href="docs/contents.html">Læs dokumentation Online</a>
</p>

<p>
<?php print $docsStr; ?>:
<ul>
<li><a href="audacity-manual-1.0.0-A.zip">Engelsk</a>
<li><a href="audacity-manual-1.0.0-bg.zip">Bulgarsk</a>
<li><a href="audacity-manual-1.0.0-es.zip">Spansk</a>
</ul>
</p>

<p>
<hr>
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