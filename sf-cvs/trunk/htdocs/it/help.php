<?php BoxTop("$helpStr - Audacity 1.2.0"); ?>

<p>
Manuale completo di Audacity 1.2: 
[<a href="manual-1.2">Navigazione Online</a>]
[<a href="audacity-manual-1.2.zip">Download (450k ZIP)</a>]
</p>

<p>
Audacity 1.2 Online Help:
[<a href="onlinehelp-1.2/contents.htm">Navigazione Online</a>]
</p>

<p>

Ricerca documentazione Audacity:

<?php
/* Includi la ricerca tramite iSearch */

/* Referenza relativa rispetto la isearch directory */
$isearch_path = "isearch";

/* Scegli la larghezza della casella in caratteri */
$isearch_searchFormWidth = 20;

/* Opzionale - seleziona il frame di destinazione per i risultati della ricerca.
 * Default value is "_self".
 */
$isearch_resultFrame = "_self";

/* Opzionale - permetti agli utenti di selezionare "tutte le parole" o "una delle parole" per la corrispondenza.
 * Il valore di default è falso.
 */
$isearch_allowSetOperator = False;

/* Opzionale - aggiungi la "Ricerca Internet" perchè sia visualizzato la casella per la ricerca.
 * Default value is True.
 */
$isearch_allowSearchInternet = False;

/* Opzionale - Determina quale pagina contiene il form.
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
?>Manuali</a>
</p>

<p>
<?php print "<a href=faq.php?$langLinkStr>";
?>Domande frequentemente fatte (F.A.Q.)</a>
</p>

<p>
<a href="http://sourceforge.net/mailarchive/forum.php?forum_id=828"
>Archivi della mailing list Audacity-Help</a>
</p>

<p>
<a href="mailto:audacity-help@lists.sourceforge.net"
>audacity-help@lists.sourceforge.net</a>
</p>

<?php BoxBottom(); ?>

<p>

<?php BoxTop("$helpStr - Audacity 1.0.0"); ?>

<p>
<a href="docs/contents.html">Leggi la documentazione Online</a>
</p>

<p>
<?php print $docsStr; ?>:
<ul>
<li><a href="audacity-manual-1.0.0-A.zip">Inglese</a>
<li><a href="audacity-manual-1.0.0-bg.zip">Bulgaro</a>
<li><a href="audacity-manual-1.0.0-es.zip">Spagnolo</a>
</ul>
</p>

<p>
<hr>
</p>

<p>
<?php print "<a href=tutorials.php?$langLinkStr>";
?>Manuali</a>
</p>

<p>
<?php print "<a href=faq.php?$langLinkStr>";
?>Domande frequentemente fatte (F.A.Q.)</a>
</p>

<p>
<a href="http://sourceforge.net/mailarchive/forum.php?forum_id=828"
>Archivi della mailing list Audacity-Help</a>
</p>

<p>
<a href="mailto:audacity-help@lists.sourceforge.net"
>audacity-help@lists.sourceforge.net</a>
</p>

<?php BoxBottom(); ?>
