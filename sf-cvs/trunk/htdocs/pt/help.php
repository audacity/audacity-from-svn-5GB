<?php BoxTop($helpStr); ?>

<p>
<a href="docs/contents.html">Leia</a> a documentação online.
</p>

<p>
<?php print $docsStr; ?>:
<ul>
<li><a href="audacity-manual-1.0.0-A.zip">Inglês</a>
<li><a href="audacity-manual-1.0.0-bg.zip">Búlgaro</a>
<li><a href="audacity-manual-1.0.0-es.zip">Espanhol</a>
</ul>
</p>

<p>
<?php print "<a href=tutorials.php?$langLinkStr>";
?>Tutorials</a>
</p>

<p>
<?php print "<a href=faq.php?$langLinkStr>";
?>Perguntas e Respostas - F.A.Q.</a>
</p>

<p>
<a href="http://sourceforge.net/mailarchive/forum.php?forum_id=828"
>Audacity-Help Mailing List Archives</a> - Arquivos da Lista de usuários do Audacity para ajuda mútua. Aqui é possível verificar se uma questão específica já foi respondida  por outro usuário. 
</p>

<p>
<a href="mailto:audacity-help@lists.sourceforge.net"
>audacity-help@lists.sourceforge.net</a> - Escreva por este endereço para obter ajuda de outros usuários do Audacity, assim como partilhar experiências e conhecimentos.
</p>

<?php BoxBottom(); ?>
