<?php BoxTop($helpStr); ?>
<p><B>
<font size=4>
Bem-vindo à Ajuda online!</font></b>
<p>
O Audacity dispões de diversas fontes de informação que poderão lhe ajudar a responder uma questão, resolver uma dúvida ou usá-lo melhor.
<p>
Infelizmente, todos os caminhos para conseguir ajuda estão ainda em inglês.
Caso domine o idioma, você pode ajudar! Conheça os caminhos abaixo, escolha
um, clique em <a href="http://audacity.sourceforge.net/translation/">Projetos
de Tradução</a>  e inscreva-se na lista de tradutores do Audacity. A
comunidade brasileira do programa agradece!
<p>
 
<p>
Os caminhos para conseguir ajuda são:
<p><p>
<b>1. Ler as P&R - Perguntas e Respostas ou FAQ.</b>
<blockquote>
  <p>Esta página apresenta uma lista de perguntas bastante comuns que surgem no
  uso do programa. Por serem frequentes, estão todas listadas aqui. Comece
  lendo esta página, quem sabe sua dúvida já não foi respondida lá?
<p>
<?php print "<a href=faq.php?$langLinkStr>";?> Clique
  aqui</a> para ler a página de P&R (ou "FAQ")
<p><p>
 </blockquote>
<p><b>2. Buscar nas páginas de Documentação e Referência:</b>
<blockquote>
  <p>As páginas online do Manual e de Referência estão disponíveis para
  busca. Utilize o campo abaixo, lembrando-se de inserir termos em inglês:
  <form method="post" action="http://audacityteam.org/isearch/index.php" target="_self">
  <input maxLength="255" name="s" size="26" value=''>
  <input type="submit" value="Buscar">
  <font size="-1"><a TARGET="_blank" HREF="http://audacityteam.org/isearch/help.php">Ajuda para a 
  busca</a> </font>
<input type="hidden" name="action" value="search">
</form>


</blockquote><p><br>
<p><b>3. Ler a documentação online:</b>
<blockquote>
  <p>A documentação online (Manual do usuário) exibe e explica cada recurso, menu e
  opção do programa. <p><a href="docs/contents.html">Clique aqui</a>  e leia a versão online
  do manual do usuário para o <b>Audacity versão 1.0</b>
  </blockquote><p></p><p><br>
  <b>4. Ler a Guia de Referência Rápida do Audacity 1.2:</b>
<blockquote>
<p> A Guia de Referência Rápida explica os recursos do programa um a um,
de maneira simples e objetiva: <p><a href="onlinehelp-1.2/contents.htm">Clique aqui e leia a Guia.</a>

</blockquote>
  <p><p><br><b>5. Copiar o manual do usuário</b>
<blockquote>
  <p>É possível ter o manual do usuário para consulta mesmo sem estar
  conectado à Internet e em diversos idiomas. <p>Clique no idioma desejado
  para copiar o manual da versão <b> 1.0 </b> do Audacity:
<ul>
<li><a href="audacity-manual-1.0.0-A.zip">Inglês</a>

<li><a href="audacity-manual-1.0.0-bg.zip">Búlgaro</a>
   
<li><a href="audacity-manual-1.0.0-es.zip">Espanhol</a>
   
</ul>

  <p>O manual completo para a versão <b> 1.2</b> já está disponível em inglês e sua tradução para o português brasileiro está em andamento. A versão em inglês pode ser acessada:</p>
  <ul>
    <li><a href="audacity-manual-1.2.zip">Clicando aqui</a> para copiar (450kb, em formato ZIP) </li>
    <li>Acesso online: <a href="manual-1.2">clique aqui</a></li>
  </ul>

</blockquote>
<p>  
<p><b>6. Conhecer os tutoriais</b>
<blockquote>
  <p>O Audacity é utilizado por uma larga gama de usuários, inclusive por profissionais. Nesta página são colocados os tutoriais criados por estes
  usuários explicando como utilizar o programa, executar uma tarefa específica
  ou explorar um recurso especial. <p>
<?php print "<a href=tutorials.php?$langLinkStr>";?>
 Clique aqui</a>
 e conheça a página de tutoriais. <p>
 
</blockquote>
<p><b>7. Conheça o Audacity Wiki</b>
<blockquote>
<p>O Wiki é um site onde todas as pessoas podem
postar e editar textos de outros usuários. Cada página tem em seu rodapé um
link para a edição, é só clicar e inserir ou editar os textos.<p>Lá é
possível encontrar dicas de usuários, documentação e um espaço para pedir
por novos recursos. <p>
<a href="http://www.audacityteam.org/wiki">Clique aqui</a>
 e abra o site do Audacity Wiki.<p> 
</blockquote>
<p><b>8. Inscreva-se nas listas</b>
<blockquote>
  <p>O Audacity é um projeto mantido por pessoas como eu ou você com um
  objetivo em comum, que é o de criar um editor de áudio simples, fácil de
  usar e totalmente livre. Se a sua dúvida não foi respondida por nenhum dos
  caminhos apontados acima, experimente entrar em contato com um dos milhares
  de usuários do Audacity. <p>A lista de ajuda do programa é o lugar para
  começar. Nela, usuários e desenvolvedores do programa se juntam para
  responder questões sobre o uso do programa. Sua dúvida pode tanto ser
  respondida por um outro usuário que enfrentou um problema igual ao seu ou por
  um dos desenvolvedores do programa, explicando como proceder.<p>As listas são
  mantidas em inglês, no entanto a possibilidade de uma lista exclusivamente
  portuguesa aguarda apenas uma demanda maior por parte dos usuários. <p>Comece
  buscando por perguntas que descrevam sua dúvida na <a href="http://sourceforge.net/mailarchive/forum.php?forum_id=828"
>Audacity-Help Mailing List Archives</a>, e veja se ela já foi respondida
  antes.<p>Caso não encontre referências anteriores, sinta-se a vontade para
  escrever para <a href="mailto:audacity-help@lists.sourceforge.net"
>audacity-help@lists.sourceforge.net</a>. 
<br><font size=-1><?php print "$listPrivacyStr"; ?></font></br>
</blockquote>

<?php BoxBottom(); ?>