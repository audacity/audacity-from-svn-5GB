<h2>O que há de novo desde o Audacity 1.0</h2>

Esta seção lista as principais alterações da versão mais recente em relação à última versão estável do programa.
Usuários das versões beta devem visitar a <a href="<?php print "betanotes.php?$langLinkStr"; ?>">página de versões beta</a> para saber as alterações desde a versão 1.1.3.



<h3>Áudio de qualidade profissional</h3>

<ul>
<li>
O Audacity agora grava e edita áudio captado com amostragens a 24-bit e 32-bit (ponto-flutuante). Faixas com amostragens diferentes podem ser editadas no mesmo projeto e o programa se encarrega de converter as taxas onde for necessário, utilizando algoritmos de conversão de alta-qualidade .
As conversões agora utilizam algoritmos da
<a href="http://www.mega-nerd.com/SRC/">SRC</a>, de Erik de Castro Lopo.
</li>

<li>
A importação e exportação de som foi melhorada. O programa agora pode gravar mais de duas faixas simultaneamente. O tempo de latência foi reduzido, diminuindo as chances de falhas na captação de som.
</li>
</ul>


<h3>Efeitos</h3>

<ul>
<li>
Três novos efeitos alteram a altura, tempo e velocidade das faixas de áudio:
 <ul>
  <li>"Alterar Altura" aumenta ou diminui o tom do trecho selecionado, sem afetar a velocidade.</li>
  <li>"Alterar Tempo" faz a seleção ser executada mais rápido ou mais devagar, sem alterar a altura.</li>
  <li>"Alterar Velocidade" opera simultaneamente Altura e Tempo, similar a aumentar a velocidade de um disco de vinil ou uma fita cassete.</li>
 </ul>
</li>

<li>
A maioria dos efeitos inclui agora a opção "Preview" que permite experimentar diferentes efeitos sem ter de abrir e fechar a janela diversas vezes. Há também a opção "Repetir efeito", que aplica o último efeito novamente à seleção.
</li>

<li>
Inclui também outros efeitos:
 <ul>
  <li>Compressor, para compressão de extensão dinâmnica ("dynamic range compression").</li>
  <li>Repetir, executa amostras repetidamente ("loop").</li>
  <li>Normalizar, ajusta o volume e corrige desvios DC.</li>
 </ul>
</li>
</ul>


<h3>Novos recursos de edição:</h3>

<ul>
<li>
A ferramenta Envelope, usada para efeitos de "fading" (diminuir e aumentar o volume), agora pode ser utilizada também para aumentar o volume do som para além do original, ou torná-lo mais baixo.
</li>

<li>
O novo recurso "Faixa de Tempo" (Time track) executa funções semelhantes às da ferramenta Envelope, alterando a velocidade da faixa durante a reprodução.
</li>

<li>
É possível ajustar o Ganho e Balanço de cada faixa individualmente.
</li>

<li>
O Audacity agora encontra cruzamentos em zero (zero-crossings), que ajudam na criação de sons contínuos ("loopings"). Pressionar Z move o cursor de seleção para o cruzamento em zero mais próximo.
</li>
</ul>


<h3>Plugins</h3>

<ul>
<li>
Em sistemas rodando Linux, o Audacity agora pode abrir efeitos <a href="http://www.ladspa.org/">LADSPA</a>.
</li>

<li>
Esta versão oferece suporte ao processador digital de sinais <a href="nyquist.php">Nyquist</a>, que permite a criação de novos efeitos baseados em linguagem de programação semelhante à LISP.
</li>
</ul>


<h3>Importação e Exportação de arquivos</h3>

<ul>
<li>
O Audacity 1.2 apresenta seu novo formato de arquivos de projeto, baseado em XML. É possível que os arquivos salvos em qualquer versão anterior do programa sejam lidos.
</li>

<li>
O Audacity 1.2 utiliza a nova biblioteca <a href="http://www.underbit.com/products/mad/">libmad</a> para a importação mais rápida de arquivos do tipo MP3. A biblioteca <a href="http://www.zip.com.au/~erikd/libsndfile/">libsndfile</a> de Erik de Castro Lopo oferece compatibilidade melhorada com outros tipos de arquivo de áudio não-comprimidos.
</li>

<li>
A última versão dos arquivos da <a href="http://www.vorbis.com/">Vorbis</a> está incluída com o programa, para uma melhor exportação em formato OGG Vorbis.
</li>

<li>
As janelas de "Importar" e "Abrir" agora permitem a seleção de vários arquivos para serem abertos/importandos ao mesmo tempo  em um mesmo projeto. O novo formato "LOF" permite que o programa abra um grupo de arquivos com compensação offset em um arquivo de texto.
</li>
</ul>


<h3>Nova interface:</h3>

<ul>
<li>
As novas barras de ferramentas "Edição" e "Mixagem" trazem rápido acesso às funções mais comuns.
</li>

<li>
A nova ferramenta "Desenhar" permite o ajuste de amostras individualmente, estando com o Zoom máximo. E a nova "Multi-ferramenta" acessa diferentes comandos de edição simultaneamente.
</li>

<li>
Novos atalhos de teclado foram adicionados, podendo agora ser personalizados completamente pelo usuário.
</li>

<li>
Novos comandos:
 <ul>
  <li>Reprodução contínua.  Pressione "L", segure Shift ao clicar em Executar.</li>
  <li>Pressione "1" para executar uma amostra de 1 segundo ao redor do cursor.</li>
 </ul>
</li>

<li>
A rodinha do mouse agora opera os comandos de Aproximar e Diminuir o Zoom.
</li>

<li>
As faixas de áudio podem ter o zoom ampliado verticalmente, clicando nas réguas verticais ao lado de cada faixa. Segure Shift ao clicar ou use o botão direito para diminuir o zoom.
</li>

<li>
As réguas de tempo e a barra de status podem exibir o tempo com diversas medidas diferentes, incluindo segundos, amostras e quadros de vídeo PAL, NTSC e Filme
</li>

<li>
A interface agora está disponível em diferentes línguas, graças ao trabalho de times de tradutores voluntários. Você também pode ajudar <a href="translation/">traduzindo o Audacity</a>.
</li>
</ul>
