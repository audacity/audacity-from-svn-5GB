<?php BoxTop($develNewsStr); ?>

<p>
Esta página mostra em que os desenvolvedores estão envolvidos neste momento.</p>

<p>
<b>Novos recursos da versão 1.1:</b>
<pre>
  * Processamento central:
    - Suporte a amostras de som em 24-bit e 32-bit;
    - Re-amostragem em tempo real com interpolação linear.
  * Efeitos:
    - Suporte a plugins LADSPA em Linux / Unix
  * Tipos de arquivo:
    - Novo tipo de arquivo baseado em XML para os projetos;
    - Suporte completo a Ogg Vorbis;
    - Suporte a gravação e leitura em mais tipos de áudio não-comprimidos 
      incluindo ADPCM-WAV.
  * Barra de ferramentas:
    - Cores que se ajustam às do sistema operacional;
    - Novos botões;
    - Nova barra de edição.
  * Interface:
    - Dicas sobre as ferramentas na barra de status;
    - As telas rolam acompanhando o cursor durante a reprodução / gravação;
    - Botão de Pausa;
    - Ferramenta de desenho de pontos em 3 diferentes modos;
    - Ajuste do tamanho de cada faixa na tela mais fácil;
    - Barra de ferramentas sensível ao contexto;
    - Funções de Zoom aprimoradas com centralização automática;
    - Modo de seleção "Snap-to"
    - Mudança de ordem das faixas pelo mouse;
    - Funções de alinhamento e agrupamento de ítens;
    - Salva e restaura posições do cursor e seleções;
    - Janela de Histórico com "desfazer" ilimitados.
  * Internacionalização:
    - Suporte melhorado a novas línguas de interface;
    - Suporte a Português do Brasil!
    - Nas versões para Windows e MacOS, o sistema procura por traduções 
      nas pastas Languages e Plug-ins do programa.
    - Na versão para Unix, procura por traduções em <prefix>/share/locale e
      em qualquer pasta dentro da variável de ambiente AUDACITY_PATH
</pre>
</p>

<p>
<b>Tabela de bibliotecas necessárias para o funcionamento da versão 1.1.0 e posteriores:</b>
<table border=0 cellpadding=8 cellspacing=2>
<tr>
<th>Biblioteca</th>
<th>Uso</th>
</tr>
<tr>
<td bgcolor="#ccccff"
><a href="http://www.wxwindows.org">wxWindows</a>
<td bgcolor="#ccccff"> Esta biblioteca possibilita o uso dos botões da interface em sistemas Windows, MacOS e Linux. O Audacity utiliza-se 100% desta biblioteca e recomendamos fortemente o seu uso para o desenvolvimento em múltiplas plataformas.
</tr>

<tr>
<td bgcolor="#ccccff"
><a href="http://www.mars.org/home/rob/proj/mpeg/">libmad</a>
<td bgcolor="#ccccff"
><p>"MAD" quer dizer "Mpeg Audio Decoder". Este é um dos poucos codecs para importação em formato MP3 disponíveis com licença GPL. É também o único capaz de produzir saída de áudio em 24-bit (que ainda não foi totalmente aproveitada). Ainda que os áudios MP3 sejam criados com entrada em 16-bit, um importador com capacidade de saída em 24-bit permite uma qualidade áudio muito superior e um controle maior sobre sua produção. Há ainda a vantagem da extrema rapidez - comparada ao xaudio, utilizado nas versões anteriores. É desenvolvido por Rob Leslie. </p>
    <p>Necessária para a importação de áudio em formato MP3</p>
</tr>
<tr>
<td bgcolor="#ccccff"
><a href="http://www.mars.org/home/rob/proj/mpeg/">libid3tag</a></td>
<td bgcolor="#ccccff"
><p> Escrita por Rob Leslie (o mesmo autor da "libmad"), esta biblioteca permite a leitura das etiquetas ID3, como nome de faixa, compositor, álbum, etc, anexados aos arquivos MP3.</p>
    <p>Esta biblioteca é opcional. Note que é distribuída junto com o MAD.</p>
</td>
</tr>
<tr>
<td bgcolor="#ccccff"><a ref="http://www.xiph.org/ogg/vorbis/">
libogg<br>libvorbis<br>libvorbisfile</a>
</td>

<td bgcolor="#ccccff">
<p>O Ogg Vorbis é um compressor e descompressor de áudio no formato OGG. Foi desenvolvido para substituir o MP3 como formato de compressão e é capaz de exportar áudio com qualidade superior ao seu substituto. O Audacity é capaz de importar e exportar neste formato.
</p>
</td>
</tr>
<tr>
<td bgcolor="#ccccff"
>
<a href="http://www.portaudio.com">portaudio</a>
</td>
<td bgcolor="#ccccff">
Esta biblioteca permite a entrada e saída de áudio utilizando uma API comum a diversas plataformas. Ela adapta as especificidades de cada E/S de som nos diferentes sistemas operacionais com boa performance. Código estável disponível para Windows (MME e Directx), Unix/OSS e MacOS 9. Portabilidade para MacOS-X, Linux/ALSA e Linux/aRTs está em desenvolvimento.
</td>
</tr>
<tr>
<td bgcolor="#ccccff"
><a href="http://www.zip.com.au/~erikd/libsndfile/">libsndfile</a></td>

<td bgcolor="#ccccff"> Esta biblioteca permite ler e salvar arquivos de áudio WAV, AIFF e AU. Permite a manipulação de compressões simples como a ADPCM, sem perdas.
</td>
</tr>
</table>
</p>


<?php BoxBottom(); ?>
