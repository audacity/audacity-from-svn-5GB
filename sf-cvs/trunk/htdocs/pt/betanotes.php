<?php BoxTop("$releaseNotesStr $betaVersion"); ?>

<pre>
Problemas encontrados na versão 1.1.3:
  - O programa insere um arquivo chamado "Audacity-Commands.xml" em alguma pasta no computador onde está instalado. Este arquivo é necessário para executar o Audacity. Na próxima versão, este arquivo será salvo na pasta do próprio programa. 

New features in Audacity 1.1.3: (Em inglês, apenas para referência)
  * User Interface
    - New Mixer toolbar allows you to control the output
      volume, input volume, and input source directly
      from Audacity.
    - Every track now has its own gain and pan controls.

  * File I/O
    - Uses improved project file format.  (Unfortunately reading
      previous formats, including 1.1.1, is not supported.)
    - Block files (stored in Audacity project directories) now
      use the standard AU format.  Though some Audacity
      meta-information is in these files, they can now be
      read by many other popular audio programs as well.
    - Fixed some bugs relating to reading/writing audio
      files with more than 16 bits per sample.
    - Import RAW is functional again, with a simpler GUI
      but support for far more file formats.  The
      autodetection algorithms are much more accurate than
      in 1.0.

  * Audio I/O
    - Completely rewritten audio I/O, with lower latency
      and minimal chance of buffer underruns while
      recording.

  * Resampling
    - Using high quality resampling algorithms, with the
      option of better quality for mixing than for real-time
      playback

    - Preliminary support for Time Tracks, for changing
      playback speed over time.

  * Many more bug fixes and new features




Problemas encontrados na versão 1.1.1:

  * Não utilize a opção "Executar Diagnóstico" do menu Ajuda - além de instável, 
    pode fazer o sistema parar.
    Esta função é feita para uso avançado apenas e não afeta a edição de áudio.

  * Mac OS X: Alguns segundos de áudio podem se perder se o botão do mouse for 
    mantido pressionado durante a captura. Deixe o sistema livre de interrupções 
    voluntárias enquanto é feita a capptura de áudio.


Novos recursos da versão 1.1.1:

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
  * Efeitos:
    - Efeitos divididos em três categorias: Inserir, Efeitos e Analisar;
    - O menu Inserir permite a inserção de Silêncio, Ruído branco ou Tom
    - Suporte a efeitos escritos em linguagem Nyquist
  * Internacionalização:
    - Suporte melhorado a novas línguas de interface;
    - Suporte a Português do Brasil!
    - Nas versões para Windows e MacOS, o sistema procura por traduções 
      nas pastas Languages e Plug-ins do programa.
    - Na versão para Unix, procura por traduções em <prefix>/share/locale e
      em qualquer pasta dentro da variável de ambiente AUDACITY_PATH
  * Mac OS X:
    - Suporte a novas placas de som;
    - Áudio full-duplex
    - Exportação em formato MP3 via LameLib Carbon
  * Unix:
    - O Audacity agora dispõe de uma página descrevendo suas opções de 
      linha de comando.
      options and how to set the search path)
  * Tipos de arquivo:
    - A nova versão utiliza a biblioteca libsndfile 1.0, com problemas corrigidos 
      e uma melhor performance.




Novos recursos da versão 1.1.0:

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


Novas bibliotecas do Audacity 1.1:

  * libmad - para importação em MP3;
  * libid3tag  - para edição das propriedades do MP3;
  * libsndfile - leitura e gravação em novos formatos de áudio
  * PortAudio  - para leitura e gravação em múltiplas plataformas.
</pre>

<?php BoxBottom(); ?>
