<?php BoxTop($develNewsStr); ?>

<p>
　このウェブページは、開発者の全員がめざしていることをあなたに知らせる場所になる予定です。しばらくの間はとりあえず、Audacity1.1(ベータ版)に関する情報の一部をここに書いておきます。
</p>

<p>
<b>バージョン1.1での新しい特徴</b>
<pre>
  * コアオーディオプロセッシング:
    - 24ビットと32ビットのサンプリングフォーマットをサポート
    - 自動的なリアルタイムリサンプリング(線形補間を使用)
  * エフェクト:
    - Linux / UnixでLADSPAプラグインをサポート
  * ファイルフォーマット:
    - 新しいXMLベースのAudacityプロジェクトフォーマット
    - Ogg Vorbisをフルにサポート (読み込みと書き出し)
    - Unixではコマンドラインプログラムへの出力
    - 多種の無圧縮オーディオファイルの読み書きをサポート
      ADPCMウェーブファイルをも含む
  * ツールバー
    - 新しいツールバー描画コード
      あなたのOSの色に自動的に適応する
    - 新しいツールバーボタン(最初に戻る, 最後に行く)
    - 新しい編集ツールバー
    - ツールバーボタンは使えないときはディスエーブルになる
  * ユーザーインターフェイス
    - フルカスタマイズ可能なキーボードコマンド
    - 再生中・録音中はオートスクロール
    - 新しい目盛り
      メインビューとFFTフィルターエフェクトで使われている
    - 波形は、より明るい色でピーク値の平均値を表示する
  * ローカライゼーション
    - Audacityは他の外国の言語にも対応
</pre>
</p>

<p>
<b>使用しているライブラリ (バージョン1.1.0以降):</b>
<table border=0 cellpadding=8 cellspacing=2>
<tr>
<th>Library</th>
<th>Purpose</th>
</tr>
<tr>
<td bgcolor="#ccccff">
<a href="http://wxwidgets.org/">wxWidgets</a>
<td bgcolor="#ccccff">
これは、GUI(メニュー、ボタン、ウィンドウ、描画など)をMac, Windows, Unixシステム上でネイティブに走らせるための、クロスプラットフォームのライブラリです。wxWidgetsは他にも便利なC++クラスを提供します。Audacityは100％このライブラリに依存しています。クロスプラットフォームの開発をするのであればこのライブラリを使うことを強く推奨します。
</tr>

<tr>
<td bgcolor="#ccccff">
<a href="http://www.mars.org/home/rob/proj/mpeg/">libmad</a>
<td bgcolor="#ccccff">
<p>MADはMPEG Audio Decoderを表します。これはいくつかあるフリー(GPL)のMP3デコーダーのうちのひとつです。そして、整数だけを使っている唯一のライブラリであり、24ビットの出力能力をもつ唯一のライブラリでもあります(私たちはもうその機能を利用していませんが)。たとえ16ビットの入力で普通に作られたMP3ファイルだとしても、私たち自身のディザリング機能を使って24ビットの出力をすることができ、より高品質なオーディオクオリティへと導きます。このライブラリはとても高速(私たちが以前に使っていたxaudioと比べても)で、しかも非常に安定しています。このライブラリはRob Leslieによって書かれています。</p>
<p>このライブラリはMP3ファイルを読み込みたい場合のみ必要です。</p>
</tr>
<tr>
<td bgcolor="#ccccff">
<a href="http://www.mars.org/home/rob/proj/mpeg/">libid3tag</a></td>
<td bgcolor="#ccccff"><p>
このライブラリもまたRob Leslie(上述のlibmadの作者)により書かれています。これは、MP3ファイル内のID3タグを読み書きするための、上質でシンプルなライブラリです。</p>
<p>このライブラリはオプションです。もしリンクされていれば、MP3ファイルを書き出す時にはタグダイアログが表示されますし、読み込む時にはタグもいっしょに読み込むことでしょう。libid3tagは別々に公開されているのではなく、MADの一部に含まれていることにご注意ください。</p>
</td>
</tr>
<tr>
<td bgcolor="#ccccff">
<a href="http://www.xiph.org/ogg/vorbis/">libogg<br>libvorbis<br>libvorbisfile</a>
</td>

<td bgcolor="#ccccff">
<p>Ogg Vorbisは、フリーのオーディオ圧縮フォーマットであると同時に、このフォーマットでのエンコード・デコードを行うライブラリでもあります。これはMP3に取って代わることを目的としており、多くの人々が音質や圧縮率の点でMP3と同等かそれ以上に良いと感じています。Audacityの最新(のベータ)版は、Ogg Vorbisファイルの読み込みと書き出しの両方に対応しています。
</p>
</td>
</tr>
<tr>
<td bgcolor="#ccccff"
>
<a href="http://www.portaudio.com">portaudio</a>
</td>
<td bgcolor="#ccccff"
>
このライブラリは、共通のAPIを使って複数のプラットフォーム上でオーディオ入出力を可能にします。オーディオ入出力におけるプラットフォーム間の違いを吸収し、非常によい性能を供給します。ネイティブのオーディオコードは、Windows (MME と DirectX),
Unix/OSS, MacOS 9用は安定しており、MacOS X, Linux/ALSA用はできたところ、またLinux/aRts用は現在製作中です。
</td>
</tr>
<tr>
<td bgcolor="#ccccff"><a href="http://www.zip.com.au/~erikd/libsndfile/">libsndfile</a></td>

<td bgcolor="#ccccff">
これはWAV, AIFF, AUなどのオーディオファイルの読み書きをするための新しいライブラリです。ADPCMのようなシンプルな圧縮(ロッシーではない圧縮)ファイルも取り扱います。
</td>
</tr>
</table>
</p>


<?php BoxBottom(); ?>
