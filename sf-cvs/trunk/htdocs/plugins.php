<?php

  require_once 'main.inc.php';
  $title = "$pluginsStr";
  include 'top.inc.php';

  BoxTop($pluginsStr);

  include GetTranslation("plugins");
?>

<p>
<b>Cross-platform <a href="nyquist.php">Nyquist</a>
plug-ins (contributed by users)</b>:
</p>
<p>
(To use, unzip and drop in Audacity's <b>Plug-ins</b> folder.
</p>

<!--
<li><a href="nyquist/.zip"></a>
    <a href="nyquist/.html">[Info]</a>
-->

<ul>
<li><a href="nyquist/all.html">All Nyquist Plug-in Descriptions</a>
    <p>

<li><b>Generators</b>
    <p>

<?php

function plugins($process) {
 if ($dir = @opendir("nyquist/")) {
  while(($file = readdir($dir)) != false) {
    if (ereg("^(.*)\.ny$", $file, $matches)) {
      $base = $matches[1];
      system("grep -q \";type generate\" nyquist/$file", $result);
      if ($result == $process) {
        $nameline = `grep ";name" nyquist/$file`;
        if (preg_match(';name "*([0-9a-zA-Z ]*);', $nameline, $matches)) {
          $name = $matches[1];
          print "<li>";
          print "<a href=\"nyquist/$base.zip\">";
          print "$name</a>\n";
          print "<a href=\"nyquist/$base.html\">[Info]</a>\n";
          if (file_exists("nyquist/${base}1.mp3")) {
            print "<a href=\"nyquist/${base}1.mp3\">[MP3 Clip]</a>\n";
          }
        }
        else {
          print "<li>No name from $nameline\n";
        }
      }
    }
  }
 }
}

plugins(0);

?>

<p>
<li><b>Effects</b>
<p>

<?php

plugins(1);

?>

</ul>

<b>Windows:</b>
<ul>

  <li>
  <a href="http://kvr-vst.com">KVR-VST.com</a> is one of the best
  sites for VST and other plug-ins</a>.

  <li>
  <a href="http://www.digitalfishphones.com"
  >Fish Fillets bundle</a> - includes a Compressor, a DeEsser and a
  Noisegate.

  <li><a href="http://www.smartelectronix.com/~bram/"
  >Bram/Smarteletronix</a> - Cyanide is one of the best
  distortion units, H20 is an interesting compressor, and
  SupaPhaser is a legend among VST users.

  <li><a href="http://www.mda-vst.com/effects.htm"
  >Maxim Digital Audio</a> - 30 great free plug-ins.

  <li>
  <a href="http://www.cs.bath.ac.uk/~rwd/"
  >Richard Dobson's Plug-ins</a> - includes a good
  Pitch Shifter.

  <li>
  <a href="http://www.sounduser.com/software/vstplugs/p1.cfm">
  Free Plug-Ins from sonicstate.com</a>

  <li><a href="http://www.audiomidi.com/plugins/vst.cfm">
  Commercial Plug-Ins</a> from <a href="http://audiomidi.com/">
  audiomidi.com</a>

  <li><a href="http://www.crmav.com/gear/index_28.shtml">Plug-ins</a>
  from <a href="http://www.crmav.com/">CRMAV</a>

  <li><a href="http://www.smartelectronix.com"
  >Freeware plug-ins from SmartElectronix</a>

  <li><a href="http://www.sinusweb.de/"
  >Free and shareware plug-ins from Sinus</a>

  <li><a href="http://www.bojo.dk/product.html"
  >Freeware Windows plug-in pack at Bojo.dk</a>

  <li>
  <a href="http://www.databaseaudio.co.uk"
  >Database Audio</a> - this site has lots of VST plug-ins, along with
  many other free tools for electronic musicians.

</ul>

<b>Mac:</b>
<ul>

  <li>
  <a href="http://kvr-vst.com">KVR-VST.com</a> is one of the best
  sites for VST and other plug-ins</a>.

  <li>
  <a href="http://www.digitalfishphones.com"
  >Fish Fillets bundle</a> - includes a Compressor, a DeEsser and a
  Noisegate.

  <li><a href="http://www.smartelectronix.com/~bram/"
  >Bram/Smarteletronix</a> - Cyanide is one of the best
  distortion units, H20 is an interesting compressor, and
  SupaPhaser is a legend among VST users.

  <li><a href="http://www.mda-vst.com/effects.htm"
  >Maxim Digital Audio</a> - 30 great free plug-ins.

  <li>
  <a href="http://www.cs.bath.ac.uk/~rwd/"
  >Richard Dobson's Plug-ins</a> - includes a good
  Pitch Shifter.

  <li><a href="http://www.audiomidi.com/plugins/vst.cfm">
  Commercial Plug-Ins</a> from <a href="http://audiomidi.com/">
  audiomidi.com</a>

  <li><a href="http://www.macmusic.org/eng/share/sh_freewares.shtml">
  Freeware and shareware plugins</a> from
  <a href="http://www.macmusic.org/">MacMusic</a>.

  <li><a href="http://www.smartelectronix.com"
  >Freeware plug-ins from SmartElectronix</a>

  <li><a href="http://www.greenoak.com/vst.html"
  >Free plug-ins from GreekOak</a>

  <li><a href="http://www.crmav.com/gear/index_28.shtml">Plug-ins</a>
  from <a href="http://www.crmav.com/">CRMAV</a>

  <li>
  <a href="http://www.databaseaudio.co.uk"
  >Database Audio</a> - this site has lots of VST plug-ins, along with
  many other free tools for electronic musicians.


</ul>

<p>
Note: most Mac VST plug-ins do not work on Mac OS X.  You need special
Carbon VST plug-ins.
</p>

<b>Mac OS X (Carbon) plug-ins</b>

<ul>

  <li>
  <a href="http://kvr-vst.com">KVR-VST.com</a> is one of the best
  sites for VST and other plug-ins</a>.

  <li>
  <a href="http://www.digitalfishphones.com"
  >Fish Fillets bundle</a> - includes a Compressor, a DeEsser and a
  Noisegate.

  <li><a href="http://www.smartelectronix.com/~bram/"
  >Bram/Smarteletronix</a> - Cyanide is one of the best
  distortion units, H20 is an interesting compressor, and
  SupaPhaser is a legend among VST users.

  <li><a href="http://www.mda-vst.com/effects.htm"
  >Maxim Digital Audio</a> - 30 great free plug-ins.

<li><a href="http://www.lone-electron.com/Canz3D"
    >Canz3D - free spatial simulation plug-in</a>

<li>Some Carbon VST plug-ins come with <a href="http://www.bias-inc.com/"
    >Deck or Peak</a> for Mac OS X.

</ul>

<p>
<br>
</p>

<?php

  BoxBottom();

  include 'bottom.inc.php';

?>
