<?php
/*
 * Copyright 2004 Dominic Mazzoni
 * Copyright 2005 - 12 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/3.0/
 */
  require_once "main.inc.php";
  $pageId = "plugins";
  $pageTitle = _("Download Nyquist Plug-Ins");
  include "../include/header.inc.php";

  echo "<h2>$pageTitle</h2>";
  echo _('<p>These are optional generator, effect and analyze plug-ins for the Audacity audio editor, written in the <a href="/help/nyquist">Nyquist programming language</a>. Nyquist plug-ins support Windows, Mac and GNU/Linux.</p>');
  echo "<ul><li>";
  echo _('<a href="http://wiki.audacityteam.org/wiki/Download_Nyquist_Plug-ins">Complete list of optional Nyquist plug-ins</a> on the <a href="http://wiki.audacityteam.org/wiki/">Audacity Wiki</a>.');
  echo "</li><li>";
  echo _('See the main <a href="plugins">plug-ins page</a> for installation instructions.');
  echo "</li><li>";
  echo _('Help us test <a href="http://forum.audacityteam.org/viewforum.php?f=42">new Nyquist plug-ins still under development</a> on the Audacity <a href="http://forum.audacityteam.org/">Forum</a>.');
  echo "</li></ul>";
  echo _('<p>Anyone can customise these plug-ins by opening the .ny file in a text editor, just as can be done with the Nyquist plug-ins already <a href="http://wiki.audacityteam.org/wiki/Download_Nyquist_Plug-ins#shipped">shipped with Audacity</a>.</p>');
?>

<!-- Commenting out old list of plug-ins as we now add new plug-ins to
and host them on http://wiki.audacityteam.org/wiki/Download_Nyquist_Plug-ins . 

  function plugins($process) {
    if ($dir = @opendir("../nyquist/")) {
      while(($file = readdir($dir)) != false) {
        if (ereg("^(.*)\.ny$", $file, $matches)) {
          $base = $matches[1];
          system("grep -q \";type generate\" ../nyquist/$file", $result);
          if ($result == $process) {
            $nameline = `grep ";name" ../nyquist/$file`;
            if (preg_match(';name "*([0-9a-zA-Z ]*);', $nameline, $matches)) {
              $name = $matches[1];
              print "<b>$name</b> | \n";
              print "<a href=\"../nyquist/$base.ny\">"."View $base.ny"."</a> | ";
              print "<a href=\"../nyquist/$base.zip\">"."Download $base.zip"."</a>\n";
              print "<p>\n";

              if (file_exists("../nyquist/${base}1.mp3")) {
                print "Example audio clips: ";
                for($i=1; $i<=9; $i++) {
                  if (file_exists("../nyquist/${base}$i.mp3")) {
                    print "<a href=\"../nyquist/${base}$i.mp3\">[MP3 Clip $i]</a>";
                    print "&nbsp;&nbsp;";
                  }
                }
                print "<p>\n";
              }
              system("cat ../nyquist/$base.html");
              print "<p><hr><p>\n";
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
  print "<p>\n";
  plugins(1);
-->

<?php
  include "../include/footer.inc.php";
?>
