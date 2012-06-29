<?php
/*
 * Copyright 2004 Dominic Mazzoni
 * Copyright 2005 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
  require_once "main.inc.php";
  $pageId = "plugins";
  $pageTitle = _("Download Nyquist Plug-Ins");
  include "../include/header.inc.php";

  echo "<h2>$pageTitle</h2>";
  // i18n-hint: Sorry, the actual plug-in descriptions are not translatable yet.
  echo _('<p>These are optional plug-in effects for the Audacity sound editor.  They are written in the <a href="../help/nyquist">Nyquist programming language</a>.  For installation instructions, see the main <a href="plugins">plug-ins page</a>.</p>');

  echo "<hr>";

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

  include "../include/footer.inc.php";
?>
