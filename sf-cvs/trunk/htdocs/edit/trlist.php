<?php

include 'passwd.php';
include 'main.inc.php';

if (crypt($passwd, $correctPassword) != $correctPassword) {
  print "<html><body>";
  BoxTop('Incorrect Password');
  print "Sorry, that password was incorrect.";
  BoxBottom();
  print "</body></html>";
  exit();
}

$fp = fopen("../main.inc.php", "r");
if ($fp) {
  while($line = fgets($fp, 1024)) {
    if (ereg("\\\$([a-zA-Z0-9]+).*=.*\"(.*)\"", $line, $matches)) {
      $english[$matches[1]] = $matches[2];
    }
  }
  fclose($fp);

  $target = "../updates/$lang/main.inc.php";
  if (!file_exists($target)) {
    $target = "../$lang/main.inc.php";
  }

  $fp = fopen($target, "r");
  if ($fp) {
    while($line = fgets($fp, 1024)) {
      if (ereg("\\\$([a-zA-Z0-9]+).*=.*\"(.*)\"", $line, $matches)) {
        $translation[$matches[1]] = $matches[2];
      }
    }
    fclose($fp);
  }
}

if (!($fp)) {
  print "<html><body>";
  BoxTop('Error');
  print "Error opening one of the language files.";
  BoxBottom();
  print "</body></html>";
  exit();
}

$charset = $translation["charset"];
header("Content-type: text/html; charset=$charset");

?>

<html>
<head>
<title>Edit Translations</title>
</head>
<body>

<?php

if ($newtranslation) {
  $target = "../updates/$lang/$file";
  $fp = fopen($target, "w");
  if (!($fp)) {
    $update = "Could not write updated file to disk.";
  }
  else {
    fwrite($fp, stripslashes($newtext));
    fclose($fp);
    $val = stripslashes(${$key});
    chmod($target, 0666);
    $update = "Successfully updated translation of $file.";
  }
}

print "$update";


BoxTop("Main Phrase List");

$yes=0;
$total=0;
foreach($english as $key => $phrase) {
	if ($translation[$key]) {
      $yes++;
   }
   $total++;
}

if ($yes == $total) {
	print "All phrases translated.";
}
else if ($yes == 0) {
	print "No phrases have been translated.";
}
else {
	print "$yes of $total phrases translated.";
}

print "<form action=trmain.php method=post>\n";
print "<input type=hidden name=lang value=$lang>\n";
print "<input type=hidden name=passwd value=\"$passwd\">\n";
print "<input type=submit value=\"Edit Main Phrase List\">\n";
print "</form>\n";

BoxBottom();

print "<p>\n";

BoxTop("Edit Translations");

?>

<p>
<form action=trfile.php method=post>

<table cellpadding=4 cellspacing=0 width=100%>
<tr>
<td bgcolor=#ddddff></td>
<td bgcolor=#ddddff><b>File</b></td>
<td bgcolor=#ddddff width=30%><b>Status</b></td>
<td bgcolor=#ddddff width=50%></td>
</tr>

<?php

if ($dir = @opendir("../en/")) {
  $n = 0;
  while (($file = readdir($dir)) !== false) {
    if (ereg(".php$", $file, $matches) && $file != "main.inc.php") {
      $old = filemtime("../en/$file");
      $s = "";
      $bg = "";
      $b1 = "";
      $b2 = "";
      $n++;
      if (is_file("../updates/$lang/$file")) {
	     $s = "update pending...";
        $langfile = "../updates/$lang/$file";
      }
      else {
	     $langfile = "../$lang/$file";
        if (is_file($langfile)) {
          $new = filemtime($langfile);
          if ($new > $old + 60)
            $s = "up to date";
          else {
            $when = date("Y-m-d", $old);
            $s = "modified $when";
            $bg = " bgcolor=#eeeeee";
            $b1 = "<b>";
            $b2 = "</b>";
          }
        }
        else {
          $s = "not translated";
          $bg = " bgcolor=#eeeeee";
          $b1 = "<b>";
          $b2 = "</b>";
        }
      }

      print "<tr>\n";
      print "<td$bg><input type=submit value=\"Edit Translation\" name=\"e$n\"></td>";

      if (is_file("../$file")) {
	     print "<td$bg><a href=\"http://audacity.sourceforge.net/";
	     print "$file?lang=$lang\">$b1$file$b2</a></td>";
      }
      else {
	     print "<td$bg>$b1$file$b2</td>";
      }

      print "<td$bg>$b1$s$b2</td>";
      print "<td$bg></td>";
      print "</tr>\n";
    }
  }  
  closedir($dir);
}

print "</table>\n";

print "<input type=hidden name=lang value=$lang>\n";
print "<input type=hidden name=passwd value=\"$passwd\">\n";
print "</form>\n";


print "<form action=trfile.php method=post>\n";
print "<input type=hidden name=lang value=$lang>\n";
print "<input type=hidden name=passwd value=\"$passwd\">\n";
print "<input type=submit value=\"Refresh\">\n";
print "</form>\n";

BoxBottom();

?>

</body>
</html>
