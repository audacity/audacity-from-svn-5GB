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

if ($dir = @opendir("../en/")) {
  $n = 0;
  while (($file = readdir($dir)) !== false) {
    if (ereg(".php$", $file, $matches) && $file != "main.inc.php") {
      $n++;
      $v = "e$n";
      if ($$v) {
			$srcfile = "../en/$file";
			$dstfile = "../updates/$lang/$file";
         if (!file_exists($dstfile)) {         
			  $dstfile = "../$lang/$file";
         }
			$filename = "$file";
      }
    }
  }  
  closedir($dir);
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

BoxTop("$filename");
?>

<p>
<form action=trlist.php method=post>

<?php

print "<center><b><font size=+1>English(do not edit)</font></b></center>\n";
print "<textarea readonly rows=25 cols=80>\n";
$fp = fopen($srcfile, "r");
if ($fp) {
  while($line = fgets($fp, 1024)) {
    print "$line";
  }
}
fclose($fp);
print "</textarea>\n";

print "<center><b><font size=+1>Your Translation ($lang):</font></b></center>\n";
print "<textarea name=newtext rows=25 cols=80>\n";
if (is_file($dstfile)) {
  $fp = fopen($dstfile, "r");
  if ($fp) {
    while($line = fgets($fp, 1024)) {
      print "$line";
    }
  }
  fclose($fp);
}
print "</textarea>\n";

print "<center>\n";
print "<input type=submit name=newtranslation value=\"Submit Translation\">";
print "<input type=submit name=cancel value=\"Cancel\">";
print "</center>\n";

print "<input type=hidden name=lang value=$lang>\n";
print "<input type=hidden name=file value=$filename>\n";
print "<input type=hidden name=passwd value=\"$passwd\">\n";

print "</form>\n";

BoxBottom();

?>

</body>
</html>
