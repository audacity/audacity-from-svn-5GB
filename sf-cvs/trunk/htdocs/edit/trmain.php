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

  $updatefile = "../updates/$lang/main.inc.php";

  $target = $updatefile;
  if (!file_exists($target)) {
    $target = "../$lang/main.inc.php";
  }

  if ($update == "true") {
	 if (!is_dir("../updates/$lang")) {
      mkdir("../updates/$lang", 0777);
    }

    $fp = fopen($updatefile, "w");
    if (!($fp)) {
      $update = "Could not write temp file.";
    }
    else {
      fwrite($fp, "<?php\n");
      foreach($english as $key => $phrase) {
        $val = stripslashes(${$key});
        if ($val != "") {
          fwrite($fp, "\$$key = \"$val\";\n");
        }
      }
      fwrite($fp, "?>\n");
      fclose($fp);
      chmod($updatefile, 0666);
    }
    $target = $updatefile;
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

if ($update == "true") {
  BoxTop("Success");
  print "<center><p>Main phrase list updated.";
  print "</p>\n";
  print "<p>\n";
  print "<form action=trmain.php method=post>\n";
  print "<input type=hidden name=lang value=$lang>\n";
  print "<input type=hidden name=passwd value=\"$passwd\">\n";
  print "<input type=submit value=\"Back to translation editor\">\n";
  print "</form>\n";
  print "</p>\n";
  print "</center>\n";
  BoxBottom();
  print "<p>\n";
}
else if ($update != "") {
  BoxTop("Error");
  print "<center><p>$update<p></center>\n";
  BoxBottom();
  print "<p>\n";
}

BoxTop("Edit Translations");
?>

<p>


<form action=trmain.php method=post>

<table cellpadding=4 cellspacing=0>
<tr>
<td bgcolor=#ddddff><b>label</b></td>
<td bgcolor=#ddddff><b>en</b></td>
<td bgcolor=#ddddff><b><?php print $lang; ?></b></td>
</tr>

<?php

$odd = 0;
$tds = array("<td>", "<td bgcolor=#eeeeee>");
foreach($english as $key => $phrase) {
  print "<tr>";
  print "$tds[$odd]<font size=-1>$key</font></td>";
  print "$tds[$odd]$phrase</td>";
  print "$tds[$odd]";
  print "<input name=$key value=\"$translation[$key]\" size=50>";
  print "</td>";
  print "</tr>\n";
  if ($odd == 1)
    $odd = 0;
  else
    $odd = 1;
}
print "</table>\n";
print "</p>\n";

print "<p><center>";
print "<input type=submit value=\"Update Changes\">";
print "</center></p>\n";

print "<input type=hidden name=update value=true>\n";
print "<input type=hidden name=lang value=$lang>\n";
print "<input type=hidden name=passwd value=\"$passwd\">\n";
print "</form>\n";

BoxBottom();

?>

</body>
</html>
