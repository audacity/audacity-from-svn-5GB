<html>
<head>
<title>Audacity Website Translation Editor</title>
</head>

<body>

<?php

include 'main.inc.php';

$numLangs = 0;
$dir = opendir("..");
while($x = readdir($dir)) {
  if (ereg("^([a-z][a-z])$", $x, $matches)) {
    $numLangs++;
    $langs[$numLangs] = $matches[1];
  }
}
closedir($dir);

BoxTop('Audacity Website Translation Editor');

?>

<center>

<p>
<h1>
Under development.  Please don't use it yet...
</h1>
</p>

<form action=trmain.php method=post>

<p>
Choose language to edit:

<select name=lang>
<?php
foreach ($langs as $lang) {
  print "<option>$lang\n";
}
?>
</select>
</p>

<p>
Enter the password:
<input name=passwd type=password size=12>
</p>

<p>
<input type=submit value="Begin Editing">
</p>

</form>

</center>

<?php
BoxBottom();
?>

</body>
</html>
