<?php

require_once 'main.inc.php';
$title = $screenshotsStr;
include 'top.inc.php';

BoxTop($screenshotsStr);

?>

<center>

<p>
<?php print "$clickEnlargeStr"; ?>
</p>

<p>
<a href="images/audacity-1.2.0-pre1.png"
><img src="images/audacity-1.2.0-pre1-small.png" border=0 width=273 height=226></a>
<br>
Audacity 1.2.0-pre1 / Linux
</p>

<p>
<a href="images/audacity-1.0.png"
><img src="images/audacity-1.0-small.png" border=0 width=304 height=224></a>
<br>
Audacity 1.0.0 / Linux.
</p>

<p>
<a href="images/audacity-osx-screen.png"
><img src="images/audacity-osx-small.png" border=0 width=314 height=224></a>
<br>
Audacity 1.0.0 / Mac OS X
</p>

</center>

<?php

BoxBottom();

include 'bottom.inc.php';

?>






