<?php

require_once 'main.inc.php';
$title = $screenshotsStr;
include 'top.inc.php';

BoxTop($screenshotsStr);

?>

<center>
<p>
<?php print "<a href=\"screenshots-win98.php$langQueryStr\">"; ?><img src="images/screen/win98/thumb.png" border=0 width=299 height=255><br>Windows 98</a>
</p>

<p>
<?php print "<a href=\"screenshots-macosx.php$langQueryStr\">"; ?><img src="images/screen/macosx/thumb.png" border=0 width=300 height=229><br>Mac OS X</a>
</p>

<p>
<?php print "<a href=\"screenshots-linux.php$langQueryStr\">"; ?><img src="images/screen/linux/thumb.png" border=0 width=300 height=265><br>Linux</a>
</p>
</center>


<?php

BoxBottom();

include 'bottom.inc.php';

?>






