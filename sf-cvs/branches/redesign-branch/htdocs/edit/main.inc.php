<?php

function BoxTop($title)
{
	print '<table border="0" cellpadding="1" cellspacing="0" bgcolor="#000099" width="100%"><tr><td>';
	print '<table border="0" cellspacing="1" cellpadding="4" width="100%">';
	print '<tr><td align="center" bgcolor="#dddddd">';
	print '<font size="+1"><b>';
	print $title;
	print '<b></font>';
	print '</td></tr>';
	print '<tr><td align="left" bgcolor="#ffffff">';
}

function BoxBottom()
{
	print '</td></tr></table></td></tr></table>';
}

?>
