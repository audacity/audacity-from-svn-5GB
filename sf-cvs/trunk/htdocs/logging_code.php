// Put this code in main.inc.php to log everything...

// logging
/* */$d = date("Y-m-d G:i:s");
/* */$fp = fopen("stats/browsers", "a");
fwrite($fp, "$d\t$HTTP_USER_AGENT\n");
fclose($fp);
/* */$r = $HTTP_REFERER;
if (strlen($r)>0 && strncmp($r, "http://audacity.sourceforge.net", 31)) {
  /* */$fp = fopen("stats/refer", "a");
  fwrite($fp, "$d\t$r\n");
  fclose($fp);
}

