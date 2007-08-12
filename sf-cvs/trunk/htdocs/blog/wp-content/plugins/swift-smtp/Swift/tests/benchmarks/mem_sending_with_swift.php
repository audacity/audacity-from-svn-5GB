<?php

/**
 * Benchmarking tests for Swift Mailer
 * @author Chris Corbyn <chris@w3style.co.uk>
 */

set_time_limit(500);
error_reporting(E_ALL);
ini_set("display_errors", "On");
require_once "../TestConfiguration.php";

xdebug_start_trace($GLOBALS["CONF"]->WRITABLE_PATH . "/bench-output");

//Number of emails to send
$n = isset($_GET["n"]) ? $_GET["n"] : 1;

require_once $GLOBALS["CONF"]->SWIFT_LIBRARY_PATH . "/Swift.php";

Swift_CacheFactory::setClassName("Swift_Cache_Disk");
Swift_Cache_Disk::setSavePath($GLOBALS["CONF"]->WRITABLE_PATH);

$conn = null;
switch ($GLOBALS["CONF"]->CONNECTION_TYPE)
{
  case "smtp":
    require_once $GLOBALS["CONF"]->SWIFT_LIBRARY_PATH . "/Swift/Connection/SMTP.php";
    $enc = null;
    $test_enc = $GLOBALS["CONF"]->SMTP_ENCRYPTION;
    if ($test_enc == "ssl") $enc = SWIFT_SMTP_ENC_SSL;
    elseif ($test_enc == "tls") $enc = SWIFT_SMTP_ENC_TLS;
    $conn =& new Swift_Connection_SMTP(
      $GLOBALS["CONF"]->SMTP_HOST, $GLOBALS["CONF"]->SMTP_PORT, $enc);
    if ($user = $GLOBALS["CONF"]->SMTP_USER) $conn->setUsername($user);
    if ($pass = $GLOBALS["CONF"]->SMTP_PASS) $conn->setPassword($pass);
    break;
  case "sendmail":
    require_once $GLOBALS["CONF"]->SWIFT_LIBRARY_PATH . "/Swift/Connection/Sendmail.php";
    $conn =& new Swift_Connection_Sendmail($GLOBALS["CONF"]->SENDMAIL_PATH);
    break;
  case "nativemail":
    require_once $GLOBALS["CONF"]->SWIFT_LIBRARY_PATH . "/Swift/Connection/NativeMail.php";
    $conn =& new Swift_Connection_NativeMail();
    break;
}

?>Run Test for number of recipients:
<ul>
  <li><a href="?n=1">1</a></li>
  <li><a href="?n=10">10</a></li>
  <li><a href="?n=30">30</a></li>
  <li><a href="?n=100">100</a></li>
</ul><?php

$swift =& new Swift($conn);
$message =& new Swift_Message("Test", "Test");
$from =& new Swift_Address($GLOBALS["CONF"]->FROM_ADDRESS, $GLOBALS["CONF"]->FROM_NAME);
$to =& new Swift_Address($GLOBALS["CONF"]->TO_ADDRESS, $GLOBALS["CONF"]->TO_NAME);
for ($i = 0; $i < $n; $i++)
{
  $swift->send($message, $to, $from);
}

xdebug_stop_trace();

echo "Check the output file [" . $GLOBALS["CONF"]->WRITABLE_PATH . "/bench-output.xt] for the trace if the peak memory value below is zero<br />";
echo xdebug_peak_memory_usage();
