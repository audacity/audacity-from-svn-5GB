<?php

error_reporting(E_ALL);
ini_set("display_errors", "On");

require dirname(__FILE__) . "/../../TestConfiguration.php";
require $GLOBALS["CONF"]->SWIFT_LIBRARY_PATH . "/Swift.php";
require $GLOBALS["CONF"]->SWIFT_LIBRARY_PATH . "/Swift/Connection/SMTP.php";
require $GLOBALS["CONF"]->SWIFT_LIBRARY_PATH . "/Swift/Connection/Sendmail.php";
require $GLOBALS["CONF"]->SWIFT_LIBRARY_PATH . "/Swift/Connection/NativeMail.php";

class Runner
{
  var $failed = false;
  var $error = "";
  var $sent;
  
  function Runner()
  {
    @set_error_handler(array($this, "handleError"), E_USER_ERROR);
  }
  
  function handleError($errno, $errstr, $errfile, $errline)
  {
    $this->failed = true;
    $this->setError($errstr);
  }
  
  function &getConnection()
  {
    switch ($GLOBALS["CONF"]->CONNECTION_TYPE)
    {
      case "smtp":
        $enc = null;
        $test_enc = $GLOBALS["CONF"]->SMTP_ENCRYPTION;
        if ($test_enc == "ssl") $enc = SWIFT_SMTP_ENC_SSL;
        elseif ($test_enc == "tls") $enc = SWIFT_SMTP_ENC_TLS;
        $conn =& new Swift_Connection_SMTP(
          $GLOBALS["CONF"]->SMTP_HOST, $GLOBALS["CONF"]->SMTP_PORT, $enc);
        if ($user = $GLOBALS["CONF"]->SMTP_USER) $conn->setUsername($user);
        if ($pass = $GLOBALS["CONF"]->SMTP_PASS) $conn->setPassword($pass);
        return $conn;
      case "sendmail":
        $conn =& new Swift_Connection_Sendmail($GLOBALS["CONF"]->SENDMAIL_PATH);
        return $conn;
      case "nativemail":
        $conn =& new Swift_Connection_NativeMail();
        return $conn;
    }
  }
  
  function setSent($sent)
  {
    $this->sent = $sent;
    if (!$sent)
    {
      $this->failed = true;
      $this->error = "Message did not send!";
    }
  }
  
  function go() {}
  
  function paintTestName() {}
  
  function paintTopInfo() {}
  
  function paintImageName() {}
  
  function isError()
  {
    return $this->failed;
  }
  
  function setError($errstr)
  {
    $this->error = $errstr;
  }
  
  function paintError()
  {
    echo $this->error;
  }
  
  function paintResult()
  {
    echo "Message sent successfully";
  }
  
  function paintBottomInfo()
  {
    //
  }
  
  function render()
  {
    require dirname(__FILE__) . "/template.php";
  }
}
