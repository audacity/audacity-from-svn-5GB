<?php

require dirname(__FILE__) . "/components/Runner.php";

class TestOfHeaderEncoding extends Runner
{
  var $to;
  var $from;
  
  function go()
  {
    Swift_Errors::expect($e, "Swift_Exception");
      if (!$e) $swift =& new Swift($this->getConnection());
      if (!$e) $message =& new Swift_Message("Smoke Test 4 - Esto es un correo electrónico con algunos caracteres especiales en jefes");
      if (!$e) $message->setBody("This is a test message");
      if (!$e) $to =& new Swift_Address($GLOBALS["CONF"]->TO_ADDRESS, $GLOBALS["CONF"]->TO_NAME);
      if (!$e) $from =& new Swift_Address($GLOBALS["CONF"]->FROM_ADDRESS, $GLOBALS["CONF"]->FROM_NAME);
      if (!$e) $this->setSent($swift->send($message, $to, $from));
      if (!$e) $this->to = $to->build();
      if (!$e) $this->from = $from->build();
      if (!$e) $swift->disconnect();
    if ($e) {
      $this->failed = true;
      $this->setError($e->getMessage());
    }
    Swift_Errors::clear("Swift_Exception");
    $this->render();
  }
  
  function paintTestName()
  {
    echo "Test of Encoding in Headers";
  }
  
  function paintTopInfo()
  {
    echo "An email containing a UTF-8 string in the subject will be sent to the address given in the test configuration. " .
    "<ul><li>Check that the subject is \"<em>Smoke Test 4 - Esto es un correo electrónico con algunos caracteres especiales en jefes</em>\"</li></ul>";
  }
  
  function paintBottomInfo()
  {
    echo "<strong style=\"color: #aa1111;\">NOTE:</strong> Some web-based e-mail service like Hotmail may not display the message correctly due to their own character encoding compatability problems";
  }
  
  function paintImageName()
  {
    echo "smoke4.png";
  }
}

$runner =& new TestOfHeaderEncoding();
$runner->go();
