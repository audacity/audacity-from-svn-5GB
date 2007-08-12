<?php

require dirname(__FILE__) . "/components/Runner.php";

class TestOfMultipart extends Runner
{
  var $to;
  var $from;
  
  function go()
  {
    Swift_Errors::expect($e, "Swift_Exception");
      if (!$e) $swift =& new Swift($this->getConnection());
      if (!$e) $message =& new Swift_Message("Smoke Test 2 - Multipart");
      if (!$e) $message->attach(new Swift_Message_Part("This message was sent in plain text"));
      if (!$e) $message->attach(new Swift_Message_Part("This message was sent in <strong>HTML</strong>", "text/html"));
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
    echo "Test of Sending Multipart E-Mail";
  }
  
  function paintTopInfo()
  {
    echo "An email containing a HTML parts, and an alternative plain text part will be sent from Swift, to the account given in the test configuration.  Simply open up the email and " .
    "check that the details given below are accurate: " .
    "<ul><li>The subject of the message is \"<em>Smoke Test 2 - Multipart</em>\"</li>" .
    "<li>The sender of the message is \"<em>" . htmlentities($this->from) . "</em>\"</li>" . 
    "<li>The recipient in the To: header is \"<em>" . htmlentities($this->to) . "</em>\"</li>" .
    "<li>The plain text message body is<br />\"<em>This message was sent in plain text</em>\"</li>" .
    "<li>The HTML message body is<br />\"<em>This message was sent in <strong>HTML</strong></em>\"</li>" .
    "<li>The E-mail DOES NOT appear to have any attachments</li>" .
    "<li>The Date: header relects the date on the server.</li></ul>";
  }
  
  function paintBottomInfo()
  {
    echo "<strong style=\"color: #aa1111;\">NOTE:</strong> You'll need to find the option in your e-mail client to switch between HTML and plain text viewing.";
  }
  
  function paintImageName()
  {
    echo "smoke2.png";
  }
}

$runner = new TestOfMultipart();
$runner->go();
