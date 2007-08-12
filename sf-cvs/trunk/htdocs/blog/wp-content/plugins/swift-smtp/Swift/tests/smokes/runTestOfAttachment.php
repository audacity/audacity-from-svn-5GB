<?php

require dirname(__FILE__) . "/components/Runner.php";

class TestOfAttachment extends Runner
{
  var $to;
  var $from;
  
  function go()
  {
    Swift_ClassLoader::load("Swift_Cache_Disk");
    Swift_Cache_Disk::setSavePath($GLOBALS["CONF"]->WRITABLE_PATH);
    Swift_CacheFactory::setClassName("Swift_Cache_Disk");
    Swift_Errors::expect($e, "Swift_Exception");
      if (!$e) $swift =& new Swift($this->getConnection());
      if (!$e) $message =& new Swift_Message("Smoke Test 3 - Attachment");
      if (!$e) $message->attach(new Swift_Message_Part("This message contains an attachment"));
      if (!$e) $message->attach(new Swift_Message_Part("This message contains an <em>attachment</em>", "text/html"));
      if (!$e) $message->attach(new Swift_Message_Attachment(new Swift_File(dirname(__FILE__) . "/../files/cv.pdf"), "Authors_CV.pdf", "application/pdf"));
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
    echo "Test of Sending Attachment";
  }
  
  function paintTopInfo()
  {
    echo "An email containing a PDF attachment will be sent from Swift, to the account given in the test configuration.  Open up the email & its attachment and " .
    "check that the details given below are accurate: " .
    "<ul><li>The message body is<br />\"<em>This message contains an an attachment</em>\"</li>" .
    "<li>There is an attachment included named <em>Authors_CV.pdf</em></li>" .
    "<li>The attachment opens successfully in a PDF reader such as Adobe Acrobat Reader</li></ul>";
  }
  
  function paintImageName()
  {
    echo "smoke3.png";
  }
}

$runner =& new TestOfAttachment();
$runner->go();
