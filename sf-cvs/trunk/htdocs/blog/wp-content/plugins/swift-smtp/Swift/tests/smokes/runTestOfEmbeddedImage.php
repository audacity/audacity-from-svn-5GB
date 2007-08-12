<?php

require dirname(__FILE__) . "/components/Runner.php";

class TestOfEmbeddedImage extends Runner
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
      if (!$e) $message =& new Swift_Message("Smoke Test 5 - Embedded Image");
      if (!$e) $part =& new Swift_Message_Part(
        "Here is an embedded image: <br /><img src=\"" .
          $message->attach(new Swift_Message_Image(new Swift_File(dirname(__FILE__) . "/../files/manchester.jpeg"))) .
          "\" alt=\"image\" /><br />And here is the rest of the message.",
        "text/html"
      );
      if (!$e) $message->attach($part);
      if (!$e) $message->attach(new Swift_Message_Part("You are viewing this message in plain text.  Switch to HTML mode to see the image.", "text/plain"));
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
    echo "Test of Sending Embedded Image";
  }
  
  function paintTopInfo()
  {
    echo "An email containing an embedded JPEG image will be sent from Swift, to the account given in the test configuration.  Open up the email and " .
    "check that the details given below are accurate: " .
    "<ul><li>The message body is<br />\"<em>Here is an embedded image: <br /><img src=\"../files/manchester.jpeg\" alt=\"image\" /><br />And here is the rest of the message.</em>\"</li>" .
    "<li>The image displays inline with the content</li></ul>";
  }
  
  function paintBottomInfo()
  {
    echo "<strong style=\"color: #aa1111;\">NOTE:</strong> Make sure you have your e-mail client in HTML mode.  Some web-based e-mail providers do not work with embedded images at all.";
  }
  
  function paintImageName()
  {
    echo "smoke5.png";
  }
}

$runner =& new TestOfEmbeddedImage();
$runner->go();
