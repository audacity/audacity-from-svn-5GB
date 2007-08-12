<?php

class TestOfMimePart extends UnitTestCase
{
  function setUp()
  {
    Swift_Errors::reset();
  }
  
  function testPartContainsNeededHeaders()
  {
    $part = new Swift_Message_Part("Just some random message", SWIFT_MIME_PLAIN, "7bit");
    $structure =& $part->build(); $structure = $structure->readFull();
    $this->assertPattern("~Content-Type: text/plain;\\s* charset=.*?\r\nContent-Transfer-Encoding: 7bit\r\n\r\nJust some random message~", $structure);
  }
  
  function testCharsetIsSetToUTF8IfRequiredAndNotOverridden()
  {
    $part = new Swift_Message_Part("cenvéla", "text/plain");
    $structure =& $part->build(); $structure = $structure->readFull();
    $this->assertPattern("~Content-Type: text/plain;\\s* charset=utf-8\r\nContent-Transfer-Encoding: 8bit\r\n\r\ncenvéla~", $structure);
  }
  
  //Ok, so this is one thing that *could* piss people off but you can force it (read the test properly!)
  function testEncodingIsForcedAs8BitIfUTF8IsUsedButOnlyIfNoCharsetIsGiven()
  {
    $part = new Swift_Message_Part("cenvéla", "text/plain", "7bit");
    $structure =& $part->build(); $structure = $structure->readFull();
    $this->assertPattern("~Content-Type: text/plain;\\s* charset=utf-8\r\nContent-Transfer-Encoding: 8bit\r\n\r\ncenvéla~", $structure);
    
    $part = new Swift_Message_Part("cenvéla", "text/plain", "7bit", "utf-8");
    $structure =& $part->build(); $structure = $structure->readFull();
    $this->assertPattern("~Content-Type: text/plain;\\s* charset=utf-8\r\nContent-Transfer-Encoding: 8bit\r\n\r\ncenvéla~", $structure);
    $part->setEncoding("7bit"); //Override!
    $structure =& $part->build(); $structure = $structure->readFull();
    $this->assertPattern("~Content-Type: text/plain;\\s* charset=utf-8\r\nContent-Transfer-Encoding: 7bit\r\n\r\ncenvéla~", $structure);
  }
  
  function testCharsetCanBeChangedAfterInstantiation()
  {
    $part = new Swift_Message_Part("Just some random message");
    $part->setEncoding("8bit");
    $part->setCharset("iso-8859-1");
    $structure =& $part->build(); $structure = $structure->readFull();
    $this->assertPattern("~Content-Type: text/plain;\\s* charset=iso-8859-1\r\nContent-Transfer-Encoding: 8bit\r\n\r\nJust some random message~", $structure);
    
    $part->setCharset("utf-8");
    $structure =& $part->build(); $structure = $structure->readFull();
    $this->assertPattern("~Content-Type: text/plain;\\s* charset=utf-8\r\nContent-Transfer-Encoding: 8bit\r\n\r\nJust some random message~", $structure);
  }
  
  function testEncodingCanBeSetAfterInstantiation()
  {
    $part = new Swift_Message_Part("Just some random message");
    $part->setEncoding("8bit");
    $part->setCharset("iso-8859-1");
    $structure =& $part->build(); $structure = $structure->readFull();
    $this->assertPattern("~Content-Type: text/plain;\\s* charset=iso-8859-1\r\nContent-Transfer-Encoding: 8bit\r\n\r\nJust some random message~", $structure);
    
    $part->setEncoding("7bit");
    $structure =& $part->build(); $structure = $structure->readFull();
    $this->assertPattern("~Content-Type: text/plain;\\s* charset=iso-8859-1\r\nContent-Transfer-Encoding: 7bit\r\n\r\nJust some random message~", $structure);
  }
  
  function testContentTypeCanBeSetAfterInstantiation()
  {
    $part = new Swift_Message_Part("Just some random message");
    $part->setEncoding("8bit");
    $part->setCharset("iso-8859-1");
    $structure =& $part->build(); $structure = $structure->readFull();
    $this->assertPattern("~Content-Type: text/plain;\\s* charset=iso-8859-1\r\nContent-Transfer-Encoding: 8bit\r\n\r\nJust some random message~", $structure);
    
    $part->setContentType(SWIFT_MIME_HTML);
    $structure =& $part->build(); $structure = $structure->readFull();
    $this->assertPattern("~Content-Type: text/html;\\s* charset=iso-8859-1\r\nContent-Transfer-Encoding: 8bit\r\n\r\nJust some random message~", $structure);
  }
  
  function testFormatCanBeSetToFlowed()
  {
    $part = new Swift_Message_Part("Just some random message");
    $part->setEncoding("8bit");
    $part->setCharset("iso-8859-1");
    $part->setFlowed(true);
    $structure =& $part->build(); $structure = $structure->readFull();
    $this->assertPattern("~Content-Type: text/plain;\\s* charset=iso-8859-1;\\s* format=flowed\r\nContent-Transfer-Encoding: 8bit\r\n\r\nJust some random message~", $structure);
    
    $part->setFlowed(false);
    $structure =& $part->build(); $structure = $structure->readFull();
    $this->assertPattern("~Content-Type: text/plain;\\s* charset=iso-8859-1\r\nContent-Transfer-Encoding: 8bit\r\n\r\nJust some random message~", $structure);
  }
}
