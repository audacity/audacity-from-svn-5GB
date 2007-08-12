<?php

/*
 * TODO
 *  - Memory optimizations
 */

class TestOfMime extends UnitTestCase
{
  function setUp()
  {
    Swift_Errors::reset();
  }
  
  function makeString($len)
  {
    $ret = "";
    $chr = 32;
    for ($i = 0; $i < $len; $i++) //cycle through the ascii sequence
    {
      $ret .= chr($chr++);
      if ($chr >= 126) $chr = 32;
    }
    return $ret;
  }
  
  function testHeadersAreSeparatedFromBodyByCRLF()
  {
    $mime = new MimeExtension();
    $mime->headers->set("Subject", "Test subject");
    $mime->headers->set("Date", date("r"));
    $mime->setData("Just a test body");
    $structure =& $mime->build(); $structure = $structure->readFull();
    $this->assertPattern("/^.*?\r\n\r\n.*$/s", $structure);
  }
  
  function testBodyIsEncodedAccordingToSpecification()
  {
    $mime = new MimeExtension();
    $mime->headers->set("To", "joe@bloggs.com");
    $mime->headers->set("Subject", "Test");
    $mime->setData($this->makeString(1000));
    $mime->setEncoding("QP");
    $structure =& $mime->build(); $structure = $structure->readFull();
    $this->assertPattern("~^.*?^Content-Transfer-Encoding: quoted-printable\b.*?\r\n\r\n(^[\\x21-\\x3C\\x3E-\\x7E\\x09\\x20=]{0,76}\r?\$)+\$~ism", $structure);
    
    $mime = new MimeExtension();
    $mime->headers->set("To", "joe@bloggs.com");
    $mime->headers->set("Subject", "Test");
    $mime->setData($this->makeString(1000));
    $mime->setEncoding("Base64");
    $structure =& $mime->build(); $structure = $structure->readFull();
    $this->assertPattern("~^.*?^Content-Transfer-Encoding: base64\b.*?\r\n\r\n(^[a-z0-9/\\+=]{0,76}\r?\$)+\$~ism", $structure);
  }
  
  function testLinesInBodyCannotExceedSpecifiedLength()
  {
    $mime = new MimeExtension();
    $mime->setLineWrap(76);
    $mime->setData($this->makeString(2000));
    
    $structure =& $mime->build(); $structure = $structure->readFull();
    $body = substr($structure, strpos($structure, "\r\n\r\n")+4);
    
    $lines = explode("\r\n", $body);
    foreach ($lines as $line)
    {
      $line .= "\r\n"; //lost in explode()
      $this->assertWithinMargin(0, strlen($line), 76);
    }
    
    $mime = new MimeExtension();
    $mime->setLineWrap(1000);
    $mime->setData($this->makeString(1001));
    
    $structure =& $mime->build(); $structure = $structure->readFull();
    $body = substr($structure, strpos($structure, "\r\n\r\n")+4);
    
    $lines = explode("\r\n", $body);
    foreach ($lines as $line)
    {
      $line .= "\r\n"; //lost in explode()
      $this->assertWithinMargin(0, strlen($line), 1000);
    }
    
    $mime = new MimeExtension();
    $mime->setLineWrap(999);
    $mime->setData($this->makeString(5003));
    
    $structure =& $mime->build(); $structure = $structure->readFull();
    $body = substr($structure, strpos($structure, "\r\n\r\n")+4);
    
    $lines = explode("\r\n", $body);
    foreach ($lines as $line)
    {
      $line .= "\r\n"; //lost in explode()
      $this->assertWithinMargin(0, strlen($line), 999);
    }
    
    $mime = new MimeExtension();
    $mime->setLineWrap(999);
    $mime->setData($this->makeString(5003));
    $mime->setEncoding("QP");
    $structure =& $mime->build(); $structure = $structure->readFull();
    $body = substr($structure, strpos($structure, "\r\n\r\n")+4);
    
    $lines = explode("\r\n", $body);
    foreach ($lines as $line)
    {
      $line .= "\r\n"; //lost in explode()
      $this->assertWithinMargin(0, strlen($line), 999);
    }
    
    $mime = new MimeExtension();
    $mime->setLineWrap(76);
    $mime->setData($this->makeString(4000));
    $mime->setEncoding("base64");
    $structure =& $mime->build(); $structure = $structure->readFull();
    $body = substr($structure, strpos($structure, "\r\n\r\n")+4);
    
    $lines = explode("\r\n", $body);
    foreach ($lines as $line)
    {
      $line .= "\r\n"; //lost in explode()
      $this->assertWithinMargin(0, strlen($line), 76);
    }
  }
  
  function testDataReadFromFileStreamIsSameAsDataReadFromString()
  {
    //Already tested in TestOfEncoder
  }
  
  function testDoucmentsCanBeNested()
  {
    $outer_part = new MimeExtension();
    $outer_part->setData("Default text");
    $outer_part->setContentType("multipart/alternative");
    $outer_part->setEncoding("8bit");
    
    $inner_part1 = new MimeExtension();
    $inner_part1->setData("Inner part 1");
    $inner_part1->setContentType("text/plain");
    $inner_part1->setEncoding("8bit");
    
    $outer_part->addChild($inner_part1);
    
    $inner_part2 = new MimeExtension();
    $inner_part2->setData("Inner part 2");
    $inner_part2->setContentType("text/plain");
    $inner_part2->setEncoding("8bit");
    
    $outer_part->addChild($inner_part2);
 		
    $structure = $outer_part->build();
    $structure = $structure->readFull();
    
    $this->assertPattern(
      "~^Content-Type: multipart/alternative;\\s* boundary=(\")?(\\S+)\\1\r\n" .
      "Content-Transfer-Encoding: 8bit\r\n\r\n" .
      "Default Text\r\n" .
      "--\\2\r\n" .
      "Content-Type: text/plain\r\n" .
      "Content-Transfer-Encoding: 8bit\r\n\r\n" .
      "Inner part 1\r\n" .
      "--\\2\r\n" .
      "Content-Type: text/plain\r\n" .
      "Content-Transfer-Encoding: 8bit\r\n\r\n" .
      "Inner part 2\r\n" .
      "--\\2--\\s*\$~is", $structure);
  }
  
  function testBuildingPartMultipleTimesYieldsSameResultDueToCachingIfUnchanged()
  {
    $outer_part = new MimeExtension();
    $outer_part->setData("Default text");
    $outer_part->setContentType("multipart/mixed");
    $outer_part->setEncoding("8bit");
    
    $inner_part1 = new MimeExtension();
    $inner_part1->setData("Inner part 1");
    $inner_part1->setContentType("text/plain");
    $inner_part1->setEncoding("8bit");
    
    $outer_part->addChild($inner_part1);
    
    $inner_part2 = new MimeExtension();
    $inner_part2->setData(new Swift_File("../files/manchester.jpeg"));
    $inner_part2->setContentType("image/jpeg");
    $inner_part2->setEncoding("base64");
    
    $outer_part->addChild($inner_part2);
    
    $structure1 = $outer_part->build();
    $structure1 = $structure1->readFull();
    
    $structure2 = $outer_part->build();
    $structure2 = $structure2->readFull();
    
    $this->assertEqual($structure1, $structure2);
    
    $structure3 = $outer_part->build();
    $structure3 = $structure3->readFull();
    
    $this->assertEqual($structure2, $structure3);
  }
  
  function testChildrenCanBeRemoved()
  {
    $outer_part = new MimeExtension();
    $outer_part->setData("Default text");
    $outer_part->setContentType("multipart/alternative");
    $outer_part->setEncoding("8bit");
    
    $inner_part1 = new MimeExtension();
    $inner_part1->setData("Inner part 1");
    $inner_part1->setContentType("text/plain");
    $inner_part1->setEncoding("8bit");
    
    $id1 = $outer_part->addChild($inner_part1);
    
    $inner_part2 = new MimeExtension();
    $inner_part2->setData("Inner part 2");
    $inner_part2->setContentType("text/plain");
    $inner_part2->setEncoding("8bit");
    
    $id2 = $outer_part->addChild($inner_part2);
    
    $outer_part->removeChild($id1);
    
    $structure = $outer_part->build();
    $structure = $structure->readFull();
    
    $this->assertPattern(
      "~^Content-Type: multipart/alternative;\\s* boundary=(\")?(\\S+)\\1\r\n" .
      "Content-Transfer-Encoding: 8bit\r\n\r\n" .
      "Default Text\r\n" .
      "--\\2\r\n" .
      "Content-Type: text/plain\r\n" .
      "Content-Transfer-Encoding: 8bit\r\n\r\n" .
      "Inner part 2\r\n" .
      "--\\2--\\s*\$~is", $structure);
  }
  
  function testLineEndingsCanBeCorrectedAccordingToSpecification()
  {
    $mime = new MimeExtension();
    $mime->setData("foo \n zip \r\n button \r test \n");
    $mime->setLE("\n");
    $body = $mime->buildData();
    $body = $body->readFull();
    $this->assertEqual("foo \n zip \n button \n test \n", $body);
    
    $mime->setLE("\r\n");
    $body = $mime->buildData();
    $body = $body->readFull();
    $this->assertEqual("foo \r\n zip \r\n button \r\n test \r\n", $body);
    
    $mime->setLE("\r");
    $body = $mime->buildData();
    $body = $body->readFull();
    $this->assertEqual("foo \r zip \r button \r test \r", $body);
  }
  
  function testDotsAreEscapedOnStartOfLines()
  {
    $mime = new MimeExtension();
    $mime->setData("foo \n. zip \r\n. button \r. test \n");
    $mime->setLE("\n");
    $body = $mime->buildData();
    $body = $body->readFull();
    $this->assertEqual("foo \n.. zip \n.. button \n.. test \n", $body);
    
    $mime = new MimeExtension();
    $mime->setData("foo \n. zip \r\n. button \r. test \n");
    $mime->setLE("\r\n");
    $body = $mime->buildData();
    $body = $body->readFull();
    $this->assertEqual("foo \r\n.. zip \r\n.. button \r\n.. test \r\n", $body);
    
    $mime = new MimeExtension();
    $mime->setData("foo \n. zip \r\n. button \r. test \n");
    $mime->setLE("\r");
    $body = $mime->buildData();
    $body = $body->readFull();
    $this->assertEqual("foo \r.. zip \r.. button \r.. test \r", $body);
  }
  
  function testEncodingCanBeAppliedRecursively()
  {
    $parent = new MimeExtension();
    $child = new MimeExtension();
    $this->assertFalse($parent->getEncoding());
    $this->assertFalse($child->getEncoding());
    $parent->addChild($child);
    $parent->setEncoding("QP", true);
    $this->assertEqual("quoted-printable", $parent->getEncoding());
    $this->assertEqual("quoted-printable", $child->getEncoding());
  }
}
  