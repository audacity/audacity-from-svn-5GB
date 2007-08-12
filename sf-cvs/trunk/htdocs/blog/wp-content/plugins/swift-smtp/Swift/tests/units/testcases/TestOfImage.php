<?php

class TestOfImage extends UnitTestCase
{
  function setUp()
  {
    Swift_Errors::reset();
  }
  
  function testImageTypeIsDetected()
  {
    $image = new Swift_Message_Image(new Swift_File("../files/manchester.jpeg"));
    $this->assertEqual("image/jpeg", $image->getContentType());
    
    $image = new Swift_Message_Image(new Swift_File("../files/gecko.png"));
    $this->assertEqual("image/png", $image->getContentType());
    
    $image = new Swift_Message_Image(new Swift_File("../files/durham.gif"));
    $this->assertEqual("image/gif", $image->getContentType());
  }
  
  function testExceptionIsThrownIfWrongFileTypeGiven()
  {
    $image = new Swift_Message_Image(new Swift_File("../files/cv.pdf"));
    $this->assertError();
  }
  
  function testFilenameSetsInConstructor()
  {
    $image = new Swift_Message_Image(new Swift_File("../files/manchester.jpeg"));
    $this->assertEqual("manchester.jpeg", $image->getFileName());
    
    $image = new Swift_Message_Image(new Swift_File("../files/manchester.jpeg"), "joe.gif");
    $this->assertEqual("joe.gif", $image->getFileName());
  }
}
