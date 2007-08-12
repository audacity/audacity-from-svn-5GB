<?php

/**
 * Swift Mailer Unit Test Case for Swift_File
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */


/**
 * Swift Mailer Unit Test Case for Swift_File
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */
class TestOfFile extends UnitTestCase
{
  /**
   * The current working dir, so we can change directories.
   * @var string
   */
  var $cwd;
  /**
   * Store the working dir.
   */
  function setUp()
  {
    Swift_Errors::reset();
    $this->cwd = getcwd();
  }
  /**
   * Restore the working dir.
   */
  function tearDown()
  {
    chdir($this->cwd);
  }
  /**
   * We want an error triggered if the file doesn't exist.
   */
  function testExceptionIsThrowIfFileNotFound()
  {
    $this->assertFalse(file_exists("/no/such/file.php"));
    $file = new Swift_File("/no/such/file.php");
    $this->assertError();
  }
  /**
   * Streaming data from the file shoudl yield the same results as file_get_contents().
   */
  function testBytesReadFromFileAreCorrect()
  {
    $file = new Swift_File("../files/manchester.jpeg");
    
    $data = "";
    while (false !== $byte = $file->getByte()) $data .= $byte;
    
    $this->assertIdentical($data, file_get_contents("../files/manchester.jpeg"));
  }
  /**
   * ReadFull() should just stream all the data.
   */
  function testReadFullIsSameAsActualFileContents()
  {
    $file = new Swift_File("../files/manchester.jpeg");
    $this->assertIdentical($file->readFull(), file_get_contents("../files/manchester.jpeg"));
  }
  /**
   * The filename should be returned, without the rest of the path.
   */
  function testFileNameIsReturned()
  {
    $file = new Swift_File("../files/manchester.jpeg");
    $this->assertEqual("manchester.jpeg", $file->getFileName());
    
    $file->setPath(__FILE__); //TestOfFile.php
    $this->assertEqual(basename(__FILE__), $file->getFileName());
    
    chdir("../files");
    $file = new Swift_File("manchester.jpeg");
    $this->assertEqual("manchester.jpeg", $file->getFileName());
  }
  
  //Not easy to test exception throw if file cannot be read so I just trust it ;)
}
