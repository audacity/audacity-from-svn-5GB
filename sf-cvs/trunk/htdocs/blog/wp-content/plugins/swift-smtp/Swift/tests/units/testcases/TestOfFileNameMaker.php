<?php

/**
 * Swift Mailer Unit Test Case for the Component which generates filenames.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */


/**
 * Swift Mailer Unit Test Case for the Component which generates filenames.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */
class TestOfFileNameMaker extends UnitTestCase
{
  var $_maker;
  
  function setUp()
  {
    Swift_Errors::reset();
    $this->_maker =& Swift_FileNameMaker::instance();
  }
  /**
   * It should be possible to specify our own prefix on the filename.
   */
  function testPrefixIsUsed()
  {
    $name = $this->_maker->generate("prefix");
    $this->assertPattern("/^prefix\\d+/", $name);
    $name = $this->_maker->generate("filename");
    $this->assertPattern("/^filename\\d+/", $name);
    $name = $this->_maker->generate("xxx.yyy.zzz.");
    $this->assertPattern("/^xxx\\.yyy\\.zzz\\.\\d+/", $name);
  }
  /**
   * The number of the file should go up by one each time.
   */
  function testSequenceNumberIsAlwaysIncremented()
  {
    $name = $this->_maker->generate();
    $id1 = (int) substr($name, -1);
    $name = $this->_maker->generate();
    $id2 = (int) substr($name, -1);
    $this->assertTrue($id2 > $id1);
    $name = $this->_maker->generate();
    $id3 = (int) substr($name, -1);
    $this->assertTrue($id3 > $id2);
  }
}
