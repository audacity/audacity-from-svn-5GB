<?php

/**
 * Swift Mailer Unit Test Case for the Encoder component.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */

//We need to use smoke tests to verify the encoding "actually works" ;)

/**
 * Swift Mailer Unit Test Case for the Encoder component.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */
class TestOfEncoder extends UnitTestCase
{
  var $_encoder;
  
  function setUp()
  {
    Swift_Errors::reset();
    $this->_encoder =& Swift_Message_Encoder::instance();
  }
  /**
   * Base64 encoding is pretty simple and PHP does most of the work.
   * It is important that only 3 bytes are encoded at a time however to ensure the 4/3 decoding works.
   */
  function testBase64EncodingFileIsSameAsBase64EncodingString()
  {
    $str_encoded = $this->_encoder->base64Encode(file_get_contents("../files/manchester.jpeg"), 76);
    $file_encoded = $this->_encoder->base64EncodeFile(new Swift_File("../files/manchester.jpeg"), 76);
    $file_encoded = $file_encoded->readFull();
    $this->assertEqual($str_encoded, $file_encoded);
  }
  /**
   * Base64 encoded data should get rid of any and all 8 bit sequences.
   */
  function testBase64EncodeContainsOnlyRelevant7BitChars()
  {
    $data = file_get_contents("../files/manchester.jpeg");
    $this->assertNoPattern("~^[a-zA-Z0-9\\+/=]+\$~", $data);
    $encoded = $this->_encoder->rawBase64Encode($data);
    $this->assertPattern("~^[a-zA-Z0-9\\+/=]+\$~", $encoded);
  }
  /**
   * Base64 encoded lines cannot be longer than 76 chars (including CRLF)
   */
  function testBase64EncodedLinesAreNotLongerThanSpecified()
  {
    $data = file_get_contents("../files/manchester.jpeg");
    
    $encoded = $this->_encoder->base64Encode($data, 76);
    $lines = explode("\r\n", $encoded);
    foreach ($lines as $line)
    {
      $line .= "\r\n"; //lost in explode()
      $this->assertWithinMargin(0, strlen($line), 76);
    }
    
    $encoded = $this->_encoder->base64Encode($data, 1000);
    $lines = explode("\r\n", $encoded);
    foreach ($lines as $line)
    {
      $line .= "\r\n"; //lost in explode()
      $this->assertWithinMargin(0, strlen($line), 1000);
    }
  }
  /**
   * The first line may need to be a different length to the rest if encoded for headers.
   */
  function testFirstBase64EncodedLineCanHaveUniqueLength()
  {
    $data = file_get_contents("../files/manchester.jpeg");
    
    $encoded = $this->_encoder->base64Encode($data, 76, 10);
    $lines = explode("\r\n", $encoded);
    $this->assertWithinMargin(0, strlen($lines[0] . "\r\n"), 10);
    
    $encoded = $this->_encoder->base64Encode($data, 76, 13);
    $lines = explode("\r\n", $encoded);
    $this->assertWithinMargin(0, strlen($lines[0] . "\r\n"), 13);
    
    $encoded = $this->_encoder->base64Encode($data, 76, 27);
    $lines = explode("\r\n", $encoded);
    $this->assertWithinMargin(0, strlen($lines[0] . "\r\n"), 27);
  }
  /**
   * No longer used. QP encoded files must have the EOL encoded too.  Strings do not.
   * @deprecated
   */
  function testQPEncodingFileIsSameAsQPEncodingString()
  {
    //This test does not comply with RFC 2045 so has been removed
  }
  /**
   * QP encoded data should get rid of any 8 bit sequences.
   */
  function testQPEncodeContainsOnlyRelevant7BitChars()
  {
    $data = file_get_contents("../files/manchester.jpeg");
    $this->assertNoPattern("~^[^\\x21-\\x3C\\x3E-\\x7E\\x09\\x20\\x0D\\x0A=]+\$~", $data);
    $encoded = $this->_encoder->rawQPEncode($data);
    $this->assertPattern("~^[\\x21-\\x3C\\x3E-\\x7E\\x09\\x20\\x0D\\x0A=]+\$~", $encoded);
  }
  /**
   * QP encoding cannot have lines over 76 chars (incl CRLF)
   */
  function testQPEncodedLinesAreNotLongerThanSpecified()
  {
    $data = file_get_contents("../files/manchester.jpeg");
    
    $encoded = $this->_encoder->QPEncode($data, 76);
    $lines = explode("\r\n", $encoded);
    foreach ($lines as $line)
    {
      $line .= "\r\n"; //lost in explode()
      $this->assertWithinMargin(0, strlen($line), 76);
    }
    
    $encoded = $this->_encoder->QPEncode($data, 1000);
    $lines = explode("\r\n", $encoded);
    foreach ($lines as $line)
    {
      $line .= "\r\n"; //lost in explode()
      $this->assertWithinMargin(0, strlen($line), 1000);
    }
  }
  /**
   * If used in headers, the first line may need to be a special size.
   */
  function testFirstQPEncodedLineCanHaveUniqueLength()
  {
    $data = file_get_contents("../files/manchester.jpeg");
    
    $encoded = $this->_encoder->QPEncode($data, 76, 10);
    $lines = explode("\r\n", $encoded);
    $this->assertWithinMargin(0, strlen($lines[0] . "\r\n"), 10);
    
    $encoded = $this->_encoder->QPEncode($data, 76, 13);
    $lines = explode("\r\n", $encoded);
    $this->assertWithinMargin(0, strlen($lines[0] . "\r\n"), 13);
    
    $encoded = $this->_encoder->QPEncode($data, 76, 27);
    $lines = explode("\r\n", $encoded);
    $this->assertWithinMargin(0, strlen($lines[0] . "\r\n"), 27);
  }
  /**
   * Just a wordwrap() feature.
   */
  function test7BitEncodedLinesAreNotLongerThanSpecified()
  {
    $data = file_get_contents("../files/manchester.jpeg");
    
    $encoded = $this->_encoder->encode7Bit($data, 76);
    $lines = explode("\r\n", $encoded);
    foreach ($lines as $line)
    {
      $line .= "\r\n"; //lost in explode()
      $this->assertWithinMargin(0, strlen($line), 76);
    }
    
    $encoded = $this->_encoder->encode7Bit($data, 1000);
    $lines = explode("\r\n", $encoded);
    foreach ($lines as $line)
    {
      $line .= "\r\n"; //lost in explode()
      $this->assertWithinMargin(0, strlen($line), 1000);
    }
  }
  /**
   * Just a wordwrap() feature.
   */
  function test8BitEncodedLinesAreNotLongerThanSpecified()
  {
    $data = file_get_contents("../files/manchester.jpeg");
    
    $encoded = $this->_encoder->encode8Bit($data, 76);
    $lines = explode("\r\n", $encoded);
    foreach ($lines as $line)
    {
      $line .= "\r\n"; //lost in explode()
      $this->assertWithinMargin(0, strlen($line), 76);
    }
    
    $encoded = $this->_encoder->encode8Bit($data, 1000);
    $lines = explode("\r\n", $encoded);
    foreach ($lines as $line)
    {
      $line .= "\r\n"; //lost in explode()
      $this->assertWithinMargin(0, strlen($line), 1000);
    }
  }
  /**
   * Addresses should be able to be preserved in the string if in <> quotes.
   */
  function testQuoteChunkIsLossless()
  {
    $string = 'Some arbitrary string with an <add@re.ss> in it';
    $parts = $this->_encoder->quoteChunk($string);
    $this->assertEqual(array(0 => 'Some arbitrary string with an ', 'a1' => '<add@re.ss>', 2 => ' in it'), $parts);
    
    $string = "Joe Bloggs <some@address>,\r\n " . 
    "Fred Jones <fred>,\r\n " .
    "Zip Button <zip@button.com>";
    $parts = $this->_encoder->quoteChunk($string);
    $this->assertEqual(array(0 => "Joe Bloggs ", "a1" => "<some@address>", 2 => ",\r\n Fred Jones ", "a3" => "<fred>", 4 => ",\r\n Zip Button ", "a5" => "<zip@button.com>"), $parts);
    
    $string = "Some arbitrary string with an invalid <ad\nd@re.ss> in it";
    $parts = $this->_encoder->quoteChunk($string);
    $this->assertEqual(array(0 => $string), $parts);
  }
  /**
   * Detecting UTF8 automatically is useful so we want to be able to do this.
   */
  function testUTF8Detection()
  {
    $iso88591 = file_get_contents("../files/encodings/iso-8859-1.txt");
    $this->assertFalse($this->_encoder->isUTF8($iso88591));
    
    $utf8 = file_get_contents("../files/encodings/utf-8.txt");
    $this->assertTrue($this->_encoder->isUTF8($utf8));
  }
}
