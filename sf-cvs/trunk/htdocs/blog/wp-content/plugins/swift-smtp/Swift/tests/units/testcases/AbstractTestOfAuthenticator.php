<?php

/**
 * Swift Mailer abstract test case for Authenticator tests.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */

if (!class_exists("PartialSmtpConnectionIO")) Mock::GeneratePartial("Swift_Connection_SMTP", "PartialSmtpConnectionIO", array("read", "write"));

/**
 * Swift Mailer abstract test case for Authenticator tests.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */
class AbstractTestOfAuthenticator extends UnitTestCase
{
  /**
   * Test that RSET is issued at the appropriate time.
   */
  function testRSETIsSentOnFailure() {}
  /**
   * Get the name of the authentication mechanism (e.g. LOGIN)
   * @return string
   */
  function getAuthMethod() {}
  /**
   * Get the authenticator instance.
   * @return Swift_Authenticator
   */
  function &getAuthObject() {}
  /**
   * Test that an exception is thrown is a bad response is sent (test uses a 500 response)
   */
  function testExceptionIsThrownIfBadResponseReceived()
  {
    $auth =& $this->getAuthObject();
    $smtp =& new PartialSmtpConnectionIO($this);
    $smtp->setReturnValueAt(0, "read", "500 Something");
    $smtp->setReturnValueAt(1, "read", "250 reset");
    
    $smtp->setExtension("AUTH", array($this->getAuthMethod()));
    $smtp->setUsername("foo");
    $smtp->setPassword("bar");
    $smtp->attachAuthenticator($auth);
    $smtp->postConnect(new Swift($smtp, "xx", SWIFT_NO_START));
    $this->assertError();
  }
}