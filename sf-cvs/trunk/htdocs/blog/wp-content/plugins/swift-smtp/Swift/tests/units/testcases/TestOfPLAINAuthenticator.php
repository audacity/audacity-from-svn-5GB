<?php

/**
 * Swift Mailer Unit Test Case of PLAIN authentication.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */


/**
 * Swift Mailer Unit Test Case of PLAIN authentication.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */
class TestOfPLAINAuthenticator extends AbstractTestOfAuthenticator
{
  function setUp()
  {
    Swift_Errors::reset();
  }
  /**
   * Returns the name of the authentication method (i.e. PLAIN)
   * @return string
   */
  function getAuthMethod()
  {
    return "PLAIN";
  }
  /**
   * Returns an instance of the authenticator.
   * @return Swift_Authenticator_PLAIN
   */
  function &getAuthObject()
  {
    $auth =& new Swift_Authenticator_PLAIN();
    return $auth;
  }
  /**
   * Test that null bytes are used as separators.
   */
  function testAuthenticatorSendsCorrectlyFormattedNullDelimitedRequests()
  {
    $auth =& $this->getAuthObject();
    $smtp =& new PartialSmtpConnectionIO($this);
    $smtp->setReturnValueAt(0, "read", "235 Authenticated");
    $smtp->expectAt(0, "write", array("AUTH PLAIN " . base64_encode("foo\0foo\0bar"), "*"));
    
    $smtp->setExtension("AUTH", array("PLAIN"));
    $smtp->setUsername("foo");
    $smtp->setPassword("bar");
    $smtp->attachAuthenticator($auth);
    $smtp->postConnect(new Swift($smtp, "xx", SWIFT_NO_START));
  }
  /**
   * Test that RSET is issued after a bad response.
   */
  function testRSETIsSentOnFailure()
  {
    $auth =& $this->getAuthObject();
    $smtp =& new PartialSmtpConnectionIO($this);
    $smtp->setReturnValueAt(0, "read", "500 Something");
    $smtp->setReturnValueAt(1, "read", "250 reset");
    $smtp->expectAt(0, "write", array("AUTH PLAIN " . base64_encode("foo\0foo\0bar"), "*"));
    $smtp->expectAt(1, "write", array("RSET", "*"));
    
    $smtp->setExtension("AUTH", array("PLAIN"));
    $smtp->setUsername("foo");
    $smtp->setPassword("bar");
    $smtp->attachAuthenticator($auth);
    $smtp->postConnect(new Swift($smtp, "xx", SWIFT_NO_START));
    $this->assertError();
  }
}
