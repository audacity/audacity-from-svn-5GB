<?php

/**
 * Swift Mailer Unit Test Case of LOGIN authentication.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */


/**
 * Swift Mailer Unit Test Case of LOGIN authentication.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */
class TestOfLOGINAuthenticator extends AbstractTestOfAuthenticator
{
  function setUp()
  {
    Swift_Errors::reset();
  }
  /**
   * Returns the name of the authentication method (i.e. LOGIN)
   * @return string
   */
  function getAuthMethod()
  {
    return "LOGIN";
  }
  /**
   * Returns an instance of the authenticator.
   * @return Swift_Authenticator_LOGIN
   */
  function &getAuthObject()
  {
    $auth =& new Swift_Authenticator_LOGIN();
    return $auth;
  }
  /**
   * Test that base64 is used.
   */
  function testAuthenticatorSendsCorrectlyFormattedBase64EncodedRequests()
  {
    $auth =& $this->getAuthObject();
    $smtp =& new PartialSmtpConnectionIO($this);
    $smtp->setReturnValueAt(0, "read", "334 " . base64_encode("username:"));
    $smtp->setReturnValueAt(1, "read", "334 " . base64_encode("password:"));
    $smtp->setReturnValueAt(2, "read", "235 Authenticated");
    $smtp->expectAt(0, "write", array("AUTH LOGIN", "*"));
    $smtp->expectAt(1, "write", array(base64_encode("foo"), "*"));
    $smtp->expectAt(2, "write", array(base64_encode("bar"), "*"));
    
    $smtp->setExtension("AUTH", array("LOGIN"));
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
    $smtp->setReturnValueAt(0, "read", "334 " . base64_encode("username:"));
    $smtp->setReturnValueAt(1, "read", "400 No");
    $smtp->setReturnValueAt(2, "read", "250 Ok");
    $smtp->expectAt(0, "write", array("AUTH LOGIN", "*"));
    $smtp->expectAt(1, "write", array(base64_encode("foo"), "*"));
    $smtp->expectAt(2, "write", array("RSET", "*"));
    $smtp->setExtension("AUTH", array("LOGIN"));
    $smtp->setUsername("foo");
    $smtp->setPassword("bar");
    $smtp->attachAuthenticator($auth);
    $smtp->postConnect(new Swift($smtp, "xx", SWIFT_NO_START));
    $this->assertError();
  }
}
