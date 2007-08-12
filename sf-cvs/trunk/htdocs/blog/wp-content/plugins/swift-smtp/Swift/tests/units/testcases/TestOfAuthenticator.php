<?php

/**
 * Swift Mailer Unit Test for the Swift_Authenticator Interface.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */

Mock::Generate("Swift_Authenticator", "MockAuthenticator");
Mock::GeneratePartial("Swift_Connection_SMTP", "PartialSmtpConnection", array("read"));

/**
 * Swift Mailer Unit Test for the Swift_Authenticator Interface.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */
class TestOfAuthenticator extends UnitTestCase
{
  function setUp()
  {
    Swift_Errors::reset();
  }
  /**
   * Checks that the interface is actually invoked when needed.
   */
  function testAuthenticatorIsInvokedIfSMTPUsernameAndPasswordSet()
  {
    $auth =& new MockAuthenticator($this);
    $auth->setReturnValue("getAuthExtensionName", "FOO");
    $auth->expectOnce("isAuthenticated", array("foo", "bar", "*"));
    $smtp =& new PartialSmtpConnection($this);
    $smtp->setExtension("AUTH", array("FOO"));
    $smtp->setUsername("foo");
    $smtp->setPassword("bar");
    $smtp->attachAuthenticator($auth);
    @$smtp->postConnect(new Swift($smtp, "xx", SWIFT_NO_START));
  }
}
