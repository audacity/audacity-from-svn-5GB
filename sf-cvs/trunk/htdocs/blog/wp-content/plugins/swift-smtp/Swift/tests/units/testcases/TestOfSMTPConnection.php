<?php

Mock::Generate("Swift_Authenticator", "MockAuthenticator");

class TestOfSMTPConnection extends UnitTestCase
{
  function setUp()
  {
    Swift_Errors::reset();
  }
  
  function testPortDefaultsTo25ForUnencryptedConnections()
  {
    $conn = new Swift_Connection_SMTP();
    $this->assertEqual(25, $conn->getPort());
  }
  
  function testPortDefaultsTo465ForSSLConnections()
  {
    $conn = new Swift_Connection_SMTP("localhost", null, SWIFT_SMTP_ENC_SSL);
    $this->assertEqual(465, $conn->getPort());
  }
  
  function testPortDefaultsTo465ForTLSConnections()
  {
    $conn = new Swift_Connection_SMTP("localhost", null, SWIFT_SMTP_ENC_TLS);
    $this->assertEqual(465, $conn->getPort());
  }
  
  function testAuthenticationIsOnlyInvokedIfUsernameAndPasswordSet()
  {
    $auth =& new MockAuthenticator($this);
    $auth->setReturnValue("isAuthenticated", true);
    $auth->setReturnValue("getAuthExtensionName", "foo");
    $auth->expectOnce("isAuthenticated");
    
    $conn =& new Swift_Connection_SMTP();
    $conn->setExtension("AUTH", array("foo"));
    $conn->attachAuthenticator($auth);
    $conn->setUsername("xxx");
    $conn->setPassword("yyyy");
    $conn->postConnect(new Swift($conn, "xxx", SWIFT_NO_START));
    
    $auth =& new MockAuthenticator();
    $auth->setReturnValue("isAuthenticated", true);
    $auth->setReturnValue("getAuthExtensionName", "foo");
    $auth->expectNever("isAuthenticated");
    
    $conn =& new Swift_Connection_SMTP();
    $conn->setExtension("AUTH", array("foo"));
    $conn->attachAuthenticator($auth);
    //No username/password set
    $conn->postConnect(new Swift($conn, "xxx", SWIFT_NO_START));
  }
  
  function testAuthExtensionsWithAsteriskAsNameAreRunAlways()
  {
    $auth =& new MockAuthenticator($this);
    $auth->setReturnValue("isAuthenticated", true);
    $auth->setReturnValue("getAuthExtensionName", "*foo");
    $auth->expectOnce("isAuthenticated");
    
    $conn =& new Swift_Connection_SMTP();
    $conn->setExtension("AUTH", array("not-an-asterisk"));
    $conn->attachAuthenticator($auth);
    $conn->setUsername("xxx");
    $conn->setPassword("yyyy");
    $conn->postConnect(new Swift($conn, "xxx", SWIFT_NO_START));
  }
}
