<?php

/**
 * Swift Mailer Unit Test Case for EasySwift Wrapper.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */

Mock::Generate("Swift_Message", "MockMessage");
if (!class_exists("MockSendListener")) Mock::GeneratePartial("Swift_Events_Listener", "MockSendListener", array("sendPerformed"));
Mock::Generate("Swift_Connection_SMTP", "MockSMTPConnection");
Mock::GeneratePartial("Swift_Connection_SMTP", "MockSMTPConnectionAuth", array("read", "write", "isAlive", "start", "stop"));
Mock::Generate("Swift_Message_Headers", "MockHeaders");

/**
 * Swift Mailer Unit Test Case for EasySwift Wrapper.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */
class TestOfEasySwift extends AbstractTestWithSend
{
  function setUp()
  {
    Swift_Errors::reset();
  }
  /**
   * The old API would never spit out errors, so we should be catching them in EasySwift too.
   */
  function testNoExceptionsAreThrownBut_HasFailed_IsTrueOnBadConnection()
  {
    $mockConnection =& new FullMockConnection($this);
    $mockConnection->setReturnValue("read", "500 Bad");
    $swift =& new EasySwift($mockConnection);
    $this->assertTrue($swift->hasFailed());
    $this->assertFalse($swift->isConnected());
  }
  /**
   * Because the old API doesn't show errors, isConnected() needs to give useful info.
   */
  function testIsConnectedReturnsTrueOnSuccess()
  {
    $mockConnection =& $this->getWorkingMockConnection(1);
    
    $swift =& new EasySwift($mockConnection);
    $this->assertFalse($swift->hasFailed());
    $this->assertTrue($swift->isConnected());
  }
  /**
   * The old API, by default sent emails as a batch.
   */
  function testSendCallsBatchSendWithRecipientListInSwift()
  {
    $mockConnection =& $this->getWorkingMockConnection(3);
    $mockConnection->expectMinimumCallCount("write", 13);
    $mockConnection->expectAt(0, "write", array("EHLO xxx", "*"));
    $mockConnection->expectAt(1, "write", array("MAIL FROM: <me@myplace.com>", "*"));
    $mockConnection->expectAt(2, "write", array("RCPT TO: <foo@bar.com>", "*"));
    $mockConnection->expectAt(3, "write", array("DATA", "*"));
    $mockConnection->expectAt(4, "write", array("*", "*"));
    
    $swift =& new EasySwift($mockConnection, "xxx");
    $addresses = array("Foo Bar <foo@bar.com>", '"Zip Button" <zip@button.tld>', "mail@cheese.domain");
    $this->assertEqual(3, $swift->send($addresses, "My name <me@myplace.com>", "My subject", "My message"));
  }
  /**
   * useExactCopy() should be called to enable the new API behaviour when sending.
   */
  function testSendIsASingleEmailWhenUseExactCopyIsCalled()
  {
    $mockConnection =& $this->getWorkingMockConnection(3, null, 0, 0, true);
    $mockConnection->expectMinimumCallCount("write", 7);
    $mockConnection->expectAt(0, "write", array("EHLO xxx", "*"));
    $mockConnection->expectAt(1, "write", array("MAIL FROM: <me@myplace.com>", "*"));
    $mockConnection->expectAt(2, "write", array("RCPT TO: <foo@bar.com>", "*"));
    $mockConnection->expectAt(3, "write", array("RCPT TO: <zip@button.tld>", "*"));
    $mockConnection->expectAt(4, "write", array("RCPT TO: <mail@cheese.domain>", "*"));
    $mockConnection->expectAt(5, "write", array("DATA", "*"));
    $mockConnection->expectAt(6, "write", array("*", "*"));
    
    $swift =& new EasySwift($mockConnection, "xxx");
    $swift->useExactCopy();
    $addresses = array("Foo Bar <foo@bar.com>", '"Zip Button" <zip@button.tld>', "mail@cheese.domain");
    $this->assertEqual(3, $swift->send($addresses, "My name <me@myplace.com>", "My subject", "My message"));
  }
  /**
   * Equates to the same test as testSendIsASingleEmailWhenUseExactCopyIsCalled.
   * @see testSendIsASingleEmailWhenUseExactCopyIsCalled
   */
  function testSendIsASingleEmailWhenCcAddressesAreUsed()
  {
    $mockConnection =& $this->getWorkingMockConnection(5, null, 0, 0, true);
    $mockConnection->expectMinimumCallCount("write", 7);
    $mockConnection->expectAt(0, "write", array("EHLO xxx", "*"));
    $mockConnection->expectAt(1, "write", array("MAIL FROM: <me@myplace.com>", "*"));
    $mockConnection->expectAt(2, "write", array("RCPT TO: <foo@bar.com>", "*"));
    $mockConnection->expectAt(3, "write", array("RCPT TO: <zip@button.tld>", "*"));
    $mockConnection->expectAt(4, "write", array("RCPT TO: <mail@cheese.domain>", "*"));
    $mockConnection->expectAt(5, "write", array("RCPT TO: <cc1@address.co.uk>", "*"));
    $mockConnection->expectAt(6, "write", array("RCPT TO: <cc2@address.xxx>", "*"));
    $mockConnection->expectAt(7, "write", array("DATA", "*"));
    $mockConnection->expectAt(8, "write", array("*", "*"));
    
    $swift =& new EasySwift($mockConnection, "xxx");
    $swift->addCc("Carbon Copy Recipient One <cc1@address.co.uk>");
    $swift->addCc("cc2@address.xxx");
    $addresses = array("Foo Bar <foo@bar.com>", '"Zip Button" <zip@button.tld>', "mail@cheese.domain");
    $this->assertEqual(5, $swift->send($addresses, "My name <me@myplace.com>", "My subject", "My message"));
  }
  /**
   * The new API has a complete MIME layer, all message operations should be handed to it.
   */
  function testAddingPartsIsHandledByMessageObject()
  {
    $mockConnection =& $this->getWorkingMockConnection();
    
    $swift =& new EasySwift($mockConnection, "xxx");
    $mockMessage =& new MockMessage($this);
    $mockMessage->expectOnce("attach");
    $swift->newMessageFromObject($mockMessage);
    $swift->addPart("my part");
  }
  /**
   * Attachments should be dealt with by the MIME layer too.
   * @see testAddingPartsIsHandledByMessageObject
   */
  function testAddingAttachmentsIsHandledByMessageObject()
  {
    $mockConnection =& $this->getWorkingMockConnection();
    
    $swift =& new EasySwift($mockConnection, "xxx");
    $mockMessage =& new MockMessage($this);
    $mockMessage->expectCallCount("attach", 2);
    $swift->newMessageFromObject($mockMessage);
    $swift->addAttachment("my attachment", "my name.txt");
    $swift->addAttachment(new Swift_File("../files/gecko.png"));
  }
  /**
   * Images handled by MIME layer.
   * @see testAddingPartsIsHandledByMessageObject
   */
  function testAddingImagesIsHandledByMessageObject()
  {
    $mockConnection =& $this->getWorkingMockConnection();
    
    $swift =& new EasySwift($mockConnection, "xxx");
    $mockMessage =& new MockMessage($this);
    $mockMessage->expectCallCount("attach", 3);
    $swift->newMessageFromObject($mockMessage);
    $swift->addImage("../files/manchester.jpeg");
    $swift->addImage("../files/durham.gif");
    $swift->addImage("../files/gecko.png");
  }
  /**
   * Special case, adding images (or embedded files) returns Content-ID
   */
  function testCIDSrcValueIsReturnedWhenAddingImage()
  {
    $mockConnection =& $this->getWorkingMockConnection();
    
    $swift =& new EasySwift($mockConnection, "xxx");
    
    $this->assertPattern("/^cid:.+\$/i", $swift->addImage("../files/manchester.jpeg"));
    $this->assertPattern("/^cid:.+\$/i", $swift->addImage("../files/durham.gif"));
    $this->assertPattern("/^cid:.+\$/i", $swift->addImage(new Swift_File("../files/gecko.png")));
  }
  /**
   * Special case, adding images (or embedded files) returns Content-ID
   */
  function testCIDSrcValueIsReturnedWhenAddingEmbeddedFile()
  {
    $mockConnection =& $this->getWorkingMockConnection();
    
    $swift =& new EasySwift($mockConnection, "xxx");
    
    $this->assertPattern("/^cid:.+\$/i", $swift->embedFile(file_get_contents("../files/manchester.jpeg"), "image/jpeg"));
    $this->assertPattern("/^cid:.+\$/i", $swift->embedFile(new Swift_File("../files/durham.gif"), "image/gif", "myimage.gif"));
    $this->assertPattern("/^cid:my_cid123\$/i", $swift->embedFile(file_get_contents("../files/gecko.png"), "image/png", "myimage.png", "my_cid123"));
  }
  /**
   * EasySwift loads plugins in the manner of the old API but simply wraps around the new API.
   */
  function testPluginsAreLoadedWithEasySwiftButHandledBySwiftAsNormal()
  {
    $conn =& $this->getWorkingMockConnection();
    $swift =& new EasySwift($conn);
    $plugin =& new MockSendListener($this);
    $plugin->expectOnce("sendPerformed");
    $swift->loadPlugin($plugin, "myplugin");
    $swift->send("foo@bar.com", "me@mydomain.com", "Subject", "body");
  }
  /**
   * SMTP authentication will be enabled if SMTP connection is used.
   */
  function testSmtpAuthenticatorsAreAddedIfSmtpConnectionIsUsed()
  {
    $conn =& $this->getWorkingMockConnection(1, new MockSMTPConnection());
    $auth =& new Swift_Authenticator_PLAIN();
    $conn->expectOnce("attachAuthenticator", array($auth));
    
    $swift =& new EasySwift($conn);
    $swift->loadAuthenticator($auth);
  }
  /**
   * The authenticate() method comes from the old API, but is now wrapped around the new API.
   */
  function testSMTPAuthenticationReturnsTrueOnSuccess()
  {
    $conn =& new MockSMTPConnectionAuth($this);
    $conn->setReturnValue("isAlive", true);
    $conn->setReturnValueAt(0, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt(1, "read", "250-Hello xxx\r\n250-AUTH PLAIN\r\n250 HELP");
    $conn->setReturnValueAt(2, "read", "235 Authenticated");
    $conn->expectAt(1, "write", array("AUTH PLAIN " . base64_encode("foo\0foo\0bar"), "*"));
    
    $auth =& new Swift_Authenticator_PLAIN();
    
    $swift =& new EasySwift($conn);
    $swift->loadAuthenticator($auth);
    $this->assertTrue($swift->authenticate("foo", "bar"));
  }
  /**
   * The authenticate() method comes from the old API, but is now wrapped around the new API.
   * @see testSMTPAuthenticationReturnsTrueOnSuccess
   */
  function testSMTPAuthenticationReturnsFalseOnFailure()
  {
    $conn =& new MockSMTPConnectionAuth($this);
    $conn->setReturnValue("isAlive", true);
    $conn->setReturnValueAt(0, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt(1, "read", "250-Hello xxx\r\n250-AUTH PLAIN\r\n250 HELP");
    $conn->setReturnValueAt(2, "read", "500 No good");
    $conn->setReturnValueAt(3, "read", "250 reset ok");
    $conn->expectAt(1, "write", array("AUTH PLAIN " . base64_encode("foo\0foo\0bar"), "*"));
    
    $auth =& new Swift_Authenticator_PLAIN();
    
    $swift =& new EasySwift($conn);
    $swift->loadAuthenticator($auth);
    $this->assertFalse($swift->authenticate("foo", "bar"));
  }
  /**
   * addHeaders() from the old API should translate calls into the new API.
   */
  function testMessageHeadersAreInvokedWhenAddHeadersIsCalled()
  {
    $conn =& $this->getWorkingMockConnection();
    $swift =& new EasySwift($conn);
    $headers =& new MockHeaders($this);
    $headers->expectCallCount("set", 2);
    $headers->expectAt(0, "set", array("Foo", "test"));
    $headers->expectAt(1, "set", array("Bar", "test2"));
    $swift->message->setHeaders($headers);
    $swift->addHeaders("Foo: test\r\nBar: test2");
  }
  /**
   * Headers should be parsed and attributes passed to the setAttribute() method of the Swift_Message_Headers class.
   */
  function testAttributesCanBeSetInHeaders()
  {
    $conn =& $this->getWorkingMockConnection();
    $swift =& new EasySwift($conn);
    $headers =& new MockHeaders($this);
    $headers->expectCallCount("set", 2);
    $headers->expectAt(0, "set", array("Foo", "test"));
    $headers->expectAt(1, "set", array("Bar", "test2"));
    $headers->expectCallCount("setAttribute", 4);
    $headers->expectAt(0, "setAttribute", array("Foo", "xxx", "yyy"));
    $headers->expectAt(1, "setAttribute", array("Bar", "abc", "def"));
    $headers->expectAt(2, "setAttribute", array("Bar", "example", "something"));
    $headers->expectAt(3, "setAttribute", array("Bar", "foo", "bar"));
    $swift->message->setHeaders($headers);
    $swift->addHeaders("Foo: test; xxx=\"yyy\"\r\nBar: test2;\r\n abc=def; example=\"something\"; foo=bar");
  }
}
