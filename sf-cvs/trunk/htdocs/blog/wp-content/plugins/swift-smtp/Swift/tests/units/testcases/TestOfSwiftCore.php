<?php

if (!class_exists("FullMockConnection")) Mock::Generate("DummyConnection", "FullMockConnection");
if (!class_exists("MockBaseConnection")) Mock::GeneratePartial("DummyConnection", "MockBaseConnection", array("start", "stop", "read", "write"));
Mock::GeneratePartial("Swift_Message", "Message_RecipientsMocked", array("setTo", "setCc", "setBcc", "setFrom", "setReplyTo"));
Mock::GeneratePartial("Swift_Message", "Message_IdMocked", array("generateId"));
Mock::GeneratePartial("Swift_Message", "Message_EncodingMocked", array("setEncoding", "getEncoding"));
Mock::GeneratePartial("Swift_Log_DefaultLog", "MockLogger", array("add"));

class TestOfSwiftCore extends UnitTestCase
{
  function setUp()
  {
    Swift_Errors::reset();
  }
  
  function testConnectionIsInvokedAtInstantiation()
  {
    $conn =& new FullMockConnection($this);
    $conn->expectOnce("start");
    $swift =& new Swift($conn, false, SWIFT_NO_HANDSHAKE);
  }
  
  function testConnectionIsNotStartedIfNO_STARTFlagIsSet()
  {
    $conn =& new FullMockConnection($this);
    $conn->expectNever("start");
    $swift =& new Swift($conn, null, SWIFT_NO_START);
  }
  
  function testHELOIsSentAfterA220Response()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 xxx");
    $conn->setReturnValueAt(1, "read", "250 xxx");
    $conn->expectOnce("write", array("HELO mydomain", "*"));
    $swift =& new Swift($conn, "mydomain");
  }
  
  function testEHLOIsSentIf220ResponseContainsESMTP()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 abc ESMTP xx");
    $conn->setReturnValueAt(1, "read", "250 xxx");
    $conn->expectOnce("write", array("EHLO mydomain", "*"));
    $swift =& new Swift($conn, "mydomain");
    
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 ESMTP");
    $conn->setReturnValueAt(1, "read", "250 xxx");
    $conn->expectOnce("write", array("EHLO mydomain", "*"));
    $swift =& new Swift($conn, "mydomain");
  }
  
  function testExceptionIsThrownIf200ResponseIsNotReceivedAtStart()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValue("read", "000 abc ESMTP xx");
    $this->expectError();
    $swift =& new Swift($conn, "mydomain");
    
    $this->expectError();
    $conn =& new FullMockConnection($this);
    $conn->setReturnValue("read", "120 abc ESMTP xx");
    $swift =& new Swift($conn, "mydomain");
    
    $this->expectError();
    $conn =& new FullMockConnection($this);
    $conn->setReturnValue("read", "x220 abc ESMTP xx");
    $swift =& new Swift($conn, "mydomain");
  }
  
  function testExtensionListIsStored()
  {
    $conn =& new MockBaseConnection($this);
    $conn->setReturnValueAt(0, "read", "220 abc ESMTP xx");
    $conn->setReturnValueAt(1, "read",
      "250-Hello xxxx\r\n" .
      "250-SIZE 52428800\r\n" .
      "250-8BITMIME\r\n" .
      "250-PIPELINING\r\n" .
      "250-STARTTLS\r\n" .
      "250-AUTH LOGIN PLAIN CRAM-MD5\r\n" .
      "250 HELP");
    $swift =& new Swift($conn, "mydomain");
    $this->assertTrue($swift->connection->hasExtension("SIZE"));
    $this->assertTrue($swift->connection->hasExtension("8BITMIME"));
    $this->assertTrue($swift->connection->hasExtension("PIPELINING"));
    $this->assertTrue($swift->connection->hasExtension("STARTTLS"));
    $this->assertTrue($swift->connection->hasExtension("AUTH"));
    $this->assertTrue($swift->connection->hasExtension("HELP"));
    $this->assertFalse($swift->connection->hasExtension("FOOBAR"));
    $this->assertFalse($swift->connection->hasExtension("XXXX"));
  }
  
  function testAttributesCanBeReadFromExtensions()
  {
    $conn =& new MockBaseConnection($this);
    $conn->setReturnValueAt(0, "read", "220 abc ESMTP xx");
    $conn->setReturnValueAt(1, "read",
      "250-Hello xxxx\r\n" .
      "250-SIZE 52428800\r\n" .
      "250-8BITMIME\r\n" .
      "250-PIPELINING\r\n" .
      "250-STARTTLS\r\n" .
      "250-AUTH LOGIN PLAIN CRAM-MD5\r\n" .
      "250 HELP");
    $swift =& new Swift($conn, "mydomain");
    $this->assertEqual(array("52428800"), $swift->connection->getAttributes("SIZE"));
    $this->assertEqual(array(), $swift->connection->getAttributes("8BITMIME"));
    $this->assertEqual(array(), $swift->connection->getAttributes("PIPELINING"));
    $this->assertEqual(array(), $swift->connection->getAttributes("STARTTLS"));
    $this->assertEqual(array("LOGIN", "PLAIN", "CRAM-MD5"), $swift->connection->getAttributes("AUTH"));
    $this->assertEqual(array(), $swift->connection->getAttributes("HELP"));
  }
  
  function testExceptionIsThrownIfAttributesAreReadFromNonExistentExtension()
  {
    $conn =& new MockBaseConnection($this);
    $conn->setReturnValueAt(0, "read", "220 abc ESMTP xx");
    $conn->setReturnValueAt(1, "read",
      "250-Hello xxxx\r\n" .
      "250 SIZE 52428800");
    $swift =& new Swift($conn, "mydomain");
    
    $this->expectError();
    $x = $swift->connection->getAttributes("AUTH");
    
    $this->expectError();
    $x = $swift->connection->getAttributes("8BITMIME");
  }
  
  function testPostConnectIsInvokedInConnectionAfterHandshake()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 ESMTP");
    $conn->setReturnValueAt(1, "read", "250 xxx");
    $conn->expectOnce("postConnect");
    $swift =& new Swift($conn, "mydomain");
  }
  
  function testSMTPCommandsAreExecutedOnSend()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt(1, "read", "250-Hello xxx\r\n250 HELP");
    $conn->setReturnValueAt(2, "read", "250 Ok");
    $conn->setReturnValueAt(3, "read", "250 Ok");
    $conn->setReturnValueAt(4, "read", "354 Go ahead");
    $conn->setReturnValueAt(5, "read", "250 Ok");
    $conn->expectMinimumCallCount("write", 5);
    $conn->expectAt(0, "write", array("EHLO abc", "*"));
    $conn->expectAt(1, "write", array("MAIL FROM: <foo@bar.tld>", "*"));
    $conn->expectAt(2, "write", array("RCPT TO: <xxx@yyy.tld>", "*"));
    $conn->expectAt(3, "write", array("DATA", "*"));
    $conn->expectAt(4, "write", array("*", "*"));
    $swift =& new Swift($conn, "abc");
    $message =& new Swift_Message("My Subject", "my body");
    $swift->send($message, new Swift_Address("xxx@yyy.tld", "XXX YYY"), new Swift_Address("foo@bar.tld", "Foo Bar"));
    
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt(1, "read", "250-Hello xxx\r\n250 HELP");
    $conn->setReturnValueAt(2, "read", "250 Ok");
    $conn->setReturnValueAt(3, "read", "250 Ok");
    $conn->setReturnValueAt(4, "read", "250 Ok");
    $conn->setReturnValueAt(5, "read", "354 Go ahead");
    $conn->setReturnValueAt(6, "read", "250 Ok");
    $conn->expectMinimumCallCount("write", 6);
    $conn->expectAt(0, "write", array("EHLO abc", "*"));
    $conn->expectAt(1, "write", array("MAIL FROM: <foo@bar.tld>", "*"));
    $conn->expectAt(2, "write", array("RCPT TO: <xxx@yyy.tld>", "*"));
    $conn->expectAt(3, "write", array("RCPT TO: <abc@def.tld>", "*"));
    $conn->expectAt(4, "write", array("DATA", "*"));
    $conn->expectAt(5, "write", array("*", "*"));
    $swift =& new Swift($conn, "abc");
    $message =& new Swift_Message("My Subject", "my body");
    
    $recipients =& new Swift_RecipientList();
    $recipients->addTo("xxx@yyy.tld", "XXX YYY");
    $recipients->addCc("abc@def.tld");
    $swift->send($message, $recipients, new Swift_Address("foo@bar.tld", "Foo Bar"));
  }
  
  function testZeroIsReturnedIfNo250ResponsesAreIssuedAtRCPT()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt(1, "read", "250-Hello xxx\r\n250 HELP");
    $conn->setReturnValueAt(2, "read", "250 Ok");
    $conn->setReturnValueAt(3, "read", "550 Denied");
    $conn->setReturnValueAt(4, "read", "250 ok");
    $conn->expectAt(0, "write", array("EHLO abc", "*"));
    $conn->expectAt(1, "write", array("MAIL FROM: <foo@bar.tld>", "*"));
    $conn->expectAt(2, "write", array("RCPT TO: <xxx@yyy.tld>", "*"));
    $conn->expectAt(3, "write", array("RSET", "*"));
    $conn->expectMinimumCallCount("write", 4);
    $swift =& new Swift($conn, "abc");
    $message =& new Swift_Message("My Subject", "my body");
    $this->assertEqual(0, $swift->send($message, new Swift_Address("xxx@yyy.tld", "XXX YYY"), new Swift_Address("foo@bar.tld", "Foo Bar")));
  }
  
  function testRSETIsIssuedOnFailure()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt(1, "read", "250-Hello xxx\r\n250 HELP");
    $conn->setReturnValueAt(2, "read", "250 Ok");
    $conn->setReturnValueAt(3, "read", "550 Denied");
    $conn->setReturnValueAt(4, "read", "550 Denied");
    $conn->setReturnValueAt(5, "read", "250 Reset ok");
    $conn->expectAt(0, "write", array("EHLO abc", "*"));
    $conn->expectAt(1, "write", array("MAIL FROM: <foo@bar.tld>", "*"));
    $conn->expectAt(2, "write", array("RCPT TO: <xxx@yyy.tld>", "*"));
    $conn->expectAt(3, "write", array("RCPT TO: <abc@def.tld>", "*"));
    $conn->expectAt(4, "write", array("RSET", "*"));
    
    $swift =& new Swift($conn, "abc");
    $message =& new Swift_Message("My Subject", "my body");
    $recipients =& new Swift_RecipientList();
    $recipients->addTo("xxx@yyy.tld", "XXX YYY");
    $recipients->addCc("abc@def.tld");
    $sent = $swift->send($message, $recipients, new Swift_Address("foo@bar.tld", "Foo Bar"));
    $this->assertFalse($sent);
  }
  
  function testAddressHeadersAreInjectedBeforeSending()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt(1, "read", "250-Hello xxx\r\n250 HELP");
    $conn->setReturnValueAt(2, "read", "250 Ok");
    $conn->setReturnValueAt(3, "read", "250 Ok");
    $conn->setReturnValueAt(4, "read", "250 Ok");
    $conn->setReturnValueAt(5, "read", "354 Go ahead");
    $conn->setReturnValueAt(6, "read", "250 Ok");
    $conn->expectMinimumCallCount("write", 6);
    $conn->expectAt(0, "write", array("EHLO abc", "*"));
    $conn->expectAt(1, "write", array("MAIL FROM: <foo@bar.tld>", "*"));
    $conn->expectAt(2, "write", array("RCPT TO: <xxx@yyy.tld>", "*"));
    $conn->expectAt(3, "write", array("RCPT TO: <abc@def.tld>", "*"));
    $conn->expectAt(4, "write", array("DATA", "*"));
    $conn->expectAt(5, "write", array("*", "*"));
    $swift =& new Swift($conn, "abc");
    
    $recipients =& new Swift_RecipientList();
    $recipients->addTo("xxx@yyy.tld", "XXX YYY");
    $recipients->addCc("abc@def.tld");
    $from =& new Swift_Address("foo@bar.tld", "Foo Bar");
    
    $message =& new Message_RecipientsMocked($this);
    $message->Swift_Message();
    $message->setSubject("the subject");
    $message->setBody("the body");
    $message->expectAt(0, "setTo", array(array("XXX YYY <xxx@yyy.tld>")));
    $message->expectAt(0, "setCc", array(array("abc@def.tld")));
    $message->expectAt(0, "setFrom", array($from->build()));
    
    $swift->send($message, $recipients, $from);
  }
  
  function testRecipientHeadersAreRestoredAfterSending()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt(1, "read", "250-Hello xxx\r\n250 HELP");
    $conn->setReturnValueAt(2, "read", "250 Ok");
    $conn->setReturnValueAt(3, "read", "250 Ok");
    $conn->setReturnValueAt(4, "read", "250 Ok");
    $conn->setReturnValueAt(5, "read", "354 Go ahead");
    $conn->setReturnValueAt(6, "read", "250 Ok");
    $conn->expectMinimumCallCount("write", 6);
    $conn->expectAt(0, "write", array("EHLO abc", "*"));
    $conn->expectAt(1, "write", array("MAIL FROM: <foo@bar.tld>", "*"));
    $conn->expectAt(2, "write", array("RCPT TO: <xxx@yyy.tld>", "*"));
    $conn->expectAt(3, "write", array("RCPT TO: <abc@def.tld>", "*"));
    $conn->expectAt(4, "write", array("DATA", "*"));
    $conn->expectAt(5, "write", array("*", "*"));
    $swift =& new Swift($conn, "abc");
    
    $message =& new Swift_Message("the subject", "the body");
    $recipients =& new Swift_RecipientList();
    $recipients->addTo("xxx@yyy.tld", "XXX YYY");
    $recipients->addCc("abc@def.tld");
    $swift->send($message, $recipients, new Swift_Address("foo@bar.tld", "Foo Bar"));
    
    $this->assertEqual(array(), $message->getTo());
    $this->assertFalse($message->getCc());
    $this->assertFalse($message->getReturnPath());
    $this->assertFalse($message->getReplyTo());
  }
  
  function testMessageIdIsGeneratedBeforeSending()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt(1, "read", "250-Hello xxx\r\n250 HELP");
    $conn->setReturnValueAt(2, "read", "250 Ok");
    $conn->setReturnValueAt(3, "read", "250 Ok");
    $conn->setReturnValueAt(4, "read", "250 Ok");
    $conn->setReturnValueAt(5, "read", "354 Go ahead");
    $conn->setReturnValueAt(6, "read", "250 Ok");
    $conn->expectMinimumCallCount("write", 6);
    $conn->expectAt(0, "write", array("EHLO abc", "*"));
    $conn->expectAt(1, "write", array("MAIL FROM: <foo@bar.tld>", "*"));
    $conn->expectAt(2, "write", array("RCPT TO: <xxx@yyy.tld>", "*"));
    $conn->expectAt(3, "write", array("RCPT TO: <abc@def.tld>", "*"));
    $conn->expectAt(4, "write", array("DATA", "*"));
    $conn->expectAt(5, "write", array("*", "*"));
    $swift =& new Swift($conn, "abc");
    
    $recipients =& new Swift_RecipientList();
    $recipients->addTo("xxx@yyy.tld", "XXX YYY");
    $recipients->addCc("abc@def.tld");
    $from =& new Swift_Address("foo@bar.tld", "Foo Bar");
    
    $message =& new Message_IdMocked($this);
    $message->Swift_Message();
    $message->setSubject("the subject");
    $message->setBody("the body");
    $message->expectOnce("generateId");
    
    $swift->send($message, $recipients, $from);
  }
  
  function testQPEncodingIsUsedIf8BITMIMENotPresentAndCharactersOutside7BitRange()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt(1, "read", "250-Hello xxx\r\n250 HELP");
    $conn->setReturnValueAt(2, "read", "250 Ok");
    $conn->setReturnValueAt(3, "read", "250 Ok");
    $conn->setReturnValueAt(4, "read", "354 Go ahead");
    $conn->setReturnValueAt(5, "read", "250 Ok");
    
    $message =& new Message_EncodingMocked($this);
    $message->Swift_Message();
    $message->setSubject("foobar");
    $message->setBody("cenvéla");
    $message->setReturnValue("getEncoding", false);
    $message->setReturnValueAt(2, "getEncoding", "quoted-printable");
    $message->expectCallCount("setEncoding", 2);
    $message->expectAt(1, "setEncoding", array("QP", true, true));
    
    $swift =& new Swift($conn, "xxx");
    $swift->send($message, new Swift_Address("xxx@yyy.com"), new Swift_Address("abc@vvv.tld"));
  }
  
  function testSendReturnsNumberOfSuccessfulEnvelopes()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt(1, "read", "250-Hello xxx\r\n250 HELP");
    $conn->setReturnValueAt(2, "read", "250 Ok");
    $conn->setReturnValueAt(3, "read", "250 Ok");
    $conn->setReturnValueAt(4, "read", "354 Go ahead");
    $conn->setReturnValueAt(5, "read", "250 Ok");
    $swift =& new Swift($conn, "abc");
    $message =& new Swift_Message("My Subject", "my body");
    $ret = $swift->send($message, new Swift_Address("xxx@yyy.tld", "XXX YYY"), new Swift_Address("foo@bar.tld", "Foo Bar"));
    $this->assertEqual(1, $ret);
    
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt(1, "read", "250-Hello xxx\r\n250 HELP");
    $conn->setReturnValueAt(2, "read", "250 Ok");
    $conn->setReturnValueAt(3, "read", "250 Ok");
    $conn->setReturnValueAt(4, "read", "250 Ok");
    $conn->setReturnValueAt(5, "read", "354 Go ahead");
    $conn->setReturnValueAt(6, "read", "250 Ok");
    $swift =& new Swift($conn, "abc");
    $message =& new Swift_Message("My Subject", "my body");
    
    $recipients =& new Swift_RecipientList();
    $recipients->addTo("xxx@yyy.tld", "XXX YYY");
    $recipients->addCc("abc@def.tld");
    $ret = $swift->send($message, $recipients, new Swift_Address("foo@bar.tld", "Foo Bar"));
    $this->assertEqual(2, $ret);
  }
  
  function testFailedRecipientsAreReturned()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt(1, "read", "250-Hello xxx\r\n250 HELP");
    $conn->setReturnValueAt(2, "read", "250 Ok");
    $conn->setReturnValueAt(3, "read", "550 Denied");
    $conn->setReturnValueAt(4, "read", "250 ok");
    $conn->setReturnValueAt(5, "read", "550 Denied");
    $conn->setReturnValueAt(6, "read", "354 Go ahead");
    $conn->setReturnValueAt(7, "read", "250 ok");
    $swift =& new Swift($conn, "abc", SWIFT_ENABLE_LOGGING);
    $message =& new Swift_Message("My Subject", "my body");
    $recipients =& new Swift_RecipientList();
    $recipients->addTo("xxx@yyy.tld", "XXX YYY");
    $recipients->addTo("someone@somewhere.tld");
    $recipients->addCc("abc@def.tld");
    
    $this->assertEqual(1, $swift->send($message, $recipients, new Swift_Address("foo@bar.tld", "Foo Bar")));
    $this->assertEqual(array("xxx@yyy.tld", "abc@def.tld"), $swift->log->getFailedRecipients());
  }
  
  function testBatchSendingDoesNotCopyAllRecipientsInOnASingleEmail()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt(1, "read", "250-Hello xxx\r\n250 HELP");
    $conn->setReturnValueAt(2, "read", "250 Ok");
    $conn->setReturnValueAt(3, "read", "250 Ok");
    $conn->setReturnValueAt(4, "read", "354 Go ahead");
    $conn->setReturnValueAt(5, "read", "250 ok");
    $conn->setReturnValueAt(6, "read", "250 Ok");
    $conn->setReturnValueAt(7, "read", "250 Ok");
    $conn->setReturnValueAt(8, "read", "354 Go ahead");
    $conn->setReturnValueAt(9, "read", "250 ok");
    
    $message =& new Swift_Message("My Subject", "my body");
    $message->setEncoding("8bit");
    
    $conn->expectAt(1, "write", array("MAIL FROM: <foo@bar.tld>", "*"));
    $conn->expectAt(2, "write", array("RCPT TO: <xxx@yyy.tld>", "*"));
    $conn->expectAt(3, "write", array("DATA", "*"));
    $conn->expectAt(4, "write", array("*", "*"));
    
    $swift =& new Swift($conn, "abc", SWIFT_ENABLE_LOGGING);
    $recipients =& new Swift_RecipientList();
    $recipients->addTo("xxx@yyy.tld", "XXX YYY");
    $recipients->addTo("someone@somewhere.tld");
    
    $this->assertEqual(2, $swift->batchSend($message, $recipients, new Swift_Address("foo@bar.tld", "Foo Bar")));
  }
  
  function testLoggerIsInvokedIfSetActive()
  {
    $conn =& new FullMockConnection();
    $conn->setReturnValueAt(0, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt(1, "read", "250-Hello xxx\r\n250 HELP");
    $conn->setReturnValueAt(2, "read", "250 Ok");
    $conn->setReturnValueAt(3, "read", "250 ok");
    $conn->setReturnValueAt(4, "read", "354 Go ahead");
    $conn->setReturnValueAt(5, "read", "250 ok");
    $swift =& new Swift($conn, "abc");
    $logger =& new MockLogger($this);
    $logger->expectMinimumCallCount("add", 8);
    $swift->setLogger($logger);
    $swift->log->enable();
    $message =& new Swift_Message("My Subject", "my body");
    $swift->send($message, new Swift_Address("zip@button.tld"), new Swift_Address("foo@bar.tld", "Foo Bar"));
  }
}
