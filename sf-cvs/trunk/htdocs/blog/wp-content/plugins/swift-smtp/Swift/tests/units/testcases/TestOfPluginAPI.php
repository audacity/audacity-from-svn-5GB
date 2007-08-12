<?php

if (!class_exists("FullMockConnection")) Mock::Generate("DummyConnection", "FullMockConnection");
Mock::GeneratePartial("Swift_Events_Listener", "MockSendListener", array("sendPerformed"));
Mock::GeneratePartial("Swift_Events_Listener", "MockBeforeSendListener", array("beforeSendPerformed"));
Mock::GeneratePartial("Swift_Events_Listener", "MockCommandListener", array("commandSent"));
Mock::GeneratePartial("Swift_Events_Listener", "MockBeforeCommandListener", array("beforeCommandSent"));
Mock::GeneratePartial("Swift_Events_Listener", "MockResponseListener", array("responseReceived"));
Mock::GeneratePartial("Swift_Events_Listener", "MockConnectListener", array("connectPerformed"));
Mock::GeneratePartial("Swift_Events_Listener", "MockDisconnectListener", array("disconnectPerformed"));

class TestOfPluginAPI extends UnitTestCase
{
  /** Get a mock connection for testing
   * @param int The number emails you expect to send
   * @return FullMockConnection
   */
  function &getWorkingMockConnection($send=1)
  {
    $count = 0;
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt($count++, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt($count++, "read", "250-Hello xxx\r\n250 HELP");
    for ($i = 0; $i < $send; $i++)
    {
      $conn->setReturnValueAt($count++, "read", "250 Ok");
      $conn->setReturnValueAt($count++, "read", "250 Ok");
      $conn->setReturnValueAt($count++, "read", "354 Go ahead");
      $conn->setReturnValueAt($count++, "read", "250 Ok");
    }
    $conn->setReturnValueAt($count++, "read", "221 Bye");
    return $conn;
  }
  
  /** Get a mock connection for testing
   * @param int The number emails you expect to send
   * @return FullMockConnection
   */
  function &getFailingMockConnection($send=1)
  {
    $count = 0;
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt($count++, "read", "220 xxx ESMTP");
    $conn->setReturnValueAt($count++, "read", "250-Hello xxx\r\n250 HELP");
    for ($i = 0; $i < $send; $i++)
    {
      $conn->setReturnValueAt($count++, "read", "250 Ok");
      $conn->setReturnValueAt($count++, "read", "500 Denied");
      $conn->setReturnValueAt($count++, "read", "250 Reset done");
    }
    $conn->setReturnValueAt($count++, "read", "221 Bye");
    return $conn;
  }
  
  function setUp()
  {
    Swift_Errors::reset();
  }
  
  function testListenersIdentifyWhatTheyImplement()
  {
    $listener =& new MockSendListener($this);
    $this->assertTrue($listener->implementing("SendListener"));
    $this->assertFalse($listener->implementing("ConnectListener"));
    $this->assertTrue(method_exists($listener, "sendPerformed"));
    
    $listener =& new MockCommandListener($this);
    $this->assertTrue($listener->implementing("CommandListener"));
    
    $listener =& new MockResponseListener($this);
    $this->assertTrue($listener->implementing("ResponseListener"));
  }
  
  function testListenersCanBeRetreivedByReference()
  {
    $listener =& new MockSendListener($this);
    $conn =& $this->getWorkingMockConnection(1);
    $swift =& new Swift($conn);
    $swift->attachPlugin($listener, "myplugin");
    $this->assertReference($listener, $swift->getPlugin("myplugin"));
  }
  
  function testListenersCanBeRemovedOnceAdded()
  {
    $listener =& new MockSendListener($this);
    $conn = $this->getWorkingMockConnection(1);
    $swift =& new Swift($conn);
    $swift->attachPlugin($listener, "myplugin");
    $this->assertReference($listener, $swift->getPlugin("myplugin"));
    $swift->removePlugin("myplugin");
    $this->assertNull($swift->getPlugin("myplugin"));
  }
  
  function testSendListenerIsNotifiedOnSend()
  {
    $listener =& new MockSendListener($this);
    $listener->expectOnce("sendPerformed");
    $conn =& $this->getWorkingMockConnection(1);
    $message =& new Swift_Message("Subject", "Body");
    $swift =& new Swift($conn);
    $swift->attachPlugin($listener, "myplugin");
    $swift->send($message, new Swift_Address("foo@bar.com"), new Swift_Address("me@myplace.com"));
    
    $listener =& new MockSendListener($this);
    $listener->expectCallCount("sendPerformed", 5);
    $conn =& $this->getWorkingMockConnection(5);
    $message =& new Swift_Message("Subject", "Body");
    $swift =& new Swift($conn);
    $swift->attachPlugin($listener, "myplugin");
    for ($i = 0; $i < 5; $i++)
      $swift->send($message, new Swift_Address("foo@bar.com"), new Swift_Address("me@myplace.com"));
  }
  
  function testSendListenerDoesntRunWhenSendNotSuccessful()
  {
    //This changed in 3.0.7
  }
  
  function testBeforeSendListenerIsNotifiedBeforeSending()
  {
    $before_send_listener =& new MockBeforeSendListener($this);
    $before_send_listener->expectOnce("beforeSendPerformed");
    $conn =& $this->getWorkingMockConnection(1);
    $message =& new Swift_Message("Subject", "Body");
    $swift =& new Swift($conn);
    $swift->attachPlugin($before_send_listener, "myplugin");
    $swift->send($message, new Swift_Address("foo@bar.com"), new Swift_Address("me@myplace.com"));
    
    $before_send_listener =& new MockBeforeSendListener($this);
    $before_send_listener->expectCallCount("beforeSendPerformed", 5);
    $conn =& $this->getWorkingMockConnection(5);
    $message =& new Swift_Message("Subject", "Body");
    $swift =& new Swift($conn);
    $swift->attachPlugin($before_send_listener, "myplugin");
    for ($i = 0; $i < 5; $i++)
      $swift->send($message, new Swift_Address("foo@bar.com"), new Swift_Address("me@myplace.com"));
  }
  
  function testBeforeSendListenerRunsEvenWhenSendNotSuccessful()
  {
    $before_send_listener =& new MockBeforeSendListener($this);
    $before_send_listener->expectOnce("beforeSendPerformed");
    $conn =& $this->getFailingMockConnection(1);
    $message =& new Swift_Message("Subject", "Body");
    $swift =& new Swift($conn);
    $swift->attachPlugin($before_send_listener, "myplugin");
    $this->assertFalse($swift->send($message, new Swift_Address("foo@bar.com"), new Swift_Address("me@myplace.com")));
    
    $before_send_listener =& new MockBeforeSendListener($this);
    $before_send_listener->expectCallCount("beforeSendPerformed", 5);
    $conn =& $this->getFailingMockConnection(5);
    $message =& new Swift_Message("Subject", "Body");
    $swift =& new Swift($conn);
    $swift->attachPlugin($before_send_listener, "myplugin");
    for ($i = 0; $i < 5; $i++)
    {
      $this->assertFalse($swift->send($message, new Swift_Address("foo@bar.com"), new Swift_Address("me@myplace.com")));
    }
  }
  
  function testCommandListenerRunsAfterEachCommand()
  {
    $conn =& $this->getWorkingMockConnection();
    //ehlo, mail from, rcpt to, data, msg
    $conn->expectMinimumCallCount("write", 5);
    
    $command_listener =& new MockCommandListener($this);
    $command_listener->expectMinimumCallCount("commandSent", 5);
    
    $swift =& new Swift($conn, "xxx", SWIFT_NO_START);
    $swift->attachPlugin($command_listener, "myplugin");
    $swift->connect();
    $message =& new Swift_Message("Subject", "Body");
    $swift->send($message, new Swift_Address("foo@bar.com"), new Swift_Address("me@myplace.com"));
    
// 		$this->assertEqual($conn->getCallCount("write"), $command_listener->getCallCount("commandSent"));
  }
  
  function testBeforeCommandListenerRunsBeforeEachCommand()
  {
    $conn =& $this->getWorkingMockConnection();
    //ehlo, mail from, rcpt to, data, msg
    $conn->expectMinimumCallCount("write", 5);
    
    $before_command_listener =& new MockBeforeCommandListener($this);
    $before_command_listener->expectMinimumCallCount("beforeCommandSent", 5);
    
    $swift =& new Swift($conn, "xxx", SWIFT_NO_START);
    $swift->attachPlugin($before_command_listener, "myplugin");
    $swift->connect();
    $message =& new Swift_Message("Subject", "Body");
    $swift->send($message, new Swift_Address("foo@bar.com"), new Swift_Address("me@myplace.com"));
// 		$this->assertEqual($conn->getCallCount("write"), $before_command_listener->getCallCount("beforeCommandSent"));
  }
  
  function testResponseListenerRunsAfterEachResponse()
  {
    $conn =& $this->getWorkingMockConnection();
    //ehlo, mail from, rcpt to, data, msg
    $conn->expectMinimumCallCount("read", 6);
    
    $response_listener =& new MockResponseListener($this);
    $response_listener->expectMinimumCallCount("responseReceived", 6);
    
    $swift =& new Swift($conn, "xxx", SWIFT_NO_START);
    $swift->attachPlugin($response_listener, "myplugin");
    $swift->connect();
    $message =& new Swift_Message("Subject", "Body");
    $swift->send($message, new Swift_Address("foo@bar.com"), new Swift_Address("me@myplace.com"));
    
// 		$this->assertEqual($conn->getCallCount("read"), $response_listener->getCallCount("responseReceived"));
  }
  
  function testConnectListenerRunsUponConnect()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 Hello xx");
    $conn->setReturnValueAt(1, "read", "250 Hello xxx");
    $conn->setReturnValueAt(2, "read", "221 Bye");
    
    $connect_listener =& new MockConnectListener($this);
    $connect_listener->expectCallCount("connectPerformed", 1);
    
    $swift =& new Swift($conn, "xxx", SWIFT_NO_START);
    $swift->attachPlugin($connect_listener, "myplugin");
    $swift->connect();
    
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 Hello xx");
    $conn->setReturnValueAt(1, "read", "250 Hello xxx");
    $conn->setReturnValueAt(2, "read", "221 Bye");
    $conn->setReturnValueAt(3, "read", "220 Hello xx");
    $conn->setReturnValueAt(4, "read", "250 Hello xxx");
    $conn->setReturnValueAt(5, "read", "221 Bye");
    $conn->setReturnValueAt(6, "read", "220 Hello xx");
    $conn->setReturnValueAt(7, "read", "250 Hello xxx");
    $conn->setReturnValueAt(8, "read", "221 Bye");
    
    $connect_listener =& new MockConnectListener($this);
    $connect_listener->expectCallCount("connectPerformed", 3);
    
    $swift =& new Swift($conn, "xxx", SWIFT_NO_START);
    $swift->attachPlugin($connect_listener, "myplugin");
    $swift->connect();
    $swift->disconnect();
    $swift->connect();
    $swift->disconnect();
    $swift->connect();
    $swift->disconnect();
  }
  
  function testDisconnectListenerRunsUponDisconnect()
  {
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 Hello xx");
    $conn->setReturnValueAt(1, "read", "250 Hello xxx");
    $conn->setReturnValueAt(2, "read", "221 Bye");
    
    $disconnect_listener =& new MockDisconnectListener($this);
    $disconnect_listener->expectCallCount("disconnectPerformed", 1);
    
    $swift =& new Swift($conn);
    $swift->attachPlugin($disconnect_listener, "myplugin");
    $swift->disconnect();
    
    $conn =& new FullMockConnection($this);
    $conn->setReturnValueAt(0, "read", "220 Hello xx");
    $conn->setReturnValueAt(1, "read", "250 Hello xxx");
    $conn->setReturnValueAt(2, "read", "221 Bye");
    $conn->setReturnValueAt(3, "read", "220 Hello xx");
    $conn->setReturnValueAt(4, "read", "250 Hello xxx");
    $conn->setReturnValueAt(5, "read", "221 Bye");
    $conn->setReturnValueAt(6, "read", "220 Hello xx");
    $conn->setReturnValueAt(7, "read", "250 Hello xxx");
    $conn->setReturnValueAt(8, "read", "221 Bye");
    
    $disconnect_listener =& new MockDisconnectListener($this);
    $disconnect_listener->expectCallCount("disconnectPerformed", 3);
    
    $swift =& new Swift($conn);
    $swift->attachPlugin($disconnect_listener, "myplugin");
    $swift->disconnect();
    $swift->connect();
    $swift->disconnect();
    $swift->connect();
    $swift->disconnect();
  }
}
