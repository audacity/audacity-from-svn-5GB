<?php

if (!class_exists("FullMockConnection")) Mock::Generate("DummyConnection", "FullMockConnection");

class TestOfRotatorConnection extends UnitTestCase
{
  function setUp()
  {
    Swift_Errors::reset();
  }
  
  function testConnectionIsRotatedWhenNextConnectionIsCalled()
  {
    $mock1 =& new FullMockConnection($this);
    $mock1->setReturnValue("start", true);
    $mock1->setReturnValue("isAlive", true);
    $mock1->setReturnValueAt(0, "isAlive", false);
    $mock1->setReturnValueAt(1, "isAlive", true);
    $mock1->expectOnce("start");
    $mock1->expectOnce("read");
    $mock1->expectOnce("write", array("foo", "*"));
    
    $mock2 =& new FullMockConnection($this);
    $mock2->setReturnValue("start", true);
    $mock2->setReturnValue("isAlive", true);
    $mock2->setReturnValueAt(0, "isAlive", false);
    $mock2->setReturnValueAt(1, "isAlive", true);
    $mock2->expectOnce("start");
    $mock2->expectOnce("read");
    $mock2->expectNever("write");
    
    $mock3 =& new FullMockConnection($this);
    $mock3->setReturnValue("start", true);
    $mock3->setReturnValue("isAlive", true);
    $mock3->setReturnValueAt(0, "isAlive", false);
    $mock3->setReturnValueAt(1, "isAlive", true);
    $mock3->expectOnce("start");
    $mock3->expectOnce("read");
    $mock3->expectOnce("write", array("bar", "*"));
    
    $multi =& new Swift_Connection_Rotator();
    $multi->addConnection($mock1);
    $multi->addConnection($mock2);
    $multi->addConnection($mock3);
    
    $multi->start();
    $multi->read();
    $multi->write("foo");
    $multi->nextConnection();
    $multi->read();
    $multi->nextConnection();
    $multi->read();
    $multi->write("bar");
  }
  
  function testDeadConnectionsAreSkipped()
  {
    $mock1 =& new FullMockConnection($this);
    $mock1->setReturnValue("start", false);
    $mock1->setReturnValue("isAlive", false);
    $mock1->expectOnce("start");
    $mock1->expectNever("read");
    
    $mock2 =& new FullMockConnection($this);
    $mock2->setReturnValue("start", false);
    $mock2->setReturnValue("isAlive", false);
    $mock2->expectOnce("start");
    $mock2->expectNever("read");
    
    $mock3 =& new FullMockConnection($this);
    $mock3->setReturnValue("start", true);
    $mock3->setReturnValue("isAlive", true);
    $mock3->setReturnValueAt(0, "isAlive", false);
    $mock3->setReturnValueAt(1, "isAlive", true);
    $mock3->expectOnce("start");
    $mock3->expectOnce("read");
    
    $multi =& new Swift_Connection_Rotator();
    $multi->addConnection($mock1);
    $multi->addConnection($mock2);
    $multi->addConnection($mock3);
    
    $multi->start();
    $multi->read();
  }
  
  function testConnectionsRotateBackToBeginningOnceAllUsed()
  {
    $mock1 =& new FullMockConnection($this);
    $mock1->setReturnValue("start", true);
    $mock1->setReturnValue("isAlive", true);
    $mock1->setReturnValueAt(0, "isAlive", false);

    $mock1->expectCallCount("read", 3);
    
    $mock2 =& new FullMockConnection($this);
    $mock2->setReturnValue("start", true);
    $mock2->setReturnValue("isAlive", true);
    $mock2->setReturnValueAt(0, "isAlive", false);
    $mock2->setReturnValueAt(1, "isAlive", true);
    $mock2->expectOnce("read");
    
    $mock3 =& new FullMockConnection($this);
    $mock3->setReturnValue("start", true);
    $mock3->setReturnValue("isAlive", true);
    $mock3->setReturnValueAt(0, "isAlive", false);
    $mock3->setReturnValueAt(1, "isAlive", true);
    $mock3->expectOnce("read");
    
    $multi =& new Swift_Connection_Rotator();
    $multi->addConnection($mock1);
    $multi->addConnection($mock2);
    $multi->addConnection($mock3);
    
    $multi->start();
    $multi->read();
    $multi->nextConnection();
    $multi->read();
    $multi->nextConnection();
    $multi->read();
    $multi->nextConnection();
    $multi->read();
    $multi->read();
  }
  
  function testStartIsOnlyCalledOncePerActiveConnection()
  {
    $mock1 =& new FullMockConnection($this);
    $mock1->setReturnValue("start", true);
    $mock1->setReturnValue("isAlive", true);
    $mock1->setReturnValueAt(0, "isAlive", false);
    $mock1->setReturnValueAt(1, "isAlive", true);
    $mock1->expectOnce("start");
    
    $mock2 =& new FullMockConnection($this);
    $mock2->setReturnValue("start", true);
    $mock2->setReturnValue("isAlive", true);
    $mock2->setReturnValueAt(0, "isAlive", false);
    $mock2->setReturnValueAt(1, "isAlive", true);
    $mock2->expectOnce("start");
    
    $mock3 =& new FullMockConnection($this);
    $mock3->setReturnValue("start", true);
    $mock3->setReturnValue("isAlive", true);
    $mock3->setReturnValueAt(0, "isAlive", false);
    $mock3->setReturnValueAt(1, "isAlive", true);
    $mock3->expectOnce("start");
    
    $multi =& new Swift_Connection_Rotator();
    $multi->addConnection($mock1);
    $multi->addConnection($mock2);
    $multi->addConnection($mock3);
    
    $multi->start();
    for ($i = 0; $i < 15; $i++)
    {
      $multi->nextConnection();
    }
  }
  
  function testDeadConnectionsAreNeverReTried()
  {
    $mock1 =& new FullMockConnection($this);
    $mock1->setReturnValue("start", false);
    $mock1->setReturnValue("isAlive", false);
    $mock1->expectCallCount("isAlive", 2); //Once for first check, twice for check after calling start()
    
    $mock2 =& new FullMockConnection($this);
    $mock2->setReturnValue("start", false);
    $mock2->setReturnValue("isAlive", false);
    $mock2->expectCallCount("isAlive", 2);
    
    $mock3 =& new FullMockConnection($this);
    $mock3->setReturnValue("start", true);
    $mock3->setReturnValue("isAlive", true);
    $mock3->setReturnValueAt(0, "isAlive", false);
    $mock3->setReturnValueAt(1, "isAlive", true);
    $mock3->expectMinimumCallCount("isAlive", 8);
    
    $multi =& new Swift_Connection_Rotator();
    $multi->addConnection($mock1);
    $multi->addConnection($mock2);
    $multi->addConnection($mock3);
    
    $multi->start();
    for ($i = 0; $i < 3; $i++)
    {
      $multi->nextConnection();
    }
  }
  
  function testAllConnectionsAreClosed()
  {
    $mock1 =& new FullMockConnection($this);
    $mock1->setReturnValue("start", true);
    $mock1->setReturnValue("isAlive", true);
    $mock1->setReturnValueAt(0, "isAlive", false);
    $mock1->setReturnValueAt(1, "isAlive", true);
    $mock1->expectOnce("stop");
    
    $mock2 =& new FullMockConnection($this);
    $mock2->setReturnValue("start", true);
    $mock2->setReturnValue("isAlive", true);
    $mock2->setReturnValueAt(0, "isAlive", false);
    $mock2->setReturnValueAt(1, "isAlive", true);
    $mock2->expectOnce("stop");
    
    $mock3 =& new FullMockConnection($this);
    $mock3->setReturnValue("start", true);
    $mock3->setReturnValue("isAlive", true);
    $mock3->setReturnValueAt(0, "isAlive", false);
    $mock3->setReturnValueAt(1, "isAlive", true);
    $mock3->expectOnce("stop");
    
    $multi =& new Swift_Connection_Rotator();
    $multi->addConnection($mock1);
    $multi->addConnection($mock2);
    $multi->addConnection($mock3);
    
    $multi->start();
    for ($i = 0; $i < 10; $i++)
    {
      $multi->nextConnection();
    }
    $multi->stop();
  }
}
