<?php

class MyException extends Swift_Exception {}

class TestOfErrors extends UnitTestCase
{
  function setUp()
  {
    Swift_Errors::reset();
  }
  
  function testErrorIsTriggeredIfItNotCaught()
  {
    $ex =& new Swift_Exception("Foo");
    Swift_Errors::trigger($ex);
    $this->assertError(); //SimpleTest's of course
    
    $ex =& new Swift_Exception("BAR");
    Swift_Errors::trigger($ex);
    $this->assertError(); //SimpleTest's of course
  }
  
  function testErrorCanBeCaught()
  {
    $ex =& new Swift_Exception("foo");
    Swift_Errors::expect($e);
    Swift_Errors::trigger($ex);
    $this->assertEqual($e, $ex);
    
    $ex =& new Swift_Exception("bar");
    Swift_Errors::expect($e);
    Swift_Errors::trigger($ex);
    $this->assertEqual($e, $ex);
  }
  
  function testOnlyOneErrorCaught()
  {
    $ex =& new Swift_Exception("foo");
    Swift_Errors::expect($e);
    Swift_Errors::trigger($ex);
    $this->assertEqual($e, $ex);
    
    
    $ex =& new Swift_Exception("bar");
    Swift_Errors::trigger($ex);
    $this->assertError();
  }
  
  function testErrorTypeCanBeSpecified()
  {
    $ex =& new MyException("test");
    Swift_Errors::trigger($ex);
    $this->assertError();
    
    $ex =& new MyException("test2");
    Swift_Errors::expect($e, "MyException");
    Swift_Errors::trigger($ex);
    $this->assertEqual($e, $ex);
  }
  
  function testErrorTypeKnowsAboutInheritance()
  {
    $ex =& new MyException("test2");
    Swift_Errors::expect($e, "MyException");
    Swift_Errors::trigger($ex);
    $this->assertEqual($e, $ex);
    
    $ex =& new MyException("test2");
    Swift_Errors::expect($e, "Swift_Exception");
    Swift_Errors::trigger($ex);
    $this->assertEqual($e, $ex);
  }
  
  function testCatchStatementsCanBeEndedWithoutThrowingException()
  {
    $ex =& new MyException("test2");
    Swift_Errors::expect($e, "MyException");
    //Swift_Errors::trigger($ex); let's not throw this one!
    Swift_Errors::clear("MyException");
    $ex =& new MyException("test3");
    Swift_Errors::trigger($ex);
    $this->assertError();
  }
  
  function testCatchStatementsCanBeNested()
  {
    $ex1 =& new MyException("ex1");
    $ex2 =& new MyException("ex2");
    Swift_Errors::expect($e1, "MyException");
    Swift_Errors::expect($e2, "MyException");
    Swift_Errors::trigger($ex1);
    Swift_Errors::trigger($ex2);
    $this->assertEqual($e1, $ex2);
    $this->assertEqual($e2, $ex1);
  }
  
  function testRealCaseScenario()
  {
    Swift_Errors::expect($e, "MyException");
    if (true) {
      Swift_Errors::trigger(new MyException("foo"));
    } else {
      Swift_Errors::clear("MyException");
    }
    $this->assertIsA($e, "MyException");
    $this->assertEqual("foo", $e->getMessage());
    
    Swift_Errors::expect($e, "MyException");
    if (false) {
      Swift_Errors::trigger(new MyException("foo"));
    } else {
      Swift_Errors::clear("MyException");
    }
    $this->assertNull($e);
    
    if (true) {
      Swift_Errors::trigger(new Swift_Exception("bar"));
    } else {
      Swift_Errors::clear("MyException");
    }
    $this->assertError();
  }
}
