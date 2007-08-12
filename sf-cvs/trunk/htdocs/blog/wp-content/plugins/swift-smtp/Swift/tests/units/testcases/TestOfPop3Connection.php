<?php

class TestOfPop3Connection extends UnitTestCase
{
  function setUp()
  {
    Swift_Errors::reset();
  }
  
  function testAssertOkThrowsExceptionOnBadResponse()
  {
    $conn =& new Swift_Authenticator_PopB4Smtp_Pop3Connection("host");
    $conn->assertOk("+OK Foo");
    $conn->assertOk("bad response");
    $this->assertError();
  }
}
