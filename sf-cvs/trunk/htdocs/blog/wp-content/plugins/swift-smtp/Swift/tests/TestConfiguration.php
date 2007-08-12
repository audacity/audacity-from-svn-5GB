<?php

define("TEST_CONFIG_PATH", dirname(__FILE__));
define("DEFAULT_WRITABLE_PATH", TEST_CONFIG_PATH . "/tmp");
define("DEFAULT_LIBRARY_PATH", TEST_CONFIG_PATH . "/../lib");

/**
 * Adjust the values contained inside this class in order to run the tests
 * NOTE: SimpleTest is NOT provided with Swift.  You must download this from SouceForge yourself.
 * Paths given should be either relative to the "tests/units" directory or absolute.
 * @package Swift_Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */
class TestConfiguration
{
  /**
   * Somewhere to write to when testing disk cache
   */
  var $WRITABLE_PATH = DEFAULT_WRITABLE_PATH;
  /**
   * The location of SimpleTest (Unit Test Tool)
   */
  var $SIMPLETEST_PATH = "/Users/d11wtq/simpletest";
  /**
   * The location of the Swift library directory
   */
  var $SWIFT_LIBRARY_PATH = DEFAULT_LIBRARY_PATH;
  
  /*
   * EVERYTHING BELOW IS FOR SMOKE TESTING ONLY
   */
   
  /**
   * The connection tye to use in testing
   * "smtp", "sendmail" or "nativemail"
   */
  var $CONNECTION_TYPE = "smtp";
  /**
   * An address to send emails from
   */
  var $FROM_ADDRESS = "chris@w3style.co.uk";
  /**
   * The name of the sender
   */
  var $FROM_NAME = "Chris Corbyn - from";
  /**
   * An address to send emails to
   */
  var $TO_ADDRESS = "chris@w3style.co.uk";
  /**
   * The name of the recipient
   */
  var $TO_NAME = "Chris Corbyn - to";
  
  /*
   * SMTP SETTINGS - IF APPLICABLE
   */
   
  /**
   * The FQDN of the host
   */
  var $SMTP_HOST = "smtp.swiftmailer.org";
  /**
   * The remote port of the SMTP server
   */
  var $SMTP_PORT = 25;
  /**
   * Encryption to use if any
   * "ssl", "tls" or false
   */
  var $SMTP_ENCRYPTION = false;
  /**
   * A username for SMTP, if any
   */
  var $SMTP_USER = false;
  /**
   * Password for SMTP, if any
   */
  var $SMTP_PASS = false;
  
  /*
   * SENDMAIL BINARY SETTINGS - IF APPLICABLE
   */
  
  /**
   * The path to sendmail, including the -bs options
   */
  var $SENDMAIL_PATH = "/usr/sbin/sendmail -bs";
}

$GLOBALS["CONF"] = new TestConfiguration();
