<?php

set_time_limit(10);
//I'd use E_STRICT but SimpleTest pukes on it.
// The smoke tests run under E_STRICT however
error_reporting(E_ALL);

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require_once $CONF->SWIFT_LIBRARY_PATH . '/Swift.php';
Swift_ClassLoader::load("Swift_Log_DefaultLog");
Swift_ClassLoader::load("Swift_Connection");
Swift_ClassLoader::load("Swift_Cache_Memory");
Swift_ClassLoader::load("Swift_Cache_Disk");
Swift_ClassLoader::load("Swift_Message_Headers");
Swift_ClassLoader::load("Swift_Message_Part");
Swift_ClassLoader::load("Swift_Authenticator_PLAIN");
Swift_ClassLoader::load("Swift_Connection_SMTP");
Swift_ClassLoader::load("EasySwift");

require_once 'stubs/DummyConnection.php';
require_once 'stubs/MimeExtension.php';

require_once 'testcases/AbstractTestOfCache.php';
require_once 'testcases/AbstractTestWithSend.php';
require_once 'testcases/TestOfMemoryCache.php';
require_once 'testcases/TestOfDiskCache.php';
require_once 'testcases/TestOfAddress.php';
require_once 'testcases/TestOfArrayIterator.php';
require_once 'testcases/TestOfRecipientList.php';
require_once 'testcases/TestOfDefaultLog.php';
require_once 'testcases/TestOfSwiftCore.php';
require_once 'testcases/TestOfPluginAPI.php';
require_once 'testcases/TestOfEasySwift.php';

$test =& new GroupTest("All Swift Core Tests");
$test->addTestCase(new TestOfMemoryCache());
$test->addTestCase(new TestOfDiskCache());
$test->addTestCase(new TestOfAddress());
$test->addTestCase(new TestOfArrayIterator());
$test->addTestCase(new TestOfRecipientList());
$test->addTestCase(new TestOfDefaultLog());
$test->addTestCase(new TestOfSwiftCore());
$test->addTestCase(new TestOfPluginAPI());
$test->addTestCase(new TestOfEasySwift());
$test->run(new HtmlReporter());
