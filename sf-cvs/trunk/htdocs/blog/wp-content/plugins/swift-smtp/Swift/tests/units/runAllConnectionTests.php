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
Swift_ClassLoader::load("Swift_Connection");
Swift_ClassLoader::load("Swift_Authenticator");
Swift_ClassLoader::load("Swift_Connection_SMTP");
Swift_ClassLoader::load("Swift_Connection_Sendmail");
Swift_ClassLoader::load("Swift_Connection_Multi");
Swift_ClassLoader::load("Swift_Connection_Rotator");
Swift_ClassLoader::load("Swift_Connection_NativeMail");
Swift_ClassLoader::load("Swift_Plugin_ConnectionRotator");
Swift_ClassLoader::load("Swift_Plugin_MailSend");

require_once 'stubs/DummyConnection.php';

require_once 'testcases/AbstractTestWithSend.php';
require_once 'testcases/TestOfSMTPConnection.php';
require_once 'testcases/TestOfSendmailConnection.php';
require_once 'testcases/TestOfMultiConnection.php';
require_once 'testcases/TestOfRotatorConnection.php';
require_once 'testcases/TestOfNativeMailConnection.php';
require_once 'testcases/TestOfConnectionRotatorPlugin.php';
require_once 'testcases/TestOfMailSendPlugin.php';

$test =& new GroupTest("All Swift Connection Tests");
$test->addTestCase(new TestOfSMTPConnection());
$test->addTestCase(new TestOfSendmailConnection());
$test->addTestCase(new TestOfMultiConnection());
$test->addTestCase(new TestOfRotatorConnection());
$test->addTestCase(new TestOfNativeMailConnection());
$test->addTestCase(new TestOfConnectionRotatorPlugin());
$test->addTestCase(new TestOfMailSendPlugin());
$test->run(new HtmlReporter());
