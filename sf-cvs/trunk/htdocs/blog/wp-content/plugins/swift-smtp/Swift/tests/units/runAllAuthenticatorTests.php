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
Swift_ClassLoader::load("Swift_Authenticator_LOGIN");
Swift_ClassLoader::load("Swift_Authenticator_PLAIN");
Swift_ClassLoader::load("Swift_Authenticator_CRAMMD5");
Swift_ClassLoader::load("Swift_Connection_SMTP");
Swift_ClassLoader::load("Swift_Authenticator_PopB4Smtp_Pop3Connection");
require_once $CONF->SWIFT_LIBRARY_PATH . '/Swift/Authenticator/$PopB4Smtp$.php';

require_once 'stubs/DummyConnection.php';
require_once 'stubs/MimeExtension.php';

require_once 'testcases/AbstractTestOfAuthenticator.php';
require_once 'testcases/AbstractTestWithSend.php';
require_once 'testcases/TestOfAuthenticator.php';
require_once 'testcases/TestOfLOGINAuthenticator.php';
require_once 'testcases/TestOfPLAINAuthenticator.php';
require_once 'testcases/TestOfCRAMMD5Authenticator.php';
require_once 'testcases/TestOfPop3Connection.php';
require_once 'testcases/TestOfPopB4SmtpAuthenticator.php';

$test =& new GroupTest("All Swift Authenticator Tests");
$test->addTestCase(new TestOfAuthenticator());
$test->addTestCase(new TestOfLOGINAuthenticator());
$test->addTestCase(new TestOfPLAINAuthenticator());
$test->addTestCase(new TestOfCRAMMD5Authenticator());
$test->addTestCase(new TestOfPop3Connection());
$test->addTestCase(new TestOfPopB4SmtpAuthenticator());
$test->run(new HtmlReporter());
