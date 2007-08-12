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
Swift_ClassLoader::load("Swift_Message_Encoder");
Swift_ClassLoader::load("Swift_Message_Headers");
Swift_ClassLoader::load("Swift_Message_Mime");
Swift_ClassLoader::load("Swift_Message_Part");
Swift_ClassLoader::load("Swift_Message_Attachment");
Swift_ClassLoader::load("Swift_Message_EmbeddedFile");
Swift_ClassLoader::load("Swift_Message");

require_once 'stubs/MimeExtension.php';

require_once 'testcases/AbstractTestWithSend.php';
require_once 'testcases/TestOfEncoder.php';
require_once 'testcases/TestOfHeaders.php';
require_once 'testcases/TestOfFile.php';
require_once 'testcases/TestOfMime.php';
require_once 'testcases/TestOfMimePart.php';
require_once 'testcases/TestOfAttachment.php';
require_once 'testcases/TestOfEmbeddedFile.php';
require_once 'testcases/TestOfImage.php';
require_once 'testcases/TestOfMessage.php';

$test =& new GroupTest("All Swift MIME Tests");
$test->addTestCase(new TestOfEncoder());
$test->addTestCase(new TestOfHeaders());
$test->addTestCase(new TestOfFile());
$test->addTestCase(new TestOfMime());
$test->addTestCase(new TestOfMimePart());
$test->addTestCase(new TestOfAttachment());
$test->addTestCase(new TestOfEmbeddedFile());
$test->addTestCase(new TestOfImage());
$test->addTestCase(new TestOfMessage());
$test->run(new HtmlReporter());
