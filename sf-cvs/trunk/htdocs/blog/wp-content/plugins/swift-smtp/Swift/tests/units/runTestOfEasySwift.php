<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require_once $CONF->SWIFT_LIBRARY_PATH . '/EasySwift.php';
require_once $CONF->SWIFT_LIBRARY_PATH . '/Swift/Connection/SMTP.php';
require_once $CONF->SWIFT_LIBRARY_PATH . '/Swift/Authenticator/PLAIN.php';
require_once $CONF->SWIFT_LIBRARY_PATH . '/Swift/Message/Headers.php';
require 'stubs/DummyConnection.php';
require_once 'testcases/AbstractTestWithSend.php';
require 'testcases/TestOfEasySwift.php';

$test = new TestOfEasySwift();
$test->run(new HtmlReporter());
