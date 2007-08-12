<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift.php';
require_once $CONF->SWIFT_LIBRARY_PATH . '/Swift/Connection/Rotator.php';
require_once $CONF->SWIFT_LIBRARY_PATH . '/Swift/Plugin/ConnectionRotator.php';
require_once 'stubs/DummyConnection.php';
require_once 'testcases/AbstractTestWithSend.php';
require 'testcases/TestOfConnectionRotatorPlugin.php';

$test = new TestOfConnectionRotatorPlugin();
$test->run(new HtmlReporter());
