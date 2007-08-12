<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift.php';
require $CONF->SWIFT_LIBRARY_PATH . '/Swift/Connection/Rotator.php';
require_once 'stubs/DummyConnection.php';
require 'testcases/TestOfRotatorConnection.php';

$test = new TestOfRotatorConnection();
$test->run(new HtmlReporter());
