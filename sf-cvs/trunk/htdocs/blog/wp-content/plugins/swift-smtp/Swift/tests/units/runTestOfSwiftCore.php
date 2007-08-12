<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift.php';
require_once $CONF->SWIFT_LIBRARY_PATH . '/Swift/Log/DefaultLog.php';
require 'stubs/DummyConnection.php';
require 'testcases/TestOfSwiftCore.php';

$test = new TestOfSwiftCore();
$test->run(new HtmlReporter());
