<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift/Exception.php';
require $CONF->SWIFT_LIBRARY_PATH . '/Swift/Errors.php';
require 'testcases/TestOfErrors.php';

$test = new TestOfErrors();
$test->run(new HtmlReporter());
