<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require_once $CONF->SWIFT_LIBRARY_PATH . '/Swift/Errors.php';
require $CONF->SWIFT_LIBRARY_PATH . '/Swift/FileNameMaker.php';
require 'testcases/TestOfFileNameMaker.php';

$test = new TestOfFileNameMaker();
$test->run(new HtmlReporter());
