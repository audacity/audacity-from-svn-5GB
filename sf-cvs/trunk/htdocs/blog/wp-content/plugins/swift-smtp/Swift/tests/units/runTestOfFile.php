<?php

set_time_limit(10);
error_reporting(E_ALL);

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require_once $CONF->SWIFT_LIBRARY_PATH . '/Swift/File.php';

require_once 'testcases/TestOfFile.php';

$test = new TestOfFile();
$test->run(new HtmlReporter());
