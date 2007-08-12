<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift/Cache/Memory.php';
require_once 'testcases/AbstractTestOfCache.php';
require 'testcases/TestOfMemoryCache.php';

$test =& new TestOfMemoryCache();
$test->run(new HtmlReporter());
