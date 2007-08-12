<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift/Cache/Disk.php';
require_once 'testcases/AbstractTestOfCache.php';
require 'testcases/TestOfDiskCache.php';

$test =& new TestOfDiskCache();
$test->run(new HtmlReporter());
