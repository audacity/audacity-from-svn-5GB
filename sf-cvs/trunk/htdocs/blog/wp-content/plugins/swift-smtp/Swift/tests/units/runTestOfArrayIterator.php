<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift/Iterator/Array.php';
require 'testcases/TestOfArrayIterator.php';

$test =& new TestOfArrayIterator();
$test->run(new HtmlReporter());
