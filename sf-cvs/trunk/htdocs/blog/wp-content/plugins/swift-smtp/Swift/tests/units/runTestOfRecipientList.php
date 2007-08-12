<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift/Errors.php';
require $CONF->SWIFT_LIBRARY_PATH . '/Swift/RecipientList.php';
require 'testcases/TestOfRecipientList.php';

$test = new TestOfRecipientList();
$test->run(new HtmlReporter());
