<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift.php';
require $CONF->SWIFT_LIBRARY_PATH . '/Swift/Connection/Sendmail.php';
require 'testcases/TestOfSendmailConnection.php';

$test = new TestOfSendmailConnection();
$test->run(new HtmlReporter());
