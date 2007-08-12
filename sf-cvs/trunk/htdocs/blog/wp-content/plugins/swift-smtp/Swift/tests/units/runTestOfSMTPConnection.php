<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift.php';
require $CONF->SWIFT_LIBRARY_PATH . '/Swift/Connection/SMTP.php';
require_once $CONF->SWIFT_LIBRARY_PATH . '/Swift/Authenticator.php';
require 'testcases/TestOfSMTPConnection.php';

$test = new TestOfSMTPConnection();
$test->run(new HtmlReporter());
