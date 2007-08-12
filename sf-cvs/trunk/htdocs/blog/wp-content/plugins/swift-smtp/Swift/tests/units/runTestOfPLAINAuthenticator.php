<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift.php';
require $CONF->SWIFT_LIBRARY_PATH . '/Swift/Connection/SMTP.php';
require $CONF->SWIFT_LIBRARY_PATH . '/Swift/Authenticator/PLAIN.php';
require_once 'testcases/AbstractTestOfAuthenticator.php';
require 'testcases/TestOfPLAINAuthenticator.php';

$test = new TestOfPLAINAuthenticator();
$test->run(new HtmlReporter());
