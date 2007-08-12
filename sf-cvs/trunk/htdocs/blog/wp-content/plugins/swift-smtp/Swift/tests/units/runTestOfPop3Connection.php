<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift.php';
require $CONF->SWIFT_LIBRARY_PATH . '/Swift/Authenticator/PopB4Smtp/Pop3Connection.php';
require 'testcases/TestOfPop3Connection.php';

$test = new TestOfPop3Connection();
$test->run(new HtmlReporter());
