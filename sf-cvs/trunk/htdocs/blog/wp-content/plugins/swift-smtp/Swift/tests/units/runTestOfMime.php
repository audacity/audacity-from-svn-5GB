<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift/Message.php';
require 'stubs/MimeExtension.php';
require 'testcases/TestOfMime.php';

$test = new TestOfMime();
$test->run(new HtmlReporter());
