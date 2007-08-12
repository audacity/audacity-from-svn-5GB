<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift.php';
require_once 'stubs/DummyConnection.php';
require_once 'stubs/MimeExtension.php';
require 'testcases/TestOfPluginAPI.php';

$test = new TestOfPluginAPI();
$test->run(new HtmlReporter());
