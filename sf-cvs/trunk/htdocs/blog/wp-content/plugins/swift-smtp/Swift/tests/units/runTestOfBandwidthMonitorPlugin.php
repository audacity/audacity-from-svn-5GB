<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift.php';
require_once $CONF->SWIFT_LIBRARY_PATH . '/Swift/Plugin/BandwidthMonitor.php';
require_once 'stubs/DummyConnection.php';
require 'testcases/TestOfBandwidthMonitorPlugin.php';

$test =& new TestOfBandwidthMonitorPlugin();
$test->run(new HtmlReporter());
