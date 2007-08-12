<?php

require '../TestConfiguration.php';
require $CONF->SIMPLETEST_PATH . '/unit_tester.php';
require $CONF->SIMPLETEST_PATH . '/mock_objects.php';
require $CONF->SIMPLETEST_PATH . '/reporter.php';

require $CONF->SWIFT_LIBRARY_PATH . '/Swift.php';
require $CONF->SWIFT_LIBRARY_PATH . '/Swift/Connection/SMTP.php';
require_once $CONF->SWIFT_LIBRARY_PATH . '/Swift/Authenticator/$PopB4Smtp$.php';
require_once $CONF->SWIFT_LIBRARY_PATH . '/Swift/Authenticator/PopB4Smtp/Pop3Connection.php';
require 'testcases/TestOfPopB4SmtpAuthenticator.php';

$test = new TestOfPopB4SmtpAuthenticator();
$test->run(new HtmlReporter());
