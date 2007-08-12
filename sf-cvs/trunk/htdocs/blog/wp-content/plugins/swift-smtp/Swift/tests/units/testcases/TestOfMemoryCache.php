<?php

/**
 * Swift Mailer Test Case for Memory Cache.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */
class TestOfMemoryCache extends AbstractTestOfCache
{
  function &getCache()
  {
    $cache =& new Swift_Cache_Memory();
    return $cache;
  }
}
