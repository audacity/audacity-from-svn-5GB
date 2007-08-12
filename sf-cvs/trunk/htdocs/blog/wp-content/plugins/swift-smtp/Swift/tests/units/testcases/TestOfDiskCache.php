<?php

/**
 * Swift Mailer Disk Cache Test Case.
 * @package Swift
 * @subpackage Tests
 * @author Chris Corbyn <chris@w3style.co.uk>
 */
class TestOfDiskCache extends AbstractTestOfCache
{
  /**
   * Clean out any files already in the cache dir.
   */
  function setUp()
  {
    $list = glob($GLOBALS["CONF"]->WRITABLE_PATH . "/*");
    foreach ((array)$list as $f)
    {
      if (!is_dir($f)) @unlink($f);
    }
  }
  
  function &getCache()
  {
    Swift_Cache_Disk::setSavePath($GLOBALS["CONF"]->WRITABLE_PATH);
    $cache =& new Swift_Cache_Disk();
    return $cache;
  }
}
