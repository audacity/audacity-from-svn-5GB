<?php
//
// lang.inc.php
//
// This script simply guesses the language of the remote host and
// stores it in the variable phpLang_current.  The language can
// be set explicitly by passing ?lang=XX in the URL.
//
// This code is a simplification of phpLang version 0.5.0, with
// the license and copyright below.  The code was modified primarily
// so that it doesn't try to include any special files; it just
// figures out the language and returns.  The code that prints the
// HTML code for the language flags is used too.
//
// - Dominic Mazzoni
//
// +--------------------------------------------------------------------------+
// | phpLang version 0.5.0 - 2002/02/23                                       |
// +--------------------------------------------------------------------------+
// | Copyright (c) 2000-2001 The phpHeaven-team                               |
// +--------------------------------------------------------------------------+
// | License:  GNU/GPL - http://www.gnu.org/copyleft/gpl.html                 |
// +--------------------------------------------------------------------------+
// | It is a simple script that enables you to simply have different          |
// | languages on you web site.                                               |
// |                                                                          |
// | usage:    read attached documentation                                    |
// +--------------------------------------------------------------------------+
// | Last release available on phpHeaven:                                     |
// |    http://www.phpheaven.net/projects/phpLang/                            |
// |                                                                          |
// | Authors:  Nicolas Hoizey <nhoizey@phpheaven.net>                         |
// |           Loïc Chapeaux <lolo@phpheaven.net>                             |
// +--------------------------------------------------------------------------+

// path to image files
if(!defined('phpLang_images'))
 define('phpLang_images', './flags/');

// path to translated files
if(!defined('phpLang_localDir'))
 define('phpLang_localDir', './');

// parameter to add in url
if(!defined('phpLang_urlParam'))
 define('phpLang_urlParam', 'lang');

// list of available languages, order it as you need
$phpLang_languages = array(
 "en([-_][[:alpha:]]{2})?|english"  => array('en', 'english'),
 "fr([-_][[:alpha:]]{2})?|french"  => array('fr', 'french'),
 "ca|catalan"   => array('ca', 'catalan'),
 "cs|czech"   => array('cs', 'czech'),
 "da|danish"   => array('da', 'danish'),
 "nl([-_][[:alpha:]]{2})?|dutch"   => array('nl', 'dutch'),
 "de([-_][[:alpha:]]{2})?|german"  => array('de', 'german'),
 "fi|finnish"  => array('fi', 'finnish'),
 "mk|macedonian" => array('mk', 'macedonian'),
 "hu|hungarian"  => array('hu', 'hungarian'),
 "is|icelandic"  => array('is', 'icelandic'),
 "it|italian"  => array('it', 'italian'),
 "ja|japanese"  => array('ja', 'japanese'),
 "no|norwegian"  => array('no', 'norwegian'),
 "pl|polish"   => array('pl', 'polish'),
 "ru|russian"  => array('ru', 'russian'),
 "sk|slovak"   => array('sk', 'slovak'),
 "sv|swedish"   => array('sv', 'swedish'),
 "si|slovenian" => array('si', 'slovenian'),
 "es([-_][[:alpha:]]{2})?|spanish"  => array('es', 'spanish'),
 "th|thai"   => array('th', 'thai'),
 "pt[-_]br"   => array('pt-br', 'brazilian portuguese'),
 "pt([-_][[:alpha:]]{2})?|portuguese" => array('pt', 'portuguese'),
 "uk([-_][[:alpha:]]{2})?|ukrainian"  => array('ua', 'ukrainian'),
 "zh[-_]tw"   => array('zh-tw', 'chinese_traditional'),
 "zh([-_][[:alpha:]]{2})?|chinese"  => array('zh', 'chinese_simplified'),
 "bg|bulgarian"  => array('bg', 'bulgarian'),

);

// finds current file name, extension and uri
if(ereg("([^/?]+)(\?.*)?$", $SCRIPT_NAME, $regs)) {
 define('phpLang_currentFile', $regs[1]);
 if(ereg("(.*)(\.[^.]+)$", phpLang_currentFile, $regs2)) {
  define('phpLang_currentFileName', $regs2[1]);
  define('phpLang_currentFileExtension', $regs2[2]);
 } else {
  define('phpLang_currentFileName', phpLang_currentFile);
 }
 $uri = ereg_replace("[?&]".phpLang_urlParam."=[^&]*", "", $regs[0]);
 $uri .= ereg("\?", $uri) ? '&' : '?';
 define('phpLang_currentURI', $uri);
} else {
 // it should not be possible
 define('phpLang_currentFile', '');
 define('phpLang_currentURI', '');
 define('phpLang_currentFileName', '');
 define('phpLang_currentFileExtension', '');
}

$HTTP_ACCEPT_LANGUAGE = getenv('HTTP_ACCEPT_LANGUAGE');
$HTTP_USER_AGENT = getenv('HTTP_USER_AGENT');

// language code detection
function phpLang_detectLanguage($str, $from)
{
 $ext = '';
 reset($GLOBALS['phpLang_languages']);
 while($ext == '' && list($key, $name) = each($GLOBALS['phpLang_languages'])) {
  if (($from == 1 && eregi("^".$key."$",$str)) || ($from == 2 && eregi("(\(|\[|;[[:space:]])".$key."(;|\]|\))",$str))) {
   $ext = $name[0];
  }
 }

 return $ext;
}

// If a valid language was passed in the command line, use that
if (isset($HTTP_GET_VARS[phpLang_urlParam]) && file_exists($HTTP_GET_VARS[phpLang_urlParam])) {
 // a language as been chosen by the user
 define('phpLang_current', $HTTP_GET_VARS[phpLang_urlParam]);
 // defines a string to add at the end of each link
 define('phpLang_link', phpLang_urlParam.'='.phpLang_current);
 // defines a url query string
 define('phpLang_query', '?'.phpLang_urlParam.'='.phpLang_current);
}
else {
 define('phpLang_link', 'lang=');
 define('phpLang_query', '');
}

// Otherwise, see if the HTTP headers indicate which languages the
// browser accepts
if (!defined('phpLang_current') && isset($HTTP_ACCEPT_LANGUAGE) && trim($HTTP_ACCEPT_LANGUAGE) != '') {
 $accepted = explode(',', $HTTP_ACCEPT_LANGUAGE);
 while(!defined('phpLang_current') && list($key, $name) = each($accepted)) {
  $code = explode(';', $name);
  $ext = phpLang_detectLanguage($code[0], 1);
  if(file_exists($ext)) {
   define('phpLang_current', $ext);
  }
 }
}

// Finally, try guessing the language from the browser's identification
// string
if (!defined('phpLang_current') && isset($HTTP_USER_AGENT) && trim($HTTP_USER_AGENT) != '') {
 $ext = phpLang_detectLanguage($HTTP_USER_AGENT, 2);
 if(file_exists($ext)) {
  define('phpLang_current', $ext);
 }
}

// if no language yet found, chose the first existing in site's list
if(!defined('phpLang_current')) {
 reset($phpLang_languages);
 while(!defined('phpLang_current') && list($key, $name) = each($phpLang_languages)) {
  if(file_exists($name[0])) {
   define('phpLang_current', $name[0]);
  }
 }
}

// function that adds the flags with links for existing files
// give as first parameter the HTML string to put between each flag
function AddFlags($between = "", $betw2 = "", $showCurrent = false)
{
 reset($GLOBALS["phpLang_languages"]);
 $temp = "";
 $count=0;
 while(list($key, $name) = each($GLOBALS["phpLang_languages"])) {
  if(file_exists($name[0]) && ($showCurrent || $name[0] != phpLang_current)) {
   echo($temp.'<a href="'.phpLang_currentURI.phpLang_urlParam.'='.$name[0].'">');
   echo('<img src="'.phpLang_images.$name[0].'.gif" border="0" align="middle" width="24" height="16" alt="'.$name[1].'" />');
   echo('</a>');
   if (($count & 1) == 1) {
     $temp = $betw2;
   }
   else {
     $temp = $between;
   }
	$count++;
  }
 }
}

// Now, you can use these constants in your scripts :
//   phpLang_current : current language code
//   phpLang_link    : add this in the links after a '?' or a '&'
//   phpLang_query   : add this immediately after the url

?>
