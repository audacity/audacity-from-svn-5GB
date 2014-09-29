<?php
  function download_url($filename) {
    $url_array =
      array("http://www.fosshub.com/Audacity.html/",
            "http://sourceforge.net/projects/audacity/files/");
    $url = $url_array[mt_rand() % count($url_array)];
    return $url . $filename;
  }
?>
