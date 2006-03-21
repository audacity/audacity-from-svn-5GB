<?php
/*
 * Copyright 2004 Matt Brubeck
 * This file is licensed under a Creative Commons license:
 * http://creativecommons.org/licenses/by/2.0/
 */
?>
</div>
<div id="footer">
  <hr>
  <h2><?=_("Page Footer")?></h2>
  <p>
    <a href="<?=$sitePath?>/contact/privacy"><?=_("Privacy")?></a>
    | <a href="<?=$sitePath?>/copyright"><?=_("Copyright")?></a>
    | <a href="<?=$sitePath?>/site-map"><?=_("Site Map")?></a>
  </p>
  <p><a title="<?=_("Creative Commons Attribution license")?>" href="http://creativecommons.org/licenses/by/2.0/"><img alt="<?=_("Creative Commons: Some rights reserved.")?>" src="<?=$sitePath?>/images/creative-commons.gif"></a>
  <a title="<?=_("Development hosted by SourceForge")?>" href="http://sourceforge.net/projects/audacity/"><img alt="SourceForge.net" src="http://sflogo.sourceforge.net/sflogo.php?group_id=6235&amp;type=1"></a></p>
  <h2><?=_("View this page in a different language")?></h2>
  <p>
    <?php
      $query = $_SERVER["QUERY_STRING"];
      $query = ereg_replace("&?lang=[^&]*", "", $query);
      if ($query != "")
        $query = $query."&";
      $query = htmlspecialchars($query);
      
      $temp_first_item = true;
      foreach ($available_locales as $locale_lang => $i) {
        $locale_name = $i[1];
        if (!$temp_first_item)
          echo " | ";
        $temp_first_item = false;
        if ($locale_lang == $lang)
          echo "$locale_name";
        else
          echo "<a lang=\"$locale_lang\" href=\"?{$query}lang=$locale_lang\">$locale_name</a>";
      }
    ?>
  </p>
</div>
<!-- License metadata -->
<!--
<rdf:RDF xmlns="http://web.resource.org/cc/" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
  <Work rdf:about="">
     <license rdf:resource="http://creativecommons.org/licenses/by/2.0/" />
  </Work>
  <License rdf:about="http://creativecommons.org/licenses/by/2.0/">
     <permits rdf:resource="http://web.resource.org/cc/Reproduction" />
     <permits rdf:resource="http://web.resource.org/cc/Distribution" />
     <requires rdf:resource="http://web.resource.org/cc/Notice" />
     <requires rdf:resource="http://web.resource.org/cc/Attribution" />
     <permits rdf:resource="http://web.resource.org/cc/DerivativeWorks" />
  </License>
</rdf:RDF>
-->

<script src="http://www.google-analytics.com/urchin.js" type="text/javascript">
</script>
<script type="text/javascript">
_uacct = "UA-230676-1";
urchinTracker();
</script>

</body></html>
