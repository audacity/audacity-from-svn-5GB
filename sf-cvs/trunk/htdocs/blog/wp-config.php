<?php
// ** MySQL settings ** //
define('DB_NAME', 'a6235_main');    // The name of the database
define('DB_USER', 'a6235_admin');     // Your MySQL username
define('DB_PASSWORD', 'n9ine0Klok'); // ...and password
define('DB_HOST', 'mysql4-a.sourceforge.net');    // 99% chance you won't need to change this value
define('DB_CHARSET', 'utf8');
define('DB_COLLATE', '');

// You can have multiple installations in one database if you give each a unique prefix
$table_prefix  = 'wp_';   // Only numbers, letters, and underscores please!

// Change this to localize WordPress.  A corresponding MO file for the
// chosen language must be installed to wp-content/languages.
// For example, install de.mo to wp-content/languages and set WPLANG to 'de'
// to enable German language support.
define ('WPLANG', '');

/* That's all, stop editing! Happy blogging. */

define('ABSPATH', dirname(__FILE__).'/');
require_once(ABSPATH.'wp-settings.php');
?>
