<?php
/*
Plugin Name: ShiftThis.net | Swift SMTP
Plugin URI: http://www.shiftthis.net/wordpress-swift-smtp-plugin/
Description: Send email via SMTP (Compatible with GMAIL)
Author: Marcus Vanstone
Version: 1.0
Author URI: http://www.shiftthis.net


CHANGELOG
June 11, 2007 - Updated to Swift v3.2.5, WordPress MU compatible, Wordpress 2.2 Compatible, removed all PHP shorthand tags
Jan 28, 2007 - Fixed outdated use of $user_level variable with WordPress 2.1 compatible current_user_can() function.
Dec 17, 2006 - Fixed "Cannot redeclare class swift" Error.
Oct 29, 2006 - Fixed compatibility issue with ShiftThis Newsletter Plugin.  Added connection test to Options page.
*/
#---------------------------------------------------

include('st_swift.php');

	st_smtp_check_config(); //Initialize Configuration Variables
	
	add_action('admin_menu', 'st_smtp_add_pages'); //Add page menu links

	if ( isset($_POST['st_smtp_submit_options']) ) 
		add_action('init', 'st_smtp_options_submit'); //Update Options 
		

	// Load Options
	$st_smtp_config = get_option('st_smtp_config');
	
	// Determine plugin filename
	$sts_scriptname = basename(__FILE__);



/*-------------------------------------------------------------
 Name:      st_smtp_add_pages
 Purpose:   Add pages to admin menus
-------------------------------------------------------------*/
function st_smtp_add_pages() {

	global $st_smtp_config;
	add_options_page('SMTP', 'SMTP', 10, __FILE__, 'st_smtp_options_page');
	

}

function st_smtp_options_page() {

	// Make sure we have the freshest copy of the options
	$st_smtp_config = get_option('st_smtp_config');
	global $wpdb, $table_prefix, $php;

	// Default options configuration page
	if ( !isset($_GET['error']) && current_user_can('level_10') ) {
		?>
		<div class="wrap">
		  	<h2>ShiftThis Swift SMTP options</h2>
		  	<form method="post" action="<?php echo $_SERVER['REQUEST_URI'];?>&amp;updated=true">
		    	<input type="hidden" name="st_smtp_submit_options" value="true" />
				<label for="server">Server Address: </label> <input name="server" type="text" size="25" value="<?php echo $st_smtp_config['server'];?>" /><br />
				<label for="username">Username: </label> <input name="username" type="text" size="25" value="<?php echo $st_smtp_config['username'];?>" /><br />
				<label for="password">Password: </label> <input name="password" type="password" size="25" value="<?php echo $st_smtp_config['password'];?>" /><br />
				<label for="ssl">Use SSL or TLS?: </label> <select name="ssl">
					<option value="" <?php if ($st_smtp_config['ssl'] == ''){echo 'selected="selected"';}?>>No</option>
					<option value="ssl" <?php if ($st_smtp_config['ssl'] == 'ssl'){echo 'selected="selected"';}?>>SSL</option>
					<option value="tls" <?php if ($st_smtp_config['ssl'] == 'tls'){echo 'selected="selected"';}?>>TLS (Use for Gmail)</option>
					</select><br />

				<label for="port">Port: </label> <select name="port">
							<option value="25" <?php if ($st_smtp_config['port'] == "25"){echo "selected='selected'";}?>>25 (Default SMTP Port)</option>
							<option value="465" <?php if ($st_smtp_config['port'] == "465"){echo "selected='selected'";}?>>465 (Use for SSL/TLS/GMAIL)</option>
							<option value="custom" <?php if ( ($st_smtp_config['port'] != "465") && ($st_smtp_config['port'] != "25") ){echo "selected='selected'";}?>>Custom Port: (Use Box)</option>
							
					</select>&nbsp;<input name="customport" type="text" size="4" value="<?php if ($st_smtp_config['port'] == "465"){}elseif ($st_smtp_config['port'] == "25"){} else{echo $st_smtp_config['port']; } ?>" />
				
			    <p class="submit" style="text-align:left">
			      	<input type="submit" name="Submit" value="Update Options &raquo;" />
			    </p>
			</form>
			<h2>Test Connection</h2>
			<p>Once you've saved your settings, click the link below to test your connection.</p>
			<form method="post" action="<?php echo $_SERVER['REQUEST_URI'];?>&amp;test=true">
			<label>Send Test Email to this Address:<input type="text" name="testemail" size="25" /> <input type="submit" value="Send Test" /></label><br />
			</form>

			<?php
			if ($_GET['test'] == true){
			$email = $_POST['testemail'];
			$text = "This is a test mail sent using the ShiftThis SMTP Plugin.  If you've received this email it means your connection has been set up properly!  Sweet!";
	if(wp_mail($email, 'ShiftThis SMTP Test', $text)){
		echo '<p><strong>TEST EMAIL SENT - Connection Verified.</strong></p>';
	}
			}
	
			?>
		  <h2>Instructions</h2>
		  <p><strong>Fill in the blanks!</strong> (Gmail users need to use the server 'smtp.gmail.com' with TLS enabled and port 465.)</p>
				
		</div>
		<?php

	} // End If

}



function st_smtp_check_config() {

	if ( !$option = get_option('st_smtp_config') ) {

		// Default Options
		

		update_option('st_smtp_config', $option);

	}


}

function st_smtp_options_submit() {


	if ( current_user_can('level_10') ) {

		//options page
		$option['server'] = $_POST['server'];
		$option['username'] = $_POST['username'];
		$option['password'] = $_POST['password'];
		$option['ssl'] = $_POST['ssl'];
		if ($_POST['port'] != 'custom'){
			$option['port'] = $_POST['port'];
		} else {
			$option['port'] = $_POST['customport'];
		}
		
		update_option('st_smtp_config', $option);

	}

}
?>