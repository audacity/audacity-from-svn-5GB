=== ShiftThis | Swift SMTP ===
Contributors: shiftthis
Donate link: https://www.paypal.com/cgi-bin/webscr?cmd=_xclick&business=paypal%40shiftthis%2enet&item_name=WordPress%20Plugin%20Development%20Donation&no_shipping=0&return=http%3a%2f%2fwww%2eshiftthis%2enet%2f&no_note=1&tax=0&currency_code=CAD&lc=CA&bn=PP%2dDonationsBF&charset=UTF%2d8&notify_url=http%3a%2f%2fwww%2eshiftthis%2enet%2f
Tags: mail, email, smtp, gmail
Requires at least: 2.0.2
Tested up to: 2.2
Stable tag: 1.0

Use an SMTP server for sending email from your blog. Includes support for Gmail's SMTP.

== Description ==

Use an SMTP server for sending all email from your blog. Includes support for Gmail's SMTP. The plugin uses the robust Swift Mailer for all SMTP handling.

== Installation ==


1. Upload the 'swift-smtp' folder to the 'wp-content\plugins' directory
2. Activate the plugin titled 'ShiftThis | Swift SMTP' through the 'Plugins' menu in WordPress
3. Access the **Options** Tab in your Admin Panel and select the **SMTP** SubMenu Item.

== SMTP Options ==

* **Server Address** - set to your email SMTP server setting (smtp.yourhost.com).
* **Username** - set to the username used to login to your SMTP account.
* **Password** - set to the password used to login to your SMTP account.
* **Use TLS or SSL?** - If your server requires TLS or SSL select the appropriate setting, otherwise set to No.
* **Port** - Select 25 for a default port setting, if you use TLS or SSL then choose the 465, otherwise choose Custom Port and enter your port number in the input box to the right.

Once you've set your options and clicked the **Update Options** button, you can use the **Test Connection** area to send yourself a test email using your SMTP settings. If everything is working, you should see the message **TEST EMAIL SENT - Connection Verified**. and of course receive the test email in your account.


== Frequently Asked Questions ==

= I've installed a plugin that sends email and does not seem to be working correctly with the Swift SMTP Plugin =

1.Open the problem Plugin in a text editor.
2.Do a search for the function `mail(` and replace it with `wp_mail(`.
3.Save and upload the revised plugin and it should now use the SMTP setting for sending mail.

= Where can I ask support questions for this plugin? =

[support.ShiftThis.net](http://support.shiftthis.net)

== Screenshots ==
1. The SMTP Options Page.
