<?php BoxTop("CVS"); ?>

We use <a href="http://www.cvshome.org">CVS</a>, the Concurrent Versions
System, to help the us
develop Audacity collaboratively. Click
<a
href="http://cvs.sourceforge.net/cgi-bin/viewcvs.cgi/audacity/audacity-src/"
>here</a> to browse the source code in our CVS repository.

<p>
<h3>Note: Version 1.2 is now branched:</h3>
If you want the stable branch of Audacity, version 1.2.x, you now need to
type <tt>-r AUDACITY_1_2</tt> when you do a cvs update or checkout.
Otherwise you will get the CVS HEAD, which may become quite unstable
now as we start working on version 1.3.0.

<p><h3>Quick Audacity CVS Howto:</h3>
  If you want to access the Audacity source code, you can
  use a cvs client to download a cvs branch onto your
  computer. Once you have checked out a branch once, your CVS
  client software will be able to help you keep your version
  updated with the other Audacity developers. Follow the
  instructions below to get the source code.


<h4>Anonymous CVS access with a command-line cvs client:</h4>
<p>Enter the following on the command line (note that it
is a single line with no carriage returns):<br>
<ul><tt>cvs -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity
login </tt><br></ul>
and hit the enter key when it asks for a password.

<p>Then, <b>to get the latest cutting-edge code (1.3.0)</b> (as a single line):
<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity co
audacity</tt></ul>
or <b>for the stable branch (1.2.0)</b> (as a single line):<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity co -r AUDACITY_1_2 audacity</tt></ul>
or <b>for the old 1.0 branch (1.0.0)</b> (as a single line):<br>
   <ul><tt>cvs -z3 -d:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity co -r audacity-0_9-branch audacity-old</tt></ul>

<p> Alternately, you can set your <tt>CVSROOT</tt> environment
variable to
<tt>:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt>
(see below).<br>
Then, <b>to get the latest unstable branch (1.3.0)</b>, enter<br> 
   <ul><tt>cvs checkout audacity</tt></ul>
Or, <b>for the stable branch (1.2)</b>, enter <br>
   <ul><tt>cvs checkout -r AUDACITY_1_2 audacity</tt></ul>
or <b>for the old 1.0 branch</b>, enter <br>
   <ul><tt>cvs checkout -r audacity-0_9-branch audacity-old</tt></ul>

<p> To set the  <tt>CVSROOT</tt> environment variable, you
can use your command shell's resource file, or one of the following
commands:
<h5>In bash or bourne shell, as one line:</h5>
<ul><tt>export
CVSROOT=:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt><br></ul>
<h5>In csh or its descendents, as one line:</h5>
<ul><tt>setenv CVSROOT
:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt><br></ul>

<hr width="80%">

<h4>Anonymous CVS access with a graphical client</h4>

For a graphical client like wincvs, maccvs, or gcvs,
(available at <a href="http://cvsgui.org">cvsgui.org</a>) you must set
the 
<tt>CVSROOT</tt> variable (in the Admin|Preferences submenu) to be 
<tt>:pserver:anonymous@cvs.audacity.sourceforge.net:/cvsroot/audacity</tt>
and select "pserver" or "Password" authentication. Then,
under the Globals tab of the Preferences dialog, make sure
you have unchecked "Checkout read-only (CVSREAD)".  Next,
choose login, and hit the enter key for the password (it is
""). Finally, choose "Create|Checkout module", choose a
location that you want to put the checked-out branch, and
follow the directions below depending on which branch you want.
If you get an error, make sure that your <tt>CVSROOT</tt>
variable does not contain any white space at the end--which
can happen if you copied the variables directly from this web page.

<p>
<b>To get the latest unstable branch (1.1.0):</b><br>
Under the "Checkout Settings" dialog, enter
<tt>audacity</tt> as the module name. Hit "OK" and the
branch will be automatically downloaded onto your computer.
<p>
<b>To get the stable (0.9-1.0) branch: </b><br>
Under the "Checkout Settings" dialog, enter
<tt>audacity-old</tt> as the module name.  Then, under the
"Sticky options" tab, check the "Retrieve rev./tag/branch
(-r)" box and enter <tt>audacity-0_9-branch</tt> into the
box beside it. Hit "OK" and the branch will be automatically
downloaded onto your computer.

<hr width="80%">
New to CVS?  Get started by reading Jim Blandy's <a href="">Introduction
to
CVS</a>, Bob Arnson's <a
href="http://www.cvshome.org/new_users.html">CVS for new
users</a>, or visit the cvs webpage at <a
href="http://www.cvshome.org/">www.cvshome.org</a>.
More detailed information is available in the GPLed chapters of Karl
Fogel's
<a href="http://cvsbook.red-bean.com/cvsbook.html">CVS Book
at cvsbook.red-bean.com</a>, or the "Official" <a
href="http://www.cvshome.org/docs/manual">Per
 Cederqvist manual</a>.


<p> For specific help with CVS on sourceforge.net, try the
sourceforge documentation for
<a
href="http://sourceforge.net/docman/display_doc.php?docid=763&group_id=1">Unix</a>,
<a
href="http://sourceforge.net/docman/display_doc.php?docid=766&group_id=1">Microsoft Windows</a>, and <a
href="http://sourceforge.net/docman/display_doc.php?docid=2973&group_id=1">MacOS
(prior to OS X)</a> platforms.

<hr width="80%">

<p>
<b>More details:</b>

</p>

<p>Audacity uses many third party libraries. Many of them require tweaking
to build on all our target platforms. Therefore, we keep a local repository
of all third party library source code in CVS. Here is how it works:
</p>
<p>There are two repositories: 'audacity-src', which contains all of
the code we have written, and
'lib-src,' which contains the source code to all the
libraries we use.
In order to guarantee interoperability between Audacity and our
libraries, we recommend that you use the versions of the libraries
contained in 'lib-src'.  However, on a Unix system you can avoid compiling
some of the libraries by using libraries you already have on your system.
Type 'configure --help' to see the options.
</p>
<p>
So, if you want to check out everything, including the library source code,
checkout the module 'audacity' which will check out audacity-src but also
grab the lib-src repository as a subdirectory of 'audacity'.
</td>
</tr>
</table>
</p>

<?php BoxBottom(); ?>
