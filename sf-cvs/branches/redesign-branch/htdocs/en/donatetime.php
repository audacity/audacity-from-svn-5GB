<?php BoxTop($donateTimeStr); ?>

<p>
Audacity was written by volunteers in their spare time.  If you
appreciate Audacity and would like to help out, here are some
ideas:
</p>

<ul>

<li><b>Tell us Hello!</b>
<p>
If you enjoy Audacity, tell us!  Send email to
<a href="mailto:audacity-help@lists.sourceforge.net"
>audacity-help@lists.sourceforge.net</a>
and let us know that you like it.  It makes us happy to know that
people enjoy it.  Also feel free to send us your comments and
suggestions - we're always improving Audacity, and we value your
input.
<br><?php print "$listPrivacyStr"; ?>
<p>

<li><b>Join the User Community</b>
<p>
This mailing list is a place where Audacity users can talk with one another
and share their ideas.
<a href="http://lists.sourceforge.net/lists/listinfo/audacity-users">Join the Audacity-Users Mailing List</a><br>


<li><b>Be a translator</b>
<p>
If you are fluent in both English and another language, you can help
out other people who speak your language.  We need people to translate
the program menus and dialogs, the documentation, and the website.
We also need people to join mailing lists for people to ask questions about
Audacity in their native language - you can help out either by answering
questions, or by translating the questions into English and posting them
to one of the English mailing lists.
<p>
<a href="http://audacity.sourceforge.net/translation/"
>Click here to learn more about translation.</a>
<p>

<li><b>Report bugs</b>
<p>
If you find a bug in Audacity,
<a href="mailto:audacity-help@lists.sourceforge.net"
>let us know</a>.
It is the most helpful if you take the time to reproduce the bug and
send us a detailed description of what you need to do in order to make
it happen.  Be sure to tell us what operating system you run (i.e.
Windows 2000, Mac OS 9, etc.).
<p>


<li><b>Be a beta-tester</b>
<p>
In addition to the latest stable version of Audacity, we always have
a beta version of Audacity.  This version usually contains many new
features that haven't been tested yet.  If you want to help out,
download this version of Audacity and play with it.  Let us know what
you like and what you don't like, and tell us how you think it should
be improved.  Be on the lookout for <i>regressions</i> - things that
used to work but don't work anymore.
<p>

<li><b>Be a developer</b>
<p>
If you are already an experienced C++ programmer and you are comfortable
using tools such as CVS, we would love to have help developing Audacity.
Please download the latest source code from CVS and compile it, and then
introduce yourself on the
<a href=http://lists.sourceforge.net/lists/listinfo/audacity-devel>
<?php print $develListStr; ?></a>.
<p>

<li><b>Donate money</b>
<p>
If none of the above options work for you, consider
<?php print "<a href=donatemoney.php?$langLinkStr>"; ?>donating money</a>.
</p>

</ul>

<?php BoxBottom(); ?>
