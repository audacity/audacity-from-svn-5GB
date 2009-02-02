/**********************************************************************

  Audacity: A Digital Audio Editor

  HelpText.cpp

  James Crook

********************************************************************//**

\file HelpText.cpp
\brief Given a key, returns some html.
*//********************************************************************/

#include <wx/string.h>
#include <wx/intl.h>

#include "Audacity.h"
#include "HelpText.h"


wxString WrapText( const wxString & Text )
{
   return wxString(wxT(""))+
      wxT("<html><head></head>") +
      wxT("<body bgcolor=\"#ffffff\">") +
      wxT("<p>") + Text +
      wxT("</body></html>");
}

wxString Link( const wxString &Key, const wxString& Text )
{
   return wxString(wxT("")) +
      wxT("<a href='innerlink:") +
      Key +
      wxT("'>") +
      Text +
      wxT("</a>");
}

wxString WikiLink( const wxString &Key, const wxString& Text )
{
   return wxString(wxT("")) +
      wxT("<a href='http://www.audacityteam.org/wiki/index.php?title=") +
      Key +
      wxT("'>") +
      Text +
      wxT("</a>");
}

wxString LinkExpand( const wxString & Text )
{
   wxString Temp = Text;
   int i,j,k;
   while( (i=Temp.First( wxT("[[") ))!= wxNOT_FOUND )
   {
      wxString Key = Temp.Mid(i+2);
      j = Key.First( wxT("|") );
      if( j==wxNOT_FOUND )
         return Temp;
      wxString LinkText = Key.Mid( j+1);
      k = LinkText.First( wxT("]]") );
      if( k==wxNOT_FOUND )
         return Temp;
      Key = Key.Mid( 0, j );
      LinkText = LinkText.Mid( 0, k );

      wxString Replacement;
      if( Key.StartsWith( wxT("wiki:") ))
      {
         Replacement = WikiLink( Key.Mid( 5 ), LinkText );
      }
      else
      {
         Replacement = Link( Key, LinkText );
      }

      Temp = Temp.Mid( 0, i ) + Replacement + Temp.Mid( i + j + k + 5 );// 5 for the [[|]]
   }
   return Temp;
}

wxString ToWelcome( )
{
   return _("To [[welcome|Welcome screen]]");
}

wxString TitleText( const wxString & Key )
{
   if(Key==wxT("welcome"))
   {
      return _("Welcome!");
   }

   if(Key ==wxT("play") )
   {
      return _("Playing Audio");
   }
   if((Key ==wxT("record") ) || (Key ==wxT("norecord") ))
   {
      return _("Recording Audio");
   }
   if(Key ==wxT("inputdevice") )
   {
      return _("Recording - Choosing the Input Device");
   }
   if(Key ==wxT("inputsource") )
   {
      return _("Recording - Choosing the Input Source");
   }
   if(Key ==wxT("inputlevel") )
   {
      return _("Recording - Setting the Input Level");
   }
   if((Key ==wxT("edit") ) || (Key==wxT("grey")))
   {
      return _("Editing and greyed out Menus");
   }
   if(Key ==wxT("export") )
   {
      return _("Exporting an Audio File");
   }
   if(Key ==wxT("save") )
   {
      return _("Saving an Audacity Project");
   }
   if(Key ==wxT("wma-proprietary") )
   {
      return _("Unsupported Formats");
   }
   if(Key ==wxT("burncd") )
   {
      return _("Burn to CD" );
   }
   if(Key ==  wxT("remotehelp") )
   {
      return _("No Local Help");
   }
   return Key;
}

wxString HelpTextBuiltIn( const wxString & Key )
{
   if(Key==wxT("welcome"))
   {
      return WrapText(
wxString(wxT("")) +
_("</p><center><h3>Getting Started</h3></center><br>") +
/** i18n-hint: where you see [[key|text]] translate 'text' and don't translate 'key' */
_("Welcome to Audacity ") + AUDACITY_VERSION_STRING + wxT("! ") +
_("Learn how to: \
<ul><li>\
[[play|play]] an existing audio file \
<li>[[record|record]] your voice, LP or tape \
<li>[[edit|edit]] sounds \
<li>[[save|save or open an Audacity project]] \
<li>[[export|export]] to an MP3 or other audio file, or [[burncd|burn to a CD]]  \
</ul></p><p>") +
_("This screen can be viewed at any time \
by clicking the <i>Help</i> menu, then <i>Show Welcome Message</i>. \
For a detailed guide to all the Audacity menus and controls, click  \
<i>Help</i> menu then <i>Index</i>, or download our <a href=\"http://audacity.sourceforge.net/help/documentation/manual_1.4.pdf\">full Manual</a> \
in PDF format.\
</p>")
         );
   }

   if(Key ==wxT("play") )
   {
      return WrapText(
wxString(wxT(""))+
_("<p><b>Opening audio files:</b> \
Either drag the files into the current project window, or click <i>File &gt; Import &gt; Audio</i>. \
Files can be opened into a new project window with <i>File &gt; Open</i>. \
The main formats Audacity plays are <b>AIFF, AU, FLAC, M4A</b> (only on a \
Mac), <b>MP2/MP3, OGG Vorbis</b> and <b>WAV</b>. Click [[wma-proprietary|here]] if your file is in some other format. \
<b>To import a CD:</b> extract it to WAV or AIFF using iTunes, Windows Media Player 11 or similar. \
See our online guide: [[wiki:How_to_import_CDs|importing CDs]].</p><p> \
<p><b>Playing audio:</b> Press the green Play button to start playback. \
Press the blue Pause button once to pause playback, and again to resume. \
To stop, press the yellow Stop button. \
Spacebar can be used to either play or stop. \
After stopping, playback resumes from its last starting point. \
To change the starting point, click in the track at your desired starting point. \
The <b>|&lt;&lt;</b> and <b>&gt;&gt;|</b> buttons can be used to skip to the start or end of the track respectively. \
</p><br><br>")+
ToWelcome()
         );
   }
   if((Key ==wxT("record") ) || (Key ==wxT("norecord") ))
   {
      return WrapText(
wxString(wxT(""))+ 
_("<p><b>To record:</b> Set the recording device and input source, adjust the input level, \
then press the red Record button.\
</b></p><p> \
1) [[inputdevice|Set Recording Device]] either in the <i>Audio I/O tab of Preferences</i> \
or in <i>Device Toolbar</i>. Audio I/O Preferences lets you choose stereo recording if required.<br> \
2) [[inputsource|Set input source]] for that device (for example, microphone or line-in) \
in the dropdown selector of the <i>Mixer Toolbar</i>, \
or (on some systems) at Recording Device in the <i>Audio I/O tab of Preferences</i> \
or in <i>Device Toolbar</i>.<br> \
3) [[inputlevel|Adjust input level]] using the right-hand slider \
on the <i>Mixer Toolbar</i>. Correct adjustment of level before recording \
is essential to avoid noise or distortion. \
</p><p> \
More help at: \"Recording\" <a href=\"http://audacity.sourceforge.net/help/faq\">FAQs</a>, \
our Tutorial <a href=\"http://www.audacityteam.org/manual/index.php?title=Tutorial_-_Your_First_Recording\"> \
Your First Recording</a> in the \
<a href=\"http://www.audacityteam.org/manual/index.php?title=Main_Page\">Manual</a>, \
and our Wiki [[wiki:Recording_Tips|Recording Tips]] and \
[[wiki:Troubleshooting_Recordings|Troubleshooting Recordings]] pages. \
</p><br><br>") +
ToWelcome()
         );
   }
   if (Key ==wxT("inputdevice") )
   {
      return WrapText(
wxString(wxT(""))+ 
_("<p>The input device is the actual physical sound device you are recording from. \
This is chosen at \"Recording Device\" in the <i>Audio I/O tab of Preferences</i>, \
On Windows and Linux, Preferences are at the bottom of the <i>Edit</i> Menu. \
On a Mac, they in the <i>Audacity</i> Menu. \
You can also conveniently select Recording Device in the <i>Device Toolbar</i>, \
which can be enabled at <i>View &gt; Toolbars...</i>. \
</p><p> \
By default, Audacity uses the device currently being used by the system, \
(usually your inbuilt sound device), \
so it is often not necessary to change the input device. \
On Windows, this current system device can be selected as \"Microsoft Sound Mapper\". \
If you are recording from an external USB or Firewire device such as a USB turntable, \
select it explicitly by name as recording device after connecting it. \
</p><p> \
Back to [[record|Recording Audio]] \
<br></p>") +
ToWelcome()
         );
   }
   if (Key ==wxT("inputsource") ) 
   {
      return WrapText(
wxString(wxT(""))+ 
_("<p>Select the input source (such as microphone or line-in) for your [[inputdevice|recording device]]:<br> \
- <b>On Windows (except Vista)</b> and <b>Linux</b>, in the dropdown selector on the right of the <i>Mixer Toolbar</i>.<br> \
- <b>On Windows Vista</b> and <b>Mac</b>, at Recording Device on the \
<i>Audio I/O tab of Preferences</i>, or in <i>Device Toolbar</i>. \
The source will be shown as part of the parent device \
(for example, <i>High Def Audio Card: Microphone</i>). \
If necessary, Mac users can choose the input source at Apple Audio MIDI Setup. \
<br><i><b>Note:</b> many USB or Firewire recording devices don't have a choice of inputs, \
so this step is not necessary. If in doubt, see the documentation for your device. \
</i></p><p> \
<b>Tip:</b> To <a href=\"http://audacity.sourceforge.net/help/faq?s=recording&i=streaming\">record audio playing on the computer</a>, \
Windows and Linux users can choose the \"Stereo Mix\", \"Wave Out\", \"Sum\" \
or comparable input source. \
</p><p> \
Back to [[record|Recording Audio]] \
<br></p>") +
ToWelcome()
         );
   }
   if(Key ==wxT("inputlevel") )
   {
      return WrapText(
wxString(wxT(""))+
_("<p>The input level must be set correctly. \
If the level is too low, there will be background noise. \
If the level is too high, there will be distortion in the loud parts. \
</p><p>\
To set the level, either see this [[wiki:Recording_levels|illustrated online guide]] \
or follow these steps: \
<ol><li>\
Turn on <b>monitoring</b>. Double-click the right-hand of the two <i>VU Meters</i> \
(or right-click over it) and click \"Start Monitoring\". \
If the meters are not visible, click <i>View &gt; Toolbars</i> and check \"Meter Toolbar\". \
</li><li>Adjust the input slider on the <i>Mixer Toolbar</i> \
(by the microphone symbol), so that the red bars in the meter come close to <b>but do not touch</b> \
the right edge of the scale. \
If you can't hear the monitored audio, go to the <i>Audio I/O tab of Preferences</i> \
and check \"Software Playthrough\". \
</li></ol></p><p> \
Back to [[record|Recording Audio]] \
<br></p>")+
ToWelcome()
         );
   }
   if((Key ==wxT("edit") ) || (Key==wxT("grey")))
   {
      return WrapText(
wxString(wxT(""))+
_("<p>The main commands for editing audio are under the <i>Edit</i> menu (such \
as cut, copy and paste) and the <i>Effect</i> menu (you can do things like \
boost the bass, change pitch or tempo, or remove noise). </p><p> \
Audacity applies edits to selected areas of the audio track. To select a \
particular area, click in the track and drag the shaded area with the mouse. \
If no audio is selected, Audacity selects all the audio in the project window.\
</p><p> \
When playing or recording, the Edit and Effect menus will appear greyed out, \
because a moving track can't be edited. Commands can sometimes be unavailable \
for other reasons too. For example, you can't run effects until you have audio \
on the screen, and you can't paste audio until you've cut or copied it to \
Audacity's clipboard. </p><br><br>")+
ToWelcome()
         );
   }
   if(Key ==wxT("save") )
   {
      return WrapText(
wxString(wxT(""))+
_("<p>To return later to your unfinished work in Audacity \
with all tracks and edits as you left them, save an Audacity project. \
To play your current work in other media programs or send it to others, \
[[export|export]] an audio file such as WAV or MP3. \
</p><p> \
<i>File &gt; Save Project</i> saves an <b>.aup</b> project file and  \
a <b>_data</b> folder containing the actual audio. \
To re-open a saved project, click <i>File &gt; Open</i> and open the .aup file. \
If you save and exit more than once, an .aup.bak backup file is created, \
but the .aup file should always be opened if present. \
When your final exported file is exactly as you want, delete the \
project files and _data folder to save disc space. \
</p><p><i>File &gt; Save Project As...</i> is for saving an empty project. \
It also lets you save an existing Project to a <b> new</b> name. \
This is the only safe way to rename a project. The .aup file and _data folder \
should not be moved or renamed manually. \
</p><p>")+
ToWelcome()
         );
   }
   if(Key ==wxT("export") )
   {
      return WrapText(
wxString(wxT(""))+
_("To hear your work in other media programs, export it to an audio file such as WAV or MP3 \
(some formats are [[wma-proprietary|unsupported]]). \
[[save|Saving an Audacity project]] creates a project file (and linked _data folder) \
containing all your edited tracks, so you can return to your work later. \
However, other programs can't read Audacity projects. </p><p> \
To export, click <i>File &gt; Export</i> then choose the file format you want to export to \
in the \"Save as type\" dropdown. \
To [[burncd|burn a CD]] for standalone CD players, choose WAV or AIFF. \
To export as MP3, the [[wiki:Lame_Installation|Lame Encoder]] must be installed. \
</p><p> \
<i>File &gt; Export Selection</i> exports only a selected area of audio. <i>Use File &gt; Export Multiple</i> \
to export multiple files at the same time, either \
one for each audio track, or one for each labeled area in a single track. \
<i>Tracks &gt; Add Label At Selection</i> lets you \
[[wiki:Splitting_recordings_into_separate_tracks|split the tracks]] \
from an album for multiple export as separate files. \
</p><br><br>")+
ToWelcome()
         );
   }
   if(Key ==wxT("wma-proprietary") )
   {
      return WrapText(
         wxString(wxT(""))+
_("<p>Audacity <b>cannot</b> [[play|play]] or [[export|export]] files in <b>WMA, RealAudio, Shorten (SHN)</b> \
or most other proprietary formats, due to licensing and patent restrictions.  \
On a Mac computer, unprotected <b>AAC</b> formats such as <b>M4A</b> can be imported. \
Some open formats are not yet supported, including <b>Ogg Speex</b> and <b>Musepack</b>. \
Files protected by any kind of Digital Rights Management (DRM), including \
most purchased online such as from iTunes or Napster, <b>cannot</b> be imported.</p><p> \
If you can't import your file into Audacity, \
then if it is not DRM-protected, use iTunes or <a href=\"http://www.erightsoft.com/SUPER.html#Dnload\">SUPER</a> \
to convert it to WAV or AIFF. \
If it is protected, burn it to an audio CD in the program licensed to play it, \
then extract the CD track to WAV or AIFF using \
iTunes or (on Windows) <a href=\"http://cdexos.sourceforge.net/?q=download\">CDex</a> or \
Windows Media Player 11. You could also \
<a href=\"http://audacity.sourceforge.net/help/faq?s=recording&amp;i=streaming\">record</a> the CD to your computer. \
<i>To export to an unsupported format, export to WAV or AIFF and \
convert it to the desired format in iTunes or <a href=\"http://www.erightsoft.com/SUPER.html#Dnload\">SUPER</a>. \
</i></p><p>")+
ToWelcome()
         );
     }
   if(Key ==wxT("burncd") )
   {
      return WrapText(
         wxString(wxT(""))+
_("<p>To burn your work to an audio CD for standalone CD players, first \
[[export|export]] it as WAV or AIFF. \
The exported file should be stereo, 44100 Hz (set in the \"Project Rate\" button bottom left) \
and 16 bit (set in the \"Options\" button in the file export dialog). \
These are Audacity's default settings, \
so normally you will not need to change these before exporting. \
<p>Burn the exported file to an \"audio CD\" - not a \"data CD\" or \"MP3 CD\" - \
with a CD burning program like iTunes or Windows Media Player. \
For more help, see [[wiki:How_to_burn_CDs|How to burn CDs]] online. \
</p><p>") +
ToWelcome()
         );
   }
   // Remote help allows us to link to a local copy of the help if it exists,
   // or provide a message that takes you to the internet if it does not.
   // It's used by the menu item Help > Index
   if(Key ==  wxT("remotehelp") )
   {
// *URL* will be replaced by whatever URL we are looking for.
      return WrapText(
_("<p>Help for Audacity Beta is currently unfinished. \
For now, a draft manual is available <a href=\"http://www.audacityteam.org/manual/index.php?title=Main_Page\">online</a>. \
Alternatively, click <a href=\"http://www.audacityteam.org/manual/index.php?title=Temporary_HTML_Manual_Export\">here</a> \
to download a single-page version of the manual which can be viewed in this window.")
         );
   }
   return wxT("");
}

wxString HelpText( const wxString & Key )
{

   // Possible future enhancement...
   // We could look for the text as a local file and use
   // that if we find it...
   // if( wxFileExists( Path+Key ) )
   // ...

   wxString Text;
   Text = HelpTextBuiltIn( Key );

   if( !Text.IsEmpty())
      return LinkExpand( Text );

   // Perhaps useful for debugging - we'll return key that we didn't find.
   return WrapText( Key );
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: a8955864-40e2-47aa-923b-cace3994493a

