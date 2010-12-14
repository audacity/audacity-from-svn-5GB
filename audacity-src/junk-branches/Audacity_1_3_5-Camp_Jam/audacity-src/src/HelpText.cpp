/**********************************************************************

  Audacity: A Digital Audio Editor

  HelpText.cpp

  James Crook
  Vaughan Johnson

********************************************************************//**

\file HelpText.cpp
\brief Given a key, returns some html.
*//********************************************************************/

#include <wx/string.h>
#include <wx/html/htmlcell.h>
#include <wx/intl.h>

#include "Audacity.h"
#include "AudacityApp.h"
#include "AudacityBranding.h"
#include "HelpText.h"

#if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   #include "effects/Effect.h"
   #include "toolbars/ControlToolBar.h"
   #include "widgets/LinkingHtmlWindow.h"
   #include "Project.h"
#endif

wxString WrapText( const wxString & Text )
{
   return wxString(wxT(""))+
      wxT("<html><head></head>") +
      #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
         wxT("<body bgcolor=\"#ffffff\" link=\"#e76e34\">") + // orange links, rgb = (231, 110,  52)
      #else
         wxT("<body bgcolor=\"#ffffff\">") +
      #endif
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

#if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   wxString MoreHelp()
   {
      return _("Need [[moreHelp|more help]]?<br><br>");
   }
#endif

wxString gStrCurrKey = wxT("welcome");
wxString gStrPrevKey = wxT("welcome");

wxString ToWelcome( )
{
   wxString strResult = _("Back to [[") + gStrPrevKey + wxT("|") + TitleText(gStrPrevKey);
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      strResult += _("]]<br><br>Back to [[welcome|\"I want to...\"]]");
   #else
      strResult += _("To [[welcome|Welcome screen]]");
   #endif
   return strResult;
}

wxString TitleText( const wxString & Key )
{
   if(Key==wxT("welcome"))
   {
      #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
         return _("Jamling Audacity");
      #else
         return _("Welcome!");
      #endif
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
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      //if (Key == wxT("clickTrack"))
      //   return _("How do I get rid of the metronome sound?");
      if (Key == wxT("playSong"))
         return _("Play the Song I bought from Jamling");
      if (Key == wxT("jamlingRecord"))
         return _("Record my instrument or voice");
      if (Key == wxT("fixTrack"))
         return _("Fix part of a Track");
      if (Key == wxT("slowDown"))
         return _("Slow down a section of this Song");
      if (Key == wxT("changeKey"))
         return _("Change the key of this Song");
      if (Key == wxT("loopPlay"))
         return _("Loop Play a section of this Song");
      if (Key == wxT("uploadTrack"))
         return _("Upload my Track to Jamling");
      if (Key == wxT("getTrack"))
         return _("Get another Jamling\'s Track for this Song");
      if (Key == wxT("uploadProject"))
         return _("Upload my finished Mix to Jamling");
      if (Key == wxT("getSong"))
         return _("Get a different Song from Jamling");
      if (Key == wxT("jamlingExport"))
         return _("Export my finished Mix to my media player");
      if (Key == wxT("exportMultiple"))
         return _("Use these tracks in a different recording application");
      if (Key == wxT("save"))
         return _("Save my Song or Open a different Song");

      if (Key == wxT("internalMic"))
         return _("Use my computer's internal microphone");
      if (Key == wxT("PCHints"))
         return _("Recording Setup for PC");
      if (Key == wxT("MacHints"))
         return _("Recording Setup for Mac");
      if (Key == wxT("lineIn"))
         return _("Plug my instrument or mic into my computer");
      if (Key == wxT("USBin"))
         return _("Use a USB mic or instrument interface");
   #endif
   return Key;
}

wxString HelpTextBuiltIn( const wxString & Key )
{
   gStrPrevKey = gStrCurrKey;
   gStrCurrKey = Key;
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      wxString varKey = Key;
      wxString strImagesDir = 
#if defined(__WXMAC__)
         wxGetApp().audacityPathList[0] + wxT("/../Resources/");
#else
         wxGetApp().audacityPathList[0] + wxFileName::GetPathSeparator() + 
         wxT("Jamling_HelpText_images") + wxFileName::GetPathSeparator();
#endif
      if (Key == wxT("welcome"))
         return WrapText(
                  wxString(wxT("")) +
                  _("</p><center><h3>I want to...</h3></center><br>") +
                  /** i18n-hint: where you see [[key|text]] translate 'text' and don't translate 'key' */
                  /* _("<ul><li>How do I [[clickTrack|get rid of the metronome sound]]? \ */
                  _("<ul><li>[[playSong|Play]] the Song I bought from Jamling. \
                        <li>[[jamlingRecord|Record]] my instrument or voice. \
                        <li>[[fixTrack|Fix]] part of a Track. \
                        <br>&nbsp;<br> \
                        <li>[[slowDown|Slow down]] a section of this Song. \
                        <li>[[changeKey|Change the key]] of this Song. \
                        <li>[[loopPlay|Loop Play]] a section of this Song. \
                        <br>&nbsp;<br> \
                        <li>[[uploadTrack|Upload my Track]] to Jamling. \
                        <li>[[getTrack|Get another Jamling\'s Track]] for this Song. \
                        <li>[[uploadProject|Upload my finished Mix]] to Jamling. \
                        <li>[[getSong|Get a different Song]] from Jamling. \
                        <li>[[jamlingExport|Export my finished Mix]] to my media player. \
                        <li>Use these tracks in a [[exportMultiple|different recording application]]. \
                        <br>&nbsp;<br> \
                        <li>[[save|Save my Song or Open a different Song]]. \
                     </ul></p><p>"));
      if (Key == wxT("moreHelp")) // Appears at the bottom of pages with "Back to...".
         return WrapText(wxString(wxT("")) + 
            _("<p><b>Need more help? Try:</b> \
               <ul><li><a href=\"http://jamling.com/forum\">Jamling Forums</a></li> \
                  <li><a href=\"http://audacity.sourceforge.net/manual-1.2/tutorials.html\"> \
                     Audacity Tutorial</a></li> \
                  <li><a href=\"http://audacityteam.org/wiki/index.php?title=Recording_Tips\"> \
                     Recording Tips</a> at Audacity Wiki</li> \
                  <li><a href=\"http://audacityteam.org/wiki/index.php?title=Troubleshooting_Recordings\"> \
                     Troubleshooting Recording</a> at Audacity Wiki</li> \
               </ul></p><br><br>") + ToWelcome());
      //if (Key == wxT("clickTrack"))
      //   return WrapText(wxString(wxT("")) + 
      //      _("<p><b>How do I get rid of the metronome sound?</b> \
      //         <p>&nbsp;</p> \
      //         <img src=\"") + strImagesDir + _("ClickTrack.jpg\"> \
      //         <ul><li>If you hear an annoying tick-tock sound that isn't in the original recording, \
      //               the click track is on.</li> \
      //            <li>The click track is at the bottom of the main window, and if you press \
      //               the Mute button it will be silent.</li> \
      //         </ul></p><br><br>") + MoreHelp() + ToWelcome());
      if (Key == wxT("playSong"))
         return WrapText(wxString(wxT("")) + 
            _("<p><b>Play the Song I bought from Jamling</b> \
               <p>&nbsp;</p> \
               <ul><li>Unzip your Jamling purchase.</li> \
                  <li>Open the Folder.</li> \
                  <li>Double-click on the .AUP file with the headphones.<br> \
                     <img src=\"") + strImagesDir + _("AUPfile.jpg\"></li> \
                  <li>Press Play.<br> \
                     <img src=\"") + strImagesDir + _("PlayBtn.jpg\"></li> \
               </ul></p><br><br>") + MoreHelp() + ToWelcome());
      if (Key == wxT("jamlingRecord"))
         return WrapText(wxString(wxT("")) + 
            _("<p><b>Record my instrument or voice</b> \
               <ul><li>Jamling Audacity will be set to your computer's default audio input - most likely \
                     your computer's internal microphone.   \
                     This is the easiest and fastest way to get started with Jamling.</li> \
                  <li>To use your computer's internal mic to record a track, [[internalMic|click here]].</li> \
                  <li>To plug your instrument or mic into your line-in port on your computer, [[lineIn|click here]].</li> \
               </ul></p><br><br>") + MoreHelp() + ToWelcome());
      if (Key == wxT("internalMic"))
         return WrapText(wxString(wxT("")) + 
            _("<p><b>Use my computer's internal microphone</b> \
               <ul><li>Your internal computer microphone is probably already set up. \
                     <ol><li>Press the red Record button \
                           <img src=\"") + strImagesDir + _("RecordBtn.jpg\"> \
                           and make some noise.</li> \
                        <li>Press Stop.</li> \
                        <li>Press Play. Listen for the noise you made.</li></ol></li> \
                  <li>If you hear it, you're ready to go!  \
                     If not, click [[PCHints|here for PC]] or [[MacHints|here for Mac]].</li> \
                  <li>Erase your test noise track by selecting it and going to the top menu:  \
                     <i>Tracks &gt; Remove Tracks</i>.</li> \
                  <li>IMPORTANT NOTE - you'll want to plug in headphones to prevent feedback and help \
                     the music be loud enough for you to play or sing along!</li> \
               </ul></p><br><br>") + MoreHelp() + ToWelcome());
      if (Key == wxT("PCHints"))
         return WrapText(wxString(wxT("")) + 
            _("<p><b>Recording Setup for PC</b> \
               <ul><li><i>Start &gt; Control Panel &gt; Sounds and Audio Devices &gt; \
                     Audio &gt; Sound Recording &gt; Volume...</i> Button, then check <i>Internal Mic</i> \
                     and adjust volume.</li> \
                  <li>You won't be able to hear the sound you're recording through your headphones. \
                     If you want to hear it, go to <i>Edit &gt; Preferences &gt; Audio I/O</i> tab, \
                     and check <i>Play other tracks...</i> and <i>Software Playthrough</i>.</li> \
                  <li>While you're there, make sure your <i>Playback Device</i> and <i>Recording Device</i> \
                     are set for Windows DirectSound.  \
                     If you don't have DirectSound on your PC, you'll want to get it to make \
                     Jamling rock as hard as possible.  \
                     Download it \
                     <a href=\"http://www.microsoft.com/downloads/details.aspx?familyid=2da43d38-db71-4c1b-bc6a-9b6652cd92a3&displaylang=en\"> \
                        here</a>. \
                     (Don't worry - if your PC isn't equipped for DirectSound, Jamling will work just fine \
                     with whatever sound card and drivers you have.)</li> \
                  <li>Then click on the Input Level Meter \
                     <img src=\"") + strImagesDir + _("InputLevelMeter.jpg\"></li> \
                     in Audacity to start monitoring yourself.  \
                     (Be sure to plug in headphones first.)  \
                     You'll experience an echo - don't worry, that's normal!</li>  \
               </ul></p><br><br>") + MoreHelp() + ToWelcome());
      if (Key == wxT("MacHints"))
         return WrapText(wxString(wxT("")) + 
            _("<p><b>Recording Setup for Mac</b> \
               <ul><li><i>Apple Menu &gt; System Preferences &gt; Sound &gt; Input &gt; \
                     Internal Microphone</i></li> \
                  <li>You won't be able to hear the sound you're recording through your headphones \
                     when recording. \
                     If you want to hear it, go to <i>Audacity &gt; Preferences &gt; Audio I/O</i> tab, \
                     and check <i>Hardware Playthrough</i> and <i>Software Playthrough</i>.</li> \
                  <li>Click on the Input Level Meter \
                     <img src=\"") + strImagesDir + _("InputLevelMeter.jpg\"></li> \
                     in Audacity to start monitoring yourself.  \
                     (Be sure to plug in headphones first.)  \
                     You'll experience an echo - don't worry, that's normal!</li>  \
               </ul></p><br><br>") + MoreHelp() + ToWelcome());
      if (Key == wxT("lineIn"))
         return WrapText(wxString(wxT("")) + 
            _("<p><b>Plug my instrument or mic into my computer</b> \
               <ul><li>You'll need a cable with a 1/8' (mini) plug in order to plug into the Line In \
                        on your computer, probably indicated by one of these symbols: \
                     <img src=\"") + strImagesDir + _("LineIn1.jpg\"> &nbsp; &nbsp; &nbsp; \
                     <img src=\"") + strImagesDir + _("LineIn2.jpg\"></li> \
                  <li>If you're using a normal 1/4' instrument cable for your guitar, bass, electronic drums, \
                     etc. you'll need an adapter like this, available for about 4 bucks:\
                     <img src=\"") + strImagesDir + _("Adapter.jpg\"></li> \
                  <li>Once you have the right gear, you'll need to change the default audio input: \
                     <ul><li>For PC, <i>Start &gt; Control Panel &gt; Sounds and Audio Devices &gt; \
                           Audio &gt; Sound Recording...</i> \
                           From here it varies by computer - select the option resembling <i>Line Input</i>.</li> \
                        <li>For Mac, <i>Apple Menu &gt; System Preferences &gt; Sound &gt; Input &gt; \
                           Line In</i></li> \
                     </ul></li> \
                  <li>You may have to restart Jamling Audacity after you've made this change.</li> \
                  <li>Press the red Record button \
                     <img src=\"") + strImagesDir + _("RecordBtn.jpg\"> \
                     and make some noise. \
                     Press the space bar to stop recording and press it again to play back the noise you \
                     just made.  If you hear your noise, you're in business!</li> \
                  <li>Adjust the input level on your internal mic with the slider:  \
                     <img src=\"") + strImagesDir + _("InputLevelSlider.jpg\"></li> \
                  <li>Click on the Input Level Meter \
                     <img src=\"") + strImagesDir + _("InputLevelMeter.jpg\"></li> \
                     in Audacity to start monitoring yourself.  \
                  <li>Erase your test noise track by selecting it and going to the top menu:  \
                     <i>Tracks &gt; Remove Tracks</i>, or by clicking the X button on the track.</li> \
               </ul></p><br><br>") + MoreHelp() + ToWelcome());
      if (Key == wxT("USBin"))
         return WrapText(wxString(wxT("")) + 
            _("<p><b>Use a USB mic or instrument interface</b></p> \
                <p><i>USB interfaces sometimes have trouble working with Jamling Audacity.  \
                   I could explain a bunch of technical and legal reasons that is the case, but instead \
                   I'll just give you the basic steps to try...</i></p> \
                <p><ul><li>First you'll need to change the default audio input: \
                     <ul><li>For PC, <i>Start &gt; Control Panel &gt; Sounds and Audio Devices &gt; \
                           Audio &gt; Sound Recording &gt; Volume...</i> Button, then check <i>Internal Mic</i> \
                           and adjust volume.</li> \
                        <li>For Mac, <i>Apple Menu &gt; System Preferences &gt; Sound &gt; Input &gt; \
                           Internal Microphone</i></li> \
                     </ul></li> \
                  <li>You may have to restart Jamling Audacity after you've made this change.</li> \
                  <li>Once it says 'USB Device' at the top, press the red Record button \
                     <img src=\"") + strImagesDir + _("RecordBtn.jpg\"> \
                     and make some noise \
                     on your instrument or mic.  \
                     Press the space bar to stop recording and press it again to play back the noise you \
                     just made.  If you hear your noise, you're in business!</li> \
                  <li>If so, you're ready to go!  \
                     You can adjust the input level on your internal mic with the slider:  \
                     <img src=\"") + strImagesDir + _("InputLevelSlider.jpg\"></li> \
                  <li>You want it to be loud enough to pick up your sound, but not so loud it hits the red \
                     on the 'Input Volume' monitor.  This is called 'clipping' and it makes those ugly \
                     crackling noises you probably don't want in your track.</li> \
                  <li>You can erase your test noise track by selecting it and going to the top menu:  \
                     <i>Tracks &gt; Remove Tracks</i>, or by clicking the X button on the track.</li> \
               </ul></p><br><br>") + MoreHelp() + ToWelcome());
      if (Key == wxT("fixTrack"))
         return WrapText(wxString(wxT("")) + 
                  _("<p><b>Fix part of a Track</b> \
                     <ul> \
                        <li>Select the portion of the desired track.</li> \
                        <li><i>Edit &gt; Silence</i> (Ctrl+L)</li> \
                        <li>[[record|Record]] my voice or instrument.</li> \
                        <li><i>Tracks &gt; Mix and Render</i></li> \
                     </ul></p><br><br>") + MoreHelp() + ToWelcome());
      if ((Key == wxT("ChangeTempo")) || (Key == wxT("ChangePitch")))
      {
         Effect* pEffect = Effect::GetEffectByIdentifier(Key, BUILTIN_EFFECT | PROCESS_EFFECT | ADVANCED_EFFECT);
         if (pEffect)
         {
            AudacityProject* pProject = GetActiveProject();
            if (Key == wxT("ChangeTempo"))
               pProject->SelectAllIfNone(); 
            else // Always select all for ChangePitch, per Nick's email 2008-12-13.
               pProject->OnSelectAll(); 
            pProject->OnEffect(BUILTIN_EFFECT, pEffect);
         }
         // Don't return. Change Key to be handled below.
         if (Key == wxT("ChangeTempo")) 
            varKey = wxT("slowDown");
         else
            varKey = wxT("changeKey");
      }
      if (varKey == wxT("slowDown"))
         return WrapText(wxString(wxT("")) + 
                           _("<p><b>Slow down a section of this Song</b></p> \
                              <p>Before slowing down, go to <i>File &gt; Save Project As</i> and name your \
                                 session something new. \
                                 This way you can always get back to the Song at the right speed!</p> \
                              <p>Highlight the section you want to slow down and [[ChangeTempo|click here]].</p> \
                              <br><br>") + MoreHelp() + ToWelcome());
      if (varKey == wxT("changeKey"))
         return WrapText(wxString(wxT("")) + 
                  _("<p><b>Change the key of this Song</b> \
                     <ul><li>Use the [[ChangePitch|Change Pitch dialog]] to change the key.</li> \
                        <li>Jamling Audacity determines the original key based on the beginning.</li> \
                        <li>You may want to go to <i>File &gt; Save As</i> before changing the key, \
                           so you can return back to the original key later.</li> \
                        <li>To immediately return to the previous key: <i>Edit &gt; Undo</i>.</li> \
                     </ul></p><br><br>") + MoreHelp() + ToWelcome());
      if (Key == wxT("loopPlay"))
      //{
      //   AudacityProject* pProject = GetActiveProject();
      //   pProject->SelectAllIfNone();
      //   pProject->GetControlToolBar()->PlayCurrentRegion(true); 
      //   return HelpTextBuiltIn(wxT("welcome"));
      //}
         return WrapText(wxString(wxT("")) + 
                  _("<p><b>Loop play a section of this Song</b> \
                     <ul><li>Highlight the section of the Song you want to loop (repeat).</li> \
                        <li>Then hold down the shift key and click on the Play button.</li> \
                        <li>The section you've highlighted will loop until you hit Stop (or the space bar).</li> \
                     </ul></p><br><br>") + MoreHelp() + ToWelcome());
      if (Key == wxT("uploadTrack"))
         return WrapText(wxString(wxT("")) + 
                  _("<p><b>Upload my Track to Jamling</b> \
                     <ul> \
                        <li>Select the track you want to upload.</li> \
                        <li><i>File &gt; Export Selection</i>. Export in OGG format.</li> \
                        <li>Go to the <a href=\"http://jamling.com/node/add/audio\">Jamling Submit Audio form</a>.</li> \
                        <li>In the Audio File area of the form, use the Browse button to find your Track.</li> \
                        <li>Optionally add a description and artwork.</li> \
                        <li>Submit the file.</li> \
                     </ul></p><br><br>") + MoreHelp() + ToWelcome());
                        //<li>Log on to <a href=\"") + 
                        //   AUDACITY_BRANDING_BRANDURL + 
                        //   _("\">Jamling</a>.</li> 
      if (Key == wxT("getTrack"))
         return WrapText(wxString(wxT("")) + 
                  _("<p><b>Get another Jamling\'s Track for this Song</b> \
                     <ul> \
                        <li>Go to <a href=\"http://jamling.com/content/jamling-stuff\">Jamling Stuff</a>.</li> \
                        <li>Select the Track you want to download and double-click it.</li> \
                        <li>Drag and drop the Track from your desktop (or wherever you downloaded it) into Audacity.</li> \
                     </ul></p><br><br>") + MoreHelp() + ToWelcome());
      if (Key == wxT("uploadProject"))
      {
         return WrapText(wxString(wxT("")) + 
                  _("<p><b>Upload my finished Mix to Jamling</b> \
                     <ul> \
                        <li><i>File &gt; Export</i>. Export in OGG format. \
                           This will mix all the tracks together.</li> \
                        <li>Go to the <a href=\"http://jamling.com/node/add/audio\">Jamling Submit Audio form</a>.</li> \
                        <li>In the Audio File area of the form, use the Browse button to find your Mix.</li> \
                        <li>Optionally add a description and artwork.</li> \
                        <li>Submit the file.</li> \
                     </ul></p><br><br>") + MoreHelp() + ToWelcome());
                        //<li>Log on to <a href=\"") + 
                        //   AUDACITY_BRANDING_BRANDURL + 
                        //   _("\">Jamling</a>.</li> 
      }
      if (Key == wxT("getSong"))
         return WrapText(wxString(wxT("")) + 
                  _("<p><b>Get a different Song from Jamling</b> \
                     <ul> \
                        <li>Go to the <a href=\"http://jamling.com/content/music\">Jamling Music</a> page.</li> \
                     </ul></p><br><br>") + MoreHelp() + ToWelcome());
      if (Key == wxT("jamlingExport"))
         return WrapText(wxString(wxT("")) + 
                  _("<p><b>Export my finished Mix to my media player</b> \
                     <ul> \
                        <li>Go to <i>File &gt; Export &gt; Save as Type &gt; \
                           WAV, AIFF, and other uncompressed types</i>.</li>  \
                        <li>Then choose where to save your Mix, and \
                           import it into your media player from there.</li> \
                        <li>Note: Do not select MP3 unless you've downloaded the MP3 encoder, \
                           which you can find by searching for 'Audacity .mp3 encoder' on the Internet.</li> \
                        <li>Before you export your Mix, make sure to take note of what you've named it and \
                           where you're saving it.</li> \
                     </ul></p><br><br>") + MoreHelp() + ToWelcome());
      if (Key == wxT("exportMultiple"))
         return WrapText(wxString(wxT("")) + 
                  _("<p><b>Use these tracks in a different recording application</b> \
                     <p>Easy - go to <i>File &gt; Export Multiple</i>.  \
                        Choose your uncompressed format (most recording apps can use either .wav or. aiff) \
                        and export the files.  From there, import them into your other program. \
                     </p><br><br>") + MoreHelp() + ToWelcome());
   #else // !((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
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
   #endif // ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))

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

