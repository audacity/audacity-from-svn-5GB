/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.cpp

  Dominic Mazzoni

*******************************************************************//**

\class ExportMixerDialog
\brief Dialog for advanced mixing.

*//****************************************************************//**

\class ExportMixerPanel
\brief Panel that displays mixing for advanced mixing option.

*//********************************************************************/

#include <wx/textctrl.h>
#include <wx/file.h>
#include <wx/timer.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/string.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/slider.h>

#include "Export.h"
#include "ExportPCM.h"
#include "ExportMP3.h"
#include "ExportOGG.h"
#include "ExportFLAC.h"
#include "ExportCL.h"
#include "ExportMP2.h"

#include "sndfile.h"

#include "../Audacity.h"
#include "../DirManager.h"
#include "../FileFormats.h"
#include "../Internat.h"
#include "../LabelTrack.h"
#include "../Mix.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../Track.h"
#include "../WaveTrack.h"
#include "../widgets/Warning.h"
#include "../AColor.h"

/* Declare Static functions */
static wxString ExportCommon( AudacityProject *project, wxString format, 
      wxString extension, bool selectionOnly, double *t0, double *t1,
      int *numChannels, wxString &actualName, int maxNumChannels = 2, 
      MixerSpec **mixerSpec = NULL );

/*
 * This first function contains the code common to both
 * Export() and ExportLossy()
 *
 * For safety, if the file already exists it stores the filename
 * the user wants in actualName, and returns a temporary file name.
 * The calling function should rename the file when it's successfully
 * exported.
 */
wxString ExportCommon( AudacityProject *project, wxString format, 
      wxString defaultExtension, bool selectionOnly, double *t0, double *t1,
      int *numChannels, wxString &actualName, int maxNumChannels, 
      MixerSpec **mixerSpec )
{
   TrackList *tracks = project->GetTracks();

   /* First analyze the selected audio, perform sanity checks, and provide
    * information as appropriate. */

   /* Tally how many are right, left, mono, and make sure at
      least one track is selected (if selectionOnly==true) */

   int numSelected = 0, numLeft = 0, numRight = 0, numMono = 0;
   float earliestBegin = *t1;
   float latestEnd = *t0;

   TrackListIterator iter1(tracks);
   Track *tr = iter1.First();

   while (tr) {
      if (tr->GetKind() == Track::Wave) {
         if (tr->GetSelected() || !selectionOnly) {

            numSelected++;

            if (tr->GetChannel() == Track::LeftChannel)
               numLeft++;
            else if (tr->GetChannel() == Track::RightChannel)
               numRight++;
            else if (tr->GetChannel() == Track::MonoChannel) {
               // It's a mono channel, but it may be panned
               float pan = ((WaveTrack*)tr)->GetPan();
               
               if (pan == -1.0)
                  numLeft++;
               else if (pan == 1.0)
                  numRight++;
               else if (pan == 0)
                  numMono++;
               else {
                  // Panned partially off-center. Mix as stereo.
                  numLeft++;
                  numRight++;
               }
            }
            if(tr->GetOffset() < earliestBegin)
               earliestBegin = tr->GetOffset();

            if(tr->GetEndTime() > latestEnd)
               latestEnd = tr->GetEndTime();

         }
      }

      tr = iter1.Next();
   }

   if(*t0 < earliestBegin)
      *t0 = earliestBegin;
   
   if(*t1 > latestEnd)
      *t1 = latestEnd;

   if (numSelected == 0 && selectionOnly) {
      wxMessageBox(_("No tracks are selected! Use Ctrl-A (Select All)\nChoose Export... to export all tracks."),
                     _("Unable to export"),
                     wxOK | wxICON_INFORMATION);

      return wxT("");
   }
   
   /* Detemine if exported file will be stereo or mono or multichannel,
      and if mixing will occur */

   bool downMix = (gPrefs->Read( wxT("/FileFormats/ExportDownMix" ), true ) !=0) ? true:false ;
   
   int channels;
   if( downMix || !mixerSpec )
   {
      if (numRight > 0 || numLeft > 0)
         channels = 2;
      else
         channels = 1;

      numRight += numMono;
      numLeft += numMono;
   
      if (numLeft > 1 || numRight > 1)
         if (channels == 2) {
            ShowWarningDialog(project, wxT("MixStereo"),
                              _("Your tracks will be mixed down to two stereo channels in the exported file."));
         }
         else {
            ShowWarningDialog(project, wxT("MixMono"),
                              _("Your tracks will be mixed down to a single mono channel in the exported file."));
         }
   }
   else
   {
      ExportMixerDialog md( tracks, selectionOnly, maxNumChannels, NULL, 
            1, _( "Advanced Mixing Options" ) );
      
      if( md.ShowModal() != wxID_OK )
         return wxT( "" );
      
      *mixerSpec = new MixerSpec( *( md.GetMixerSpec() ) );
      channels = ( *mixerSpec )->GetNumChannels();
   }

   /* Prepare and display the filename selection dialog */

   wxString path = gPrefs->Read(wxT("/DefaultExportPath"),
                                ::wxGetCwd());
   wxString nameOnly;
   wxString extension;
   wxString defaultName = project->GetName();
   wxString fName;
   wxString maskString;
   wxString endOfPathSep;

   #if 0 // this code shouldn't be here --dmazzoni
   //MERGE exercise exception
   if (defaultName == wxT("ThrowExceptionOnExport")) {  //lda
      throw("Exercise exception");
   }
   #endif

   if (defaultExtension.Left(1) == wxT("."))
      defaultExtension =
         defaultExtension.Right(defaultExtension.Length()-1);

   maskString.Printf(wxT("%s files (*.%s)|*.%s|All files (*.*)|*.*"), format.c_str(),
                     defaultExtension.c_str(), defaultExtension.c_str());

   bool fileOkay;

   do {
      fileOkay = true;

      fName = defaultName + wxT(".") + defaultExtension;
      fName = wxFileSelector(wxString::Format(_("Save %s File As:"),
                                              format.c_str()),
                             path,
                             fName,       // default file name
                             defaultExtension,
                             maskString,
                             wxSAVE | wxOVERWRITE_PROMPT);
      
      if (fName.Length() >= 256) {
         wxMessageBox
            (_("Sorry, pathnames longer than 256 characters not supported."));
         return wxT("");
      }
      
      if (fName == wxT(""))
         return wxT("");

      ::wxSplitPath(fName, &path, &nameOnly, &extension);

      //
      // Make sure the user doesn't accidentally save the file
      // as an extension with no name, like just plain ".wav".
      //

      if ((nameOnly.Left(1)==wxT(".") && extension==wxT("")) ||
          (nameOnly==wxT("") && extension!=wxT(""))) {
         wxString prompt =
            _("Are you sure you want to save the file as \"")+
            ::wxFileNameFromPath(fName)+wxT("\"?\n");
         
         int action = wxMessageBox(prompt,
                                   wxT("Warning"),
                                   wxYES_NO | wxICON_EXCLAMATION,
                                   project);
         
         fileOkay = (action == wxYES);
         continue;
      }

      //
      // Check the extension - add the default if it's not there,
      // and warn user if it's abnormal.
      //

      wxString defaultExtension3 = defaultExtension;
      if (defaultExtension.Length() > 3)
         defaultExtension = defaultExtension.Left(3);
      
      if (extension == wxT("")) {
         #ifdef __WXMSW__
         // Windows prefers 3-char uppercase extensions
         extension = defaultExtension;
         #else
         // Linux and Mac prefer lowercase extensions
         extension = defaultExtension.Lower();
         #endif
      }
      else if (extension.Upper() != defaultExtension.Upper() &&
               extension.Upper() != defaultExtension3.Upper()) {
         #ifdef __WXMSW__
         // Windows prefers 3-char extensions
         defaultExtension3 = defaultExtension3;
         #endif

         wxString prompt;
         prompt.Printf(_("You are about to save a %s file with the name %s.\nNormally these files end in %s, and some programs will not open files with nonstandard extensions.\nAre you sure you want to save the file under this name?"),
                       format.c_str(),
                       (wxT("\"")+nameOnly+wxT(".")+extension+wxT("\"")).c_str(),
                       (wxT("\".")+defaultExtension+wxT("\"")).c_str());

         int action = wxMessageBox(prompt,
                                   wxT("Warning"),
                                   wxYES_NO | wxICON_EXCLAMATION,
                                   project);

         if (action == wxYES)
            fileOkay = true;
         else {
            fileOkay = false;
            defaultName = nameOnly + wxT(".") + extension;
         }
      }

      if (path.Length() > 0 && path.Last() == wxFILE_SEP_PATH)
         endOfPathSep = wxT("");
      else
         endOfPathSep = wxFILE_SEP_PATH;

      fName = path + endOfPathSep + 
         nameOnly + wxT(".") + extension;
   } while(!fileOkay);

   /*
    * Ensure that exporting a file by this name doesn't overwrite
    * one of the existing files in the project.  (If it would
    * overwrite an existing file, DirManager tries to rename the
    * existing file.)
    */

   if (!project->GetDirManager()->EnsureSafeFilename(wxFileName(fName)))
      return wxT("");

   gPrefs->Write(wxT("/DefaultExportPath"), path);

   *numChannels = channels;

   /*
    * To be even MORE safe, return a temporary file name based
    * on this one...
    */

   actualName = fName;

   int suffix = 0;
   while(::wxFileExists(fName)) {
      fName = path + endOfPathSep + 
         nameOnly + wxString::Format(wxT("%d"), suffix) + wxT(".") + extension;
      suffix++;
   }

   return fName;
}

bool ExportPCM(AudacityProject *project,
               bool selectionOnly, double t0, double t1)
{
   wxString fName;
   wxString formatStr;
   wxString extension;
   wxString actualName;
   bool     success = false;
   int      format;
   int      numChannels;
   MixerSpec *mixerSpec = NULL;
   
	try {  //lda

   format = ReadExportFormatPref();

   formatStr = sf_header_name(format & SF_FORMAT_TYPEMASK);
   extension = wxT(".") + sf_header_extension(format & SF_FORMAT_TYPEMASK);

   fName = ExportCommon(project, formatStr, extension,
                        selectionOnly, &t0, &t1, &numChannels,
                        actualName, 32, &mixerSpec);

   if (fName != wxT(""))
      success = ::ExportPCM(project, numChannels, fName,
                         selectionOnly, t0, t1, mixerSpec);

   if( mixerSpec )
      delete mixerSpec;

   if (success && actualName != fName)
      ::wxRenameFile(fName, actualName);

   return success;
}
   catch(...) {
      wxMessageBox(wxString::Format(_("File may be invalid or corrupted: %s"), 
                   (const wxChar *)project->GetName()), _("Error exporting file or project"),
                   wxOK | wxCENTRE);
     
      if( mixerSpec )
         delete mixerSpec;

      return false;
   }
}

bool ExportCompressed(AudacityProject *project, const wxString& format,
                      bool selectionOnly, double t0, double t1)
{
   wxString fName;
   int numChannels;
   wxString actualName;
   bool     success = false;
   MixerSpec *mixerSpec = NULL;

	try { //lda
   if( format == wxT("MP3") ) {
      fName = ExportCommon(project, wxT("MP3"), wxT(".mp3"),
                           selectionOnly, &t0, &t1, &numChannels,
                           actualName, 2, &mixerSpec);

      if (fName != wxT(""))
         success = ::ExportMP3(project, (numChannels == 2), fName,
                            selectionOnly, t0, t1, mixerSpec);
   }
   else if( format == wxT("OGG") ) {
#ifdef USE_LIBVORBIS
      fName = ExportCommon(project, wxT("OGG"), wxT(".ogg"),
                           selectionOnly, &t0, &t1, &numChannels,
                           actualName, 32, &mixerSpec);

      if (fName != wxT(""))
         success = ::ExportOGG(project, numChannels, fName,
                      selectionOnly, t0, t1, mixerSpec);
#else
      wxMessageBox(_("Ogg Vorbis support is not included in this build of Audacity"));
#endif
   }
   else if( format == wxT("FLAC") ) {
#ifdef USE_LIBFLAC
      fName = ExportCommon(project, wxT("FLAC"), wxT(".flac"),
                           selectionOnly, &t0, &t1, &numChannels,
                           actualName, 8, &mixerSpec);
      if (fName == wxT(""))
         return false;

      success = ::ExportFLAC(project, numChannels, fName,
                      selectionOnly, t0, t1,mixerSpec);
#else
      wxMessageBox(_("Flac support is not included in this build of Audacity"));
#endif
   }
   else if( format == wxT("External Program") ) {
#ifdef __WXGTK__
      wxString extension = gPrefs->Read( wxT("/FileFormats/ExternalProgramExportExtension"), wxT("") );
      fName = ExportCommon(project, wxT("External Program"), wxT(".") + extension,
                           selectionOnly, &t0, &t1, &numChannels,
                           actualName, 2, &mixerSpec);

      if (fName != wxT(""))
         success = ::ExportCL(project, (numChannels == 2), fName,
                           selectionOnly, t0, t1, mixerSpec);
#else
      wxMessageBox(_("Command-line exporting is only supported on UNIX"));
#endif
   }
   else if( format == wxT("MP2") ) {
#if USE_LIBTWOLAME
      fName = ExportCommon(project, wxT("MP2"), wxT(".mp2"),
                           selectionOnly, &t0, &t1, &numChannels,
                           actualName, 2, &mixerSpec);

      if (fName != wxT(""))
         success = ::ExportMP2(project, (numChannels == 2), fName,
                            selectionOnly, t0, t1, mixerSpec);
#else
      wxMessageBox(_("MP2 export support is not included in this build of Audacity"));
#endif
   }

   if (success && actualName != fName)
      ::wxRenameFile(fName, actualName);

   if( mixerSpec )
      delete mixerSpec;
   
   return success;
}
   catch(...) {
      wxMessageBox(wxString::Format(_("File may be invalid or corrupted: %s"), 
                   (const wxChar *)project->GetName()), _("Error exporting file or project"),
                   wxOK | wxCENTRE);
   
      if( mixerSpec )
         delete mixerSpec;

      return false;
   }
}

//----------------------------------------------------------------------------
// ExportMixerPanel
//----------------------------------------------------------------------------

BEGIN_EVENT_TABLE(ExportMixerPanel, wxPanel)
    EVT_PAINT(ExportMixerPanel::OnPaint)
    EVT_MOUSE_EVENTS(ExportMixerPanel::OnMouseEvent)
END_EVENT_TABLE()

ExportMixerPanel::ExportMixerPanel( MixerSpec *mixerSpec, 
      wxArrayString trackNames,wxWindow *parent, wxWindowID id, 
      const wxPoint& pos, const wxSize& size):
   wxPanel(parent, id, pos, size)
{
   mBitmap = NULL;
   mWidth = 0;
   mHeight = 0;
   mMixerSpec = mixerSpec;
   mSelectedTrack = mSelectedChannel = -1;

   mTrackRects = new wxRect[ mMixerSpec->GetNumTracks() ];
   mChannelRects = new wxRect[ mMixerSpec->GetMaxNumChannels() ];

   mTrackNames = trackNames;
}

ExportMixerPanel::~ExportMixerPanel()
{
   delete[] mTrackRects;
   delete[] mChannelRects;
}

//set the font on memDC such that text can fit in specified width and height
void ExportMixerPanel::SetFont( wxMemoryDC &memDC, wxString text, int width,
      int height )
{
   int l = 0, u = 13, m, w, h;
   wxFont font = memDC.GetFont();
   while( l < u - 1 )
   {
      m = ( l + u ) / 2;
      font.SetPointSize( m );
      memDC.SetFont( font );
      memDC.GetTextExtent( text, &w, &h );

      if( w < width && h < height )
         l = m;
      else
         u = m;
   }
   font.SetPointSize( l );
   memDC.SetFont( font );
}

void ExportMixerPanel::OnPaint(wxPaintEvent & evt)
{
   wxPaintDC dc( this );

   int width, height;
   GetSize( &width, &height );
 
   if( !mBitmap || mWidth != width || mHeight != height ) 
   {
      if( mBitmap )
         delete mBitmap;

      mWidth = width;
      mHeight = height;
      mBitmap = new wxBitmap( mWidth, mHeight );
   }

   wxColour bkgnd = GetBackgroundColour();
   wxBrush bkgndBrush( bkgnd, wxSOLID );
  
   wxMemoryDC memDC;
   memDC.SelectObject( *mBitmap );

   //draw background
   wxRect bkgndRect;
   bkgndRect.x = 0;
   bkgndRect.y = 0;
   bkgndRect.width = mWidth;
   bkgndRect.height = mHeight;

   memDC.SetBrush( *wxWHITE_BRUSH );
   memDC.SetPen( *wxBLACK_PEN );
   memDC.DrawRectangle( bkgndRect );

   //box dimensions
   mBoxWidth = mWidth / 6;
   
   mTrackHeight = ( mHeight * 3 ) / ( mMixerSpec->GetNumTracks() * 4 );
   if( mTrackHeight > 30 )
      mTrackHeight = 30;
   
   mChannelHeight = ( mHeight * 3 ) / ( mMixerSpec->GetNumChannels() * 4 );
   if( mChannelHeight > 30 )
      mChannelHeight = 30;

   static double PI = 2 * acos( 0.0 );
   double angle = atan( ( 3.0 * mHeight ) / mWidth );
   double radius = mHeight / ( 2.0 * sin( PI - 2.0 * angle ) );
   double totAngle = ( asin( mHeight / ( 2.0 * radius ) ) * 2.0 );

   //draw tracks
   memDC.SetBrush( AColor::envelopeBrush );
   angle = totAngle / ( mMixerSpec->GetNumTracks() + 1 );
  
   int max = 0, w, h;
   for( int i = 1; i < mMixerSpec->GetNumTracks(); i++ )
      if( mTrackNames[ i ].length() > mTrackNames[ max ].length() )
         max = i;
   
   SetFont( memDC, mTrackNames[ max ], mBoxWidth, mTrackHeight );
   
   for( int i = 0; i < mMixerSpec->GetNumTracks(); i++ )
   {
      mTrackRects[ i ].x = ( int )( mBoxWidth * 2 + radius - radius * 
         cos( totAngle / 2.0 - angle * ( i + 1 ) ) - mBoxWidth + 0.5 );
      mTrackRects[ i ].y = ( int )( mHeight * 0.5 - radius * 
            sin( totAngle * 0.5 - angle * ( i + 1.0 ) ) - 
            0.5 * mTrackHeight + 0.5 );

      mTrackRects[ i ].width = mBoxWidth;
      mTrackRects[ i ].height = mTrackHeight;
      
      memDC.SetPen( mSelectedTrack == i ? *wxRED_PEN : *wxBLACK_PEN );
      memDC.DrawRectangle( mTrackRects[ i ] );

      memDC.GetTextExtent( mTrackNames[ i ], &w, &h );
      memDC.DrawText( mTrackNames[ i ], 
            mTrackRects[ i ].x + ( mBoxWidth - w ) / 2, 
            mTrackRects[ i ].y + ( mTrackHeight - h ) / 2 );
   }

   //draw channels
   memDC.SetBrush( AColor::playRegionBrush[ 0 ] );
   angle = ( asin( mHeight / ( 2.0 * radius ) ) * 2.0 ) / 
      ( mMixerSpec->GetNumChannels() + 1 );

   SetFont( memDC, wxT( "Channel: XX" ), mBoxWidth, mChannelHeight );
   memDC.GetTextExtent( wxT( "Channel: XX" ), &w, &h );
      
   for( int i = 0; i < mMixerSpec->GetNumChannels(); i++ )
   {
      mChannelRects[ i ].x = ( int )( mBoxWidth * 4 - radius  + radius * 
         cos( totAngle * 0.5 - angle * ( i + 1 ) ) + 0.5 );
      mChannelRects[ i ].y = ( int )( mHeight * 0.5 - radius * 
            sin( totAngle * 0.5 - angle * ( i + 1 ) ) - 
            0.5 * mChannelHeight + 0.5 );

      mChannelRects[ i ].width = mBoxWidth;
      mChannelRects[ i ].height = mChannelHeight;

      memDC.SetPen( mSelectedChannel == i ? *wxRED_PEN : *wxBLACK_PEN );
      memDC.DrawRectangle( mChannelRects[ i ] );
      
      memDC.DrawText( wxString::Format( _( "Channel: %2d" ), i + 1 ),
            mChannelRects[ i ].x + ( mBoxWidth - w ) / 2,
            mChannelRects[ i ].y + ( mChannelHeight - h ) / 2 );
   }
  
   //draw links
   memDC.SetPen( wxPen( *wxBLACK, mHeight / 200 ) );
   for( int i = 0; i < mMixerSpec->GetNumTracks(); i++ )
      for( int j = 0; j < mMixerSpec->GetNumChannels(); j++ )
         if( mMixerSpec->mMap[ i ][ j ] )
            memDC.DrawLine( mTrackRects[ i ].x + mBoxWidth, 
                  mTrackRects[ i ].y + mTrackHeight / 2, mChannelRects[ j ].x,
                  mChannelRects[ j ].y + mChannelHeight / 2 );
   
   dc.Blit( 0, 0, mWidth, mHeight, &memDC, 0, 0, wxCOPY, FALSE );
}

double ExportMixerPanel::Distance( wxPoint &a, wxPoint &b )
{
   return sqrt( pow( a.x - b.x, 2.0 ) + pow( a.y - b.y, 2.0 ) );
}

//checks if p is on the line connecting la, lb with tolerence
bool ExportMixerPanel::IsOnLine( wxPoint p, wxPoint la, wxPoint lb )
{
   return Distance( p, la ) + Distance( p, lb ) - Distance( la, lb ) < 0.1;
}

void ExportMixerPanel::OnMouseEvent(wxMouseEvent & event)
{
   if( event.ButtonDown() ) 
   {
      CaptureMouse();

      bool reset = true;
      //check tracks 
      for( int i = 0; i < mMixerSpec->GetNumTracks(); i++ )
         if( mTrackRects[ i ].Inside( event.m_x, event.m_y ) )
         {
            reset = false;
            if( mSelectedTrack == i )
               mSelectedTrack = -1;
            else
            {
               mSelectedTrack = i;
               if( mSelectedChannel != -1 )
                  mMixerSpec->mMap[ mSelectedTrack ][ mSelectedChannel ] = 
                     !mMixerSpec->mMap[ mSelectedTrack ][ mSelectedChannel ];
            }
            goto found;
         }

      //check channels
      for( int i = 0; i < mMixerSpec->GetNumChannels(); i++ )
         if( mChannelRects[ i ].Inside( event.m_x, event.m_y ) )
         {
            reset = false;
            if( mSelectedChannel == i )
               mSelectedChannel = -1;
            else
            {
               mSelectedChannel = i;
               if( mSelectedTrack != -1 )
                  mMixerSpec->mMap[ mSelectedTrack ][ mSelectedChannel ] = 
                     !mMixerSpec->mMap[ mSelectedTrack ][ mSelectedChannel ];
            }
            goto found;
         }

      //check links
      for( int i = 0; i < mMixerSpec->GetNumTracks(); i++ )
         for( int j = 0; j < mMixerSpec->GetNumChannels(); j++ )
            if( mMixerSpec->mMap[ i ][ j ]  && IsOnLine( wxPoint( event.m_x,
                        event.m_y ), wxPoint( mTrackRects[ i ].x + mBoxWidth, 
                           mTrackRects[ i ].y + mTrackHeight / 2 ),
                     wxPoint( mChannelRects[ j ].x, mChannelRects[ j ].y + 
                     mChannelHeight / 2 ) ) )
               mMixerSpec->mMap[ i ][ j ] = false;

found:
      if( reset )
         mSelectedTrack = mSelectedChannel = -1;
      Refresh( false );
   }
   
   if( event.ButtonUp() ) 
   {
      if( HasCapture() )
         ReleaseMouse();
   }
}

//----------------------------------------------------------------------------
// ExportMixerDialog
//----------------------------------------------------------------------------

enum 
{
	ID_MIXERPANEL = 10001, 
	ID_SLIDER_CHANNEL
};

BEGIN_EVENT_TABLE( ExportMixerDialog,wxDialog )
   EVT_BUTTON( wxID_OK, ExportMixerDialog::OnOk )
   EVT_BUTTON( wxID_CANCEL, ExportMixerDialog::OnCancel )
   EVT_SIZE( ExportMixerDialog::OnSize )
   EVT_SLIDER( ID_SLIDER_CHANNEL, ExportMixerDialog::OnSlider )
END_EVENT_TABLE()

ExportMixerDialog::ExportMixerDialog( TrackList *tracks, bool selectionOnly,
      int maxNumChannels, wxWindow *parent, wxWindowID id, const wxString &title, 
      const wxPoint &position, const wxSize& size, long style ) :
   wxDialog( parent, id, title, position, size, style | wxRESIZE_BORDER )
{
   int numTracks = 0;
   TrackListIterator iter( tracks );
   
   for( Track *t = iter.First(); t; t = iter.Next() )
      if( t->GetKind() == Track::Wave && ( t->GetSelected() || !selectionOnly ) )
      {
         numTracks++;
         if( t->GetChannel() == Track::LeftChannel )
         {
            mTrackNames.Add( t->GetName() + _( " - Left" ) );
            mTrackNames.Add( t->GetName() + _( " - Right" ) );
            t = iter.Next();
            numTracks++;
         }
         else
            mTrackNames.Add( t->GetName() );
      }

   mMixerSpec = new MixerSpec( numTracks, maxNumChannels );

   wxBoxSizer *vertSizer = new wxBoxSizer( wxVERTICAL );

   wxWindow *mixerPanel = new ExportMixerPanel( mMixerSpec, mTrackNames, this, 
         ID_MIXERPANEL, wxDefaultPosition, wxSize( 400, -1 ) );
   vertSizer->Add( mixerPanel, 1, wxEXPAND | wxALIGN_CENTRE | wxALL, 5 );

   wxBoxSizer *horSizer = new wxBoxSizer( wxHORIZONTAL );
   
   mChannelsText = new wxStaticText( this, -1, 
         wxString::Format( _( "Output Channels: %2d" ), 
            mMixerSpec->GetNumChannels() ) );
   horSizer->Add( mChannelsText, 0, wxALIGN_LEFT | wxALL, 5 );

   wxSlider *channels = new wxSlider( this, ID_SLIDER_CHANNEL, 
         mMixerSpec->GetNumChannels(), 1, mMixerSpec->GetMaxNumChannels(),
         wxDefaultPosition, wxSize( 300, -1 ) );
   horSizer->Add( channels, 0, wxEXPAND | wxALL, 5 );
   
   vertSizer->Add( horSizer, 0, wxALIGN_CENTRE | wxALL, 5 );

   horSizer = new wxBoxSizer( wxHORIZONTAL );
   
   wxButton *cancel = new wxButton( this, wxID_CANCEL, _( "&Cancel" ) );
   horSizer->Add( cancel, 0, wxALIGN_CENTRE | wxALL, 5 );

   wxButton *ok = new wxButton( this, wxID_OK, _( "&OK" ) );
   ok->SetDefault();
   horSizer->Add( ok, 0, wxALIGN_CENTRE | wxALL, 5 );

   vertSizer->Add( horSizer, 0, wxALIGN_CENTRE | wxALL, 5 );

   SetAutoLayout( true );
   SetSizer( vertSizer );
   vertSizer->Fit( this );
   vertSizer->SetSizeHints( this );

   SetSizeHints( 640, 480, 20000, 20000 );

   SetSize( 640, 480 );
}

ExportMixerDialog::~ExportMixerDialog()
{
   if( mMixerSpec )
   {
      delete mMixerSpec;
      mMixerSpec = NULL;
   }
}

void ExportMixerDialog::OnSize(wxSizeEvent &event)
{
   ExportMixerPanel *pnl = ( ( ExportMixerPanel* ) FindWindow( ID_MIXERPANEL ) );
   pnl->Refresh( false );
   event.Skip();
}

void ExportMixerDialog::OnSlider( wxCommandEvent &event )
{
   wxSlider *channels = ( wxSlider* )FindWindow( ID_SLIDER_CHANNEL );
   ExportMixerPanel *pnl = ( ( ExportMixerPanel* ) FindWindow( ID_MIXERPANEL ) );
   mMixerSpec->SetNumChannels( channels->GetValue() );
   pnl->Refresh( false );
   mChannelsText->SetLabel( wxString::Format( _( "Output Channels: %2d" ), 
            mMixerSpec->GetNumChannels() ) );
}

void ExportMixerDialog::OnOk(wxCommandEvent &event)
{
   EndModal( wxID_OK );
}

void ExportMixerDialog::OnCancel(wxCommandEvent &event)
{
   EndModal( wxID_CANCEL );
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
// arch-tag: e6901653-9e2a-4a97-8ba8-377928b8e45a

