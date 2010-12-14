/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.cpp

  Dominic Mazzoni

*******************************************************************//**

\class Export
\brief Main class to control the export function.

*//****************************************************************//**

\class ExportType
\brief Container for information about supported export types.

*//****************************************************************//**

\class ExportMixerDialog
\brief Dialog for advanced mixing.

*//****************************************************************//**

\class ExportMixerPanel
\brief Panel that displays mixing for advanced mixing option.

*//********************************************************************/

#include <wx/dynarray.h>
#include <wx/file.h>
#include <wx/filename.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textctrl.h>
#include <wx/timer.h>

#include "Export.h"
#include "ExportPCM.h"
#include "ExportMP3.h"
#include "ExportOGG.h"
#include "ExportFLAC.h"
#include "ExportCL.h"
#include "ExportMP2.h"

#include "sndfile.h"

#include "FileDialog.h"

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

// Callback to display format options
static void ExportCallback(void *cbdata, int index)
{
   ((Exporter *) cbdata)->DisplayOptions(index);
}

//----------------------------------------------------------------------------
// ExportPlugin
//----------------------------------------------------------------------------
#include <wx/arrimpl.cpp>

WX_DEFINE_OBJARRAY(ExportPluginArray);

ExportPlugin::ExportPlugin()
{
   mMaxChannels = 0;
   mCanMetaData = false;
}

ExportPlugin::~ExportPlugin()
{
}

void ExportPlugin::Destroy()
{
   delete this;
}

void ExportPlugin::SetFormat(const wxString & format)
{
   mFormat = format;
}

void ExportPlugin::SetDescription(const wxString & description)
{
   mDescription = description;
}

void ExportPlugin::SetExtension(const wxString & extension)
{
   mExtension = extension;
}

void ExportPlugin::SetExtensions(const wxArrayString & extensions)
{
   mExtensions = extensions;
}

void ExportPlugin::SetMask(const wxString & mask)
{
   mMask = mask;
}

void ExportPlugin::SetMaxChannels(int maxchannels)
{
   mMaxChannels = maxchannels;
}

void ExportPlugin::SetCanMetaData(bool canmetadata)
{
   mCanMetaData = canmetadata;
}

wxString ExportPlugin::GetFormat()
{
   return mFormat;
}

wxString ExportPlugin::GetDescription()
{
   return mDescription;
}

wxString ExportPlugin::GetExtension()
{
   return mExtension;
}

wxArrayString ExportPlugin::GetExtensions()
{
   return mExtensions;
}

wxString ExportPlugin::GetMask()
{
   if (!mMask.IsEmpty()) {
      return mMask;
   }

   wxString mask = GetDescription() + wxT("|");

   // Build the mask, but cater to the Mac FileDialog and put the default
   // extension at the end of the mask.
   wxString ext = GetExtension();
   wxArrayString exts = GetExtensions();
   for (size_t i = 0; i < exts.GetCount(); i++) {
      if (ext != exts[i]) {
         mask += wxT("*.") + exts[i] + wxT(";");
      }
   }

   return mask + wxT("*.") + ext;
}

int ExportPlugin::GetMaxChannels()
{
   return mMaxChannels;
}

bool ExportPlugin::GetCanMetaData()
{
   return mCanMetaData;
}

bool ExportPlugin::IsExtension(wxString & ext)
{
   return GetExtension() == wxT("") ||
          GetExtensions().Index(ext, false) != wxNOT_FOUND;
}

bool ExportPlugin::DisplayOptions(AudacityProject *project)
{
   if (project == NULL) {
      project = GetActiveProject();
   }

   return DoDisplayOptions(project);
}

bool ExportPlugin::DoDisplayOptions(AudacityProject *project)
{
   return false;
}

bool ExportPlugin::Export(AudacityProject *project,
                          int channels,
                          wxString fName,
                          bool selectedOnly,
                          double t0,
                          double t1,
                          MixerSpec *mixerSpec,
                          Tags *metadata)
{
   if (project == NULL) {
      project = GetActiveProject();
   }

  return DoExport(project, channels, fName, selectedOnly, t0, t1, mixerSpec);
}

bool ExportPlugin::DoExport(AudacityProject *project,
                            int channels,
                            wxString fName,
                            bool selectedOnly,
                            double t0,
                            double t1,
                            MixerSpec *mixerSpec)
{
   return false;
}

//----------------------------------------------------------------------------
// Export
//----------------------------------------------------------------------------

Exporter::Exporter()
{
   mMixerSpec = NULL;

   RegisterPlugin(New_ExportPCM());
   RegisterPlugin(New_ExportMP3());

#ifdef USE_LIBVORBIS
   RegisterPlugin(New_ExportOGG());
#endif

#ifdef USE_LIBFLAC
   RegisterPlugin(New_ExportFLAC());
#endif

#if USE_LIBTWOLAME
   RegisterPlugin(New_ExportMP2());
#endif

   // Command line export not available on Windows and Mac platforms
   RegisterPlugin(New_ExportCL());
}

Exporter::~Exporter()
{
   for (size_t i = 0; i < mPlugins.GetCount(); i++) {
      mPlugins[i]->Destroy();
   }
   mPlugins.Clear();

   if (mMixerSpec) {
      delete mMixerSpec;
   }
}

void Exporter::RegisterPlugin(ExportPlugin *ExportPlugin)
{
   mPlugins.Add(ExportPlugin);
}

const ExportPluginArray Exporter::GetPlugins()
{
   return mPlugins;
}

#if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   wxString gStrInstrument = wxT("Mix"); // default
#endif

bool Exporter::Process(AudacityProject *project, bool selectedOnly, double t0, double t1)
{
   // Save parms
   mProject = project;
   mSelectedOnly = selectedOnly;
   mT0 = t0;
   mT1 = t1;

   // Gather track information
   if (!ExamineTracks()) {
      return false;
   }

   // Ask user for file name
   if (!GetFilename()) {
      return false;
   }

#if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   project->GetTags()->SetTag(wxT("INSTRUMENT"), gStrInstrument); 
#else
   // Let user edit MetaData 
   if (!(project->GetTags()->ShowEditDialog(project, _("Edit Metadata"), mProject->GetShowId3Dialog()))) {
      return false;
   }
#endif

   // Check for down mixing
   if (!CheckMix()) {
      return false;
   }

   // Export the tracks
   bool success = ExportTracks();

   // Get rid of mixerspec
   if (mMixerSpec) {
      delete mMixerSpec;
      mMixerSpec = NULL;
   }

   return success;
}

bool Exporter::Process(AudacityProject *project, int numChannels,
                       const wxChar *type, const wxString filename,
                       bool selectedOnly, double t0, double t1)
{
   // Save parms
   mProject = project;
   mChannels = numChannels;
   mFilename = filename;
   mSelectedOnly = selectedOnly;
   mT0 = t0;
   mT1 = t1;
   mActualName = mFilename;

   for (size_t i = 0; i < mPlugins.GetCount(); i++) {
      if (mPlugins[i]->GetFormat().IsSameAs(type, false)) {
         mFormat = i;
         return ExportTracks();
      }
   }

   return false;
}

#if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   #include <wx/choicdlg.h>

   // GetValidInstrumentName() sets gStrInstrument to a valid instrument name if necessary.
   // Returns true if name changed.
   bool GetValidInstrumentName(const wxString str)
   {
      wxArrayString strChoices;
      strChoices.Add(wxT("Lead Vocals"));
      strChoices.Add(wxT("Backing Vocals"));
      strChoices.Add(wxT("Lead Guitar"));
      strChoices.Add(wxT("Rhythm Guitar"));
      strChoices.Add(wxT("Keyboards"));
      strChoices.Add(wxT("Bass"));
      strChoices.Add(wxT("Drums"));
      strChoices.Add(wxT("Percussion"));
      strChoices.Add(wxT("Other"));
      bool bChangeStr = (strChoices.Index(str) == wxNOT_FOUND); // str is not a valid instrument.
      if (bChangeStr) 
      {
         gStrInstrument = 
            ::wxGetSingleChoice(
               _("Current track needs a valid instrument name."), 
               _("Choose an Instrument"), 
               strChoices);
         //if (gStrInstrument == wxT("")) // User canceled. 
         //   gStrInstrument = strChoices.Last(); // (wxT("Other Craziness!"));
      }
      else
         gStrInstrument = str;
      return bChangeStr;
   }
#endif

bool Exporter::ExamineTracks()
{
   // Init
   mNumSelected = 0;
   mNumLeft = 0;
   mNumRight = 0;
   mNumMono = 0;

   // First analyze the selected audio, perform sanity checks, and provide
   // information as appropriate.

   // Tally how many are right, left, mono, and make sure at
   // least one track is selected (if selectedOnly==true)

   float earliestBegin = mT1;
   float latestEnd = mT0;

   TrackList *tracks = mProject->GetTracks();
   TrackListIterator iter1(tracks);
   Track *tr = iter1.First();

   while (tr) {
      if (tr->GetKind() == Track::Wave) {
         if (tr->GetSelected() || !mSelectedOnly) {

            mNumSelected++;

            if (tr->GetChannel() == Track::LeftChannel) {
               mNumLeft++;
            }
            else if (tr->GetChannel() == Track::RightChannel) {
               mNumRight++;
            }
            else if (tr->GetChannel() == Track::MonoChannel) {
               // It's a mono channel, but it may be panned
               float pan = ((WaveTrack*)tr)->GetPan();
               
               if (pan == -1.0)
                  mNumLeft++;
               else if (pan == 1.0)
                  mNumRight++;
               else if (pan == 0)
                  mNumMono++;
               else {
                  // Panned partially off-center. Mix as stereo.
                  mNumLeft++;
                  mNumRight++;
               }
            }

            if (tr->GetOffset() < earliestBegin) {
               earliestBegin = tr->GetOffset();
            }

            if (tr->GetEndTime() > latestEnd) {
               latestEnd = tr->GetEndTime();
            }

         }
      }

      tr = iter1.Next();
   }

   if (mSelectedOnly && mNumSelected == 0) {
      wxMessageBox(_("No tracks are selected! Use Ctrl-A (Select All)\nChoose Export... to export all tracks."),
                    _("Unable to export"),
                    wxOK | wxICON_INFORMATION);
      return false;
   }
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      // In order for exported files to line up, i.e., to fake their start time, 
      // mix down with silence from 0.0s, which inserts silence in the track. 
      if (mT0 > 0.0)
         mT0 = 0.0;
      if (earliestBegin > 0.0)
         earliestBegin = 0.0;

      if (mSelectedOnly && 
            ((mNumSelected == 1) || // single mono Track
               ((mNumSelected == 2) && (mNumLeft == 1) && (mNumRight == 1)))) // single stereo Track
      {
         tr = iter1.First();
         while (!tr->GetSelected()) 
            tr = iter1.Next();
         wxASSERT(tr);

         if (GetValidInstrumentName(tr->GetName())) 
         {
            if (gStrInstrument == wxT("")) // User canceled. 
               return false;
            tr->SetName(gStrInstrument); 
         }
      }
      else
         gStrInstrument = wxT("Mix"); 
   #endif

   if (mT0 < earliestBegin)
      mT0 = earliestBegin;

   if (mT1 > latestEnd)
      mT1 = latestEnd;

   return true;
}

//
// For safety, if the file already exists it stores the filename
// the user wants in actualName, and returns a temporary file name.
// The calling function should rename the file when it's successfully
// exported.
//
bool Exporter::GetFilename()
{
   mFormat = 0;

   wxString maskString;
#if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   wxString defaultFormat = wxT("OGG");
#else
   wxString defaultFormat = gPrefs->Read(wxT("/Export/Format"),
                                         wxT("WAV"));
#endif

   for (size_t i = 0; i < mPlugins.GetCount(); i++) {
      maskString += mPlugins[i]->GetMask() + wxT("|");

      if (mPlugins[i]->GetFormat() == defaultFormat) {
         mFormat = i;
      }
   }
   maskString.RemoveLast();

   mFilename.SetPath(gPrefs->Read(wxT("/Export/Path"), ::wxGetCwd()));
#if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   wxString strName;
   if (mSelectedOnly && 
         ((mNumSelected == 1) || // single mono Track
            ((mNumSelected == 2) && (mNumLeft == 1) && (mNumRight == 1)))) // single stereo Track
      strName = gStrInstrument;
   else
      strName = wxT("Mix");
   strName += wxT("-") + mProject->GetName();
   mFilename.SetName(strName);
#else
   mFilename.SetName(mProject->GetName());
#endif

   while (true) {

      FileDialog fd(mProject,
                    _("Export File"),
                    mFilename.GetPath(),
                    mFilename.GetFullName(),
                    maskString,
                    wxSAVE);

      fd.SetFilterIndex(mFormat);

      fd.EnableButton(_("&Options..."), ExportCallback, this);

      if (fd.ShowModal() == wxID_CANCEL) {
         return false;
      }

      mFilename = fd.GetPath();
      mFormat = fd.GetFilterIndex();

      if (mFilename == wxT("")) {
         return false;
      }

      wxString ext = mFilename.GetExt();
      wxString defext = mPlugins[mFormat]->GetExtension().Lower();

      //
      // Check the extension - add the default if it's not there,
      // and warn user if it's abnormal.
      //
      if (ext.IsEmpty()) {
         //
         // Make sure the user doesn't accidentally save the file
         // as an extension with no name, like just plain ".wav".
         //
         if (mFilename.GetName().Left(1) == wxT(".")) {
            wxString prompt = _("Are you sure you want to save the file as \"") +
                              mFilename.GetFullName() +
                              wxT("\"?\n");
            
            int action = wxMessageBox(prompt,
                                      _("Warning"),
                                      wxYES_NO | wxICON_EXCLAMATION);
            if (action != wxYES) {
               continue;
            }
         }

         mFilename.SetExt(defext);
      }
      else if (!ext.IsEmpty() && !mPlugins[mFormat]->IsExtension(ext) && ext.CmpNoCase(defext)) {
         wxString prompt;
         prompt.Printf(_("You are about to save a %s file with the name \"%s\".\n\nNormally these files end in \".%s\", and some programs will not open files with nonstandard extensions.\n\nAre you sure you want to save the file under this name?"),
                       mPlugins[mFormat]->GetFormat().c_str(),
                       mFilename.GetFullName().c_str(),
                       defext.c_str());

         int action = wxMessageBox(prompt,
                                   _("Warning"),
                                   wxYES_NO | wxICON_EXCLAMATION);
         if (action != wxYES) {
            continue;
         }
      }

      if (mFilename.GetFullPath().Length() >= 256) {
         wxMessageBox(_("Sorry, pathnames longer than 256 characters not supported."));
         continue;
      }

      if (mFilename.FileExists()) {
         wxString prompt;

         prompt.Printf(_("A file named \"%s\" already exists.  Replace?"),
                       mFilename.GetFullPath().c_str());
         
         int action = wxMessageBox(prompt,
                                   _("Warning"),
                                   wxYES_NO | wxICON_EXCLAMATION);
         if (action != wxYES) {
            continue;
         }
      }
         
      break;
   }

   //
   // Ensure that exporting a file by this name doesn't overwrite
   // one of the existing files in the project.  (If it would
   // overwrite an existing file, DirManager tries to rename the
   // existing file.)
   //

   if (!mProject->GetDirManager()->EnsureSafeFilename(mFilename))
      return false;

   gPrefs->Write(wxT("/Export/Format"), mPlugins[mFormat]->GetFormat());
   gPrefs->Write(wxT("/Export/Path"), mFilename.GetPath());

   //
   // To be even safer, return a temporary file name based
   // on this one...
   //

   mActualName = mFilename;

   int suffix = 0;
   while (mFilename.FileExists()) {
      mFilename.SetName(mActualName.GetName() +
                        wxString::Format(wxT("%d"), suffix));
      suffix++;
   }

   return true;
}

void Exporter::DisplayOptions(int index)
{
   // This shouldn't happen...
   if (index >= (int)mPlugins.GetCount()) {
      return;
   }

   mPlugins[index]->DisplayOptions(mProject);
}

bool Exporter::CheckMix()
{
   // Clean up ... should never happen
   if (mMixerSpec) {
      delete mMixerSpec;
      mMixerSpec = NULL;
   }

   // Detemine if exported file will be stereo or mono or multichannel,
   // and if mixing will occur.

   int downMix = gPrefs->Read(wxT("/FileFormats/ExportDownMix"), true);

   if (downMix) {
      if (mNumRight > 0 || mNumLeft > 0) {
         mChannels = 2;
      }
      else {
         mChannels = 1;
      }

      int numLeft =  mNumLeft + mNumMono;
      int numRight = mNumRight + mNumMono;
   
      if (numLeft > 1 || numRight > 1)
         if (mChannels == 2) {
            ShowWarningDialog(mProject,
                              wxT("MixStereo"),
                              _("Your tracks will be mixed down to two stereo channels in the exported file."));
         }
         else {
            ShowWarningDialog(mProject,
                              wxT("MixMono"),
                              _("Your tracks will be mixed down to a single mono channel in the exported file."));
         }
   }
   else
   {
      ExportMixerDialog md(mProject->GetTracks(),
                           mSelectedOnly,
                           mPlugins[mFormat]->GetMaxChannels(),
                           NULL, 
                           1,
                           _("Advanced Mixing Options"));
      
      if (md.ShowModal() != wxID_OK) {
         return false;
      }

      mMixerSpec = new MixerSpec(*(md.GetMixerSpec()));
      mChannels = mMixerSpec->GetNumChannels();
   }

   return true;
}

bool Exporter::ExportTracks()
{
   bool success;

   // Keep original in case of failure
   if (mActualName != mFilename) {
      ::wxRenameFile(mActualName.GetFullPath(), mFilename.GetFullPath());
   }

   success = mPlugins[mFormat]->Export(mProject,
                                    mChannels,
                                    mActualName.GetFullPath(),
                                    mSelectedOnly,
                                    mT0,
                                    mT1,
                                    mMixerSpec);

   if (mActualName != mFilename) {
      // Remove backup
      if (success) {
         ::wxRemoveFile(mFilename.GetFullPath());
      }
      else {
         // Restore original, if needed
         ::wxRemoveFile(mActualName.GetFullPath());
         ::wxRenameFile(mFilename.GetFullPath(), mActualName.GetFullPath());
      }
   }

   return success;
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

ExportMixerDialog::ExportMixerDialog( TrackList *tracks, bool selectedOnly,
      int maxNumChannels, wxWindow *parent, wxWindowID id, const wxString &title, 
      const wxPoint &position, const wxSize& size, long style ) :
   wxDialog( parent, id, title, position, size, style | wxRESIZE_BORDER )
{
   int numTracks = 0;
   TrackListIterator iter( tracks );
   
   for( Track *t = iter.First(); t; t = iter.Next() )
   {
      if( t->GetKind() == Track::Wave && ( t->GetSelected() || !selectedOnly ) )
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
   }

   // JKC: This is an attempt to fix a 'watching brief' issue, where the slider is
   // sometimes not slidable.  My suspicion is that a mixer may incorrectly
   // state the number of channels - so we assume there are always at least two.
   // The downside is that if someone is exporting to a mono device, the dialog
   // will allow them to output to two channels. Hmm.  We may need to revisit this.
   if (maxNumChannels < 2 )
      maxNumChannels = 2;
   if (maxNumChannels > 32)
      maxNumChannels = 32;

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

   vertSizer->Add( CreateStdButtonSizer(this, eCancelButton|eOkButton), 0, wxEXPAND );

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

