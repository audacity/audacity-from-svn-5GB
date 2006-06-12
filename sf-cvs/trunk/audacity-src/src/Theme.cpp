/**********************************************************************

  Audacity: A Digital Audio Editor

  Theme.cpp

  James Crook
   
  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

********************************************************************//**

\class Theme
\brief Based on ThemeBase, Theme manages image and icon resources.

   Theme is a class which manages theme resources.
   It maps sets of ids to the resources and to names of the resources,
   so that they can be loaded/saved from files.

\see \ref Themability

*//********************************************************************/

#include <wx/wx.h>
#include <wx/image.h>
#include <wx/arrimpl.cpp> 
#include <wx/file.h>
#include <wx/mstream.h>
#include "Project.h" 
#include "ToolBar.h"
#include "ControlToolBar.h"
#include "ImageManipulation.h"
#include "Theme.h"
#include "FileNames.h"
#include "Prefs.h"


WX_DEFINE_OBJARRAY( ArrayOfBitmaps );
WX_DEFINE_OBJARRAY( ArrayOfColours );

#include "../images/ControlButtons.h"
#include "../images/EditButtons.h"
#include "../images/MixerImages.h"
#include "../images/Cursors.h"
#include "../images/ToolBarButtons.h"

#include "../images/ToolsButtons/Draw.xpm"
#include "../images/ToolsButtons/DrawAlpha.xpm"
#include "../images/ToolsButtons/Envelope.xpm"
#include "../images/ToolsButtons/EnvelopeAlpha.xpm"
#include "../images/ToolsButtons/IBeam.xpm"
#include "../images/ToolsButtons/IBeamAlpha.xpm"
#include "../images/ToolsButtons/Multi.xpm"
#include "../images/ToolsButtons/MultiAlpha.xpm"
#include "../images/ToolsButtons/TimeShift.xpm"
#include "../images/ToolsButtons/TimeShiftAlpha.xpm"
#include "../images/ToolsButtons/Zoom.xpm"
#include "../images/ToolsButtons/ZoomAlpha.xpm"

#include "../images/ExpandingToolBar/ToolBarToggle.xpm"
#include "../images/ExpandingToolBar/ToolBarTarget.xpm"
#include "../images/ExpandingToolBar/ToolBarGrabber.xpm"


// This declares the variables such as
// int BmpRecordButton = -1;
#define THEME_DECLARATIONS
#include "AllThemeResources.h"

#if 0
#include "../images/ToolsButtons.h"
#include "../images/TranscriptionButtons.h"

// Include files to get the default images
#include "../images/Aqua.xpm"
#include "../images/Arrow.xpm"
#include "../images/AudacityLogo.xpm"
#include "../images/AudacityLogo48x48.xpm"
#include "../images/SliderThumb.xpm"
#include "../images/SliderThumbAlpha.xpm"
#endif

#if 0
#include "../images/Aqua/Down.xpm"
#include "../images/Aqua/DownButtonSquare.xpm"
#include "../images/Aqua/DownButtonStripes.xpm"
#include "../images/Aqua/DownButtonWhite.xpm"
#include "../images/Aqua/Hilite.xpm"
#include "../images/Aqua/HiliteButtonSquare.xpm"
#include "../images/Aqua/HiliteButtonStripes.xpm"
#include "../images/Aqua/HiliteButtonWhite.xpm"
#include "../images/Aqua/Slider.xpm"
#include "../images/Aqua/SliderThumb.xpm"
#include "../images/Aqua/Up.xpm"
#include "../images/Aqua/UpButtonSquare.xpm"
#include "../images/Aqua/UpButtonStripes.xpm"
#include "../images/Aqua/UpButtonWhite.xpm"


#include "../images/ControlButtons/Disabled.xpm"
#include "../images/ControlButtons/Down.xpm"
#include "../images/ControlButtons/DownButton.xpm"
#include "../images/ControlButtons/Hilite.xpm"
#include "../images/ControlButtons/HiliteButton.xpm"
#include "../images/ControlButtons/Slider.xpm"
#include "../images/ControlButtons/SliderThumb.xpm"
#include "../images/ControlButtons/Up.xpm"
#include "../images/ControlButtons/UpButton.xpm"
#include "../images/ControlButtons/Zoom.xpm"
#include "../images/ControlButtons/ZoomAlpha.xpm"


#include "../images/ToolBarImages/DockDown.xpm"
#include "../images/ToolBarImages/DockDownShort.xpm"
#include "../images/ToolBarImages/DockOver.xpm"
#include "../images/ToolBarImages/DockOverShort.xpm"
#include "../images/ToolBarImages/DockUp.xpm"
#include "../images/ToolBarImages/DockUpShort.xpm"

#include "../images/TranscriptionImages/Automate.xpm"
#include "../images/TranscriptionImages/AutomateSelection.xpm"
#include "../images/TranscriptionImages/AutomateSelectionAlpha.xpm"
#include "../images/TranscriptionImages/AutomateSelectionDisabled.xpm"
#include "../images/TranscriptionImages/CalibrateAlpha.xpm"
#include "../images/TranscriptionImages/CalibrateDisabled.xpm"
#include "../images/TranscriptionImages/CalibrateUp.xpm"
#include "../images/TranscriptionImages/Down.xpm"
#include "../images/TranscriptionImages/EndOff.xpm"
#include "../images/TranscriptionImages/EndOffAlpha.xpm"
#include "../images/TranscriptionImages/EndOffDisabled.xpm"
#include "../images/TranscriptionImages/EndOn.xpm"
#include "../images/TranscriptionImages/EndOnAlpha.xpm"
#include "../images/TranscriptionImages/EndOnDisabled.xpm"
#include "../images/TranscriptionImages/Hilite.xpm"
#include "../images/TranscriptionImages/MakeTag.xpm"
#include "../images/TranscriptionImages/MakeTagAlpha.xpm"
#include "../images/TranscriptionImages/MakeTagDisabled.xpm"
#include "../images/TranscriptionImages/SelectSilence.xpm"
#include "../images/TranscriptionImages/SelectSilenceAlpha.xpm"
#include "../images/TranscriptionImages/SelectSilenceDisabled.xpm"
#include "../images/TranscriptionImages/SelectSound.xpm"
#include "../images/TranscriptionImages/SelectSoundAlpha.xpm"
#include "../images/TranscriptionImages/SelectSoundDisabled.xpm"
#include "../images/TranscriptionImages/StartOff.xpm"
#include "../images/TranscriptionImages/StartOffAlpha.xpm"
#include "../images/TranscriptionImages/StartOffDisabled.xpm"
#include "../images/TranscriptionImages/StartOn.xpm"
#include "../images/TranscriptionImages/StartOnAlpha.xpm"
#include "../images/TranscriptionImages/StartOnDisabled.xpm"
#include "../images/TranscriptionImages/Up.xpm"
#endif


// Include the ImageCache...
unsigned char ImageCacheAsData[] = {
#include "ThemeAsCeeCode.h"
};

// theTheme is a global variable.
Theme theTheme;

Theme::Theme(void)
{
   mbInitialised=false;
}

Theme::~Theme(void)
{
}


void Theme::EnsureInitialised()
{
   if( mbInitialised )
      return;
   RegisterImages();
   RegisterColours();

   bool bLoadThemeAtStart;
   gPrefs->Read( wxT("/Theme/LoadAtStart"), &bLoadThemeAtStart, false );
   LoadThemeAtStartUp( bLoadThemeAtStart );
}

void Theme::ApplyUpdatedImages()
{
   AudacityProject *p = GetActiveProject();
   if( p->GetControlToolBar() )
   {
      p->GetControlToolBar()->ReCreateButtons();     
   }
}

void Theme::RegisterImages()
{
   if( mbInitialised )
      return;
   mFlags = resFlagPaired;
   mOldFlags = mFlags;
   mbInitialised = true;
   int i=0;

// This initialises the variables e.g
// RegisterBitmap( bmpRecordButton, some bitmap, wxT("RecordButton"));
#define THEME_INITS
#include "AllThemeResources.h"


}


void Theme::RegisterColours()
{
   RegisterColour( clrBlank,      wxColour(214, 214, 214), wxT("Blank"));
   RegisterColour( clrUnselected, wxColour(192, 192, 192), wxT("Unselected"));
   RegisterColour( clrSelected,   wxColour(148, 148, 170), wxT("Selected"));
   RegisterColour( clrSample,     wxColour( 50,  50, 200), wxT("Sample"));
   RegisterColour( clrSelSample,  wxColour( 50,  50, 200), wxT("SelSample"));
   RegisterColour( clrDragSample, wxColour(  0,   0,   0), wxT("DragSample"));
                                                                
   RegisterColour( clrMuteSample, wxColour(136, 136, 144), wxT("MuteSample"));
   RegisterColour( clrRms,        wxColour(100, 100, 220), wxT("Rms"));
   RegisterColour( clrMuteRms,    wxColour(136, 136, 144), wxT("MuteRms"));
   RegisterColour( clrShadow,     wxColour(148, 148, 148), wxT("Shadow"));
}

ThemeBase::ThemeBase(void)
{
}

ThemeBase::~ThemeBase(void)
{
}

/// This function is called to load the initial Theme images.
/// There are many possible choices for what this function
/// should do, as we have (potentially) four sources of images.
///   - (deprecated) programmed in XPMs.
///   - Programmed in in-built theme.
///   - External image Cache file.
///   - External component files.
///
/// We currently still have the deprecated XPMs, so we have
/// those being used if the user decides not to load themes.
///
/// @param bLookForExternalFiles uses file iff true.
void ThemeBase::LoadThemeAtStartUp( bool bLookForExternalFiles )
{
   EnsureInitialised();

   const bool cbBinaryRead =true;
   const bool cbOkIfNotFound = true;

   // IF not interested in external files, 
   // THEN just use the internal default set.
   if( !bLookForExternalFiles )
   {
      // JKC: Next line commented out for the moment!
      // We have an alternative set of defaults already in place.
//    ReadThemeInternal();
      return;
   }
   // ELSE IF can't read the external image cache.
   else if( !ReadImageCache( cbBinaryRead, cbOkIfNotFound ) )
   {
      // THEN get the default set.
      ReadThemeInternal();

      // JKC: Now we could go on and load the individual images
      // on top of the default images using the commented out 
      // code that follows...
      //
      // However, I think it is better to get the user to 
      // build a new image cache, which they can do easily
      // from the Theme preferences tab.
#if 0
      // and now add any available component images.
      LoadComponents( cbOkIfNotFound );

      // JKC: I'm usure about doing this next step automatically.
      // Suppose the disk is write protected?
      // Is having the image cache created automatically 
      // going to confuse users?  Do we need version specific names?
      // and now save the combined image as a cache for later use.
      // We should load the images a little faster in future as a result.
      CreateImageCache();
#endif
   }

   // Next line is not required as we haven't yet built the GUI 
   // when this function is (or should be) called.
   // ApplyUpdatedImages();
}


wxBitmap ThemeBase::MaskedBmp( char const ** pXpm, char const ** pMask )
{
   wxBitmap Bmp1( pXpm );
   wxBitmap Bmp2( pMask );

//   wxLogDebug( wxT("Image 1: %i Image 2: %i"), 
//      Bmp1.GetDepth(), Bmp2.GetDepth() );

   // We want a 24-bit-depth bitmap if all is working, but on some
   // platforms it might just return -1 (which means best available
   // or not relevant).
   // JKC: \todo check that we're not relying on 24 bit elsewhere.
   wxASSERT( Bmp1.GetDepth()==-1 || Bmp1.GetDepth()==24);
   wxASSERT( Bmp1.GetDepth()==-1 || Bmp2.GetDepth()==24);

   int i,nBytes;
   nBytes = Bmp1.GetWidth() * Bmp1.GetHeight();
   wxImage Img1( Bmp1.ConvertToImage());
   wxImage Img2( Bmp2.ConvertToImage());

   unsigned char *src = Img1.GetData();
   unsigned char *mk = Img2.GetData();
   unsigned char *alpha = (unsigned char*)malloc( nBytes );

   // Extract alpha channel from second XPM.
   for(i=0;i<nBytes;i++)
   {
      alpha[i] = mk[0];
      mk+=3;
   }

   Img1.SetAlpha( alpha);

   //dmazzoni: the top line does not work on wxGTK
   //wxBitmap Result( Img1, 32 );
   wxBitmap Result( Img1 );

   return Result;
}

void ThemeBase::RegisterBitmap( int &iIndex, char const ** pXpm, const wxString & Name )
{

   wxASSERT( iIndex == -1 ); // Don't initialise same bitmap twice!
   wxBitmap Bmp( pXpm ); // a 24 bit bitmap.
   wxImage Img( Bmp.ConvertToImage() );
   Img.InitAlpha();

   //dmazzoni: the top line does not work on wxGTK
   //wxBitmap Bmp2( Img, 32 );
   wxBitmap Bmp2( Img );

   RegisterBitmap( iIndex, Bmp2, Name );
}

void ThemeBase::RegisterBitmap( int &iIndex, const wxBitmap &Bmp, const wxString & Name )
{
   wxASSERT( iIndex == -1 ); // Don't initialise same bitmap twice!
   mBitmaps.Add( Bmp );
   mBitmapNames.Add( Name );
   mBitmapFlags.Add( mFlags );
   iIndex = mBitmaps.GetCount()-1;
}

void ThemeBase::RegisterColour( int iIndex, const wxColour &Clr, const wxString & Name )
{
//   wxASSERT( iIndex == -1 ); // Don't initialise same colour twice!
   mColours.Add( Clr );
   mColourNames.Add( Name );
}

void ThemeBase::SetNewGroup( int iGroupSize )
{
   myPosBase +=myHeight * iImageGroupSize;
   mxPos =0;
   mOldFlags = mFlags;
   iImageGroupSize = iGroupSize;
   iImageGroupIndex = -1;
}

void ThemeBase::GetNextPosition( int xSize, int ySize )
{
   // if the height has increased, then we are on a new group.
   if(( ySize > myHeight )||(mFlags != mOldFlags ))
   {
      SetNewGroup( mFlags && resFlagPaired ? 2 : 1 );
      mOldFlags = mFlags;
   }
   myHeight = ySize;

   iImageGroupIndex++;
   if( iImageGroupIndex == iImageGroupSize )
   {
      iImageGroupIndex = 0;
      mxPos += xSize;
   }

   if(mxPos > (mxCacheWidth - xSize ))
   {
      SetNewGroup(iImageGroupSize);
      iImageGroupIndex++;
   }
   myPos = myPosBase + iImageGroupIndex * myHeight;
}
   
/// \brief Helper class based on wxOutputStream used to get a png file in text format
///
/// The trick used here is that wxWidgets can write a PNG image to a stream.
/// By writing to a custom stream, we get to see each byte of data in turn, convert
/// it to text, put in commas, and then write that out to our own text stream.
class SourceOutputStream : public wxOutputStream
{
public:
   SourceOutputStream(){;};
   int OpenFile(const wxString & Filename);
   virtual ~SourceOutputStream();

protected:
   virtual size_t OnSysWrite(const void *buffer, size_t bufsize);
   wxFile File;
   int nBytes;
};

/// Opens the file and also adds a standard comment at the start of it.
int SourceOutputStream::OpenFile(const wxString & Filename)
{
   nBytes = 0;
   bool bOk;
   bOk = File.Open( Filename, wxFile::write );
   if( bOk )
   {
      File.Write( wxT("//   ThemeAsCeeCode.h\r\n") );
      File.Write( wxT("//\r\n") );
      File.Write( wxT("//   This file was Auto-Generated.\r\n") );
      File.Write( wxT("//   It is included by Theme.cpp.\r\n") );
      File.Write( wxT("//   Only check this into CVS if you've read and understood the guidelines!\r\n\r\n   ") );
   }
   return bOk;
}

/// This is the 'callback' function called with each write of PNG data
/// to the stream.  This is where we conveet to text and add commas.
size_t SourceOutputStream::OnSysWrite(const void *buffer, size_t bufsize)
{
   wxString Temp;
   for(int i=0;i<(int)bufsize;i++)
   {
      // Write one byte with a comma
      Temp = wxString::Format( wxT("%i,"),(int)(((unsigned char*)buffer)[i]) );
      File.Write( Temp );
      nBytes++;
      // New line if more than 20 bytes written since last time.
      if( (nBytes %20)==0 )
      {
         File.Write( wxT("\r\n   "));
      }
   }
   return bufsize;
}

/// Destructor.  We close our text stream in here.
SourceOutputStream::~SourceOutputStream()
{
   File.Write( wxT("\r\n") );
   File.Close();
}

void ThemeBase::CreateImageCache( bool bBinarySave )
{
   EnsureInitialised();
   const int nBmpsPerRow=6;
   const int depth = 32; //32 bits depth.  Includes alpha channel.
   const int width = 32 * nBmpsPerRow;// we want to be 6 32 bit images across.
   int height = 32 * (((int)mBitmapNames.GetCount()+nBmpsPerRow-1)/nBmpsPerRow);
   height +=200;

   wxImage Image( width, height );

   // Ensure we have an alpha channel...
   if( !Image.HasAlpha() )
   {
      Image.InitAlpha();
   }

   mxCacheWidth = width;

//   mImageCache.m_hasAlpha = true;
   int xStart=0;
   int yStart=0;
   int iHeight=0;

   int xWidth1;
   int yHeight1;
   int i;
   wxBitmap * pBmp;
   myPos = 0;
   myPosBase =0;
   myHeight = 0;
   iImageGroupSize = 1;
   SetNewGroup(1);
   // Save the bitmaps
   for(i=0;i<(int)mBitmaps.GetCount();i++)
   {
      pBmp = &mBitmaps[i];
      mFlags = mBitmapFlags[i];
      xWidth1=pBmp->GetWidth();
      yHeight1=pBmp->GetHeight();
      GetNextPosition( xWidth1, yHeight1 );
      wxImage SrcImage( pBmp->ConvertToImage() );
      PasteSubImage( &Image, &SrcImage, mxPos, myPos );
   }

   // Now save the colours.
   int x,y;

   SetNewGroup(1);
      xWidth1 = 10;
      yHeight1 = 10;
   for(i=0;i<(int)mColours.GetCount();i++)
   {
      GetNextPosition( xWidth1, yHeight1 );
      wxColour c = mColours[i];
      Image.SetRGB( wxRect( mxPos, myPos, xWidth1, yHeight1), c.Red(), c.Green(), c.Blue() );

      // YUCK!  No function in wxWidgets to set a rectangle of alpha...
      for(x=0;x<xWidth1;x++)
      {
         for(y=0;y<yHeight1;y++)
         {
            Image.SetAlpha( mxPos + x, myPos+y, 255);
         }
      }
   }

   // IF nBinarySave, THEN saving to a normal PNG file.
   if( bBinarySave )
   {
      wxString FileName = FileNames::ThemeCachePng();

      // Perhaps we should prompt the user if they are overwriting 
      // an existing theme cache?
#if 0
      if( wxFileExist( FileName ))
      {
         wxMessageBox(
            wxString::Format( 
            _("Theme cache file:\r\n  %s\r\nalready exists.\r\n"
               wxT("Are you sure you want to replace it?")),
               FileName.c_str() ));
         return;
      }
#endif
      if( !Image.SaveFile( FileName, wxBITMAP_TYPE_PNG ))
      {
         wxMessageBox(
            wxString::Format( 
            _("Audacity could not write file:\r\n  %s."),
               FileName.c_str() ));
         return;
      }
      wxMessageBox(
         wxString::Format( 
            wxT("Theme written to:\r\n  %s."),
            FileName.c_str() ));
   }
   // ELSE saving to a C code textual version.
   else
   {
      SourceOutputStream OutStream;
      wxString FileName = FileNames::ThemeCacheAsCee( );
      if( !OutStream.OpenFile( FileName ))
      {
         wxMessageBox(
            wxString::Format( 
            _("Audacity could not open file:\r\n  %s\r\nfor writing."),
            FileName.c_str() ));
         return;
      }
      if( !Image.SaveFile(OutStream, wxBITMAP_TYPE_PNG ) )
      {
         wxMessageBox(
            wxString::Format( 
            _("Audacity could not write images to file:\r\n  %s."),
            FileName.c_str() ));
         return;
      }
      wxMessageBox(
         wxString::Format( 
            wxT("Theme as Cee code written to:\r\n  %s."),
            FileName.c_str() ));
   }
}

/// Reads an image cache including images, cursors and colours.
/// @param bBinaryRead if true means read from an external bbinary file.  
///   otherwise the data is taken from a compiled in block of memory.
/// @param bOkIfNotFound if true means do not report absent file.
/// @return true iff we loaded the images.
bool ThemeBase::ReadImageCache( bool bBinaryRead, bool bOkIfNotFound)
{
   EnsureInitialised();
   const int nBmpsPerRow=6;
   const int depth = 32; //32 bits depth.  Includes alpha channel.
   const int width = 32 * nBmpsPerRow;// we want to be 6 32 bit images across.
   int height = 32 * (((int)mBitmapNames.GetCount()+nBmpsPerRow-1)/nBmpsPerRow);
   height +=200;
   mImageCache = wxBitmap(width, height, depth);

   mxCacheWidth = width;

//   mImageCache.m_hasAlpha = true;
   int xStart=0;
   int yStart=0;
   int iHeight=0;

   // IF bBinary read THEN a normal read from a PNG file
   if(  bBinaryRead )
   {
      wxString FileName = FileNames::ThemeCachePng();
      if( !wxFileExists( FileName ))
      {  
         if( bOkIfNotFound )
            return false; // did not load the images, so return false.
         wxMessageBox(
            wxString::Format( 
            _("Audacity could not find file:\r\n  %s.\r\nTheme not loaded."),
               FileName.c_str() ));
         return false;
      }
      if( !mImageCache.LoadFile( FileName, wxBITMAP_TYPE_PNG ))
      {
         wxMessageBox(
            wxString::Format( 
            _("Audacity could not load file:\r\n  %s.\r\nBad png format perhaps?"),
               FileName.c_str() ));
         return false;
      }
   }
   // ELSE we are reading from internal storage.
   else
   {  
      wxImage LoadedImage;
      wxMemoryInputStream InternalStream(
         (char *)ImageCacheAsData, sizeof(ImageCacheAsData));
      if( !LoadedImage.LoadFile( InternalStream, wxBITMAP_TYPE_PNG ))
      {
         // If we get this message, it means that the data in file
         // was not a valid png image.
         // Most likely someone edited it by mistake, 
         // Or some experiment is being tried with new formats for it.
         wxMessageBox(
            wxString::Format( 
               _("Audacity could not read its default theme.\r\n"
               wxT("We're not sure how this happened.\r\n")
               wxT("Please report the problem.")))
               );
         return false;
      }
      mImageCache = wxBitmap( LoadedImage, 32 );
   }

   int xWidth1;
   int yHeight1;
   int i;
   wxBitmap * pBmp;
   myPos = 0;
   myPosBase =0;
   myHeight = 0;
   iImageGroupSize = 1;
   SetNewGroup(1);
   // Load the bitmaps
   for(i=0;i<(int)mBitmaps.GetCount();i++)
   {
      pBmp = &mBitmaps[i];
      mFlags = mBitmapFlags[i];
      xWidth1=pBmp->GetWidth();
      yHeight1=pBmp->GetHeight();
      GetNextPosition( xWidth1, yHeight1 );

//      wxLogDebug(wxT("Copy at %i %i (%i,%i)"), mxPos, myPos, xWidth1, yHeight1 );
      *pBmp = mImageCache.GetSubBitmap( wxRect( mxPos, myPos, xWidth1, yHeight1 ));
   }

   // Now load the colours.
   wxImage Image( mImageCache.ConvertToImage() );
   int x,y;
   SetNewGroup(1);
      xWidth1 = 10;
      yHeight1 = 10;
   for(i=0;i<(int)mColours.GetCount();i++)
   {
      GetNextPosition( xWidth1, yHeight1 );
      x=mxPos + xWidth1/2;
      y=myPos + yHeight1/2;
      mColours[i] = wxColour( Image.GetRed( x,y), Image.GetGreen( x,y), Image.GetBlue(x,y));
   }
   return true;
}

void ThemeBase::LoadComponents( bool bOkIfNotFound )
{
   // IF directory doesn't exist THEN return early.
   if( !wxDirExists( FileNames::ThemeComponentsDir() ))
      return;

   wxBusyCursor();
   int i;
   int n=0;
   wxString FileName;
   for(i=0;i<(int)mBitmaps.GetCount();i++)
   {
      FileName = FileNames::ThemeComponent( mBitmapNames[i] );
      if( wxFileExists( FileName ))
      {
         if( !mBitmaps[i].LoadFile( FileName, wxBITMAP_TYPE_PNG ))
         {
            wxMessageBox(
               wxString::Format( 
               _("Audacity could not load file:\r\n  %s.\r\nBad png format perhaps?"),
                  FileName.c_str() ));
            return;
         }
         n++;
      }
   }
   if( n==0 )
   {
      if( bOkIfNotFound )
         return;
      wxMessageBox(
         wxString::Format( 
            _("None of the expected theme component files\r\n"
            wxT("were found in:\r\n  %s.")),
            FileNames::ThemeComponentsDir().c_str() ));
   }
}

void ThemeBase::SaveComponents()
{
   // IF directory doesn't exist THEN create it
   if( !wxDirExists( FileNames::ThemeComponentsDir() ))
   {
      /// \bug 1 in wxWidgets documentation; wxMkDir returns false if 
      /// directory didn't exist, even if it successfully creates it.
      /// so we create and then test if it exists instead.
      /// \bug 2 in wxWidgets documentation; wxMkDir has only one argument
      /// under MSW
#ifdef __WXMSW__
      wxMkDir( FileNames::ThemeComponentsDir().fn_str() );
#else
      wxMkDir( FileNames::ThemeComponentsDir().fn_str(), 0700 );
#endif
      if( !wxDirExists( FileNames::ThemeComponentsDir() ))
      {
         wxMessageBox(
            wxString::Format( 
            _("Could not create directory:\r\n  %s"),
               FileNames::ThemeComponentsDir().c_str() ));
         return;
      }
   }

   wxBusyCursor();
   int i;
   int n=0;
   wxString FileName;
   for(i=0;i<(int)mBitmaps.GetCount();i++)
   {
      FileName = FileNames::ThemeComponent( mBitmapNames[i] );
      if( !wxFileExists( FileName ))
      {
         if( !mBitmaps[i].SaveFile( FileName, wxBITMAP_TYPE_PNG ))
         {
            wxMessageBox(
               wxString::Format( 
               _("Audacity could not save file:\r\n  %s"),
                  FileName.c_str() ));
            return;
         }
         n++;
      }
   }
   if( n==0 )
   {
      wxMessageBox(
         wxString::Format( 
         _("All required files in:\r\n  %s\r\nwere already present."),
            FileNames::ThemeComponentsDir().c_str() ));
      return;
   }
   wxMessageBox(
      wxString::Format( 
         wxT("Theme written to:\r\n  %s."),
         FileNames::ThemeComponentsDir().c_str() ));
}

void ThemeBase::ReadThemeInternal()
{
   // false indicates not using standard binary method.
   ReadImageCache( false );
}

void ThemeBase::SaveThemeAsCode()
{
   // false indicates not using standard binary method.
   CreateImageCache( false );
}

wxImage ThemeBase::MakeImageWithAlpha( wxBitmap & Bmp )
{
   // BUG in wxWidgets.  Conversion from BMP to image does not preserve alpha.
   wxImage image( Bmp.ConvertToImage() );
   return image;
}

wxColour & ThemeBase::Colour( int iIndex )
{
   wxASSERT( iIndex >= 0 );
   EnsureInitialised();
   return mColours[iIndex];
}

void ThemeBase::SetBrushColour( wxBrush & Brush, int iIndex )
{
   wxASSERT( iIndex >= 0 );
   Brush.SetColour( Colour( iIndex ));
}

void ThemeBase::SetPenColour(   wxPen & Pen, int iIndex )
{
   wxASSERT( iIndex >= 0 );
   Pen.SetColour( Colour( iIndex ));
}

wxBitmap & ThemeBase::Bitmap( int iIndex )
{
   wxASSERT( iIndex >= 0 );
   EnsureInitialised();
   return mBitmaps[iIndex];
}

wxImage  * ThemeBase::Image( int iIndex )
{
   wxASSERT( iIndex >= 0 );
   EnsureInitialised();
   return new wxImage(Bitmap(iIndex).ConvertToImage());
}

wxCursor & ThemeBase::Cursor( int iIndex )
{
   wxASSERT( iIndex >= 0 );
   EnsureInitialised();
   return *(wxCursor*)NULL;
}

wxFont   & ThemeBase::Font( int iIndex )
{
   wxASSERT( iIndex >= 0 );
   EnsureInitialised();
   return *(wxFont*)NULL;
}

