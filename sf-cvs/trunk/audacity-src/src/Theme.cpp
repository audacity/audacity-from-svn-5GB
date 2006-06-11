/**********************************************************************

  Audacity: A Digital Audio Editor

  Theme.cpp

  James Crook
   
   Theme is a class which manages theme resources.
   It maps sets of ids to the resources and to names of the resources,
   so that they can be loaded/saved from files.

  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

**********************************************************************/

#include <wx/wx.h>
#include <wx/image.h>
#include <wx/arrimpl.cpp> 
#include "ImageManipulation.h"
#include "Theme.h"


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
}

void Theme::RegisterImages()
{
   if( mbInitialised )
      return;
   mFlags = resFlagPaired;
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

wxBitmap ThemeBase::MaskedBmp( char const ** pXpm, char const ** pMask )
{
   wxBitmap Bmp1( pXpm );
   wxBitmap Bmp2( pMask );

//   wxLogDebug( wxT("Image 1: %i Image 2: %i"), 
//      Bmp1.GetDepth(), Bmp2.GetDepth() );

   // We want a 24-bit-depth bitmap if all is working, but on some
   // platforms it might just return -1 (which means best available
   // or not relevant).
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

wxString ThemeBase::GetCacheFileName()
{
#ifdef  __WXMSW__
   return wxT("C:\\ImageCache.png");
#else
   return wxT("ImageCache.png");
#endif
}

void ThemeBase::SetNewGroup( int iGroupSize )
{
   myPosBase +=myHeight * iImageGroupSize;
   mxPos =0;
   iImageGroupSize = iGroupSize;
   iImageGroupIndex = -1;
}

void ThemeBase::GetNextPosition( int xSize, int ySize )
{
   // if the height has changed, then we are on a new group.
   if( ySize != myHeight )
   {
      SetNewGroup( mFlags && resFlagPaired ? 2 : 1 );
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
   
void ThemeBase::CreateImageCache()
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

#if 0
//   mImageCache = wxBitmap(width, height, depth);

   // UseAlpha is a deprecated function!
   // However (under windows) there is no other way to ensure the
   // cache file has the alpha channel in it!
//   mImageCache.UseAlpha();

   wxMemoryDC DestMemDC;
   DestMemDC.SelectObject(mImageCache);
   DestMemDC.SetBrush( *wxWHITE_BRUSH );
   DestMemDC.DrawRectangle( 0,0, width,height );

   wxMemoryDC SrcMemDC;

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

      SrcMemDC.SelectObject(*pBmp);
//    wxLogDebug(wxT("Draw at %i %i (%i,%i)"), mxPos, myPos, xWidth1, yHeight1 );
      DestMemDC.Blit( mxPos, myPos, xWidth1,yHeight1, &SrcMemDC, 0,0,wxCOPY);
      SrcMemDC.SelectObject( wxNullBitmap);
   }

   DestMemDC.SelectObject( wxNullBitmap );

   // UseAlpha is a deprecated function!
   // However (under windows) there is no other way to ensure the
   // cache file has the alpha channel in it!
   mImageCache.UseAlpha();
   wxImage Image( mImageCache.ConvertToImage() );
#endif


   // Now save the colours.
   int x,y;
   for(i=0;i<(int)mColours.GetCount();i++)
   {
      xWidth1 = 10;
      yHeight1 = 10;
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

   if( !Image.SaveFile( GetCacheFileName(), wxBITMAP_TYPE_PNG ))
   {
      wxASSERT( false );
   }
}

void ThemeBase::ReadImageCache()
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

//   wxImage image = MakeImageWithAlpha( mImageCache );
   if( !mImageCache.LoadFile( GetCacheFileName(), wxBITMAP_TYPE_PNG ))
   {
      wxASSERT( false );
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
   for(i=0;i<(int)mColours.GetCount();i++)
   {
      xWidth1 = 10;
      yHeight1 = 10;
      GetNextPosition( xWidth1, yHeight1 );
      x=mxPos + xWidth1/2;
      y=myPos + yHeight1/2;
      mColours[i] = wxColour( Image.GetRed( x,y), Image.GetGreen( x,y), Image.GetBlue(x,y));
   }
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
//   return *(wxBitmap*)NULL;
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

