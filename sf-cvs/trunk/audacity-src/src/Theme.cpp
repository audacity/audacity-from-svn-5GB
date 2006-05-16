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
#include "Theme.h"


WX_DEFINE_OBJARRAY( ArrayOfBitmaps );

#include "..\images\ControlButtons.h"
#include "..\images\Cursors.h"
#include "..\images\EditButtons.h"
#include "..\images\MixerImages.h"
#include "..\images\ToolBarButtons.h"


#include "..\images\ToolsButtons\Draw.xpm"
#include "..\images\ToolsButtons\DrawAlpha.xpm"
#include "..\images\ToolsButtons\Envelope.xpm"
#include "..\images\ToolsButtons\EnvelopeAlpha.xpm"
#include "..\images\ToolsButtons\IBeam.xpm"
#include "..\images\ToolsButtons\IBeamAlpha.xpm"
#include "..\images\ToolsButtons\Multi.xpm"
#include "..\images\ToolsButtons\MultiAlpha.xpm"
#include "..\images\ToolsButtons\TimeShift.xpm"
#include "..\images\ToolsButtons\TimeShiftAlpha.xpm"
#include "..\images\ToolsButtons\Zoom.xpm"
#include "..\images\ToolsButtons\ZoomAlpha.xpm"


#if 0
#include "..\images\ToolsButtons.h"
#include "..\images\TranscriptionButtons.h"

// Include files to get the default images
#include "..\images\Aqua.xpm"
#include "..\images\Arrow.xpm"
#include "..\images\AudacityLogo.xpm"
#include "..\images\AudacityLogo48x48.xpm"
#include "..\images\SliderThumb.xpm"
#include "..\images\SliderThumbAlpha.xpm"
#endif

#if 0
#include "..\images\Aqua\Down.xpm"
#include "..\images\Aqua\DownButtonSquare.xpm"
#include "..\images\Aqua\DownButtonStripes.xpm"
#include "..\images\Aqua\DownButtonWhite.xpm"
#include "..\images\Aqua\Hilite.xpm"
#include "..\images\Aqua\HiliteButtonSquare.xpm"
#include "..\images\Aqua\HiliteButtonStripes.xpm"
#include "..\images\Aqua\HiliteButtonWhite.xpm"
#include "..\images\Aqua\Slider.xpm"
#include "..\images\Aqua\SliderThumb.xpm"
#include "..\images\Aqua\Up.xpm"
#include "..\images\Aqua\UpButtonSquare.xpm"
#include "..\images\Aqua\UpButtonStripes.xpm"
#include "..\images\Aqua\UpButtonWhite.xpm"


#include "..\images\ControlButtons\Disabled.xpm"
#include "..\images\ControlButtons\Down.xpm"
#include "..\images\ControlButtons\DownButton.xpm"
#include "..\images\ControlButtons\Hilite.xpm"
#include "..\images\ControlButtons\HiliteButton.xpm"
#include "..\images\ControlButtons\Slider.xpm"
#include "..\images\ControlButtons\SliderThumb.xpm"
#include "..\images\ControlButtons\Up.xpm"
#include "..\images\ControlButtons\UpButton.xpm"
#include "..\images\ControlButtons\Zoom.xpm"
#include "..\images\ControlButtons\ZoomAlpha.xpm"


#include "..\images\ToolBarImages\DockDown.xpm"
#include "..\images\ToolBarImages\DockDownShort.xpm"
#include "..\images\ToolBarImages\DockOver.xpm"
#include "..\images\ToolBarImages\DockOverShort.xpm"
#include "..\images\ToolBarImages\DockUp.xpm"
#include "..\images\ToolBarImages\DockUpShort.xpm"

#include "..\images\TranscriptionImages\Automate.xpm"
#include "..\images\TranscriptionImages\AutomateSelection.xpm"
#include "..\images\TranscriptionImages\AutomateSelectionAlpha.xpm"
#include "..\images\TranscriptionImages\AutomateSelectionDisabled.xpm"
#include "..\images\TranscriptionImages\CalibrateAlpha.xpm"
#include "..\images\TranscriptionImages\CalibrateDisabled.xpm"
#include "..\images\TranscriptionImages\CalibrateUp.xpm"
#include "..\images\TranscriptionImages\Down.xpm"
#include "..\images\TranscriptionImages\EndOff.xpm"
#include "..\images\TranscriptionImages\EndOffAlpha.xpm"
#include "..\images\TranscriptionImages\EndOffDisabled.xpm"
#include "..\images\TranscriptionImages\EndOn.xpm"
#include "..\images\TranscriptionImages\EndOnAlpha.xpm"
#include "..\images\TranscriptionImages\EndOnDisabled.xpm"
#include "..\images\TranscriptionImages\Hilite.xpm"
#include "..\images\TranscriptionImages\MakeTag.xpm"
#include "..\images\TranscriptionImages\MakeTagAlpha.xpm"
#include "..\images\TranscriptionImages\MakeTagDisabled.xpm"
#include "..\images\TranscriptionImages\SelectSilence.xpm"
#include "..\images\TranscriptionImages\SelectSilenceAlpha.xpm"
#include "..\images\TranscriptionImages\SelectSilenceDisabled.xpm"
#include "..\images\TranscriptionImages\SelectSound.xpm"
#include "..\images\TranscriptionImages\SelectSoundAlpha.xpm"
#include "..\images\TranscriptionImages\SelectSoundDisabled.xpm"
#include "..\images\TranscriptionImages\StartOff.xpm"
#include "..\images\TranscriptionImages\StartOffAlpha.xpm"
#include "..\images\TranscriptionImages\StartOffDisabled.xpm"
#include "..\images\TranscriptionImages\StartOn.xpm"
#include "..\images\TranscriptionImages\StartOnAlpha.xpm"
#include "..\images\TranscriptionImages\StartOnDisabled.xpm"
#include "..\images\TranscriptionImages\Up.xpm"
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

void Theme::RegisterImages()
{
   if( mbInitialised )
      return;
   mFlags = resFlagPaired;
   mbInitialised = true;
   int i=0;
   RegisterBitmap( i++,MaskedBmp(Pause, PauseAlpha ),"Pause");
   RegisterBitmap( i++,MaskedBmp(PauseDisabled,PauseAlpha),"PauseDisabled");

   RegisterBitmap( i++,MaskedBmp(Play,PlayAlpha),"Play");
   RegisterBitmap( i++,MaskedBmp(PlayDisabled,PlayAlpha),"PlayDisabled");

   RegisterBitmap( i++,MaskedBmp((const char **)Loop,(const char **)LoopAlpha),"Loop");
   RegisterBitmap( i++,MaskedBmp((const char **)LoopDisabled,(const char **)LoopAlpha),"LoopDisabled");

   RegisterBitmap( i++,MaskedBmp(Stop,StopAlpha),"Stop");
   RegisterBitmap( i++,MaskedBmp(StopDisabled,StopAlpha),"StopDisabled");

   RegisterBitmap( i++,MaskedBmp(Rewind,RewindAlpha),"Rewind");
   RegisterBitmap( i++,MaskedBmp(RewindDisabled,RewindAlpha),"RewindDisabled");

   RegisterBitmap( i++,MaskedBmp(FFwd,FFwdAlpha),"FFwd");
   RegisterBitmap( i++,MaskedBmp(FFwdDisabled,FFwdAlpha),"FFwdDisabled");

   RegisterBitmap( i++,MaskedBmp(Record,RecordAlpha),"Record");
   RegisterBitmap( i++,MaskedBmp(RecordDisabled,RecordAlpha),"RecordDisabled");

   RegisterBitmap( i++,MaskedBmp(CleanSpeech,CleanSpeechAlpha),"CleanSpeech");
   RegisterBitmap( i++,MaskedBmp(CleanSpeechDisabled,CleanSpeechAlpha),"CleanSpeechDisabled");

   mFlags = resFlagPaired | resFlagCursor;
   RegisterBitmap( i++,IBeamCursorXpm, "IBeamCursor");
   RegisterBitmap( i++,DrawCursorXpm, "DrawCursor");
   RegisterBitmap( i++,EnvCursorXpm, "EnvCursor");
   RegisterBitmap( i++,MaskedBmp(TimeCursorXpm,TimeCursorXpm), "TimeCursor");
   RegisterBitmap( i++,ZoomInCursorXpm, "ZoomInCursor");
   RegisterBitmap( i++,ZoomOutCursorXpm, "ZoomOutCursor");
   RegisterBitmap( i++,LabelCursorLeftXpm, "LabelCursorLeft");
   RegisterBitmap( i++,LabelCursorRightXpm, "LabelCursorRight");
   RegisterBitmap( i++,DisabledCursorXpm, "DisabledCursor");

   mFlags = resFlagPaired;
   RegisterBitmap( i++,MaskedBmp(Cut,CutAlpha), "");
   RegisterBitmap( i++,MaskedBmp(CutDisabled,CutAlpha), "");

   RegisterBitmap( i++,MaskedBmp(Copy,CopyAlpha), "");
   RegisterBitmap( i++,MaskedBmp(CopyDisabled,CopyAlpha), "");

   RegisterBitmap( i++,MaskedBmp(Paste,PasteAlpha), "");
   RegisterBitmap( i++,MaskedBmp(PasteDisabled,PasteAlpha), "");

   RegisterBitmap( i++,MaskedBmp(Trim,TrimAlpha), "");
   RegisterBitmap( i++,MaskedBmp(TrimDisabled,TrimAlpha), "");

   RegisterBitmap( i++,MaskedBmp(Silence,SilenceAlpha), "");
   RegisterBitmap( i++,MaskedBmp(SilenceDisabled,SilenceAlpha), "");

   RegisterBitmap( i++,MaskedBmp(Undo,UndoAlpha), "");
   RegisterBitmap( i++,MaskedBmp(UndoDisabled,UndoAlpha), "");

   RegisterBitmap( i++,MaskedBmp(Redo,RedoAlpha), "");
   RegisterBitmap( i++,MaskedBmp(RedoDisabled,RedoAlpha), "");

   RegisterBitmap( i++,MaskedBmp(ZoomFit,ZoomFitAlpha), "");
   RegisterBitmap( i++,MaskedBmp(ZoomFitDisabled,ZoomFitAlpha), "");

   RegisterBitmap( i++,MaskedBmp(ZoomIn,ZoomInAlpha), "");
   RegisterBitmap( i++,MaskedBmp(ZoomInDisabled,ZoomInAlpha), "");

   RegisterBitmap( i++,MaskedBmp(ZoomOut,ZoomOutAlpha), "");
   RegisterBitmap( i++,MaskedBmp(ZoomOutDisabled,ZoomOutAlpha), "");

   RegisterBitmap( i++,MaskedBmp(ZoomSel,ZoomSelAlpha), "");
   RegisterBitmap( i++,MaskedBmp(ZoomSelDisabled,ZoomSelAlpha), "");

//   RegisterBitmap( i++,MaskedBmp(ZoomToggle,ZoomToggleAlpha), "");
//   RegisterBitmap( i++,MaskedBmp(ZoomToggleDisabled,ZoomToggleAlpha), "");

   RegisterBitmap( i++,MaskedBmp(IBeam,IBeamAlpha), "");
   RegisterBitmap( i++,MaskedBmp(Zoom,ZoomAlpha), "");
   RegisterBitmap( i++,MaskedBmp(Envelope,EnvelopeAlpha), "");
   RegisterBitmap( i++,MaskedBmp(TimeShift,TimeShiftAlpha), "");
   RegisterBitmap( i++,MaskedBmp(Draw,DrawAlpha), "");
   RegisterBitmap( i++,MaskedBmp(Multi,MultiAlpha), "");

   mFlags = resFlagNone;
   RegisterBitmap( i++,MaskedBmp(Mic,MicAlpha), "");
   RegisterBitmap( i++,MaskedBmp(Speaker,SpeakerAlpha), "");

//   RegisterBitmap( i++,DisabledXpm, "");
   RegisterBitmap( i++,Up, "");
   RegisterBitmap( i++,Down, "");
   RegisterBitmap( i++,Hilite, "");

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
   wxLogDebug( "Image 1: %i Image 2: %i", 
      Bmp1.GetDepth(), Bmp2.GetDepth() );
   
   wxASSERT( Bmp1.GetDepth()==24);
   wxASSERT( Bmp2.GetDepth()==24);

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

   wxBitmap Result( Img1, 32 );
   return Result;
}

void ThemeBase::RegisterBitmap( int iIndex, char const ** pXpm, const wxString & Name )
{

   wxBitmap Bmp( pXpm ); // a 24 bit bitmap.
   wxImage Img( Bmp.ConvertToImage() );
   Img.InitAlpha();
   wxBitmap Bmp2( Img, 32 );
   RegisterBitmap( iIndex, Bmp2, Name );
}

void ThemeBase::RegisterBitmap( int iIndex, const wxBitmap &Bmp, const wxString & Name )
{
   mBitmaps.Add( Bmp );
   mBitmapNames.Add( Name );
   mBitmapFlags.Add( mFlags );
}

wxString ThemeBase::GetCacheFileName()
{
#ifdef  __WXMSW__
   return "C:\\ImageCache.png";
#else
   return "IamgeCache.png";
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
   }
   myPos = myPosBase + iImageGroupIndex * myHeight;
}
   
void ThemeBase::CreateImageCache()
{
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
   myPosBase;
   myHeight = 0;
   iImageGroupSize = 1;
   SetNewGroup(1);
   for(i=0;i<(int)mBitmaps.GetCount();i++)
   {
      pBmp = &mBitmaps[i];
      mFlags = mBitmapFlags[i];
      xWidth1=pBmp->GetWidth();
      yHeight1=pBmp->GetHeight();
      GetNextPosition( xWidth1, yHeight1 );

      SrcMemDC.SelectObject(*pBmp);
      wxLogDebug("Draw at %i %i (%i,%i)", mxPos, myPos, xWidth1, yHeight1 );
      DestMemDC.Blit( mxPos, myPos, xWidth1,yHeight1, &SrcMemDC, 0,0,wxCOPY);
      SrcMemDC.SelectObject( wxNullBitmap);
   }
   DestMemDC.SelectObject( wxNullBitmap );

   // UseAlpha is a deprecated function!
   // However (under windows) there is no other way to ensure the
   // cache file has the alpha channel in it!
   mImageCache.UseAlpha();

//   wxImage image = MakeImageWithAlpha( mImageCache );
   if( !mImageCache.SaveFile( GetCacheFileName(), wxBITMAP_TYPE_PNG ))
   {
      wxASSERT( false );
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
   return *(wxColour*)NULL;
}

wxBitmap & ThemeBase::Bitmap( int iIndex )
{
   return *(wxBitmap*)NULL;
}

wxImage  & ThemeBase::Image( int iIndex )
{
   return *(wxImage*)NULL;
}

wxCursor & ThemeBase::Cursor( int iIndex )
{
   return *(wxCursor*)NULL;
}

wxFont   & ThemeBase::Font( int iIndex )
{
   return *(wxFont*)NULL;
}

