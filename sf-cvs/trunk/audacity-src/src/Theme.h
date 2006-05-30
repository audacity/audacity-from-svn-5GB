/**********************************************************************

  Audacity: A Digital Audio Editor

  Theme.h

  James Crook

  Audacity is free software.
  This file is licensed under the wxWindows license, see License.txt

**********************************************************************/

#ifndef __AUDACITY_THEME__
#define __AUDACITY_THEME__

#include <wx/defs.h>
#include <wx/dynarray.h>
#include <wx/bitmap.h>

class wxBitmap;
class wxImage;
class wxColour;
class wxFont;

enum teBmps 
{
   bmpPause=0,
   bmpPauseDisabled,
   bmpPlay,
   bmpPlayDisabled,
   bmpLoop,
   bmpLoopDisabled,
   bmpStop,
   bmpStopDisabled,
   bmpRewind,
   bmpRewindDisabled,
   bmpFFwd,
   bmpFFwdDisabled,
   bmpRecord,
   bmpRecordDisabled,
   bmpCleanSpeech,
   bmpCleanSpeechDisabled,

   bmpToolBarToggle,
   bmpToolBarTarget,
   bmpToolBarGrabber,

   bmpUpButton,
   bmpDownButton,
   bmpHiliteButton,
   bmpRecoloredUpButton,
   bmpRecoloredDownButton,
   bmpRecoloredHiliteButton,

   bmpFirstCursor
};

enum teColours
{
   clrBlank=0,
   clrUnselected,
   clrSelected,
   clrSample,
   clrSelSample,
   clrDragSample,
                  
   clrMuteSample,
   clrRms,
   clrMuteRms,
   clrShadow
};

enum teResourceType
{ 
   resTypeColour,
   resTypeBitmap,
   resTypeImage = resTypeBitmap,
   resTypeCursor,
   resTypeFont
};

enum teResourceFlags
{
   resFlagNone   =0x00,
   resFlagPaired =0x01,
   resFlagCursor =0x02
};


WX_DECLARE_OBJARRAY(wxBitmap, ArrayOfBitmaps);
WX_DECLARE_OBJARRAY(wxColour, ArrayOfColours);

class ThemeBase
{
public:
   ThemeBase(void);
public:
   virtual ~ThemeBase(void);

public:
   virtual void EnsureInitialised()=0;
   void RegisterBitmap( int iIndex,char const** pXpm, const wxString & Name);
   void RegisterBitmap( int iIndex, const wxBitmap &Bmp, const wxString & Name );
   void RegisterColour( int iIndex, const wxColour &Clr, const wxString & Name );

   wxString GetCacheFileName();
   void CreateImageCache();
   void ReadImageCache();
   void SetNewGroup( int iGroupSize );
   void GetNextPosition( int xSize, int ySize );

   wxColour & Colour( int iIndex );
   wxBitmap & Bitmap( int iIndex );
   wxImage  * Image( int iIndex ); // beware!  This one allocates storage.
   wxCursor & Cursor( int iIndex );
   wxFont   & Font( int iIndex );

   void SetBrushColour( wxBrush & Brush, int iIndex );
   void SetPenColour(   wxPen & Pen, int iIndex );

   // Utility function that combines a bitmap and a mask, both in XPM format.
   wxBitmap MaskedBmp( char const ** pXpm, char const ** pMask );
   // Utility functiuon that takes a 32 bit bitmap and makes it into an image.
   wxImage MakeImageWithAlpha( wxBitmap & Bmp );

protected:
   wxBitmap mImageCache;
   ArrayOfBitmaps mBitmaps;
   wxArrayString mBitmapNames;
   wxArrayInt mBitmapFlags;

   ArrayOfColours mColours;
   wxArrayString mColourNames;

   int mxPos;
   int myPos;
   int myPosBase;
   int mxWidth;
   int myHeight;
   int mxCacheWidth;
   int iImageGroupSize; 
   int iImageGroupIndex;
   int mFlags;
};


class Theme : public ThemeBase
{
public:
   Theme(void);
public:
   ~Theme(void);
public:
   virtual void EnsureInitialised();
   void RegisterImages();
   void RegisterColours();
   bool mbInitialised;
};

extern Theme theTheme;

#endif // __AUDACITY_THEME__
