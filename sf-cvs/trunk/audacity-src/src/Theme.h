/**********************************************************************

  Audacity: A Digital Audio Editor

  Theme.h

  James Crook


**********************************************************************/

#pragma once

class wxBitmap;
class wxImage;
class wxColour;
class wxFont;


enum eResourceType
{ 
   resTypeColour,
   resTypeBitmap,
   resTypeImage = resTypeBitmap,
   resTypeCursor,
   resTypeFont
};

enum eResourceFlags
{
   resFlagNone   =0x00,
   resFlagPaired =0x01,
   resFlagCursor =0x02
};


WX_DECLARE_OBJARRAY(wxBitmap, ArrayOfBitmaps);


class ThemeBase
{
public:
   ThemeBase(void);
public:
   ~ThemeBase(void);

   void RegisterBitmap( int iIndex,char const** pXpm, const wxString & Name);
   void RegisterBitmap( int iIndex, const wxBitmap &Bmp, const wxString & Name );

   wxString GetCacheFileName();
   void CreateImageCache();
   void SetNewGroup( int iGroupSize );
   void GetNextPosition( int xSize, int ySize );

   wxColour & Colour( int iIndex );
   wxBitmap & Bitmap( int iIndex );
   wxImage  & Image( int iIndex );
   wxCursor & Cursor( int iIndex );
   wxFont   & Font( int iIndex );

   // Utility function that combines a bitmap and a mask, both in XPM format.
   wxBitmap MaskedBmp( char const ** pXpm, char const ** pMask );
   // Utility functiuon that takes a 32 bit bitmap and makes it into an image.
   wxImage MakeImageWithAlpha( wxBitmap & Bmp );

protected:
   wxBitmap mImageCache;
   ArrayOfBitmaps mBitmaps;
   wxArrayString mBitmapNames;
   wxArrayInt mBitmapFlags;

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
   void RegisterImages();
   bool mbInitialised;
};

extern Theme theTheme;