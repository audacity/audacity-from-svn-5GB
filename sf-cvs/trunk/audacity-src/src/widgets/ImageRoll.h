/**********************************************************************

  Audacity: A Digital Audio Editor

  ImageRoll.h

  Dominic Mazzoni

  An ImageRoll is an image that can be expanded to an arbitrary size;
  it is made up of both fixed pieces and repeating pieces.  A typical
  ImageRoll might be made up of two fixed ends and a repeating
  middle part:

  /-----\                       /-----\
  |LEFT |--REPEATING--REPEATING-|RIGHT|
  \-----/                       \-----/

  As you resize the image, it could look like this:

  /-----\ /-----\
  |LEFT |-|RIGHT|
  \-----/ \-----/

  Or like this:

  /-----\                                  /-----\
  |LEFT |--REPEATING--REPEATING--REPEATING-|RIGHT|
  \-----/                                  \-----/

  Note that an ImageRoll can have a center piece; in fact, its pieces
  always alternate fixed, repeating, fixed, repeating, etc. - although
  one of these pieces is allowed to be of size zero, making it skipped.
  Here's an example with a center piece:

  /-----\                /------\                /-----\
  |LEFT |-REPEAT--REPEAT-|CENTER|-repeat--repeat-|RIGHT|
  \-----/                \------/                \-----/

  Note that the left and right repeating sections can be different.
  Of course, an ImageRoll can be oriented vertically as well.
  In the future, support for an ImageRoll that expands both horizontally
  and vertically at the same time will be supported.

  An ImageRoll is initialized with a _single_ wxImage that defines
  all of its pieces.  This is done by way of a "magic color" which
  separates each piece in the image.  If the magic colored pixel is
  denoted by "X", the above ImageRoll could be encoded like this:

  /-----\X        X/------\X        X/-----\
  |LEFT |X-REPEAT-X|CENTER|X-repeat-X|RIGHT|
  \-----/X        X\------/X        X\-----/

  Putting two lines of magic color in a row will create a blank
  piece.  For example, for an ImageRoll with a center piece but no
  left and right pieces:

  X        X/------\X        X
  X-REPEAT-X|CENTER|X-repeat-X
  X        X\------/X        X

  Once again, the pieces are always assumed to alternate: fixed,
  repeating, fixed, repeating, etc.  The magic color is specified
  when you construct the ImageRoll from a wxImage.

  In the constructor, you also choose whether it is a horizontal or
  vertical ImageRoll (and later a "Frame" as well).  You can also
  choose a "fixed" ImageRoll, which behaves just like a wxImage -
  this is handy so that you can use ImageRolls everywhere you were
  previously using wxImages.

**********************************************************************/

#ifndef __AUDACITY_IMAGE_ROLL__
#define __AUDACITY_IMAGE_ROLL__

#include <wx/defs.h>
#include <wx/dynarray.h>
#include <wx/panel.h>

WX_DECLARE_OBJARRAY(wxBitmap, BitmapArray);
WX_DECLARE_OBJARRAY(wxImage, ImageArray);

class ImageRoll
{
 public:
   enum RollType {
      Uninitialized,
      FixedImage,
      HorizontalRoll,
      VerticalRoll,
      Frame
   };

   ImageRoll();
   ImageRoll(const wxImage &src);
   ImageRoll(RollType type, const wxImage &src, wxColour magicColor);

   bool Ok() const;
   
   wxSize GetMinSize() const { return mMinSize; }
   wxSize GetMaxSize() const { return mMaxSize; }
   
   void Draw(wxDC &dc, wxRect rect,
             int logicalFunc = wxCOPY);

   static ImageArray SplitH(const wxImage &src, wxColour magicColor);
   static ImageArray SplitV(const wxImage &src, wxColour magicColor);
   
 protected:

   void DrawBitmap(wxDC &dc, wxBitmap &bitmap,
                   int x, int y, int logicalFunc = wxCOPY);

   void Init(RollType type, const wxImage &src, wxColour magicColor);

   RollType     mType;
   BitmapArray  mPieces;
   wxSize       mMinSize;
   wxSize       mMaxSize;
};

// A very simple class that just display an ImageRoll that doesn't
// do anything
class ImageRollPanel : public wxPanel
{
 public:
   DECLARE_DYNAMIC_CLASS(ImageRollPanel);
   
   ImageRollPanel(wxWindow *parent,
                  wxWindowID id,
                  ImageRoll &imgRoll,
                  const wxPoint& pos = wxDefaultPosition,
                  const wxSize& size = wxDefaultSize,
                  long style = wxTAB_TRAVERSAL);
   
   void SetLogicalFunction(int func);

   void OnPaint(wxPaintEvent &evt);
   void OnSize(wxSizeEvent &evt);
   
 protected:
   ImageRoll mImageRoll;

   int mLogicalFunction;
   
   DECLARE_EVENT_TABLE();

};

#endif // __AUDACITY_IMAGE_ROLL__

// Indentation settings for Vim and Emacs.
// Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// 

