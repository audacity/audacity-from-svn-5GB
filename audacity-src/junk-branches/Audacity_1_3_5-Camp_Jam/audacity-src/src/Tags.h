/**********************************************************************

  Audacity: A Digital Audio Editor

  Tags.h

  Dominic Mazzoni

  This class holds a few informational tags, such as Title, Author,
  etc. that can be associated with a project or other audio file.
  It is modeled after the ID3 format for MP3 files, and it can
  both import ID3 tags from MP3 files, and export them as well.

  It can present the user with a dialog for editing this information.

  It only keeps track of the fields that are standard in ID3v1
  (title, author, artist, track num, year, genre, and comments),
  but it can export both ID3v1 or the newer ID3v2 format.  The primary
  reason you would want to export ID3v2 tags instead of ID3v1,
  since we're not supporting any v2 fields, is that ID3v2 tags are
  inserted at the BEGINNING of an mp3 file, which is far more
  useful for streaming.
  
  Use of this functionality requires that libid3tag be compiled in
  with Audacity.
  
**********************************************************************/

#ifndef __AUDACITY_TAGS__
#define __AUDACITY_TAGS__

#include "Audacity.h"
#include "AudacityBranding.h"
#include "widgets/Grid.h"
#include "xml/XMLTagHandler.h"

#include <wx/dialog.h>
#include <wx/hashmap.h>
#include <wx/notebook.h>
#include <wx/string.h>

class wxButton;
class wxChoice;
class wxComboBox;
class wxGridCellChoiceEditor;
class wxRadioButton;
class wxTextCtrl;

class Grid;
class ShuttleGui;
class TagsEditor;

WX_DECLARE_STRING_HASH_MAP(wxString, TagMap);

#define TAG_TITLE     wxT("TITLE")
#define TAG_ARTIST   wxT("ARTIST")
#define TAG_ALBUM    wxT("ALBUM")
#define TAG_TRACK    wxT("TRACKNUMBER")
#define TAG_YEAR     wxT("YEAR")
#if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   #define TAG_PERFORMER wxT("PERFORMER") // in addition to ARTIST
   #define TAG_DATE wxT("DATE") // instead of YEAR
   #define TAG_ISRC wxT("ISRC") 
#else
#endif
#define TAG_GENRE    wxT("GENRE")
#define TAG_COMMENTS wxT("COMMENTS")

class Tags: public XMLTagHandler {

 public:
   Tags();  // constructor
   virtual ~Tags();

   Tags & operator= (const Tags & src );
   
   bool ShowEditDialog(wxWindow *parent, wxString title, bool force = false);
   bool ShowEditDialog1(wxWindow *parent, wxString title, bool force = false);
   bool ShowEditDialog2(wxWindow *parent, wxString title, bool force = false);
   
   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);
   virtual void WriteXML(XMLWriter &xmlFile);

   void SetID3V2(bool id3v2);
   bool GetID3V2();

   void AllowEditTitle(bool editTitle);
   void AllowEditTrackNumber(bool editTrackNumber);

   void LoadDefaultGenres();
   void LoadGenres();

   int GetNumGenres();
   int GetGenre(const wxString & name);
   wxString GetGenre(int value);

   bool HasTag(const wxString & name);
   wxString GetTag(const wxString & name);

   bool GetFirst(wxString & name, wxString & value);
   bool GetNext(wxString & name, wxString & value);

   void SetTag(const wxString & name, const wxString & value);
   void SetTag(const wxString & name, const int & value);

   bool IsEmpty();
   void Clear();

 private:
   void LoadDefaults();

   bool mID3V2;

   TagMap::iterator mIter;
   TagMap mXref;
   TagMap mMap;

   wxArrayString mGenres;

   bool mEditTitle;
   bool mEditTrackNumber;
};

class TagsEditor: public wxDialog
{
 public:
   // constructors and destructors
   TagsEditor(wxWindow * parent,
              wxString title,
              Tags * tags,
              bool editTitle,
              bool editTrackNumber);

   virtual ~TagsEditor();

   void PopulateOrExchange(ShuttleGui & S);

   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

 private:
   void PopulateGenres();
   void SetEditors();

   void OnChange(wxGridEvent & event);

   void OnEdit(wxCommandEvent & event);
   void OnReset(wxCommandEvent & event);

   void OnClear(wxCommandEvent & event);

   void OnLoad(wxCommandEvent & event);
   void OnSave(wxCommandEvent & event);
   void OnSaveDefaults(wxCommandEvent & event);

   void OnAdd(wxCommandEvent & event);
   void OnRemove(wxCommandEvent & event);

   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
   Tags *mTags;
   bool mEditTitle;
   bool mEditTrack;

   Tags mLocal;

   Grid *mGrid;

   DECLARE_EVENT_TABLE()
};

class TagsEditor1: public wxDialog
{
 public:
   // constructors and destructors
   TagsEditor1(wxWindow * parent,
              wxString title,
              Tags * tags,
              bool editTitle,
              bool editTrackNumber);

   virtual ~TagsEditor1();

//   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

 private:
   void Rebuild();
   void PopulateOrExchange(ShuttleGui & S);

   void PopulateGenres();

   void OnEdit(wxCommandEvent & event);
   void OnReset(wxCommandEvent & event);

   void OnClear(wxCommandEvent & event);

   void OnLoad(wxCommandEvent & event);
   void OnSave(wxCommandEvent & event);
   void OnSaveDefaults(wxCommandEvent & event);

   void OnAdd(wxCommandEvent & event);
   void OnRemove(wxCommandEvent & event);

   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
   Tags *mTags;
   bool mEditTitle;
   bool mEditTrack;

   Tags mLocal;

   wxScrolledWindow *mScroller;

   wxTextCtrl *mTitleText;
   wxTextCtrl *mTrackNumText;
   wxComboBox *mGenreCombo;
   wxButton *mClear;
   wxTextCtrl *mEdit;

   wxString mTitle;
   wxString mArtist;
   wxString mAlbum;
   wxString mTrackNum;
   wxString mYear;
   wxString mGenre;
   wxString mComments;
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      wxString mPerformer;
      wxString mDate;
      wxString mISRC;
   #endif

   wxString mCopyright;
   wxString mEncodedBy;
   wxString mComposer;
   wxString mLyricist;

   wxArrayString mCustomTags;
   wxArrayString mCustomVals;

   bool mID3V2;

   bool mTransfering;

   DECLARE_EVENT_TABLE()
};

class TagsEditor2: public wxDialog
{
 public:
   // constructors and destructors
   TagsEditor2(wxWindow * parent,
              wxString title,
              Tags * tags,
              bool editTitle,
              bool editTrackNumber);

   virtual ~TagsEditor2();

   void PopulateOrExchange(ShuttleGui & S);

   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

 private:
   void PopulateGenres();

   bool TransferDataFromBasic();
   bool TransferDataFromAdvanced();
   bool TransferDataToBasic();
   bool TransferDataToAdvanced();

   void OnPage(wxNotebookEvent & event);

   void OnEdit(wxCommandEvent & event);
   void OnReset(wxCommandEvent & event);

   void OnClear(wxCommandEvent & event);

   void OnLoad(wxCommandEvent & event);
   void OnSave(wxCommandEvent & event);
   void OnSaveDefaults(wxCommandEvent & event);

   void OnAdd(wxCommandEvent & event);
   void OnRemove(wxCommandEvent & event);

   void OnOk(wxCommandEvent & event);
   void OnCancel(wxCommandEvent & event);

 private:
   Tags *mTags;
   bool mEditTitle;
   bool mEditTrack;

   Tags mLocal;

   wxNotebook *mNotebook;
   wxNotebookPage *mBasic;
   wxNotebookPage *mAdvanced;
   wxButton *mClear;

   ChoiceEditor *mChoice;
   wxGridCellChoiceEditor *mCombo;

   wxTextCtrl *mTitleText;
   wxTextCtrl *mTrackNumText;
   wxComboBox *mGenreCombo;
   Grid *mGrid;

   wxString mTitle;
   wxString mArtist;
   wxString mAlbum;
   wxString mTrackNum;
   wxString mYear;
   wxString mGenre;
   wxString mComments;
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      wxString mPerformer;
      wxString mDate;
      wxString mISRC;
   #endif

   bool mID3V2;

   bool mTransfering;

   DECLARE_EVENT_TABLE()
};

#endif

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 5f3abeea-3488-4afc-8d03-c5baf6dbff71

