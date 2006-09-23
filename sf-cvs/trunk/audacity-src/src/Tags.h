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
#include "widgets/ExpandingToolBar.h"
#include "xml/XMLTagHandler.h"

#include <wx/string.h>
#include <wx/dialog.h>

#ifdef USE_LIBFLAC
#include "FLAC++/encoder.h"
#endif

class wxRadioBox;
class wxTextCtrl;
class wxChoice;

class TagsEditor;

class Tags: public XMLTagHandler {

   friend class TagsEditor;

public:
   Tags();  // constructor
   virtual ~Tags();
   
   bool ShowEditDialog(wxWindow *parent, wxString title);
   
   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);
   virtual void WriteXML(XMLWriter &xmlFile);

   // Import any ID3 tags from this file   
   void ImportID3(wxString fileName);
   
   // returns buffer len; caller frees
   // if endOfFile is true, should put the ID3 tags at
   // the END, rather than the beginning, of the MP3 file
   int ExportID3(char **buffer, bool *endOfFile);

#ifdef USE_LIBFLAC
   void ExportFLACTags(FLAC::Encoder::File *encoder);
#endif
   
   void AllowEditTitle(bool editTitle);
   void SetTitle(wxString title);
   wxString GetTitle();

   void AllowEditTrackNumber(bool editTrackNumber);
   void SetTrackNumber(int trackNumber);
   int GetTrackNumber();
   
   bool IsEmpty();

private:
   void EditorIsClosing();

   wxString      mTitle;
   wxString      mArtist;
   wxString      mAlbum;
   int           mTrackNum;
   wxString      mYear;
   int           mGenre;
   wxString      mComments;
   bool          mID3V2;

   wxArrayString mExtraNames;
   wxArrayString mExtraValues;

   bool          mEditTitle;
   bool          mEditTrackNumber;

   TagsEditor   *mTagsEditor;
   ToolBarFrame *mTagsEditorFrame;

#ifdef USE_LIBFLAC
   ::FLAC__StreamMetadata **mFLACMeta;
#endif
};

class TagsEditor: public ExpandingToolBar
{
 public:
   // constructors and destructors
   TagsEditor(wxWindow * parent, wxWindowID id,
              Tags * tags,
              bool editTitle, bool editTrackNumber);

   virtual ~TagsEditor();

   void BuildMainPanel();
   void BuildExtraPanel();

   void RebuildMainPanel();

   virtual bool Validate();
   virtual bool TransferDataToWindow();
   virtual bool TransferDataFromWindow();

 private:
   void OnChange(wxCommandEvent & event);

   void OnClose(wxCommandEvent & event);

   void OnMore(wxCommandEvent & event);
   void OnFewer(wxCommandEvent & event);

   void OnLoad(wxCommandEvent & event);
   void OnSave(wxCommandEvent & event);
   void OnSaveDefaults(wxCommandEvent & event);

   wxTextCtrl  *mTitleText;
   wxTextCtrl  *mArtistText;
   wxTextCtrl  *mAlbumText;
   wxTextCtrl  *mTrackNumText;
   wxTextCtrl  *mYearText;
   wxTextCtrl  *mCommentsText;
   wxChoice    *mGenreChoice;
   wxRadioBox  *mFormatRadioBox;

   int          mNumExtras;
   wxTextCtrl **mExtraNameTexts;
   wxTextCtrl **mExtraValueTexts;

   bool         mTransfering;

 private:
   DECLARE_EVENT_TABLE()

 public:
   Tags *mTags;
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

