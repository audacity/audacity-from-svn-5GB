/**********************************************************************

  Audacity: A Digital Audio Editor

  Tags.cpp

  Dominic Mazzoni
 
*******************************************************************//**

\class Tags
\brief ID3 Tags (for MP3)

  This class started as an ID3 tag 

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

*//****************************************************************//**

\class TagsEditor
\brief Derived from ExpandingToolBar, this dialog allows editing of Tags.

*//*******************************************************************/

#include "Tags.h"

#include "Audacity.h"
#include "FileDialog.h"
#include "FileNames.h"
#include "Internat.h"
#include "Prefs.h"
#include "ShuttleGui.h"
#include "widgets/Grid.h"
#include "xml/XMLFileReader.h"

#include <wx/button.h>
#include <wx/choice.h>
#include <wx/filename.h>
#include <wx/intl.h>
#include <wx/listctrl.h>
#include <wx/msgdlg.h>
#include <wx/notebook.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/string.h>
#include <wx/textctrl.h>
#include <wx/textfile.h>
#include <wx/combobox.h>

#ifdef _DEBUG
    #ifdef _MSC_VER
        #undef THIS_FILE
        static char*THIS_FILE= __FILE__;
        #define new new(_NORMAL_BLOCK, THIS_FILE, __LINE__)
    #endif
#endif

static const wxChar *DefaultGenres[] =
{
   wxT("Blues"),
   wxT("Classic Rock"),
   wxT("Country"),
   wxT("Dance"),
   wxT("Disco"),
   wxT("Funk"),
   wxT("Grunge"),
   wxT("Hip-Hop"),
   wxT("Jazz"),
   wxT("Metal"),
   wxT("New Age"),
   wxT("Oldies"),
   wxT("Other"),
   wxT("Pop"),
   wxT("R&B"),
   wxT("Rap"),
   wxT("Reggae"),
   wxT("Rock"),
   wxT("Techno"),
   wxT("Industrial"),
   wxT("Alternative"),
   wxT("Ska"),
   wxT("Death Metal"),
   wxT("Pranks"),
   wxT("Soundtrack"),
   wxT("Euro-Techno"),
   wxT("Ambient"),
   wxT("Trip-Hop"),
   wxT("Vocal"),
   wxT("Jazz+Funk"),
   wxT("Fusion"),
   wxT("Trance"),
   wxT("Classical"),
   wxT("Instrumental"),
   wxT("Acid"),
   wxT("House"),
   wxT("Game"),
   wxT("Sound Clip"),
   wxT("Gospel"),
   wxT("Noise"),
   wxT("Alt. Rock"),
   wxT("Bass"),
   wxT("Soul"),
   wxT("Punk"),
   wxT("Space"),
   wxT("Meditative"),
   wxT("Instrumental Pop"),
   wxT("Instrumental Rock"),
   wxT("Ethnic"),
   wxT("Gothic"),
   wxT("Darkwave"),
   wxT("Techno-Industrial"),
   wxT("Electronic"),
   wxT("Pop-Folk"),
   wxT("Eurodance"),
   wxT("Dream"),
   wxT("Southern Rock"),
   wxT("Comedy"),
   wxT("Cult"),
   wxT("Gangsta Rap"),
   wxT("Top 40"),
   wxT("Christian Rap"),
   wxT("Pop/Funk"),
   wxT("Jungle"),
   wxT("Native American"),
   wxT("Cabaret"),
   wxT("New Wave"),
   wxT("Psychedelic"),
   wxT("Rave"),
   wxT("Showtunes"),
   wxT("Trailer"),
   wxT("Lo-Fi"),
   wxT("Tribal"),
   wxT("Acid Punk"),
   wxT("Acid Jazz"),
   wxT("Polka"),
   wxT("Retro"),
   wxT("Musical"),
   wxT("Rock & Roll"),
   wxT("Hard Rock"),
   wxT("Folk"),
   wxT("Folk/Rock"),
   wxT("National Folk"),
   wxT("Swing"),
   wxT("Fast-Fusion"),
   wxT("Bebob"),
   wxT("Latin"),
   wxT("Revival"),
   wxT("Celtic"),
   wxT("Bluegrass"),
   wxT("Avantgarde"),
   wxT("Gothic Rock"),
   wxT("Progressive Rock"),
   wxT("Psychedelic Rock"),
   wxT("Symphonic Rock"),
   wxT("Slow Rock"),
   wxT("Big Band"),
   wxT("Chorus"),
   wxT("Easy Listening"),
   wxT("Acoustic"),
   wxT("Humour"),
   wxT("Speech"),
   wxT("Chanson"),
   wxT("Opera"),
   wxT("Chamber Music"),
   wxT("Sonata"),
   wxT("Symphony"),
   wxT("Booty Bass"),
   wxT("Primus"),
   wxT("Porn Groove"),
   wxT("Satire"),
   wxT("Slow Jam"),
   wxT("Club"),
   wxT("Tango"),
   wxT("Samba"),
   wxT("Folklore"),
   wxT("Ballad"),
   wxT("Power Ballad"),
   wxT("Rhythmic Soul"),
   wxT("Freestyle"),
   wxT("Duet"),
   wxT("Punk Rock"),
   wxT("Drum Solo"),
   wxT("A Cappella"),
   wxT("Euro-House"),
   wxT("Dance Hall"),
   wxT("Goa"),
   wxT("Drum & Bass"),
   wxT("Club-House"),
   wxT("Hardcore"),
   wxT("Terror"),
   wxT("Indie"),
   wxT("BritPop"),

   // Standard name is offensive (see "http://www.audacityteam.org/forum/viewtopic.php?f=11&t=3924").
   wxT("Offensive"), // wxT("Negerpunk"),

   wxT("Polsk Punk"),
   wxT("Beat"),
   wxT("Christian Gangsta Rap"),
   wxT("Heavy Metal"),
   wxT("Black Metal"),
   wxT("Crossover"),
   wxT("Contemporary Christian"),
   wxT("Christian Rock"),
   wxT("Merengue"),
   wxT("Salsa"),
   wxT("Thrash Metal"),
   wxT("Anime"),
   wxT("JPop"),
   wxT("Synthpop")
};

Tags::Tags()
{
   mID3V2 = true;

   mEditTitle = true;
   mEditTrackNumber = true;

   LoadDefaults();
   LoadGenres();
}

Tags::~Tags()
{
}

Tags & Tags::operator=(const Tags & src)
{
   mID3V2 = src.mID3V2;

   mEditTitle = src.mEditTitle;
   mEditTrackNumber = src.mEditTrackNumber;

   mXref.clear();
   mXref = src.mXref;
   mMap.clear();
   mMap = src.mMap;

   mGenres.clear();
   mGenres = src.mGenres;

   return *this;
}

void Tags::LoadDefaults()
{
   wxString path;
   wxString name;
   wxString value;
   long ndx;
   bool cont;

   // Set the parent group
   path = gPrefs->GetPath();
   gPrefs->SetPath(wxT("/Tags"));

   // Process all entries in the group
   cont = gPrefs->GetFirstEntry(name, ndx);
   while (cont) {
      gPrefs->Read(name, &value, wxT(""));

      if (name == wxT("ID3V2")) {
         mID3V2 = value == wxT("1");
      }
      else {
         SetTag(name, value);
      }

      cont = gPrefs->GetNextEntry(name, ndx);
   }

   // Restore original group
   gPrefs->SetPath(path);
}

bool Tags::IsEmpty()
{
   // At least one of these should be filled in, otherwise
   // it's assumed that the tags have not been set...
   if (HasTag(TAG_TITLE) || HasTag(TAG_ARTIST) || HasTag(TAG_ALBUM)) {
      return false;
   }

   return true;
}

void Tags::Clear()
{
   mID3V2 = true;
   mXref.clear();
   mMap.clear();
}

void Tags::SetID3V2(bool id3v2)
{
   mID3V2 = id3v2;
}

bool Tags::GetID3V2()
{
   return mID3V2;
}

void Tags::AllowEditTitle(bool editTitle)
{
   mEditTitle = editTitle;
}

void Tags::AllowEditTrackNumber(bool editTrackNumber)
{
   mEditTrackNumber = editTrackNumber;
}

int Tags::GetNumGenres()
{
   return mGenres.GetCount();
}

void Tags::LoadDefaultGenres()
{
   mGenres.Clear();
   for (size_t i = 0; i < WXSIZEOF(DefaultGenres); i++) {
      mGenres.Add(DefaultGenres[i]);
   }
}

void Tags::LoadGenres()
{
   wxFileName fn(FileNames::DataDir(), wxT("genres.txt"));
   wxTextFile tf(fn.GetFullPath());

   if (!tf.Exists() || !tf.Open()) {
      LoadDefaultGenres();
      return;
   }

   mGenres.Clear();

   int cnt = tf.GetLineCount();
   for (int i = 0; i < cnt; i++) {
      mGenres.Add(tf.GetLine(i));
   }
}

wxString Tags::GetGenre(int i)
{
   if (i >= 0 && i < GetNumGenres()) {
      return mGenres[i];
   }

   return wxT("");
}

int Tags::GetGenre(const wxString & name)
{
   int cnt = WXSIZEOF(DefaultGenres);

   for (int i = 0; i < cnt; i++) {
      if (name.CmpNoCase(DefaultGenres[i])) {
         return i;
      }
   }

   return 0;
}

bool Tags::HasTag(const wxString & name)
{
   wxString key = name;
   key.UpperCase();

   TagMap::iterator iter = mXref.find(key);
   return (iter != mXref.end());
}

wxString Tags::GetTag(const wxString & name)
{
   wxString key = name;
   key.UpperCase();

   TagMap::iterator iter = mXref.find(key);

   if (iter == mXref.end()) {
      return wxEmptyString;
   }

   return mMap[iter->second];
}

bool Tags::GetFirst(wxString & name, wxString & value)
{
   mIter = mMap.begin();
   if (mIter == mMap.end()) {
      return false;
   }

   name = mIter->first;
   value = mIter->second;

   return true;
}

bool Tags::GetNext(wxString & name, wxString & value)
{
   mIter++;
   if (mIter == mMap.end()) {
      return false;
   }

   name = mIter->first;
   value = mIter->second;

   return true;
}

void Tags::SetTag(const wxString & name, const wxString & value)
{
   // We don't like empty names
   if (name.IsEmpty()) {
      return;
   }

   // All keys are uppercase
   wxString key = name;
   key.UpperCase();

   // Look it up
   TagMap::iterator iter = mXref.find(key);

   // Didn't find the tag
   if (iter == mXref.end()) {
      // Intention was to delete so no need to add it
      if (value.IsEmpty()) {
         return;
      }

      // Add a new tag
      mXref[key] = name;
      mMap[name] = value;
      return;
   }

   // Intention was to delete
   if (value.IsEmpty()) {
      mMap.erase(iter->second);
      mXref.erase(iter);
      return;
   }

   // Update the value
   mMap[iter->second] = value;
}

void Tags::SetTag(const wxString & name, const int & value)
{
   SetTag(name, wxString::Format(wxT("%d"), value));
}

bool Tags::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (wxStrcmp(tag, wxT("tags")) == 0) {
      return true;
   }

   if (wxStrcmp(tag, wxT("tag")) == 0) {
      wxString n, v;

      while (*attrs) {
         wxString attr = *attrs++;
         if (!*attr)
            break;
         wxString value = *attrs++;

         if (!XMLValueChecker::IsGoodString(attr) ||
             !XMLValueChecker::IsGoodString(value)) {
            break;
         }

         if (attr == wxT("name")) {
            n = value;
         }
         else if (attr == wxT("value")) {
            v = value;
         }
      }

      if (n == wxT("id3v2")) {
         long nValue;
         if (XMLValueChecker::IsGoodInt(v) && v.ToLong(&nValue)) {
            mID3V2 = (nValue != 0);
         }
      }
      else {
         SetTag(n, v);
      }

      return true;
   }

   return false;
}

XMLTagHandler *Tags::HandleXMLChild(const wxChar *tag)
{
   if (wxStrcmp(tag, wxT("tags")) == 0) {
      return this;
   }

   if (wxStrcmp(tag, wxT("tag")) == 0) {
      return this;
   }

   return NULL;
}

void Tags::WriteXML(XMLWriter &xmlFile)
{
   xmlFile.StartTag(wxT("tags"));

   xmlFile.StartTag(wxT("tag"));
   xmlFile.WriteAttr(wxT("name"), wxT("id3v2"));
   xmlFile.WriteAttr(wxT("value"), mID3V2);
   xmlFile.EndTag(wxT("tag"));

   wxString n, v;
   for (bool cont = GetFirst(n, v); cont; cont = GetNext(n, v)) {
      xmlFile.StartTag(wxT("tag"));
      xmlFile.WriteAttr(wxT("name"), n);
      xmlFile.WriteAttr(wxT("value"), v);
      xmlFile.EndTag(wxT("tag"));
   }

   xmlFile.EndTag(wxT("tags"));
}

bool Tags::ShowEditDialog(wxWindow *parent, wxString title, bool force)
{
   if (force) {
      TagsEditor dlg(parent, title, this, mEditTitle, mEditTrackNumber);

      return dlg.ShowModal() == wxID_OK;
   }

   return true;
}

bool Tags::ShowEditDialog1(wxWindow *parent, wxString title, bool force)
{
   if (force || IsEmpty()) {
      TagsEditor1 dlg(parent, title, this, mEditTitle, mEditTrackNumber);

      return dlg.ShowModal() == wxID_OK;
   }

   return true;
}

bool Tags::ShowEditDialog2(wxWindow *parent, wxString title, bool force)
{
   if (force || IsEmpty()) {
      TagsEditor2 dlg(parent, title, this, mEditTitle, mEditTrackNumber);

      return dlg.ShowModal() == wxID_OK;
   }

   return true;
}

//
// ComboEditor - Wrapper to prevent unwanted background erasure
//

class ComboEditor:public wxGridCellChoiceEditor
{
public:
   ComboEditor(const wxArrayString& choices, bool allowOthers = false)
   :  wxGridCellChoiceEditor(choices, allowOthers)
   {
   }

   virtual void PaintBackground(const wxRect& rectCell, wxGridCellAttr *attr)
   {
      // Ignore it (a must on the Mac as the erasure causes problems.)
   }

   void SetParameters(const wxString& params)
   {
      wxGridCellChoiceEditor::SetParameters(params);

      // Refresh the wxComboBox with new values
      if (Combo()) {
         Combo()->Clear();
         Combo()->Append(m_choices);
      }
   }

   void SetSize(const wxRect& rectOrig)
   {
      wxRect rect(rectOrig);
      wxRect r = Combo()->GetRect();

      // Center the combo box in or over the cell
      rect.y -= (r.GetHeight() - rect.GetHeight()) / 2;
      rect.height = r.GetHeight();

      wxGridCellChoiceEditor::SetSize(rect);
   }
};

//
// Editor
//

#if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
   #define LABEL_ARTIST    _("ARTIST")
   #define LABEL_PERFORMER _("PERFORMER")
   #define LABEL_TITLE     _("TITLE")
   #define LABEL_GENRE     _("GENRE")
   #define LABEL_DATE      _("DATE")
   #define LABEL_ISRC      _("ISRC")
   #define LABEL_COMMENTS  _("COMMENTS")
   // Define these although they shouldn't show up in Jamling versions, 
   // just so less standard code has to be #ifdef-ed out. 
   #define LABEL_ALBUM     _("Album Title")
   #define LABEL_TRACK     _("Track Number")
   #define LABEL_YEAR      _("Year")

   static wxString names[] =
   {
      LABEL_ARTIST,
      LABEL_PERFORMER, 
      LABEL_TITLE,
      LABEL_GENRE,
      LABEL_DATE,
      LABEL_ISRC,
      LABEL_COMMENTS
   };

   static struct
   {
      wxString label;
      wxString name;
   }
   labelmap[] =
   {
      {  LABEL_ARTIST,     TAG_ARTIST     },
      {  LABEL_PERFORMER,  TAG_PERFORMER  },
      {  LABEL_TITLE,      TAG_TITLE      },
      {  LABEL_GENRE,      TAG_GENRE      },
      {  LABEL_DATE,       TAG_DATE       },
      {  LABEL_ISRC,       TAG_ISRC       },
      {  LABEL_COMMENTS,   TAG_COMMENTS   }
   };
#else
   #define LABEL_ARTIST    _("Artist Name")
   #define LABEL_TITLE     _("Track Title")
   #define LABEL_ALBUM     _("Album Title")
   #define LABEL_TRACK     _("Track Number")
   #define LABEL_YEAR      _("Year")
   #define LABEL_GENRE     _("Genre")
   #define LABEL_COMMENTS  _("Comments")

   static wxString names[] =
   {
      LABEL_ARTIST,
      LABEL_TITLE,
      LABEL_ALBUM,
      LABEL_TRACK,
      LABEL_YEAR,
      LABEL_GENRE,
      LABEL_COMMENTS
   };

   static struct
   {
      wxString label;
      wxString name;
   }
   labelmap[] =
   {
      {  LABEL_ARTIST,     TAG_ARTIST     },
      {  LABEL_TITLE,      TAG_TITLE      },
      {  LABEL_ALBUM,      TAG_ALBUM      },
      {  LABEL_TRACK,      TAG_TRACK      },
      {  LABEL_YEAR,       TAG_YEAR       },
      {  LABEL_GENRE,      TAG_GENRE      },
      {  LABEL_COMMENTS,   TAG_COMMENTS   }
   };
#endif

#define STATICCNT WXSIZEOF(labelmap)

enum {
   ClearID = 10000,
   EditID,
   ResetID,
   LoadID,
   SaveID,
   SaveDefaultsID,
   AddID,
   RemoveID
};

BEGIN_EVENT_TABLE(TagsEditor, wxDialog)
   EVT_GRID_CELL_CHANGE(TagsEditor::OnChange)
   EVT_BUTTON(EditID, TagsEditor::OnEdit)
   EVT_BUTTON(ResetID, TagsEditor::OnReset)
   EVT_BUTTON(ClearID, TagsEditor::OnClear)
   EVT_BUTTON(LoadID, TagsEditor::OnLoad)
   EVT_BUTTON(SaveID, TagsEditor::OnSave)
   EVT_BUTTON(SaveDefaultsID, TagsEditor::OnSaveDefaults)
   EVT_BUTTON(AddID, TagsEditor::OnAdd)
   EVT_BUTTON(RemoveID, TagsEditor::OnRemove)
   EVT_BUTTON(wxID_CANCEL, TagsEditor::OnCancel)
   EVT_BUTTON(wxID_OK, TagsEditor::OnOk)
END_EVENT_TABLE()

TagsEditor::TagsEditor(wxWindow * parent,
                       wxString title,
                       Tags * tags,
                       bool editTitle,
                       bool editTrack)
:  wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
   mTags(tags),
   mEditTitle(editTitle),
   mEditTrack(editTrack)
{
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      names[0] = LABEL_ARTIST;
      names[1] = LABEL_PERFORMER;
      names[2] = LABEL_TITLE;
      names[3] = LABEL_GENRE;
      names[4] = LABEL_DATE;
      names[5] = LABEL_ISRC;
      names[6] = LABEL_COMMENTS;

      labelmap[0].label = LABEL_ARTIST;
      labelmap[1].label = LABEL_PERFORMER;
      labelmap[2].label = LABEL_TITLE;
      labelmap[3].label = LABEL_GENRE;
      labelmap[4].label = LABEL_DATE;
      labelmap[5].label = LABEL_ISRC;
      labelmap[6].label = LABEL_COMMENTS;

      labelmap[0].name = TAG_ARTIST;
      labelmap[1].name = TAG_PERFORMER;
      labelmap[2].name = TAG_TITLE;
      labelmap[3].name = TAG_GENRE;
      labelmap[4].name = TAG_DATE;
      labelmap[5].name = TAG_ISRC;
      labelmap[6].name = TAG_COMMENTS;
   #else
      names[0] = LABEL_ARTIST;
      names[1] = LABEL_TITLE;
      names[2] = LABEL_ALBUM;
      names[3] = LABEL_TRACK;
      names[4] = LABEL_YEAR;
      names[5] = LABEL_GENRE;
      names[6] = LABEL_COMMENTS;

      labelmap[0].label = LABEL_ARTIST;
      labelmap[1].label = LABEL_TITLE;
      labelmap[2].label = LABEL_ALBUM;
      labelmap[3].label = LABEL_TRACK;
      labelmap[4].label = LABEL_YEAR;
      labelmap[5].label = LABEL_GENRE;
      labelmap[6].label = LABEL_COMMENTS;

      labelmap[0].name = TAG_ARTIST;
      labelmap[1].name = TAG_TITLE;
      labelmap[2].name = TAG_ALBUM;
      labelmap[3].name = TAG_TRACK;
      labelmap[4].name = TAG_YEAR;
      labelmap[5].name = TAG_GENRE;
      labelmap[6].name = TAG_COMMENTS;
   #endif

   mGrid = NULL;

   // Make a local copy of the passed in tags
   mLocal = *mTags;

   // Build, size, and position the dialog
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);

   TransferDataToWindow();

   Layout();
   Fit();
   Center();
   SetSizeHints(GetSize());

   // Restore the original tags because TransferDataToWindow() will be called again
   mLocal.Clear();
   mLocal = *mTags;

   // Override size and position with last saved
   wxRect r = GetRect();
   gPrefs->Read(wxT("/TagsEditor/x"), &r.x, r.x);
   gPrefs->Read(wxT("/TagsEditor/y"), &r.y, r.y);
   gPrefs->Read(wxT("/TagsEditor/width"), &r.width, r.width);
   gPrefs->Read(wxT("/TagsEditor/height"), &r.height, r.height);
   Move(r.GetPosition());
//   SetSize(r.GetSize());

   // Resize value column width based on width of columns and the vertical scrollbar
   wxScrollBar sb(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSB_VERTICAL);
   r = mGrid->GetClientRect();
   r.width -= mGrid->GetColSize(0);
   r.width -= sb.GetSize().GetWidth();
   r.width -= 10;
   r.width -= r.x;
   mGrid->SetColSize(1, r.width);

   // Load the genres
   PopulateGenres();
}

TagsEditor::~TagsEditor()
{
}

void TagsEditor::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay();
   {
      S.StartHorizontalLay(wxALIGN_LEFT, false);
      {
         S.AddUnits(_("Press F2 or double click to edit."));
      }
      S.EndHorizontalLay();

      if (mGrid == NULL) {
         mGrid = new Grid(S.GetParent(),
                          wxID_ANY,
                          wxDefaultPosition,
                          wxDefaultSize,
                          wxSUNKEN_BORDER);

         mGrid->RegisterDataType(wxT("Combo"),
                                 new wxGridCellStringRenderer,
                                 new ComboEditor(wxArrayString(), true));

         mGrid->SetColLabelSize(mGrid->GetDefaultRowSize());

         wxArrayString cs(WXSIZEOF(names), names);

         // Build the initial (empty) grid
         mGrid->CreateGrid(0, 2); 
         mGrid->SetRowLabelSize(0);
         mGrid->SetDefaultCellAlignment(wxALIGN_LEFT, wxALIGN_CENTER);
         mGrid->SetDefaultCellBackgroundColour(*wxWHITE);
         mGrid->SetColLabelValue(0, _("Tag Name"));
         mGrid->SetColLabelValue(1, _("Tag Value"));

         // Resize the name column and set default row height.
         wxComboBox tc(this, wxID_ANY, wxT(""), wxDefaultPosition, wxDefaultSize, cs);
         mGrid->SetColSize(0, tc.GetSize().x);
         mGrid->SetColMinimalWidth(0, tc.GetSize().x);
      }
      S.Prop(true);
      S.AddWindow(mGrid, wxEXPAND | wxALL);

      S.StartMultiColumn(4, wxALIGN_CENTER);
      {
         S.Id(AddID).AddButton(_("&Add"));
         S.Id(RemoveID).AddButton(_("&Remove"));
         S.AddTitle(wxT(" "));
         S.Id(ClearID).AddButton(_("&Clear"));
      }
      S.EndMultiColumn();

      S.StartHorizontalLay(wxALIGN_CENTRE, false);
      {
         S.StartStatic(_("Genres"));
         {
            S.StartMultiColumn(4, wxALIGN_CENTER);
            {
               S.Id(EditID).AddButton(_("E&dit..."));
               S.Id(ResetID).AddButton(_("Rese&t..."));
            }
            S.EndMultiColumn();
         }
         S.EndStatic();
         S.StartStatic(_("Template"));
         {
            S.StartMultiColumn(4, wxALIGN_CENTER);
            {
               S.Id(LoadID).AddButton(_("&Load..."));
               S.Id(SaveID).AddButton(_("&Save..."));
               S.AddTitle(wxT(" "));
               S.Id(SaveDefaultsID).AddButton(_("S&et Default"));
            }
            S.EndMultiColumn();
         }
         S.EndStatic();
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   S.AddStandardButtons(eOkButton | eCancelButton);
}

bool TagsEditor::TransferDataFromWindow()
{
   int i, cnt = mGrid->GetNumberRows();

   if (mGrid->IsCellEditControlShown()) {
      mGrid->SaveEditControlValue();
      mGrid->HideCellEditControl();
   }

   mLocal.Clear();
   for (i = 0; i < cnt; i++) {
      wxString n = mGrid->GetCellValue(i, 0);
      wxString v = mGrid->GetCellValue(i, 1);

      if (n.IsEmpty()) {
         continue;
      }

      if (n.CmpNoCase(LABEL_ARTIST) == 0) {
         n = TAG_ARTIST;
      }
      else if (n.CmpNoCase(LABEL_TITLE) == 0) {
         n = TAG_TITLE;
      }
      else if (n.CmpNoCase(LABEL_ALBUM) == 0) {
         n = TAG_ALBUM;
      }
      else if (n.CmpNoCase(LABEL_TRACK) == 0) {
         n = TAG_TRACK;
      }
      else if (n.CmpNoCase(LABEL_YEAR) == 0) {
         n = TAG_YEAR;
      }
      else if (n.CmpNoCase(LABEL_GENRE) == 0) {
         n = TAG_GENRE;
      }
      else if (n.CmpNoCase(LABEL_COMMENTS) == 0) {
         n = TAG_COMMENTS;
      }
      #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
         else if (n.CmpNoCase(LABEL_PERFORMER) == 0) {
            n = TAG_PERFORMER;
         }
         else if (n.CmpNoCase(LABEL_DATE) == 0) {
            n = TAG_DATE;
         }
         else if (n.CmpNoCase(LABEL_ISRC) == 0) {
            n = TAG_ISRC;
         }
      #endif

      mLocal.SetTag(n, v);
   }

   return true;
}

bool TagsEditor::TransferDataToWindow()
{
   size_t i;
   wxString n;
   wxString v;

   // Disable redrawing until we're done
   mGrid->BeginBatch();

   // Delete all rows
   if (mGrid->GetNumberRows()) {
      mGrid->DeleteRows(0, mGrid->GetNumberRows());
   }

   // Populate the static rows
   for (i = 0; i < STATICCNT; i++) {
      mGrid->AppendRows();

      mGrid->SetReadOnly(i, 0);
      mGrid->SetCellValue(i, 0, labelmap[i].label);
      mGrid->SetCellValue(i, 1, mLocal.GetTag(labelmap[i].name));

      if (!mEditTitle && mGrid->GetCellValue(i, 0).CmpNoCase(LABEL_TITLE) == 0) {
         mGrid->SetReadOnly(i, 1);
      }

      if (!mEditTrack && mGrid->GetCellValue(i, 0).CmpNoCase(LABEL_TRACK) == 0) {
         mGrid->SetReadOnly(i, 1);
      }

      mLocal.SetTag(labelmap[i].name, wxEmptyString);
   }

   // Populate the rest
   for (bool cont = mLocal.GetFirst(n, v); cont; cont = mLocal.GetNext(n, v)) {
      mGrid->AppendRows();

      mGrid->SetCellValue(i, 0, n);
      mGrid->SetCellValue(i, 1, v);
      i++;
   }

   // Add an extra one to help with initial sizing and to show it can be done
   mGrid->AppendRows(1);

   // We're done, so allow the grid to redraw
   mGrid->EndBatch();

   // Set the editors
   SetEditors();

   return true;
}

void TagsEditor::OnChange(wxGridEvent & event)
{
   static bool ischanging = false;

   // Prevent recursion
   if (ischanging) {
      return;
   }

   event.Skip();

   if (event.GetCol() != 0) {
      return;
   }

   wxString n = mGrid->GetCellValue(event.GetRow(), 0);
   for (size_t i = 0; i < STATICCNT; i++) {
      if (n.CmpNoCase(labelmap[i].label) == 0) {
         ischanging = true;
         wxBell();
         mGrid->SetGridCursor(i, 0);
         event.Veto();
         ischanging = false;
         break;
      }
   }

   return;
}

void TagsEditor::OnEdit(wxCommandEvent & event)
{
   if (mGrid->IsCellEditControlShown()) {
      mGrid->SaveEditControlValue();
      mGrid->HideCellEditControl();
   }

   wxDialog dlg(this, wxID_ANY, _("Edit Genres"),
                wxDefaultPosition, wxDefaultSize,
                wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);
   wxTextCtrl *tc;

   ShuttleGui S(&dlg, eIsCreating);

   S.StartVerticalLay(true);
   {
      tc = S.AddTextWindow(wxT(""));
   }
   S.EndVerticalLay();

   S.AddStandardButtons();

   wxSortedArrayString g;
   int cnt = mLocal.GetNumGenres();
   for (int i = 0; i < cnt; i++) {
      g.Add(mLocal.GetGenre(i));
   }

   for (int i = 0; i < cnt; i++) {
      tc->AppendText(g[i] + wxT("\n"));
   }

   dlg.Center();
   if (dlg.ShowModal() == wxID_CANCEL) {
      return;
   }

   wxFileName fn(FileNames::DataDir(), wxT("genres.txt"));
   wxFile f(fn.GetFullPath(), wxFile::write);
   if (!f.IsOpened() || !f.Write(tc->GetValue())) {
      wxMessageBox(_("Unable to save genre file."), _("Reset Genres"));
      return;
   }

   mLocal.LoadGenres();

   PopulateGenres();
}

void TagsEditor::OnReset(wxCommandEvent & event)
{
   int id = wxMessageBox(_("Are you sure you want to reset the genre list to defaults?"),
                         _("Reset Genres"),
                         wxYES_NO);

   if (id == wxNO) {
      return;
   }
   mLocal.LoadDefaultGenres();

   wxFileName fn(FileNames::DataDir(), wxT("genres.txt"));
   wxTextFile tf(fn.GetFullPath());

   bool open = (tf.Exists() && tf.Open()) ||
               (!tf.Exists() && tf.Create());

   if (!open) {
      wxMessageBox(_("Unable to open genre file."), _("Reset Genres"));
      mLocal.LoadGenres();
      return;
   }

   tf.Clear();
   int cnt = mLocal.GetNumGenres();
   for (int i = 0; i < cnt; i++) {
      tf.AddLine(mLocal.GetGenre(i));
   }

   tf.Write();
   if (!tf.Write()) {
      wxMessageBox(_("Unable to save genre file."), _("Reset Genres"));
      mLocal.LoadGenres();
      return;
   }

   mLocal.LoadGenres();

   PopulateGenres();
}

void TagsEditor::OnClear(wxCommandEvent & event)
{
   mLocal.Clear();

   TransferDataToWindow();
}

void TagsEditor::OnLoad(wxCommandEvent & event)
{
   wxString fn;

   // Ask the user for the real name
   fn = FileSelector(_("Load Metadata As:"),
                     FileNames::DataDir(),
                     wxT("Tags.xml"),
                     wxT("xml"),
                     wxT("*.xml"),
                     wxOPEN,
                     this);

   // User canceled...
   if (fn.IsEmpty()) {
      return;
   }

   // Remember title and track in case they're read only
   wxString title = mLocal.GetTag(TAG_TITLE);
   wxString track = mLocal.GetTag(TAG_TRACK);

   // Clear current contents
   mLocal.Clear();

   // Load the metadata
   XMLFileReader reader;
   if (!reader.Parse(&mLocal, fn)) {
      // Inform user of load failure
      wxMessageBox(reader.GetErrorStr(),
                   _("Error loading metadata"),
                   wxOK | wxCENTRE,
                   this);
   }

   // Restore title
   if (!mEditTitle) {
      mLocal.SetTag(TAG_TITLE, title);
   }

   // Restore track
   if (!mEditTrack) {
      mLocal.SetTag(TAG_TRACK, track);
   }

   // Go fill up the window
   TransferDataToWindow();

   return;
}

void TagsEditor::OnSave(wxCommandEvent & event)
{
   wxString fn;

   // Refresh tags
   TransferDataFromWindow();

   // Ask the user for the real name
   fn = FileSelector(_("Save Metadata As:"),
                     FileNames::DataDir(),
                     wxT("Tags.xml"),
                     wxT("xml"),
                     wxT("*.xml"),
                     wxSAVE | wxOVERWRITE_PROMPT,
                     this);

   // User canceled...
   if (fn.IsEmpty()) {
      return;
   }

   // Create/Open the file
   XMLFileWriter writer;
   writer.Open(fn, wxT("wb"));

   // Complain if open failed
   if (!writer.IsOpened())
   {
      // Constructor will emit message
      return;
   }

   // Remember title and track in case they're read only
   wxString title = mLocal.GetTag(TAG_TITLE);
   wxString track = mLocal.GetTag(TAG_TRACK);

   // Clear title
   if (!mEditTitle) {
      mLocal.SetTag(TAG_TITLE, wxEmptyString);
   }

   // Clear track
   if (!mEditTrack) {
      mLocal.SetTag(TAG_TRACK, wxEmptyString);
   }

   // Write the metadata
   mLocal.WriteXML(writer);

   // Restore title
   if (!mEditTitle) {
      mLocal.SetTag(TAG_TITLE, title);
   }

   // Restore track
   if (!mEditTrack) {
      mLocal.SetTag(TAG_TRACK, track);
   }

   // Close the file
   writer.Close();

   return;
}

void TagsEditor::OnSaveDefaults(wxCommandEvent & event)
{
   // Refresh tags
   TransferDataFromWindow();

   // Remember title and track in case they're read only
   wxString title = mLocal.GetTag(TAG_TITLE);
   wxString track = mLocal.GetTag(TAG_TRACK);

   // Clear title
   if (!mEditTitle) {
      mLocal.SetTag(TAG_TITLE, wxEmptyString);
   }

   // Clear track
   if (!mEditTrack) {
      mLocal.SetTag(TAG_TRACK, wxEmptyString);
   }

   // Remove any previous defaults
   gPrefs->DeleteGroup(wxT("/Tags"));

   // Write out each tag
   wxString n, v;
   for (bool cont = mLocal.GetFirst(n, v); cont; cont = mLocal.GetNext(n, v)) {
      gPrefs->Write(wxT("/Tags/") + n, v);
   }

   // Restore title
   if (!mEditTitle) {
      mLocal.SetTag(TAG_TITLE, title);
   }

   // Restore track
   if (!mEditTrack) {
      mLocal.SetTag(TAG_TRACK, track);
   }
}

void TagsEditor::OnAdd(wxCommandEvent & event)
{
   mGrid->AppendRows();
}

void TagsEditor::OnRemove(wxCommandEvent & event)
{
   size_t row = mGrid->GetCursorRow();

   if (!mEditTitle && mGrid->GetCellValue(row, 0).CmpNoCase(LABEL_TITLE) == 0) {
      return;
   }
   else if (!mEditTrack && mGrid->GetCellValue(row, 0).CmpNoCase(LABEL_TRACK) == 0) {
      return;
   }
   else if (row < STATICCNT - 1) {
      mGrid->SetCellValue(row, 1, wxEmptyString);
   }
   else if (row >= STATICCNT) {
      mGrid->DeleteRows(row, 1);
   }
}

void TagsEditor::OnOk(wxCommandEvent & event)
{
   if (mGrid->IsCellEditControlShown()) {
      mGrid->SaveEditControlValue();
      mGrid->HideCellEditControl();
      return;
   }

   if (!Validate() || !TransferDataFromWindow()) {
      return;
   }

   *mTags = mLocal;

   wxRect r = GetRect();
   gPrefs->Write(wxT("/TagsEditor/x"), r.x);
   gPrefs->Write(wxT("/TagsEditor/y"), r.y);
   gPrefs->Write(wxT("/TagsEditor/width"), r.width);
   gPrefs->Write(wxT("/TagsEditor/height"), r.height);

   EndModal(wxID_OK);
}

void TagsEditor::OnCancel(wxCommandEvent & event)
{
   if (mGrid->IsCellEditControlShown()) {
      mGrid->GetCellEditor(mGrid->GetGridCursorRow(),
                           mGrid->GetGridCursorCol())
                           ->Reset();
      mGrid->HideCellEditControl();
      return;
   }

   EndModal(wxID_CANCEL);
}

void TagsEditor::SetEditors()
{
   int cnt = mGrid->GetNumberRows();

   for (int i = 0; i < cnt; i++) {
      wxString label = mGrid->GetCellValue(i, 0);
      if (label.CmpNoCase(LABEL_GENRE) == 0) {
         mGrid->SetCellEditor(i, 1, mGrid->GetDefaultEditorForType(wxT("Combo")));
      }
      else {
         mGrid->SetCellEditor(i, 1, NULL); //mGrid->GetDefaultEditor());
      }
   }
}

void TagsEditor::PopulateGenres()
{
   int cnt = mLocal.GetNumGenres();
   int i;
   wxString parm;
   wxSortedArrayString g;

   for (i = 0; i < cnt; i++) {
      g.Add(mLocal.GetGenre(i));
   }

   for (i = 0; i < cnt; i++) {
      parm = parm + (i == 0 ? wxT("") : wxT(",")) + g[i];
   }

   mGrid->GetDefaultEditorForType(wxT("Combo"))->SetParameters(parm);
}

BEGIN_EVENT_TABLE(TagsEditor1, wxDialog)
   EVT_BUTTON(EditID, TagsEditor1::OnEdit)
   EVT_BUTTON(ClearID, TagsEditor1::OnClear)
   EVT_BUTTON(LoadID, TagsEditor1::OnLoad)
   EVT_BUTTON(SaveID, TagsEditor1::OnSave)
   EVT_BUTTON(SaveDefaultsID, TagsEditor1::OnSaveDefaults)
   EVT_BUTTON(AddID, TagsEditor1::OnAdd)
   EVT_BUTTON(RemoveID, TagsEditor1::OnRemove)
   EVT_BUTTON(wxID_CANCEL, TagsEditor1::OnCancel)
   EVT_BUTTON(wxID_OK, TagsEditor1::OnOk)
END_EVENT_TABLE()

TagsEditor1::TagsEditor1(wxWindow * parent,
                       wxString title,
                       Tags * tags,
                       bool editTitle,
                       bool editTrack)
:  wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
   mTags(tags),
   mEditTitle(editTitle),
   mEditTrack(editTrack)
{
   // Make a local copy of the passed in tags
   mLocal = *mTags;

   // Make local backup copies of metadata
   mTitle    = mLocal.GetTag(TAG_TITLE);
   mArtist   = mLocal.GetTag(TAG_ARTIST);
   mAlbum    = mLocal.GetTag(TAG_ALBUM);
   mTrackNum = mLocal.GetTag(TAG_TRACK);
   mYear     = mLocal.GetTag(TAG_YEAR);
   mGenre    = mLocal.GetTag(TAG_GENRE);
   mComments = mLocal.GetTag(TAG_COMMENTS);
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      mPerformer  = mLocal.GetTag(TAG_PERFORMER);
      mDate       = mLocal.GetTag(TAG_DATE);
      mISRC       = mLocal.GetTag(TAG_ISRC);
   #endif
   mID3V2    = mLocal.GetID3V2();

   Rebuild();
}

TagsEditor1::~TagsEditor1()
{
}

void TagsEditor1::Rebuild()
{
   SetSizer(NULL);
   DestroyChildren();
   mClear = NULL;

   mTransfering = true; // avoid endless update loop

   // Build, size, and position the dialog
   SetSizeHints(0, 0);
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   PopulateGenres();
   GetSizer()->AddSpacer(5);
   Layout();
   Fit();
   Center();
   SetSizeHints(GetSize());

   // Populate all the controls
   TransferDataToWindow();

   // Done building
   mTransfering = false;
}

void TagsEditor1::PopulateOrExchange(ShuttleGui & S)
{
   S.StartVerticalLay();
   {
      S.SetStyle(wxNO_BORDER);
      S.StartScroller(1);
      {
         S.StartMultiColumn(2, wxALIGN_LEFT | wxEXPAND);
         {
            S.SetBorder(3);
            S.SetStretchyCol(1);
            S.TieTextBox(_("Title:"), mTitle, 20)->Enable(mEditTitle);
            S.TieTextBox(_("Artist:"), mArtist, 20);
            S.TieTextBox(_("Album:"), mAlbum, 20);
            S.TieTextBox(_("Composer:"), mComposer, 20);
            S.TieTextBox(_("Lyricist:"), mLyricist, 20);
            S.TieTextBox(_("Comments:"), mComments, 20);

            S.AddPrompt(_("Encoded By:"));
            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieTextBox(wxT(""), mCopyright, 20);
               S.TieTextBox(_("Copyright:"), mEncodedBy, 20);
            }
            S.EndHorizontalLay();

            S.AddPrompt(_("Track Number:"));
            S.StartHorizontalLay(wxALIGN_LEFT);
            {
               S.TieTextBox(wxT(""), mTrackNum, 0)->Enable(mEditTrack);
               S.TieTextBox(_("Year:"), mYear, 0);
            }
            S.EndHorizontalLay();

            S.AddPrompt(_("Genre:"));
            S.StartMultiColumn(2, wxALIGN_LEFT | wxEXPAND);
            {
               S.SetStretchyCol(0);
               wxArrayString dummy;
               S.SetStyle(wxCB_SORT);
               mGenreCombo = S.AddCombo(wxT(""), mGenre, &dummy);
               S.Id(EditID).AddButton(_("Edit"));
            }
            S.EndMultiColumn();

            for (int i = 0; i < (int)mCustomTags.GetCount(); i++) {
               S.TieTextBox(mCustomTags[i], mCustomTags[i], 0);
               S.TieTextBox(mCustomVals[i], mCustomVals[i], 0);
            }
            S.SetBorder(5);
         }
         S.EndMultiColumn();
      }
      S.EndScroller();

      S.StartMultiColumn(2, wxALIGN_CENTER);
      {
         S.Id(AddID).AddButton(_("&Add"));
         S.Id(RemoveID).AddButton(_("&Remove"));
      }
      S.EndMultiColumn();

      S.StartStatic(_("Template"));
      {
         S.StartMultiColumn(4, wxALIGN_CENTER);
         {
            S.Id(LoadID).AddButton(_("&Load..."));
            S.Id(SaveID).AddButton(_("&Save..."));
            S.AddTitle(wxT(" "));
            S.Id(SaveDefaultsID).AddButton(_("S&et Default"));
         }
         S.EndMultiColumn();
      }
      S.EndStatic();
   }
   S.EndVerticalLay();

   if (mClear == NULL) {
      mClear = new wxButton(this, ClearID, _("&Clear"));
   }

   S.AddStandardButtons(eOkButton | eCancelButton, mClear);
}

void TagsEditor1::PopulateGenres()
{
   mGenreCombo->Clear();
   int cnt = mLocal.GetNumGenres();

   // This fixes a problem on the Mac where the popup menu actually pops up,
   // but you can't see it.
   if (cnt == 0) {
      mGenreCombo->Append(_("No Genres"));
      return;
   }

   for (int i = 0; i < cnt; i++) {
      mGenreCombo->Append(mLocal.GetGenre(i));
   }
}

bool TagsEditor1::TransferDataFromWindow()
{
   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   // These aren't handled by ShuttleGui
   mGenre = mGenreCombo->GetValue();

   // Store
   mLocal.SetTag(TAG_TITLE, mTitle);
   mLocal.SetTag(TAG_ARTIST, mArtist);
   mLocal.SetTag(TAG_ALBUM, mAlbum);
   mLocal.SetTag(TAG_TRACK, mTrackNum);
   mLocal.SetTag(TAG_YEAR, mYear);
   mLocal.SetTag(TAG_GENRE, mGenre);
   mLocal.SetTag(TAG_COMMENTS, mComments);
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      mLocal.SetTag(TAG_PERFORMER, mPerformer);
      mLocal.SetTag(TAG_DATE, mDate);
      mLocal.SetTag(TAG_ISRC, mISRC);
   #endif
   mLocal.SetID3V2(mID3V2);
#if 0
   mLocal.Clear();
   for (i = 0; i < cnt; i++) {
      wxString n = mGrid->GetCellValue(i, 0);
      wxString v = mGrid->GetCellValue(i, 1);

      if (n.IsEmpty()) {
         continue;
      }

      if (n.CmpNoCase(LABEL_ARTIST) == 0) {
         n = TAG_ARTIST;
      }
      else if (n.CmpNoCase(LABEL_TITLE) == 0) {
         n = TAG_TITLE;
      }
      else if (n.CmpNoCase(LABEL_ALBUM) == 0) {
         n = TAG_ALBUM;
      }
      else if (n.CmpNoCase(LABEL_TRACK) == 0) {
         n = TAG_TRACK;
      }
      else if (n.CmpNoCase(LABEL_YEAR) == 0) {
         n = TAG_YEAR;
      }
      else if (n.CmpNoCase(LABEL_GENRE) == 0) {
         n = TAG_GENRE;
      }
      else if (n.CmpNoCase(LABEL_COMMENTS) == 0) {
         n = TAG_COMMENTS;
      }
      #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
         else if (n.CmpNoCase(LABEL_PERFORMER) == 0) {
            n = TAG_PERFORMER;
         }
         else if (n.CmpNoCase(LABEL_DATE) == 0) {
            n = TAG_DATE;
         }
         else if (n.CmpNoCase(LABEL_ISRC) == 0) {
            n = TAG_ISRC;
         }
      #endif

      mLocal.SetTag(n, v);
   }
#endif
   return true;
}

bool TagsEditor1::TransferDataToWindow()
{
   mTitle    = mLocal.GetTag(TAG_TITLE);
   mArtist   = mLocal.GetTag(TAG_ARTIST);
   mAlbum    = mLocal.GetTag(TAG_ALBUM);
   mTrackNum = mLocal.GetTag(TAG_TRACK);
   mYear     = mLocal.GetTag(TAG_YEAR);
   mGenre    = mLocal.GetTag(TAG_GENRE);
   mComments = mLocal.GetTag(TAG_COMMENTS);
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      mPerformer  = mLocal.GetTag(TAG_PERFORMER);
      mDate       = mLocal.GetTag(TAG_DATE);
      mISRC       = mLocal.GetTag(TAG_ISRC);
   #endif
   mID3V2    = mLocal.GetID3V2();

   ShuttleGui S(this, eIsSettingToDialog);
   PopulateOrExchange(S);

   // These aren't handled by ShuttleGui
   mGenreCombo->SetValue(mGenre);

#if 0
   int i = 0;
   wxString n, v;

   // Disable redrawing until we're done
   mGrid->BeginBatch();

   // Delete all rows
   if (mGrid->GetNumberRows()) {
      mGrid->DeleteRows(0, mGrid->GetNumberRows());
   }

   // Populate the rows
   for (bool cont = mLocal.GetFirst(n, v); cont; cont = mLocal.GetNext(n, v)) {
      mGrid->AppendRows(1);

      if (n.CmpNoCase(TAG_ARTIST) == 0) {
         n = LABEL_ARTIST;
      }
      else if (n.CmpNoCase(TAG_TITLE) == 0) {
         n = LABEL_TITLE;
      }
      else if (n.CmpNoCase(TAG_ALBUM) == 0) {
         n = LABEL_ALBUM;
      }
      else if (n.CmpNoCase(TAG_TRACK) == 0) {
         n = LABEL_TRACK;
      }
      else if (n.CmpNoCase(TAG_YEAR) == 0) {
         n = LABEL_YEAR;
      }
      else if (n.CmpNoCase(TAG_GENRE) == 0) {
         n = LABEL_GENRE;

         wxSortedArrayString g;
         int cnt = mLocal.GetNumGenres();
         for (int i = 0; i < cnt; i++) {
            g.Add(mLocal.GetGenre(i));
         }

         mGrid->SetCellEditor(mGrid->GetNumberRows() - 1, 1, new ComboEditor(g, true));
      }
      else if (n.CmpNoCase(TAG_COMMENTS) == 0) {
         n = LABEL_COMMENTS;
      }
      #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
         else if (n.CmpNoCase(TAG_PERFORMER) == 0) {
            n = LABEL_PERFORMER;
         }
         else if (n.CmpNoCase(TAG_DATE) == 0) {
            n = LABEL_DATE;
         }
         else if (n.CmpNoCase(TAG_ISRC) == 0) {
            n = LABEL_ISRC;
         }
      #endif

      mGrid->SetCellValue(i, 0, n);
      mGrid->SetCellValue(i, 1, v);
      i++;
   }

   // We're done, so allow the grid to redraw
   mGrid->EndBatch();
#endif
   return true;
}

void TagsEditor1::OnEdit(wxCommandEvent & event)
{
   wxDialog dlg(this, wxID_ANY, _("Edit Genres"),
                wxDefaultPosition, wxDefaultSize,
                wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);

   ShuttleGui S(&dlg, eIsCreating);

   S.StartVerticalLay(true);
   {
      mEdit = S.AddTextWindow(wxT(""));
   }
   S.EndVerticalLay();

   S.AddStandardButtons(eOkButton | eCancelButton,
                        new wxButton(&dlg, ResetID, _("Reset to Defaults")));
   dlg.Connect(ResetID,
               wxEVT_COMMAND_BUTTON_CLICKED,
               wxCommandEventHandler(TagsEditor1::OnReset),
               NULL,
               this);

   wxSortedArrayString g;
   int cnt = mLocal.GetNumGenres();
   for (int i = 0; i < cnt; i++) {
      g.Add(mLocal.GetGenre(i));
   }

   for (int i = 0; i < cnt; i++) {
      mEdit->AppendText(g[i] + wxT("\n"));
   }

   dlg.Center();
   if (dlg.ShowModal() == wxID_CANCEL) {
      return;
   }

   wxFileName fn(FileNames::DataDir(), wxT("genres.txt"));
   wxFile f(fn.GetFullPath(), wxFile::write);
   if (!f.IsOpened() || !f.Write(mEdit->GetValue())) {
      wxMessageBox(_("Unable to save genre file."), _("Reset Genres"));
      return;
   }

   mLocal.LoadGenres();

   PopulateGenres();
}

void TagsEditor1::OnReset(wxCommandEvent & event)
{
   int id = wxMessageBox(_("Are you sure you want to reset the genre list to defaults?"),
                         _("Reset Genres"),
                         wxYES_NO);

   if (id == wxNO) {
      return;
   }
   mLocal.LoadDefaultGenres();

   wxFileName fn(FileNames::DataDir(), wxT("genres.txt"));
   wxTextFile tf(fn.GetFullPath());

   bool open = (tf.Exists() && tf.Open()) ||
               (!tf.Exists() && tf.Create());

   if (!open) {
      wxMessageBox(_("Unable to open genre file."), _("Reset Genres"));
      mLocal.LoadGenres();
      return;
   }

   tf.Clear();
   int cnt = mLocal.GetNumGenres();
   for (int i = 0; i < cnt; i++) {
      tf.AddLine(mLocal.GetGenre(i));
   }

   tf.Write();
   if (!tf.Write()) {
      wxMessageBox(_("Unable to save genre file."), _("Reset Genres"));
      mLocal.LoadGenres();
      return;
   }

   mLocal.LoadGenres();

   PopulateGenres();

   wxSortedArrayString g;
   cnt = mLocal.GetNumGenres();
   for (int i = 0; i < cnt; i++) {
      g.Add(mLocal.GetGenre(i));
   }

   mEdit->SetValue(wxT(""));
   for (int i = 0; i < cnt; i++) {
      mEdit->AppendText(g[i] + wxT("\n"));
   }
}

void TagsEditor1::OnClear(wxCommandEvent & event)
{
   mLocal.Clear();

   TransferDataToWindow();
}

void TagsEditor1::OnLoad(wxCommandEvent & event)
{
   wxString fn;

   // Ask the user for the real name
   fn = FileSelector(_("Save Metadata As:"),
                     FileNames::DataDir(),
                     wxT("Tags.xml"),
                     wxT("xml"),
                     wxT("*.xml"),
                     wxOPEN,
                     this);

   // User canceled...
   if (fn.IsEmpty()) {
      return;
   }

   // Clear current contents
   mLocal.Clear();

   // Load the metadata
   XMLFileReader reader;
   if (!reader.Parse(&mLocal, fn)) {
      // Inform user of load failure
      wxMessageBox(reader.GetErrorStr(),
                   _("Error loading metadata"),
                   wxOK | wxCENTRE,
                   this);
   }

   TransferDataToWindow();

   return;
}

void TagsEditor1::OnSave(wxCommandEvent & event)
{
   wxString fn;

   // Ask the user for the real name
   fn = FileSelector(_("Save Metadata As:"),
                     FileNames::DataDir(),
                     wxT("Tags.xml"),
                     wxT("xml"),
                     wxT("*.xml"),
                     wxSAVE | wxOVERWRITE_PROMPT,
                     this);

   // User canceled...
   if (fn.IsEmpty()) {
      return;
   }

   // Create/Open the file
   XMLFileWriter writer;
   writer.Open(fn, wxT("wb"));

   // Complain if open failed
   if (!writer.IsOpened()) {
      // Constructor will emit message
      return;
   }

   // Write the metadata
   mLocal.WriteXML(writer);

   // Close the file
   writer.Close();

   return;
}

void TagsEditor1::OnSaveDefaults(wxCommandEvent & event)
{
   TransferDataFromWindow();

   gPrefs->DeleteGroup(wxT("/Tags"));
   gPrefs->Write(wxT("/Tags/ID3V2"), mLocal.GetID3V2());

   wxString n, v;
   for (bool cont = mLocal.GetFirst(n, v); cont; cont = mLocal.GetNext(n, v)) {
      gPrefs->Write(wxT("/Tags/") + n, v);
   }
}

void TagsEditor1::OnAdd(wxCommandEvent & event)
{
   mCustomTags.Add(wxT(""));
   mCustomVals.Add(wxT(""));
   Rebuild();
}

void TagsEditor1::OnRemove(wxCommandEvent & event)
{
   int cnt = mCustomTags.GetCount();

   if (cnt > 0) {
      mCustomTags.RemoveAt(cnt - 1);
      mCustomVals.RemoveAt(cnt - 1);
   }

   Rebuild();
}

void TagsEditor1::OnOk(wxCommandEvent & event)
{
   if (!Validate() || !TransferDataFromWindow()) {
      return;
   }

   *mTags = mLocal;

   wxRect r = GetRect();
   gPrefs->Write(wxT("/TagsEditor1/x"), r.x);
   gPrefs->Write(wxT("/TagsEditor1/y"), r.y);
   gPrefs->Write(wxT("/TagsEditor1/width"), r.width);
   gPrefs->Write(wxT("/TagsEditor1/height"), r.height);

   EndModal(wxID_OK);
}

void TagsEditor1::OnCancel(wxCommandEvent & event)
{
   EndModal(wxID_CANCEL);
}

BEGIN_EVENT_TABLE(TagsEditor2, wxDialog)
   EVT_NOTEBOOK_PAGE_CHANGING(wxID_ANY, TagsEditor2::OnPage)
   EVT_BUTTON(EditID, TagsEditor2::OnEdit)
   EVT_BUTTON(ResetID, TagsEditor2::OnReset)
   EVT_BUTTON(ClearID, TagsEditor2::OnClear)
   EVT_BUTTON(LoadID, TagsEditor2::OnLoad)
   EVT_BUTTON(SaveID, TagsEditor2::OnSave)
   EVT_BUTTON(SaveDefaultsID, TagsEditor2::OnSaveDefaults)
   EVT_BUTTON(AddID, TagsEditor2::OnAdd)
   EVT_BUTTON(RemoveID, TagsEditor2::OnRemove)
   EVT_BUTTON(wxID_CANCEL, TagsEditor2::OnCancel)
   EVT_BUTTON(wxID_OK, TagsEditor2::OnOk)
END_EVENT_TABLE()

TagsEditor2::TagsEditor2(wxWindow * parent,
                       wxString title,
                       Tags * tags,
                       bool editTitle,
                       bool editTrack)
:  wxDialog(parent, wxID_ANY, title, wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER),
   mTags(tags),
   mEditTitle(editTitle),
   mEditTrack(editTrack)
{
   mTransfering = true; // avoid endless update loop
   mGrid = NULL;
   mBasic = NULL;
   mAdvanced = NULL;

   // Make a local copy of the passed in tags
   mLocal = *mTags;

   // Make local backup copies of metadata
   mTitle    = mLocal.GetTag(TAG_TITLE);
   mArtist   = mLocal.GetTag(TAG_ARTIST);
   mAlbum    = mLocal.GetTag(TAG_ALBUM);
   mTrackNum = mLocal.GetTag(TAG_TRACK);
   mYear     = mLocal.GetTag(TAG_YEAR);
   mGenre    = mLocal.GetTag(TAG_GENRE);
   mComments = mLocal.GetTag(TAG_COMMENTS);
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      mPerformer  = mLocal.GetTag(TAG_PERFORMER);
      mDate       = mLocal.GetTag(TAG_DATE);
      mISRC       = mLocal.GetTag(TAG_ISRC);
   #endif
   mID3V2    = mLocal.GetID3V2();

   // Build, size, and position the dialog
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
   PopulateGenres();
   GetSizer()->AddSpacer(5);
   Layout();
   Fit();
   Center();
   SetSizeHints(GetSize());

   // Override size and position with last saved
   wxRect r = GetRect();
   gPrefs->Read(wxT("/TagsEditor2/x"), &r.x, r.x);
   gPrefs->Read(wxT("/TagsEditor2/y"), &r.y, r.y);
   gPrefs->Read(wxT("/TagsEditor2/width"), &r.width, r.width);
   gPrefs->Read(wxT("/TagsEditor2/height"), &r.height, r.height);
   Move(r.GetPosition());
   SetSize(r.GetSize());

   // Show the user the last page viewed
   mNotebook->SetSelection(gPrefs->Read(wxT("/TagsEditor2/page"), 0l));

   // Resize value column width based on width of columns and the vertical scrollbar
   wxScrollBar sb(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxSB_VERTICAL);
   r = mGrid->GetClientRect();
   r.width -= mGrid->GetColSize(0);
   r.width -= sb.GetSize().GetWidth();
   r.width -= 10;
   r.width -= r.x;
   mGrid->SetColSize(1, r.width);

   // Populate all the controls
   TransferDataToWindow();

   // Done building
   mTransfering = false;
}

TagsEditor2::~TagsEditor2()
{
}

void TagsEditor2::PopulateOrExchange(ShuttleGui & S)
{
   mNotebook = S.StartNotebook();
   {
      /***/

      if (mBasic == NULL) {
         mBasic = S.StartNotebookPage(_("Basic"));
         mBasic->GetSizer()->SetMinSize(wxDefaultSize);
      }

      // "Basic" notebook page
      {
         S.StartVerticalLay();
         {
            S.StartMultiColumn(2, wxALIGN_LEFT | wxEXPAND);
            {
               S.SetBorder(3);
               S.SetStretchyCol(1);
               S.TieTextBox(_("Title:"), mTitle, 32)->Enable(mEditTitle);
               S.TieTextBox(_("Artist:"), mArtist, 32);
               S.TieTextBox(_("Album:"), mAlbum, 32);
               S.TieTextBox(_("Track Number:"), mTrackNum, 4)->Enable(mEditTrack);
               S.TieTextBox(_("Year:"), mYear, 4);
               S.TieTextBox(_("Comments:"), mComments, 32);
               S.AddPrompt(_("Genre:"));
               S.StartHorizontalLay();
               {
                  wxArrayString dummy;
                  S.SetStyle(wxCB_SORT);
                  mGenreCombo = S.AddCombo(wxT(""), mGenre, &dummy);
                  S.Id(EditID).AddButton(_("Edit"));
                  S.Id(ResetID).AddButton(_("Reset"));
               }
               S.EndHorizontalLay();
               S.SetBorder(5);
            }
            S.EndMultiColumn();

            S.StartStatic(_("Template"));
            {
               S.StartMultiColumn(4, wxALIGN_CENTER);
               {
                  S.Id(LoadID).AddButton(_("&Load..."));
                  S.Id(SaveID).AddButton(_("&Save..."));
                  S.AddTitle(wxT(" "));
                  S.Id(SaveDefaultsID).AddButton(_("S&et Default"));
               }
               S.EndMultiColumn();
            }
            S.EndStatic();
         }
         S.EndVerticalLay();
      }
      S.EndNotebookPage();

      /***/

      if (mAdvanced == NULL) {
         mAdvanced = S.StartNotebookPage(_("Advanced"));
         mAdvanced->GetSizer()->SetMinSize(wxDefaultSize);
      }

      // "Advanced" notebook page
      {
         S.StartVerticalLay();
         {
            S.StartHorizontalLay(wxALIGN_LEFT, false);
            {
               S.AddUnits(_("Press F2 or double click to edit."));
            }
            S.EndHorizontalLay();

            if (mGrid == NULL) {
               mGrid = new Grid(S.GetParent(), wxID_ANY);

               wxArrayString cs(WXSIZEOF(names), names);
               mCombo = new ComboEditor(cs, true);

               // Build the initial (empty) grid
               mGrid->CreateGrid(0, 2);
               mGrid->SetRowLabelSize(0);
               mGrid->SetDefaultCellAlignment(wxALIGN_LEFT, wxALIGN_CENTER);
               mGrid->SetColLabelValue(0, _("Name"));
               mGrid->SetColLabelValue(1, _("Value"));

               wxGridCellAttr *attr = new wxGridCellAttr();
               attr->SetEditor(mCombo);
               attr->SetBackgroundColour(*wxWHITE);
               mGrid->SetColAttr(0, attr);

               attr = new wxGridCellAttr();
               attr->SetBackgroundColour(*wxWHITE);
               mGrid->SetColAttr(1, attr);

               // Resize the name column and set default row height.
               wxComboBox tc(this, wxID_ANY, wxT(""), wxDefaultPosition, wxDefaultSize, cs);
               mGrid->SetColSize(0, tc.GetSize().x);
               mGrid->SetColMinimalWidth(0, tc.GetSize().x);
               mGrid->SetDefaultRowSize(tc.GetSize().y);
            }
            S.Prop(true);
            S.AddWindow(mGrid, wxEXPAND);

            S.StartMultiColumn(2, wxALIGN_CENTER);
            {
               S.Id(AddID).AddButton(_("&Add"));
               S.Id(RemoveID).AddButton(_("&Remove"));
            }
            S.EndMultiColumn();
         }
         S.EndVerticalLay();
      }
      S.EndNotebookPage();
   }
   S.EndNotebook();

   S.StartMultiColumn(3, wxALIGN_LEFT | wxEXPAND);
   {
      S.SetStretchyCol(0);
      S.Id(ClearID).AddButton(_("Clea&r"), wxALIGN_LEFT);
#if defined(__WXGTK20__) || defined(__WXMAC__)
      S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
      S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
#else
      S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
      S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
#endif
   }
   S.EndMultiColumn();
}

void TagsEditor2::PopulateGenres()
{
   mGenreCombo->Clear();
   int cnt = mLocal.GetNumGenres();

   // This fixes a problem on the Mac where the popup menu actually pops up,
   // but you can't see it.
   if (cnt == 0) {
      mGenreCombo->Append(_("No Genres"));
      return;
   }

   for (int i = 0; i < cnt; i++) {
      mGenreCombo->Append(mLocal.GetGenre(i));
   }
}

bool TagsEditor2::Validate()
{
#if 0
   wxString errorString =
      _("Maximum length of attribute '%s' is %d characters. Data was truncated.");

   if(!mLocal.mID3V2)
   {
      if(mLocal.mTitle.Length() > 30)
      {
         wxMessageBox(wxString::Format(errorString, _("Title"), 30));

         mLocal.mTitle = mLocal.mTitle.Left(30);
         TransferDataToWindow();

         return FALSE;
      }

      if(mLocal.mArtist.Length() > 30)
      {
         wxMessageBox(wxString::Format(errorString, _("Artist"), 30));

         mLocal.mArtist = mLocal.mArtist.Left(30);
         TransferDataToWindow();

         return FALSE;
      }

      if(mLocal.mAlbum.Length() > 30)
      {
         wxMessageBox(wxString::Format(errorString, _("Album"), 30));

         mLocal.mAlbum = mLocal.mAlbum.Left(30);
         TransferDataToWindow();

         return FALSE;
      }

      if(mLocal.mYear.Length() > 4)
      {
         wxMessageBox(wxString::Format(errorString, _("Year"), 4));

         mLocal.mYear = mLocal.mYear.Left(4);
         TransferDataToWindow();

         return FALSE;
      }

      if(mLocal.mComments.Length() > 30)
      {
         wxMessageBox(wxString::Format(errorString, _("Comments"), 30));

         mLocal.mComments = mLocal.mComments.Left(30);
         TransferDataToWindow();

         return FALSE;
      }
   }
#endif
   return true;
}

bool TagsEditor2::TransferDataFromWindow()
{
   switch (mNotebook->GetSelection())
   {
      case 0:
         return TransferDataFromBasic();
      break;

      case 1:
         return TransferDataFromAdvanced();
      break;
   }

   return true;
}

bool TagsEditor2::TransferDataToWindow()
{
   switch (mNotebook->GetSelection())
   {
      case 0:
         return TransferDataToBasic();
      break;

      case 1:
         return TransferDataToAdvanced();
      break;
   }

   return true;
}

bool TagsEditor2::TransferDataFromBasic()
{
   ShuttleGui S(this, eIsGettingFromDialog);
   PopulateOrExchange(S);

   // These aren't handled by ShuttleGui
   mGenre = mGenreCombo->GetValue();

   // Store
   mLocal.SetTag(TAG_TITLE, mTitle);
   mLocal.SetTag(TAG_ARTIST, mArtist);
   mLocal.SetTag(TAG_ALBUM, mAlbum);
   mLocal.SetTag(TAG_TRACK, mTrackNum);
   mLocal.SetTag(TAG_YEAR, mYear);
   mLocal.SetTag(TAG_GENRE, mGenre);
   mLocal.SetTag(TAG_COMMENTS, mComments);
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      mLocal.SetTag(TAG_PERFORMER, mPerformer);
      mLocal.SetTag(TAG_DATE, mDate);
      mLocal.SetTag(TAG_ISRC, mISRC);
   #endif
   mLocal.SetID3V2(mID3V2);

   return true;
}

bool TagsEditor2::TransferDataFromAdvanced()
{
   int i, cnt = mGrid->GetNumberRows();

   mLocal.Clear();
   for (i = 0; i < cnt; i++) {
      wxString n = mGrid->GetCellValue(i, 0);
      wxString v = mGrid->GetCellValue(i, 1);

      if (n.IsEmpty()) {
         continue;
      }

      if (n.CmpNoCase(LABEL_ARTIST) == 0) {
         n = TAG_ARTIST;
      }
      else if (n.CmpNoCase(LABEL_TITLE) == 0) {
         n = TAG_TITLE;
      }
      else if (n.CmpNoCase(LABEL_ALBUM) == 0) {
         n = TAG_ALBUM;
      }
      else if (n.CmpNoCase(LABEL_TRACK) == 0) {
         n = TAG_TRACK;
      }
      else if (n.CmpNoCase(LABEL_YEAR) == 0) {
         n = TAG_YEAR;
      }
      else if (n.CmpNoCase(LABEL_GENRE) == 0) {
         n = TAG_GENRE;
      }
      else if (n.CmpNoCase(LABEL_COMMENTS) == 0) {
         n = TAG_COMMENTS;
      }
      #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
         else if (n.CmpNoCase(LABEL_PERFORMER) == 0) {
            n = TAG_PERFORMER;
         }
         else if (n.CmpNoCase(LABEL_DATE) == 0) {
            n = TAG_DATE;
         }
         else if (n.CmpNoCase(LABEL_ISRC) == 0) {
            n = TAG_ISRC;
         }
      #endif

      mLocal.SetTag(n, v);
   }

   return true;
}

bool TagsEditor2::TransferDataToBasic()
{
   mTitle    = mLocal.GetTag(TAG_TITLE);
   mArtist   = mLocal.GetTag(TAG_ARTIST);
   mAlbum    = mLocal.GetTag(TAG_ALBUM);
   mTrackNum = mLocal.GetTag(TAG_TRACK);
   mYear     = mLocal.GetTag(TAG_YEAR);
   mGenre    = mLocal.GetTag(TAG_GENRE);
   mComments = mLocal.GetTag(TAG_COMMENTS);
   #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
      mPerformer  = mLocal.GetTag(TAG_PERFORMER);
      mDate       = mLocal.GetTag(TAG_DATE);
      mISRC       = mLocal.GetTag(TAG_ISRC);
   #endif
   mID3V2    = mLocal.GetID3V2();

   ShuttleGui S(this, eIsSettingToDialog);
   PopulateOrExchange(S);

   // These aren't handled by ShuttleGui
   mGenreCombo->SetValue(mGenre);

   return true;
}

bool TagsEditor2::TransferDataToAdvanced()
{
   int i = 0;
   wxString n, v;

   // Disable redrawing until we're done
   mGrid->BeginBatch();

   // Delete all rows
   if (mGrid->GetNumberRows()) {
      mGrid->DeleteRows(0, mGrid->GetNumberRows());
   }

   // Populate the rows
   for (bool cont = mLocal.GetFirst(n, v); cont; cont = mLocal.GetNext(n, v)) {
      mGrid->AppendRows(1);

      if (n.CmpNoCase(TAG_ARTIST) == 0) {
         n = LABEL_ARTIST;
      }
      else if (n.CmpNoCase(TAG_TITLE) == 0) {
         n = LABEL_TITLE;
      }
      else if (n.CmpNoCase(TAG_ALBUM) == 0) {
         n = LABEL_ALBUM;
      }
      else if (n.CmpNoCase(TAG_TRACK) == 0) {
         n = LABEL_TRACK;
      }
      else if (n.CmpNoCase(TAG_YEAR) == 0) {
         n = LABEL_YEAR;
      }
      else if (n.CmpNoCase(TAG_GENRE) == 0) {
         n = LABEL_GENRE;

         wxSortedArrayString g;
         int cnt = mLocal.GetNumGenres();
         for (int i = 0; i < cnt; i++) {
            g.Add(mLocal.GetGenre(i));
         }

         mGrid->SetCellEditor(mGrid->GetNumberRows() - 1, 1, new ComboEditor(g, true));
      }
      else if (n.CmpNoCase(TAG_COMMENTS) == 0) {
         n = LABEL_COMMENTS;
      }
      #if ((AUDACITY_BRANDING == BRAND_JAMLING__EASY) || (AUDACITY_BRANDING == BRAND_JAMLING__FULL))
         else if (n.CmpNoCase(TAG_PERFORMER) == 0) {
            n = LABEL_PERFORMER;
         }
         else if (n.CmpNoCase(TAG_DATE) == 0) {
            n = LABEL_DATE;
         }
         else if (n.CmpNoCase(TAG_ISRC) == 0) {
            n = LABEL_ISRC;
         }
      #endif

      mGrid->SetCellValue(i, 0, n);
      mGrid->SetCellValue(i, 1, v);
      i++;
   }

   // We're done, so allow the grid to redraw
   mGrid->EndBatch();

   return true;
}

void TagsEditor2::OnPage(wxNotebookEvent & event)
{
   if (mTransfering) { // avoid endless update loop
      return;
   }

   if (mGrid->IsCellEditControlShown()) {
      mGrid->HideCellEditControl();
      mGrid->SaveEditControlValue();
   }

   switch (event.GetOldSelection())
   {
      case 0:
         TransferDataFromBasic();
         TransferDataToAdvanced();
      break;

      case 1:
         TransferDataFromAdvanced();
         TransferDataToBasic();
      break;
   }
}

void TagsEditor2::OnEdit(wxCommandEvent & event)
{
   wxDialog dlg(this, wxID_ANY, _("Edit Genres"),
                wxDefaultPosition, wxDefaultSize,
                wxDEFAULT_DIALOG_STYLE | wxRESIZE_BORDER);
   wxTextCtrl *tc;

   ShuttleGui S(&dlg, eIsCreating);

   S.StartVerticalLay(true);
   {
      tc = S.AddTextWindow(wxT(""));
   }
   S.EndVerticalLay();

   S.StartHorizontalLay(wxALIGN_RIGHT, false);
   {
#if defined(__WXGTK20__) || defined(__WXMAC__)
      S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
      S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
#else
      S.Id(wxID_OK).AddButton(_("&OK"))->SetDefault();
      S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
#endif
   }
   S.EndHorizontalLay();

   wxSortedArrayString g;
   int cnt = mLocal.GetNumGenres();
   for (int i = 0; i < cnt; i++) {
      g.Add(mLocal.GetGenre(i));
   }

   for (int i = 0; i < cnt; i++) {
      tc->AppendText(g[i] + wxT("\n"));
   }

   dlg.Center();
   if (dlg.ShowModal() == wxID_CANCEL) {
      return;
   }

   wxFileName fn(FileNames::DataDir(), wxT("genres.txt"));
   wxFile f(fn.GetFullPath(), wxFile::write);
   if (!f.IsOpened() || !f.Write(tc->GetValue())) {
      wxMessageBox(_("Unable to save genre file."), _("Reset Genres"));
      return;
   }

   mLocal.LoadGenres();

   PopulateGenres();
}

void TagsEditor2::OnReset(wxCommandEvent & event)
{
   int id = wxMessageBox(_("Are you sure you want to reset the genre list to defaults?"),
                         _("Reset Genres"),
                         wxYES_NO);

   if (id == wxNO) {
      return;
   }
   mLocal.LoadDefaultGenres();

   wxFileName fn(FileNames::DataDir(), wxT("genres.txt"));
   wxTextFile tf(fn.GetFullPath());

   bool open = (tf.Exists() && tf.Open()) ||
               (!tf.Exists() && tf.Create());

   if (!open) {
      wxMessageBox(_("Unable to open genre file."), _("Reset Genres"));
      mLocal.LoadGenres();
      return;
   }

   tf.Clear();
   int cnt = mLocal.GetNumGenres();
   for (int i = 0; i < cnt; i++) {
      tf.AddLine(mLocal.GetGenre(i));
   }

   tf.Write();
   if (!tf.Write()) {
      wxMessageBox(_("Unable to save genre file."), _("Reset Genres"));
      mLocal.LoadGenres();
      return;
   }

   mLocal.LoadGenres();

   PopulateGenres();
}

void TagsEditor2::OnClear(wxCommandEvent & event)
{
   mLocal.Clear();

   TransferDataToWindow();
}

void TagsEditor2::OnLoad(wxCommandEvent & event)
{
   wxString fn;

   // Ask the user for the real name
   fn = FileSelector(_("Save Metadata As:"),
                     FileNames::DataDir(),
                     wxT("Tags.xml"),
                     wxT("xml"),
                     wxT("*.xml"),
                     wxOPEN,
                     this);

   // User canceled...
   if (fn.IsEmpty()) {
      return;
   }

   // Clear current contents
   mLocal.Clear();

   // Load the metadata
   XMLFileReader reader;
   if (!reader.Parse(&mLocal, fn)) {
      // Inform user of load failure
      wxMessageBox(reader.GetErrorStr(),
                   _("Error loading metadata"),
                   wxOK | wxCENTRE,
                   this);
   }

   TransferDataToWindow();

   return;
}

void TagsEditor2::OnSave(wxCommandEvent & event)
{
   wxString fn;

   // Ask the user for the real name
   fn = FileSelector(_("Save Metadata As:"),
                     FileNames::DataDir(),
                     wxT("Tags.xml"),
                     wxT("xml"),
                     wxT("*.xml"),
                     wxSAVE | wxOVERWRITE_PROMPT,
                     this);

   // User canceled...
   if (fn.IsEmpty()) {
      return;
   }

   // Create/Open the file
   XMLFileWriter writer;
   writer.Open(fn, wxT("wb"));

   // Complain if open failed
   if (!writer.IsOpened())
   {
      // Constructor will emit message
      return;
   }

   // Write the metadata
   mLocal.WriteXML(writer);

   // Close the file
   writer.Close();

   return;
}

void TagsEditor2::OnSaveDefaults(wxCommandEvent & event)
{
   TransferDataFromWindow();

   gPrefs->DeleteGroup(wxT("/Tags"));
   gPrefs->Write(wxT("/Tags/ID3V2"), mLocal.GetID3V2());

   wxString n, v;
   for (bool cont = mLocal.GetFirst(n, v); cont; cont = mLocal.GetNext(n, v)) {
      gPrefs->Write(wxT("/Tags/") + n, v);
   }
}

void TagsEditor2::OnAdd(wxCommandEvent & event)
{
   mGrid->AppendRows();
}

void TagsEditor2::OnRemove(wxCommandEvent & event)
{
   int row = mGrid->GetCursorRow();

   if (row >= 0) {
      mGrid->DeleteRows(row, 1);
   }
}

void TagsEditor2::OnOk(wxCommandEvent & event)
{
   if (!Validate() || !TransferDataFromWindow()) {
      return;
   }

   *mTags = mLocal;

   wxRect r = GetRect();
   gPrefs->Write(wxT("/TagsEditor2/x"), r.x);
   gPrefs->Write(wxT("/TagsEditor2/y"), r.y);
   gPrefs->Write(wxT("/TagsEditor2/width"), r.width);
   gPrefs->Write(wxT("/TagsEditor2/height"), r.height);
   gPrefs->Write(wxT("/TagsEditor2/page"), mNotebook->GetSelection());

   EndModal(wxID_OK);
}

void TagsEditor2::OnCancel(wxCommandEvent & event)
{
   EndModal(wxID_CANCEL);
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
// arch-tag: 94f72c20-970b-4f4e-bbf3-3880fce7b965


