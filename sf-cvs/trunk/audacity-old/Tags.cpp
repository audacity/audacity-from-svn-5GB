/**********************************************************************

  Audacity: A Digital Audio Editor

  Tags.cpp

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
  
  Use of this functionality requires that id3lib be compiled in
  with Audacity.
  
**********************************************************************/

#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/button.h>

#include "Audacity.h"

#ifdef USE_ID3LIB
   #include "id3/tag.h"
   #include "id3/misc_support.h"
#endif

#include "Tags.h"

const int gNumGenres = 128;
extern const wxString gGenreList[gNumGenres];

Tags::Tags()
{
   mTrackNum = -1;
   mGenre = 127;
   
   mID3V2 = true;
}

Tags::~Tags()
{
}

bool Tags::Load(wxTextFile * in, DirManager * dirManager)
{
/*   Oddly enought, MSVC doesn't understand #warning  */
/*   #warning TODO */

   return true;
}

bool Tags::Save(wxTextFile * out, bool overwrite)
{
   return true;
}

bool Tags::ShowEditDialog(wxWindow *parent, wxString title)
{
#ifdef USE_ID3LIB
   Tags theCopy;
   theCopy.mTitle = mTitle;
   theCopy.mArtist = mArtist;
   theCopy.mAlbum = mAlbum;
   theCopy.mTrackNum = mTrackNum;
   theCopy.mYear = mYear;
   theCopy.mGenre = mGenre;
   theCopy.mComments = mComments;
   theCopy.mID3V2 = mID3V2;

   TagsDialog dlog(parent, -1, title);
   dlog.mTags = this;
   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode()) {
      mTitle = theCopy.mTitle;
      mArtist = theCopy.mArtist;
      mAlbum = theCopy.mAlbum;
      mTrackNum = theCopy.mTrackNum;
      mYear = theCopy.mYear;
      mGenre = theCopy.mGenre;
      mComments = theCopy.mComments;
      mID3V2 = theCopy.mID3V2;

      return false;
   }

   return true;
#else
   return true; // we want callers to think it succeeded, otherwise they'll cancel what
                // they were doing...
#endif
}

void Tags::ImportID3(wxString fileName)
{
#ifdef USE_ID3LIB
   ID3_Tag tag((char *) fileName.c_str());

   mTitle = ID3_GetTitle(&tag);
   mArtist = ID3_GetArtist(&tag);
   mAlbum = ID3_GetAlbum(&tag);
   mTrackNum = ID3_GetTrackNum(&tag);
   mYear = ID3_GetYear(&tag);
   mGenre = ID3_GetGenreNum(&tag);
   mComments = ID3_GetComment(&tag);
#endif // USE_ID3LIB
}

// returns buffer len; caller frees
int Tags::ExportID3(char **buffer, bool *endOfFile)
{
#ifdef USE_ID3LIB
   ID3_Tag tag;   

   ID3_AddTitle(&tag, (const char *)mTitle);
   ID3_AddArtist(&tag, (const char *)mArtist);
   ID3_AddAlbum(&tag, (const char *)mAlbum);
   if (mTrackNum >=0 && mTrackNum<=255)
      ID3_AddTrack(&tag, (uchar)mTrackNum);
   ID3_AddGenre(&tag, mGenre);
   ID3_AddComment(&tag, (const char *)mComments, STR_V1_COMMENT_DESC);
   ID3_AddYear(&tag, (const char *)mYear);
   
   if (mID3V2) {
      int tagSize = tag.Size();
      *buffer = new char[tagSize];
      tag.Render((uchar *)*buffer, ID3TT_ID3V2);
      *endOfFile = false;
      return tagSize;
   }
   else {
      *buffer = new char[128];
      for(int i=0; i<128; i++)
         (*buffer)[i] = 0;
      tag.Render((uchar *)*buffer, ID3TT_ID3V1);
      *endOfFile = true;
      return 128;
   }
   
#else
   *buffer = new char[0];
   *endOfFile = true;
   return 0;
#endif // USE_ID3LIB
}

//
// TagsDialog
//

BEGIN_EVENT_TABLE(TagsDialog, wxDialog)
    EVT_BUTTON(wxID_OK, TagsDialog::OnOk)
    EVT_BUTTON(wxID_CANCEL, TagsDialog::OnCancel)
END_EVENT_TABLE()

TagsDialog::TagsDialog(wxWindow * parent, wxWindowID id, const wxString & title, const wxPoint & position, const wxSize & size, long style):
  wxDialog(parent, id, title, position, size, style)
{
   MakeTagsDialog(this, TRUE, TRUE);
}

bool TagsDialog::Validate()
{
   return TRUE;
}

bool TagsDialog::TransferDataToWindow()
{
   wxTextCtrl *text = GetTitleText();
   if (text) {
      text->SetValue(mTags->mTitle);
   }

   text = GetArtistText();
   if (text) {
      text->SetValue(mTags->mArtist);
   }
   
   text = GetAlbumText();
   if (text) {
      text->SetValue(mTags->mAlbum);
   }
   
   text = GetTrackNumText();
   if (text && mTags->mTrackNum != -1) {
      wxString numStr;
      numStr.Printf("%d", mTags->mTrackNum);
      text->SetValue(numStr);
   }

   text = GetYearText();
   if (text) {
      text->SetValue(mTags->mYear);
   }
   
   wxChoice *genre = GetGenreChoice();
   if (genre && mTags->mGenre>=0 && mTags->mGenre<gNumGenres) {
      genre->SetSelection(mTags->mGenre);
   }
   
   wxRadioBox *format = GetFormatRadioBox();
   if (format) {
      format->SetSelection((int)mTags->mID3V2);
   }
   
   text = GetCommentsText();
   if (text) {
      text->SetValue(mTags->mComments);
   }


   return TRUE;
}

bool TagsDialog::TransferDataFromWindow()
{
   wxTextCtrl *c = GetTitleText();
   if (c) {
      mTags->mTitle = c->GetValue();
   }

   c = GetArtistText();
   if (c) {
      mTags->mArtist = c->GetValue();
   }

   c = GetAlbumText();
   if (c) {
      mTags->mAlbum = c->GetValue();
   }

   c = GetTrackNumText();
   if (c) {
      wxString str = c->GetValue();
      if (str == "")
         mTags->mTrackNum = -1;
      else {
         long i;
         str.ToLong(&i);
         mTags->mTrackNum = i;
      }
   }

   c = GetYearText();
   if (c) {
      mTags->mYear = c->GetValue();
   }

   c = GetCommentsText();
   if (c) {
      mTags->mComments = c->GetValue();
   }
   
   wxChoice *genre = GetGenreChoice();
   if (genre && mTags->mGenre>=0 && mTags->mGenre<gNumGenres) {
      mTags->mGenre = genre->GetSelection();
   }
   
   wxRadioBox *format = GetFormatRadioBox();
   if (format) {
      mTags->mID3V2 = (bool)format->GetSelection();
   }

   return TRUE;
}

void TagsDialog::OnOk(wxCommandEvent & event)
{
   TransferDataFromWindow();

   if (Validate())
      EndModal(true);
   else {
      event.Skip();
   }
}

void TagsDialog::OnCancel(wxCommandEvent & event)
{
   EndModal(false);
}

wxSizer *MakeTagsDialog(wxPanel * parent, bool call_fit,
                             bool set_sizer)
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);

   /***/

   wxString formats[2] = {"ID3v1 (more compatible)",
                          "ID3v2 (more flexible)"};
   wxRadioBox *format = new wxRadioBox(parent, ID_FORMAT, "Format:",
                                       wxDefaultPosition, wxDefaultSize,
                                       2, formats,
                                       0, wxRA_VERTICAL);
   mainSizer->Add(format, 1, wxEXPAND | wxALL, 5);

   /***/
   
   wxFlexGridSizer *gridSizer = new wxFlexGridSizer(2, 0, 0);

   wxStaticText *item3 =
       new wxStaticText(parent, ID_TEXT, "Title:",
                        wxDefaultPosition, wxDefaultSize, 0);
   gridSizer->Add(item3, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

   wxTextCtrl *item4 =
       new wxTextCtrl(parent, ID_TITLE_TEXT, "", wxDefaultPosition,
                      wxSize(200, -1), 0);
   gridSizer->Add(item4, 1, wxEXPAND | wxALL, 5);

   wxStaticText *item5 =
       new wxStaticText(parent, ID_TEXT, "Artist:",
                        wxDefaultPosition, wxDefaultSize, 0);
   gridSizer->Add(item5, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

   wxTextCtrl *item6 =
       new wxTextCtrl(parent, ID_ARTIST_TEXT, "", wxDefaultPosition,
                      wxSize(200, -1), 0);
   gridSizer->Add(item6, 1, wxEXPAND | wxALL, 5);

   wxStaticText *item7 =
       new wxStaticText(parent, ID_TEXT, "Album:",
                        wxDefaultPosition, wxDefaultSize, 0);
   gridSizer->Add(item7, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

   wxTextCtrl *item8 =
       new wxTextCtrl(parent, ID_ALBUM_TEXT, "", wxDefaultPosition,
                      wxSize(200, -1), 0);
   gridSizer->Add(item8, 1, wxEXPAND | wxALL, 5);

   mainSizer->Add(gridSizer, 0, wxALIGN_CENTRE | wxALL, 5);

   /***/
   
   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);

   wxStaticText *item9 =
       new wxStaticText(parent, ID_TEXT, "Track Number:",
                        wxDefaultPosition, wxDefaultSize, 0);
   hSizer->Add(item9, 0, wxALIGN_CENTRE | wxALL, 5);

   wxTextCtrl *item10 =
       new wxTextCtrl(parent, ID_TRACK_NUM_TEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0);
   hSizer->Add(item10, 0, wxALIGN_CENTRE | wxALL, 5);

   wxStaticText *item11 =
       new wxStaticText(parent, ID_TEXT, "Year:",
                        wxDefaultPosition, wxDefaultSize, 0);
   hSizer->Add(item11, 0, wxALIGN_CENTRE | wxALL, 5);

   wxTextCtrl *item12 =
       new wxTextCtrl(parent, ID_YEAR_TEXT, "", wxDefaultPosition,
                      wxSize(40, -1), 0);
   hSizer->Add(item12, 0, wxALIGN_CENTRE | wxALL, 5);
   
   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE | wxALL, 5);

   /***/
   gridSizer = new wxFlexGridSizer(2, 0, 0);

   wxStaticText *item20 =
       new wxStaticText(parent, ID_TEXT, "Genre:",
                        wxDefaultPosition, wxDefaultSize, 0);
   gridSizer->Add(item20, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

   wxChoice *item21 =
       new wxChoice(parent, ID_GENRE,
                    wxDefaultPosition, wxSize(-1, -1),
                    gNumGenres, gGenreList);
   gridSizer->Add(item21, 1, wxEXPAND | wxALL, 5);
   
   wxStaticText *item22 =
       new wxStaticText(parent, ID_TEXT, "Comments:",
                        wxDefaultPosition, wxDefaultSize, 0);
   gridSizer->Add(item22, 0, wxALIGN_RIGHT | wxALIGN_CENTER_VERTICAL | wxALL, 5);

   wxTextCtrl *item23 =
       new wxTextCtrl(parent, ID_COMMENTS_TEXT, "", wxDefaultPosition,
                      wxSize(200, -1), 0);
   gridSizer->Add(item23, 1, wxEXPAND | wxALL, 5);
   
   mainSizer->Add(gridSizer, 0, wxALIGN_CENTRE | wxALL, 5);
   
   /***/
   
   wxBoxSizer *okSizer = new wxBoxSizer(wxHORIZONTAL);

   wxButton *item14 =
       new wxButton(parent, wxID_OK, "OK", wxDefaultPosition,
                    wxDefaultSize, 0);
   item14->SetDefault();
   item14->SetFocus();
   okSizer->Add(item14, 0, wxALIGN_CENTRE | wxALL, 5);

   wxButton *item15 =
       new wxButton(parent, wxID_CANCEL, "Cancel", wxDefaultPosition,
                    wxDefaultSize, 0);
   okSizer->Add(item15, 0, wxALIGN_CENTRE | wxALL, 5);

   mainSizer->Add(okSizer, 0, wxALIGN_CENTRE | wxALL, 5);

   if (set_sizer) {
      parent->SetAutoLayout(TRUE);
      parent->SetSizer(mainSizer);
      if (call_fit) {
         mainSizer->Fit(parent);
         mainSizer->SetSizeHints(parent);
      }
   }
   
   return mainSizer;
}

const wxString gGenreList[gNumGenres] = {
/*  0*/ "Blues",
/*  1*/ "Classic Rock",
/*  2*/ "Country",
/*  3*/ "Dance",
/*  4*/ "Disco",
/*  5*/ "Funk",
/*  6*/ "Grunge",
/*  7*/ "Hip-Hop",
/*  8*/ "Jazz",
/*  9*/ "Metal",
/* 10*/ "New Age",
/* 11*/ "Oldies",
/* 12*/ "Other",
/* 13*/ "Pop",
/* 14*/ "R&B",
/* 15*/ "Rap",
/* 16*/ "Reggae",
/* 17*/ "Rock",
/* 18*/ "Techno",
/* 19*/ "Industrial",
/* 20*/ "Alternative",
/* 21*/ "Ska",
/* 22*/ "Death Metal",
/* 23*/ "Pranks",
/* 24*/ "Soundtrack",
/* 25*/ "Euro-Techno",
/* 26*/ "Ambient",
/* 27*/ "Trip-Hop",
/* 28*/ "Vocal",
/* 29*/ "Jazz+Funk",
/* 30*/ "Fusion",
/* 31*/ "Trance",
/* 32*/ "Classical",
/* 33*/ "Instrumental",
/* 34*/ "Acid",
/* 35*/ "House",
/* 36*/ "Game",
/* 37*/ "Sound Clip",
/* 38*/ "Gospel",
/* 39*/ "Noise",
/* 40*/ "AlternRock",
/* 41*/ "Bass",
/* 42*/ "Soul",
/* 43*/ "Punk",
/* 44*/ "Space",
/* 45*/ "Meditative",
/* 46*/ "Instrumental Pop",
/* 47*/ "Instrumental Rock",
/* 48*/ "Ethnic",
/* 49*/ "Gothic",
/* 50*/ "Darkwave",
/* 51*/ "Techno-Industrial",
/* 52*/ "Electronic",
/* 53*/ "Pop-Folk",
/* 54*/ "Eurodance",
/* 55*/ "Dream",
/* 56*/ "Southern Rock",
/* 57*/ "Comedy",
/* 58*/ "Cult",
/* 59*/ "Gangsta",
/* 60*/ "Top 40",
/* 61*/ "Christian Rap",
/* 62*/ "Pop/Funk",
/* 63*/ "Jungle",
/* 64*/ "Native American",
/* 65*/ "Cabaret",
/* 66*/ "New Wave",
/* 67*/ "Psychadelic",
/* 68*/ "Rave",
/* 69*/ "Showtunes",
/* 70*/ "Trailer",
/* 71*/ "Lo-Fi",
/* 72*/ "Tribal",
/* 73*/ "Acid Punk",
/* 74*/ "Acid Jazz",
/* 75*/ "Polka",
/* 76*/ "Retro",
/* 77*/ "Musical",
/* 78*/ "Rock & Roll",
/* 79*/ "Hard Rock",
/*** Winamp extensions ***/
/* 80*/ "Folk",
/* 81*/ "Folk-Rock",
/* 82*/ "National Folk",
/* 83*/ "Swing",
/* 84*/ "Fast Fusion",
/* 85*/ "Bebop",
/* 86*/ "Latin",
/* 87*/ "Revival",
/* 88*/ "Celtic",
/* 89*/ "Bluegrass",
/* 90*/ "Avantgarde",
/* 91*/ "Gothic Rock",
/* 92*/ "Progressive Rock",
/* 93*/ "Psychedelic Rock",
/* 94*/ "Symphonic Rock",
/* 95*/ "Slow Rock",
/* 96*/ "Big Band",
/* 97*/ "Chorus",
/* 98*/ "Easy Listening",
/* 99*/ "Acoustic",
/*100*/ "Humour",
/*101*/ "Speech",
/*102*/ "Chanson",
/*103*/ "Opera",
/*104*/ "Chamber Music",
/*105*/ "Sonata",
/*106*/ "Symphony",
/*107*/ "Booty Bass",
/*108*/ "Primus",
/*109*/ "Porn Groove",
/*110*/ "Satire",
/*111*/ "Slow Jam",
/*112*/ "Club",
/*113*/ "Tango",
/*114*/ "Samba",
/*115*/ "Folklore",
/*116*/ "Ballad",
/*117*/ "Power Ballad",
/*118*/ "Rhythmic Soul",
/*119*/ "Freestyle",
/*120*/ "Duet",
/*121*/ "Punk Rock",
/*122*/ "Drum Solo",
/*123*/ "A capella",
/*124*/ "Euro-House",
/*125*/ "Dance Hall",
/*126*/ "---",
/*127*/ "None"};

