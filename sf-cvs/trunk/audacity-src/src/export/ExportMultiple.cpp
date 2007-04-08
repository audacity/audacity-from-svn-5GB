/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportMultiple.cpp

  Dominic Mazzoni

  2004.06.23 added mp3 export using tompg.exe for multiple export, by ChackoN
  tested under w2k only

*******************************************************************//**

\class ExportMultiple
\brief Presents a dialog box allowing the user to export multiple files
  either by exporting each track as a separate file, or by
  exporting each label as a separate file.

*//********************************************************************/

#include "../Audacity.h"

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/dirdlg.h>
#include <wx/dynarray.h>
#include <wx/event.h>
#include <wx/filedlg.h>
#include <wx/filefn.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>

#include "Export.h"
#include "ExportMultiple.h"

#include "../Internat.h"
#include "../FileFormats.h"
#include "../LabelTrack.h"
#include "../Project.h"
#include "../Prefs.h"
#include "../Tags.h"

enum {
   FormatID = 10001,
   OptionsID,
   DirID,
   CreateID,
   ChooseID,
   LabelID,
   FirstID,
   FirstFileNameID,
   TrackID,
   ByNameID,
   ByNumberID,
   PrefixID,
   OverwriteID
};

//
// ExportMultiple methods
//

BEGIN_EVENT_TABLE(ExportMultiple, wxDialog)
   EVT_CHOICE(FormatID, ExportMultiple::OnFormat)
   EVT_BUTTON(OptionsID, ExportMultiple::OnOptions)
   EVT_BUTTON(CreateID, ExportMultiple::OnCreate)
   EVT_BUTTON(ChooseID, ExportMultiple::OnChoose)
   EVT_BUTTON(wxID_OK, ExportMultiple::OnExport)
   EVT_BUTTON(wxID_CANCEL, ExportMultiple::OnCancel)
   EVT_RADIOBUTTON(LabelID, ExportMultiple::OnLabel)
   EVT_RADIOBUTTON(TrackID, ExportMultiple::OnTrack)
   EVT_RADIOBUTTON(ByNameID, ExportMultiple::OnByName)
   EVT_RADIOBUTTON(ByNumberID, ExportMultiple::OnByNumber)
   EVT_CHECKBOX(FirstID, ExportMultiple::OnFirst)
   EVT_TEXT(FirstFileNameID, ExportMultiple::OnFirstFileName)
   EVT_TEXT(PrefixID, ExportMultiple::OnPrefix)
END_EVENT_TABLE()

ExportMultiple::ExportMultiple(AudacityProject *project)
: wxDialog(project, wxID_ANY, wxString(_("Export Multiple")))
{
   mProject = project;
   mTracks = project->GetTracks();
   mTypes = Export::GetTypes();

   ShuttleGui S(this, eIsCreatingFromPrefs);

   // Creating some of the widgets cause cause events to fire
   // and we don't want that until after we're completely
   // created.  (Observed on Windows)
   mInitialized = false;
   PopulateOrExchange(S);
   mInitialized = true;

   GetSizer()->AddSpacer(5);
   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();

   EnableControls();
}

ExportMultiple::~ExportMultiple()
{
}

int ExportMultiple::ShowModal()
{
   Track *tr;

   mLabels = NULL;
   mNumLabels = 0;
   mNumTracks = 0;

   // Examine the track list looking for Wave and Label tracks
   for (tr = mIterator.First(mTracks); tr != NULL; tr = mIterator.Next()) {
      switch (tr->GetKind())
      {
         // Only count WaveTracks, and for linked pairs, only count the
         // second one of the pair
         case Track::Wave:
         {
            if (tr->GetLinked() == false) {
               mNumTracks++;
            }

            break;
         }

         // Only support one label track???
         case Track::Label:
         {
            if (mLabels == NULL) {
               mLabels = (LabelTrack *)tr;
               mNumLabels = mLabels->GetNumLabels();
            }

            break;
         }
      }
   }

   if (mNumTracks < 2 && mNumLabels < 1) {
      ::wxMessageBox(_("If you have more than one Audio Track, you can export each track as a separate file,\nor if you have a Label Track, you can export a new file for each label.\n\nThis project does not have multiple tracks or a Label Track, so you cannot export multiple files."),
                     _("Can't export multiple files"),
                     wxOK | wxCENTRE, this);
      return wxID_CANCEL;
   }

   if (mNumLabels < 1) {
      mLabel->Enable(false);
      mTrack->SetValue(true);
      mLabel->SetValue(false);
   }

   if (mNumTracks < 2) {
      mTrack->Enable(false);
      mLabel->SetValue(true);
      mTrack->SetValue(false);
   }

   EnableControls();

   return wxDialog::ShowModal();
}

void ExportMultiple::PopulateOrExchange(ShuttleGui& S)
{
   S.SetBorder(5);
   S.StartMultiColumn(4, true);
   {
      wxArrayString formats;
      
      for (size_t i = 0; i < mTypes.GetCount(); i++) {
         formats.Add(mTypes[i].GetFormat());
      }

      S.AddPrompt(_("Export format:"));
      mFormat = S.Id(FormatID)
         .TieChoice(wxT(""),
                    wxT("/Export/MultipleFormat"),
                    gPrefs->Read(wxT("/Export/Format"), wxT("WAV")),
                    formats,
                    formats);
      S.Id(OptionsID).AddButton(_("Options..."));
      S.AddVariableText(wxT(""), false);

      S.AddPrompt(_("Export location:"));
      mDir = S.Id(DirID)
         .TieTextBox(wxT(""),
                     wxT("/Export/MultiplePath"),
                     gPrefs->Read(wxT("/Export/Path"), ::wxGetCwd()),
                     64);
      S.Id(ChooseID).AddButton(_("Choose..."));
      S.Id(CreateID).AddButton(_("Create"));
   }
   S.EndMultiColumn();

   S.StartHorizontalLay(wxEXPAND, true);
   {
      S.SetBorder(5);
      S.StartStatic(_("Split files based on:"), true);
      {
         S.StartMultiColumn(2, false);
         S.SetStretchyCol(1);
         {
            // Row 1
            S.SetBorder(1);
            mLabel = S.AddRadioButton(wxT(""));
            S.SetBorder(3);
            mLabelLabel = S.Id(LabelID)
               .AddVariableText(_("Labels"), false);

            // Row 2
            S.AddVariableText(wxT(""), false);
            mFirst = S.Id(FirstID)
               .AddCheckBox(_("Include audio before first label"), wxT("false"));

            // Row 3
            S.AddVariableText(wxT(""), false);
            S.StartHorizontalLay(wxEXPAND, false);
            {
               mFirstFileLabel = S.AddVariableText(_("First file name:"), true);
               mFirstFileName = S.Id(FirstFileNameID)
                  .TieTextBox(wxT(""),
                              wxT("/Export/MultipleFirstName"),
                              wxT(""),
                              30);
            }
            S.EndHorizontalLay();

            // Row 4
            S.SetBorder(1);
            mTrack = S.Id(TrackID)
               .AddRadioButtonToGroup(wxT(""));
            S.SetBorder(3);
            mTrackLabel = S.AddVariableText(_("Tracks"), false);
         }
         S.EndMultiColumn();
      }
      S.EndStatic();

      S.SetBorder(5);
      S.StartStatic(_("Name files:"), false);
      {
         S.StartMultiColumn(2, false);
         S.SetStretchyCol(1);
         {
            // Row 1
            S.SetBorder(1);
            mByName = S.Id(ByNameID)
               .AddRadioButton(wxT(""));
            S.SetBorder(3);
            mByNameLabel = S.AddVariableText(_("Using Label/Track Name"), false);

            // Row 2
            S.SetBorder(1);
            mByNumber = S.Id(ByNumberID)
               .AddRadioButtonToGroup(wxT(""));
            S.SetBorder(3);
            mByNumberLabel = S.AddVariableText(_("Numbering consecutively"), false);

            // Row 3
            S.AddVariableText(wxT(""), false);
            S.StartHorizontalLay(wxEXPAND, false);
            {
               mPrefixLabel = S.AddVariableText(_("File name prefix:"), true);
               mPrefix = S.Id(PrefixID)
                  .TieTextBox(wxT(""),
                              wxT("/Export/MultiplePrefix"),
                              wxT(""),
                              30);
            }
            S.EndHorizontalLay();
         }
         S.EndMultiColumn();
      }
      S.EndStatic();
   }
   S.EndHorizontalLay();

   S.SetBorder(5);
   S.StartHorizontalLay(wxEXPAND, false);
   {
      mOverwrite = S.Id(OverwriteID)
         .AddCheckBox(_("Overwrite existing files"), wxT("true"));
   }
   S.EndHorizontalLay();
      
   S.StartHorizontalLay(wxALIGN_CENTER, false);
   {
#if defined(__WXGTK20__) || defined(__WXMAC__)
      S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
      mExport = S.Id(wxID_OK).AddButton(_("E&xport"));
#else
      mExport = S.Id(wxID_OK).AddButton(_("E&xport"));
      S.Id(wxID_CANCEL).AddButton(_("&Cancel"));
#endif
      mExport->SetDefault();
   }
   S.EndHorizontalLay();
}

void ExportMultiple::EnableControls()
{
   bool enable;

   if (!mInitialized) {
      return;
   }

   mFirst->Enable(mLabel->GetValue() && mByName->GetValue());
   
   enable = mLabel->GetValue() &&
            mByName->GetValue() &&
            mFirst->GetValue();
   mFirstFileLabel->Enable(enable);
   mFirstFileName->Enable(enable);

   enable = mByNumber->GetValue();
   mPrefixLabel->Enable(enable);
   mPrefix->Enable(enable);

   bool ok = true;

   if (mLabel->GetValue() && mFirst->GetValue() &&
       mFirstFileName->GetValue() == wxT(""))
      ok = false;

   if (mByNumber->GetValue() &&
       mPrefix->GetValue() == wxT(""))
      ok = false;

   mExport->Enable(ok);
}

void ExportMultiple::OnFormat(wxCommandEvent& event)
{
   EnableControls();
}

void ExportMultiple::OnOptions(wxCommandEvent& event)
{
   mTypes[mFormat->GetSelection()].DisplayOptions(mProject);
}

void ExportMultiple::OnCreate(wxCommandEvent& event)
{
   wxFileName fn;

   fn.AssignDir(mDir->GetValue());

   bool ok = fn.Mkdir(0777, wxPATH_MKDIR_FULL);

   if (!ok) {
      // Mkdir will produce an error dialog
      return;
   }

   ::wxMessageBox(wxString::Format(_("\"%s\" successfully created."),
                                   fn.GetPath().c_str()),
                  _("Export Multiple"),
                  wxOK | wxCENTRE, this);
}

void ExportMultiple::OnChoose(wxCommandEvent& event)
{
   wxDirDialog dlog(this,
                    _("Choose a location to save the exported files"),
                    mDir->GetValue());
   dlog.ShowModal();
   if (dlog.GetPath() != wxT(""))
      mDir->SetValue(dlog.GetPath());
}

void ExportMultiple::OnLabel(wxCommandEvent& event)
{
   EnableControls();
}

void ExportMultiple::OnFirst(wxCommandEvent& event)
{
   EnableControls();
}

void ExportMultiple::OnFirstFileName(wxCommandEvent& event)
{
   EnableControls();
}

void ExportMultiple::OnTrack(wxCommandEvent& event)
{
   EnableControls();
}

void ExportMultiple::OnByName(wxCommandEvent& event)
{
   EnableControls();
}

void ExportMultiple::OnByNumber(wxCommandEvent& event)
{
   EnableControls();
}

void ExportMultiple::OnPrefix(wxCommandEvent& event)
{
   EnableControls();
}

void ExportMultiple::OnCancel(wxCommandEvent& event)
{
   EndModal(0);
}

void ExportMultiple::OnExport(wxCommandEvent& event)
{
   ShuttleGui S(this, eIsSavingToPrefs);
   PopulateOrExchange(S);

   // Make sure the output directory is in good shape
   if (!DirOk()) {
      return;
   }

   mFormatIndex = mFormat->GetSelection();
   bool overwrite = mOverwrite->GetValue();

   if (mTypes[mFormatIndex].GetCanMetaData()) {
      Tags *tags = mProject->GetTags();

      if (tags->IsEmpty()) {
         wxString saveTitle = tags->GetTitle();
         int saveTrackNumber = tags->GetTrackNumber();
         tags->SetTitle(wxT("(automatic)"));
         tags->SetTrackNumber(0);
         tags->AllowEditTitle(false);
         tags->AllowEditTrackNumber(false);
         bool rval = tags->ShowEditDialog(mProject,
                                          _("Edit the ID3 tags for all MP3 files"),
                                          true);
         tags->AllowEditTitle(true);
         tags->AllowEditTrackNumber(true);
         if (!rval) {
            tags->SetTitle(saveTitle);
            tags->SetTrackNumber(saveTrackNumber);
            return;
         }
      }
   }

   bool ok;

   if (mLabel->GetValue()) {
      ok = ExportMultipleByLabel(mByName->GetValue(),
                                 mPrefix->GetValue());
   }
   else {
      ok = ExportMultipleByTrack(mByName->GetValue(),
                                 mPrefix->GetValue());
   }

   if (!ok) {
      return;
   }

   EndModal(1);
}

bool ExportMultiple::DirOk()
{
   wxFileName fn;

   fn.AssignDir(mDir->GetValue());

   if (fn.DirExists()) {
      return true;
   }

   wxString prompt;

   prompt.Printf(_("\"%s\" doesn't exist.\n\nWould you like to create it?"),
                 fn.GetFullPath().c_str());
            
   int action = wxMessageBox(prompt,
                             wxT("Warning"),
                             wxYES_NO | wxICON_EXCLAMATION);
   if (action != wxYES) {
      return false;
   }

   return fn.Mkdir(0777, wxPATH_MKDIR_FULL);
}

bool ExportMultiple::ExportMultipleByLabel(bool byName, wxString prefix)
{
   Track *tr;
   int numFiles = mNumLabels;
   int l = 0;
   int numLeft = 0;
   int numRight = 0;
   int numMono = 0;

   // Account for exporting before first label
   if (mFirst->GetValue()) {
      l--;
      numFiles++;
   }

   // Figure out if we're exporting stereo or mono
   for (tr = mIterator.First(mTracks); tr != NULL; tr = mIterator.Next()) {

      // Only want wave tracks
      if (!tr->GetKind() == Track::Wave) {
         continue;
      }

      // Found a left channel
      if (tr->GetChannel() == Track::LeftChannel) {
         numLeft++;
      }

      // Found a right channel
      else if (tr->GetChannel() == Track::RightChannel) {
         numRight++;
      }

      // Found a mono channel, but it may be panned
      else if (tr->GetChannel() == Track::MonoChannel) {
         float pan = ((WaveTrack*)tr)->GetPan();

         // Figure out what kind of channel it should be
         if (pan == -1.0) {
            numLeft++;
         }
         else if (pan == 1.0) {
            numRight++;
         }
         else if (pan == 0) {
            numMono++;
         }
         else {
            numLeft++;
            numRight++;
         }
      }
   }

   // We we have a stereo or mono mix?
   bool stereo = false;
   if (numRight > 0 || numLeft > 0) {
      stereo = true;
   }

   wxArrayString otherNames;
   wxString name;
   double t0, t1;
   int count = 0;
   bool ok = true;

   // Examine all labels
   while (l < mNumLabels) {
      const LabelStruct *info = NULL;

      // Get file name and starting time
      if (l < 0) {
         name = mFirstFileName->GetValue();
         t0 = 0.0;
      }
      else {
         info = mLabels->GetLabel(l);
         name = info->title;
         t0 = info->t;
      }

      // Figure out the ending time
      if (info && info->t < info->t1) {
         t1 = info->t1;
      }
      else if (l < mNumLabels-1) {
         const LabelStruct *info1 = mLabels->GetLabel(l+1);
         t1 = info1->t;
      }
      else {
         t1 = mTracks->GetEndTime();
      }

      // Numbering files...
      if (!byName) {
         name.Printf(wxT("%s-%d"), prefix.c_str(), count+1);
      }

      // Make sure the file name is unique within the directory
      MakeNameUnique(otherNames, name);
      
      // Export it
      ok = DoExport(stereo, name, false, t0, t1, count+1);
      if (!ok) {
         break;
      }

      count++;
      l++;
   }
   
   // Give 'em the result
   ::wxMessageBox(wxString::Format(ok
                                   ? _("Successfully exported %d file(s).")
                                   : _("Something went wrong after exporting %d file(s)."),
                                   count),
                  _("Export Multiple"),
                  wxOK | wxCENTRE, this);

   return ok;
}

bool ExportMultiple::ExportMultipleByTrack(bool byName,
                                           wxString prefix)
{
   Track *tr, *tr2;
   double t0, t1;
   bool stereo;
   bool ok = true;
   int i=0;
   wxString name;
   wxArrayString otherNames;
   wxArrayPtrVoid selected;
   
   // Remember which tracks were selected and set to unselected
   for (tr = mIterator.First(mTracks); tr != NULL; tr = mIterator.Next()) {
      if (tr->GetKind() != Track::Wave) {
         continue;
      }

      if (tr->GetSelected()) {
         selected.Add(tr);
         tr->SetSelected(false);
      }
   }

   // Examine all tracks
   for (tr = mIterator.First(mTracks); tr != NULL; tr = mIterator.Next()) {

      // Only want wave tracks
      if (tr->GetKind() != Track::Wave) {
         continue;
      }

      // Get the times for the track
      t0 = tr->GetStartTime();
      t1 = tr->GetEndTime();

      // Select the track
      tr->SetSelected(true);

      // Check for a linked track
      tr2 = NULL;
      if (tr->GetLinked()) {
         tr2 = mIterator.Next();
         if (tr2) {

            // Select it also
            tr2->SetSelected(true);

            // Make sure it gets included
            if (tr2->GetStartTime() < t0) {
               t0 = tr2->GetStartTime();
            }

            if (tr2->GetEndTime() > t1) {
               t1 = tr2->GetEndTime();
            }
         }
      }

      // Stereo or mono
      stereo = !(tr2 == NULL && 
                 tr->GetChannel() == WaveTrack::MonoChannel &&
                 ((WaveTrack *)tr)->GetPan() == 0.0);

      // Get name base
      if (byName) {
         name = tr->GetName();
      }
      else {
         name.Printf(wxT("%s-%d"), prefix.c_str(), i+1);
      }

      // Make sure it's unique
      MakeNameUnique(otherNames, name);

      // Go export it
      ok = DoExport(stereo, name, true, t0, t1, i+1);

      // Reset selection state
      tr->SetSelected(false);
      if (tr2) {
         tr2->SetSelected(false);
      }

      // Stop if an error occurred
      if (!ok) {
         break;
      }

      i++;
   }   
   
   // Restore the selection states
   for (size_t i = 0; i < mSelected.GetCount(); i++) {
      ((Track *) selected[i])->SetSelected(true);
   }

   // Give 'em the result
   ::wxMessageBox(wxString::Format(ok
                                   ? _("Successfully exported %d file(s).")
                                   : _("Something went wrong after exporting %d file(s)."),
                                   i),
                  _("Export Multiple"),
                  wxOK | wxCENTRE, this);
   
   return ok;
}

void ExportMultiple::MakeNameUnique(wxArrayString &otherNames, wxString &newName)
{
   if (otherNames.Index(newName, false) >= 0) {
      int i=2;
      wxString orig = newName;
      do {
         newName.Printf(wxT("%s-%d"), orig.c_str(), i);
         i++;
      } while (otherNames.Index(newName, false) >= 0);
   }
   otherNames.Add(newName);
}

bool ExportMultiple::DoExport(bool stereo,
                              wxString name,
                              bool selectedOnly,
                              double t0,
                              double t1,
                              int trackNumber)
{
   wxFileName fn(mDir->GetValue(), name, mTypes[mFormatIndex].GetExtension());

   // Generate a unique name if we're not allowed to overwrite
   if (!mOverwrite->GetValue()) {
      int i = 2;
      while (fn.FileExists()) {
         fn.SetName(wxString::Format(wxT("%s-%d"), name.c_str(), i++));
      }
   }

   // If the format supports tags, then set the name and track number
   if (mTypes[mFormatIndex].GetCanMetaData()) {
      Tags *tags = mProject->GetTags();
      tags->SetTitle(name);
      tags->SetTrackNumber(trackNumber);
   }

   // Call the format export routine
   return mTypes[mFormatIndex].Export(mProject,
                                      stereo ? 2 : 1,
                                      fn.GetFullPath(),
                                      selectedOnly,
                                      t0,
                                      t1);
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
// arch-tag: d6904b91-a320-4194-8d60-caa9175b6bb4
