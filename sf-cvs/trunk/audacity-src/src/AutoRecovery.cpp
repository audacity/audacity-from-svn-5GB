#include "AutoRecovery.h"
#include "Audacity.h"
#include "AudacityApp.h"
#include "FileNames.h"
#include "blockfile/SimpleBlockFile.h"

#include <wx/wxprec.h>
#include <wx/filefn.h>
#include <wx/dir.h>
#include <wx/dialog.h>
#include <wx/app.h>

enum {
   ID_RECOVER_ALL = 10000,
   ID_RECOVER_NONE,
   ID_QUIT_AUDACITY,
   ID_FILE_LIST
};

class AutoRecoveryDialog : public wxDialog
{
public:
   AutoRecoveryDialog(wxWindow *parent);

private:
   void PopulateList();
   void PopulateOrExchange(ShuttleGui & S);
   
   void OnQuitAudacity(wxCommandEvent &evt);
   void OnRecoverNone(wxCommandEvent &evt);
   void OnRecoverAll(wxCommandEvent &evt);

   wxListCtrl *mFileList;

public:
   DECLARE_EVENT_TABLE()
};

AutoRecoveryDialog::AutoRecoveryDialog(wxWindow *parent) :
   wxDialog(parent, -1, _("Automatic Crash Recovery"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE & (~wxCLOSE_BOX)) // no close box
{
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

BEGIN_EVENT_TABLE(AutoRecoveryDialog, wxDialog)
   EVT_BUTTON(ID_RECOVER_ALL, AutoRecoveryDialog::OnRecoverAll)
   EVT_BUTTON(ID_RECOVER_NONE, AutoRecoveryDialog::OnRecoverNone)
   EVT_BUTTON(ID_QUIT_AUDACITY, AutoRecoveryDialog::OnQuitAudacity)
END_EVENT_TABLE()

void AutoRecoveryDialog::PopulateOrExchange(ShuttleGui& S)
{
   S.SetBorder(5);
   S.StartVerticalLay();
   {
      S.AddVariableText(_("Some projects were not saved properly the last time Audacity was run.\nFortunately, the following projects can automatically be recovered:"), false);

      S.StartStatic(_("Recoverable projects"));
      {  
         mFileList = S.Id(ID_FILE_LIST).AddListControlReportMode();
         mFileList->InsertColumn(0, _("Name"));
         mFileList->SetColumnWidth(0, 220);
         PopulateList();
      }
      S.EndStatic();

      S.AddVariableText(_("Recovering a project will not change any files on disk before you save it."), false);

      S.StartHorizontalLay(true);
      {
         S.Id(ID_QUIT_AUDACITY).AddButton(_("Quit Audacity"));
         S.Id(ID_RECOVER_NONE).AddButton(_("Don't recover"));
         S.Id(ID_RECOVER_ALL).AddButton(_("Recover projects"));
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   Layout();
   Fit();
   SetMinSize(GetSize());
   Center();
}

void AutoRecoveryDialog::PopulateList()
{
   mFileList->DeleteAllItems();
   
   wxDir dir(FileNames::AutoSaveDir());
   if (!dir.IsOpened())
      return;
      
   wxString filename;
   int i = 0;
   for (bool c = dir.GetFirst(&filename); c; c = dir.GetNext(&filename))
      mFileList->InsertItem(i++, wxFileName(filename).GetName());
      
   mFileList->SetColumnWidth(0, wxLIST_AUTOSIZE);
}

void AutoRecoveryDialog::OnQuitAudacity(wxCommandEvent &evt)
{
   EndModal(ID_QUIT_AUDACITY);
}

void AutoRecoveryDialog::OnRecoverNone(wxCommandEvent &evt)
{
   int ret = wxMessageBox(
      _("You have choosen not to recover any projects. You will not be able to recover those projects later if you change your mind. Do you really want to continue without recovering?"),
      _("Question"), wxICON_QUESTION | wxYES_NO | wxNO_DEFAULT, this);

   if (ret == wxYES)
      EndModal(ID_RECOVER_NONE);
}

void AutoRecoveryDialog::OnRecoverAll(wxCommandEvent &evt)
{
   EndModal(ID_RECOVER_ALL);
}

////////////////////////////////////////////////////////////////////////////

static bool HaveFilesToRecover()
{
   wxDir dir(FileNames::AutoSaveDir());
   if (!dir.IsOpened())
   {
      wxMessageBox(_("Could not enumerate files in auto save directory"),
                   _("Error"), wxICON_STOP);
      return false;
   }
   
   wxString filename;
   bool c = dir.GetFirst(&filename, wxT(""), wxDIR_FILES);
   
   return c;
}

static bool RemoveAllAutoSaveFiles()
{
   wxDir dir(FileNames::AutoSaveDir());
   if (!dir.IsOpened())
   {
      wxMessageBox(_("Could not enumerate files in auto save directory"),
                   _("Error"), wxICON_STOP);
      return false;
   }

   wxString filename;
   for (bool c = dir.GetFirst(&filename, wxT(""), wxDIR_FILES);
        c; c = dir.GetNext(&filename))
   {
      wxFileName fullPath(FileNames::AutoSaveDir(), filename);
      if (!wxRemoveFile(fullPath.GetFullPath()))
      {
         wxMessageBox(_("Could not remove auto save file: " + filename),
                      _("Error"), wxICON_STOP);
         return false;
      }
   }
   
   return true;
}

static bool RecoverAllProjects(AudacityProject** pproj)
{
   wxDir dir(FileNames::AutoSaveDir());
   if (!dir.IsOpened())
   {
      wxMessageBox(_("Could not enumerate files in auto save directory"),
                   _("Error"), wxICON_STOP);
      return false;
   }
   
   // Open a project window for each auto save file
   wxString filename;
   AudacityProject* proj = NULL;
   
   for (bool c = dir.GetFirst(&filename, wxT(""), wxDIR_FILES);
        c; c = dir.GetNext(&filename))
   {
      wxFileName fullPath(FileNames::AutoSaveDir(), filename);

      if (*pproj)
      {
         // Reuse existing project window
         proj = *pproj;
         *pproj = NULL;
      } else
      {
         // Create new project window
         proj = CreateNewAudacityProject(gParentWindow);
      }

      // Open project. When an auto-save file has been opened successfully,
      // the opened auto-save file is automatically deleted and a new one
      // is created.
      proj->OpenFile(fullPath.GetFullPath());
   }
   
   return true;
}

bool ShowAutoRecoveryDialogIfNeeded(AudacityProject** pproj)
{
   if (HaveFilesToRecover())
   {
      AutoRecoveryDialog dlg(*pproj);
      int ret = dlg.ShowModal();
   
      switch (ret)
      {
      case ID_RECOVER_NONE:
         return RemoveAllAutoSaveFiles();
   
      case ID_RECOVER_ALL:
         return RecoverAllProjects(pproj);

      default:
         // This includes ID_QUIT_AUDACITY
         return false;
      }
   } else
   {
      // Nothing to recover, move along
      return true;
   }
}

////////////////////////////////////////////////////////////////////////////
/// Recording recovery handler

RecordingRecoveryHandler::RecordingRecoveryHandler(AudacityProject* proj)
{
   mProject = proj;
   mChannel = -1;
   mNumChannels = -1;
}

bool RecordingRecoveryHandler::HandleXMLTag(const wxChar *tag,
                                            const wxChar **attrs)
{
   if (wxStrcmp(tag, wxT("simpleblockfile")) == 0)
   {
      // Check if we have a valid channel and numchannels
      if (mChannel < 0 || mNumChannels < 0 || mChannel >= mNumChannels)
      {
         // This should only happen if there is a bug
         wxASSERT(false);
         return false;
      }

      // We need to find the track and sequence where the blockfile belongs
      WaveTrackArray tracks = mProject->GetTracks()->GetWaveTrackArray(false);
      int index = tracks.GetCount() - mNumChannels + mChannel;
      if (index < 0 || index >= (int)tracks.GetCount())
      {
         // This should only happen if there is a bug
         wxASSERT(false);
         return false;
      }
      WaveTrack* track = tracks.Item(index);
      Sequence* seq = track->GetLastOrCreateClip()->GetSequence();
      
      // Load the blockfile from the XML
      BlockFile* blockFile = NULL;
      DirManager* dirManager = mProject->GetDirManager();
      dirManager->SetLoadingFormat(seq->GetSampleFormat());
      dirManager->SetLoadingTarget(&blockFile);
      if (!dirManager->HandleXMLTag(tag, attrs) || !blockFile)
      {
         // This should only happen if there is a bug
         wxASSERT(false);
         return false;
      }

      seq->AppendBlockFile(blockFile);

   } else if (wxStrcmp(tag, wxT("recordingrecovery")) == 0)
   {
      // loop through attrs, which is a null-terminated list of
      // attribute-value pairs
      while(*attrs)
      {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            break;
         
         if (wxStrcmp(attr, wxT("channel")) == 0)
            mChannel = wxAtoi(value);
         if (wxStrcmp(attr, wxT("numchannels")) == 0)
            mNumChannels = wxAtoi(value);
      }
   }
   
   return true;
}

XMLTagHandler* RecordingRecoveryHandler::HandleXMLChild(const wxChar *tag)
{
   if (wxStrcmp(tag, wxT("simpleblockfile")) == 0)
      return this; // HandleXMLTag also handles <simpleblockfile>
   
   return NULL;
}
