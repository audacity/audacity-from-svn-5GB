/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_EXPORT_MULTIPLE__
#define __AUDACITY_EXPORT_MULTIPLE__

#include <wx/dialog.h>
#include <wx/string.h>

#include "Export.h"
#include "../Track.h"

class wxButton;
class wxCheckBox;
class wxChoice;
class wxRadioButton;
class wxTextCtrl;

class AudacityProject;
class ShuttleGui;

class ExportMultiple : public wxDialog
{
public:

   ExportMultiple(AudacityProject *parent);
   virtual ~ExportMultiple();

   int ShowModal();

private:

   // Export
   void CanExport();
   bool DirOk();
   bool ExportMultipleByLabel(bool byName, wxString prefix);
   bool ExportMultipleByTrack(bool byName, wxString prefix);
   void MakeNameUnique(wxArrayString &otherNames, wxString &newName);
   bool DoExport(bool stereo,
                 wxString name,
                 bool selectedOnly,
                 double t0,
                 double t1,
                 int trackNumber);

   // Dialog
   void PopulateOrExchange(ShuttleGui& S); 
   void EnableControls();

   void OnFormat(wxCommandEvent& event);
   void OnOptions(wxCommandEvent& event);
   void OnCreate(wxCommandEvent& event);
   void OnChoose(wxCommandEvent& event);
   void OnLabel(wxCommandEvent& event);
   void OnFirst(wxCommandEvent& event);
   void OnFirstFileName(wxCommandEvent& event);
   void OnTrack(wxCommandEvent& event);
   void OnByName(wxCommandEvent& event);
   void OnByNumber(wxCommandEvent& event);
   void OnPrefix(wxCommandEvent& event);
   void OnCancel(wxCommandEvent& event);
   void OnExport(wxCommandEvent& event);

private:
   ExportTypeArray mTypes;
   AudacityProject *mProject;
   TrackList *mTracks;
   TrackListIterator mIterator;
   LabelTrack *mLabels;
   int mNumLabels;
   int mNumTracks;
   wxArrayPtrVoid mSelected;
   int mFormatIndex;
   bool mInitialized;

   wxChoice      *mFormat;
   wxButton      *mOptions;

   wxTextCtrl    *mDir;
   wxButton      *mCreate;
   wxButton      *mChoose;
   
   wxRadioButton *mLabel;
   wxStaticText  *mLabelLabel;

   wxCheckBox    *mFirst;
   wxStaticText  *mFirstFileLabel;
   wxTextCtrl    *mFirstFileName;

   wxRadioButton *mTrack;
   wxStaticText  *mTrackLabel;
   
   wxRadioButton *mByName;
   wxStaticText  *mByNameLabel;

   wxRadioButton *mByNumber;
   wxStaticText  *mByNumberLabel;

   wxStaticText  *mPrefixLabel;
   wxTextCtrl    *mPrefix;

   wxCheckBox    *mOverwrite;

   wxButton      *mCancel;
   wxButton      *mExport;

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
// arch-tag: d6904b91-a320-4194-8d60-caa9175b6bb4
