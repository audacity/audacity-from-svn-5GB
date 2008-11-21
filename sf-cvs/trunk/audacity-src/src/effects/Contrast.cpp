/**********************************************************************

  Audacity: A Digital Audio Editor

  Contrast.cpp

*******************************************************************//**

\class EffectContrast
\brief An effect to measure the difference between two sections of audio

*//****************************************************************//**

\class ContrastDialog
\brief Dialog used with EffectContrast

*//*******************************************************************/

#include "../Audacity.h"

#include "Contrast.h"

#include "../Envelope.h"
#include "../FFT.h"
#include "../WaveTrack.h"
#include "../Prefs.h"
#include "../Project.h"
#include "../FileNames.h"
#include "../widgets/LinkingHtmlWindow.h"

#include <math.h>

#if defined(__WXMSW__) && !defined(__CYGWIN__)
#include <float.h>
#define finite(x) _finite(x)
#endif

// all these headers may not be needed
#include <wx/file.h>
#include <wx/ffile.h>
#include <wx/bitmap.h>
#include <wx/brush.h>
#include <wx/button.h>
#include <wx/dcmemory.h>
#include <wx/image.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/stattext.h>
#include <wx/textctrl.h>
#include <wx/valtext.h>

#include "../AudacityApp.h"
#include "../PlatformCompatibility.h"

EffectContrast::EffectContrast()
{
   bFGset = false;
   bBGset = false;
}

EffectContrast::~EffectContrast()
{
   return;
}

bool EffectContrast::PromptUser()
{
   if( !bFGset )
   {
      SaveTimes(true, mT0, mT1);
      bFGset = true;
      if( !bBGset )
      {
         wxMessageDialog m(NULL, _("Foreground times selected.\nNow select background and use Ctrl+Alt+C or menu again."), _("Contrast Tool Foreground"), wxOK);
         m.ShowModal();
      }
   }
   else
   {
      if( !bBGset )
      {
         SaveTimes(false, mT0, mT1);
         bBGset = true;
      }
   }

   if( bFGset && bBGset )
   {
      ContrastDialog dlog(this, mParent);

      // Copy parameters into dialog from effect
      dlog.startTimeF = mStartTimeF;
      dlog.endTimeF = mEndTimeF;
      dlog.startTimeB = mStartTimeB;
      dlog.endTimeB = mEndTimeB;

      wxCommandEvent dummyEvt;
      dlog.OnGetForegroundDB(dummyEvt);
      dlog.OnGetBackgroundDB(dummyEvt);

      dlog.CentreOnParent();
      dlog.ShowModal();

      // Copy parameters from dialog back into effect
      mStartTimeF = dlog.startTimeF;
      mEndTimeF = dlog.endTimeF;
      mStartTimeB = dlog.startTimeB;
      mEndTimeB = dlog.endTimeB;
   }

   return false;
}

float EffectContrast::GetDB()
{
   float rms = float(0.0);

   TrackListIterator iter(mWaveTracks);
   Track *t = iter.First();
   if(mT0 >= mT1)
   {
      wxMessageDialog m(NULL, _("Start time after after end time!\nPlease enter reasonable times."), _("Error"), wxOK);
      m.ShowModal();
      return 1234.0;
   }
   if(mT0 < t->GetStartTime())
      mT0 = t->GetStartTime();
   if(mT1 > t->GetEndTime())
      mT1 = t->GetEndTime();
   if(mT0 >= mT1)
   {
      wxMessageDialog m(NULL, _("Times are not reasonable!\nPlease enter reasonable times."), _("Error"), wxOK);
      m.ShowModal();
      return 1234.0;
   }
   while(t) {  // this isn't quite right.  What to do if more than one track selected?
      ((WaveTrack *)t)->GetRMS(&rms, mT0, mT1);
      t = iter.Next();
   }
   return 20.0*log10(fabs(rms));
}

double EffectContrast::GetStartTime()
{
   return(mT0);
}

void EffectContrast::SetStartTime(double t)
{
   mT0 = t;
}

double EffectContrast::GetEndTime()
{
   return(mT1);
}

void EffectContrast::SetEndTime(double t)
{
   mT1 = t;
}

void EffectContrast::SaveTimes(bool isForeground, double start, double end)
{
   if(isForeground)
   {
      mStartTimeF = start;
      mEndTimeF = end;
   }
   else
   {
      mStartTimeB = start;
      mEndTimeB = end;
   }
   return;
}

bool EffectContrast::Process()
{
   wxMessageBox(_("Hello.\n"));  // you should never see this!
   return true;
}

bool EffectContrast::CheckWhetherSkipEffect()
{
   return true;
}

// WDR: class implementations

//----------------------------------------------------------------------------
// ContrastDialog
//----------------------------------------------------------------------------

// WDR: event table for ContrastDialog

enum {
   ID_BUTTON_GETFOREGROUND = 10001,
   ID_BUTTON_GETBACKGROUND,
   ID_BUTTON_USECURRENTF,
   ID_BUTTON_USECURRENTB,
   ID_BUTTON_GETURL,
   ID_BACKGROUNDSTART_TEXT,
   ID_BACKGROUNDEND_TEXT,
   ID_BACKGROUNDDB_TEXT,
   ID_FOREGROUNDSTART_TEXT,
   ID_FOREGROUNDEND_TEXT,
   ID_FOREGROUNDDB_TEXT,
   ID_RESULTS_TEXT,
   ID_RESULTSDB_TEXT
};

BEGIN_EVENT_TABLE(ContrastDialog,wxDialog)
   EVT_BUTTON(wxID_OK, ContrastDialog::OnOK)
   EVT_BUTTON(ID_BUTTON_GETFOREGROUND, ContrastDialog::OnGetForegroundDB)
   EVT_BUTTON(ID_BUTTON_GETBACKGROUND, ContrastDialog::OnGetBackgroundDB)
   EVT_BUTTON(ID_BUTTON_USECURRENTF, ContrastDialog::OnUseSelectionF)
   EVT_BUTTON(ID_BUTTON_USECURRENTB, ContrastDialog::OnUseSelectionB)
   EVT_BUTTON(ID_BUTTON_GETURL, ContrastDialog::OnGetURL)
   EVT_TEXT(ID_FOREGROUNDSTART_TEXT, ContrastDialog::OnForegroundStartText)
   EVT_TEXT(ID_FOREGROUNDEND_TEXT, ContrastDialog::OnForegroundEndText)
   EVT_TEXT(ID_BACKGROUNDSTART_TEXT, ContrastDialog::OnBackgroundStartText)
   EVT_TEXT(ID_BACKGROUNDEND_TEXT, ContrastDialog::OnBackgroundEndText)
END_EVENT_TABLE()

ContrastDialog::ContrastDialog(EffectContrast * effect, 
                                       wxWindow *parent) :
   EffectDialog( parent, _("WCAG2 Contrast Analyzer"), ANALYZE_EFFECT)
{
   m_pEffect = effect;
   
   // NULL out the control members until the controls are created.
   m_pButton_GetForeground = NULL;
   m_pButton_GetBackground = NULL;

   mT0orig = m_pEffect->GetStartTime();  // keep copy of selection times
   mT1orig = m_pEffect->GetEndTime();

   Init();
}

void ContrastDialog::OnGetForegroundDB( wxCommandEvent &event )
{
   m_pEffect->SetStartTime(startTimeF);
   m_pEffect->SetEndTime(endTimeF);
   foregrounddB = m_pEffect->GetDB();
   if(foregrounddB == 1234.0) // magic number, is there a better way?
      mForegroundRMSText->SetLabel(wxString::Format(_(" ")));
   else
      mForegroundRMSText->SetLabel(wxString::Format(_("%.1f dB"), foregrounddB));
   m_pButton_GetForeground->Enable(false);
   m_pButton_GetForeground->SetLabel(_("Measured"));
   results();
}

void ContrastDialog::OnGetBackgroundDB( wxCommandEvent &event )
{
   m_pEffect->SetStartTime(startTimeB);
   m_pEffect->SetEndTime(endTimeB);
   backgrounddB = m_pEffect->GetDB();
   if(backgrounddB == 1234.0) // magic number, is there a better way?
      mBackgroundRMSText->SetLabel(wxString::Format(_(" ")));
   else
      mBackgroundRMSText->SetLabel(wxString::Format(_("%.1f dB"), backgrounddB));
   m_pButton_GetBackground->Enable(false);
   m_pButton_GetBackground->SetLabel(_("Measured"));
   results();
}

void ContrastDialog::OnGetURL(wxCommandEvent &event)
{
   wxString page = wxT("http://www.w3.org/TR/WCAG20/");  // yet to determine where this should point
   ::OpenInDefaultBrowser(page);
}

void ContrastDialog::OnOK(wxCommandEvent &event)
{
   EndModal(0);
}

void ContrastDialog::PopulateOrExchange(ShuttleGui & S)
{
   wxTextValidator vld(wxFILTER_NUMERIC);
   wxString number;

   S.StartHorizontalLay(wxCENTER, false);
   {
      S.AddTitle(_("Contrast Analyzer"));
   }
   S.EndHorizontalLay();
   
   S.StartStatic( _("Parameters") );
   {
      S.StartMultiColumn(6, wxEXPAND);
      {

         // Headings
         S.AddFixedText(wxT(""), false); // spacer
         S.AddFixedText(_("Start"), false);
         S.AddFixedText(_("End"), false);
         S.AddFixedText(wxT(""), false); // spacer
         S.AddFixedText(wxT(""), false); // spacer
         S.AddFixedText(_("Volume"), false);

         //Foreground
         S.AddFixedText(_("Foreground:"), false);
         S.StartMultiColumn(2, wxCENTER);
         {
            mForegroundStartText = S.Id(ID_FOREGROUNDSTART_TEXT).AddTextBox(wxT(""), wxT(""), 12);
            mForegroundStartText->SetValidator(vld);
            number = wxString::Format(wxT("%.2f"), startTimeF);
            mForegroundStartText->ChangeValue(number);
         }
         S.EndMultiColumn();
         S.StartMultiColumn(3, wxCENTER);
         {
            mForegroundEndText = S.Id(ID_FOREGROUNDEND_TEXT).AddTextBox(wxT(""), wxT(""), 12);
            mForegroundEndText->SetValidator(vld);
            number = wxString::Format(wxT("%.2f"), endTimeF);
            mForegroundEndText->ChangeValue(number);
            S.AddFixedText(_("seconds"), false);
         }
         S.EndMultiColumn();
         m_pButton_GetForeground = S.Id(ID_BUTTON_GETFOREGROUND).AddButton(_("Measured"));
         m_pButton_GetForeground->Enable(false);   // Disabled as we do the measurement as we put up the dialog
         m_pButton_UseCurrentF = S.Id(ID_BUTTON_USECURRENTF).AddButton(_("Use selection"));
         mForegroundRMSText=S.Id(ID_FOREGROUNDDB_TEXT).AddVariableText(wxString::Format(wxT("%.1f dB"), foregrounddB));

         //Background
         S.AddFixedText(_("Background:"));
         S.StartMultiColumn(2, wxCENTER);
         {
            mBackgroundStartText = S.Id(ID_BACKGROUNDSTART_TEXT).AddTextBox(wxT(""), wxT(""), 12);
            mBackgroundStartText->SetValidator(vld);
            number = wxString::Format(wxT("%.2f"), startTimeB);
            mBackgroundStartText->ChangeValue(number);
         }
         S.EndMultiColumn();
         S.StartMultiColumn(3, wxCENTER);
         {
            mBackgroundEndText = S.Id(ID_BACKGROUNDEND_TEXT).AddTextBox(wxT(""), wxT(""), 12);
            mBackgroundEndText->SetValidator(vld);
            number = wxString::Format(wxT("%.2f"), endTimeB);
            mBackgroundEndText->ChangeValue(number);
            S.AddFixedText(_("seconds"));
         }
         S.EndMultiColumn();
         m_pButton_GetBackground = S.Id(ID_BUTTON_GETBACKGROUND).AddButton(_("Measured"));
         m_pButton_GetBackground->Enable(false);
         m_pButton_UseCurrentB = S.Id(ID_BUTTON_USECURRENTB).AddButton(_("Use selection"));
         mBackgroundRMSText=S.Id(ID_BACKGROUNDDB_TEXT).AddVariableText(wxString::Format(wxT("%.1f dB"), backgrounddB));
      }
      S.EndMultiColumn();
   }
   S.EndStatic();

   //Result
   S.StartStatic( _("Result") );
   {
      S.StartMultiColumn(2, wxCENTER);
      {
         S.AddFixedText(_("Contrast Result:"));
         mPassFailText = S.Id(ID_RESULTS_TEXT).AddVariableText(wxString::Format(wxT("%s"), _("Fail")));
         S.AddFixedText(_("Difference:"));
         mDiffText = S.Id(ID_RESULTSDB_TEXT).AddVariableText(wxString::Format(wxT("%.1f  Average RMS"), foregrounddB - backgrounddB));
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
   results();

   //Information
   S.StartStatic( _("Information") );
   {
      S.AddFixedText(_("Before you use this tool,"));
      S.AddFixedText(_("   Ctrl+Alt+C sets the 'foreground' times."));
      S.AddFixedText(_("   Ctrl+Alt+C (again) sets the 'background' times and opens the tool."));
      S.AddFixedText(_("After you've used this tool,"));
      S.AddFixedText(_("   Select a region then use Ctrl+Alt+C to open it."));
      S.AddFixedText(_("   Click 'Use Selected' above to set the fore/background times."));
      S.AddFixedText(_("   Or enter the times manually and click 'Measure'."));
      m_pButton_GetURL = S.Id(ID_BUTTON_GETURL).AddButton(_("WCAG Information on web"));
   }
   S.EndStatic();
   Fit();
}

void ContrastDialog::OnForegroundStartText(wxCommandEvent & event)
{
   wxString val = mForegroundStartText->GetValue();
   val.ToDouble(&startTimeF);
   m_pButton_GetForeground->Enable(true);
   m_pButton_GetForeground->SetLabel(_("Measure"));
}

void ContrastDialog::OnForegroundEndText(wxCommandEvent & event)
{
   wxString val = mForegroundEndText->GetValue();
   val.ToDouble(&endTimeF);
   m_pButton_GetForeground->Enable(true);
   m_pButton_GetForeground->SetLabel(_("Measure"));
}

void ContrastDialog::OnBackgroundStartText(wxCommandEvent & event)
{
   wxString val = mBackgroundStartText->GetValue();
   val.ToDouble(&startTimeB);
   m_pButton_GetBackground->Enable(true);
   m_pButton_GetBackground->SetLabel(_("Measure"));
}

void ContrastDialog::OnBackgroundEndText(wxCommandEvent & event)
{
   wxString val = mBackgroundEndText->GetValue();
   val.ToDouble(&endTimeB);
   m_pButton_GetBackground->Enable(true);
   m_pButton_GetBackground->SetLabel(_("Measure"));
}

void ContrastDialog::OnUseSelectionF(wxCommandEvent & event)
{
   wxString number;

   startTimeF = mT0orig;
   endTimeF = mT1orig;
   number = wxString::Format(wxT("%.2f"), startTimeF);
   mForegroundStartText->ChangeValue(number);
   number = wxString::Format(wxT("%.2f"), endTimeF);
   mForegroundEndText->ChangeValue(number);

   m_pEffect->SetStartTime(startTimeB);
   m_pEffect->SetEndTime(endTimeB);
   OnGetForegroundDB(event);
   results();
}

void ContrastDialog::OnUseSelectionB(wxCommandEvent & event)
{
   wxString number;

   startTimeB = mT0orig;
   endTimeB = mT1orig;
   number = wxString::Format(wxT("%.2f"), startTimeB);
   mBackgroundStartText->ChangeValue(number);
   number = wxString::Format(wxT("%.2f"), endTimeB);
   mBackgroundEndText->ChangeValue(number);

   m_pEffect->SetStartTime(startTimeB);
   m_pEffect->SetEndTime(endTimeB);
   OnGetBackgroundDB(event);
   results();
}

void ContrastDialog::results()
{
   if(foregrounddB - backgrounddB > 20)
      mPassFailText->SetLabel(_("WCAG2 Pass"));
   else
      mPassFailText->SetLabel(_("WCAG2 Fail"));
   mDiffText->SetLabel(wxString::Format(wxT("%.1f dB Average RMS"), foregrounddB - backgrounddB));
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
// arch-tag: 685e0d8c-89eb-427b-8933-af606cf33c2b

