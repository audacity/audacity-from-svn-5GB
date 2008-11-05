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
}

EffectContrast::~EffectContrast()
{
   return;
}

bool EffectContrast::PromptUser()
{
   ContrastDialog dlog(this, mParent);

   dlog.TransferDataToWindow();
   dlog.CentreOnParent();
   dlog.ShowModal();

   return false;
}

float EffectContrast::GetDB()
{
   float rms = float(0.0);

   TrackListIterator iter(mWaveTracks);
   Track *t = iter.First();
   if(mT0 >= mT1)
   {  // do something 'reasonable' in this case
      mT0 = (mT0 - mT1)/2 - 0.25;
      mT1 = mT0 + 0.5;
   }
   if(mT0 < t->GetStartTime())
      mT0 = t->GetStartTime();
   if(mT1 > t->GetEndTime())
      mT1 = t->GetEndTime();
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

   gPrefs->Read(wxT("/Contrast/startTimeF"), &startTimeF, 0.0);
   gPrefs->Read(wxT("/Contrast/endTimeF"), &endTimeF, 0.0);
   gPrefs->Read(wxT("/Contrast/startTimeB"), &startTimeB, 0.0);
   gPrefs->Read(wxT("/Contrast/endTimeB"), &endTimeB, 0.0);

   m_pEffect->SetStartTime(startTimeF);
   m_pEffect->SetEndTime(endTimeF);
   foregrounddB = m_pEffect->GetDB();
   m_pEffect->SetStartTime(startTimeB);
   m_pEffect->SetEndTime(endTimeB);
   backgrounddB = m_pEffect->GetDB();
   Init();
}

void ContrastDialog::OnGetForegroundDB( wxCommandEvent &event )
{
   m_pEffect->SetStartTime(startTimeF);
   m_pEffect->SetEndTime(endTimeF);
   foregrounddB = m_pEffect->GetDB();
   mForegroundRMSText->SetLabel(wxString::Format(_("%.1f dB"), foregrounddB));
   m_pButton_GetForeground->Enable(false);
   results();
}

void ContrastDialog::OnGetBackgroundDB( wxCommandEvent &event )
{
   m_pEffect->SetStartTime(startTimeB);
   m_pEffect->SetEndTime(endTimeB);
   backgrounddB = m_pEffect->GetDB();
   mBackgroundRMSText->SetLabel(wxString::Format(_("%.1f dB"), backgrounddB));
   m_pButton_GetBackground->Enable(false);
   results();
}

void ContrastDialog::OnGetURL(wxCommandEvent &event)
{
   wxString page = wxT("http://www.w3.org/TR/WCAG20/");  // yet to determine where this should point
   ::OpenInDefaultBrowser(page);
}

void ContrastDialog::OnOK(wxCommandEvent &event)
{
   gPrefs->Write(wxT("/Contrast/startTimeF"), startTimeF);
   gPrefs->Write(wxT("/Contrast/endTimeF"), endTimeF);
   gPrefs->Write(wxT("/Contrast/startTimeB"), startTimeB);
   gPrefs->Write(wxT("/Contrast/endTimeB"), endTimeB);

   EndModal(0);
}

void ContrastDialog::PopulateOrExchange(ShuttleGui & S)
{
   wxTextValidator vld(wxFILTER_NUMERIC);
   wxString number;
/*   wxString step1Label;
   wxString step1Prompt;
   wxString step2Label;
   wxString step2Prompt;

   step1Label = _("Step 1");
   step1Prompt = _("Select the dialogue (foreground) then click Measure Foreground:");
   step2Label = _("Step 2");
   step2Prompt = _("Select the background noise to measure, then click Measure Background:");*/

   S.StartHorizontalLay(wxCENTER, false);
   {
      S.AddTitle(_("Contrast Analyzer"));
   }
   S.EndHorizontalLay();
   
/*   S.StartStatic(step1Label);
   {
      S.AddVariableText(step1Prompt);
      m_pButton_GetBackground = S.Id(ID_BUTTON_GETFOREGROUND).
         AddButton(_("Measure Foreground"));
   }
   S.EndStatic();

   S.StartStatic(step2Label);
   {
      S.AddVariableText(step2Prompt);
      m_pButton_GetForeground = S.Id(ID_BUTTON_GETBACKGROUND).
         AddButton(_("Measure Background"));
   }
   S.EndStatic();*/

   S.StartStatic( _("Parameters") );
   {
      S.StartMultiColumn(5, wxEXPAND);
      {

         // Headings
         S.AddFixedText(_(" "), false);
         S.AddFixedText(_("Start"), false);
         S.AddFixedText(_("End"), false);
         S.AddFixedText(_(""), false); // spacer
         S.AddFixedText(_("Volume"), false);

         //Foreground
         S.AddFixedText(_("Foreground:"), false);
         S.StartMultiColumn(2, wxCENTER);
         {
            mForegroundStartText = S.Id(ID_FOREGROUNDSTART_TEXT).AddTextBox(_(""), wxT(""), 12);
            mForegroundStartText->SetValidator(vld);
            number = wxString::Format(wxT("%.2f"), startTimeF);
            mForegroundStartText->ChangeValue(number);
         }
         S.EndMultiColumn();
         S.StartMultiColumn(3, wxCENTER);
         {
            mForegroundEndText = S.Id(ID_FOREGROUNDEND_TEXT).AddTextBox(_(""), wxT(""), 12);
            mForegroundEndText->SetValidator(vld);
            number = wxString::Format(wxT("%.2f"), endTimeF);
            mForegroundEndText->ChangeValue(number);
            S.AddFixedText(_("seconds"), false);
         }
         S.EndMultiColumn();
         m_pButton_GetForeground = S.Id(ID_BUTTON_GETFOREGROUND).AddButton(_("Measure"));
         m_pButton_GetForeground->Enable(false);   // Disabled as we do the measurement as we put up the dialog
         mForegroundRMSText=S.Id(ID_FOREGROUNDDB_TEXT).AddVariableText(wxString::Format(wxT("%.1f dB"), foregrounddB));

         //Background
         S.AddFixedText(_("Background:"));
         S.StartMultiColumn(2, wxCENTER);
         {
            mBackgroundStartText = S.Id(ID_BACKGROUNDSTART_TEXT).AddTextBox(_(""), wxT(""), 12);
            mBackgroundStartText->SetValidator(vld);
            number = wxString::Format(wxT("%.2f"), startTimeB);
            mBackgroundStartText->ChangeValue(number);
         }
         S.EndMultiColumn();
         S.StartMultiColumn(3, wxCENTER);
         {
            mBackgroundEndText = S.Id(ID_BACKGROUNDEND_TEXT).AddTextBox(_(""), wxT(""), 12);
            mBackgroundEndText->SetValidator(vld);
            number = wxString::Format(wxT("%.2f"), endTimeB);
            mBackgroundEndText->ChangeValue(number);
            S.AddFixedText(_("seconds"));
         }
         S.EndMultiColumn();
         m_pButton_GetBackground = S.Id(ID_BUTTON_GETBACKGROUND).AddButton(_("Measure"));
         m_pButton_GetBackground->Enable(false);
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
      m_pButton_GetURL = S.Id(ID_BUTTON_GETURL).AddButton(_("WCAG Information on web"));
   }
   S.EndStatic();
}

void ContrastDialog::OnForegroundStartText(wxCommandEvent & event)
{
   wxString val = mForegroundStartText->GetValue();
   val.ToDouble(&startTimeF);
   m_pButton_GetForeground->Enable(true);
}

void ContrastDialog::OnForegroundEndText(wxCommandEvent & event)
{
   wxString val = mForegroundEndText->GetValue();
   val.ToDouble(&endTimeF);
   m_pButton_GetForeground->Enable(true);
}

void ContrastDialog::OnBackgroundStartText(wxCommandEvent & event)
{
   wxString val = mBackgroundStartText->GetValue();
   val.ToDouble(&startTimeB);
   m_pButton_GetBackground->Enable(true);
}

void ContrastDialog::OnBackgroundEndText(wxCommandEvent & event)
{
   wxString val = mBackgroundEndText->GetValue();
   val.ToDouble(&endTimeB);
   m_pButton_GetBackground->Enable(true);
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

