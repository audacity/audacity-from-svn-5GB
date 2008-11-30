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
#include "FileDialog.h"

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
#include <wx/datetime.h>
#include <wx/dcmemory.h>
#include <wx/file.h>
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
         wxMessageDialog m(NULL, _("Foreground times selected.\nNow select background and use Ctrl+Shift+T or menu again."), _("Contrast Tool Foreground"), wxOK);
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
      dlog.mForegroundStartT->SetTimeValue(mStartTimeF);
      dlog.mForegroundEndT->SetTimeValue(mEndTimeF);
      dlog.mBackgroundStartT->SetTimeValue(mStartTimeB);
      dlog.mBackgroundEndT->SetTimeValue(mEndTimeB);

      wxCommandEvent dummyEvt;
      dlog.OnGetForegroundDB(dummyEvt);
      dlog.OnGetBackgroundDB(dummyEvt);

      dlog.CentreOnParent();
      dlog.ShowModal();

      // Copy parameters from dialog back into effect
      mStartTimeF = dlog.mForegroundStartT->GetTimeValue();
      mEndTimeF = dlog.mForegroundEndT->GetTimeValue();
      mStartTimeB = dlog.mBackgroundStartT->GetTimeValue();
      mEndTimeB = dlog.mBackgroundEndT->GetTimeValue();
   }

   return false;
}

float EffectContrast::GetDB()
{
   float rms = float(0.0);

   TrackListIterator iter(mWaveTracks);
   Track *t = iter.First();
   if(mT0 > mT1)
   {
      wxMessageDialog m(NULL, _("Start time after after end time!\nPlease enter reasonable times."), _("Error"), wxOK);
      m.ShowModal();
      return 1234.0;
   }
   if(mT0 < t->GetStartTime())
      mT0 = t->GetStartTime();
   if(mT1 > t->GetEndTime())
      mT1 = t->GetEndTime();
   if(mT0 > mT1)
   {
      wxMessageDialog m(NULL, _("Times are not reasonable!\nPlease enter reasonable times."), _("Error"), wxOK);
      m.ShowModal();
      return 1234.0;
   }
   if(mT0 == mT1)
      return 1234.0;
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
   ID_BUTTON_EXPORT,
   ID_BUTTON_RESET,
   ID_FOREGROUNDSTART_T,
   ID_FOREGROUNDEND_T,
   ID_FOREGROUNDDB_TEXT,
   ID_BACKGROUNDSTART_T,
   ID_BACKGROUNDEND_T,
   ID_BACKGROUNDDB_TEXT,
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
   EVT_BUTTON(ID_BUTTON_EXPORT, ContrastDialog::OnExport)
   EVT_BUTTON(ID_BUTTON_RESET, ContrastDialog::OnReset)
   EVT_COMMAND(wxID_ANY, EVT_TIMETEXTCTRL_UPDATED, ContrastDialog::OnTimeCtrlUpdate)

   EVT_COMMAND(ID_FOREGROUNDSTART_T, wxEVT_COMMAND_TEXT_UPDATED, ContrastDialog::OnForegroundStartT)
   EVT_COMMAND(ID_FOREGROUNDEND_T, wxEVT_COMMAND_TEXT_UPDATED, ContrastDialog::OnForegroundEndT)
   EVT_COMMAND(ID_BACKGROUNDSTART_T, wxEVT_COMMAND_TEXT_UPDATED, ContrastDialog::OnBackgroundStartT)
   EVT_COMMAND(ID_BACKGROUNDEND_T, wxEVT_COMMAND_TEXT_UPDATED, ContrastDialog::OnBackgroundEndT)
END_EVENT_TABLE()

ContrastDialog::ContrastDialog(EffectContrast * effect, 
                                       wxWindow *parent) :
   EffectDialog( parent, _("WCAG2 Contrast Analyzer"), ANALYZE_EFFECT)
{
   m_pEffect = effect;
   
   // NULL out the control members until the controls are created.
   m_pButton_GetForeground = NULL;
   m_pButton_GetBackground = NULL;
   mForegroundStartT = NULL;
   mForegroundEndT = NULL;
   mBackgroundStartT = NULL;
   mBackgroundEndT = NULL;

   mT0orig = m_pEffect->GetStartTime();  // keep copy of selection times
   mT1orig = m_pEffect->GetEndTime();

   Init();
}

void ContrastDialog::OnGetForegroundDB( wxCommandEvent &event )
{
   m_pEffect->SetStartTime(mForegroundStartT->GetTimeValue());
   m_pEffect->SetEndTime(mForegroundEndT->GetTimeValue());
   foregrounddB = m_pEffect->GetDB();
   if(foregrounddB == 1234.0) // magic number, is there a better way?
      mForegroundRMSText->SetLabel(wxString::Format(_(" ")));
   else
      mForegroundRMSText->SetLabel(wxString::Format(_("%.1f dB"), foregrounddB));
   m_pButton_GetForeground->Enable(false);
   m_pButton_GetForeground->SetLabel(_("Measured"));
   m_pButton_UseCurrentF->SetFocus();
   results();
}

void ContrastDialog::OnGetBackgroundDB( wxCommandEvent &event )
{
   m_pEffect->SetStartTime(mBackgroundStartT->GetTimeValue());
   m_pEffect->SetEndTime(mBackgroundEndT->GetTimeValue());
   backgrounddB = m_pEffect->GetDB();
   if(backgrounddB == 1234.0) // magic number, is there a better way?
      mBackgroundRMSText->SetLabel(wxString::Format(_(" ")));
   else
      mBackgroundRMSText->SetLabel(wxString::Format(_("%.1f dB"), backgrounddB));
   m_pButton_GetBackground->Enable(false);
   m_pButton_GetBackground->SetLabel(_("Measured"));
   m_pButton_UseCurrentB->SetFocus();
   results();
}

void ContrastDialog::OnGetURL(wxCommandEvent &event)
{
   wxString page = wxT("http://www.eramp.com/WCAG_2_audio_contrast_tool_help.htm");
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
         S.AddFixedText(wxT(""));   // spacer
         S.AddFixedText(_("Start"));
         S.AddFixedText(_("End"));
         S.AddFixedText(wxT(""));   // spacer
         S.AddFixedText(wxT(""));   // spacer
         S.AddFixedText(_("Volume"));

         //Foreground
         S.AddFixedText(_("Foreground:"), false);
         if (mForegroundStartT == NULL)
         {
            AudacityProject *p = GetActiveProject();
            mForegroundStartT = new
            TimeTextCtrl(this,
                         ID_FOREGROUNDSTART_T,
                         wxT(""),
                         0.0,
                         m_pEffect->mProjectRate,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
            mForegroundStartT->SetFormatString(p->GetSelectionBar()->mLeftTime->GetFormatString());
            mForegroundStartT->EnableMenu();
         }
         S.AddWindow(mForegroundStartT);

         if (mForegroundEndT == NULL)
         {
            AudacityProject *p = GetActiveProject();
            mForegroundEndT = new
            TimeTextCtrl(this,
                         ID_FOREGROUNDEND_T,
                         wxT(""),
                         0.0,
                         m_pEffect->mProjectRate,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
            mForegroundEndT->SetFormatString(p->GetSelectionBar()->mLeftTime->GetFormatString());
            mForegroundEndT->EnableMenu();
         }
         S.AddWindow(mForegroundEndT);

         m_pButton_GetForeground = S.Id(ID_BUTTON_GETFOREGROUND).AddButton(_("Measured"));
         m_pButton_GetForeground->Enable(false);   // Disabled as we do the measurement as we put up the dialog
         m_pButton_UseCurrentF = S.Id(ID_BUTTON_USECURRENTF).AddButton(_("Use selection"));
         mForegroundRMSText=S.Id(ID_FOREGROUNDDB_TEXT).AddVariableText(wxString::Format(wxT("%.1f dB"), foregrounddB));

         //Background
         S.AddFixedText(_("Background:"));
         if (mBackgroundStartT == NULL)
         {
            AudacityProject *p = GetActiveProject();
            mBackgroundStartT = new
            TimeTextCtrl(this,
                         ID_BACKGROUNDSTART_T,
                         wxT(""),
                         0.0,
                         m_pEffect->mProjectRate,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
            mBackgroundStartT->SetFormatString(p->GetSelectionBar()->mLeftTime->GetFormatString());
            mBackgroundStartT->EnableMenu();
         }
         S.AddWindow(mBackgroundStartT);

         if (mBackgroundEndT == NULL)
         {
            AudacityProject *p = GetActiveProject();
            mBackgroundEndT = new
            TimeTextCtrl(this,
                         ID_BACKGROUNDEND_T,
                         wxT(""),
                         0.0,
                         m_pEffect->mProjectRate,
                         wxDefaultPosition,
                         wxDefaultSize,
                         true);
            mBackgroundEndT->SetFormatString(p->GetSelectionBar()->mLeftTime->GetFormatString());
            mBackgroundEndT->EnableMenu();
         }
         S.AddWindow(mBackgroundEndT);

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
      S.StartMultiColumn(3, wxCENTER);
      {
         S.AddFixedText(_("Contrast Result:"));
         mPassFailText = S.Id(ID_RESULTS_TEXT).AddVariableText(wxString::Format(wxT("%s"), _("Fail")));
         m_pButton_Export = S.Id(ID_BUTTON_EXPORT).AddButton(_("Export")); //right justify
         S.AddFixedText(_("Difference:"));
         mDiffText = S.Id(ID_RESULTSDB_TEXT).AddVariableText(wxString::Format(wxT("%.1f  Average RMS"), foregrounddB - backgrounddB));
         m_pButton_Reset = S.Id(ID_BUTTON_RESET).AddButton(_("Reset")); //right justify
      }
      S.EndMultiColumn();
   }
   S.EndStatic();
   results();

   //Information
   S.StartStatic( _("Information") );
   {
      S.AddFixedText(_("Contrast, for analysing rms volume differences between selections, in dBs."));
      m_pButton_GetURL = S.Id(ID_BUTTON_GETURL).AddButton(_("WCAG contrast tool help on the web"));
   }
   S.EndStatic();
   Fit();
}

void ContrastDialog::OnForegroundStartT(wxCommandEvent & event)
{
   m_pButton_GetForeground->Enable(true);
   m_pButton_GetForeground->SetLabel(_("Measure"));
}

void ContrastDialog::OnForegroundEndT(wxCommandEvent & event)
{
   m_pButton_GetForeground->Enable(true);
   m_pButton_GetForeground->SetLabel(_("Measure"));
}

void ContrastDialog::OnBackgroundStartT(wxCommandEvent & event)
{
   m_pButton_GetBackground->Enable(true);
   m_pButton_GetBackground->SetLabel(_("Measure"));
}

void ContrastDialog::OnBackgroundEndT(wxCommandEvent & event)
{
   m_pButton_GetBackground->Enable(true);
   m_pButton_GetBackground->SetLabel(_("Measure"));
}

void ContrastDialog::OnUseSelectionF(wxCommandEvent & event)
{
   mForegroundStartT->SetTimeValue(mT0orig);
   mForegroundEndT->SetTimeValue(mT1orig);

   m_pEffect->SetStartTime(mT0orig);
   m_pEffect->SetEndTime(mT1orig);
   OnGetForegroundDB(event);
   results();
}

void ContrastDialog::OnUseSelectionB(wxCommandEvent & event)
{
   mBackgroundStartT->SetTimeValue(mT0orig);
   mBackgroundEndT->SetTimeValue(mT1orig);

   m_pEffect->SetStartTime(mT0orig);
   m_pEffect->SetEndTime(mT1orig);
   OnGetBackgroundDB(event);
   results();
}

void ContrastDialog::results()
{
   if( (foregrounddB != 1234.0) && (backgrounddB != 1234.0) )
   {
      if(foregrounddB - backgrounddB > 20)
         mPassFailText->SetLabel(_("WCAG2 Pass"));
      else
         mPassFailText->SetLabel(_("WCAG2 Fail"));
      mDiffText->SetLabel(wxString::Format(wxT("%.1f dB Average RMS"), foregrounddB - backgrounddB));
   }
   else
   {
      mPassFailText->SetLabel(wxT(""));
      mDiffText->SetLabel(wxT(""));
   }
   Fit();
}

void ContrastDialog::OnExport(wxCommandEvent & event)
{
   AudacityProject * project = GetActiveProject();
   wxString fName = _("contrast.txt");

   fName = FileSelector(_("Export Contrast Result As:"),
                        NULL, fName, wxT("txt"), wxT("*.txt"), wxFD_SAVE | wxRESIZE_BORDER, this);

   if (fName == wxT(""))
      return;

   wxTextFile f(fName);
#ifdef __WXMAC__
   wxFile *temp = new wxFile();
   temp->Create(fName);
   delete temp;
#else
   f.Create();
#endif
   f.Open();
   if (!f.IsOpened()) {
      wxMessageBox(_("Couldn't write to file: ") + fName);
      return;
   }

   f.AddLine(wxT("==================================="));
   f.AddLine(_("WCAG 2.0 Success Criteria 1.4.7 Contrast Results\r\n"));
   f.AddLine(wxString::Format(wxT("Filename = %s."), project->GetFileName() ));
   f.AddLine(wxT("\r\nForeground"));
   f.AddLine(wxString::Format(wxT("Time started = %f seconds."), (float)mForegroundStartT->GetTimeValue() ));
   f.AddLine(wxString::Format(wxT("Time ended = %f seconds."), (float)mForegroundEndT->GetTimeValue() ));
   if(foregrounddB != 1234.0)
      f.AddLine(wxString::Format(wxT("Average RMS = %.1f dB."), foregrounddB ));
   else
      f.AddLine(wxString::Format(wxT("Average RMS =  dB.")));

   f.AddLine(wxT("\r\nBackground"));
   f.AddLine(wxString::Format(wxT("Time started = %f seconds."), (float)mBackgroundStartT->GetTimeValue() ));
   f.AddLine(wxString::Format(wxT("Time ended = %f seconds."), (float)mBackgroundEndT->GetTimeValue() ));
   if(backgrounddB != 1234.0)
      f.AddLine(wxString::Format(wxT("Average RMS = %.1f dB."), backgrounddB ));
   else
      f.AddLine(wxString::Format(wxT("Average RMS =  dB.")));
   f.AddLine(wxT("\r\nResults"));
   float diff = foregrounddB - backgrounddB;
   f.AddLine(wxString::Format(wxT("Difference = %f Average RMS dBs."), diff ));
   if( diff > 20. )
      f.AddLine(_("Pass Success Criteria 1.4.7 of WCAG 2.0"));
   else
      f.AddLine(_("Fail Success Criteria 1.4.7 of WCAG 2.0"));

   f.AddLine(wxT("\r\nData gathered"));
   wxString sNow;
   wxDateTime now = wxDateTime::Now();
   int year = now.GetYear();
   wxDateTime::Month month = now.GetMonth();
   wxString monthName = now.GetMonthName(month);
   int dom = now.GetDay();
   int hour = now.GetHour();
   int minute = now.GetMinute();
   int second = now.GetSecond();
   sNow = wxString::Format(wxT("%d %s %02d %02dh %02dm %02ds"), 
        dom, monthName.c_str(), year, hour, minute, second);
   f.AddLine(sNow);

   f.AddLine(wxT("==================================="));

#ifdef __WXMAC__
   f.Write(wxTextFileType_Mac);
#else
   f.Write();
#endif
   f.Close();
}

void ContrastDialog::OnTimeCtrlUpdate(wxCommandEvent & event) {
   Fit();
}


void ContrastDialog::OnReset(wxCommandEvent & event)
{
   m_pEffect->bFGset = false;
   m_pEffect->bBGset = false;

   mForegroundStartT->SetTimeValue(0.0);
   mForegroundEndT->SetTimeValue(0.0);
   mBackgroundStartT->SetTimeValue(0.0);
   mBackgroundEndT->SetTimeValue(0.0);

   wxCommandEvent dummyEvt;
   OnGetForegroundDB(event);
   OnGetBackgroundDB(event);
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

