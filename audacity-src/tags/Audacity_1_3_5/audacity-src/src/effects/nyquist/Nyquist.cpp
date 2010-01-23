/**********************************************************************

  Audacity: A Digital Audio Editor

  Nyquist.cpp

  Dominic Mazzoni

******************************************************************//**

\class EffectNyquist
\brief An Effect that calls up a Nyquist (XLISP) plug in, i.e. many possible 
effects from this one class.

*//****************************************************************//**

\class NyquistInputDialog
\brief Dialog used with EffectNyquist

*//****************************************************************//**

\class NyquistOutputDialog
\brief Dialog used with EffectNyquist

*//****************************************************************//**

\class NyquistDialog
\brief Base class for NyquistInputDialog and NyquistOutputDialog.

*//****************************************************************//**

\class NyqControl
\brief A control on a NyquistDialog.

*//*******************************************************************/



#include "../../Audacity.h"

#include <math.h>
#include <locale.h>

#include <wx/defs.h>
#include <wx/log.h>
#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/textfile.h>
#include <wx/choice.h>

#include "../../AudacityApp.h"
#include "../../LabelTrack.h"
#include "../../Internat.h"
#include "../../ShuttleGui.h"

#include "Nyquist.h"

#ifndef nyx_returns_start_and_end_time
#error You need to update lib-src/libnyquist
#endif

#include <locale.h>

#include <wx/arrimpl.cpp>
WX_DEFINE_OBJARRAY(NyqControlArray);

wxString EffectNyquist::mXlispPath;

#define NYQ_CTRL_INT 0
#define NYQ_CTRL_REAL 1
#define NYQ_CTRL_STRING 2
#define NYQ_CTRL_CHOICE 3

extern "C" {
   extern void set_xlisp_path(const char *p);
}

#if defined(USE_NYQUIST) && defined(__WXMSW__) && !defined(__CYGWIN__)
// Nyquist needs a random function and non-Cygwin MSW doesn't provide one...
extern "C" {
   long random_seed = 1534781L;
   short random(short lo, short hi)
   {
      random_seed *= 13L;
      random_seed += 1874351L;
      return((short)(lo + (((hi + 1 - lo) * ((0x00ffff00 & random_seed) >> 8)) >> 16)));
   }
}
#endif


#define UNINITIALIZED_CONTROL ((double)99999999.99)

wxString EffectNyquist::UnQuote(wxString s)
{
   wxString out;
   int len = s.Length();
   
   if (len>=2 && s.GetChar(0)==wxT('\"') && s.GetChar(len-1)==wxT('\"'))
      return s.Mid(1, len-2);
   else
      return s;
}

double EffectNyquist::GetCtrlValue(wxString s)
{
   if (s == wxT("rate")) {
      TrackListIterator iter(mWaveTracks);
      return ((WaveTrack *)iter.First())->GetRate();
   }
   else {
      return Internat::CompatibleToDouble(s);
   }
}

void EffectNyquist::Parse(wxString line)
{
   wxArrayString tokens;

   int i;
   int len = line.Length();
   bool sl = false;
   bool q = false;
   wxString tok = wxT("");
   for(i=1; i<len; i++) {
      if (line.GetChar(i)==wxT('\\'))
         sl = true;
      else if (line.GetChar(i)==wxT('"'))
         q = !q;
      else {
         if (!q && !sl && line.GetChar(i)==wxT(' ') || line.GetChar(i)==wxT('\t')) {
            tokens.Add(tok);
            tok = wxT("");
         }
         else if (sl && line.GetChar(i)==wxT('n'))
            tok += wxT('\n');
         else
            tok += line.GetChar(i);
         
         sl = false;
      }
   }
   if (tok != wxT(""))
      tokens.Add(tok);

   len = tokens.GetCount();
   if (len < 1)
      return;

   if (len==2 && tokens[0]==wxT("nyquist") && tokens[1]==wxT("plug-in")) {
      mOK = true;
      return;
   }

   if (len>=2 && tokens[0]==wxT("type")) {
      if (tokens[1]==wxT("process"))
         mFlags = PROCESS_EFFECT | PLUGIN_EFFECT;
      if (tokens[1]==wxT("generate"))
         mFlags = INSERT_EFFECT | PLUGIN_EFFECT;
      if (tokens[1]==wxT("analyze"))
         mFlags = ANALYZE_EFFECT | PLUGIN_EFFECT;
      return;
   }

   // We support versions 1, 2 and 3
   // (Version 2 added support for string parameters.)
   // (Version 3 added support for choice parameters.)
   if (len>=2 && tokens[0]==wxT("version")) {
      if (tokens[1]==wxT("1") || tokens[1]==wxT("2") || tokens[1]==wxT("3")) {
         // We're okay
      }
      else {
         // This is an unsupported plug-in version
         mOK = false;
         return;
      }
   }

   if (len>=2 && tokens[0]==wxT("name")) {
      mName = UnQuote(tokens[1]);
      return;
   }

   if (len>=2 && tokens[0]==wxT("action")) {
      mAction = UnQuote(tokens[1]);
      return;
   }

   if (len>=2 && tokens[0]==wxT("info")) {
      mInfo = UnQuote(tokens[1]);
      return;
   }

   if (len>=6 && tokens[0]==wxT("control")) {
      NyqControl ctrl;
  
      ctrl.var = tokens[1];
      ctrl.name = tokens[2];
      ctrl.label = tokens[4];
      ctrl.valStr = tokens[5];
 
      if (tokens[3]==wxT("string"))
         ctrl.type = NYQ_CTRL_STRING;
      else if( tokens[ 3 ] == wxT( "choice" ) )
         ctrl.type = NYQ_CTRL_CHOICE;
      else {
         if (len < 8)
            return;
          
         if (tokens[3]==wxT("real"))
            ctrl.type = NYQ_CTRL_REAL;
         else
            ctrl.type = NYQ_CTRL_INT;
          
         ctrl.lowStr = tokens[6];
         ctrl.highStr = tokens[7];
      }

      ctrl.val = UNINITIALIZED_CONTROL;

      mControls.Add(ctrl);
   }
}

void EffectNyquist::ParseFile()
{
   wxTextFile f(mFileName.GetFullPath());
   if (!f.Open())
      return;

   mCmd = wxT("");
   mFlags = PROCESS_EFFECT | PLUGIN_EFFECT;
   mOK = false;
   mControls.Clear();

   int i;
   int len = f.GetLineCount();
   wxString line;
   for(i=0; i<len; i++) {
      line = f[i];
      if (line.Length()>1 && line.GetChar(0)==wxT(';'))
         Parse(line);
      else
         mCmd += line + wxT("\n");
   }
}

EffectNyquist::EffectNyquist(wxString fName)
{
   mInteractive = false;
   mAction = _("Applying Nyquist Effect...");

   if (fName == wxT("")) {
      // Interactive Nyquist
      mOK = true;
      mInteractive = true;
      mCmd = wxT("");
      mName = _("Nyquist Prompt...");
      mFlags = PROCESS_EFFECT | BUILTIN_EFFECT | ADVANCED_EFFECT;
      return;
   }

   wxLogNull dontLog;

   mName = wxFileName(fName).GetName();
   mFileName = wxFileName(fName);
   mFileModified = mFileName.GetModificationTime();
   ParseFile();
}

EffectNyquist::~EffectNyquist()
{
}

bool EffectNyquist::SetXlispPath()
{
   wxString fname;

   fname = mXlispPath + wxFILE_SEP_PATH + wxT("nyinit.lsp");
   if (!(::wxFileExists(fname)))
      mXlispPath = wxT("");

   if (mXlispPath == wxT("")) {
      wxArrayString audacityPathList = wxGetApp().audacityPathList;
      wxArrayString pathList;
      wxArrayString files;
      unsigned int i;

      for(i=0; i<audacityPathList.GetCount(); i++) {
         wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
         wxGetApp().AddUniquePathToPathList(prefix + wxT("nyquist"),
                                            pathList);
      }

      wxGetApp().FindFilesInPathList(wxT("nyquist.lsp"), pathList, wxFILE, files);

      if (files.GetCount() > 0) {
         mXlispPath = ::wxPathOnly(files[0]);
      }
   }

   #ifdef _UNICODE
      /* set_xlisp_path doesn't handle fn_Str() in Unicode build. May or may not actually work. */
      set_xlisp_path(mXlispPath.mb_str());
   #else // ANSI
      set_xlisp_path(mXlispPath.fn_str());
   #endif // Unicode/ANSI

   fname = mXlispPath + wxFILE_SEP_PATH + wxT("nyinit.lsp");
   return ::wxFileExists(fname);
}

bool EffectNyquist::PromptUser()
{
   if (!SetXlispPath())
      return false;

   if (mInteractive) {
      NyquistInputDialog dlog(mParent, -1,
                              _NoAcc("Nyquist Prompt..."),
                              _("Enter Nyquist Command: "),
                              mCmd);
      dlog.CentreOnParent();
      int result = dlog.ShowModal();

      if (result == wxID_CANCEL)
         return false;

      if (result == eDebugID)
         mDebug = true;
      else
         mDebug = false;
      
      mCmd = dlog.GetCommand();

      return true;
   }

   if (mFileName.GetModificationTime().IsLaterThan(mFileModified)) {
      ParseFile();
      mFileModified = mFileName.GetModificationTime();
   }

   if (mControls.GetCount() == 0) {
      mDebug = false;
      return true;
   }

   for(unsigned int i=0; i<mControls.GetCount(); i++) {
      NyqControl *ctrl = &mControls[i];

      if (ctrl->type == NYQ_CTRL_STRING)
         continue;

      if (ctrl->val == UNINITIALIZED_CONTROL)
         ctrl->val = GetCtrlValue(ctrl->valStr);

      if( ctrl->type == NYQ_CTRL_CHOICE )
         continue;
      
      ctrl->low = GetCtrlValue(ctrl->lowStr);
      ctrl->high = GetCtrlValue(ctrl->highStr);

      if (ctrl->high < ctrl->low)
         ctrl->high = ctrl->low + 1;
      if (ctrl->val < ctrl->low)
         ctrl->val = ctrl->low;
      if (ctrl->val > ctrl->high)
         ctrl->val = ctrl->high;

      ctrl->ticks = 1000;
      if (ctrl->type==NYQ_CTRL_INT &&
          (ctrl->high - ctrl->low < ctrl->ticks))
         ctrl->ticks = (int)(ctrl->high - ctrl->low);
   }

   NyquistDialog dlog(mParent, -1, mName, mInfo, &mControls);
   dlog.CentreOnParent();
   int result = dlog.ShowModal();

   if (result == wxID_CANCEL)
      return false;
   
   if (result == eDebugID)
      mDebug = true;
   else
      mDebug = false;

   return true;
}

bool EffectNyquist::Process()
{
   bool success = true;
   
   this->CopyInputWaveTracks(); // Set up m_pOutputWaveTracks.
   TrackListIterator iter(m_pOutputWaveTracks);
   mCurTrack[0] = (WaveTrack *) iter.First();
   mOutputTime = mT1 - mT0;
   mCount = 0;
   mProgress = 0;
   mDebugOutput = wxT("");
   while (mCurTrack[0]) {
      mCurNumChannels = 1;
      if (mT1 >= mT0) {
         if (mCurTrack[0]->GetLinked()) {
            mCurNumChannels = 2;

            mCurTrack[1] = (WaveTrack *)iter.Next();
            if (mCurTrack[1]->GetRate() != mCurTrack[0]->GetRate()) {
               wxMessageBox(_("Sorry, cannot apply effect on stereo tracks where the tracks don't match."), 
                            wxT("Nyquist"),
                            wxOK | wxCENTRE, mParent);
               success = false;
               goto finish;
            }
            mCurStart[1] = mCurTrack[1]->TimeToLongSamples(mT0);
         }

         mCurStart[0] = mCurTrack[0]->TimeToLongSamples(mT0);
         longSampleCount end = mCurTrack[0]->TimeToLongSamples(mT1);
         mCurLen = (sampleCount)(end - mCurStart[0]);

         success = ProcessOne();
         if (!success)
            goto finish;
      }

      mCurTrack[0] = (WaveTrack *) iter.Next();
      mCount += mCurNumChannels;
   }

   mT1 = mT0 + mOutputTime;

 finish:

   if (mDebug) {
      NyquistOutputDialog dlog(mParent, -1,
                               _("Nyquist"),
                               _("Nyquist Output: "),
                               mDebugOutput);
      dlog.CentreOnParent();
      dlog.ShowModal();
   }

   this->ReplaceProcessedWaveTracks(success); 
   return success;
}


int EffectNyquist::GetCallback(float *buffer, int ch,
                               long start, long len)
{
   if (mCurBuffer[ch]) {
      if ((mCurStart[ch] + start) < mCurBufferStart[ch] ||
          (mCurStart[ch] + start)+len >
          mCurBufferStart[ch]+mCurBufferLen[ch]) {
         delete[] mCurBuffer[ch];
         mCurBuffer[ch] = NULL;
      }
   }

   if (!mCurBuffer[ch]) {
      mCurBufferStart[ch] = (mCurStart[ch] + start);
      mCurBufferLen[ch] = mCurTrack[ch]->GetBestBlockSize(mCurBufferStart[ch]);
      if (mCurBufferLen[ch] < len)
         mCurBufferLen[ch] = mCurTrack[ch]->GetIdealBlockSize();
      if (mCurBufferStart[ch] + mCurBufferLen[ch] > mCurStart[ch] + mCurLen)
         mCurBufferLen[ch] = mCurStart[ch] + mCurLen - mCurBufferStart[ch];
      mCurBuffer[ch] = NewSamples(mCurBufferLen[ch], floatSample);
      if (!mCurTrack[ch]->Get(mCurBuffer[ch], floatSample,
                              mCurBufferStart[ch], mCurBufferLen[ch])) {

         wxPrintf(wxT("GET error\n"));

         return -1;
      }
   }

   long offset = (mCurStart[ch] + start) - mCurBufferStart[ch];
   CopySamples(mCurBuffer[ch] + offset*SAMPLE_SIZE(floatSample), floatSample,
               (samplePtr)buffer, floatSample,
               len);

   if (ch==0) {
      double progress = 1.0*(start+len)/mCurLen;
      if (progress > mProgress)
         mProgress = progress;

      if (TotalProgress(mProgress))
         return -1;
   }

   return 0;
}

int EffectNyquist::PutCallback(float *buffer, int channel,
                               long start, long len)
{
   
   if (mOutputTrack[channel]->Append((samplePtr)buffer, floatSample, len))
      return 0;  // success
   else
      return -1; // failure
}

int EffectNyquist::StaticGetCallback(float *buffer, int channel,
                                     long start, long len,
                                     void *userdata)
{
   EffectNyquist *This = (EffectNyquist *)userdata;
   return This->GetCallback(buffer, channel, start, len);
}

int EffectNyquist::StaticPutCallback(float *buffer, int channel,
                                     long start, long len,
                                     void *userdata)
{
   EffectNyquist *This = (EffectNyquist *)userdata;
   return This->PutCallback(buffer, channel, start, len);
}

bool EffectNyquist::ProcessOne()
{
   // libnyquist breaks except in LC_NUMERIC=="C".
   //
   // MB: setlocale is not thread-safe.  Should use uselocale()
   //     if available, or fix libnyquist to be locale-independent.
   setlocale(LC_NUMERIC, "C");

   nyx_rval rval;

   nyx_init();

   if (mDebug)
      nyx_capture_output(65536);

   if( !( mFlags & INSERT_EFFECT ) )
      nyx_set_input_audio(StaticGetCallback, (void *)this,
                          mCurNumChannels,
                          mCurLen, mCurTrack[0]->GetRate());
   else
      nyx_set_audio_params( mCurTrack[0]->GetRate() );

   wxString cmd;

   if (mDebug) {
      cmd = cmd + wxT("(setf *tracenable* T)\n");
   }

   for(unsigned int j=0; j<mControls.GetCount(); j++) {
      if (mControls[j].type==NYQ_CTRL_REAL)
         cmd = cmd+wxString::Format(wxT("(setf %s %f)\n"),
                                    mControls[j].var.c_str(),
                                    mControls[j].val);
      else if (mControls[j].type==NYQ_CTRL_INT || 
            mControls[j].type == NYQ_CTRL_CHOICE)
         cmd = cmd+wxString::Format(wxT("(setf %s %d)\n"),
                                    mControls[j].var.c_str(),
                                    (int)(mControls[j].val));
      else if (mControls[j].type==NYQ_CTRL_STRING) {
         wxString str = mControls[j].valStr;
         str.Replace(wxT("\""), wxT("'"));
         cmd = cmd+wxString::Format(wxT("(setf %s \"%s\")\n"),
                                    mControls[j].var.c_str(),
                                    str.c_str());
      }
   }
   
   cmd += mCmd;
   
   //wxMessageBox(cmd);
   
   int i;
	for (i = 0; i < mCurNumChannels; i++)
		mCurBuffer[i] = NULL;
   rval = nyx_eval_expression(cmd.mb_str());

   if (mDebug) {
      int len;
      const char *str;

      nyx_get_captured_output(&len, &str);
      mDebugOutput += wxString(str, wxConvISO8859_1, len);
   }

   if (rval == nyx_string) {
      wxMessageBox(wxString(nyx_get_string(), wxConvISO8859_1), wxT("Nyquist"),
                   wxOK | wxCENTRE, mParent);
      nyx_cleanup();
      setlocale(LC_NUMERIC, "");
      return true;
   }

   if (rval == nyx_double) {
      wxString str;
      str.Printf(_("Nyquist returned the value:") + wxString(wxT(" %f")),
                 nyx_get_double());
      wxMessageBox(str, wxT("Nyquist"),
                   wxOK | wxCENTRE, mParent);
      nyx_cleanup();
      setlocale(LC_NUMERIC, "");
      return true;
   }

   if (rval == nyx_int) {
      wxString str;
      str.Printf(_("Nyquist returned the value:") + wxString(wxT(" %d")),
                 nyx_get_int());
      wxMessageBox(str, wxT("Nyquist"),
                   wxOK | wxCENTRE, mParent);
      nyx_cleanup();
      setlocale(LC_NUMERIC, "");
      return true;
   }

   if (rval == nyx_labels) {
      int numLabels = nyx_get_num_labels();
      int l;
      LabelTrack *ltrack = NULL;

      TrackListIterator iter(mTracks);
      Track *t = iter.First();
      while(t && !ltrack) {
         if (t->GetKind() == Track::Label)
            ltrack = (LabelTrack *)t;
         t = iter.Next();
      }
      
      if (!ltrack) {
         ltrack = mFactory->NewLabelTrack();
         mTracks->Add((Track *)ltrack);
      }

      for(l=0; l<numLabels; l++) {
         double t0, t1;
         const char *str;

         nyx_get_label(l, &t0, &t1, &str);

         ltrack->AddLabel(t0 + mT0, t1 + mT0, LAT1CTOWX(str));
      }

      nyx_cleanup();
      setlocale(LC_NUMERIC, "");
      return true;
   }

   if (rval != nyx_audio) {
      wxMessageBox(_("Nyquist did not return audio.\n"), wxT("Nyquist"),
                   wxOK | wxCENTRE, mParent);
      nyx_cleanup();
      setlocale(LC_NUMERIC, "");
      return false;
   }
   
   int outChannels;

   outChannels = nyx_get_audio_num_channels();
   if (outChannels > mCurNumChannels) {
      wxMessageBox(_("Nyquist returned too many audio channels.\n"),
                   wxT("Nyquist"),
                   wxOK | wxCENTRE, mParent);
      nyx_cleanup();
      setlocale(LC_NUMERIC, "");
      return false;
   }

   double rate = mCurTrack[0]->GetRate();
   for(i=0; i<outChannels; i++) {
      sampleFormat format = mCurTrack[i]->GetSampleFormat();

      if (outChannels == mCurNumChannels)
         rate = mCurTrack[i]->GetRate();

      mOutputTrack[i] = mFactory->NewWaveTrack(format, rate);
      mCurBuffer[i] = NULL;
   }

   nyx_get_audio(StaticPutCallback, (void *)this);

   for(i=0; i<outChannels; i++) {
      mOutputTrack[i]->Flush();
      if (mCurBuffer[i])
         DeleteSamples(mCurBuffer[i]);
      mOutputTime = mOutputTrack[i]->GetEndTime();
   }

   for(i=0; i<mCurNumChannels; i++) {
      WaveTrack *out;

      if (outChannels == mCurNumChannels)
         out = mOutputTrack[i];
      else
         out = mOutputTrack[0];

      mCurTrack[i]->Clear(mT0, mT1);
      mCurTrack[i]->Paste(mT0, out);
   }

   for(i=0; i<outChannels; i++)
      delete mOutputTrack[i];

   nyx_cleanup();

   // Reset locale
   setlocale(LC_NUMERIC, "");
   
   return true;
}

/**********************************************************/

#define ID_NYQ_SLIDER 2000
#define ID_NYQ_TEXT   3000
#define ID_NYQ_CHOICE 4000

BEGIN_EVENT_TABLE(NyquistDialog, wxDialog)
   EVT_BUTTON(wxID_OK, NyquistDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL, NyquistDialog::OnCancel)
   EVT_BUTTON(eDebugID, NyquistDialog::OnDebug)
   EVT_COMMAND_RANGE(ID_NYQ_SLIDER, ID_NYQ_SLIDER+99,
                     wxEVT_COMMAND_SLIDER_UPDATED, NyquistDialog::OnSlider)
   EVT_COMMAND_RANGE(ID_NYQ_TEXT, ID_NYQ_TEXT+99,
                      wxEVT_COMMAND_TEXT_UPDATED, NyquistDialog::OnText)
   EVT_COMMAND_RANGE( ID_NYQ_CHOICE, ID_NYQ_CHOICE + 99,
                     wxEVT_COMMAND_CHOICE_SELECTED, NyquistDialog::OnChoice )
END_EVENT_TABLE()

NyquistDialog::NyquistDialog(wxWindow * parent, wxWindowID id,
                             const wxString & title,
                             wxString info,
                             NyqControlArray *controlArray)
   :wxDialog(parent, id, title)
{
   mControls = controlArray;
   mInHandler = true; // prevents race condition on MSW

   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);

   if (info.Length() > 0) {
      wxControl  *item;
      item = new wxStaticText(this, -1, info);
      mainSizer->Add(item, 0, wxALIGN_LEFT | wxALL, 5);
   }

   wxFlexGridSizer *grid = new wxFlexGridSizer(4, 0, 0);

   for(unsigned int i=0; i<mControls->GetCount(); i++) {
      wxControl  *item;
      NyqControl *ctrl = &((*mControls)[i]);

      item = new wxStaticText(this, -1, ctrl->name);
      grid->Add(item, 0, wxALIGN_RIGHT | 
                wxALIGN_CENTER_VERTICAL | wxALL, 5);

      if (ctrl->type == NYQ_CTRL_STRING) {
         grid->Add(10, 10);

         item = new wxTextCtrl(this, ID_NYQ_TEXT+i, ctrl->valStr,
                               wxDefaultPosition, wxSize(150, -1));
         grid->Add(item, 0, wxALIGN_CENTRE |
                   wxALIGN_CENTER_VERTICAL | wxALL, 5);
      }
      else if( ctrl->type == NYQ_CTRL_CHOICE )
      {
         //str is coma separated labels for each choice
         wxString str = ctrl->label;
         wxArrayString choices;
         
         while( 1 )
         {       
            int ci = str.Find( ',' ); //coma index
            
            if( ci == -1 )
            {
               choices.Add( str );
               break;
            }
            else
               choices.Add( str.Left( ci ) );
            
            str = str.Right( str.length() - ci - 1 );
         }

         wxChoice *choice = new wxChoice( this, ID_NYQ_CHOICE + i, 
               wxDefaultPosition, wxSize( 150, -1 ), choices );
         
         int val = ( int )ctrl->val;
         if( val >= 0 && val < choice->GetCount() )
            choice->SetSelection( val );
        
         grid->Add( 10, 10 );
         grid->Add( choice, 0, wxALIGN_CENTRE | 
               wxALIGN_CENTER_VERTICAL | wxALL, 5 );
      }
      else {
         // Integer or Real
         int val = (int)(0.5 + ctrl->ticks * (ctrl->val - ctrl->low)/
               (ctrl->high - ctrl->low));

         item = new wxTextCtrl(this, ID_NYQ_TEXT+i, wxT(""),
                               wxDefaultPosition, wxSize(60, -1));
         grid->Add(item, 0, wxALIGN_CENTRE |
                   wxALIGN_CENTER_VERTICAL | wxALL, 5);
         
         item = new wxSlider(this, ID_NYQ_SLIDER+i, val, 0, ctrl->ticks,
                             wxDefaultPosition, wxSize(150, -1));
         grid->Add(item, 0, wxALIGN_CENTRE |
                   wxALIGN_CENTER_VERTICAL | wxALL, 5);
      }
         
      if( ctrl->type == NYQ_CTRL_CHOICE )
      {
         grid->Add( 10, 10 );
      }
      else
      {
         item = new wxStaticText(this, -1, ctrl->label);
         grid->Add(item, 0, wxALIGN_LEFT | 
                   wxALIGN_CENTER_VERTICAL | wxALL, 5);
      }
   }
   mainSizer->Add(grid, 0, wxALIGN_CENTRE | wxALL, 5);

   mainSizer->Add(CreateStdButtonSizer(this, eDebugButton|eCancelButton|eOkButton), 0, wxEXPAND);

   mInHandler = false;

   wxCommandEvent dummy;
   OnSlider(dummy);

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
}

void NyquistDialog::OnSlider(wxCommandEvent & /* event */)
{
   if (mInHandler)
      return; // prevent recursing forever
   mInHandler = true;

   for(unsigned int i=0; i<mControls->GetCount(); i++) {
      NyqControl *ctrl = &((*mControls)[i]);

      if (ctrl->type == NYQ_CTRL_STRING || ctrl->type == NYQ_CTRL_CHOICE )
         continue;

      wxSlider *slider = (wxSlider *)FindWindow(ID_NYQ_SLIDER + i);
      wxTextCtrl *text = (wxTextCtrl *)FindWindow(ID_NYQ_TEXT + i);
      wxASSERT(slider && text);
      
      int val = slider->GetValue();
      ctrl->val = (val / (double)ctrl->ticks)*
         (ctrl->high - ctrl->low) + ctrl->low;
      
      wxString valStr;
      if (ctrl->type == NYQ_CTRL_REAL) {
         if (ctrl->high - ctrl->low < 1)
            valStr = Internat::ToDisplayString(ctrl->val, 3);
         else if (ctrl->high - ctrl->low < 10)
            valStr = Internat::ToDisplayString(ctrl->val, 2);
         else if (ctrl->high - ctrl->low < 100)
            valStr = Internat::ToDisplayString(ctrl->val, 1);
         else
            valStr.Printf(wxT("%d"), (int)floor(ctrl->val + 0.5));
      }
      else if (ctrl->type == NYQ_CTRL_INT)
         valStr.Printf(wxT("%d"), (int)floor(ctrl->val + 0.5));

      if (valStr != wxT(""))
         text->SetValue(valStr);
   }

   mInHandler = false;
}

void NyquistDialog::OnChoice( wxCommandEvent &event )
{
   if( mInHandler )
      return; // prevent recursing forever
   mInHandler = true;

   unsigned int ctrlId = event.GetId() - ID_NYQ_CHOICE;
   wxASSERT( ctrlId >= 0 && ctrlId < mControls->GetCount() );
   
   NyqControl *ctrl = &( mControls->Item( ctrlId ) );
   wxChoice *choice = ( wxChoice * )FindWindow( ID_NYQ_CHOICE + ctrlId );
   wxASSERT( choice );

   ctrl->val = choice->GetSelection();

   mInHandler = false;   
}

void NyquistDialog::OnText(wxCommandEvent &event)
{
   if (mInHandler)
      return; // prevent recursing forever
   mInHandler = true;

   unsigned int ctrlId = event.GetId() - ID_NYQ_TEXT;
   wxASSERT( ctrlId >= 0 && ctrlId < mControls->GetCount() );

   NyqControl *ctrl = &((*mControls)[ ctrlId ]);
   wxTextCtrl *text = (wxTextCtrl *)FindWindow(ID_NYQ_TEXT + ctrlId);
   wxASSERT(text);

   ctrl->valStr = text->GetValue();

   if( ctrl->type != NYQ_CTRL_STRING )
   {
      wxSlider *slider = (wxSlider *)FindWindow(ID_NYQ_SLIDER + ctrlId);
      wxASSERT(slider);

      if (ctrl->valStr.ToDouble(&ctrl->val))
      {
         int pos = (int)floor((ctrl->val - ctrl->low) /
                              (ctrl->high - ctrl->low) * ctrl->ticks + 0.5);
         if (pos < 0)
            pos = 0;
         if (pos > ctrl->ticks)
            pos = ctrl->ticks;
         slider->SetValue(pos);
      }
   }   

   mInHandler = false;   
}

void NyquistDialog::OnOk(wxCommandEvent & /* event */)
{
   // Transfer data

   EndModal(wxID_OK);
}

void NyquistDialog::OnCancel(wxCommandEvent & /* event */)
{
   EndModal(wxID_CANCEL);
}

void NyquistDialog::OnDebug(wxCommandEvent & /* event */)
{
   // Transfer data

   EndModal(eDebugID);
}

/**********************************************************/

BEGIN_EVENT_TABLE(NyquistInputDialog, wxDialog)
   EVT_BUTTON(wxID_OK, NyquistInputDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL, NyquistInputDialog::OnCancel)
   EVT_BUTTON(eDebugID, NyquistInputDialog::OnDebug)
END_EVENT_TABLE()

NyquistInputDialog::NyquistInputDialog(wxWindow * parent, wxWindowID id,
                                       const wxString & title,
                                       const wxString & prompt,
                                       wxString initialCommand)
   :wxDialog(parent, id, title)
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxControl  *item;

   item = new wxStaticText(this, -1, prompt);
   mainSizer->Add(item, 0, wxALIGN_LEFT |
                  wxLEFT | wxTOP | wxRIGHT, 10);

   mCommandText = new wxTextCtrl(this, -1, initialCommand,
                                 wxDefaultPosition, wxSize(400, 200),
                                 wxTE_MULTILINE);
   mainSizer->Add(mCommandText, 0, wxALIGN_LEFT | wxALL, 10);

   // Debug, OK, & Cancel buttons
   mainSizer->Add(CreateStdButtonSizer(this, eDebugButton|eCancelButton|eOkButton), 0, wxEXPAND);

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);

   mCommandText->SetFocus();
}

wxString NyquistInputDialog::GetCommand()
{
   return mCommandText->GetValue();
}

void NyquistInputDialog::OnOk(wxCommandEvent & /* event */)
{
   EndModal(wxID_OK);
}

void NyquistInputDialog::OnCancel(wxCommandEvent & /* event */)
{
   EndModal(wxID_CANCEL);
}

void NyquistInputDialog::OnDebug(wxCommandEvent & /* event */)
{
   // Transfer data

   EndModal(eDebugID);
}


/**********************************************************/


BEGIN_EVENT_TABLE(NyquistOutputDialog, wxDialog)
   EVT_BUTTON(wxID_OK, NyquistOutputDialog::OnOk)
END_EVENT_TABLE()

NyquistOutputDialog::NyquistOutputDialog(wxWindow * parent, wxWindowID id,
                                       const wxString & title,
                                       const wxString & prompt,
                                       wxString message)
   :wxDialog(parent, id, title)
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *hSizer;
   wxButton   *button;
   wxControl  *item;

   item = new wxStaticText(this, -1, prompt);
   mainSizer->Add(item, 0, wxALIGN_LEFT | 
                  wxLEFT | wxTOP | wxRIGHT, 10);

   item = new wxTextCtrl(this, -1, message,
                         wxDefaultPosition, wxSize(400, 200),
                         wxTE_MULTILINE | wxTE_READONLY);
   mainSizer->Add(item, 0, wxALIGN_LEFT | wxALL, 10);

   hSizer = new wxBoxSizer(wxHORIZONTAL);

   button = new wxButton(this, wxID_OK, _("OK"));
   button->SetDefault();
   hSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE |
                  wxLEFT | wxBOTTOM | wxRIGHT, 5);

   SetAutoLayout(true);
   SetSizer(mainSizer);
   mainSizer->Fit(this);
   mainSizer->SetSizeHints(this);
}

void NyquistOutputDialog::OnOk(wxCommandEvent & /* event */)
{
   EndModal(wxID_OK);
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
// arch-tag: a6e418bb-2609-42f2-a8ae-8e50373855f3

