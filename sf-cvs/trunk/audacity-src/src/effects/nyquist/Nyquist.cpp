/**********************************************************************

  Audacity: A Digital Audio Editor

  Nyquist.cpp

  Dominic Mazzoni

**********************************************************************/

#include <math.h>

#include <wx/intl.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/textfile.h>

#include "../../LabelTrack.h"
#include "Nyquist.h"

#include <wx/arrimpl.cpp>
WX_DEFINE_OBJARRAY(NyqControlArray);

#define UNINITIALIZED_CONTROL ((double)99999999.99)

wxString EffectNyquist::UnQuote(wxString s)
{
   wxString out;
   int len = s.Length();
   
   if (len>=2 && s[0]=='\"' && s[len-1]=='\"')
      return s.Mid(1, len-2);
   else
      return s;
}

double EffectNyquist::GetCtrlValue(wxString s)
{
   if (s == "rate") {
      TrackListIterator iter(mWaveTracks);
      return ((WaveTrack *)iter.First())->GetRate();
   }
   else {
      double d;
      s.ToDouble(&d);
      return d;
   }
}

void EffectNyquist::Parse(wxString line)
{
   wxArrayString tokens;

   int i;
   int len = line.Length();
   bool sl = false;
   bool q = false;
   wxString tok = "";
   for(i=1; i<len; i++) {
      if (line[i]=='\\')
         sl = true;
      else if (line[i]=='"')
         q = !q;
      else {
         if (!q && !sl && line[i]==' ' || line[i]=='\t') {
            tokens.Add(tok);
            tok = "";
         }
         else if (sl && line[i]=='n')
            tok += '\n';
         else
            tok += line[i];
         
         sl = false;
      }
   }
   if (tok != "")
      tokens.Add(tok);

   len = tokens.GetCount();
   if (len < 1)
      return;

   if (len==2 && tokens[0]=="nyquist" && tokens[1]=="plug-in") {
      mOK = true;
      return;
   }

   if (len>=2 && tokens[0]=="type") {
      if (tokens[1]=="process")
         mFlags = PROCESS_EFFECT | PLUGIN_EFFECT;
      if (tokens[1]=="generate")
         mFlags = INSERT_EFFECT | PLUGIN_EFFECT;
      if (tokens[1]=="analyze")
         mFlags = ANALYZE_EFFECT | PLUGIN_EFFECT;
      return;
   }

   if (len>=2 && tokens[0]=="name") {
      mName = UnQuote(tokens[1]);
      return;
   }

   if (len>=2 && tokens[0]=="action") {
      mAction = UnQuote(tokens[1]);
      return;
   }

   if (len>=2 && tokens[0]=="info") {
      mInfo = UnQuote(tokens[1]);
      return;
   }

   if (len>=8 && tokens[0]=="control") {
      NyqControl ctrl;

      ctrl.var = tokens[1];
      ctrl.name = tokens[2];
      ctrl.type = 0;
      if (tokens[3]=="real")
         ctrl.type = 1;
      ctrl.label = tokens[4];
      ctrl.valStr = tokens[5];
      ctrl.lowStr = tokens[6];
      ctrl.highStr = tokens[7];
      ctrl.val = UNINITIALIZED_CONTROL;

      mControls.Add(ctrl);
   }
}

EffectNyquist::EffectNyquist(wxString fName)
{
   mOK = false;
   mInteractive = false;
   mAction = _("Appling Nyquist Effect...");

   if (fName == "") {
      // Interactive Nyquist
      mOK = true;
      mInteractive = true;
      mCmd = "";
      mName = _("Nyquist Prompt...");
      mFlags = PROCESS_EFFECT | BUILTIN_EFFECT;
      return;
   }

   wxTextFile f(fName);
   if (!f.Open())
      return;

   mCmd = "";
   mName = wxFileNameFromPath(fName);
   mFlags = PROCESS_EFFECT | PLUGIN_EFFECT;

   int i;
   int len = f.GetLineCount();
   wxString line;
   for(i=0; i<len; i++) {
      line = f[i];
      if (line.Length()>1 && line[0]==';')
         Parse(line);
      else
         mCmd += line + "\n";
   }
}

EffectNyquist::~EffectNyquist()
{
}

bool EffectNyquist::PromptUser()
{
   char *env = getenv("XLISPPATH");
   if (!env) {
      wxMessageBox("The XLISPPATH environment variable is not set.\n"
                   "Nyquist cannot run.", "Nyquist",
                   wxOK | wxCENTRE, mParent);
      return false;
   }
   
   if (mInteractive) {
      wxString temp;
      wxString title = _("Nyquist Prompt");
      wxString caption = _("Enter Nyquist Command: ");

      temp = wxGetTextFromUser(caption, title,
                               mCmd, mParent, -1, -1, TRUE);
      if (temp == "")
         return false;
      
      mCmd = temp;
      return true;
   }

   if (mControls.GetCount() == 0)
      return true;

   for(unsigned int i=0; i<mControls.GetCount(); i++) {
      NyqControl *ctrl = &mControls[i];

      if (ctrl->val == UNINITIALIZED_CONTROL)
         ctrl->val = GetCtrlValue(ctrl->valStr);

      ctrl->low = GetCtrlValue(ctrl->lowStr);
      ctrl->high = GetCtrlValue(ctrl->highStr);

      if (ctrl->high < ctrl->low)
         ctrl->high = ctrl->low + 1;
      if (ctrl->val < ctrl->low)
         ctrl->val = ctrl->low;
      if (ctrl->val > ctrl->high)
         ctrl->val = ctrl->high;

      ctrl->ticks = 1000;
      if (ctrl->type==0 &&
          (ctrl->high - ctrl->low < ctrl->ticks))
         ctrl->ticks = (int)(ctrl->high - ctrl->low);
   }

   NyquistDialog dlog(mParent, -1, mName, mInfo, &mControls);
   dlog.CentreOnParent();
   dlog.ShowModal();

   if (!dlog.GetReturnCode())
      return false;

   return true;
}

bool EffectNyquist::Process()
{
   TrackListIterator iter(mWaveTracks);
   mCurTrack[0] = (WaveTrack *) iter.First();
   mOutputTime = mT1 - mT0;
   mCount = 0;
   mProgress = 0;
   while (mCurTrack[0]) {
      mCurNumChannels = 1;
      double trackStart = mCurTrack[0]->GetStartTime();
      double trackEnd = mCurTrack[0]->GetEndTime();
      double t0 = mT0 < trackStart? trackStart: mT0;
      double t1 = mT1 > trackEnd? trackEnd: mT1;
      if (t1 >= t0) {
         if (mCurTrack[0]->GetLinked()) {
            mCurNumChannels = 2;

            mCurTrack[1] = (WaveTrack *)iter.Next();
            if (mCurTrack[1]->GetRate() != mCurTrack[0]->GetRate()) {
               wxMessageBox("Sorry, cannot apply effect on stereo tracks "
                            "where the tracks don't match.", "Nyquist",
                            wxOK | wxCENTRE, mParent);
               return false;
            }
            mCurStart[1] = mCurTrack[1]->TimeToLongSamples(t0);
         }

         mCurStart[0] = mCurTrack[0]->TimeToLongSamples(t0);
         longSampleCount end = mCurTrack[0]->TimeToLongSamples(t1);
         mCurLen = (sampleCount)(end - mCurStart[0]);

         if (!ProcessOne())
            return false;
      }

      mCurTrack[0] = (WaveTrack *) iter.Next();
      mCount += mCurNumChannels;
   }

   mT1 = mT0 + mOutputTime;

   return true;
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

         printf("GET error\n");

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
   nyx_rval rval;

   nyx_init();
   nyx_set_input_audio(StaticGetCallback, (void *)this,
                       mCurNumChannels,
                       mCurLen, mCurTrack[0]->GetRate());

   wxString cmd;
   for(unsigned int j=0; j<mControls.GetCount(); j++)
      cmd = cmd+wxString::Format("(setf %s %f)\n",
                                 (const char *)mControls[j].var,
                                 mControls[j].val);
   cmd += mCmd;
   rval = nyx_eval_expression(cmd);

   if (rval == nyx_string) {
      wxMessageBox(nyx_get_string(), "Nyquist",
                   wxOK | wxCENTRE, mParent);
      nyx_cleanup();
      return true;
   }

   if (rval == nyx_double) {
      wxString str;
      str.Printf(_("Nyquist returned the value: %f"), nyx_get_double());
      wxMessageBox(str, "Nyquist",
                   wxOK | wxCENTRE, mParent);
      nyx_cleanup();
      return true;
   }

   if (rval == nyx_int) {
      wxString str;
      str.Printf(_("Nyquist returned the value: %d"), nyx_get_int());
      wxMessageBox(str, "Nyquist",
                   wxOK | wxCENTRE, mParent);
      nyx_cleanup();
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
         double t;
         const char *str;

         nyx_get_label(l, &t, &str);

         ltrack->Add(t + mT0, str);
      }

      return true;
   }

   if (rval != nyx_audio) {
      wxMessageBox(_("Nyquist did not return audio.\n"), "Nyquist",
                   wxOK | wxCENTRE, mParent);
      nyx_cleanup();
      return false;
   }
   
   int i;
   int outChannels;

   outChannels = nyx_get_audio_num_channels();
   if (outChannels > mCurNumChannels) {
      wxMessageBox(_("Nyquist returned too many audio channels.\n"),
                   "Nyquist",
                   wxOK | wxCENTRE, mParent);
      return false;
   }

   for(i=0; i<outChannels; i++) {
      sampleFormat format = mCurTrack[i]->GetSampleFormat();
      mOutputTrack[i] = mFactory->NewWaveTrack(format);
      mCurBuffer[i] = NULL;
   }

   nyx_get_audio(StaticPutCallback, (void *)this);
   
   for(i=0; i<outChannels; i++) {
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
   
   return true;
}

/**********************************************************/

#define ID_NYQ_SLIDER 2000
#define ID_NYQ_TEXT   3000

BEGIN_EVENT_TABLE(NyquistDialog, wxDialog)
   EVT_BUTTON(wxID_OK, NyquistDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL, NyquistDialog::OnCancel)
   EVT_COMMAND_RANGE(ID_NYQ_SLIDER, ID_NYQ_SLIDER+99,
                     wxEVT_COMMAND_SLIDER_UPDATED, NyquistDialog::OnSlider)
   EVT_COMMAND_RANGE(ID_NYQ_TEXT, ID_NYQ_TEXT+99,
                      wxEVT_COMMAND_TEXT_UPDATED, NyquistDialog::OnText)
END_EVENT_TABLE()

NyquistDialog::NyquistDialog(wxWindow * parent, wxWindowID id,
                             const wxString & title,
                             wxString info,
                             NyqControlArray *controlArray)
   :wxDialog(parent, id, title)
{
   mControls = controlArray;
   mInHandler = false;

   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *hSizer;
   wxButton   *button;

   if (info.Length() > 0) {
      wxControl  *item;
      item = new wxStaticText(this, -1, info);
      mainSizer->Add(item, 0, wxALIGN_LEFT | wxALL, 5);
   }

   wxFlexGridSizer *grid = new wxFlexGridSizer(4, 0, 0);

   for(unsigned int i=0; i<mControls->GetCount(); i++) {
      wxControl  *item;
      NyqControl *ctrl = &((*mControls)[i]);
      int val = (int)(0.5 + ctrl->ticks * (ctrl->val - ctrl->low)/
                      (ctrl->high - ctrl->low));

      item = new wxStaticText(this, -1, ctrl->name);
      grid->Add(item, 0, wxALIGN_RIGHT | wxALL, 5);

      item = new wxTextCtrl(this, ID_NYQ_TEXT+i, "",
                            wxDefaultPosition, wxSize(60, -1));
      grid->Add(item, 0, wxALIGN_CENTRE | wxALL, 5);

      item = new wxSlider(this, ID_NYQ_SLIDER+i, val, 0, ctrl->ticks,
                          wxDefaultPosition, wxSize(150, -1));
      grid->Add(item, 0, wxALIGN_CENTRE | wxALL, 5);

      item = new wxStaticText(this, -1, ctrl->label);
      grid->Add(item, 0, wxALIGN_LEFT | wxALL, 5);
   }
   mainSizer->Add(grid, 0, wxALIGN_CENTRE | wxALL, 5);

   hSizer = new wxBoxSizer(wxHORIZONTAL);
   button = new wxButton(this, wxID_OK, _("OK"));
   button->SetDefault();
   button->SetFocus();
   hSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

   button = new wxButton(this, wxID_CANCEL, _("Cancel"));
   hSizer->Add(button, 0, wxALIGN_CENTRE | wxALL, 5);

   mainSizer->Add(hSizer, 0, wxALIGN_CENTRE | wxALL, 5);

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
      wxSlider *slider = (wxSlider *)FindWindow(ID_NYQ_SLIDER + i);
      wxTextCtrl *text = (wxTextCtrl *)FindWindow(ID_NYQ_TEXT + i);
      wxASSERT(slider && text);
      
      int val = slider->GetValue();
      ctrl->val = (val / (double)ctrl->ticks)*
         (ctrl->high - ctrl->low) + ctrl->low;
      
      wxString valStr;
      if (ctrl->type == 1) {
         if (ctrl->high - ctrl->low < 1)
            valStr.Printf("%.3f", ctrl->val);
         else if (ctrl->high - ctrl->low < 10)
            valStr.Printf("%.2f", ctrl->val);
         else if (ctrl->high - ctrl->low < 100)
            valStr.Printf("%.1f", ctrl->val);
         else
            valStr.Printf("%d", (int)floor(ctrl->val + 0.5));
      }
      else
         valStr.Printf("%d", (int)floor(ctrl->val + 0.5));
      
      text->SetValue(valStr);
   }

   mInHandler = false;
}

void NyquistDialog::OnText(wxCommandEvent & /* event */)
{
   if (mInHandler)
      return; // prevent recursing forever
   mInHandler = true;

   for(unsigned int i=0; i<mControls->GetCount(); i++) {
      NyqControl *ctrl = &((*mControls)[i]);
      wxSlider *slider = (wxSlider *)FindWindow(ID_NYQ_SLIDER + i);
      wxTextCtrl *text = (wxTextCtrl *)FindWindow(ID_NYQ_TEXT + i);
      wxASSERT(slider && text);

      wxString valStr = text->GetValue();
      valStr.ToDouble(&ctrl->val);
      int pos = (int)floor((ctrl->val - ctrl->low) /
                           (ctrl->high - ctrl->low) * ctrl->ticks + 0.5);
      if (pos < 0)
         pos = 0;
      if (pos > ctrl->ticks)
         pos = ctrl->ticks;
      slider->SetValue(pos);
   }   

   mInHandler = false;   
}

void NyquistDialog::OnOk(wxCommandEvent & /* event */)
{
   // Transfer data

   EndModal(true);
}

void NyquistDialog::OnCancel(wxCommandEvent & /* event */)
{
   EndModal(false);
}

