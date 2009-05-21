/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.cpp

  Dominic Mazzoni
  
  This class implements a VST Plug-in effect.  The plug-in must be
  loaded in a platform-specific way and passed into the constructor,
  but from here this class handles the interfacing.  VST plug-ins
  are used in Cubase and other Steinberg products, and all of those
  files and the information within is copyrighted by Steinberg.

**********************************************************************/

#include "../../Audacity.h"

#if USE_VST

#include <wx/defs.h>
#include <wx/button.h>
#include <wx/combobox.h>
#include <wx/dialog.h>
#include <wx/filename.h>
#include <wx/frame.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/slider.h>
#include <wx/scrolwin.h>
#include <wx/stattext.h>

#if defined(__WXMAC__)
#include <wx/mac/private.h>
#else
#include <wx/dynlib.h>
#endif

#include "../../FileNames.h"
#include "../../Prefs.h"
#include "../../xml/XMLFileReader.h"
#include "../../xml/XMLWriter.h"
#include "FileDialog.h"

#include "VSTEffect.h"          // This class's header file

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffectDialog
//
///////////////////////////////////////////////////////////////////////////////

class VSTEffectDialog:public wxDialog, XMLTagHandler
{
 public:
   VSTEffectDialog(wxWindow * parent,
                      const wxString & title,
                      VSTEffect *effect,
                      AEffect *aeffect);

   void OnIdle(wxIdleEvent & evt);

   void OnProgram(wxCommandEvent & evt);
   void OnProgramText(wxCommandEvent & evt);
   void OnLoad(wxCommandEvent & evt);
   void OnSave(wxCommandEvent & evt);

   void OnSlider(wxCommandEvent &event);

   void OnOk(wxCommandEvent & evt);
   void OnCancel(wxCommandEvent & evt);
   void OnPreview(wxCommandEvent & evt);

 private:

   void BuildPlain();
   void BuildFancy();
   wxSizer *BuildProgramBar();

   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);

   VSTEffect *mEffect;
   AEffect *mAEffect;

   wxComboBox *mProgram;

   DECLARE_EVENT_TABLE()
};

enum
{
   ID_VST_PROGRAM = 11000,
   ID_VST_LOAD,
   ID_VST_SAVE,
   ID_VST_SLIDERS
};

BEGIN_EVENT_TABLE(VSTEffectDialog, wxDialog)
   EVT_IDLE(VSTEffectDialog::OnIdle)
   EVT_BUTTON(wxID_OK, VSTEffectDialog::OnOk)
   EVT_BUTTON(wxID_CANCEL, VSTEffectDialog::OnCancel)
   EVT_BUTTON(ID_EFFECT_PREVIEW, VSTEffectDialog::OnPreview)

   EVT_COMBOBOX(ID_VST_PROGRAM, VSTEffectDialog::OnProgram)
   EVT_TEXT(ID_VST_PROGRAM, VSTEffectDialog::OnProgramText)
   EVT_BUTTON(ID_VST_LOAD, VSTEffectDialog::OnLoad)
   EVT_BUTTON(ID_VST_SAVE, VSTEffectDialog::OnSave)

   EVT_SLIDER(wxID_ANY, VSTEffectDialog::OnSlider)
END_EVENT_TABLE()

VSTEffectDialog::VSTEffectDialog(wxWindow *parent,
                                       const wxString & title,
                                       VSTEffect *effect,
                                       AEffect *aeffect)
:  wxDialog(parent, wxID_ANY, title),
   mEffect(effect),
   mAEffect(aeffect)
{
   bool gui = (gPrefs->Read(wxT("/VST/GUI"), (long) true) != 0);
   if (gui && mAEffect->flags & effFlagsHasEditor) {
      BuildFancy();
   }
   else {
      BuildPlain();
   }
}

void VSTEffectDialog::BuildFancy()
{
#if defined(__WXMAC__)
   ControlRef view;
   WindowRef win = (WindowRef) MacGetTopLevelWindowRef();
   OSStatus status = HIViewFindByID(HIViewGetRoot(win), kHIViewWindowContentID, &view);
   wxASSERT(status == 0);

   mEffect->callDispatcher(effEditOpen, 0, 0, win, 0.0);

   HIViewRef subview = HIViewGetFirstSubview(view);
   wxASSERT(subview != NULL);

#elif defined(__WXMSW__)
   mEffect->callDispatcher(effEditOpen, 0, 0, GetHWND(), 0.0);

   HWND child = FindWindowEx((HWND) GetHWND(), NULL, NULL, NULL);
#else
#endif

   struct
   {
      short top, left, bottom, right;
   } *rect;

   mEffect->callDispatcher(effEditGetRect, 0, 0, &rect, 0.0);

   wxBoxSizer *vs = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer *hs = new wxBoxSizer(wxHORIZONTAL);
   wxSizerItem *si;

   vs->Add(BuildProgramBar(), 0, wxCENTER);
   
   si = hs->Add(rect->right - rect->left, rect->bottom - rect->top);
   vs->Add(hs, 0, wxCENTER);

   vs->Add(CreateStdButtonSizer(this, ePreviewButton|eCancelButton|eOkButton), 0, wxEXPAND);

   SetSizerAndFit(vs);

   wxPoint pos = si->GetPosition();

#if defined(__WXMAC__)
   HIViewPlaceInSuperviewAt(subview, pos.x, pos.y);

   // LL:  Some VST effects do not work unless this is done.  But, it must be
   //      done last since proper window sizing will not occur otherwise.
   ::RemoveEventHandler((EventHandlerRef)MacGetEventHandler());

#elif defined(__WXMSW__)
   SetWindowPos(child, NULL, pos.x, pos.y, 0, 0, SWP_NOSIZE | SWP_NOZORDER);
#else
#endif
}

void VSTEffectDialog::BuildPlain()
{
   wxControl *item;

   wxBoxSizer *vSizer = new wxBoxSizer(wxVERTICAL);
   vSizer->Add(BuildProgramBar(), 0,  wxALIGN_CENTER);

   wxScrolledWindow *sw = new wxScrolledWindow(this,
                                               wxID_ANY,
                                               wxDefaultPosition,
                                               wxDefaultSize,
                                               wxVSCROLL | wxTAB_TRAVERSAL);

   // Try to give the window a sensible default/minimum size
   wxSize sz = GetParent()->GetSize();
   sw->SetMinSize(wxSize(wxMax(600, sz.GetWidth() * 2 / 3), sz.GetHeight() / 2));
                                              
   sw->SetScrollRate(0, 20);
   vSizer->Add(sw, 1, wxEXPAND | wxALL, 5);

   // Preview, OK, & Cancel buttons
   vSizer->Add(CreateStdButtonSizer(this, ePreviewButton|eCancelButton|eOkButton), 0, wxEXPAND);

   SetSizer(vSizer);

   wxSizer *paramSizer = new wxStaticBoxSizer(wxVERTICAL, sw, _("Effect Settings"));

   wxFlexGridSizer *gridSizer = new wxFlexGridSizer(4, 0, 0);
   gridSizer->AddGrowableCol(1);

   wxString text;
   int w;
   int h;
   GetTextExtent(wxT("HHHHHHHH"), &w, &h);

   for (int i = 0; i < mAEffect->numParams; i++) {
      char buf[256];
      buf[0] = 0;
      mEffect->callDispatcher(effGetParamName, i, 0, buf, 0.0);
      text = LAT1CTOWX(buf);

      item = new wxStaticText(sw, 0, text + wxT(":"));
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

      item = new wxSlider(sw,
                          ID_VST_SLIDERS + i,
                          mEffect->callGetParameter(i) * 1000,
                          0,
                          1000,
                          wxDefaultPosition,
                          wxSize(200, -1));
      item->SetName(text);
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALL, 5);

      buf[0] = 0;
      mEffect->callDispatcher(effGetParamDisplay, i, 0, buf, 0.0);
      text.Printf(wxT("%8s"), LAT1CTOWX(buf).c_str());
      item = new wxStaticText(sw,
                              wxID_ANY,
                              text,
                              wxDefaultPosition,
                              wxSize(w, -1),
                              wxALIGN_RIGHT | wxST_NO_AUTORESIZE);
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

      buf[0] = 0;
      mEffect->callDispatcher(effGetParamLabel, i, 0, buf, 0.0);
      text.Printf(wxT("%-8s"), LAT1CTOWX(buf).c_str());
      item = new wxStaticText(sw,
                              wxID_ANY,
                              text,
                              wxDefaultPosition,
                              wxSize(w, -1),
                              wxALIGN_LEFT | wxST_NO_AUTORESIZE);
      gridSizer->Add(item, 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);
   }

   paramSizer->Add(gridSizer, 1, wxEXPAND | wxALL, 5);
   sw->SetSizer(paramSizer);

   Layout();
   Fit();
   SetSizeHints(GetSize());
}

wxSizer *VSTEffectDialog::BuildProgramBar()
{
   wxArrayString progs;

   for (int i = 0; i < mAEffect->numPrograms; i++) {
      char buf[256];
      buf[0] = 0;
      mEffect->callDispatcher(effGetProgramNameIndexed, i, 0, buf, 0.0);
      if (buf[0] != 0) {
         progs.Add(LAT1CTOWX(buf));
      }
   }

   if (progs.GetCount() == 0) {
      progs.Add(_("None"));
   }

   int progn = mEffect->callDispatcher(effGetProgram, 0, 0, NULL, 0.0);

   wxBoxSizer *hs = new wxBoxSizer(wxHORIZONTAL);

   wxStaticText *st = new wxStaticText(this, wxID_ANY, _("Presets:"));
   hs->Add(st, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

   mProgram = new wxComboBox(this,
                             ID_VST_PROGRAM,
                             progs[progn],
                             wxDefaultPosition,
                             wxSize(200, -1),
                             progs
                             );
   hs->Add(mProgram, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

   wxButton *bt = new wxButton(this, ID_VST_LOAD, _("Load"));
   hs->Add(bt, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

   bt = new wxButton(this, ID_VST_SAVE, _("Save"));
   hs->Add(bt, 0, wxALIGN_CENTER_VERTICAL | wxALL, 5);

   return hs;
}

void VSTEffectDialog::OnIdle(wxIdleEvent & evt)
{
   mEffect->callDispatcher(effEditIdle, 0, 0, NULL, 0.0);
}

void VSTEffectDialog::OnSlider(wxCommandEvent & evt)
{
   wxSlider *s = (wxSlider *) evt.GetEventObject();
   wxStaticText *d = (wxStaticText *) s->GetNextSibling();
   wxStaticText *l = (wxStaticText *) d->GetNextSibling();
   int i = s->GetId() - ID_VST_SLIDERS;
   char buf[256];
   wxString text;

   mEffect->callSetParameter(i, s->GetValue() / 1000.0);

   buf[0] = 0;
   mEffect->callDispatcher(effGetParamDisplay, i, 0, buf, 0.0);
   text.Printf(wxT("%8s"), LAT1CTOWX(buf).c_str());
   d->SetLabel(text);

   buf[0] = 0;
   mEffect->callDispatcher(effGetParamLabel, i, 0, buf, 0.0);
   text.Printf(wxT("%-8s"), LAT1CTOWX(buf).c_str());
   l->SetLabel(text);
}

void VSTEffectDialog::OnProgram(wxCommandEvent & evt)
{
   mEffect->callDispatcher(effSetProgram, 0, evt.GetInt(), NULL, 0.0);
}

void VSTEffectDialog::OnProgramText(wxCommandEvent & evt)
{
   wxString str = mProgram->GetValue();
   int ndx = mProgram->GetCurrentSelection();

   char name[255];
   name[0] = 0;
   mEffect->callDispatcher(effGetProgramName, ndx, 0, name, 0.0);

   if (str.Length() > 24) {
      mProgram->SetValue(LAT1CTOWX(name));
      return;
   }

   strcpy(name, str.mb_str());

   mEffect->callDispatcher(effSetProgramName, ndx, 0, name, 0.0);
}

void VSTEffectDialog::OnLoad(wxCommandEvent & evt)
{
   wxString fn;

   // Ask the user for the real name
   fn = FileSelector(_("Load VST Program:"),
                     FileNames::DataDir(),
                     wxEmptyString,
                     wxT("xml"),
                     wxT("*.xml"),
                     wxFD_OPEN | wxRESIZE_BORDER,
                     this);

   // User canceled...
   if (fn.IsEmpty()) {
      return;
   }

   // Load the program
   XMLFileReader reader;
   if (!reader.Parse(this, fn)) {
      // Inform user of load failure
      wxMessageBox(reader.GetErrorStr(),
                   _("Error loading program"),
                   wxOK | wxCENTRE,
                   this);
   }

   return;
}

void VSTEffectDialog::OnSave(wxCommandEvent & evt)
{
   int i = mProgram->GetCurrentSelection();
   wxString fn;
   char buf[255];

   // Ask the user for the real name
   fn = FileSelector(_("Save VST Program As:"),
                     FileNames::DataDir(),
                     mProgram->GetValue() + wxT(".xml"),
                     wxT("xml"),
                     wxT("*.xml"),
                     wxFD_SAVE | wxFD_OVERWRITE_PROMPT | wxRESIZE_BORDER,
                     this);

   // User canceled...
   if (fn.IsEmpty()) {
      return;
   }

   XMLFileWriter xmlFile;

   // Create/Open the file
   xmlFile.Open(fn, wxT("wb"));

   xmlFile.StartTag(wxT("vstprogrampersistence"));
   xmlFile.WriteAttr(wxT("version"), wxT("1"));

   i = mEffect->callDispatcher(effGetVendorVersion, 0, 0, NULL, 0.0);
   xmlFile.StartTag(wxT("effect"));
   xmlFile.WriteAttr(wxT("name"), mEffect->GetEffectIdentifier());
   xmlFile.WriteAttr(wxT("version"), i);

   xmlFile.StartTag(wxT("program"));
   xmlFile.WriteAttr(wxT("name"), mProgram->GetValue());

   for (i = 1; i < mAEffect->numParams; i++) {
      xmlFile.StartTag(wxT("param"));

      buf[0] = 0;
      mEffect->callDispatcher(effGetParamName, i, 0, buf, 0.0);
      xmlFile.WriteAttr(wxT("index"), i);
      xmlFile.WriteAttr(wxT("name"), LAT1CTOWX(buf));
      xmlFile.WriteAttr(wxT("value"),
                        wxString::Format(wxT("%f"),
                        mEffect->callGetParameter(i)));

      xmlFile.EndTag(wxT("param"));
   }

   xmlFile.EndTag(wxT("program"));

   xmlFile.EndTag(wxT("effect"));

   xmlFile.EndTag(wxT("vstprogrampersistence"));

   // Close the file
   xmlFile.Close();
}

void VSTEffectDialog::OnPreview(wxCommandEvent & evt)
{
   mEffect->Preview();
}

void VSTEffectDialog::OnOk(wxCommandEvent & evt)
{
   mEffect->callDispatcher(effEditClose, 0, 0, NULL, 0.0);
   EndModal(true);
}

void VSTEffectDialog::OnCancel(wxCommandEvent & evt)
{
   mEffect->callDispatcher(effEditClose, 0, 0, NULL, 0.0);
   EndModal(false);
}

bool VSTEffectDialog::HandleXMLTag(const wxChar *tag, const wxChar **attrs)
{
   if (wxStrcmp(tag, wxT("vstprogrampersistence")) == 0) {
      while (*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;
         
         if (!value) {
            break;
         }

         const wxString strValue = value;

         if (wxStrcmp(attr, wxT("version")) == 0) {
            if (!XMLValueChecker::IsGoodInt(strValue)) {
               return false;
            }
            // Nothing to do with it for now
         }
         else {
            return false;
         }
      }

      return true;
   }

   if (wxStrcmp(tag, wxT("effect")) == 0) {
      while (*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;
         
         if (!value) {
            break;
         }

         const wxString strValue = value;

         if (wxStrcmp(attr, wxT("name")) == 0) {
            if (!XMLValueChecker::IsGoodString(strValue)) {
               return false;
            }

            if (value != mEffect->GetEffectIdentifier()) {
               wxString msg;
               msg.Printf(_("This parameter file was saved from %s.  Continue?"), value);
               int result = wxMessageBox(msg, wxT("Confirm"), wxYES_NO, this);
               if (result == wxNO) {
                  return false;
               }
            }
         }
         else if (wxStrcmp(attr, wxT("version")) == 0) {
            if (!XMLValueChecker::IsGoodInt(strValue)) {
               return false;
            }
            // Nothing to do with it for now
         }
         else {
            return false;
         }
      }

      return true;
   }
      
   if (wxStrcmp(tag, wxT("program")) == 0) {
      while (*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;
         
         if (!value) {
            break;
         }

         const wxString strValue = value;

         if (wxStrcmp(attr, wxT("name")) == 0) {
            if (!XMLValueChecker::IsGoodString(strValue)) {
               return false;
            }

            if (strValue.Length() > 24) {
               return false;
            }

            int ndx = mProgram->GetCurrentSelection();
            mProgram->SetString(ndx, strValue);
            mProgram->SetValue(strValue);

            char buf[255];
            strcpy(buf, strValue.mb_str());
            mEffect->callDispatcher(effSetProgramName, ndx, 0, buf, 0.0);
         }
         else {
            return false;
         }
      }

      return true;
   }

   if (wxStrcmp(tag, wxT("param")) == 0) {
      long ndx = -1;
      double val = -1.0;
      while (*attrs) {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value) {
            break;
         }

         const wxString strValue = value;

         if (wxStrcmp(attr, wxT("index")) == 0) {
            if (!XMLValueChecker::IsGoodInt(strValue) || !strValue.ToLong(&ndx)) {
               return false;
            }

            if (ndx < 1 || ndx > mAEffect->numParams) {
               // Could be a different version of the effect...probably should
               // tell the user
               return false;
            }
         }
         else if (wxStrcmp(attr, wxT("name")) == 0) {
            if (!XMLValueChecker::IsGoodString(strValue)) {
               return false;
            }
            // Nothing to do with it for now
         }
         if (wxStrcmp(attr, wxT("index")) == 0) {
            if (!XMLValueChecker::IsGoodInt(strValue) ||
               !Internat::CompatibleToDouble(strValue, &val)) {
               return false;
            }

            if (val < 0.0 || val > 1.0) {
               return false;
            }
         }
      }

      if (ndx == -1 || val == -1.0) {
         return false;
      }

      mEffect->callSetParameter(ndx, val);

      return true;
   }

   return false;
}

XMLTagHandler *VSTEffectDialog::HandleXMLChild(const wxChar *tag)
{
   if (wxStrcmp(tag, wxT("vstprogrampersistence")) == 0) {
      return this;
   }

   if (wxStrcmp(tag, wxT("effect")) == 0) {
      return this;
   }

   if (wxStrcmp(tag, wxT("program")) == 0) {
      return this;
   }

   if (wxStrcmp(tag, wxT("param")) == 0) {
      return this;
   }

   return NULL;
}

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffect
//
///////////////////////////////////////////////////////////////////////////////

VSTEffect::VSTEffect(const wxString & path, void *module, AEffect * aeffect)
{
   SetEffectFlags(PLUGIN_EFFECT | PROCESS_EFFECT);
   mPath = path;
   mModule = module;
   mAEffect = aeffect;

   char vendor[256];
   char name[256];

   vendor[0] = 0;
   name[0] = 0;

   callDispatcher(effGetVendorString, 0, 0, vendor, 0.0);
   callDispatcher(effGetProductString, 0, 0, name, 0.0);

   mVendor = LAT1CTOWX(vendor);
   mName = LAT1CTOWX(name);

   if (mVendor.IsEmpty()) {
      mVendor = wxT("VST");
   }

   if (mName.IsEmpty()) {
      wxFileName fn(mPath);
      mName = fn.GetName();
   }

   mBuffer = NULL;
   mInBuffer = NULL;
   mOutBuffer = NULL;
}

VSTEffect::~VSTEffect()
{
   callDispatcher(effClose, 0, 0, NULL, 0.0);

#if defined(__WXMAC__)
   CFRelease((CFBundleRef) mModule);
#else
   delete (wxDynamicLibrary *) mModule;
#endif
}

wxString VSTEffect::GetEffectName()
{
   if (mVendor.IsEmpty()) {
      return mName + wxT("...");
   }

   return mVendor + wxT(": ") + mName + wxT("...");
}

wxString VSTEffect::GetEffectIdentifier()
{
   return mName;
}

std::set<wxString> VSTEffect::GetEffectCategories()
{
   return std::set<wxString>();
}

wxString VSTEffect::GetEffectAction()
{
   return _("Performing Effect: ") + mName;
}

bool VSTEffect::Init()
{
   mInputs = mAEffect->numInputs;
   mOutputs = mAEffect->numOutputs;
   mBlockSize = 0;

   TrackListOfKindIterator iter(Track::Wave, mTracks);
   WaveTrack *left = (WaveTrack *) iter.First();
   while (left) {
      sampleCount lstart, rstart = 0;
      sampleCount llen, rlen = 0;
      GetSamples(left, &lstart, &llen);
      
      if (left->GetLinked()) {
         WaveTrack *right = (WaveTrack *) iter.Next();
         GetSamples(right, &rstart, &rlen);         

         if (left->GetRate() != right->GetRate()) {
            wxMessageBox(_("Sorry, VST Effects cannot be performed on stereo tracks where the individual channels of the track do not match."));
            return false;
         }

         if (llen != rlen) {
            wxMessageBox(_("Both channels of a stereo track must be the same length."));
            return false;
         }
      }
      
      left = (WaveTrack *) iter.Next();
   }

   return true;
}

bool VSTEffect::PromptUser()
{
   VSTEffectDialog dlog(mParent, mName, this, mAEffect);
   dlog.CentreOnParent();
   dlog.ShowModal();

   return dlog.GetReturnCode() != 0;
}

bool VSTEffect::Process()
{
   this->CopyInputWaveTracks(); // Set up m_pOutputWaveTracks.
   bool bGoodResult = true;

   TrackListIterator iter(mOutputWaveTracks);
   int count = 0;
   Track *left = iter.First();
   Track *right;
   while(left) {
      sampleCount lstart, rstart = 0;
      sampleCount len;
      GetSamples((WaveTrack *)left, &lstart, &len);

      right = NULL;
      if (left->GetLinked() && mInputs>1) {
         right = iter.Next();         
         GetSamples((WaveTrack *)right, &rstart, &len);
      }

      // Reset the effect
      callDispatcher(effOpen, 0, 0, NULL, 0.0);
      
      bGoodResult = ProcessStereo(count,
                                  (WaveTrack *)left, (WaveTrack *)right,
                                  lstart, rstart, len);

      if (!bGoodResult)
         break;
   
      left = iter.Next();
      count++;
   }
   
   ReplaceProcessedWaveTracks(bGoodResult); 
   return bGoodResult;
}

bool VSTEffect::ProcessStereo(int count, WaveTrack *left, WaveTrack *right,
                              sampleCount lstart,
                              sampleCount rstart, sampleCount len)
{
   bool rc = true;
   if (mBlockSize == 0) {
      mBlockSize = left->GetMaxBlockSize() * 2;

      mBuffer = new float[mBlockSize];
      mInBuffer = new float *[mInputs];
      int i;
      for (i = 0; i < mInputs; i++)
         mInBuffer[i] = new float[mBlockSize];
      mOutBuffer = new float *[mOutputs];
      for (i = 0; i < mOutputs; i++)
         mOutBuffer[i] = new float[mBlockSize];

   }

   callDispatcher(effSetSampleRate, 0, 0, NULL, (float) left->GetRate());
   callDispatcher(effSetBlockSize, 0, mBlockSize * 2, NULL, 0.0);

   // Actually perform the effect here

   sampleCount originalLen = len;
   sampleCount ls = lstart;
   sampleCount rs = rstart;
   while (len) {
      int i;
      int block = mBlockSize;
      if (block > len)
         block = len;

      left->Get((samplePtr)mBuffer, floatSample, ls, block);
      for (i = 0; i < block; i++)
         mInBuffer[0][i] = mBuffer[i];
      if (right) {
         right->Get((samplePtr)mBuffer, floatSample, rs, block);
         for (i = 0; i < block; i++)
            mInBuffer[1][i] = mBuffer[i];
      }

      callProcessReplacing(mInBuffer, mOutBuffer, block);

      for (i = 0; i < block; i++)
         mBuffer[i] = mOutBuffer[0][i];
      left->Set((samplePtr)mBuffer, floatSample, ls, block);
      
      if (right) {
         for (i = 0; i < block; i++)
            mBuffer[i] = mOutBuffer[1][i];
         right->Set((samplePtr)mBuffer, floatSample, rs, block);
      }      

      len -= block;
      ls += block;
      rs += block;
      
      if (mInputs > 1) {      
         if (TrackGroupProgress(count, (ls-lstart)/(double)originalLen)) {
            rc = false;
            break;
         }
      }
      else {
         if (TrackProgress(count, (ls-lstart)/(double)originalLen)) {
            rc = false;
            break;
         }
      }
   }

   return rc;
}

void VSTEffect::End()
{
   if (mBuffer) {
      int i;

      delete[]mBuffer;
      for (i = 0; i < mInputs; i++)
         delete mInBuffer[i];
      for (i = 0; i < mOutputs; i++)
         delete mOutBuffer[i];
      delete[]mInBuffer;
      delete[]mOutBuffer;

   }
   mBuffer = NULL;
   mInBuffer = NULL;
   mOutBuffer = NULL;
}

long VSTEffect::callDispatcher(long opcode,
                               long index, long value, void *ptr, float opt)
{
   return mAEffect->dispatcher(mAEffect, opcode, index, value, ptr, opt);
}

void VSTEffect::callProcess(float **inputs, float **outputs, long sampleframes)
{
   mAEffect->process(mAEffect, inputs, outputs, sampleframes);
}

void VSTEffect::callProcessReplacing(float **inputs,
                                     float **outputs, long sampleframes)
{
   mAEffect->processReplacing(mAEffect, inputs, outputs, sampleframes);
}

void VSTEffect::callSetParameter(long index, float parameter)
{
   mAEffect->setParameter(mAEffect, index, parameter);
}

float VSTEffect::callGetParameter(long index)
{
   return mAEffect->getParameter(mAEffect, index);
}

#endif // USE_VST


// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: dae3d099-e1eb-494e-b8eb-8f0af7f674d2

