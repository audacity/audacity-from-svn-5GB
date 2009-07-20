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

#include "VSTEffect.h"

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
   virtual ~VSTEffectDialog();

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
   void RefreshParameters(int skip = -1);

   virtual bool HandleXMLTag(const wxChar *tag, const wxChar **attrs);
   virtual void HandleXMLEndTag(const wxChar *tag);
   virtual void HandleXMLContent(const wxString & content);
   virtual XMLTagHandler *HandleXMLChild(const wxChar *tag);
   wxString b64encode(const void *in, int len);
   int b64decode(wxString in, void *out);

   VSTEffect *mEffect;
   AEffect *mAEffect;

   bool mGui;

   wxComboBox *mProgram;
   wxStaticText **mNames;
   wxSlider **mSliders;
   wxStaticText **mDisplays;
   wxStaticText **mLabels;

   bool mInChunk;
   wxString mChunk;

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
   mNames = NULL;
   mSliders = NULL;
   mDisplays = NULL;
   mLabels = NULL;

   // Determine if the VST editor is supposed to be used or not
   mGui = (gPrefs->Read(wxT("/VST/GUI"), (long) true) != 0) &&
          mAEffect->flags & effFlagsHasEditor;

   // Build the appropriate dialog type
   if (mGui) {
      BuildFancy();
   }
   else {
      BuildPlain();
   }
}

VSTEffectDialog::~VSTEffectDialog()
{
   if (mNames) {
      delete [] mNames;	
   }

   if (mSliders) {
      delete [] mSliders;
   }

   if (mDisplays) {
      delete [] mDisplays;
   }

   if (mLabels) {
      delete [] mLabels;
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
   wxWindow *w = new wxPanel(this, wxID_ANY);

   mEffect->callDispatcher(effEditOpen, 0, 0, w->GetHWND(), 0.0);
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
   w->SetPosition(pos);
   w->SetSize(si->GetSize());
#else
#endif
}

void VSTEffectDialog::BuildPlain()
{
   mNames = new wxStaticText *[mAEffect->numParams];
   mSliders = new wxSlider *[mAEffect->numParams];
   mDisplays = new wxStaticText *[mAEffect->numParams];
   mLabels = new wxStaticText *[mAEffect->numParams];

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

   // Find the longest parameter name.
   int namew = 0;
   int w;
   int h;
   for (int i = 0; i < mAEffect->numParams; i++) {
      wxString text = mEffect->GetString(effGetParamName, i);
      if (text.Right(1) != wxT(':')) {
         text += wxT(':');
      }
      GetTextExtent(text, &w, &h);
      if (w > namew) {
         namew = w;
      }
   }

   GetTextExtent(wxT("HHHHHHHH"), &w, &h);

   for (int i = 0; i < mAEffect->numParams; i++) {
      mNames[i] = new wxStaticText(sw,
                                    wxID_ANY,
                                    wxEmptyString,
                                    wxDefaultPosition,
                                    wxSize(namew, -1),
                                    wxALIGN_RIGHT | wxST_NO_AUTORESIZE);
      gridSizer->Add(mNames[i], 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

      mSliders[i] = new wxSlider(sw,
                                 ID_VST_SLIDERS + i,
                                 0,
                                 0,
                                 1000,
                                 wxDefaultPosition,
                                 wxSize(200, -1));
      gridSizer->Add(mSliders[i], 0, wxALIGN_CENTER_VERTICAL | wxEXPAND | wxALL, 5);

      mDisplays[i] = new wxStaticText(sw,
                                      wxID_ANY,
                                      wxEmptyString,
                                      wxDefaultPosition,
                                      wxSize(w, -1),
                                      wxALIGN_RIGHT | wxST_NO_AUTORESIZE);
      gridSizer->Add(mDisplays[i], 0, wxALIGN_CENTER_VERTICAL | wxALIGN_RIGHT | wxALL, 5);

      mLabels[i] = new wxStaticText(sw,
                                     wxID_ANY,
                                     wxEmptyString,
                                     wxDefaultPosition,
                                     wxSize(w, -1),
                                     wxALIGN_LEFT | wxST_NO_AUTORESIZE);
      gridSizer->Add(mLabels[i], 0, wxALIGN_CENTER_VERTICAL | wxALIGN_LEFT | wxALL, 5);
   }

   paramSizer->Add(gridSizer, 1, wxEXPAND | wxALL, 5);
   sw->SetSizer(paramSizer);

   Layout();
   Fit();
   SetSizeHints(GetSize());
   RefreshParameters();

   mSliders[0]->SetFocus();
}

wxSizer *VSTEffectDialog::BuildProgramBar()
{
   wxArrayString progs;

   for (int i = 0; i < mAEffect->numPrograms; i++) {
      wxString name = mEffect->GetString(effGetProgramNameIndexed, i);
      if (!name.IsEmpty()) {
         progs.Add(name);
      }
   }

   if (progs.GetCount() == 0) {
      progs.Add(_("None"));
   }

   int progn = mEffect->callDispatcher(effGetProgram, 0, 0, NULL, 0.0);

   if (progn < 0) {
      progn = 0;
   }

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

void VSTEffectDialog::RefreshParameters(int skip)
{
   if (!mGui) {
      for (int i = 0; i < mAEffect->numParams; i++) {
         wxString text = mEffect->GetString(effGetParamName, i).Trim(true).Trim(false);
         wxString name = text;

         if (text.Right(1) != wxT(':')) {
            text += wxT(':');
         }
         mNames[i]->SetLabel(text);

         // For some parameters types like on/off, setting the slider value has
         // a side effect that causes it to only move when the parameter changes
         // from off to on.  However, this prevents changing the value using the
         // keyboard, so we skip the active slider if any.
         if (i != skip) {
            mSliders[i]->SetValue(mEffect->callGetParameter(i) * 1000);
         }
         name = text;

         text = mEffect->GetString(effGetParamDisplay, i);
         if (text.IsEmpty()) {
            text.Printf(wxT("%.5g"),mEffect->callGetParameter(i));
         }
         mDisplays[i]->SetLabel(wxString::Format(wxT("%8s"), text.c_str()));
         name += wxT(' ') + text;

         text = mEffect->GetString(effGetParamDisplay, i);
         if (!text.IsEmpty()) {
            text.Printf(wxT("%-8s"), mEffect->GetString(effGetParamLabel, i).c_str());
            mLabels[i]->SetLabel(wxString::Format(wxT("%8s"), text.c_str()));
            name += wxT(' ') + text;
         }

         mSliders[i]->SetName(name);
      }
   }
}

void VSTEffectDialog::OnIdle(wxIdleEvent & evt)
{
   if (mGui) {
      mEffect->callDispatcher(effEditIdle, 0, 0, NULL, 0.0);
   }
}

void VSTEffectDialog::OnSlider(wxCommandEvent & evt)
{
   wxSlider *s = (wxSlider *) evt.GetEventObject();
   int i = s->GetId() - ID_VST_SLIDERS;

   mEffect->callSetParameter(i, s->GetValue() / 1000.0);

   RefreshParameters(i);
}

void VSTEffectDialog::OnProgram(wxCommandEvent & evt)
{
   mEffect->callDispatcher(effSetProgram, 0, evt.GetInt(), NULL, 0.0);
   RefreshParameters();
}

void VSTEffectDialog::OnProgramText(wxCommandEvent & evt)
{
   wxString name = mProgram->GetValue();
   int i = mEffect->callDispatcher(effGetProgram, 0, 0, NULL, 0.0);
   int ip = mProgram->GetInsertionPoint();

   // Limit the length of the string
   if (name.Length() > 24) {
      name = name.Left(24);
   }

   mEffect->SetString(effSetProgramName, name, i);
   mProgram->SetString(i, name);

   // On Windows, must reselect after doing a SetString()...at least that's
   // what seems to be required.
   mProgram->SetStringSelection(name);

   // Which also means we have to reposition the caret.
   if (ip >= 0) {
      mProgram->SetInsertionPoint(ip);
   }
   
   RefreshParameters();
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

   RefreshParameters();

   return;
}

void VSTEffectDialog::OnSave(wxCommandEvent & evt)
{
   int i = mProgram->GetCurrentSelection();
   wxString fn;

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

   long clen = 0;
   if (mAEffect->flags & effFlagsProgramChunks) {
      void *chunk = NULL;

      clen = mEffect->callDispatcher(effGetChunk, 1, 0, &chunk, 0.0);
      if (clen != 0) {
         xmlFile.StartTag(wxT("chunk"));
         xmlFile.WriteSubTree(b64encode(chunk, clen) + wxT('\n'));
         xmlFile.EndTag(wxT("chunk"));
      }
   }

   if (clen == 0) {
      for (i = 0; i < mAEffect->numParams; i++) {
         xmlFile.StartTag(wxT("param"));

         xmlFile.WriteAttr(wxT("index"), i);
         xmlFile.WriteAttr(wxT("name"),
                           mEffect->GetString(effGetParamName, i));
         xmlFile.WriteAttr(wxT("value"),
                           wxString::Format(wxT("%f"),
                           mEffect->callGetParameter(i)));

         xmlFile.EndTag(wxT("param"));
      }
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
   if (mGui) {
      mEffect->callDispatcher(effEditClose, 0, 0, NULL, 0.0);
   }

   EndModal(true);
}

void VSTEffectDialog::OnCancel(wxCommandEvent & evt)
{
   if (mGui) {
      mEffect->callDispatcher(effEditClose, 0, 0, NULL, 0.0);
   }

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

            mEffect->SetString(effSetProgramName, strValue, ndx);
         }
         else {
            return false;
         }
      }

      mInChunk = false;

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
         else if (wxStrcmp(attr, wxT("value")) == 0) {
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

   if (wxStrcmp(tag, wxT("chunk")) == 0) {
      mInChunk = true;
      return true;
   }

   return false;
}

void VSTEffectDialog::HandleXMLEndTag(const wxChar *tag)
{
   if (wxStrcmp(tag, wxT("chunk")) == 0) {
      if (mChunk.Length()) {
         char *buf = new char[mChunk.Length() / 4 * 3];

         int len = b64decode(mChunk, buf);
         if (len) {
            mEffect->callDispatcher(effSetChunk, 1, len, buf, 0.0);
         }

         delete [] buf;
         mChunk.Clear();
      }
      mInChunk = false;
   }
}

void VSTEffectDialog::HandleXMLContent(const wxString & content)
{
   if (mInChunk) {
      mChunk += wxString(content).Trim(true).Trim(false);
   }
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

   if (wxStrcmp(tag, wxT("chunk")) == 0) {
      return this;
   }

   return NULL;
}

////////////////////////////////////////////////////////////////////////////////
// Base64 en/decoding
//
// Original routines marked as public domain and found at:
//
// http://en.wikibooks.org/wiki/Algorithm_implementation/Miscellaneous/Base64
//
////////////////////////////////////////////////////////////////////////////////

// Lookup table for encoding
const static wxChar cset[] = wxT("ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/");
const static char padc = wxT('=');

wxString VSTEffectDialog::b64encode(const void *in, int len)
{
   unsigned char *p = (unsigned char *) in;
   wxString out;
   
	unsigned long temp;
	for (int i = 0; i < len / 3; i++) {
		temp  = (*p++) << 16; //Convert to big endian
		temp += (*p++) << 8;
		temp += (*p++);
		out += cset[(temp & 0x00FC0000) >> 18];
		out += cset[(temp & 0x0003F000) >> 12];
		out += cset[(temp & 0x00000FC0) >> 6];
		out += cset[(temp & 0x0000003F)];
	}

	switch (len % 3)
	{
      case 1:
         temp  = (*p++) << 16; //Convert to big endian
         out += cset[(temp & 0x00FC0000) >> 18];
         out += cset[(temp & 0x0003F000) >> 12];
         out += padc;
         out += padc;
         break;

      case 2:
         temp  = (*p++) << 16; //Convert to big endian
         temp += (*p++) << 8;
         out += cset[(temp & 0x00FC0000) >> 18];
         out += cset[(temp & 0x0003F000) >> 12];
         out += cset[(temp & 0x00000FC0) >> 6];
         out += padc;
         break;
	}

	return out;
}

int VSTEffectDialog::b64decode(wxString in, void *out)
{
   int len = in.Length();
   unsigned char *p = (unsigned char *) out;

	if (len % 4) { //Sanity check
		return 0;
   }

   int padding = 0;
   if (len) {
      if (in[len - 1] == padc) {
         padding++;
      }

      if (in[len - 2] == padc) {
         padding++;
      }
   }

   const char *a = in.mb_str();
	//Setup a vector to hold the result
   unsigned long temp = 0; //Holds decoded quanta
   int i = 0;
   while (i < len) {
      for (int quantumPosition = 0; quantumPosition < 4; quantumPosition++) {
         unsigned char c = in[i];
         temp <<= 6;

         if (c >= 0x41 && c <= 0x5A) {
            temp |= c - 0x41;
         }
         else if (c >= 0x61 && c <= 0x7A) {
            temp |= c - 0x47;
         }
         else if (c >= 0x30 && c <= 0x39) {
            temp |= c + 0x04;
         }
         else if (c == 0x2B) {
            temp |= 0x3E;
         }
         else if (c == 0x2F) {
            temp |= 0x3F;
         }
         else if (c == padc) {
            switch (len - i)
            {
               case 1: //One pad character
                  *p++ = (temp >> 16) & 0x000000FF;
                  *p++ = (temp >> 8) & 0x000000FF;
                  return (p - (unsigned char *) out) - 1;
               case 2: //Two pad characters
                  *p++ = (temp >> 10) & 0x000000FF;
                  return (p - (unsigned char *) out) - 1;
            }
         }
         i++;
		}
      *p++ = (temp >> 16) & 0x000000FF;
      *p++ = (temp >> 8) & 0x000000FF;
      *p++ = temp & 0x000000FF;
   }

   return (p - (unsigned char *) out) - 1;
}

///////////////////////////////////////////////////////////////////////////////
//
// VSTEffect
//
///////////////////////////////////////////////////////////////////////////////

VSTEffect::VSTEffect(const wxString & path, void *module, AEffect * aeffect)
:  mPath(path),
   mModule(module),
   mAEffect(aeffect)
{
   mBlockSize = 0;
   mInBuffer = NULL;
   mOutBuffer = NULL;
   mInputs = 0;
   mOutputs = 0;
   mChannels = 0;

   mAEffect->user = this;

   mTimeInfo.samplePos = 0.0;
   mTimeInfo.sampleRate = 44100.0;
   mTimeInfo.tempo = 120.0;
   mTimeInfo.timeSigNumerator = 4;
   mTimeInfo.timeSigDenominator = 4;
   mTimeInfo.flags = kVstTempoValid;

   callDispatcher(effOpen, 0, 0, NULL, 0.0);

   mVendor = GetString(effGetVendorString);
   mName = GetString(effGetProductString);

   if (mVendor.IsEmpty()) {
      mVendor = wxT("VST");
   }

   if (mName.IsEmpty()) {
      wxFileName fn(mPath);
      mName = fn.GetName();
   }

   mInputs = mAEffect->numInputs;
   mOutputs = mAEffect->numOutputs;

   int flags = PLUGIN_EFFECT;
   if (mInputs == 0) {
      flags |= INSERT_EFFECT;
   }
   else if (mOutputs == 0) {
      flags |= ANALYZE_EFFECT;
   }
   else {
      flags |= PROCESS_EFFECT;
   }

   SetEffectFlags(flags);
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
   mBlockSize = 0;

   TrackListIterator iter(mOutputTracks);
   WaveTrack *left = (WaveTrack *) iter.First();
   while (left) {
      sampleCount lstart;
      sampleCount llen;

      GetSamples(left, &lstart, &llen);
      
      if (left->GetLinked()) {
         WaveTrack *right = (WaveTrack *) iter.Next();
         sampleCount rstart;
         sampleCount rlen;

         GetSamples(right, &rstart, &rlen);         

         if (left->GetRate() != right->GetRate()) {
            wxMessageBox(_("Both channels of a stereo track must be the same sample rate."));
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
   CopyInputTracks();
   bool bGoodResult = true;

   mInBuffer = NULL;
   mOutBuffer = NULL;

   TrackListIterator iter(mOutputTracks);
   int count = 0;
   bool clear = false;
   WaveTrack *left = (WaveTrack *) iter.First();
   while (left) {
      WaveTrack *right;
      sampleCount len;
      sampleCount lstart;
      sampleCount rstart;

      GetSamples(left, &lstart, &len);

      mChannels = 1;

      right = NULL;
      rstart = 0;
      if (left->GetLinked() && mInputs>1) {
         right = (WaveTrack *) iter.Next();         
         GetSamples(right, &rstart, &len);
         clear = false;
         mChannels = 2;
      }

      if (mBlockSize == 0) {
         mBlockSize = left->GetMaxBlockSize() * 2;

         // Some VST effects (Antress Modern is an example), do not like
         // overly large block sizes.  Unfortunately, I have not found a
         // way to determine if the effect has a maximum it will support,
         // so just limit to small value for now.  This will increase
         // processing time and, it's a shame, because most plugins seem
         // to be able to handle much larger sizes.
         if (mBlockSize > 8192) { // The Antress limit
            mBlockSize = 8192;
         }

         mInBuffer = new float *[mInputs];
         for (int i = 0; i < mInputs; i++) {
            mInBuffer[i] = new float[mBlockSize];
         }

         mOutBuffer = new float *[mOutputs];
         for (int i = 0; i < mOutputs; i++) {
            mOutBuffer[i] = new float[mBlockSize];
         }

         // Turn the power off
         callDispatcher(effMainsChanged, 0, 0, NULL, 0.0);

         // Set processing parameters
         callDispatcher(effSetSampleRate, 0, 0, NULL, left->GetRate());
         callDispatcher(effSetBlockSize, 0, mBlockSize, NULL, 0.0);
      }

      // Clear unused input buffers
      if (!right && !clear) {
         for (int i = 1; i < mInputs; i++) {
            for (int j = 0; j < mBlockSize; j++) {
               mInBuffer[i][j] = 0.0;
            }
         }
         clear = true;
      }

      bGoodResult = ProcessStereo(count, left, right, lstart, rstart, len);
      if (!bGoodResult) {
         break;
      }

      left = (WaveTrack *) iter.Next();
      count++;
   }

   if (mOutBuffer) {
      for (int i = 0; i < mOutputs; i++) {
         delete mOutBuffer[i];
      }
      delete [] mOutBuffer;
      mOutBuffer = NULL;
   }

   if (mInBuffer) {
      for (int i = 0; i < mInputs; i++) {
         delete mInBuffer[i];
      }
      delete [] mInBuffer;
      mInBuffer = NULL;
   }

   ReplaceProcessedTracks(bGoodResult); 
   return bGoodResult;
}

bool VSTEffect::ProcessStereo(int count,
                              WaveTrack *left, WaveTrack *right,
                              sampleCount lstart, sampleCount rstart,
                              sampleCount len)
{
   bool rc = true;

   // Initialize time info
   mTimeInfo.samplePos = 0.0;
   mTimeInfo.sampleRate = left->GetRate();
   mTimeInfo.flags |= kVstTransportPlaying;

   // Turn the power on
   callDispatcher(effMainsChanged, 0, 1, NULL, 0.0);

   // Actually perform the effect here
   sampleCount originalLen = len;
   sampleCount ls = lstart;
   sampleCount rs = rstart;
   while (len) {
      int block = mBlockSize;
      if (block > len) {
         block = len;
      }

      left->Get((samplePtr)mInBuffer[0], floatSample, ls, block);
      if (right) {
         right->Get((samplePtr)mInBuffer[1], floatSample, rs, block);
      }

      callProcessReplacing(mInBuffer, mOutBuffer, block);

      left->Set((samplePtr)mOutBuffer[0], floatSample, ls, block);
      if (right) {
         right->Set((samplePtr)mOutBuffer[1], floatSample, rs, block);
      }      

      len -= block;
      ls += block;
      rs += block;
      mTimeInfo.samplePos += ((double) block / mTimeInfo.sampleRate);

      if (mInputs > 1) {      
         if (TrackGroupProgress(count, (ls - lstart) / (double)originalLen)) {
            rc = false;
            break;
         }
      }
      else {
         if (TrackProgress(count, (ls - lstart) / (double)originalLen)) {
            rc = false;
            break;
         }
      }
   }

   // Turn the power off
   callDispatcher(effMainsChanged, 0, 0, NULL, 0.0);

   // No longer playing
   mTimeInfo.samplePos = 0.0;
   mTimeInfo.sampleRate = 44100.0;
   mTimeInfo.tempo = 120.0;
   mTimeInfo.timeSigNumerator = 4;
   mTimeInfo.timeSigDenominator = 4;
   mTimeInfo.flags = kVstTempoValid;

   return rc;
}

void VSTEffect::End()
{
}

wxString VSTEffect::GetString(int opcode, int index)
{
   char buf[256];

   buf[0] = '\0';

   callDispatcher(opcode, index, 0, buf, 0.0);

   return LAT1CTOWX(buf);
}

void VSTEffect::SetString(int opcode, const wxString & str, int index)
{
   char buf[256];

   strcpy(buf, str.Left(255).mb_str());

   callDispatcher(opcode, index, 0, buf, 0.0);
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

long int VSTEffect::audioMaster(AEffect * effect,
                                long int opcode,
                                long int index,
                                long int value,
                                void * ptr,
                                float opt)
{
   switch (opcode)
   {
      // Let the effect know if a pin (channel in our case) is connected
      case audioMasterPinConnected:
         return (index < mChannels ? 0 : 1);

      // Some (older) effects depend on an effIdle call when requested.  An
      // example is the Antress Modern plugins which uses the call to update
      // the editors display when the program (preset) changes.
      case audioMasterNeedIdle:
         effect->dispatcher(effect, effIdle, 0, 0, NULL, 0.0);
         return 0;

      // Give the effect a chance to update the editor display
      case audioMasterUpdateDisplay:
         effect->dispatcher(effect, effEditIdle, 0, 0, NULL, 0.0);
         return 0;

      // Return the current time info.
      case audioMasterGetTime:
         return (long int) &mTimeInfo;

      // Ignore these
      case audioMasterBeginEdit:
      case audioMasterEndEdit:
      case audioMasterAutomate:
      case audioMasterGetCurrentProcessLevel:
         return 0;

      case audioMasterCanDo:
#if !defined(__WXMSW__)
         wxPrintf(wxT("effect: %p cando: %s\n"), effect, LAT1CTOWX((char *)ptr).c_str());
#else
         wxLogDebug(wxT("effect: %p cando: %s\n"), effect, LAT1CTOWX((char *)ptr).c_str());
#endif
         return 0;

      default:
#if 1
#if defined(__WXDEBUG__)
#if !defined(__WXMSW__)
         wxPrintf(wxT("effect: %p opcode: %d index: %d value: %d ptr: %p opt: %f user: %p\n"),
                  effect, opcode, index, value, ptr, opt, effect->user);
#else
         wxLogDebug(wxT("effect: %p opcode: %d index: %d value: %d ptr: %p opt: %f user: %p"),
                    effect, opcode, index, value, ptr, opt, effect->user);
#endif
#endif
#endif
         return 0;
   }

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

