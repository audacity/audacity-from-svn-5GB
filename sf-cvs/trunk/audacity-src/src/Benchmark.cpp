/**********************************************************************

  Audacity: A Digital Audio Editor

  Benchmark.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/log.h>
#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/choice.h>
#include <wx/dialog.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
#include <wx/timer.h>
#include <wx/utils.h>
#include <wx/valgen.h>
#include <wx/valtext.h>
#include <wx/intl.h>

#include "Benchmark.h"
#include "Project.h"
#include "WaveTrack.h"
#include "Sequence.h"

class BenchmarkDialog: public wxDialog
{
public:
   // constructors and destructors
   BenchmarkDialog( wxWindow *parent );
   
   wxSizer *MakeBenchmarkDialog( wxWindow *parent, bool call_fit = TRUE,
                                 bool set_sizer = TRUE );
   
private:
   // WDR: handler declarations
   void OnRun( wxCommandEvent &event );
   void OnSave( wxCommandEvent &event );
   void OnClear( wxCommandEvent &event );
   void OnClose( wxCommandEvent &event );

   void Printf(const wxChar *format, ...);
   void HoldPrint(bool hold);
   void FlushPrint();

   bool      mHoldPrint;
   wxString  mToPrint;

   wxString  mBlockSizeStr;
   wxString  mDataSizeStr;
   wxString  mNumEditsStr;
   wxString  mRandSeedStr;

   bool      mBlockDetail;
   bool      mEditDetail;

   wxTextCtrl  *mText;
   
private:
   DECLARE_EVENT_TABLE()
};

void RunBenchmark(wxWindow *parent)
{
   /*
   int action = wxMessageBox(_("This will close all project windows "
                               "(without saving)\n"
                               "and open the Audacity Benchmark dialog.\n\n"
                               "Are you sure you want to do this?"),
                             _("Benchmark"),
                             wxYES_NO | wxICON_EXCLAMATION,
                             NULL);

   if (action != wxYES)
      return;

   CloseAllProjects();
   */

   BenchmarkDialog dlog(parent);

   dlog.CentreOnParent();

   dlog.ShowModal();
}

//
// BenchmarkDialog
//

enum {
   RunID = 1000,
   BSaveID,
   ClearID,
   BCloseID,
   StaticTextID,
   BlockSizeID,
   DataSizeID,
   NumEditsID,
   RandSeedID
};

BEGIN_EVENT_TABLE(BenchmarkDialog,wxDialog)
   EVT_BUTTON( RunID,   BenchmarkDialog::OnRun )
   EVT_BUTTON( BSaveID,  BenchmarkDialog::OnSave )
   EVT_BUTTON( ClearID, BenchmarkDialog::OnClear )
   EVT_BUTTON( BCloseID, BenchmarkDialog::OnClose )
END_EVENT_TABLE()

BenchmarkDialog::BenchmarkDialog(wxWindow *parent):
      wxDialog( parent, 0, _("Benchmark"),
                wxDefaultPosition, wxDefaultSize,
                wxDEFAULT_DIALOG_STYLE |
                wxDIALOG_MODAL |
                wxRESIZE_BORDER)
{
   mBlockSizeStr = wxT("64");
   mNumEditsStr = wxT("100");
   mDataSizeStr = wxT("32");
   mRandSeedStr = wxT("234657");

   mBlockDetail = false;
   mEditDetail = false;

   HoldPrint(false);

   MakeBenchmarkDialog( this );
}

// WDR: handler implementations for BenchmarkDialog

void BenchmarkDialog::OnClose(wxCommandEvent &event)
{
   EndModal(0);
}

wxSizer *BenchmarkDialog::MakeBenchmarkDialog( wxWindow *parent, bool call_fit, bool set_sizer )
{
   wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
   wxFlexGridSizer *gridSizer = new wxFlexGridSizer(4, 0, 0);

   wxControl *item;
   
   // Strings don't need to be translated because this class doesn't
   // ever get used in a stable release.

   item = new wxStaticText(parent, StaticTextID, wxT("Disk Block Size (KB):"));
   gridSizer->Add(item, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 5);

   item = new wxTextCtrl(parent, BlockSizeID, wxT(""), wxDefaultPosition,
                         wxSize(100, -1), 0,
                         wxTextValidator(wxFILTER_NUMERIC,
                                         &mBlockSizeStr));
   gridSizer->Add(item, 0, wxALL, 5);

   item = new wxStaticText(parent, StaticTextID, wxT("Number of Edits :"));
   gridSizer->Add(item, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 5);

   item = new wxTextCtrl(parent, NumEditsID, wxT(""), wxDefaultPosition,
                         wxSize(100, -1), 0,
                         wxTextValidator(wxFILTER_NUMERIC,
                                         &mNumEditsStr));
   gridSizer->Add(item, 0, wxALL, 5);

   item = new wxStaticText(parent, StaticTextID, wxT("Test Data Size (MB):"));
   gridSizer->Add(item, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 5);

   item = new wxTextCtrl(parent, DataSizeID, wxT(""), wxDefaultPosition,
                         wxSize(100, -1), 0,
                         wxTextValidator(wxFILTER_NUMERIC,
                                         &mDataSizeStr));
   gridSizer->Add(item, 0, wxALL, 5);

   item = new wxStaticText(parent, StaticTextID, wxT("Random Seed :"));
   gridSizer->Add(item, 0, wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL|wxALL, 5);

   item = new wxTextCtrl(parent, RandSeedID, wxT(""), wxDefaultPosition,
                         wxSize(100, -1), 0,
                         wxTextValidator(wxFILTER_NUMERIC,
                                         &mRandSeedStr));
   gridSizer->Add(item, 0, wxALL, 5);

   mainSizer->Add(gridSizer, 0, wxALL, 5);

   item = new wxCheckBox(parent, 0,
                         wxT("Show detailed info about each block file"),
                         wxDefaultPosition, wxDefaultSize, 0,
                         wxGenericValidator(&mBlockDetail));
   mainSizer->Add(item, 0, wxALL, 5);

   item = new wxCheckBox(parent, 0,
                         wxT("Show detailed info about each editing operation"),
                         wxDefaultPosition, wxDefaultSize, 0,
                         wxGenericValidator(&mEditDetail));
   mainSizer->Add(item, 0, wxALL, 5);

   mText = new wxTextCtrl(parent, StaticTextID, wxT(""),
                          wxDefaultPosition,
                          wxSize(-1, 200),
                          wxTE_MULTILINE |
                          wxTE_READONLY);
   mainSizer->Add(mText, 1, wxALL | wxEXPAND, 5);

   wxBoxSizer *hSizer = new wxBoxSizer(wxHORIZONTAL);

   item = new wxButton( parent, RunID, wxT("Run"));
   hSizer->Add(item, 0, wxALL, 5 );

   item = new wxButton( parent, BSaveID, wxT("Save"));
   hSizer->Add(item, 0, wxALL, 5 );

   item = new wxButton( parent, ClearID, wxT("Clear"));
   hSizer->Add(item, 0, wxALL, 5 );

   hSizer->Add(5, 5, 1, wxEXPAND);

   item = new wxButton( parent, BCloseID, _("Close"));
   hSizer->Add(item, 0, wxALL, 5 );

   mainSizer->Add(hSizer, 0, wxEXPAND, 5);   

   if (set_sizer) {
      parent->SetAutoLayout( TRUE );
      parent->SetSizer( mainSizer );
      if (call_fit) {
         mainSizer->Fit( parent );
         mainSizer->SetSizeHints( parent );
      }
   }
    
   return mainSizer;
}

void BenchmarkDialog::OnSave( wxCommandEvent &event )
{
   wxString fName = wxT("benchmark.txt");

   fName = wxFileSelector(wxT("Export Benchmark Data As:"),
                          NULL, fName, wxT("txt"), wxT("*.txt"), wxSAVE, this);

   if (fName == wxT(""))
      return;

   mText->SaveFile(fName);
}

void BenchmarkDialog::OnClear(wxCommandEvent &event)
{
   mText->Clear();
}

void BenchmarkDialog::Printf(const wxChar *format, ...)
{
   va_list argptr;
   va_start(argptr, format);

   wxString s = wxString::FormatV(format, argptr);
   mToPrint += s;
   if (!mHoldPrint)
      FlushPrint();

   va_end(argptr);
}

void BenchmarkDialog::HoldPrint(bool hold)
{
   mHoldPrint = hold;

   if (!mHoldPrint)
      FlushPrint();
}

void BenchmarkDialog::FlushPrint()
{
   while(mToPrint.Length() > 100) {
      mText->AppendText(mToPrint.Left(100));
      mToPrint = mToPrint.Right(mToPrint.Length() - 100);
   }
   if (mToPrint.Length() > 0)
      mText->AppendText(mToPrint);
   mToPrint = wxT("");
}

void BenchmarkDialog::OnRun( wxCommandEvent &event )
{
   TransferDataFromWindow();
 
   if (!Validate())
      return;

   // This code will become part of libaudacity,
   // and this class will be phased out.

   long blockSize, numEdits, dataSize, randSeed;

   mBlockSizeStr.ToLong(&blockSize);
   mNumEditsStr.ToLong(&numEdits);
   mDataSizeStr.ToLong(&dataSize);
   mRandSeedStr.ToLong(&randSeed);

   if (blockSize < 1 || blockSize > 1024) {
      wxMessageBox(wxT("Block size should be in the range 1 - 1024 KB."));
      return;
   }

   if (numEdits < 1 || numEdits > 10000) {
      wxMessageBox(wxT("Number of edits should be in the range 1 - 10000."));
      return;
   }

   if (dataSize < 1 || dataSize > 2000) {
      wxMessageBox(wxT("Test data size should be in the range 1 - 2000 MB."));
      return;
   }

   Sequence::SetMaxDiskBlockSize(blockSize * 1024);

   wxBusyCursor busy;

   HoldPrint(true);

   DirManager *d = new DirManager();
   TrackFactory *fact = new TrackFactory(d);
   WaveTrack *t = fact->NewWaveTrack(int16Sample);
   Track *tmp = NULL;

   t->SetRate(1);

   srand(randSeed);

   int len, scale;
   //scale = 7500 + (rand() % 1000);
   scale = 200 + (rand() % 100);
   len = (dataSize * 1048576) / (scale*sizeof(short));
   while(len < 20 || scale > (blockSize*1024)/4) {
      scale = (scale / 2) + (rand() % 100);
      len = (dataSize * 1048576) / (scale*sizeof(short));
   }

   Printf(wxT("Using %d blocks of %d samples each, for a total of ")
          wxT("%.1f MB.\n"),
          len, scale, len*scale*sizeof(short)/1048576.0);

   int trials = numEdits;

   short *small1 = new short[len];
   short *small2 = new short[len];
   short *block = new short[scale];

   Printf(wxT("Preparing...\n"));

   wxYield();
   FlushPrint();

   int i, b, v;
   int bad;
   int z;
   long elapsed;
   wxString tempStr;
   wxStopWatch timer;

   for (i = 0; i < len; i++) {
      v = short(rand());
      small1[i] = v;
      for (b = 0; b < scale; b++)
         block[b] = v;

      t->Append((samplePtr)block, int16Sample, scale);
   }
   t->Flush();

   // This forces the WaveTrack to flush all of the appends (which is
   // only necessary if you want to access the Sequence class directly,
   // as we're about to do).
   t->GetEndTime();

   if (t->GetClipByIndex(0)->GetSequence()->GetNumSamples() != (sampleCount)len * scale) {
      Printf(wxT("Expected len %d, track len %d.\n"), len * scale,
             t->GetClipByIndex(0)->GetSequence()->GetNumSamples());
      goto fail;
   }
   //t->Debug();

   Printf(wxT("Performing %d edits...\n"), trials);
   wxYield();
   FlushPrint();

   timer.Start();
   for (z = 0; z < trials; z++) {
      int x0 = rand() % len;
      int xlen = 1 + (rand() % (len - x0));
      if (mEditDetail)
         Printf(wxT("Cut: %d - %d \n"), x0 * scale, (x0 + xlen) * scale);

      t->Cut(double (x0 * scale), double ((x0 + xlen) * scale), &tmp);
      if (!tmp) {
         Printf(wxT("Trial %d\n"), z);
         Printf(wxT("Cut (%d, %d) failed.\n"), (x0 * scale),
                (x0 + xlen) * scale);
         Printf(wxT("Expected len %d, track len %d.\n"), len * scale,
                t->GetClipByIndex(0)->GetSequence()->GetNumSamples());
         goto fail;
      }

      int y0 = rand() % (len - xlen);
      if (mEditDetail)
         Printf(wxT("Paste: %d\n"), y0 * scale);

      t->Paste(double (y0 * scale), tmp);

      if (t->GetClipByIndex(0)->GetSequence()->GetNumSamples() != (sampleCount)len * scale) {
         Printf(wxT("Trial %d\n"), z);
         Printf(wxT("Expected len %d, track len %d.\n"), len * scale,
                t->GetClipByIndex(0)->GetSequence()->GetNumSamples());
         goto fail;
      }
      // Copy
      for (i = 0; i < xlen; i++)
         small2[i] = small1[x0 + i];
      // Delete
      for (i = 0; i < (len - x0 - xlen); i++)
         small1[x0 + i] = small1[x0 + xlen + i];
      // Insert
      for (i = 0; i < (len - xlen - y0); i++)
         small1[len - i - 1] = small1[len - i - 1 - xlen];
      // Paste
      for (i = 0; i < xlen; i++)
         small1[y0 + i] = small2[i];

      delete tmp;
   }

   elapsed = timer.Time();

   if (mBlockDetail) {
      t->GetClipByIndex(0)->GetSequence()->DebugPrintf(&tempStr);
      mToPrint += tempStr;
   }
   Printf(wxT("Time to perform %d edits: %ld ms\n"), trials, elapsed);
   FlushPrint();
   wxYield();


#if 0
   Printf(wxT("Checking file pointer leaks:\n"));
   Printf(wxT("Track # blocks: %d\n"), t->GetBlockArray()->Count());
   Printf(wxT("Disk # blocks: \n"));
   system("ls .audacity_temp/* | wc --lines");
#endif

   Printf(wxT("Doing correctness check...\n"));
   FlushPrint();
   wxYield();

   bad = 0;
   timer.Start();
   for (i = 0; i < len; i++) {
      v = small1[i];
      t->Get((samplePtr)block, int16Sample, i * scale, scale);
      for (b = 0; b < scale; b++)
         if (block[b] != v) {
            bad++;
            if (bad < 10)
               Printf(wxT("Bad: block %d sample %d\n"), i, b);
            b = scale;
         }
   }
   if (bad == 0)
      Printf(wxT("Passed correctness check!\n"));
   else
      Printf(wxT("Errors in %d/%d blocks\n"), bad, len);

   elapsed = timer.Time();

   Printf(wxT("Time to check all data: %ld ms\n"), elapsed);
   Printf(wxT("Reading data again...\n"));

   wxYield();
   FlushPrint();

   timer.Start();

   for (i = 0; i < len; i++) {
      v = small1[i];
      t->Get((samplePtr)block, int16Sample, i * scale, scale);
      for (b = 0; b < scale; b++)
         if (block[b] != v)
            bad++;
   }

   elapsed = timer.Time();

   Printf(wxT("Time to check all data (2): %ld ms\n"), elapsed);
   
   Printf(wxT("At 44100 Hz, 16-bits per sample, the estimated number of\n")
          wxT("simultaneous tracks that could be played at once: %.1f\n"),
          (len*scale/44100.0)/(elapsed/1000.0));

   delete t;

   delete[]small1;
   delete[]small2;
   delete[]block;

   delete fact;
   delete d;

   Sequence::SetMaxDiskBlockSize(1048576);
   HoldPrint(false);

   return;

 fail:
   Printf(wxT("TEST FAILED!!!\n"));

   delete t;

   delete[]small1;
   delete[]small2;
   delete[]block;

   delete d;

   Sequence::SetMaxDiskBlockSize(1048576);
   HoldPrint(false);
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
// arch-tag: 79fba2fd-f170-46cb-a781-8dca643f7cf4

