/**********************************************************************

  Audacity: A Digital Audio Editor

  Export.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/choice.h>
#include <wx/file.h>
#include <wx/thread.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/timer.h>
#include <wx/filedlg.h>
#include <wx/msgdlg.h>
#include <wx/progdlg.h>
#include <wx/textfile.h>

#include "snd/snd.h"

#include "WaveTrack.h"
#include "DirManager.h"

#include "Export.h"

const int EXPORT_RADIO_ID = 13000;

BEGIN_EVENT_TABLE(ExportDialog, wxDialog)
  EVT_BUTTON(wxID_OK, ExportDialog::OnOK)
  EVT_BUTTON(wxID_CANCEL, ExportDialog::OnCancel)
END_EVENT_TABLE()

IMPLEMENT_CLASS(ExportDialog, wxDialog)

ExportDialog::ExportDialog(wxWindow *parent,
						  const wxPoint& pos)
  : wxDialog( parent, -1, "Export", pos, wxDefaultSize, wxDEFAULT_DIALOG_STYLE )
{
  wxBoxSizer *mainSizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer *sizer1 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *sizer2 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *sizer3 = new wxBoxSizer(wxHORIZONTAL);
  wxBoxSizer *sizer4 = new wxBoxSizer(wxHORIZONTAL);
  
  wxString headerStrings[5] = {"AIFF (Audio Interchange File Format)",
							   "SF (IRCAM Format)",
							   "AU (Sun AU / NeXT Audio Format)",
							   "WAV (Windows Wave)",
                               "TXT"};
  
  headerChoice = new wxChoice(this, 0, wxDefaultPosition, wxDefaultSize,
							  5, headerStrings);
  
  bitsButton[0] = new wxRadioButton(this, EXPORT_RADIO_ID, "8-bit", 
										wxDefaultPosition, wxDefaultSize,
										wxRB_GROUP);
  bitsButton[0]->SetValue(true);
  bitsButton[1] = new wxRadioButton(this, EXPORT_RADIO_ID, "16-bit");
  
  signButton[0] = new wxRadioButton(this, EXPORT_RADIO_ID, "signed",
										wxDefaultPosition, wxDefaultSize,
										wxRB_GROUP);
  signButton[0]->SetValue(true);
  signButton[1] = new wxRadioButton(this, EXPORT_RADIO_ID, "unsigned");
  
  wxButton *ok = new wxButton(this, wxID_OK, "OK");
  wxButton *cancel = new wxButton(this, wxID_CANCEL, "Cancel");
  
  sizer1->Add(headerChoice, 1, wxCENTER | wxALL, 10);
  
  sizer2->Add(bitsButton[0], 0, wxCENTER);
  sizer2->Add(bitsButton[1], 0, wxCENTER);
  
  sizer3->Add(signButton[0], 0, wxCENTER);
  sizer3->Add(signButton[1], 0, wxCENTER);
  
  sizer4->Add(ok, 0, wxCENTER);
  sizer4->Add(cancel, 0, wxCENTER);
  
  mainSizer->Add(sizer1, 0, wxCENTER);
  mainSizer->Add(sizer2, 0, wxCENTER);
  mainSizer->Add(sizer3, 0, wxCENTER);
  mainSizer->Add(sizer4, 0, wxCENTER);
  
  SetAutoLayout(true);
  SetSizer(mainSizer);
  
  mainSizer->SetSizeHints(this);
  mainSizer->Fit(this);
  
  wxSize size( GetSize() );
  
  /*
	if (size.x < size.y*3/2)
	{
	size.x = size.y*3/2;
	SetSize( size );
	}
	*/
  
  Centre(wxBOTH | wxCENTER_FRAME);
}

void ExportDialog::OnOK(wxCommandEvent& WXUNUSED(event))
{
  EndModal(TRUE);
}

void ExportDialog::OnCancel(wxCommandEvent& WXUNUSED(event))
{
  EndModal(FALSE);
}

// TODO: Dialog destructor

bool Export(WaveTrack *left, WaveTrack *right)
{
  int header = SND_HEAD_WAVE;
  bool bits16 = true;
  bool unsign = false;
  bool stereo = (right!=0);

  ExportDialog d((wxWindow *)0);
  d.headerChoice->SetSelection(header-1);
  d.bitsButton[bits16]->SetValue(TRUE);
  d.signButton[unsign]->SetValue(TRUE);
  d.bitsButton[!bits16]->SetValue(FALSE);
  d.signButton[!unsign]->SetValue(FALSE);
  d.ShowModal();

  if (!d.GetReturnCode())
	return false;

  header = d.headerChoice->GetSelection() + 1;
  bits16 = d.bitsButton[1]->GetValue();
  unsign = d.signButton[1]->GetValue();
  
  wxString extension;

  switch(header) {
  case SND_HEAD_AIFF:
	extension = ".AIFF";
	break;
  case SND_HEAD_IRCAM:
	extension = ".SF";
	break;
  case SND_HEAD_NEXT:
	extension = ".AU";
	break;
  case SND_HEAD_WAVE:
	extension = ".WAV";
	break;
  case SND_HEAD_WAVE+1:
  default:
	extension = ".TXT";
	break;
  }
  
  wxString fName =
          wxFileSelector("Export Selected Audio As:",
                         NULL,
                         "",
                         extension,
                         "*.*",
                         wxSAVE,
                         &d);

  if (fName.Length() >= 256) {
    wxMessageBox("Sorry, pathnames longer than 256 characters not supported.");
    return false;
  }
  
  if (fName == "")
    return false;

  if (header == SND_HEAD_WAVE+1) {
	// Export as text

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
	  wxMessageBox("Couldn't write to "+fName);
	  return false;
	}

	int len = left->numSamples;

	if (len > 1000000) {
	  wxMessageBox("Sorry, can't export text files more than 1MB");
	  return false;
	}

	sampleType *buffer = new sampleType[len];
	left->Get(buffer, 0, len);

	for(int i=0; i<len; i++)
	  f.AddLine(wxString::Format("%g", buffer[i]/32767.0));

	f.Write();
	f.Close();

	return true;
  }

  // Use snd library to export file

  snd_node sndfile;
  snd_node sndbuffer;

  sndfile.device = SND_DEVICE_FILE;
  sndfile.write_flag = SND_WRITE;
  strcpy(sndfile.u.file.filename, (const char *)fName);
  sndfile.u.file.file = 0;
  sndfile.u.file.header = header;
  sndfile.u.file.byte_offset = 0;
  sndfile.u.file.end_offset = 0;
  sndfile.u.file.swap = 0;
  sndfile.format.channels = stereo? 2: 1;
  sndfile.format.mode = SND_MODE_PCM; // SND_MODE_FLOAT
  sndfile.format.bits = bits16? 16: 8;
  sndfile.format.srate = left->rate;
  
  int err;
  long flags=0;

  err = snd_open(&sndfile, &flags);
  if (err) {
	wxMessageBox("Could not write to file.");
	return false;
  }
  
  sndbuffer.device = SND_DEVICE_MEM;
  sndbuffer.write_flag = SND_READ;
  sndbuffer.u.mem.buffer_max = 0;
  sndbuffer.u.mem.buffer = 0;
  sndbuffer.u.mem.buffer_len = 0;
  sndbuffer.u.mem.buffer_pos = 0;
  sndbuffer.format.channels = stereo? 2: 1;
  sndbuffer.format.mode = SND_MODE_PCM; // SND_MODE_FLOAT
  sndbuffer.format.bits = 16;
  sndbuffer.format.srate = left->rate;

  sampleCount maxblocksize = WaveTrack::GetIdealBlockSize();

  char *srcbuffer = new char[maxblocksize*4];
  char *dstbuffer = new char[maxblocksize*4];
  char *leftbuffer = new char[maxblocksize*4];
  char *rightbuffer = new char[maxblocksize*4];

  wxProgressDialog *progress = NULL;  
  wxYield();
  wxStartTimer();
  wxBusyCursor busy;

  sampleCount len = left->numSamples;
  if (stereo && right->numSamples < len)
	len = right->numSamples;

  sampleCount pos=0;
  sampleCount block=0;
  while(pos < len) {
    block = maxblocksize;
	if (block > (len - pos))
	  block = (len - pos);

    if (block > 0) {

	  if (stereo) {
		left->Get((sampleType *)leftbuffer, pos, block);
		right->Get((sampleType *)rightbuffer, pos, block);
		for(int i=0; i<block; i++) {
		  ((sampleType *)srcbuffer)[2*i] = ((sampleType *)leftbuffer)[i];
		  ((sampleType *)srcbuffer)[2*i+1] = ((sampleType *)rightbuffer)[i];
		}
	  }
	  else {
		left->Get((sampleType *)srcbuffer, pos, block);
	  }

      long b2 = snd_convert(&sndfile, dstbuffer,  // to
							&sndbuffer, srcbuffer, block); // from

	  int actual = snd_write(&sndfile, dstbuffer, b2);

	  pos += block;
    }

	if (!progress && wxGetElapsedTime(false) > 500) {
	  progress =
		new wxProgressDialog("Export","Exporting audio file",
							 len);
	}	
	if (progress) {
	  progress->Update(pos);
	}
  }

  snd_close(&sndfile);

  if (progress)
	delete progress;
  
  delete[] srcbuffer;
  delete[] dstbuffer;
  delete[] leftbuffer;
  delete[] rightbuffer;

  return true;
}

/*
  
  return false;

  short channels;

  wxASSERT(src1);
  if (src2) {
    channels = 2;
    wxASSERT(src1->rate == src2->rate);
    wxASSERT(src1->numSamples == src2->numSamples);
  }
  else
    channels = 1;

  wxFile outf;

  outf.Open(fName, wxFile::write);

  if (!outf.IsOpened()) {
	wxMessageBox("Could not open "+fName);
	return false;
  }

  char tag[5];
  int intRate = src1->rate;
  short bytesPerSample=2*channels;
  int len = src1->numSamples;
  
  short tempShort;
  int tempInt;
  
  strcpy(tag,"RIFF");
  outf.Write((void *)tag, 4);

  tempInt = wxUINT32_SWAP_ON_BE(len*sizeof(sampleType)*channels + 36);
  outf.Write((void *)&tempInt, 4);

  strcpy(tag,"WAVE");
  outf.Write((void *)tag, 4);

  strcpy(tag,"fmt ");
  outf.Write((void *)tag, 4);
  
  tempInt = wxUINT32_SWAP_ON_BE(16);
  outf.Write((void *)&tempInt, 4);

  tempShort = wxUINT16_SWAP_ON_BE(1);
  outf.Write((void *)&tempShort, 2);

  tempShort = wxUINT16_SWAP_ON_BE(channels);
  outf.Write((short *)&tempShort, 2);

  tempInt = wxUINT32_SWAP_ON_BE(intRate);
  outf.Write((int *)&tempInt, 4);

  tempInt = wxUINT32_SWAP_ON_BE(intRate*2);
  outf.Write((void *)&tempInt, 4);

  tempShort = wxUINT16_SWAP_ON_BE(bytesPerSample);
  outf.Write((short *)&tempShort, 2);

  tempShort = wxUINT16_SWAP_ON_BE(bytesPerSample * 8);
  outf.Write((void *)&tempShort, 2);
	
  strcpy(tag,"data");
  outf.Write((void *)tag, 4);

  tempInt = wxUINT32_SWAP_ON_BE(len*2);
  outf.Write((int *)&tempInt, 4);

  int blockSize = WaveTrack::GetIdealBlockSize();

  wxProgressDialog *progress = NULL;
  
  wxYield();

  wxStartTimer();

  switch(bytesPerSample) {
  case 2: {
	sampleType *buffer = new sampleType[blockSize];
	sampleType *buffer2 = new sampleType[blockSize*2];
	wxASSERT(buffer);
	wxASSERT(buffer2);
	int numSamples = len;
	int block;
	int x=0;
	while(numSamples) {
	  block = blockSize;
	  if (block > numSamples)
	    block = numSamples;

      if (channels==1) {
        int i;
	    src1->Get(buffer, x, block);
	    for(i=0; i<block; i++)
	      buffer[i] = wxUINT16_SWAP_ON_BE(buffer[i]);
	    int actual = outf.Write((void *)buffer, sizeof(sampleType)*block);
	  }
	  else {
	    int i;
        src1->Get(buffer, x, block);
	    for(i=0; i<block; i++)
	      buffer2[i*2] = wxUINT16_SWAP_ON_BE(buffer[i]);
        src2->Get(buffer, x, block);
	    for(i=0; i<block; i++)
	      buffer2[i*2+1] = wxUINT16_SWAP_ON_BE(buffer[i]);
	    int actual = outf.Write((void *)buffer2, sizeof(sampleType)*block*2);
      }

	  numSamples -= block;
	  x += block;
	  if (!progress && wxGetElapsedTime(false) > 500) {
		progress =
		  new wxProgressDialog("Export","Exporting WAV file",
							   len);
	  }

	  if (progress)
		progress->Update(x);
	}
	delete[] buffer;
	delete[] buffer2;
  }
  break;
  default:
	wxMessageBox("Sorry, WAV file not 16-bit");
	return false;
  }

  if (progress)
	delete progress;

  outf.Close();

  return true;

  */




/*

  int len = fName.Length();
  if (len > 5 && Stricmp(fName.SubString(len-5),".aiff"))
	header = SND_HEAD_AIFF;
  if (len > 3 && Stricmp(fName.SubString(len-3),".au"))
	header = SND_HEAD_NEXT;
  if (len > 6 && Stricmp(fName.SubString(len-6),".ircam"))
	header = SND_HEAD_IRCAM;
  if (len > 3 && Stricmp(fName.SubString(len-3),".sf"))
	header = SND_HEAD_IRCAM;
  if (len > 4 && Stricmp(fName.SubString(len-4),".wav"))
	header = SND_HEAD_WAV;
  if (len > 5 && Stricmp(fName.SubString(len-5),".wave"))
	header = SND_HEAD_WAVE;

*/
