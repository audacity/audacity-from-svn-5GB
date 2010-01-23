/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioDoc.cpp

  Dominic Mazzoni

**********************************************************************/

// For compilers that support precompilation, includes "wx/wx.h".
#include "wx/wxprec.h"

#ifdef __BORLANDC__
#pragma hdrstop
#endif

#ifndef WX_PRECOMP
#include "wx/wx.h"
#endif

#ifdef VERBOSE
#include <stdio.h>
#endif

#include "wx/txtstrm.h"

#include "AudioDoc.h"
#include "AudioView.h"
#include "DirManager.h"
#include "GenericStream.h"
#include "WaveTrack.h"
#include "Import.h"

IMPLEMENT_DYNAMIC_CLASS(AudioDoc, wxDocument)

BEGIN_EVENT_TABLE(AudioDoc, wxDocument)
END_EVENT_TABLE()

// The version number of the file format.  Has no correspondance
// to the version number of the program.  Every time the file format
// changes in an irreversible way, increment this number so we know
// not to try to open old-format files.
#define kAUDACITY_DOC_CURRENT_VERSION         2

AudioDoc::AudioDoc()
{

}

AudioDoc::~AudioDoc()
{
}

#if wxUSE_STD_IOSTREAM

ostream& AudioDoc::SaveObject(ostream& outStream)
{
  wxDocument::SaveObject(outStream);
	GenericStream *generic = new GenericStream(&outStream);	
	this->Save(*generic);
	delete generic;
	return outStream;
}

istream& AudioDoc::LoadObject(istream& inStream)
{
	wxDocument::LoadObject(inStream);
	GenericStream *generic = new GenericStream(&inStream);	
	this->Load(*generic);
	delete generic;
	return inStream;
}

#else

wxOutputStream& AudioDoc::SaveObject(wxOutputStream& outStream)
{
  wxDocument::SaveObject(outStream);
  GenericStream *generic = new GenericStream(&outStream);	
  this->Save(*generic);
  delete generic;
  return outStream;
}

wxInputStream& AudioDoc::LoadObject(wxInputStream& inStream)
{
	wxDocument::LoadObject(inStream);
	GenericStream *generic = new GenericStream(&inStream);	
	this->Load(*generic);
	delete generic;

	// If we don't do this, the stream returns an error.
	// (Fixed latest wxwindows, not yet avail...)
	inStream.SeekI(0,wxFromCurrent);

	return inStream;
}

#endif

void AudioDoc::Save(GenericStream &stream)
{
  // If this is a "Save", we want to overwrite the old data files
  // (safely of course) but if it's "Save As" we leave them alone.
  // We determine which this is by comparing the full path of a file
  // before and after the SetProject.
  bool overwrite = false;
  wxString checkBefore;
  wxString checkAfter;
  wxString blank;

  checkBefore = dirManager.GetProjectName();

  wxString folderName="";
  wxString full = this->GetFilename();
  wxString path;
  wxString name;
  wxString ext;

  wxSplitPath((const char *)full, &path, &name, &ext);

  wxString t = name;

  int i=0;
  int len=t.Length();

  while(i<len && t[i]!='.')
	folderName += t[i++];

  folderName += "_data";

  dirManager.SetProject(path, folderName, true);
  
  checkAfter = dirManager.GetProjectName();
  
  if (checkBefore == checkAfter)
    overwrite = true;

  stream.Write((void *)"CMU.VAF.proj",12);

  int version = wxUINT32_SWAP_ON_BE(kAUDACITY_DOC_CURRENT_VERSION);
  stream.Write(&version, 4);

  wxString proj = dirManager.GetProjectName();
  len = wxUINT32_SWAP_ON_BE(proj.Length());
  
  stream.Write(&len,4);
  stream.Write((void *)(const char *)proj, strlen((const char *)proj));

  tracks.Save(&stream, overwrite);
}

void AudioDoc::Load(GenericStream &stream)
{
  char tag[13];
  
  stream.Read((void *)tag, 12);
  tag[12] = 0;
  if (strcmp(tag, "CMU.VAF.proj"))
  {
	wxMessageBox("Invalid header.");
	return;
  }

  int version;
  stream.Read(&version, 4);
  version = wxUINT32_SWAP_ON_BE(version);

  if (version > kAUDACITY_DOC_CURRENT_VERSION) {
	wxMessageBox("This project was saved by a newer version of Audacity.  "
				 "Please upgrade.");
	return;
  }

  if (version < kAUDACITY_DOC_CURRENT_VERSION) {
	wxMessageBox("This project was saved by an older version of Audacity.  "
				 "It can no longer be opened.");
	return;
  }

  wxString full = this->GetFilename();
  wxString path;
  wxString name;
  wxString ext;

  wxSplitPath((const char *)full, &path, &name, &ext);

  wxString folderName;
  int folderNameLen;
  stream.Read(&folderNameLen, 4);
  folderNameLen = wxUINT32_SWAP_ON_BE(folderNameLen);

  if (folderNameLen<0 || folderNameLen>255) {
	wxMessageBox("Pathname too long or corrupted file.");
	return;
  }

  char str[256];
  stream.Read(str, folderNameLen);
  str[folderNameLen] = 0;
  folderName = str;

  if (!dirManager.SetProject(path, folderName, false)) {
    wxString m;
    m += "The project ";
    m += this->GetFilename();
    m += " couldn't be opened, because its associated directory ";
    m += folderName;
    m += " couldn't be found.";
	wxMessageBox(m);

	return;
  }

  bool result = tracks.Load(&stream, &dirManager);

  if (!result)
    wxMessageBox("Could not open project.");

  ((AudioView *)(GetFirstView()))->FixScrollbars();

  ((AudioView *)(GetFirstView()))->InitialState();

}

