/**********************************************************************

  Audacity: A Digital Audio Editor

  AudioDoc.h

  Dominic Mazzoni

**********************************************************************/

#ifndef _AUDIODOC_
#define _AUDIODOC_

#include "wx/docview.h"

#include "Track.h"
#include "DirManager.h"

class AudioDoc: public wxDocument
{
  DECLARE_DYNAMIC_CLASS(AudioDoc)

public:
  TrackList tracks;

  DirManager dirManager;

  AudioDoc();
  ~AudioDoc();
  
  void Load(GenericStream &stream);
  void Save(GenericStream &stream);

#if wxUSE_STD_IOSTREAM
  virtual ostream& SaveObject(ostream& stream);
  virtual istream& LoadObject(istream& stream);
#else
  wxOutputStream& SaveObject(wxOutputStream& stream);
  wxInputStream& LoadObject(wxInputStream& stream);
#endif
  
private:
  DECLARE_EVENT_TABLE()
};

#endif


