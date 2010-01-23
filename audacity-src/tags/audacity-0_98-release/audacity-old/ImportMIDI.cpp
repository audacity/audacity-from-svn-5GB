/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMIDI.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/msgdlg.h>
#include <wx/file.h>

#include "NoteTrack.h"
#include "ImportMIDI.h"

#include "allegro/allegro.h"
#include "allegro/mfmidi.h"
#include "allegro/mfallegro.h"

bool ImportMIDI(wxString fName, NoteTrack * dest)
{
   FILE *mf = fopen(fName, "rb");

   if (!mf || ferror(mf)) {
      wxMessageBox(wxString::
                   Format("Could not open %s", (const char *) fName));
      return false;
   }

   Allegro_midifile_reader reader;

   reader.initialize(mf);

   fclose(mf);

   if (reader.seq->notes.len == 0) {
      // TODO: is there a better way to see if an error occurred?
      wxMessageBox("Error parsing MIDI file.");
      return false;
   }

   dest->SetSequence(reader.seq);

   return true;
}
