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
#include "allegro/strparse.h"
#include "allegro/allegrord.h"
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

   if (fName.Length() > 4 &&
       !fName.Right(4).CmpNoCase(".gro")) {

      // Import Allegro file (Roger Dannenberg)

      Allegro_reader reader(mf);
      reader.parse();
      fclose(mf);
      
      if (reader.seq.notes.len == 0) {
         // TODO: is there a better way to see if an error occurred?
         wxMessageBox("Error parsing Allegro file.");
         return false;
      }
      // need a Seq_ptr to a seq on the heap, but all we have is a reader member
      // so copy to the heap. Be careful because reader will be deleted.
      Seq_ptr seq = new Seq;
      *seq = reader.seq;
      
      // this is probably not necessary, and would be cleaner if reader returned
      // a Seq_ptr:
      reader.seq.notes.events = NULL;
      reader.seq.notes.len = 0;
      reader.seq.map.beats.beats = NULL;
      reader.seq.map.beats.len = 0;
      reader.seq.time_sig.time_sigs = NULL;
      reader.seq.time_sig.len = 0;
      dest->SetSequence(seq);
   }
   else {

      // Import Standard MIDI file

      Allegro_midifile_reader *reader = new Allegro_midifile_reader();
      
      reader->initialize(mf);
      
      fclose(mf);
      
      if (reader->seq->notes.len == 0) {
         // TODO: is there a better way to see if an error occurred?
         wxMessageBox("Error parsing MIDI file.");
         return false;
      }

      // need a Seq_ptr to a seq on the heap, but all we have is a reader member
      // so copy to the heap. Be careful because reader will be deleted.
      //Seq_ptr seq = new Seq;
      //*seq = *(reader.seq);
      
      dest->SetSequence(reader->seq);

   }

   return true;
}
