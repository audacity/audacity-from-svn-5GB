/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportMIDI.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/msgdlg.h>
#include <wx/ffile.h>
#include <wx/intl.h>

#include "../Internat.h"
#include "../NoteTrack.h"
#include "ImportMIDI.h"

#include "allegro.h"
#include "strparse.h"
#include "allegrord.h"
#include "mfmidi.h"

#ifndef EXPERIMENTAL_NOTE_TRACK
#include "mfallegro.h"
#else /* EXPERIMENTAL_NOTE_TRACK */
#include "allegrosmfrd.h"
#endif /* EXPERIMENTAL_NOTE_TRACK */

bool ImportMIDI(wxString fName, NoteTrack * dest)
{
   wxFFile mf(fName, wxT("rb"));

   if (!mf.IsOpened()) {
      wxMessageBox( _("Could not open file: ") + fName);
      return false;
   }

   if (fName.Length() > 4 &&
       !fName.Right(4).CmpNoCase(wxT(".gro"))) {

      // Import Allegro file (Roger Dannenberg)

#ifndef EXPERIMENTAL_NOTE_TRACK
      Allegro_reader reader(mf.fp());
#else /* EXPERIMENTAL_NOTE_TRACK */
      Alg_reader reader( mf.fp(), new Alg_seq );
#endif /* EXPERIMENTAL_NOTE_TRACK */
      reader.parse();
      mf.Close();
      
#ifndef EXPERIMENTAL_NOTE_TRACK
      if (reader.seq.notes.len == 0) {
         // TODO: is there a better way to see if an error occurred?
         wxMessageBox(_("Error parsing Allegro file."));
         return false;
      }
      // need a Seq_ptr to a seq on the heap, but all we have is a reader member
      // so copy to the heap. Be careful because reader will be deleted.
#else /* EXPERIMENTAL_NOTE_TRACK */
      if(reader.error_flag)
      {
         // TODO: is there a better way to see if an error occurred?
         wxMessageBox(_("Error parsing Allegro file."));
         return false;
      }
      // need a Seq_ptr to a seq on the heap, but all we have is a reader member
      // so copy to the heap. Be careful because reader will be deleted.
#endif /* EXPERIMENTAL_NOTE_TRACK */


#ifndef EXPERIMENTAL_NOTE_TRACK
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
      reader->initialize(mf.fp());
      
      mf.Close();
      
      if (reader->seq->notes.len == 0) {
         // TODO: is there a better way to see if an error occurred?
         wxMessageBox(_("Error parsing MIDI file."));
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
#else /* EXPERIMENTAL_NOTE_TRACK */
      /*Seq_ptr seq = new Seq;
      *seq = reader.seq;*/
      
      // this is probably not necessary, and would be cleaner if reader returned
      // a Seq_ptr:
      /*
      reader.seq.notes.events = NULL;
      reader.seq.notes.len = 0;
      reader.seq.map.beats.beats = NULL;
      reader.seq.map.beats.len = 0;
      reader.seq.time_sig.time_sigs = NULL;
      reader.seq.time_sig.len = 0;
      */
      dest->SetSequence( reader.seq );
   }
   else {

      // Import Standard MIDI file

      Alg_midifile_reader *reader = new Alg_midifile_reader(mf.fp(), new Alg_seq);
      reader->parse();
      
      mf.Close();
      
      if (reader->seq->tracks() == 0) {
         // TODO: is there a better way to see if an error occurred?
         wxMessageBox(_("Error parsing MIDI file."));
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
#endif /* EXPERIMENTAL_NOTE_TRACK */

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 391b08e6-61f4-43ea-8431-c835c31ba86d

