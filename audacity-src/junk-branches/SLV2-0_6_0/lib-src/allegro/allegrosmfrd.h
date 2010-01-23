#include "../../src/Experimental.h"

#ifdef EXPERIMENTAL_NOTE_TRACK
typedef class Alg_pending {
public:
    Alg_note_ptr note;
    class Alg_pending *next;
    Alg_pending(Alg_note_ptr n, class Alg_pending *list) { 
		note = n; next = list; }
} *Alg_pending_ptr;

class Alg_midifile_reader: public Midifile_reader {
public:
    FILE *file;
    Alg_seq_ptr seq;
    int divisions;
    Alg_pending_ptr pending;
    Alg_track_ptr track;
	long channel_offset_per_track; // used to encode track number into channel
		// chan is actual_channel + channel_offset_per_track * track_num
	    // default is 100, set this to 0 to merge all tracks to 16 channels
    int channel_offset;

    Alg_midifile_reader(FILE *f, Alg_seq_ptr new_seq) {
		file = f;
		pending = NULL;
		seq = new_seq;
	}
    // delete destroys the seq member as well, so set it to NULL if you
    // copied the pointer elsewhere
    ~Alg_midifile_reader();
    // the following is used to load the Alg_seq from the file:
    void parse();

    void set_nomerge(bool flag) { Mf_nomerge = flag; }
    void set_skipinit(bool flag) { Mf_skipinit = flag; }
    long get_currtime() { return Mf_currtime; }

protected:
    double get_time();
    void update(int chan, int key, Alg_parameter_ptr param);
    void *Mf_malloc(size_t size) { return malloc(size); }
    void Mf_free(void *obj, size_t size) { free(obj); }
    /* Methods to be called while processing the MIDI file. */
    void Mf_starttrack();
    void Mf_endtrack();
    int Mf_getc();
    void Mf_eot();
    void Mf_error(char *);
    void Mf_header(int,int,int);
    void Mf_on(int,int,int);
    void Mf_off(int,int,int);
    void Mf_pressure(int,int,int);
    void Mf_controller(int,int,int);
    void Mf_pitchbend(int,int,int);
    void Mf_program(int,int);
    void Mf_chanpressure(int,int);
    void Mf_sysex(int,char*);
    void Mf_arbitrary(int,char*);
    void Mf_metamisc(int,int,char*);
    void Mf_seqnum(int);
    void Mf_smpte(int,int,int,int,int);
    void Mf_timesig(int,int,int,int);
    void Mf_tempo(int);
    void Mf_keysig(int,int);
    void Mf_sqspecific(int,char*);
    void Mf_text(int,int,char*);
};
#endif
