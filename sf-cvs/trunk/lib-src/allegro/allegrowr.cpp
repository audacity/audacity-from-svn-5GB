// allegrowr.cpp -- write sequence to an Allegro file (text)

#include "stdlib.h"
#include "stdio.h"
#include "assert.h"
#include "allegro.h"

#ifndef EXPERIMENTAL_NOTE_TRACK
#include "allegrowr.h"
#else /* EXPERIMENTAL_NOTE_TRACK */
#include "memory.h"
#endif /* EXPERIMENTAL_NOTE_TRACK */

#include "string.h"
#include "strparse.h"

// Note about precision: %g prints 6 significant digits. For 1ms precision,
// the maximum magnitude is 999.999, i.e. 1000s < 17minutes. For anything
// over 1000s, time in seconds will be printed with 10ms precision, which
// is not good. Therefore, times and durations are printed as %.4d, which
// gives 100us precision.
// The following define allows you to change this decision:
#define TIMFMT "%.4d"

#ifndef EXPERIMENTAL_NOTE_TRACK
void parameter_print(FILE *file, Parameter_ptr p)
#else /* EXPERIMENTAL_NOTE_TRACK */
void parameter_print(FILE *file, Alg_parameter_ptr p)
#endif /* EXPERIMENTAL_NOTE_TRACK */
{
    char str[256];
    fprintf(file, " -%s:", p->attr_name());
    switch (p->attr_type()) {
    case 'a':
#ifndef EXPERIMENTAL_NOTE_TRACK
        fprintf(file, "%s", p->a);
#else /* EXPERIMENTAL_NOTE_TRACK */
        fprintf(file, "'%s'", p->a);
#endif /* EXPERIMENTAL_NOTE_TRACK */
        break;
    case 'i':
        fprintf(file, "%d", p->i);
        break;
    case 'l':
        fprintf(file, "%s", p->l ? "true" : "false");
        break;
    case 'r':
        fprintf(file, "%g", p->r);
        break;
    case 's':
        string_escape(str, p->s, "\"");
        fprintf(file, "%s", str);
        break;
    }
}

#ifndef EXPERIMENTAL_NOTE_TRACK
void allegro_write(Seq_ptr seq, FILE *file)
{
    int i;
    // first write the tempo map
    Beats &beats = seq->map.beats;
    for (i = 0; i < beats.len - 1; i++) {
        Beat_ptr b = &(beats[i]);
        fprintf(file, "TW%g ", seq->map.time_to_beat(b->time) / 4);
        double tempo = (beats[i + 1].beat - beats[i].beat) /
                       (beats[i + 1].time - beats[i].time);
        fprintf(file, "-tempor:%g\n", tempo * 60);
    }
    if (seq->map.last_tempo_flag) { // we have final tempo:
        double time = seq->map.time_to_beat(beats[beats.len - 1].time) / 4;
        fprintf(file, "TW%g ", time);
        fprintf(file, "-tempor:%g\n", seq->map.last_tempo * 60.0);
    }

    // now write the notes at beat positions
    for (i = 0; i < seq->notes.len; i++) {
        Allegro_event_ptr e = seq->notes[i];
        double start = seq->map.time_to_beat(e->time);
        fprintf(file, "TW%g", start / 4);
        if (e->chan != -1) {
            fprintf(file, " V%d", e->chan);
        }
        if (e->type == 'n') {
            Allegro_note_ptr n = (Allegro_note_ptr) e;
            double dur = seq->map.time_to_beat(n->time + n->dur) - start;
            fprintf(file, " K%d P%g Q%g L%g", n->key, n->pitch, dur, n->loud);
            Parameters_ptr p = n->parameters;
            while (p) {
                parameter_print(file, &(p->parm));
                p = p->next;
            }
        } else { // an update
            Allegro_update_ptr u = (Allegro_update_ptr) e;
            if (u->key != -1) {
                fprintf(file, " K%d", u->key);
            }
            parameter_print(file, &(u->parameter));
        }
        fprintf(file, "\n");
    }
}

#else /* EXPERIMENTAL_NOTE_TRACK */

int Alg_seq::write(const char *filename)
{
    FILE *file = fopen(filename, "w");
    if (!file) return errno;
    write(file, units_are_seconds);
    fclose(file);
    return 0;
}

void Alg_seq::write(FILE *file, bool in_secs)
{
    int i, j;
    if (in_secs) convert_to_seconds();
    else convert_to_beats();
    // first write the tempo map
    fprintf(file, "#track 0\n");
    Alg_beats &beats = time_map->beats;
    for (i = 0; i < beats.len - 1; i++) {
        Alg_beat_ptr b = &(beats[i]);
        if (in_secs) {
            fprintf(file, "T" TIMFMT, b->time);
        } else {
            fprintf(file, "TW" TIMFMT, b->beat / 4);
        }
        double tempo = (beats[i + 1].beat - b->beat) /
                       (beats[i + 1].time - beats[i].time);
        fprintf(file, " -tempor:%g\n", tempo * 60);
    }
    if (time_map->last_tempo_flag) { // we have final tempo:
        Alg_beat_ptr b = &(beats[beats.len - 1]);
        if (in_secs) {
            fprintf(file, "T" TIMFMT, b->time);
        } else {
            fprintf(file, "TM" TIMFMT, b->beat / 4);
        }
        fprintf(file, " -tempor:%g\n", time_map->last_tempo * 60.0);
    }

    // write the time signatures
    for (i = 0; i < time_sig.length(); i++) {
        Alg_time_sig &ts = time_sig[i];
        double time = ts.beat;
        if (in_secs) {
            fprintf(file, "T" TIMFMT " V- -timesig_numr:%g\n", time, ts.num);
            fprintf(file, "T" TIMFMT " V- -timesig_denr:%g\n", time, ts.den);
        } else {
            double wholes = ts.beat / 4;
            fprintf(file, "TW" TIMFMT " V- -timesig_numr:%g\n",
            time / 4, ts.num);
            fprintf(file, "TW" TIMFMT " V- -timesig_denr:%g\n",
            time / 4, ts.den);
        }
    }

    for (j = 0; j < track_list.length(); j++) {
        Alg_events &notes = track_list[j];
        if (j != 0) fprintf(file, "#track %d\n", j);
        // now write the notes at beat positions
        for (i = 0; i < notes.length(); i++) {
            Alg_event_ptr e = notes[i];
            double start = e->time;
            if (in_secs) {
                fprintf(file, "T" TIMFMT " ", start);
            } else {
                fprintf(file, "TW" TIMFMT " ", start / 4);
            }
            // write the channel as Vn or V-
            if (e->chan == -1) fprintf(file, "V-");
            else fprintf(file, "V%d", e->chan);
            // write the note or update data
            if (e->is_note()) {
                Alg_note_ptr n = (Alg_note_ptr) e;
                double dur = n->dur;
                fprintf(file, " K%d P%g ", n->get_identifier(), n->pitch);
                if (in_secs) {
                    fprintf(file, "U" TIMFMT " ", dur);
                } else {
                    fprintf(file, "Q" TIMFMT " ", dur);
                }
                fprintf(file, "L%g ", n->loud);
                Alg_parameters_ptr p = n->parameters;
                while (p) {
                    parameter_print(file, &(p->parm));
                    p = p->next;
                }
            } else { // an update
                assert(e->is_update());
                Alg_update_ptr u = (Alg_update_ptr) e;
                if (u->get_identifier() != -1) {
                    fprintf(file, " K%d", u->get_identifier());
                }
                parameter_print(file, &(u->parameter));
            }
            fprintf(file, "\n");
        }
    }
}
#endif /* EXPERIMENTAL_NOTE_TRACK */
