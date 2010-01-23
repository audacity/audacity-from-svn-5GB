#include <stdio.h>
#include "xlisp.h"
#include "sound.h"
#include "falloc.h"
#include "debug.h"

/* The DEBUG_MEM related routines are:
 *    dbg_mem_allocated: called when memory is allocated
 *    dbg_mem_freed: called when memory is freed
 *    dbg_mem_released: called when memory is released
 */

#if DEBUG_MEM
typedef struct {
    long seq_num;
    char *who;
} dbg_mem_node, *dbg_mem_type;

static long dbg_mem_last_seq_num = 0;
long dbg_mem_seq_num = 0;
long dbg_mem_trace = 0;

void dbg_mem_pause(void)
{
    stdputstr("RETURN to continue: ");
    getchar();
}


void dbg_mem_allocated(void *p, char *who)
{
    dbg_mem_type info = (dbg_mem_type) p;
    if (p == (void *) dbg_mem_trace) {
        nyquist_printf("dbg_mem_allocated(%lx, %s)\n", p, who);
    }
    info--; /* info is stored (hidden) BEFORE the data */
    dbg_mem_last_seq_num++;
    if (dbg_mem_last_seq_num == dbg_mem_seq_num) {
        nyquist_printf("dbg_mem_allocated: %s just allocated %x as number %d\n",
               who, p, dbg_mem_last_seq_num);
        dbg_mem_pause();
    }
    info->seq_num = dbg_mem_last_seq_num;
    info->who = who;
}


void dbg_mem_freed(void *p, char *who)
{
    dbg_mem_type info = (dbg_mem_type) p;
    if (p == (void *) dbg_mem_trace) {
        nyquist_printf("dbg_mem_freed(%lx, %s)\n", p, who);
    }
    info--; /* info is stored (hidden) BEFORE the data */
    if (!info->who) {
        nyquist_printf("MEMORY %x FREED TWICE!, second time by: %s, seq_num %d\n",
               p, who, info->seq_num);
        fflush(stdout);
        dbg_mem_pause();
    }
    if (info->seq_num == dbg_mem_seq_num) {
        nyquist_printf("dbg_mem_freed: %s freeing %x, number %d\n",
               who, p, dbg_mem_seq_num);
        dbg_mem_pause();
    }
    info->who = NULL;
}

void dbg_mem_released(void *p, char *who)
{
    dbg_mem_type info = (dbg_mem_type) p;
    if (p == (void *) dbg_mem_trace) {
        nyquist_printf("dbg_mem_released(%lx, %s)\n", p, who);
    }
    info--; /* info is stored (hidden) BEFORE the data */
    if (!info->who) {
        nyquist_printf(
        "MEMORY %x RELEASED BUT NOT ALLOCATED!, released by: %s, seq_num %d\n",
               p, who, info->seq_num);
        fflush(stdout);
        dbg_mem_pause();
    }
    if (info->seq_num == dbg_mem_seq_num) {
        nyquist_printf("dbg_mem_released: %s releasing %x, number %d\n",
               who, p, dbg_mem_seq_num);
        dbg_mem_pause();
    }
}


void dbg_mem_check(void *p, char *who)
{
    dbg_mem_type info = (dbg_mem_type) p;
    if (!info) {
        nyquist_printf("DBG_MEM_CHECK (from %s): NULL POINTER!", who);
         fflush(stdout);
        dbg_mem_pause();
    }
    info--; /* info is stored (hidden) BEFORE the data */
    if (!info->who) {
        nyquist_printf("DBG_MEM_CHECK (from %s): %x IS FREE!, seq_num %d\n", 
               who, p, info->seq_num);
        fflush(stdout);
        dbg_mem_pause();
    }
}


void dbg_mem_print(char *msg, void *p)
{
    dbg_mem_type info = (dbg_mem_type) p;
    stdputstr(msg);
    if (!info) {
        stdputstr(" NULL POINTER");
    } else {
        info--; /* info is stored (hidden) BEFORE the data */
        if (!info->who) {
            nyquist_printf(" %x IS FREE!, ", p);
        } else {
            nyquist_printf(" %x allocated by %s, ", p, info->who);
        }
        nyquist_printf("seq_num %d\n", info->seq_num);
    }
}
#endif


void print_sound_type(sound_type s)
{
    snd_list_type list;
    int blockcount;

    nyquist_printf("sound_type: 0x%x\n", s);
    nyquist_printf("\tt0: %f\n", s->t0);
    nyquist_printf("\tsr: %f\n", s->sr);
    nyquist_printf("\tcurrent: %d\n", s->current);
    nyquist_printf("\tlogical_stop_cnt: %d\n", s->logical_stop_cnt);
    nyquist_printf("\tlist: 0x%x\n", s->list);
    nyquist_printf("\tscale: %f\n", s->scale);

    list = s->list;
    blockcount = 0;
    nyquist_printf("\t(0x%x:0x%x)->", list, list->block);
    while (list->block) {
        list = list->u.next;
        if (blockcount < 5) {
            nyquist_printf("(0x%x:0x%x)->", list, list->block);
        }
        else if (blockcount == 5) {
            stdputstr(" ... ");
            break;
        }
        blockcount++;
    }
    stdputstr("\n");
}

void print_snd_list_type(snd_list_type list)
{
    nyquist_printf("%x: [%x[%d], %x] refcnt %d ls %d", list, list->block,
            list->block_len, list->u.next, 
            list->refcnt, list->logically_stopped);
}



void print_sample_block_type(char *label,
                             sample_block_type sampblock,
                             int len)
{
    int j;
    sample_block_values_type samp;

    samp = sampblock->samples;
    nyquist_printf("%s: [%x(ref %d): len %d]: =========>>",
           label, sampblock, sampblock->refcnt, len);
    for (j = 0; j < len; j++) {
        nyquist_printf("%6g ", *samp++);
    }
    stdputstr("\n");
}


/*******/
snd_susp_type susp_to_watch = NULL;

void watch_susp(snd_susp_type s)
{
    if (!susp_to_watch) {
        susp_to_watch = s;
        nyquist_printf("watching susp %x\n", s);
    }
}

sound_type sound_to_watch = NULL;

void watch_sound(sound_type s)
{
    if (!sound_to_watch) {
        sound_to_watch = s;
        nyquist_printf("watching sound %x\n", s);
    }
}


snd_list_type snd_list_to_watch = NULL;

void watch_snd_list(snd_list_type s)
{
    snd_list_to_watch = s;
    nyquist_printf("watching snd_list %x\n", s);
}


void snd_list_debug(snd_list_type snd_list, char *s)
{
    if (snd_list == snd_list_to_watch) {
        nyquist_printf("%s%s\n", s,
               " appended to snd_list_to_watch.");
        watch_snd_list(snd_list->u.next);
    }
}


void snd_list_report(snd_list_type snd_list, char *s)
{
    if (snd_list == snd_list_to_watch) {
        nyquist_printf("%s: fetching block for watched snd_list.\n",
               s);
    }
}


#ifdef IGNORE
void test_it()
{
    if (susp_to_watch && susp_to_watch->keep_fetch) 
        stdputstr("WE FOUND A SERIOUS PROBLEM\n");
}
#endif
