#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "Buffer.h"
#include "Sequence.h"
#include "ManagedFileContext.h"
#include "SeqMemBlock.h"
#include "Exceptions.h"
#include "XMLLoadStore.h"

int compare(int step,
            Mezzo::Sequence &seq1, Mezzo::BlockedSequence &seq2)
{
   long_sample_count len = seq1.GetLength();
   
   printf("Step %d: Memory says %lld, Blocked says %lld\n",
          step, len, seq2.GetLength());
   
   if (seq2.GetLength() != len) {
      printf("Lengths disagree: Memory says %lld, Blocked says %lld\n",
             len, seq2.GetLength());
      return -1;
   }
   
   Mezzo::Buffer b1 = seq1.Get(0, len);
   Mezzo::Buffer b2 = seq2.Get(0, len);
   Mezzo::Buffer diff = b2;
   diff *= -1.0;
   diff += b1;
   Mezzo::FloatBuffer fdiff = diff.AsFloat();
   if (fdiff.GetMax() > 0) {
      seq2.ConsistencyCheck(true);
      printf("Error: seq1 and seq2 differ by %f\n", fdiff.GetMax());
      int j;
      Mezzo::FloatBuffer f1 = b1.AsFloat();
      Mezzo::FloatBuffer f2 = b2.AsFloat();
      for(j=0; j<60000; j++)
         if (f1[j] != f2[j]) {
            printf("First error: sample %d/%lld, 1:%f 2:%f\n",
                   j, len, f1[j], f2[j]);
            break;
         }
      
      return -1;
   }   

   return 0;
}

void usage()
{
   printf("testmain [save|load] <dir>\n");
}

int main(int argc, char **argv)
{
   enum {
      SAVE,
      LOAD
   } mode;

   if(argc != 3) {
      usage();
      exit(1);
   }

   if(std::string("save") == argv[1])
      mode = SAVE;
   else if(std::string("load") == argv[1])
      mode = LOAD;
   else
   {
      usage();
      exit(1);
   }

   std::string dir = argv[2];

   try {
      Mezzo::FloatBuffer f(50000);

      int i;

      for(i=0; i<50000; i++)
         f[i] = i;

      if(mode == SAVE) {
         Mezzo::ManagedFileContext context(dir);
         Mezzo::BlockedSequence seq2(&context);
         //Mezzo::BlockedSequence seq2(new SeqMemBlockContext);

         for(i=0; i<40; i++)
            seq2.Append(f);

         Mezzo::XMLStorer storer(dir + "/test.xml");
         Mezzo::AttrDict attrs;

         storer.StoreBeginNode("testmain", attrs);                // <testmain>
         context.Store(storer, dir, seq2.GetManagedFilesInUse()); // store context
         seq2.Store(storer);                                      // store sequence
         storer.StoreEndNode("testmain");                         // </testmain>

         return 0;
      }
      else if (mode == LOAD)
      {
         Mezzo::XMLLoader loader(dir + "/test.xml");

         loader.GetNextToken();                            // <testmain>
         Mezzo::ManagedFileContext loadedContext(loader);  // load context
         Mezzo::BlockedSequence seq2(loader);              // load sequence
         loader.GetNextToken();                            // </testmain>

         Mezzo::MemorySequence seq1;

         Mezzo::FloatBuffer min(1000);
         Mezzo::FloatBuffer max(1000);
         Mezzo::FloatBuffer sumsq(1000);
         float spp = seq2.GetLength() / 1000.0;
         //seq2.GetWaveDisplay(min, max, sumsq, 0, seq2.GetLength(), 1000, spp);

         for(i=0; i<40; i++)
            seq1.Append(f);

         for(i=0; i<100; i++) {
            int len = seq1.GetLength();

            if (compare(i, seq1, seq2)) {
               printf("Comparison failed, exiting.\n");
               exit(0);
            }

            if (len == 0)
               break;

            int s1 = rand() % len;
            int dlen = (len - s1 + 1);
            if (dlen>100000)
               dlen = 100000;
            int l1 = rand() % dlen;

            seq1.Delete(s1, l1);
            seq2.Delete(s1, l1);

            seq2.ConsistencyCheck(false);
         }

         for(i=0; i<10; i++) {
            seq1.Append(f);
            seq2.Append(f);
         }

         printf("Now doing cutting and pasting:\n");

         for(i=0; i<100; i++) {
            int len = seq1.GetLength();
            Mezzo::Sequence *c1, *c2;

            if (compare(i, seq1, seq2)) {
               printf("Comparison failed, exiting.\n");
               exit(0);
            }

            if (len == 0)
               break;

            int s1 = rand() % len;
            int dlen = (len - s1 + 1);
            if (dlen>100000)
               dlen = 100000;
            int l1 = rand() % dlen;

            c1 = seq1.Cut(s1, l1);
            c2 = seq2.Cut(s1, l1);

            if (compare(i, seq1, seq2)) {
               printf("Comparison failed, exiting.\n");
               exit(0);
            }

            int s2 = rand() % (len - l1 + 1);

            seq1.Paste(s2, c1);
            seq2.Paste(s2, c2);

            delete c1;
            delete c2;

            seq2.ConsistencyCheck(false);
         }

         printf("Success!\n");
         printf("Final lengths: %lld %lld\n",
                seq1.GetLength(),
                seq2.GetLength());

         seq2.ConsistencyCheck(true);

         printf("\n");
         printf("Now let's do something illegal just to test the "
                "exception handling:\n");
         seq2.Get(0, seq2.GetLength() + 1);
      }
   }
   catch (Mezzo::ClientException &e) {
      printf("## Client exception at %s:\n## %s\n",
             e.location.c_str(),
             e.description.c_str());
   } 
   catch (Mezzo::InternalException &e) {
      printf("## Internal exception at %s:\n## %s\n",
             e.location.c_str(),
             e.description.c_str());
   } 
   catch (Mezzo::UserException &e) {
      printf("## User exception at %s:\n## %s\n",
             e.location.c_str(),
             e.description.c_str());
   } 
   catch (...) {
      printf("Unknown exception!\n");
   }
      
   return 0;
}
   
// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

