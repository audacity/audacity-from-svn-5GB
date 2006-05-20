/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  TestManagedFileContext.cpp

  Copyright (c) 2004 Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include "ManagedFileContext.h"
#include "platform/DiskFunctions.h"

#include <vector>

class TestManagedFileContext {
 public:
   TestManagedFileContext(std::string dir):
      mContext(dir)
   {
      Mezzo::FloatBuffer buf(100000);

      // create a bunch of SeqBlock files
      mSeqBlocks.push_back(mContext.NewSeqBlock(buf));

      for(int i = 0; i < 100; i++)
      {
         if(rand() & 0x1)
            mSeqBlocks.push_back(mContext.NewSeqBlock(buf));
         else
            mSeqBlocks.push_back(mContext.GetSeqBlockRef(mSeqBlocks[rand()%mSeqBlocks.size()]));
      }
   }

   ~TestManagedFileContext()
   {
      FreeBlocks();
   }

   void MoveToNewDirectory(std::string newDir)
   {
      mContext.MoveToNewDirectory(newDir);
   }

   void FreeBlocks()
   {
      for(unsigned int i = 0; i < mSeqBlocks.size(); i++)
         mContext.ReleaseSeqBlockRef(mSeqBlocks[i]);
      mSeqBlocks.clear();
   }

 private:
   Mezzo::ManagedFileContext mContext;
   std::vector<Mezzo::SeqBlock*> mSeqBlocks;
};

int main(void)
{
   std::string dir = "/tmp/TestManagedFileContext-OwnsThisDir";
   std::string dir2 = "/tmp/TestManagedFileContext-OwnsThisDir2";

   Mezzo::Platform::DeleteDirectory(dir);
   Mezzo::Platform::DeleteDirectory(dir2);

   printf("Testing ManagedFileContext...\n");

   printf("  Testing basic operation...");
   fflush(stdout);
   {
      TestManagedFileContext test(dir);
   }
   if(Mezzo::Platform::GetFilesInDir(dir) != 0)
   {
      printf("Error: not all files were cleaned up\n");
      exit(1);
   }
   printf("ok\n");


   printf("  Testing moving to a new directory...");
   fflush(stdout);
   {
      TestManagedFileContext test(dir);
      int filesInOldDir = Mezzo::Platform::GetFilesInDir(dir);
      test.MoveToNewDirectory(dir2);

      if(Mezzo::Platform::GetFilesInDir(dir) != 0)
      {
         printf("Error: files still exist in old directory\n");
         exit(1);
      }
      if(Mezzo::Platform::GetFilesInDir(dir2) != filesInOldDir)
      {
         printf("Files were not successfully moved\n");
         exit(1);
      }
   }
   if(Mezzo::Platform::GetFilesInDir(dir2) != 0)
   {
      printf("Error: not all files were cleaned up\n");
      exit(1);
   }
   printf("ok\n");
}

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

