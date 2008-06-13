/**********************************************************************

  Audacity: A Digital Audio Editor

  ODPCMAliasBlockFile.cpp
  
  Created by Michael Chinen (mchinen)
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODPCMAliasBlockFile
\brief ODPCMAliasBlockFile is a special type of PCMAliasBlockFile that does not necessarily have summary data available
The summary is eventually computed and written to a file in a background thread.

Load On-Demand implementation of the AliasBlockFile for PCM files.
   
to load large files more quickly, we take skip computing the summary data and put
ODPCMAliasBlockFiles in the sequence as place holders.  A background thread loads and 
computes the summary data into these classes.
ODPCMAliasBlockFiles are unlike all other BlockFiles are not immutable (for the most part,) because when new
summary data is computed for an existing ODPCMAliasBlockFile we save the buffer then and write the Summary File.  

All BlockFile methods that treat the summary data as a buffer that exists in its BlockFile
are implemented here to behave when the data is not available yet.

*//*******************************************************************/






#ifndef __AUDACITY_ODPCMALIASBLOCKFILE__
#define __AUDACITY_ODPCMALIASBLOCKFILE__

#include "PCMAliasBlockFile.h"
#include "../BlockFile.h"
#include "../ondemand/ODTaskThread.h"
#include "../DirManager.h"
#include <wx/thread.h>

/// An AliasBlockFile that references uncompressed data in an existing file 
class ODPCMAliasBlockFile : public PCMAliasBlockFile
{
 public:

   // Constructor / Destructor

   /// Constructs a PCMAliasBlockFile, writing the summary to disk
   ODPCMAliasBlockFile(wxFileName baseFileName,
                     wxFileName aliasedFile, sampleCount aliasStart,
                     sampleCount aliasLen, int aliasChannel);
   ODPCMAliasBlockFile(wxFileName existingFileName,
                     wxFileName aliasedFile, sampleCount aliasStart,
                     sampleCount aliasLen, int aliasChannel,
                     float min, float max, float rms);
   virtual ~ODPCMAliasBlockFile();
   
   //checks to see if summary data has been computed and written to disk yet.  Thread safe.  Blocks if we are writing summary data.
   virtual bool IsSummaryAvailable();
   
   /// Returns TRUE if the summary has not yet been written, but is actively being computed and written to disk 
   virtual bool IsSummaryBeingComputed(){return mSummaryBeingComputed;}
   
   //Calls that rely on summary files need to be overidden
   virtual wxLongLong GetSpaceUsage();
   /// Gets extreme values for the specified region
   virtual void GetMinMax(sampleCount start, sampleCount len,
                          float *outMin, float *outMax, float *outRMS);
   /// Gets extreme values for the entire block
   virtual void GetMinMax(float *outMin, float *outMax, float *outRMS);
   /// Returns the 256 byte summary data block
   virtual bool Read256(float *buffer, sampleCount start, sampleCount len);
   /// Returns the 64K summary data block
   virtual bool Read64K(float *buffer, sampleCount start, sampleCount len);

   ///Makes new ODPCMAliasBlockFile or PCMAliasBlockFile depending on summary availability
   virtual BlockFile *Copy(wxFileName fileName);
   
   ///Saves as xml ODPCMAliasBlockFile or PCMAliasBlockFile depending on summary availability
   virtual void SaveXML(XMLWriter &xmlFile);

   ///Reconstructs from XML a ODPCMAliasBlockFile and reschedules it for OD loading
   static BlockFile *ODPCMAliasBlockFile::BuildFromXML(DirManager &dm, const wxChar **attrs);

   ///Writes the summary file if summary data is available
   virtual void Recover(void);
   
   ///A public interface to WriteSummary
   void DoWriteSummary(){WriteSummary();}

  protected:
   virtual void WriteSummary();
   virtual void *CalcSummary(samplePtr buffer, sampleCount len,
                             sampleFormat format);

   ODLock    mSummaryAvailableMutex;
   bool mSummaryAvailable;
   bool mSummaryBeingComputed;

};

#endif

