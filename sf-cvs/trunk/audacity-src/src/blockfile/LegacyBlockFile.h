/**********************************************************************

  Audacity: A Digital Audio Editor

  LegacyBlockFile.h

  Dominic Mazzoni

**********************************************************************/

#ifndef __AUDACITY_LEGACY_BLOCKFILE__
#define __AUDACITY_LEGACY_BLOCKFILE__

#include <wx/string.h>
#include <wx/filename.h>

#include "../BlockFile.h"

//
// This class supports loading BlockFiles in one of the old
// Audacity BlockFile formats (versions 0.98 through 1.0, or
// versions 1.1.0 through 1.1.2).  You can load a BlockFile
// in this format, and you can save information about it
// back to disk, but you can't create a new one from new
// sample data.
//
class LegacyBlockFile : public BlockFile {
 public:

   // Constructor / Destructor

   /// Create the memory structure to refer to the given block file
   LegacyBlockFile(wxFileName existingFile,
                   sampleFormat format,
                   sampleCount summaryLen,
                   sampleCount len);
   ~LegacyBlockFile();

   // Reading

   /// Read the summary section of the disk file
   bool ReadSummary(void *data);
   /// Read the data section of the disk file
   int ReadData(samplePtr data, sampleFormat format,
                sampleCount start, sampleCount len);

   /// Create a new block file identical to this one
   BlockFile *Copy(wxFileName newFileName);
   /// Write an XML representation of this file
   void SaveXML(int depth, wxFFile &xmlFile);
   static BlockFile *BuildFromXML(wxString dir, const char **attrs,
                                  sampleFormat format);
   int GetSpaceUsage();

 protected:
   sampleFormat mFormat;
};

#endif // __AUDACITY_LEGACY_BLOCKFILE__

