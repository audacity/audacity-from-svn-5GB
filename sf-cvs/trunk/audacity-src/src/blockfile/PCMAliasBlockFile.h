/**********************************************************************

  Audacity: A Digital Audio Editor

  PCMAliasBlockFile.h

  Joshua Haberman

**********************************************************************/

#ifndef __AUDACITY_PCMALIASBLOCKFILE__
#define __AUDACITY_PCMALIASBLOCKFILE__

#include "../BlockFile.h"

/// An AliasBlockFile that references uncompressed data in an existing file
class PCMAliasBlockFile : public AliasBlockFile
{
 public:

   // Constructor / Destructor

   /// Constructs a PCMAliasBlockFile, writing the summary to disk
   PCMAliasBlockFile(wxFileName baseFileName,
                     wxFileName aliasedFile, sampleCount aliasStart,
                     sampleCount aliasLen, int aliasChannel);
   PCMAliasBlockFile(wxFileName existingFileName,
                     wxFileName aliasedFile, sampleCount aliasStart,
                     sampleCount aliasLen, int aliasChannel,
                     float min, float max, float rms);
   ~PCMAliasBlockFile();

   // Reading

   /// Reads the specified data from the aliased file using libsndfile
   int ReadData(samplePtr data, sampleFormat format,
                        sampleCount start, sampleCount len);
   void SaveXML(int depth, wxFFile &xmlFile);
   static BlockFile *BuildFromXML(wxString projDir, const char **attrs);
   BlockFile *Copy(wxFileName fileName);
};

#endif

