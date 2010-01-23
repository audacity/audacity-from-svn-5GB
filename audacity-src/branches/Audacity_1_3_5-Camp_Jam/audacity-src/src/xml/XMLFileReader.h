/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLFileReader.h

  Dominic Mazzoni

**********************************************************************/

#include "../Audacity.h"         // to pull in USE_SYSTEM_EXPAT define

#if defined(USE_SYSTEM_EXPAT)
#include "expat.h"
#else
#include "xmlparse/xmlparse.h"
#endif

#include "XMLTagHandler.h"

class XMLFileReader {
 public:
   XMLFileReader();
   virtual ~XMLFileReader();

   bool Parse(XMLTagHandler *baseHandler,
              const wxString &fname);

   wxString GetErrorStr();

   // Callback functions for expat

   static void startElement(void *userData, const char *name,
                            const char **atts);

   static void endElement(void *userData, const char *name);

 private:
   XML_Parser       mParser;
   int              mMaxDepth;
   int              mDepth;
   XMLTagHandler  **mHandler;
   XMLTagHandler   *mBaseHandler;
   wxString         mErrorStr;
};



// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 367db03e-6d57-4749-928b-f690b8af3f4f

