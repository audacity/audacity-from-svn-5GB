/**********************************************************************

  Audacity: A Digital Audio Editor

  XMLFileReader.cpp

  Dominic Mazzoni

**********************************************************************/

#include <wx/defs.h>
#include <wx/intl.h>

#include <string.h>

#include "../Internat.h"
#include "XMLFileReader.h"

XMLFileReader::XMLFileReader()
{
   mParser = XML_ParserCreate(NULL);
   XML_SetUserData(mParser, (void *)this);
   XML_SetElementHandler(mParser, startElement, endElement);

   mBaseHandler = NULL;
   mMaxDepth = 128;
   mHandler = new XMLTagHandler*[mMaxDepth];
   mDepth = -1;
   mErrorStr = wxT("");
}

XMLFileReader::~XMLFileReader()
{
   delete[] mHandler;
   XML_ParserFree(mParser);
}

bool XMLFileReader::Parse(XMLTagHandler *baseHandler,
                          const wxString &fname)
{
   FILE *fp = fopen(FILENAME(fname).fn_str(), "rb");
   if (!fp || ferror(fp)) {
      wxString errStr;
      mErrorStr.Printf(_("Could not open file: \"%s\""), fname.c_str());
      return false;
   }

   mBaseHandler = baseHandler;
   mHandler[0] = NULL;

   const size_t bufferSize = 16384;
   char buffer[16384];
   int done = 0;
   do {
      size_t len = fread(buffer, 1, bufferSize, fp);
      done = (len < bufferSize);
      if (!XML_Parse(mParser, buffer, len, done)) {
         mErrorStr.Printf(_("Error: %hs at line %d"),
			  XML_ErrorString(XML_GetErrorCode(mParser)),
			  XML_GetCurrentLineNumber(mParser));
         fclose(fp);
         return false;
      }
   } while (!done);

   fclose(fp);

   // Even though there were no parse errors, we only succeed if
   // the first-level handler actually got called, and didn't
   // return false.
   if (mHandler[0])
      return true;
   else {
      mErrorStr.Printf(_("Unable to open project file."));
      return false;
   }
}

wxString XMLFileReader::GetErrorStr()
{
   return mErrorStr;
}

// static
void XMLFileReader::startElement(void *userData, const char *name,
                                 const char **atts)
{
   XMLFileReader *This = (XMLFileReader *)userData;

   This->mDepth++;

   if (This->mDepth >= This->mMaxDepth) {
      XMLTagHandler  **newHandler = new XMLTagHandler*[This->mMaxDepth*2];
      for(int i=0; i<This->mMaxDepth; i++)
         newHandler[i] = This->mHandler[i];
      This->mMaxDepth *= 2;
   }

   if (This->mDepth==0)
      This->mHandler[This->mDepth] = This->mBaseHandler;
   else {
      if (This->mHandler[This->mDepth-1])
         This->mHandler[This->mDepth] =
            This->mHandler[This->mDepth-1]->ReadXMLChild(name);
      else
         This->mHandler[This->mDepth] = NULL;
   }

   if (This->mHandler[This->mDepth]) {
      if (!This->mHandler[This->mDepth]->ReadXMLTag(name, atts))
         This->mHandler[This->mDepth] = 0;
   }
}

// static
void XMLFileReader::endElement(void *userData, const char *name)
{
   XMLFileReader *This = (XMLFileReader *)userData;

   if (This->mHandler[This->mDepth])
      This->mHandler[This->mDepth]->ReadXMLEndTag(name);

   This->mDepth--;
}

// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: 0a51946d-5a9f-46c9-92d9-ee09698a9bc3

