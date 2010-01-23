/**********************************************************************

  Audacity: A Digital Audio Editor

  ODDecodeFlacTask.h

  Created by Michael Chinen (mchinen) on 8/11/08.
  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

******************************************************************//**

\class ODDecodeFlacTask
\brief Decodes a flac file into a oddecodeBlockFile, but not immediately.

This is an abstract class that subclasses will have to derive the types
from.  For any type there should only be one ODDecodeTask associated with
a given track.  
There could be the ODBlockFiles of several FLACs in one track (after copy and pasting),
so things aren't as simple as they seem - the implementation needs to be
robust enough to allow all the user changes such as copy/paste, delete, and so on.

*//*******************************************************************/




#ifndef __AUDACITY_ODDecodeFLACTask__
#define __AUDACITY_ODDecodeFLACTask__

#include <vector>
#include "ODDecodeTask.h"
#include "ODTaskThread.h"
class ODDecodeBlockFile;
class WaveTrack;
class ODFileDecoder;


/// A class representing a modular task to be used with the On-Demand structures.
class ODDecodeFlacTask:public ODDecodeTask
{
 public:

   /// Constructs an ODTask
   ODDecodeFlacTask(){}
   virtual ~ODDecodeFlacTask(){};
   
   virtual ODTask* Clone()=0;
   ///Creates an ODFileDecoder that decodes a file of filetype the subclass handles.
   virtual ODFileDecoder* CreateFileDecoder(const char* fileName)=0;
 
};

///class to decode a particular file (one per file).  Saves info such as filename and length (after the header is read.)
class ODFlacDecoder:public ODFileDecoder
{
public:
   ///This should handle unicode converted to UTF-8 on mac/linux, but OD TODO:check on windows
   ODFlacDecoder(const char* fName):ODFileDecoder(fName){}
   virtual ~ODFlacDecoder(){}
   
   ///Decodes the samples for this blockfile from the real file into a float buffer.  
   ///This is file specific, so subclasses must implement this only.
   ///the buffer was defined like
   ///samplePtr sampleData = NewSamples(mLen, floatSample);
   ///this->ReadData(sampleData, floatSample, 0, mLen);
   ///This class should call ReadHeader() first, so it knows the length, and can prepare 
   ///the file object if it needs to. 
   virtual void Decode(samplePtr data, sampleFormat format, sampleCount start, sampleCount len)=0;
   
   ///Read header.  Subclasses must override.  Probably should save the info somewhere.
   ///Ideally called once per decoding of a file.  This complicates the task because 
   virtual void ReadHeader()=0;  

};

#endif



