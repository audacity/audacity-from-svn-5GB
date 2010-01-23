/**********************************************************************

  Audacity: A Digital Audio Editor

  VSTEffect.h

  Dominic Mazzoni

**********************************************************************/

#include "../../Audacity.h"

#if USE_VST

#include "../Effect.h"

#include "aeffectx.h"

#define audacityVSTID CCONST('a', 'u', 'D', 'y');

typedef long (*dispatcherFn)(AEffect * effect, long opCode,
                             long index, long value, void *ptr,
                             float opt);

typedef void (*processFn)(AEffect * effect, float **inputs,
                          float **outputs, long sampleframes);

typedef void (*setParameterFn)(AEffect * effect, long index,
                               float parameter);

typedef float (*getParameterFn)(AEffect * effect, long index);

typedef AEffect *(*vstPluginMain)(audioMasterCallback audioMaster);

class VSTEffect:public Effect
{
 public:

   VSTEffect(const wxString & path, void *module, AEffect * aeffect);
   virtual ~VSTEffect();

   virtual wxString GetEffectName();

   virtual wxString GetEffectIdentifier();

   virtual std::set<wxString> GetEffectCategories();

   virtual wxString GetEffectAction();

   virtual bool Init();

   virtual bool PromptUser();
   
   virtual bool Process();
   
   virtual void End();

   // Utility methods

   wxString GetString(int opcode, int index = 0);
   void SetString(int opcode, const wxString & str, int index = 0);

   // VST methods

   long callDispatcher(long opcode, long index, long value, void *ptr, float opt);
   void callProcess(float **inputs, float **outputs, long sampleframes);
   void callProcessReplacing(float **inputs, float **outputs, long sampleframes);
   void callSetParameter(long index, float parameter);
   float callGetParameter(long index);

   // VST callback
   long int audioMaster(AEffect * effect,
                        long int opcode,
                        long int index,
                        long int value,
                        void * ptr,
                        float opt);

 private:
   bool ProcessStereo(int count,
                      WaveTrack *left,
                      WaveTrack *right,
                      sampleCount lstart,
                      sampleCount rstart,
                      sampleCount len);

   wxString mPath;
   void *mModule;
   AEffect *mAEffect;

   wxString mVendor;
   wxString mName;

   sampleCount mBlockSize;
   float **mInBuffer;
   float **mOutBuffer;
   int mInputs;
   int mOutputs;
   int mChannels;
};

#endif // USE_VST


// Indentation settings for Vim and Emacs and unique identifier for Arch, a
// version control system. Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3
// arch-tag: a652b5be-4fec-414d-bc04-82cfed5687d9

