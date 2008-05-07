/*
 * PortBurn
 * Windows XP IMAPI implementation
 *
 * Authors:
 *   Dominic Mazzoni
 *
 * The following MSDN page was used as a guide:
 *   http://msdn2.microsoft.com/en-us/library/aa366236.aspx
 *
 * License: LGPL
 */

#include "portburn.h"

#define _WIN32_WINNT 0x0400

#include "windows.h"
#include "tchar.h"
#include "malloc.h"
#include "imapi.h"
#include "stdio.h"

#define DEBUG(a) printf a

typedef struct {
   HRESULT                     hres;
   IStream                    *pStream;
   IDiscMaster                *pIDiscMaster;
   IRedbookDiscMaster         *pIRedbookDiscMaster;
   IEnumDiscRecorders         *pEnumDiscRecorders;
   IDiscRecorder              *pDiscRecorder;
   IDiscMasterProgressEvents  *piEvents;
   HANDLE                      hThread;
   int                         burning;
   int                         cancel;
   float                       fraction;
   UINT_PTR                    pnCookie;
   int                         simulate;
   int                         ejectAfterBurn;
} PBHandle;

void *PortBurn_Open()
{   
   PBHandle *h;

   h = (PBHandle *) HeapAlloc(GetProcessHeap(),
                              HEAP_ZERO_MEMORY,
                              sizeof(PBHandle));
   if (h == NULL) {
      return NULL;
   }

   CoInitializeEx(NULL, COINIT_MULTITHREADED);

   h->hres = CoCreateInstance(__uuidof(MSDiscMasterObj), //CLSID_MSDiscMasterObj,
                              NULL,
                              CLSCTX_ALL,
                              IID_IDiscMaster,
                              (void **)&h->pIDiscMaster);
   if (FAILED(h->hres)) {
      free(h);
      return NULL;
   }

   h->hres = CoMarshalInterThreadInterfaceInStream(IID_IDiscMaster,
                                                   h->pIDiscMaster,
                                                   &h->pStream);
   if (FAILED(h->hres)) {
      free(h);
      return NULL;
   }

   h->hres = h->pIDiscMaster->Open();
   if (h->hres != S_OK) {
      free(h);
      return NULL;
   }

   /* Is it necessary to call EnumDiscMasterFormats?  We know we only
      want Redbook, so let's just try to open it. */
   IEnumDiscMasterFormats *pp;
   h->hres = h->pIDiscMaster->EnumDiscMasterFormats(&pp);
   h->hres = h->pIDiscMaster->SetActiveDiscMasterFormat
      (IID_IRedbookDiscMaster, (void **)&h->pIRedbookDiscMaster);
   if (h->hres != S_OK) {
      h->pIDiscMaster->Close();
      free(h);
      return NULL;
   }

   return h;
}

/* Cleanup */
void PortBurn_Close(void *handle) {
   PBHandle *h = (PBHandle *)handle;

   if (!h)
      return;

   if (h->pEnumDiscRecorders)
      h->pEnumDiscRecorders->Release();

   h->pIDiscMaster->Close();
   h->pIDiscMaster->Release();

   CoUninitialize();

   HeapFree(GetProcessHeap(), 0, h);
}


/* Return a human-readable error string for the last operating system
   specific error (NOT human readable strings for the PortBurn error
   codes).  Caller should dispose of the returned string using free(). */
const char *PortBurn_LastError(void *handle)
{
   PBHandle *h = (PBHandle *)handle;
   HRESULT hr;
   LPTSTR windowsErrorString;
   char *errorString;
   size_t len;

   if (!h)
      return NULL;

   /* Have Windows allocate a buffer for us and format the error
      message in windowsErrorString */
   hr = h->hres;
   if (HRESULT_FACILITY(hr) == FACILITY_WINDOWS)
      hr = HRESULT_CODE(hr);
   FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,
                 NULL, hr, MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                 (LPTSTR)&windowsErrorString, 0, NULL);

   /* Allocate a buffer of the same size using malloc() and copy the
      string, then free the one Windows gave us */
   len = strlen((char *)windowsErrorString);
   errorString = (char *)malloc(+1);
   strcpy_s(errorString, len, (char *)windowsErrorString);
   LocalFree(windowsErrorString);

   return errorString;
}

/* Get the number of devices capable of burning audio CDs.
   If the result is N, then calls to GetDeviceName and OpenDevice
   are guaranteed to be valid for indices from 0 up to N-1, until
   the next time you call GetNumDevices.  At that point, the list of
   devices will be rescanned, and may be different. */
int PortBurn_GetNumDevices(void *handle)
{
   PBHandle *h = (PBHandle *)handle;
   IDiscRecorder *pDiscRecorder;
   ULONG fetched;
   int count = 0;


   if (!h)
      return NULL;

   if (h->pEnumDiscRecorders) {
      h->pEnumDiscRecorders->Release();
      h->pEnumDiscRecorders = NULL;
   }

   h->hres = h->pIDiscMaster->EnumDiscRecorders(&h->pEnumDiscRecorders);
   if (h->hres != S_OK)
      return 0;

   while(S_OK == h->pEnumDiscRecorders->Next(1, &pDiscRecorder, &fetched)) {
      pDiscRecorder->Release();
      count++;
   }

   return count;
}

/* Get the name of the device with a given index.  Only valid
   after a call to GetNumDevices. */
char *PortBurn_GetDeviceName(void *handle, int index)
{
   PBHandle *h = (PBHandle *)handle;
   IDiscRecorder *pDiscRecorder;
   BSTR bVendor = NULL;
   BSTR bProduct = NULL;
   BSTR bRevision = NULL;
   ULONG fetched;
   int len;
   wchar_t *wname;
   char *name;

   if (!h)
      return NULL;

   h->hres = S_OK;

   if (!h->pEnumDiscRecorders)
      return NULL;

   h->pEnumDiscRecorders->Reset();
   if (index != 0) {
      h->pEnumDiscRecorders->Skip(index - 1);
   }
   h->hres = h->pEnumDiscRecorders->Next(1, &pDiscRecorder, &fetched);
   if (h->hres != S_OK)
      return NULL;

   h->hres = pDiscRecorder->GetDisplayNames
      (&bVendor, &bProduct, &bRevision);
   pDiscRecorder->Release();
   if (h->hres != S_OK)
      return NULL;

   len = SysStringLen(bVendor) + 1 + SysStringLen(bProduct) + 1 +
      SysStringLen(bRevision);

   wname = (LPWSTR)alloca((len + 1)*sizeof(wchar_t));
   if (wname == NULL)
      return NULL;

   _stprintf_s(wname,
               (len + 1),
               _T("%s %s %s"),
               (LPCWSTR) bVendor,
               (LPCWSTR) bProduct,
               (LPCWSTR) bRevision);

   name = (char *)malloc(len + 1);
   if (name == NULL)
      return NULL;

   WideCharToMultiByte(CP_ACP, 0, wname, -1, name, len, NULL, NULL);
   name[len] = '\0';

   return name;
}

class PortBurnProgress : public IDiscMasterProgressEvents
{
 public:
   static HRESULT STDMETHODCALLTYPE
      CreateInstance(IDiscMasterProgressEvents** ppEvents);

   void SetPortBurnHandle(PBHandle *h);

 private:
   PortBurnProgress();

   PBHandle *h;

 protected:
   STDMETHOD(QueryInterface)(REFIID riid, LPVOID* ppv);
   STDMETHOD_(ULONG, AddRef)(VOID);
   STDMETHOD_(ULONG, Release)(VOID);

 protected:
   STDMETHOD(QueryCancel)(boolean *pbCancel);
   STDMETHOD(NotifyPnPActivity)(VOID);
   STDMETHOD(NotifyAddProgress)(LONG nCompletedSteps, LONG nTotalSteps);
   STDMETHOD(NotifyBlockProgress)(LONG nCompleted, LONG nTotal);
   STDMETHOD(NotifyTrackProgress)(LONG nCurrentTrack, LONG nTotalTracks);
   STDMETHOD(NotifyPreparingBurn)(LONG nEstimatedSeconds);
   STDMETHOD(NotifyClosingDisc)(LONG nEstimatedSeconds);
   STDMETHOD(NotifyBurnComplete)(HRESULT status);
   STDMETHOD(NotifyEraseComplete)(HRESULT status);

 private:
   ULONG m_cRefs;
};

PortBurnProgress::PortBurnProgress():
   h(0), m_cRefs(0)
{
}

void PortBurnProgress::SetPortBurnHandle(PBHandle *h)
{
   this->h = h;
}

STDMETHODIMP
PortBurnProgress::CreateInstance(IDiscMasterProgressEvents
                                            **ppEvents)
{
   HRESULT hr = E_NOINTERFACE;
   if (!(ppEvents))
      hr = E_POINTER;
   else
   {
      PortBurnProgress* pThis = new
         PortBurnProgress();
      hr = pThis->QueryInterface(IID_IDiscMasterProgressEvents,
                                 (VOID**)ppEvents);
   }

   return hr;
}

STDMETHODIMP PortBurnProgress::QueryInterface(REFIID riid,
                                                         LPVOID* ppv)
{
   HRESULT hr = E_NOINTERFACE;
   if (!(ppv))
      hr = E_POINTER;
   else
   {
      *ppv = NULL;

      if ((IID_IUnknown == riid) || (IID_IDiscMasterProgressEvents == riid))
      {
         *ppv = this;
         AddRef();
         hr = S_OK;
      }
   }
   return hr;
}

STDMETHODIMP_(ULONG) PortBurnProgress::AddRef(VOID)
{
   return InterlockedIncrement((LONG*)&m_cRefs);
}

STDMETHODIMP_(ULONG) PortBurnProgress::Release(VOID)
{
//   ASSERT(0 != m_cRefs);
   ULONG cRef = InterlockedDecrement((LONG*)&m_cRefs);
   if (0 == cRef)
   {
      delete this;
   }
   return cRef;
}

STDMETHODIMP_(HRESULT) PortBurnProgress::QueryCancel(boolean *pbCancel)
{
//   DEBUG(("QueryCancel() -> %d\n", h->cancel));
   *pbCancel = h->cancel;
   return S_OK;
}

STDMETHODIMP_(HRESULT) PortBurnProgress::NotifyPnPActivity(VOID)
{
   DEBUG(("NotifyPnActivity\n"));
   return S_OK;
}

STDMETHODIMP_(HRESULT) PortBurnProgress::NotifyAddProgress(
    LONG nCompletedSteps,
    LONG nTotalSteps)
{
//   DEBUG(("NotifyAddProgress %d %d\n", nCompletedSteps, nTotalSteps));
   return S_OK;
}

STDMETHODIMP_(HRESULT) PortBurnProgress::NotifyBlockProgress(LONG nCompleted,
                                                             LONG nTotal)
{
   DEBUG(("NotifyBlockProgress %d %d\n", nCompleted, nTotal));

   /* This should never actually reach 1.0; we wait until the BurnComplete
      message for a 1.0 */
   h->fraction = nCompleted / (float)(nTotal + 1);
   return S_OK;
}

STDMETHODIMP_(HRESULT) PortBurnProgress::NotifyTrackProgress(
    LONG nCurrentTrack,
    LONG nTotalTracks)
{
   DEBUG(("NotifyTrackProgress %d %d\n", nCurrentTrack, nTotalTracks));
   return S_OK;
}

STDMETHODIMP_(HRESULT) PortBurnProgress::NotifyPreparingBurn
                                              (LONG nEstimatedSeconds)
{
   DEBUG(("NotifyPreparingBurn %d\n", nEstimatedSeconds));
   return S_OK;
}

STDMETHODIMP_(HRESULT) PortBurnProgress::NotifyClosingDisc
                                              (LONG nEstimatedSeconds)
{
   DEBUG(("NotifyClosingDisc %d\n", nEstimatedSeconds));
   return S_OK;
}

STDMETHODIMP_(HRESULT) PortBurnProgress::NotifyBurnComplete(HRESULT status)
{
   DEBUG(("NotifyBurnComplete %08x\n", status));
   h->fraction = 1.0;
   return S_OK;
}

STDMETHODIMP_(HRESULT) PortBurnProgress::NotifyEraseComplete(HRESULT status)
{
   DEBUG(("NotifyEraseComplete %08x\n", status));
   return S_OK;
}

/* Recording Thread */
DWORD WINAPI PortBurn_RecordDisc(LPVOID lpParam)
{
   PBHandle *h = (PBHandle *)lpParam;
   IDiscMaster *pIDiscMaster;

   h->hres = CoInitializeEx(0, COINIT_MULTITHREADED);
   if (SUCCEEDED(h->hres)) {
      h->hres = CoGetInterfaceAndReleaseStream(h->pStream,
                                               IID_IDiscMaster,
                                               (void**) &pIDiscMaster);
      if (SUCCEEDED(h->hres)) {
         h->hres = pIDiscMaster->RecordDisc(h->simulate,
                                            h->ejectAfterBurn);
      }
   }

   h->fraction = 1.0;

   CoUninitialize();

   if (FAILED(h->hres))
      return pbErrCannotStartBurning;
 
   return pbSuccess;
}

/* Open a particular device by index number.  Returns 0 on success;
   any nonzero value indicates an error, for example if the device is
   already open by some other program. */
int PortBurn_OpenDevice(void *handle, int index)
{
   ULONG fetched;
   PBHandle *h = (PBHandle *)handle;
   if (!h)
      return pbErrNoHandle;
   h->hres = S_OK;

   if (!h->pEnumDiscRecorders)
      return pbErrMustCallGetNumDevices;

   h->pEnumDiscRecorders->Reset();
   if (index != 0) {
      h->pEnumDiscRecorders->Skip(index - 1);
   }

   h->hres = h->pEnumDiscRecorders->Next(1, &h->pDiscRecorder, &fetched);
   if (h->hres != S_OK)
      return pbErrCannotAccessDevice;

   h->hres = h->pIDiscMaster->SetActiveDiscRecorder(h->pDiscRecorder);
   if (h->hres != S_OK) {
      h->pDiscRecorder->Release();
      h->pDiscRecorder = NULL;
      return pbErrCannotReserveDevice;
   }

   h->hres = h->pDiscRecorder->OpenExclusive();
   if (h->hres != S_OK) {
      h->pDiscRecorder->Release();
      h->pDiscRecorder = NULL;
      return pbErrCannotReserveDevice;
   }
   return pbSuccess;
}

/* Close a device */
int PortBurn_CloseDevice(void *handle)
{
   PBHandle *h = (PBHandle *)handle;

   if (!h)
      return pbErrNoHandle;
   h->hres = S_OK;

   if (!h->pDiscRecorder)
      return pbErrDeviceNotOpen;

   h->burning = 0;
   h->cancel = 0;
   h->fraction = 0.0;
   if (h->hThread) {
      CloseHandle(h->hThread);
      h->hThread = NULL;
   }

   if (h->piEvents) {
      h->pIDiscMaster->ProgressUnadvise(h->pnCookie);
      h->piEvents->Release();
      h->piEvents = NULL;
   }

   h->hres = h->pDiscRecorder->Close();
   h->pDiscRecorder->Release();
   h->pDiscRecorder = NULL;

   if (h->hres != S_OK)
      return pbErrCannotCloseDevice;

   return pbSuccess;
}

/* Eject the media in the currently opened device */
int PortBurn_EjectDevice(void *handle)
{
   PBHandle *h = (PBHandle *)handle;

   if (!h)
      return pbErrNoHandle;
   h->hres = S_OK;

   if (!h->pDiscRecorder)
      return pbErrDeviceNotOpen;

   h->hres = h->pDiscRecorder->Eject();
   if (S_OK != h->hres)
      return pbErrCannotEject;

   return pbSuccess;
}

/* Erase the media in the currently opened device */
int PortBurn_EraseDevice(void *handle)
{
   PBHandle *h = (PBHandle *)handle;

   if (!h)
      return pbErrNoHandle;
   h->hres = S_OK;

   if (!h->pDiscRecorder)
      return pbErrDeviceNotOpen;

   h->hres = h->pDiscRecorder->Erase(false);
   if (S_OK != h->hres)
      return pbErrCannotEject;

   return pbSuccess;
}

/* This indicates you're ready to start staging audio data for the
   currently opened device.  At this point you are committing to
   exclusive access to the CD burner, and this is the function that
   will fail if another program is using the device, or if there is
   no writable CD media in the device at this point. */
int PortBurn_StartStaging(void *handle, const char *tmpdir)
{
   PBHandle *h = (PBHandle *)handle;

   if (!h)
      return pbErrNoHandle;
   h->hres = S_OK;

   if (!h->pDiscRecorder)
      return pbErrDeviceNotOpen;

   if (h->piEvents)
      return pbErrAlreadyStagingOrBurning;

//   h->hres = h->pDiscRecorder->OpenExclusive();
//   if (h->hres != S_OK)
//      return pbErrCannotReserveDevice;

   h->piEvents = NULL;
   h->hres = PortBurnProgress::CreateInstance(&h->piEvents);
   if (h->hres != S_OK) {
      h->pDiscRecorder->Close();
      return pbErrCannotPrepareToBurn;
   }

   ((PortBurnProgress *)h->piEvents)->SetPortBurnHandle(h);

   h->hres = h->pIDiscMaster->ProgressAdvise(h->piEvents,
                                             &h->pnCookie);
   if (h->hres != S_OK) {
      h->piEvents->Release();
      h->pDiscRecorder->Close();
      return pbErrCannotPrepareToBurn;
   }

   return pbSuccess;
}

/* Start a new audio track.  Pass the name of the track, and the
   length in CD Audio frames (each frame is 1/75.0 of a second, exactly). */
int PortBurn_StartTrack(void *handle, const char *name, int frames)
{
   PBHandle *h = (PBHandle *)handle;
   if (!h)
      return pbErrNoHandle;
   h->hres = S_OK;

   if (!h->pDiscRecorder)
      return pbErrDeviceNotOpen;

   // this fails if the disc isn't writable...
   h->hres = h->pIRedbookDiscMaster->CreateAudioTrack(frames);
   if (S_OK != h->hres)
      return pbErrCannotStageTrack;

   return pbSuccess;
}

/* Add one frame of audio to the current track.  The buffer must be exactly
   1176 elements long, containing interleaved left and right audio samples.
   The values should be signed 16-bit numbers in the native endianness of
   this computer. */
int PortBurn_AddFrame(void *handle, short *buffer)
{
   PBHandle *h = (PBHandle *)handle;
   int oneBlockByteCount = 1176 * 2;

   if (!h)
      return pbErrNoHandle;
   h->hres = S_OK;

   if (!h->pDiscRecorder)
      return pbErrDeviceNotOpen;

   h->hres = h->pIRedbookDiscMaster->AddAudioTrackBlocks((byte *)buffer,
                                                         oneBlockByteCount);
   if (S_OK != h->hres)
      return pbErrCannotWriteToStagingFile;

   return pbSuccess;
}

/* Finish the current audio track. */
int PortBurn_EndTrack(void *handle)
{
   PBHandle *h = (PBHandle *)handle;
   if (!h)
      return pbErrNoHandle;
   h->hres = S_OK;

   if (!h->pDiscRecorder)
      return pbErrDeviceNotOpen;

   h->hres = h->pIRedbookDiscMaster->CloseAudioTrack();
   if (S_OK != h->hres)
      return pbErrCannotUseStagedFileForBurning;

   return pbSuccess;
}

/* Begin burning the disc. */
int PortBurn_StartBurning(void *handle)
{
   PBHandle *h = (PBHandle *)handle;
   DWORD dwID;

   if (!h)
      return pbErrNoHandle;
   h->hres = S_OK;

   if (!h->pDiscRecorder)
      return pbErrDeviceNotOpen;

   h->hres = h->pDiscRecorder->Close();

   h->cancel = 0;
   h->fraction = 0.0;

   h->simulate = 0;
   h->ejectAfterBurn = 1;

   h->hThread = CreateThread(NULL,
                             0,
                             PortBurn_RecordDisc,
                             h,
                             0,
                             &dwID);
   if (h->hThread == NULL)
      return pbErrCannotStartBurning;

   h->burning = 1;

   return pbSuccess;
}

/* Cancel if burning was in progress.  It might take a while for
   this to take effect; wait until GetStatus says 1.0 to close
   the device. */
int PortBurn_CancelBurning(void *handle)
{
   PBHandle *h = (PBHandle *)handle;
   float frac = 0.0;

   if (!h)
      return pbErrNoHandle;
   h->hres = S_OK;

   if (!h->pDiscRecorder)
      return pbErrDeviceNotOpen;

   if (!h->burning)
      return pbErrNotCurrentlyBurning;

   h->cancel = 1;

   return pbSuccess;
}

/* During burning, returns the fraction complete in the given
   float pointer, from 0.0 to 1.0.  If this function returns
   nonzero, the disc burning has failed and should be aborted.
   If *out_fraction_complete is equal to 1.0, the burning is done;
   you can call PortBurn_CloseDevice.
*/
int PortBurn_GetStatus(void *handle, float *out_fraction_complete)
{
   PBHandle *h = (PBHandle *)handle;

   if (!h)
      return pbErrNoHandle;
   h->hres = S_OK;

   if (!h->pDiscRecorder)
      return pbErrDeviceNotOpen;

   if (!h->burning)
      return pbErrNotCurrentlyBurning;

   WaitForSingleObject(h->hThread, 100);

   *out_fraction_complete = h->fraction;

   return pbSuccess;
}
