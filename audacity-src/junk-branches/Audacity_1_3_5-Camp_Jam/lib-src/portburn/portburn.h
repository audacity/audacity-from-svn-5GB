/*
 * PortBurn
 *
 * Dominic Mazzoni
 * License: LGPL
 *
 * A library for cross-platform audio CD burning
 *
 * All functions return 0 on success and nonzero when there's an error,
 * unless otherwise specified.  The error codes will come from the enum
 * below.  When one of the CD burning or filesystem errors was returned,
 * you might be able to call PortBurn_LastError to get operating-system
 * specific information about what went wrong.  If PortBurn_LastError
 * returns no error, no additional information was available.
 */

#ifndef __PORTBURN__
#define __PORTBURN__

typedef enum {
   pbSuccess = 0,

   /* CD burning errors */
   pbErrCannotEject = -1,
   pbErrCannotAccessDevice = -2,
   pbErrNoMediaInDrive = -3,
   pbErrMediaIsNotBlankAndWritable = -4,
   pbErrCannotReserveDevice = -5,
   pbErrCannotPrepareToBurn = -6,
   pbErrCannotStartBurning = -7,
   pbErrCannotGetBurnStatus = -8,
   pbErrBurnFailed = -9,
   pbErrCannotCloseDevice = -10,

   /* Filesystem errors */
   pbErrCannotCreateStagingDirectory = -101,
   pbErrCannotCreateStagingFile = -102,
   pbErrCannotWriteToStagingFile = -103,
   pbErrCannotStageTrack = -104,
   pbErrCannotAccessStagedFile = -105,
   pbErrCannotUseStagedFileForBurning = -106,

   /* API errors: if these happen, you are not using PortBurn correctly */
   pbErrNoHandle = -501,
   pbErrMustCallGetNumDevices = -502,
   pbErrDeviceNotOpen = -503,
   pbErrAlreadyStagingOrBurning = -504,
   pbErrMustCallStartStaging = -505,
   pbErrMustCallStartTrack = -506,
   pbErrNotCurrentlyBurning = -507
   
} PortBurn_Result;

/* Returns a handle if burning capability is available on this system,
   and NULL otherwise */
void *PortBurn_Open();

/* Cleanup */
void PortBurn_Close(void *handle);

/* Return a human-readable error string for the last operating system
   specific error (NOT human readable strings for the PortBurn error
   codes).  Caller should dispose of the returned string using free(). */
const char *PortBurn_LastError(void *handle);

/* Get the number of devices capable of burning audio CDs.
   If the result is N, then calls to GetDeviceName and OpenDevice
   are guaranteed to be valid for indices from 0 up to N-1, until
   the next time you call GetNumDevices.  At that point, the list of
   devices will be rescanned, and may be different. */
int PortBurn_GetNumDevices(void *handle);

/* Get the name of the device with a given index.  Only valid
   after a call to GetNumDevices. */
char *PortBurn_GetDeviceName(void *handle, int index);

/* Open a particular device by index number.  You can open a device
   even before you're sure if you're going to go through with the
   burn, for example to make Eject available. */
int PortBurn_OpenDevice(void *handle, int index);

/* Close a device */
int PortBurn_CloseDevice(void *handle);

/* Eject the media in the currently opened device */
int PortBurn_EjectDevice(void *handle);

/* Erase the media in the currently opened device */
int PortBurn_EraseDevice(void *handle);

/* This indicates you're ready to start staging audio data for the
   currently opened device.  At this point you are committing to
   exclusive access to the CD burner, and this is the function that
   will fail if another program is using the device, or if there is
   no writable CD media in the device at this point.
   You should pass in the path to a temporary directory that has at
   least 700 MB of free space, to stage the audio, although note that
   not all implementations will make use of this directory. */
int PortBurn_StartStaging(void *handle, const char *tmpdir);

/* Start a new audio track.  Pass the name of the track, and the
   length in CD Audio frames (each frame is 1/75.0 of a second, exactly). */
int PortBurn_StartTrack(void *handle, const char *name, int frames);

/* Add one frame of audio to the current track.  The buffer must be exactly
   1176 elements long, containing interleaved left and right audio samples.
   The values should be signed 16-bit numbers in the native endianness of
   this computer. */
int PortBurn_AddFrame(void *handle, short *buffer);

/* Finish the current audio track. */
int PortBurn_EndTrack(void *handle);

/* Begin burning the disc. */
int PortBurn_StartBurning(void *handle);

/* Cancel if burning was in progress.  It might take a while for
   this to take effect; wait until GetStatus says 1.0 to close
   the device. */
int PortBurn_CancelBurning(void *handle);

/* During burning, returns the fraction complete in the given
   float pointer, from 0.0 to 1.0.  If this function returns
   nonzero, the disc burning has failed and should be aborted.
   If *out_fraction_complete is equal to 1.0, the burning is done;
   you can call PortBurn_CloseDevice.
*/
int PortBurn_GetStatus(void *handle, float *out_fraction_complete);

#endif /* __PORTBURN__ */
