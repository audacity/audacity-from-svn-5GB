#ifndef PA_WIN_DS_H
#define PA_WIN_DS_H
/*
 * $Id: pa_win_ds.h,v 1.3 2007-08-16 20:45:34 richardash1981 Exp $
 * PortAudio Portable Real-Time Audio Library
 * DirectSound specific extensions
 *
 * Copyright (c) 1999-2000 Ross Bencina and Phil Burk
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

/** @file
 @brief DirectSound-specific PortAudio API extension header file.
*/


#include "portaudio.h"

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */


/** Retrieve the GUID of the input device.

 @param stream The stream to query.

 @return A pointer to the GUID, or NULL if none.
*/
LPGUID PaWinDS_GetStreamInputGUID( PaStream* s );


/** Retrieve the GUID of the output device.

 @param stream The stream to query.

 @return A pointer to the GUID, or NULL if none.
*/
LPGUID PaWinDS_GetStreamOutputGUID( PaStream* s );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* PA_WIN_DS_H */
#ifndef PA_WIN_DS_H
#define PA_WIN_DS_H
/*
 * $Id: pa_win_ds.h,v 1.3 2007-08-16 20:45:34 richardash1981 Exp $
 * PortAudio Portable Real-Time Audio Library
 * DirectSound specific extensions
 *
 * Copyright (c) 1999-2000 Ross Bencina and Phil Burk
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files
 * (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge,
 * publish, distribute, sublicense, and/or sell copies of the Software,
 * and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * Any person wishing to distribute modifications to the Software is
 * requested to send the modifications to the original developer so that
 * they can be incorporated into the canonical version.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR
 * ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF
 * CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 */

/** @file
 @brief DirectSound-specific PortAudio API extension header file.
*/


#include "portaudio.h"

#ifdef __cplusplus
extern "C"
{
#endif /* __cplusplus */


/** Retrieve the GUID of the input device.

 @param stream The stream to query.

 @return A pointer to the GUID, or NULL if none.
*/
LPGUID PaWinDS_GetStreamInputGUID( PaStream* s );


/** Retrieve the GUID of the output device.

 @param stream The stream to query.

 @return A pointer to the GUID, or NULL if none.
*/
LPGUID PaWinDS_GetStreamOutputGUID( PaStream* s );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* PA_WIN_DS_H */
