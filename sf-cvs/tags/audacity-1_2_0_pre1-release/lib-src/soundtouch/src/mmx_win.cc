/*****************************************************************************
 *
 * Win32 version of the MMX optimized routines. All MMX optimized functions
 * have been gathered into this single source code file, regardless to their 
 * class or original source code file, in order to ease porting the library
 * to other compiler and processor platforms.
 *
 * This file is to be compiled in Windows platform with Microsoft Visual C++ 
 * Compiler. Please see 'mmx_gcc.cpp' for the gcc compiler version for all
 * GNU platforms.
 *
 * Author        : Copyright (c) Olli Parviainen 2002
 * Author e-mail : oparviai@iki.fi
 * File created  : 13-Jan-2002
 * Last modified : 13-Jan-2002
 *
 * License :
 *
 *  This file is part of SoundTouch sound processing library.
 *
 *  SoundTouch is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  SoundTouch is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with SoundTouch; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *****************************************************************************/

//v #include <soundtouch/mmx.h>
#include <mmx.h>

#ifndef WIN32
#error wrong platform - this source code file is exclusively for Win32 platform
#endif

//////////////////////////////////////////////////////////////////////////////
//
// processor MMX capability detection routines
//
//////////////////////////////////////////////////////////////////////////////


// Flag variable indicating if MMX routines are disabled (for debugging)
static bool _mmxDisabled = false;


// Forces the 'mmxDetect' function to report no MMX support, if called with
// nonzero value in parameter 'disable'.
void mmxDisable(const bool disable)
{
    _mmxDisabled = disable;
}


// Checks if the processor supports MMX. Returns '1' if it does, otherwise '0'
bool mmxDetect(void)
{
    int res;

    if (_mmxDisabled) return false;

    _asm {

        ; check if 'cpuid' instructions is available by toggling eflags bit 21
        ;

        pushfd                      ; save eflags to stack
        pop     eax                 ; load eax from stack (with eflags)
        mov     ecx, eax            ; save the original eflags values to ecx
        xor     eax, 0x00200000     ; toggle bit 21
        push    eax                 ; store toggled eflags to stack
        popfd                       ; load eflags from stack
        pushfd                      ; save updated eflags to stack
        pop     eax                 ; load from stack
        xor     edx, edx            ; clear edx for defaulting no mmx
        cmp     eax, ecx            ; compare to original eflags values
        jz      end                 ; jumps to 'end' if cpuid not present

        ; cpuid instruction available, test for presence of mmx instructions 

        mov     eax, 1;
        cpuid;

//        mov        edx, 0x00800000;    ; force enable MMX

        end:

        mov     res, edx;
        
    }
    
    return (res & 0x00800000) ? true : false;
}


//////////////////////////////////////////////////////////////////////////////
//
// implementation of MMX optimized functions of class 'TDStretch'
//
//////////////////////////////////////////////////////////////////////////////

//v #include <soundtouch/TDStretch.h>
#include <TDStretch.h>
#include <limits.h>

// these are declared in 'TDStretch.cpp'
extern int scanOffsets[4][24];

// Calculates cross correlation of two buffers
_inline int TDStretch::mmxCalculateCrossCorrelation(const Sample *mixingPos, const Sample *compare) const
{
    int corr;
    uint local_overlapLength = overlapLength;
    uint local_overlapDividerBits = overlapDividerBits;

    _asm {
        ; Calculate cross-correlation between the tempOffset and tmpbid_buffer.
        ;
        ; Process 4 parallel batches of 2 * stereo samples each during one 
        ; round to improve CPU-level parallellization.
        ;
        ; load address of sloped compare buffer to eax
        ; load address of mixing point of the sample data buffer to ebx
        ; load counter to ecx = overlapLength / 8 - 1
        ; empty the mm0 
        ;
        ; prepare to the first round by loading 
        ; load mm1 = eax[0]
        ; load mm2 = eax[1];

        mov         eax, dword ptr mixingPos
        mov         ebx, dword ptr compare

        movq        mm1, qword ptr [eax]
        mov         ecx, local_overlapLength

        movq        mm2, qword ptr [eax+8]
        shr         ecx, 3

        pxor        mm0, mm0
        sub         ecx, 1
        
        movd        mm5, local_overlapDividerBits

    loop1:
        ; multiply-add mm1 = mm1 * ebx[0]
        ; multiply-add mm2 = mm2 * ebx[1]
        ;
        ; add mm2 += mm1
        ; mm2 >>= mm5 (=overlapDividerBits)
        ; add mm0 += mm2
        ;
        ; load mm3 = eax[2]
        ; multiply-add mm3 = mm3 * ebx[2]
        ;
        ; load mm4 = eax[3]
        ; multiply-add mm4 = mm4 * ebx[3]
        ;
        ; add mm3 += mm4
        ; mm3 >>= mm5 (=overlapDividerBits)
        ; add mm0 += mm3
        ;
        ; add eax += 4;
        ; add ebx += 4
        ; load mm1 = eax[0] (~eax[4])
        ; load mm2 = eax[1] (~eax[5])
        ;
        ; loop

        pmaddwd     mm1, qword ptr [ebx]
        movq        mm3, qword ptr [eax+16]

        pmaddwd     mm2, qword ptr [ebx+8]
        movq        mm4, qword ptr [eax+24]

        pmaddwd     mm3, qword ptr [ebx+16]
        paddd       mm2, mm1

        pmaddwd     mm4, qword ptr [ebx+24]
        movq        mm1, qword ptr [eax+32]

        psrad       mm2, mm5
        add         eax, 32

        paddd       mm3, mm4
        paddd       mm0, mm2

        movq        mm2, qword ptr [eax+8]
        psrad       mm3, mm5

        add         ebx, 32
        paddd       mm0, mm3

        dec         ecx
        jnz         loop1

        ; Finalize the last partial loop:

        movq        mm3, qword ptr [eax+16]
        pmaddwd     mm1, qword ptr [ebx]

        movq        mm4, qword ptr [eax+24]
        pmaddwd     mm2, qword ptr [ebx+8]

        pmaddwd     mm3, qword ptr [ebx+16]
        paddd       mm2, mm1

         pmaddwd     mm4, qword ptr [ebx+24]
        psrad       mm2, mm5

        paddd       mm3, mm4
        paddd       mm0, mm2

        psrad       mm3, mm5
        paddd       mm0, mm3

        ; copy hi-dword of mm0 to lo-dword of mm1, then sum mmo+mm1
        ; and finally store the result into the variable "corr"

        movq        mm1, mm0
        psrlq       mm1, 32
        paddd        mm0, mm1
        movd        corr, mm0
    }
    return corr;
    
    // Note: Warning about the missing EMMS instruction is harmless
    // as it'll be called elsewhere.
}



_inline void _clearEMMS()
{
    _asm EMMS;
}




// MMX-optimized version of the function "seekBestOverlapPositionStereo"
uint TDStretch::mmxSeekBestOverlapPositionStereo(const Sample *other)
{
    uint bestoffs;
    int bestcorr, corr;
    uint corrOffset, i;

    // Slopes the amplitude of the "midBuffer" samples
    slopeReferenceSamplesStereo();

    bestcorr = INT_MIN;
    bestoffs = 0;
    corrOffset = 0;
//    i = 0;

    // Scans for the best correlation value by testing each possible position
    // over the permitted range.
    for (i = 0; i < seekLength; i ++) {
            // Calculates the cross-correlation value for the mixing position 
            // corresponding to "i"
            corr = mmxCalculateCrossCorrelation(other + 2 * i, pRefMidBuffer);

            // Checks for the highest correlation value
            if (corr > bestcorr) {
                bestoffs = i;
                bestcorr = corr;
            }
        }
    // Clear the MMX state before leaving the subroutine
    _clearEMMS();
    
    return bestoffs;
}




// MMX-optimized version of the function "seekBestOverlapPositionStereoQuick"
uint TDStretch::mmxSeekBestOverlapPositionStereoQuick(const Sample *other)
{
    uint bestpos, scancount, i;
    int bestcorr,corr;
    uint corrPos, tempPos;

    // Slopes the amplitude of the "midBuffer" samples
    slopeReferenceSamplesStereo();

    bestcorr = INT_MIN;
    bestpos = scanOffsets[0][0];
    corrPos = 0;
    tempPos = 0;

    // Scans for the best correlation value using four-pass hierarchical search.
    //
    // The look-up table "scans" has hierarchical position adjusting steps.
    // In first pass the routine searhes for the highest correlation with 
    // relatively coarse steps, then rescans the neighbourhood of the highest
    // correlation with better resolution and so on.

    for (scancount = 0;scancount < 4; scancount ++) {
        i = 0;
        while (scanOffsets[scancount][i]) {
            tempPos = corrPos + scanOffsets[scancount][i];
            if (tempPos >= seekLength) break;

            // Calculates correlation value for the mixing position corresponding
            // to "tempPos"
            corr = mmxCalculateCrossCorrelation(other + 2 * tempPos, pRefMidBuffer);
            // Checks for the highest correlation value
            if (corr > bestcorr) {
                bestpos = tempPos;
                bestcorr = corr;
            }

            i ++;
        }
        corrPos = bestpos;
    }
    // Clear the MMX state before leaving the subroutine
    _clearEMMS();
    
    return bestpos;
}



// MMX-optimized version of the function overlapStereo
void TDStretch::mmxOverlapStereo(Sample *output, const Sample *input) const
{
    float *local_midBuffer = pMidBuffer;
    uint local_overlapLength = overlapLength;
    uint local_overlapDividerBits = overlapDividerBits;

    _asm {
        ; load sliding mixing value counter to mm6 and mm7
        ; load counter value to ecx = overlapLength / 4
        ; load divider-shifter value to esi
        ; load mixing value adder to mm5
        ; load address of midBuffer to eax
        ; load address of inputBuffer added with ovlOffset to ebx
        ; load address of end of the outputBuffer to edx

        mov         eax, local_overlapLength        ; ecx = 0x0000 OVL_
        mov         edi, 0x0002fffe     ; ecx = 0x0002 fffe

        mov            esi, local_overlapDividerBits
        movd        mm6, eax            ; mm6 = 0x0000 0000 0000 OVL_

        mov         ecx, eax;
        sub         eax, 1

        punpckldq   mm6, mm6            ; mm6 = 0x0000 OVL_ 0000 OVL_
        mov         edx, output

        or          eax, 0x00010000     ; eax = 0x0001 overlapLength-1
        mov         ebx, dword ptr input

        movd        mm5, edi            ; mm5 = 0x0000 0000 0002 fffe
        movd        mm7, eax            ; mm7 = 0x0000 0000 0001 01ff

        mov         eax, dword ptr local_midBuffer
        punpckldq   mm5, mm5            ; mm5 = 0x0002 fffe 0002 fffe

        shr         ecx, 2              ; ecx = overlapLength / 2
        punpckldq   mm7, mm7            ; mm7 = 0x0001 01ff 0001 01ff

    loop1:
        ; Process two parallel batches of 2+2 stereo samples during each round 
        ; to improve CPU-level parallellization.
        ;
        ; Load [eax] into mm0 and mm1
        ; Load [ebx] into mm3
        ; unpack words of mm0, mm1 and mm3 into mm0 and mm1
        ; multiply-add mm0*mm6 and mm1*mm7, store results into mm0 and mm1
        ; divide mm0 and mm1 by 512 (=right-shift by overlapDividerBits)
        ; pack the result into mm0 and store into [edx]
        ;
        ; Load [eax+8] into mm2 and mm3
        ; Load [ebx+8] into mm4
        ; unpack words of mm2, mm3 and mm4 into mm2 and mm3
        ; multiply-add mm2*mm6 and mm3*mm7, store results into mm2 and mm3
        ; divide mm2 and mm3 by 512 (=right-shift by overlapDividerBits)
        ; pack the result into mm2 and store into [edx+8]

                
        movq        mm0, qword ptr [eax]    ; mm0 = m1l m1r m0l m0r
        add         edx, 16

        movq        mm3, qword ptr [ebx]    ; mm3 = i1l i1r i0l i0r
        movq        mm1, mm0                ; mm1 = m1l m1r m0l m0r

        movq        mm2, qword ptr [eax+8]  ; mm2 = m3l m3r m2l m2r
        punpcklwd   mm0, mm3                ; mm0 = i0l m0l i0r m0r

        movq        mm4, qword ptr [ebx+8]  ; mm4 = i3l i3r i2l i2r
        punpckhwd   mm1, mm3                ; mm1 = i1l m1l i1r m1r

        movq        mm3, mm2                ; mm3 = m3l m3r m2l m2r
        punpcklwd   mm2, mm4                ; mm2 = i2l m2l i2r m2r

        pmaddwd     mm0, mm6                ; mm0 = i0l*m63+m0l*m62 i0r*m61+m0r*m60
        punpckhwd   mm3, mm4                ; mm3 = i3l m3l i3r m3r

        movd        mm4, esi                ; mm4 = overlapDividerBits

        pmaddwd     mm1, mm7                ; mm1 = i1l*m73+m1l*m72 i1r*m71+m1r*m70
        paddw       mm6, mm5

        paddw       mm7, mm5
        psrad       mm0, mm4                ; mmo >>= overlapDividerBits

        pmaddwd     mm2, mm6                ; mm2 = i2l*m63+m2l*m62 i2r*m61+m2r*m60
        psrad       mm1, mm4                ; mm1 >>= overlapDividerBits

        pmaddwd     mm3, mm7                ; mm3 = i3l*m73+m3l*m72 i3r*m71+m3r*m70
        psrad       mm2, mm4                ; mm2 >>= overlapDividerBits

        packssdw    mm0, mm1                ; mm0 = mm1h mm1l mm0h mm0l
        psrad       mm3, mm4                ; mm3 >>= overlapDividerBits

        add         eax, 16
        paddw       mm6, mm5

        packssdw    mm2, mm3                ; mm2 = mm2h mm2l mm3h mm3l
        paddw       mm7, mm5

        movq        qword ptr [edx-16], mm0
        add         ebx, 16

        movq        qword ptr [edx-8], mm2
        dec         ecx
    
        jnz         loop1

        emms
    }
}


//////////////////////////////////////////////////////////////////////////////
//
// implementation of MMX optimized functions of class 'FIRFilter'
//
//////////////////////////////////////////////////////////////////////////////


// (overloaded) Calculates filter coefficients for MMX routine
void FIRFilter::mmxCalculateCoeffs()
{
    uint i;

    // ... reorder the original filter coefficients for mmx routines 
    for (i = 0;i < length; i += 4) {
        filterCoeffsMMX[2 * i + 0] = filterCoeffs[i + 0];
        filterCoeffsMMX[2 * i + 1] = filterCoeffs[i + 2];
        filterCoeffsMMX[2 * i + 2] = filterCoeffs[i + 0];
        filterCoeffsMMX[2 * i + 3] = filterCoeffs[i + 2];

        filterCoeffsMMX[2 * i + 4] = filterCoeffs[i + 1];
        filterCoeffsMMX[2 * i + 5] = filterCoeffs[i + 3];
        filterCoeffsMMX[2 * i + 6] = filterCoeffs[i + 1];
        filterCoeffsMMX[2 * i + 7] = filterCoeffs[i + 3];
    }
}



// mmx-optimized version of the filter routine for stereo sound
uint FIRFilter::mmxEvaluateFilterStereo(Sample *dest, const Sample *src, const uint numSamples) const
{
    // Create stack copies of the needed member variables for asm routines :
    uint local_length = length;
    uint local_lengthDiv8 = lengthDiv8;
    uint local_resultDivider = resultDivFactor;
    short *local_filterCoeffsMMX = (short*)filterCoeffsMMX;

    _asm {
        ; Load (num_samples-aa_filter_length)/2 to edi as a i
        ; Load a pointer to samples to esi
        ; Load a pointer to destination to edx

        mov            edi, numSamples
        mov         esi, dword ptr src
        sub            edi, local_length
        mov         edx, dword ptr dest
        sar            edi, 1

        ; Load filter length/8 to ecx
        ; Load pointer to samples from esi to ebx
        ; Load counter from edi to ecx
        ; Load [ebx] to mm3
        ; Load pointer to filter coefficients to eax
loop1:
        mov            ebx, esi
        pxor        mm0, mm0

        mov            ecx, local_lengthDiv8
        pxor        mm7, mm7

        movq        mm1, [ebx]                ; mm1 = l1 r1 l0 r0
        mov            eax, local_filterCoeffsMMX
loop2:

        movq        mm2, [ebx+8]            ; mm2 = l3 r3 l2 r2
        movq        mm4, mm1                ; mm4 = l1 r1 l0 r0

        movq        mm3, [ebx+16]            ; mm3 = l5 r5 l4 r4
        punpckhwd    mm1, mm2                ; mm1 = l3 l1 r3 r1

        movq        mm6, mm2                ; mm6 = l3 r3 l2 r2
        punpcklwd    mm4, mm2                ; mm4 = l2 l0 r2 r0

        movq        mm2, qword ptr [eax]    ; mm2 = f2 f0 f2 f0
        movq        mm5, mm1                ; mm5 = l3 l1 r3 r1

        punpcklwd    mm6, mm3                ; mm6 = l4 l2 r4 r2
        pmaddwd        mm4, mm2                ; mm4 = l2*f2+l0*f0 r2*f2+r0*f0

        pmaddwd        mm5, mm2                ; mm5 = l3*f2+l1*f0 r3*f2+l1*f0
        movq        mm2, qword ptr [eax+8]    ; mm2 = f3 f1 f3 f1

        paddd        mm0, mm4                ; mm0 += s02*f02
        movq        mm4, mm3                ; mm4 = l1 r1 l0 r0

        pmaddwd        mm1, mm2                ; mm1 = l3*f3+l1*f1 r3*f3+l1*f1
        paddd        mm7, mm5                ; mm7 += s13*f02

        pmaddwd        mm6, mm2                ; mm6 = l4*f3+l2*f1 r4*f3+f4*f1
        movq        mm2, [ebx+24]            ; mm2 = l3 r3 l2 r2

        paddd        mm0, mm1                ; mm0 += s31*f31
        movq        mm1, [ebx+32]            ; mm1 = l5 r5 l4 r4

        paddd        mm7, mm6                ; mm7 += s42*f31
        punpckhwd    mm3, mm2                ; mm3 = l3 l1 r3 r1

        movq        mm6, mm2                ; mm6 = l3 r3 l2 r2
        punpcklwd    mm4, mm2                ; mm4 = l2 l0 r2 r0

        movq        mm2, qword ptr [eax+16]    ; mm2 = f2 f0 f2 f0
        movq        mm5, mm3                ; mm5 = l3 l1 r3 r1

        punpcklwd    mm6, mm1                ; mm6 = l4 l2 r4 r2
        add            eax, 32

        pmaddwd        mm4, mm2                ; mm4 = l2*f2+l0*f0 r2*f2+r0*f0
        add            ebx, 32

        pmaddwd        mm5, mm2                ; mm5 = l3*f2+l1*f0 r3*f2+l1*f0
        movq        mm2, qword ptr [eax-8]    ; mm2 = f3 f1 f3 f1

        paddd        mm0, mm4                ; mm0 += s02*f02
        pmaddwd        mm3, mm2                ; mm3 = l3*f3+l1*f1 r3*f3+l1*f1

        paddd        mm7, mm5                ; mm7 += s13*f02
        pmaddwd        mm6, mm2                ; mm6 = l4*f3+l2*f1 r4*f3+f4*f1

        paddd        mm0, mm3                ; mm0 += s31*f31
        paddd        mm7, mm6                ; mm7 += s42*f31

        dec            ecx
        jnz            loop2

        ; Divide mm0 and mm7 by 8192 (= right-shift by 13),
        ; pack and store to [edx]
        movd        mm4, local_resultDivider;

        psrad        mm0, mm4                ; divider the result

        add            edx, 8
        psrad        mm7, mm4                ; divider the result

        add            esi, 8
        packssdw    mm0, mm7

        movq        qword ptr [edx-8], mm0
        dec            edi

        jnz            loop1

        emms
    }
    return (numSamples & 0xfffffffe) - local_length;
}
