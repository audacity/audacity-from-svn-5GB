/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  Buffer.cpp

  Copyright (c) 2004 Dominic Mazzoni, Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include <posh/posh.h>

#include "Cubic.h"
#include "Buffer.h"
#include "Exceptions.h"
#include "Util.h"

namespace Mezzo {

#ifndef DOXYGEN_SHOULD_SKIP_THIS

struct BufferData {

   union {
      float *FloatData;
      posh_i32_t *Int24Data;
      posh_i16_t *Int16Data;
   };

   Buffer::sampleFormat sampleFormat;

   int          len;
   int          refCount;
   Buffer::disposalMethod disposalMethod;

};

static void ConvertInt16ToFloat(posh_i16_t *from, float *to, int len);
static void ConvertInt24ToFloat(posh_i32_t *from, float *to, int len);
static void ConvertFloatToInt24(float *from, posh_i32_t *to, int len, bool dither);
static void ConvertInt16ToInt24(posh_i16_t *from, posh_i32_t *to, int len);
static void ConvertFloatToInt16(float *from, posh_i16_t *to, int len, bool dither);
static void ConvertInt24ToInt16(posh_i32_t *from, posh_i16_t *to, int len, bool dither);

#endif

FloatBuffer EmptyFloatBuffer(0);

//
// Methods common to all Buffers
//

Buffer::Buffer()
{
   mData = new BufferData;
   mData->sampleFormat = FloatSample;
   mData->len = 0;
   mData->disposalMethod = DontDispose;
   mData->refCount = 1;
   mData->FloatData = NULL;
}

Buffer::Buffer(sampleFormat format, int len)
{
   mData = new BufferData;
   mData->sampleFormat = format;
   mData->len = len;
   mData->disposalMethod = DisposeWithDelete;
   mData->refCount = 1;
   switch(format) {
      default:
   case FloatSample:
      mData->FloatData = new float[len];
      break;
   case Int24Sample:
      mData->Int24Data = new posh_i32_t[len];
      break;
   case Int16Sample:
      mData->Int16Data = new posh_i16_t[len];
      break;
   }
}

/// Create a copy of the given buffer.  The data is reference counted, so
/// this constructor (and consequently, operations such as Buffer x = y
/// or Buffer x(y)) are cheap.
///
/// You can also pass one of the derived classes (FloatBuffer, Int24Buffer,
/// or Int16Buffer) as the parameter, and the operation is still cheap
/// (no conversion will take place).
Buffer::Buffer(const Buffer& src)
{
   // reference the same data, and add one to the refcount
   mData = src.mData;
   mData->refCount++;
}

Buffer::~Buffer()
{
   Detach();
}

// Query the length
int Buffer::GetLength() const
{
   return mData->len;
}

// Query the bytes per sample when stored in memory
int Buffer::GetBytesPerSample() const
{
   switch(mData->sampleFormat) {
   default:
   case FloatSample:
      return sizeof(float);
   case Int24Sample:
      return sizeof(posh_i32_t);
   case Int16Sample:
      return sizeof(posh_i16_t);
   }
}

void Buffer::Resize(int newLen)
{
   // special case: if no one else is using this data, and the new len is
   // less than the current len, just use the same buffer
   if(mData->refCount == 1 && newLen <= mData->len)
      mData->len = newLen;
   else
   {
      BufferData *newData = new BufferData;
      newData->refCount = 1;
      newData->disposalMethod = DisposeWithDelete;
      newData->sampleFormat = mData->sampleFormat;
      newData->len = newLen;

      switch(mData->sampleFormat) {
         default:
         case FloatSample:
            newData->FloatData = new float[newLen];
            break;
         case Int24Sample:
            newData->Int24Data = new posh_i32_t[newLen];
            break;
         case Int16Sample:
            newData->Int16Data = new posh_i16_t[newLen];
            break;
      }

      Detach();
      mData = newData;
   }
}

// Query the bytes per sample when packed (usually on disk)
int Buffer::GetPackedBytesPerSample() const
{
   switch(mData->sampleFormat) {
   default:
   case FloatSample:
      return 4;
   case Int24Sample:
      return 3;
   case Int16Sample:
      return 2;
   }
}

/// Set the value of this buffer equal to that of another buffer.  This is
/// reference-counted and thus cheap.  Note that this operation will change
/// the type of this buffer to be the type of the source buffer.
Buffer& Buffer::operator=(const Buffer& src)
{
   Detach();

   mData = src.mData;
   mData->refCount++;

   return *this;
}

/// Multiply all values in this buffer by the given factor.  This
/// effectively scales the buffer's values.
void Buffer::operator*=(float factor)
{
   CopyOnWrite();

   int i;
   switch(mData->sampleFormat)
   {
      case FloatSample:
         for(i=0; i<mData->len; i++)
            mData->FloatData[i] *= factor;
         break;

      case Int24Sample:
         for(i=0; i<mData->len; i++)
            mData->Int24Data[i] = (posh_i32_t)(mData->Int24Data[i] * factor);
         break;

      case Int16Sample:
         for(i=0; i<mData->len; i++)
            mData->Int16Data[i] = (posh_i16_t)(mData->Int16Data[i] * factor);
         break;
   }
}

FloatBuffer Buffer::AsFloat() const
{
   if(mData->sampleFormat == FloatSample)
   {
      mData->refCount++;
      return FloatBuffer(mData);
   }
   else if(mData->sampleFormat == Int24Sample)
   {
      float *floatData = new float[mData->len];
      ConvertInt24ToFloat(mData->Int24Data, floatData, mData->len);
      return FloatBuffer(floatData, 0, mData->len, 1, DisposeWithDelete);
   }
   else if(mData->sampleFormat == Int16Sample)
   {
      float *floatData = new float[mData->len];
      ConvertInt16ToFloat(mData->Int16Data, floatData, mData->len);
      return FloatBuffer(floatData, 0, mData->len, 1, DisposeWithDelete);
   }
   else
      InternalAssert(0, fmt("Unexpected sample format: %d",
                            mData->sampleFormat));

   return FloatBuffer(0);
}

Int24Buffer Buffer::AsInt24(bool dither) const
{
   if(mData->sampleFormat == Int24Sample)
   {
      mData->refCount++;
      return Int24Buffer(mData);
   }
   else if(mData->sampleFormat == FloatSample)
   {
      posh_i32_t *int24Data = new posh_i32_t[mData->len];
      ConvertFloatToInt24(mData->FloatData, int24Data, mData->len, dither);
      return Int24Buffer(int24Data, 0, mData->len, 1, DisposeWithDelete);
   }
   else if(mData->sampleFormat == Int16Sample)
   {
      posh_i32_t *int24Data = new posh_i32_t[mData->len];
      ConvertInt16ToInt24(mData->Int16Data, int24Data, mData->len);
      return Int24Buffer(int24Data, 0, mData->len, 1, DisposeWithDelete);
   }
   else
      InternalAssert(0, fmt("Unexpected sample format: %d",
                            mData->sampleFormat));

   return Int24Buffer(0);
}

Int16Buffer Buffer::AsInt16(bool dither) const
{
   if(mData->sampleFormat == Int16Sample)
   {
      mData->refCount++;
      return Int16Buffer(mData);
   }
   else if(mData->sampleFormat == FloatSample)
   {
      posh_i16_t *int16Data = new posh_i16_t[mData->len];
      ConvertFloatToInt16(mData->FloatData, int16Data, mData->len, dither);
      return Int16Buffer(int16Data, 0, mData->len, 1, DisposeWithDelete);
   }
   else if(mData->sampleFormat == Int24Sample)
   {
      posh_i16_t *int16Data = new posh_i16_t[mData->len];
      ConvertInt24ToInt16(mData->Int24Data, int16Data, mData->len, dither);
      return Int16Buffer(int16Data, 0, mData->len, 1, DisposeWithDelete);
   }
   else
      InternalAssert(0, fmt("Unexpected sample format: %d",
                            mData->sampleFormat));

   return Int16Buffer(0);
}

/// Mix another buffer with this buffer.
/// This particular method (the += operator) will never change the type of
/// the buffer being added to, it will always convert the other buffer to
/// the type of *this* buffer, if they are not the same already
void Buffer::operator+=(const Buffer &right)
{
   // only add as much as both buffers have in common
   int len = (right.mData->len > mData->len) ? mData->len : right.mData->len;
   int i;
   Buffer addFrom;

   if(mData->sampleFormat == right.mData->sampleFormat)
   {
      addFrom = right;
   }
   else
   {
      switch(mData->sampleFormat)
      {
         case FloatSample:
            addFrom = right.AsFloat();
            break;

         case Int24Sample:
            addFrom = right.AsInt24();
            break;

         case Int16Sample:
            addFrom = right.AsInt16();
            break;
      }
   }

   switch(mData->sampleFormat)
   {
      case FloatSample:
         for(i = 0; i < len; i++)
            mData->FloatData[i] += right.mData->FloatData[i];
         break;

      case Int24Sample:
         for(i = 0; i < len; i++)
            mData->Int24Data[i] += right.mData->Int24Data[i];
         break;

      case Int16Sample:
         for(i = 0; i < len; i++)
            mData->Int16Data[i] += right.mData->Int16Data[i];
         break;
   }
}

// Multiplying element-wise
// This particular method (the *= operator) will never change the type of
// the buffer being added to, it will always convert the other buffer to
// the type of *this* buffer, if they are not the same already
void Buffer::operator*=(const Buffer &right)
{
   // only add as much as both buffers have in common
   int len = (right.mData->len > mData->len) ? mData->len : right.mData->len;
   int i;
   Buffer addFrom;

   if(mData->sampleFormat == right.mData->sampleFormat)
   {
      addFrom = right;
   }
   else
   {
      switch(mData->sampleFormat)
      {
         case FloatSample:
            addFrom = right.AsFloat();
            break;

         case Int24Sample:
            addFrom = right.AsInt24();
            break;

         case Int16Sample:
            addFrom = right.AsInt16();
            break;
      }
   }

   switch(mData->sampleFormat)
   {
      case FloatSample:
         for(i = 0; i < len; i++)
            mData->FloatData[i] *= right.mData->FloatData[i];
         break;

      case Int24Sample:
         for(i = 0; i < len; i++)
            mData->Int24Data[i] *= right.mData->Int24Data[i];
         break;

      case Int16Sample:
         for(i = 0; i < len; i++)
            mData->Int16Data[i] *= right.mData->Int16Data[i];
         break;
   }
}


/// Construct a new buffer by appending two existing buffers together.
/// The new buffer will take on sample type of left buffer.
Buffer Buffer::Append(const Buffer& left,
                      const Buffer& right)
{
   switch(left.mData->sampleFormat) {
      default:
   case FloatSample:
      return FloatBuffer(left.AsFloat(), right.AsFloat());
   case Int24Sample:
      return Int24Buffer(left.AsInt24(), right.AsInt24());
   case Int16Sample:
      return Int16Buffer(left.AsInt16(), right.AsInt16());
   }
}

/// Construct a new buffer by appending two existing buffers together.
/// The new buffer will take on sample type of left buffer.
Buffer Buffer::Append(const Buffer& left,
                      const Buffer& middle,
                      const Buffer& right)
{
   switch(left.mData->sampleFormat) {
      default:
   case FloatSample:
      return FloatBuffer(left.AsFloat(),
                         middle.AsFloat(),
                         right.AsFloat());
   case Int24Sample:
      return Int24Buffer(left.AsInt24(),
                         middle.AsInt24(),
                         right.AsInt24());
   case Int16Sample:
      return Int16Buffer(left.AsInt16(),
                         middle.AsInt16(),
                         right.AsInt16());
   }
}

Buffer Buffer::Get(int start) const
{
   return Get(start, GetLength() - start, 1);
}

Buffer Buffer::Get(int start, int len, int stride) const
{
   switch(mData->sampleFormat) {
      default:
      case FloatSample:
         return AsFloat().Get(start, len, stride);
      case Int24Sample: {
         return AsInt24().Get(start, len, stride);
      case Int16Sample:
         return AsInt16().Get(start, len, stride);
      }
   }
}

/// Set samples in this buffer to be equal to the given buffer.  This
/// operation does not change the size of this buffer.
void Buffer::Set(int start, Buffer buffer)
{
   int i;
   CopyOnWrite();

   switch(mData->sampleFormat) {
      case FloatSample: {
         FloatBuffer b = buffer.AsFloat();
         for(i=0; i<b.mData->len; i++)
            mData->FloatData[start+i] = b.mData->FloatData[i];
         break;
      }
      case Int24Sample: {
         Int24Buffer b = buffer.AsInt24();
         for(i=0; i<b.mData->len; i++)
            mData->Int24Data[start+i] = b.mData->Int24Data[i];
         break;
      }
      case Int16Sample: {
         Int16Buffer b = buffer.AsInt16();
         for(i=0; i<b.mData->len; i++)
            mData->Int16Data[start+i] = b.mData->Int16Data[i];
         break;
      }
   }
}

/// Get the type of samples held in the buffer.  In most cases this shouldn't
/// be necessary, but sometimes you may want to act differently
/// based on the type of buffer.
Buffer::sampleFormat Buffer::GetSampleFormat()
{
   return mData->sampleFormat;
}

/// Get a packed representation of this buffer suitable for storing on disk.
/// Start at sample offset start, and read to the end of the buffer.
/// outDataPtr must be GetPackedBytesPerSample() * the number of samples
/// you are requesting.
void Buffer::GetPackedData(char *outDataPtr, int start)
{
   GetPackedData(outDataPtr, start, GetLength() - start);
}

/// Get a packed representation of this buffer suitable for storing on disk.
/// outDataPtr must be GetPackedBytesPerSample() * len.
void Buffer::GetPackedData(char *outDataPtr, int start, int len)
{
   switch(mData->sampleFormat) {
      case FloatSample: {
         memcpy(outDataPtr, mData->FloatData + start, len * sizeof(float));
         break;
      }
      case Int24Sample: {
         // TODO
         break;
      }
      case Int16Sample: {
         memcpy(outDataPtr, mData->Int16Data + start, len * sizeof(posh_i16_t));
         break;
      }
   }
}

//
// What follows are most of the methods for the derived classes.  For these methods,
// the only thing that differs between them is the types involved, so we use
// macros to easily construct all three methods in one fell swoop.
//

#define CONSTRUCT_WITH_INITIAL_VALUES(CLASSNAME, TYPENAME)                \
CLASSNAME ## Buffer::CLASSNAME ## Buffer(int len, TYPENAME initialValues) \
{                                                                         \
   mData = new BufferData;                                                \
   mData->CLASSNAME ## Data = new TYPENAME[len];                          \
   mData->sampleFormat = CLASSNAME ## Sample;                  \
   mData->len = len;                                                      \
   mData->refCount = 1;                                                   \
   mData->disposalMethod = DisposeWithDelete;                             \
                                                                          \
   for(int i = 0; i < len; i++)                                           \
      mData->CLASSNAME ## Data[i] = initialValues;                        \
}

CONSTRUCT_WITH_INITIAL_VALUES(Float, float);
CONSTRUCT_WITH_INITIAL_VALUES(Int24, posh_i32_t);
CONSTRUCT_WITH_INITIAL_VALUES(Int16, posh_i16_t);

#undef CONSTRUCT_WITH_INITIAL_VALUES

// ----------------------------------------------------------------------



// DMtoJH: What if stride != 1 and method != MakeCopy?
// It doesn't look like that coult work...
// It also doesn't work if start > 0 and method != MakeCopy, but
// at least that could be fixed...but do we want to?



// Construct with existing data
#define CONSTRUCT_WITH_EXISTING_DATA(CLASSNAME, TYPENAME)                 \
CLASSNAME ## Buffer::CLASSNAME ## Buffer(TYPENAME *buffer, int start,     \
                                         int len, int stride,             \
                                         disposalMethod method)           \
{                                                                         \
   mData = new BufferData;                                                \
   mData->disposalMethod = method;                                        \
   mData->len = len;                                                      \
   mData->refCount = 1;                                                   \
   mData->sampleFormat = CLASSNAME ## Sample;                              \
   if (method == MakeCopy) {                                              \
      mData->CLASSNAME ## Data = new TYPENAME[len];                       \
      mData->disposalMethod = DisposeWithDelete;                          \
                                                                          \
      if(stride == 1)                                                     \
         memcpy(mData->CLASSNAME ## Data,                                 \
                buffer + start,                                           \
                len*sizeof(TYPENAME));                                    \
      else                                                                \
         for(int i = 0; i < len; i++)                                     \
            mData->CLASSNAME ## Data[i] = buffer[start+i*stride];         \
   }                                                                      \
   else                                                                   \
      mData->CLASSNAME ## Data = buffer + start;                          \
}

CONSTRUCT_WITH_EXISTING_DATA(Float, float)
CONSTRUCT_WITH_EXISTING_DATA(Int24, posh_i32_t)
CONSTRUCT_WITH_EXISTING_DATA(Int16, posh_i16_t)

#undef CONSTRUCT_WITH_EXISTING_DATA

// ----------------------------------------------------------------------

// Construct by appending two existing blocks
#define CONSTRUCT_APPEND(CLASSNAME, TYPENAME)                            \
CLASSNAME ## Buffer::CLASSNAME ## Buffer(const CLASSNAME ## Buffer&left, \
                                         const CLASSNAME ## Buffer&right)\
{                                                                        \
   int leftLen = left.GetLength();                                       \
   int rightLen = right.GetLength();                                     \
   mData = new BufferData;                                               \
   mData->disposalMethod = DisposeWithDelete;                            \
   mData->len = leftLen + rightLen;                                      \
   mData->refCount = 1;                                                  \
   mData->sampleFormat = CLASSNAME ## Sample;                 \
   mData->CLASSNAME ## Data = new TYPENAME[mData->len];                  \
   left.As ## CLASSNAME().Get(mData->CLASSNAME ## Data,           \
                                     0, leftLen);                        \
   right.As ## CLASSNAME().Get(&mData->CLASSNAME ## Data[leftLen],\
                                     0, rightLen);                       \
}

CONSTRUCT_APPEND(Float, float)
CONSTRUCT_APPEND(Int24, posh_i32_t)
CONSTRUCT_APPEND(Int16, posh_i16_t)

#undef CONSTRUCT_APPEND

// ----------------------------------------------------------------------

// Construct by appending three existing blocks
#define CONSTRUCT_APPEND(CLASSNAME, TYPENAME)                            \
CLASSNAME ## Buffer::CLASSNAME ## Buffer(const CLASSNAME ## Buffer&left, \
                                         const CLASSNAME ## Buffer&mid,  \
                                         const CLASSNAME ## Buffer&right)\
{                                                                        \
   int leftLen = left.GetLength();                                       \
   int midLen = mid.GetLength();                                         \
   int rStart = leftLen + midLen;                                        \
   int rightLen = right.GetLength();                                     \
   mData = new BufferData;                                               \
   mData->disposalMethod = DisposeWithDelete;                            \
   mData->len = leftLen + midLen + rightLen;                             \
   mData->refCount = 1;                                                  \
   mData->sampleFormat = CLASSNAME ## Sample;                 \
   mData->CLASSNAME ## Data = new TYPENAME[mData->len];                  \
   left.As ## CLASSNAME().Get(mData->CLASSNAME ## Data,           \
                                     0, leftLen);                        \
   mid.As ## CLASSNAME().Get(&mData->CLASSNAME ## Data[leftLen],  \
                                     0, midLen);                         \
   right.As ## CLASSNAME().Get(&mData->CLASSNAME ## Data[rStart], \
                                     0, rightLen);                       \
}

CONSTRUCT_APPEND(Float, float)
CONSTRUCT_APPEND(Int24, posh_i32_t)
CONSTRUCT_APPEND(Int16, posh_i16_t)

#undef CONSTRUCT_APPEND


// ----------------------------------------------------------------------

// Create a new buffer of the same type as this one
#define NEWBUFFERSAMETYPE(CLASSNAME, TYPENAME)                        \
Buffer CLASSNAME ## Buffer::NewBufferWithSameType(int len)            \
{                                                                     \
   return CLASSNAME ## Buffer(len);                                   \
}

NEWBUFFERSAMETYPE(Float, float)
NEWBUFFERSAMETYPE(Int24, posh_i32_t)
NEWBUFFERSAMETYPE(Int16, posh_i16_t)

#undef NEWSIMILARBUFFER

// ----------------------------------------------------------------------

// Destructor
#define DESTRUCTOR(CLASSNAME, TYPENAME)                              \
CLASSNAME ## Buffer::~CLASSNAME ## Buffer()                          \
{                                                                    \
   Detach();                                                         \
}

DESTRUCTOR(Float, float)
DESTRUCTOR(Int24, posh_i32_t)
DESTRUCTOR(Int16, posh_i16_t)

#undef DESTRUCTOR

// ----------------------------------------------------------------------

#define BUFFER_GET(CLASSNAME, TYPENAME)                                           \
void CLASSNAME ## Buffer::Get(TYPENAME *buffer, int start,                  \
                              int len, int stride) const                    \
{                                                                           \
   ClientAssert(start>=0 && len>=0 && start<=mData->len && stride>0 &&      \
                start+stride*(len-1)<=mData->len,                           \
                fmt("Attempt to Get from a Buffer with "                    \
                    "start=%d ,len=%d, stride=%d, when Buffer len is %d",   \
                    start, len, stride, mData->len));                       \
                                                                            \
   int i;                                                                   \
   int j = start;                                                           \
   for(i=0; i<len; i++) {                                                   \
      buffer[i] = mData->CLASSNAME ## Data[j];                              \
      j += stride;                                                          \
   }                                                                        \
}

BUFFER_GET(Float, float)
BUFFER_GET(Int24, posh_i32_t)
BUFFER_GET(Int16, posh_i16_t)

#undef BUFFER_GET

// ----------------------------------------------------------------------

#define BUFFER_GET_NO_LEN(CLASSNAME, TYPENAME)                              \
void CLASSNAME ## Buffer::Get(TYPENAME *buffer, int start) const            \
{                                                                           \
   ClientAssert(start>=0 && start<=mData->len,                              \
                fmt("Attempt to Get from a Buffer with "                    \
                    "start=%d when Buffer len is %d",                       \
                    mData->len));                                           \
                                                                            \
   Get(buffer, start, mData->len - start);                                  \
}

BUFFER_GET_NO_LEN(Float, float)
BUFFER_GET_NO_LEN(Int24, posh_i32_t)
BUFFER_GET_NO_LEN(Int16, posh_i16_t)

#undef BUFFER_GET_NO_LEN

// ----------------------------------------------------------------------

#define BUFFER_GET_RAW(CLASSNAME, TYPENAME)                                 \
TYPENAME * CLASSNAME ## Buffer::GetPtr(int start /* = 0 */) const           \
{                                                                           \
   return mData->CLASSNAME ## Data + start;                                 \
}

BUFFER_GET_RAW(Float, float)
BUFFER_GET_RAW(Int24, posh_i32_t)
BUFFER_GET_RAW(Int16, posh_i16_t)

#undef BUFFER_GET

// ----------------------------------------------------------------------

#define BUFFER_SET(CLASSNAME, TYPENAME)                                     \
void CLASSNAME ## Buffer::Set(TYPENAME *buffer, int start,                  \
                              int len, int stride)                          \
{                                                                           \
   ClientAssert(start>=0 && len>=0 && start<=mData->len && stride>0 &&      \
                start+stride*(len-1)<=mData->len,                           \
                fmt("Attempt to Set in a Buffer with "                      \
                    "start=%d ,len=%d, stride=%d, when Buffer len is %d",   \
                    start, len, stride, mData->len));                       \
   CopyOnWrite();                                                           \
                                                                            \
   int i;                                                                   \
   int j = start;                                                           \
   for(i=0; i<len; i++) {                                                   \
      mData->CLASSNAME ## Data[j] = buffer[i];                              \
      j += stride;                                                          \
   }                                                                        \
}

BUFFER_SET(Float, float)
BUFFER_SET(Int24, posh_i32_t)
BUFFER_SET(Int16, posh_i16_t)

#undef BUFFER_SET

// ----------------------------------------------------------------------

// Subsetting
#define BUFFER_SUBSET(CLASSNAME, TYPENAME)                                 \
CLASSNAME ## Buffer CLASSNAME ## Buffer::Get(int start, int len,           \
                                             int stride) const             \
{                                                                          \
   return CLASSNAME ## Buffer(mData->CLASSNAME ## Data, start, len,        \
                              stride, MakeCopy);                           \
}

BUFFER_SUBSET(Float, float)
BUFFER_SUBSET(Int24, posh_i32_t)
BUFFER_SUBSET(Int16, posh_i16_t)

#undef BUFFER_SUBSET

// ----------------------------------------------------------------------

#define BUFFER_SUBSET_NO_LEN(CLASSNAME, TYPENAME)                          \
CLASSNAME ## Buffer CLASSNAME ## Buffer::Get(int start) const              \
{                                                                          \
   return CLASSNAME ## Buffer(mData->CLASSNAME ## Data, start,             \
                              mData->len - start, MakeCopy);               \
}

BUFFER_SUBSET_NO_LEN(Float, float)
BUFFER_SUBSET_NO_LEN(Int24, posh_i32_t)
BUFFER_SUBSET_NO_LEN(Int16, posh_i16_t)

#undef BUFFER_SUBSET_NO_LEN

// ----------------------------------------------------------------------

#define BUFFER_SUBSET(CLASSNAME, TYPENAME)                                 \
void CLASSNAME ## Buffer::Set(int start, CLASSNAME ## Buffer buffer)       \
{                                                                          \
   CopyOnWrite();                                                          \
   buffer.Get(&mData->CLASSNAME ## Data[start], 0, buffer.mData->len, 1);  \
}

BUFFER_SUBSET(Float, float)
BUFFER_SUBSET(Int24, posh_i32_t)
BUFFER_SUBSET(Int16, posh_i16_t)

#undef BUFFER_SUBSET


// ----------------------------------------------------------------------

// Adding or multiplying by scalars
#define BUFFER_OPERATOR_PLUS_EQUALS(CLASSNAME, TYPENAME)                   \
void CLASSNAME ## Buffer::operator+=(TYPENAME offset)                      \
{                                                                          \
   CopyOnWrite();                                                          \
                                                                           \
   int i;                                                                  \
   for(i=0; i<mData->len; i++)                                             \
      mData->CLASSNAME ## Data[i] += offset;                               \
}

BUFFER_OPERATOR_PLUS_EQUALS(Float, float)
BUFFER_OPERATOR_PLUS_EQUALS(Int24, posh_i32_t)
BUFFER_OPERATOR_PLUS_EQUALS(Int16, posh_i16_t)

#undef BUFFER_OPERATOR_PLUS_EQUALS

// ----------------------------------------------------------------------

// Array-like interface
#define BUFFER_OPERATOR_ARRAY(CLASSNAME, TYPENAME)                         \
TYPENAME CLASSNAME ## Buffer::operator[](int index) const                  \
{                                                                          \
   ClientAssert(index>=0 && index<mData->len,                              \
                fmt("Attempt to access Buffer[%d] when Buffer len is %d",  \
                    index, mData->len));                                   \
                                                                           \
   return mData->CLASSNAME ## Data[index];                                 \
}
BUFFER_OPERATOR_ARRAY(Float, float)
BUFFER_OPERATOR_ARRAY(Int24, posh_i32_t)
BUFFER_OPERATOR_ARRAY(Int16, posh_i16_t)

#undef BUFFER_OPERATOR_ARRAY

// ----------------------------------------------------------------------

#define BUFFER_OPERATOR_ARRAY_WRITE(CLASSNAME, TYPENAME)                   \
TYPENAME& CLASSNAME ## Buffer::operator[](int index)                       \
{                                                                          \
   ClientAssert(index>=0 && index<mData->len,                              \
                fmt("Attempt to access &Buffer[%d] when Buffer len is %d", \
                    index, mData->len));                                   \
   CopyOnWrite();                                                          \
                                                                           \
   return mData->CLASSNAME ## Data[index];                                 \
}

BUFFER_OPERATOR_ARRAY_WRITE(Float, float)
BUFFER_OPERATOR_ARRAY_WRITE(Int24, posh_i32_t)
BUFFER_OPERATOR_ARRAY_WRITE(Int16, posh_i16_t)

#undef BUFFER_OPERATOR_ARRAY_WRITE

// ----------------------------------------------------------------------

#define BUFFER_GET_MIN(CLASSNAME, TYPENAME)                                \
TYPENAME CLASSNAME ## Buffer::GetMin() const                               \
{                                                                          \
   if (mData->len <= 0)                                                    \
      return (TYPENAME)0;                                                  \
                                                                           \
   int i;                                                                  \
   TYPENAME min = mData->CLASSNAME ## Data[0];                             \
   for(i=1; i<mData->len; i++)                                             \
      if (mData->CLASSNAME ## Data[i] < min)                               \
         min = mData->CLASSNAME ## Data[i];                                \
   return min;                                                             \
}

BUFFER_GET_MIN(Float, float)
BUFFER_GET_MIN(Int24, posh_i32_t)
BUFFER_GET_MIN(Int16, posh_i16_t)

#undef BUFFER_GET_MIN

// ----------------------------------------------------------------------

#define BUFFER_GET_MAX(CLASSNAME, TYPENAME)                                \
TYPENAME CLASSNAME ## Buffer::GetMax() const                               \
{                                                                          \
   if (mData->len <= 0)                                                    \
      return (TYPENAME)0;                                                  \
                                                                           \
   int i;                                                                  \
   TYPENAME max = mData->CLASSNAME ## Data[0];                             \
   for(i=1; i<mData->len; i++)                                             \
      if (mData->CLASSNAME ## Data[i] > max)                               \
         max = mData->CLASSNAME ## Data[i];                                \
   return max;                                                             \
}

BUFFER_GET_MAX(Float, float)
BUFFER_GET_MAX(Int24, posh_i32_t)
BUFFER_GET_MAX(Int16, posh_i16_t)

#undef BUFFER_GET_MAX

// ----------------------------------------------------------------------

#define BUFFER_GET_ABS_MAX(CLASSNAME, TYPENAME, ABSFUNC)                  \
TYPENAME CLASSNAME ## Buffer::GetAbsMax() const                           \
{                                                                         \
   if (mData->len <= 0)                                                   \
      return (TYPENAME)0;                                                 \
                                                                          \
   int i;                                                                 \
   TYPENAME max = ABSFUNC(mData->CLASSNAME ## Data[0]);                   \
   for(i=1; i<mData->len; i++)                                            \
      if (fabs(mData->CLASSNAME ## Data[i]) > max)                        \
         max = (TYPENAME)ABSFUNC((TYPENAME) mData->CLASSNAME ## Data[i]); \
   return max;                                                            \
}

BUFFER_GET_ABS_MAX(Float, float, fabs)
BUFFER_GET_ABS_MAX(Int24, posh_i32_t, abs)
BUFFER_GET_ABS_MAX(Int16, posh_i16_t, abs)

#undef BUFFER_GET_ABS_MAX

// ----------------------------------------------------------------------

FloatBuffer::FloatBuffer(const FloatBuffer &src)
{
   mData = src.mData;
   mData->refCount++;
}

FloatBuffer& FloatBuffer::operator=(const FloatBuffer& src)
{
   Detach();

   mData = src.mData;
   mData->refCount++;

   return *this;
}

Int24Buffer::Int24Buffer(const Int24Buffer &src)
{
   mData = src.mData;
   mData->refCount++;
}

Int24Buffer& Int24Buffer::operator=(const Int24Buffer& src)
{
   Detach();

   mData = src.mData;
   mData->refCount++;

   return *this;
}

Int16Buffer::Int16Buffer(const Int16Buffer &src)
{
   mData = src.mData;
   mData->refCount++;
}


Int16Buffer& Int16Buffer::operator=(const Int16Buffer& src)
{
   Detach();

   mData = src.mData;
   mData->refCount++;

   return *this;
}

float FloatBuffer::GetRMS() const
{
   if (mData->len <= 0)
      return (float)0;

   int i;
   float sumsq = (float)0.0;
   for(i=0; i<mData->len; i++)
      sumsq += ((double)mData->FloatData[i]) * ((double)mData->FloatData[i]);
   return (float)(sqrt(sumsq/mData->len));
}

float FloatBuffer::GetSumSq() const
{
   int i;
   float sumsq = (float)0.0;
   for(i=0; i<mData->len; i++)
      sumsq += ((double)mData->FloatData[i]) * ((double)mData->FloatData[i]);
   return sumsq;
}

float FloatBuffer::GetSum() const
{
   int i;
   float sum = 0;
   for(i=0; i<mData->len; i++)
      sum += mData->FloatData[i];
   return sum;
}

float FloatBuffer::GetMean() const
{
   if (mData->len <= 0)
      return (float)0.0;

   int i;
   double sum = 0.0;
   for(i=0; i<mData->len; i++)
      sum += mData->FloatData[i];
   return (float)(sum / mData->len);
}

//
// Methods unique to a FloatBuffer
//

// Returns interpolated x-position of peak
float FloatBuffer::GetPeak(int start, int stop) const
{
   ClientAssert(start >= 0 && start < mData->len &&
                stop >= start && stop < mData->len,
                fmt("Attempt to GetPeak of FloatBuffer when "
                    "start=%d, stop=%d, and FloatBuffer len is %d",
                    start, stop, mData->len));

   float *a = mData->FloatData;
   float max = a[start];
   int argmax = start;
   int i;
   for(i=start+1; i<=stop; i++)
      if (a[i] > max) {
         max = a[i];
         argmax = i;
      }

   if (argmax >= start+1 && argmax <= stop-2)
      return (argmax-1) + CubicMaximize(a[argmax-1], a[argmax],
                                        a[argmax+1], a[argmax+2]);
   else
      return argmax;
}

float FloatBuffer::GetPeak() const
{
   return GetPeak(0, mData->len-1);
}

// Interpolating values between samples
float FloatBuffer::Interpolate(float x) const
{
   float *a = mData->FloatData;
   int len = mData->len;

   if (len == 0)
      return 0;
   else if (len < 4) {
      // Need at least 4 points to use cubic interpolation.
      // Just find nearest sample.
      if (x < 0)
         return a[0];
      else if (x > len-1)
         return a[len-1];
      else
         return a[(int)(x+0.5)];
   }
   else if (x < 1.0)
      return CubicInterpolate(a[0], a[1], a[2], a[3], x);
   else if (x >= len-2)
      return CubicInterpolate(a[len-4], a[len-3], a[len-2], a[len-1],
                              x-(len-4));
   else {
      int k = (int)(x-1);
      return CubicInterpolate(a[k], a[k+1], a[k+2], a[k+3], x-k);
   }
}

// Vector math operations - return new FloatBuffers.
FloatBuffer FloatBuffer::Ln() const
{
   FloatBuffer tmp(*this);
   tmp.ApplyLn();
   return tmp;
}

FloatBuffer FloatBuffer::Log10() const
{
   FloatBuffer tmp(*this);
   tmp.ApplyLog10();
   return tmp;
}

FloatBuffer FloatBuffer::ToDB() const
{
   FloatBuffer tmp(*this);
   tmp.ApplyToDB();
   return tmp;
}

FloatBuffer FloatBuffer::Exp() const
{
   FloatBuffer tmp(*this);
   tmp.ApplyExp();
   return tmp;
}

FloatBuffer FloatBuffer::Pow(float exponent) const
{
   FloatBuffer tmp(*this);
   tmp.ApplyPow(exponent);
   return tmp;
}

FloatBuffer FloatBuffer::FromDB() const
{
   FloatBuffer tmp(*this);
   tmp.ApplyFromDB();
   return tmp;
}

FloatBuffer FloatBuffer::Sqr() const // Square
{
   FloatBuffer tmp(*this);
   tmp.ApplySqr();
   return tmp;
}

FloatBuffer FloatBuffer::Sqrt() const // Square root
{
   FloatBuffer tmp(*this);
   tmp.ApplySqrt();
   return tmp;
}

// In-place vector math operations.
void FloatBuffer::ApplyLn() const
{
   int i;
   for(i=0; i<mData->len; i++)
      mData->FloatData[i] = log(mData->FloatData[i]);
}

void FloatBuffer::ApplyLog10() const
{
   int i;
   for(i=0; i<mData->len; i++)
      mData->FloatData[i] = log10(mData->FloatData[i]);
}

void FloatBuffer::ApplyToDB() const
{
   int i;
   for(i=0; i<mData->len; i++)
      mData->FloatData[i] = 20 * log10(mData->FloatData[i]);
}

void FloatBuffer::ApplyExp() const
{
   int i;
   for(i=0; i<mData->len; i++)
      mData->FloatData[i] = exp(mData->FloatData[i]);
}

void FloatBuffer::ApplyPow(float exponent) const
{
   int i;
   for(i=0; i<mData->len; i++)
      mData->FloatData[i] = pow(mData->FloatData[i], exponent);
}

void FloatBuffer::ApplyFromDB() const
{
   int i;
   for(i=0; i<mData->len; i++)
      mData->FloatData[i] = pow(10.0, mData->FloatData[i] / 20.0);
}

void FloatBuffer::ApplySqr() const  // Square
{
   int i;
   for(i=0; i<mData->len; i++)
      mData->FloatData[i] *= mData->FloatData[i];
}

void FloatBuffer::ApplySqrt() const // Square root
{
   int i;
   for(i=0; i<mData->len; i++)
      mData->FloatData[i] = sqrt(mData->FloatData[i]);
}

float FloatBuffer::operator[](float x) const
{
   return Interpolate(x);
}


//
// Here are general Buffer methods
//

#ifndef DOXYGEN_SHOULD_SKIP_THIS

void Buffer::CopyOnWrite()
{
   if (mData->refCount > 1) {
      BufferData *oldData = mData;
      mData = new BufferData;
      mData->len = oldData->len;
      mData->sampleFormat = oldData->sampleFormat;
      mData->disposalMethod = DisposeWithDelete;
      mData->refCount = 1;

      switch(mData->sampleFormat)
      {
         case FloatSample:
            mData->FloatData = new float[mData->len];
            memcpy(mData->FloatData, oldData->FloatData, mData->len * sizeof(float));
            break;

         case Int24Sample:
            mData->Int24Data = new posh_i32_t[mData->len];
            memcpy(mData->Int24Data, oldData->Int24Data, mData->len * sizeof(posh_i32_t));
            break;

         case Int16Sample:
            mData->Int16Data = new posh_i16_t[mData->len];
            memcpy(mData->Int16Data, oldData->Int16Data, mData->len * sizeof(posh_i16_t));
            break;
      }

      oldData->refCount--;
   }
}

void Buffer::Detach()
{
   if (!mData)
      return;

   mData->refCount--;
   if (mData->refCount == 0) {
      if (mData->disposalMethod == DisposeWithDelete)
      {
         switch(mData->sampleFormat)
         {
            case FloatSample:
               delete[] mData->FloatData;
               break;

            case Int24Sample:
               delete[] mData->Int24Data;
               break;

            case Int16Sample:
               delete[] mData->Int16Data;
               break;
         }
      }
      else if (mData->disposalMethod == DisposeWithFree)
      {
         switch(mData->sampleFormat)
         {
            case FloatSample:
               free(mData->FloatData);
               break;

            case Int24Sample:
               free(mData->Int24Data);
               break;

            case Int16Sample:
               free(mData->Int16Data);
               break;
         }
      }

      delete mData;
   }

   mData = NULL;
}

#endif

#ifndef DOXYGEN_SHOULD_SKIP_THIS

//
// Conversion routines
//

static void ConvertInt16ToFloat(posh_i16_t *from, float *to, int len)
{
   for(int i = 0; i < len; i++)
      to[i] = (from[i] + 0.5) / 32767.5;
}

static void ConvertInt24ToFloat(posh_i32_t *from, float *to, int len)
{
   for(int i = 0; i < len; i++)
      to[i] = (from[i] + 0.5) / 8388607.5;
}

static void ConvertFloatToInt24(float *from, posh_i32_t *to, int len, bool dither)
{
   printf("TODO: ConvertFloatToInt24\n");
}

static void ConvertInt16ToInt24(posh_i16_t *from, posh_i32_t *to, int len)
{
   for(int i = 0; i < len; i++)
      to[i] = from[i] << 8;
}

static void ConvertFloatToInt16(float *from, posh_i16_t *to, int len, bool dither)
{
   printf("TODO: ConvertFloatToInt16\n");
}

static void ConvertInt24ToInt16(posh_i32_t *from, posh_i16_t *to, int len, bool dither)
{
   printf("TODO: ConvertInt24ToInt16\n");
}

#endif


} // namespace

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

