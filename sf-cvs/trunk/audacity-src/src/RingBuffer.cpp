/**********************************************************************

  Audacity: A Digital Audio Editor

  RingBuffer.cpp

  Dominic Mazzoni

**********************************************************************/

#include "RingBuffer.h"

RingBuffer::RingBuffer(sampleFormat format, int size)
{
   mFormat = format;
   mBufferSize = (size > 64? size: 64);
   mStart = 0;
   mLen = 0;
   mBuffer = NewSamples(mBufferSize, mFormat);
}

RingBuffer::~RingBuffer()
{
   DeleteSamples(mBuffer);
}

//
// For the writer only:
//

int RingBuffer::AvailForPut()
{
   return mBufferSize - mLen;
}

int RingBuffer::Put(samplePtr buffer, sampleFormat format,
                    int samplesToCopy)
{
   samplePtr src;
   int block;
   int copied;
   int pos;

   if (samplesToCopy > mBufferSize - mLen)
      samplesToCopy = mBufferSize - mLen;

   src = buffer;
   copied = 0;
   pos = (mStart + mLen) % mBufferSize;

   while(samplesToCopy) {
      block = samplesToCopy;
      if (block > mBufferSize - pos)
         block = mBufferSize - pos;
      
      CopySamples(src, format,
                  mBuffer + pos * SAMPLE_SIZE(mFormat), mFormat,
                  block);

      src += block * SAMPLE_SIZE(format);
      pos = (pos + block) % mBufferSize;
      samplesToCopy -= block;
      copied += block;
      mLen += block;
   }

   return copied;
}

//
// For the reader only:
//

int RingBuffer::AvailForGet()
{
   return mLen;
}

int RingBuffer::Get(samplePtr buffer, sampleFormat format,
                    int samplesToCopy)
{
   samplePtr dest;
   int block;
   int copied;

   if (samplesToCopy > mLen)
      samplesToCopy = mLen;

   dest = buffer;
   copied = 0;

   while(samplesToCopy) {
      block = samplesToCopy;
      if (block > mBufferSize - mStart)
         block = mBufferSize - mStart;

      CopySamples(mBuffer + mStart * SAMPLE_SIZE(mFormat), mFormat,
                  dest, format,
                  block);

      dest += block;
      mStart = (mStart + block) % mBufferSize;
      mLen -= block;
      samplesToCopy -= block;
      copied += block;
   }

   return copied;
}

int RingBuffer::Discard(int samplesToDiscard)
{
   if (samplesToDiscard > mLen)
      samplesToDiscard = mLen;

   mStart = (mStart + samplesToDiscard) % mBufferSize;
   mLen -= samplesToDiscard;

   return samplesToDiscard;
}

