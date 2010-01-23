/**********************************************************************

  Mezzo: A Cross-Platform Audio Editing Engine

  Buffer.h

  Copyright (c) 2004 Dominic Mazzoni, Joshua Haberman

  This program is free software and comes with no warranty; for more
  information, see the file LICENSE.txt or visit
  http://audacity.sourceforge.net/mezzo/license/

**********************************************************************/

#ifndef __MEZZO_BUFFER__
#define __MEZZO_BUFFER__

#include "Exceptions.h"

#include <posh/posh.h>

#include <string>

namespace Mezzo {

struct BufferData;
class Buffer;
class FloatBuffer;
class Int24Buffer;
class Int16Buffer;

/// A reference-counted array of samples with useful methods and operator overloading.

/// A "smart" array of samples with reference-counting, lots of useful
/// methods, and operator overloading.
///
/// There is an inheritance hierarchy, where "Buffer" is the parent and
/// FloatBuffer, Int24Buffer, and Int16Buffer are the children.  The
/// design is unusual, though; the parent class contains all the data
/// and all of the methods whose functions signatures do not expose
/// the type of the buffer.  The subclasses contain no extra data,
/// only methods that cannot go in the base class.

class Buffer {
 public:

   Buffer();

   /// How the Buffer class should dispose of memory supplied to it in the constructor.
   typedef enum {
      DontDispose,        ///< Take no action to free the memory
      MakeCopy,           ///< Copy the supplied buffer, and delete it when finished
      DisposeWithFree,    ///< Delete the buffer with the C call free() when finished
      DisposeWithDelete   ///< Delete the buffer with the C++ operator delete when finished
   } disposalMethod;

   /// The format of samples contained in this buffer
   typedef enum {
      FloatSample,     ///< 32-bit IEEE floating-point numbers
      Int24Sample,     ///< 24-bit ints packed in the low 3 bits of a 32-bit int
      Int16Sample      ///< 16-bit signed integers
   } sampleFormat;


   // Destructor
   ~Buffer();

   /// Create a new buffer of the specified type and length
   Buffer(sampleFormat format, int len);

   /// Create a copy of the given buffer.
   Buffer(const Buffer &src);

   /// Set the value of this buffer equal to that of another buffer.
   Buffer& operator=(const Buffer& src);


   /// Construct a new buffer by appending two existing buffers together.
   static Buffer Append(const Buffer& left,
                        const Buffer& right);

   /// Construct a new buffer by appending three existing buffers together.
   static Buffer Append(const Buffer& left,
                        const Buffer& middle,
                        const Buffer& right);

   /// Query the length of this buffer in samples.
   int GetLength() const;

   /// Query the bytes per sample when stored in memory
   int GetBytesPerSample() const;

   /// Query the bytes per sample when packed on disk
   int GetPackedBytesPerSample() const;

   /// Get a packed representation of this buffer suitable for storing on disk.
   void GetPackedData(char *outDataPtr, int start = 0);

   /// Get a packed representation of this buffer suitable for storing on disk.
   void GetPackedData(char *outDataPtr, int start, int len);

   /// Change the size of the buffer.  The data is not preserved.
   void Resize(int newSize);

   /// Get the type of samples held in the buffer.
   sampleFormat GetSampleFormat();

   /// Get a subset of the buffer
   Buffer Get(int start) const;

   /// Get a subset of the buffer.
   Buffer Get(int start, int len, int stride = 1) const;

   /// Set samples in this buffer to be equal to the given buffer.
   void Set(int start, Buffer buffer);

   /// Multiply all values in this buffer by the given factor.
   void operator*=(float factor);

   /// Get this buffer as a FloatBuffer, converting if necessary
   FloatBuffer AsFloat() const;

   /// Get this buffer as an Int24Buffer, converting if necessary
   Int24Buffer AsInt24(bool dither = false) const;

   /// Get this buffer as an Int16Buffer, converting if necessary
   Int16Buffer AsInt16(bool dither = false) const;

   /// Mix another buffer with this buffer.
   void operator+=(const Buffer &right);

   // Multiplying element-wise
   void operator*=(const Buffer &right);

 protected:
#ifndef DOXYGEN_SHOULD_SKIP_THIS
   BufferData *mData;

   void Detach();
   void CopyOnWrite();

   // without these, copy constructors of the form:
   //    FloatBuffer(const Buffer& src)
   // aren't given access to protected members of src.  Weird.
   friend class FloatBuffer;
   friend class Int24Buffer;
   friend class Int16Buffer;
#endif
};

#define COMMON_DERIVED_CLASS_METHODS(CLASSNAME, TYPENAME)                    \
 private:                                                                    \
   CLASSNAME ## Buffer(BufferData* data) { mData = data; }                   \
                                                                             \
 public:                                                                     \
   /* Construct with zeros */                                                \
   CLASSNAME ## Buffer(int len, TYPENAME initialValues=0);                   \
                                                                             \
   /* Create a new buffer of the same type as this one */                    \
   Buffer NewBufferWithSameType(int len);                                    \
                                                                             \
   /* Construct with existing data */                                        \
   CLASSNAME ## Buffer(TYPENAME *buffer, int start, int len, int stride=1,   \
                       disposalMethod method=MakeCopy);                      \
                                                                             \
   /* Copy constructor. Only adds references to existing data */             \
   CLASSNAME ## Buffer(const CLASSNAME ## Buffer &src);                      \
                                                                             \
   /* Construct by appending two or three existing buffers together */       \
   CLASSNAME ## Buffer(const CLASSNAME ## Buffer &left,                      \
                       const CLASSNAME ## Buffer &right);                    \
                                                                             \
   CLASSNAME ## Buffer(const CLASSNAME ## Buffer &left,                      \
                       const CLASSNAME ## Buffer &middle,                    \
                       const CLASSNAME ## Buffer &right);                    \
                                                                             \
   /* Assignment operator  */                                                \
   CLASSNAME ## Buffer& operator=(const CLASSNAME ## Buffer &src);           \
                                                                             \
   /* Destructor */                                                          \
   ~CLASSNAME ## Buffer();                                                   \
                                                                             \
   /* Getting samples */                                                     \
   void Get(TYPENAME *buffer, int start) const;                              \
   void Get(TYPENAME *buffer, int start, int len, int stride=1) const;       \
   TYPENAME *GetPtr(int start = 0) const;                                    \
                                                                             \
   /* Setting samples */                                                     \
   void Set(TYPENAME *buffer, int start, int len, int stride=1);             \
                                                                             \
   /* Query summary statistics */                                            \
   TYPENAME GetMin() const;                                                  \
   TYPENAME GetMax() const;                                                  \
   TYPENAME GetAbsMax() const;                                               \
                                                                             \
   /* Subsetting */                                                          \
   CLASSNAME ## Buffer Get(int start) const;                                 \
   /* Subsetting */                                                          \
   CLASSNAME ## Buffer Get(int start, int len, int stride=1) const;          \
                                                                             \
   void Set(int start, CLASSNAME ## Buffer buffer);                          \
                                                                             \
   /* Adding or multiplying by scalars */                                    \
   void operator+=(TYPENAME offset);                                         \
                                                                             \
   /* Array-like interface */                                                \
   TYPENAME operator[](int index) const;                                     \
   TYPENAME& operator[](int index);                                          \
                                                                             \
   friend class Buffer;


class FloatBuffer : public Buffer
{
   COMMON_DERIVED_CLASS_METHODS(Float, float)

   // Vector math operations - return buffers of the same type
   FloatBuffer Ln() const;
   FloatBuffer Log10() const;
   FloatBuffer ToDB() const;
   FloatBuffer Exp() const;
   FloatBuffer Pow(float exponent) const;
   FloatBuffer FromDB() const;
   FloatBuffer Sqr() const;  // Square
   FloatBuffer Sqrt() const; // Square root

   // In-place vector math operations.
   void ApplyLn() const;
   void ApplyLog10() const;
   void ApplyToDB() const;
   void ApplyExp() const;
   void ApplyPow(float exponent) const;
   void ApplyFromDB() const;
   void ApplySqr() const;  // Square
   void ApplySqrt() const; // Square root

   /* Synonym for Interpolate; allows you to query values between samples */
   float operator[](float x) const;

   /* Returns interpolated x-position of peak */
   float GetPeak() const;
   float GetPeak(int start, int stop) const;

   float GetRMS() const; /* Root-Mean-Squared */
   float GetMean() const;
   float GetSum() const;    /* get a sum of the samples */
   float GetSumSq() const;  /* get the sum of squaring every sample */

   /* Interpolating values between samples */
   float Interpolate(float x) const;

};

class Int24Buffer : public Buffer
{
   COMMON_DERIVED_CLASS_METHODS(Int24, posh_i32_t)
};

class Int16Buffer : public Buffer
{
   COMMON_DERIVED_CLASS_METHODS(Int16, posh_i16_t)
};

inline std::string SampleFormatToStr(Buffer::sampleFormat type)
{
   switch(type)
   {
      case Buffer::FloatSample:
         return std::string("float");

      case Buffer::Int24Sample:
         return std::string("int24");

      case Buffer::Int16Sample:
         return std::string("int16");

      default:
         InternalAssert(false, "Unknown sample format");
         return std::string("");
   }
}

inline Buffer::sampleFormat StrToSampleFormat(std::string str)
{
   if(str == "float")
      return Buffer::FloatSample;
   else if(str == "int24")
      return Buffer::Int24Sample;
   else if(str == "int16")
      return Buffer::Int16Sample;
   else
   {
      InternalAssert(false, "Unknown sample format");
      return Buffer::FloatSample;   // shut the compiler up
   }
}

} // namespace Mezzo

#endif

// Indentation settings for Vim and Emacs.  Please do not modify past this point.
//
// Local Variables:
// c-basic-offset: 3
// indent-tabs-mode: nil
// End:
//
// vim: et sts=3 sw=3

