/**********************************************************************

  Audacity: A Digital Audio Editor

  BlockFile

  Dominic Mazzoni

  This class is a wrapper for a file that represents a block of
  information.  It supports reference counting (the constructor
  defaults the reference count to 1) so that a single block can
  be pointed to by multiple tracks or virtual tracks.  In addition,
  this class hides the internals of the low-level file operations
  (one more layer beyond wxWindows) so that the details can be
  changed later.  For example, one could add a feature that would
  cache frequently used blocks in memory.

  Currently only DirManager should be creating and destroying
  BlockFiles, or managing their reference counts.  You should
  not need to call Ref or Deref directly from anywhere else in
  the program other than from DirManager.
  
**********************************************************************/

#ifndef __AUDACITY_BLOCKFILE__
#define __AUDACITY_BLOCKFILE__

#include <wx/string.h>
#include <wx/ffile.h>

class BlockFile
{
public:
  BlockFile(wxString name, wxString fullPath);
  ~BlockFile();  

  bool Open(bool write);
  void Close();

  int Read(void *data, int len);
  int Write(void *data, int len);
  bool Seek(int where, wxSeekMode mode = wxFromStart);
  int Tell();

  // Eventually these should be put behind accessors

  int refCount;
  wxString name;
  wxString fullPath;  

private:
  
  friend class DirManager;
  
  void Ref();
  bool Deref();

  wxFFile *theFile;

};

#endif


