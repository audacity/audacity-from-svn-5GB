/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApp.h

  Dominic Mazzoni

  This is the main source file for Audacity which handles
  initialization and termination by subclassing wxApp.

**********************************************************************/

// Increment this every time you release a new version
#define AUDACITY_VERSION_STRING "0.9b"

// Don't change this unless the file format changes
// in an irrevocable way
#define AUDACITY_FILE_FORMAT_VERSION "0.9"

class AudacityApp: public wxApp
{
  public:
    bool OnInit(void);
};
