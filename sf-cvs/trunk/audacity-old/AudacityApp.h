/**********************************************************************

  Audacity: A Digital Audio Editor

  AudacityApp.h

  Dominic Mazzoni

  This is the main source file for Audacity which handles
  initialization and termination by subclassing wxApp.

**********************************************************************/

// Increment this every time you release a new version
#define AUDACITY_VERSION_STRING "0.93"

// Don't change this unless the file format changes
// in an irrevocable way
#define AUDACITY_FILE_FORMAT_VERSION "0.9"

class wxWindow;

// Macintosh-specific: Audacity's four-character "creator" code
// and project type code
#define AUDACITY_CREATOR      'auDy'
#define AUDACITY_PROJECT_TYPE 'auDp'

extern wxWindow *gParentWindow;

void QuitAudacity();

class AudacityApp: public wxApp
{
public:
  bool OnInit(void);

private:
  void RunTest();
};
