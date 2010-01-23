#ifndef __COMMANDMISC__
#define __COMMANDMISC__

#include <map>
#include <utility>

class wxString;
class wxVariant;
class Validator;

// Map from parameter name to the value of the parameter, with a suitable Validator
typedef std::map<wxString, std::pair<wxVariant, Validator> > ParamMap;

#endif /* End of include guard: __COMMANDMISC__ */
