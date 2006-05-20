import string

dbg = Environment(CCFLAGS = '-Wall -g', CPPPATH= ["#/lib", "#/src"])

release = Environment( CPPFLAGS = '-Wall')

env = dbg
# env = release

SConscript('src/SConscript', build_dir='build', duplicate=0, exports='env')
SConscript('lib/SConscript', exports='env')

env.Program('tests/MezzoTest',
            source = 'tests/MezzoTest.cpp',
            LIBS=['audacity', 'expat', 'posh'], LIBPATH=['build', 'lib'])

env.Program('tests/TestManagedFileContext',
            source = 'tests/TestManagedFileContext.cpp',
            LIBS=['audacity', 'expat', 'posh'],
            LIBPATH=['build', 'lib'])

#env.Program('tests/MezzoRecord',
#            source = 'tests/MezzoRecord.cpp',
#            LIBS=['audacity', 'expat', 'posh', 'portaudio'],
#            LIBPATH=['build', 'lib'],
#            LINKFLAGS=['-framework','CoreAudio',
#                       '-framework','AudioUnit',
#                       '-framework','AudioToolbox'])

#env.Program('tests/MezzoPlay',
#            source = 'tests/MezzoPlay.cpp',
#            LIBS=['audacity', 'expat', 'posh', 'portaudio'],
#            LIBPATH=['build', 'lib'],
#            LINKFLAGS=['-framework','CoreAudio',
#                       '-framework','AudioUnit',
#                       '-framework','AudioToolbox'])

#env.Program('tests/MezzoCacheDisplay',
#            source = 'tests/MezzoCacheDisplay.cpp',
#            LIBS=['audacity', 'expat', 'posh', 'portaudio'],
#            LIBPATH=['build', 'lib'],
#            LINKFLAGS=['-framework','CoreAudio',
#                       '-framework','AudioUnit',
#                       '-framework','AudioToolbox'])

#for test in tests:
#   testname = "tests/" + test
#   env.Program(testname, source = testname + ".cpp",
#               LIBS=['audacity', 'expat'], LIBPATH=['build', 'lib'])

