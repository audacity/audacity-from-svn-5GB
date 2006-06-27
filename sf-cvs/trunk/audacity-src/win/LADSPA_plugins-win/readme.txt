"readme.txt" for LADSPA_plugins-win

Leland Lucius
Vaughan Johnson (vaughan @ audacityteam . org)
May 2006

LADSPA_plugins-win is a project to port Steve Harris's LADSPA plugins 
(http://plugin.org.uk/) to Windows OS as DLLs. 

The primary goal is to port these plugins for use in the Windows version of 
Audacity (http://audacity.sourceforge.net/), but the plugins should work in 
any LADSPA host on Windows (if there are any others).

Because the GNU C library ("GLIBC", http://www.gnu.org/software/libc/libc.html) 
#include files typically are part of Linux but not Windows, we need to track 
those too, so another goal is to make it easy to update for new releases of 
GLIBC include files. GLIBC is a big download that changes infrequently, 
and that code requires more changes to work properly on Windows. So, we don't 
keep the same directory structure as the GLIBC release, but instead keep 
them in a simple directory structure that best fits the #include statements 
in Steve's code. 


REQUIREMENTS:
* Microsoft Visual Studio 2005. 

* Latest plugin distro from http://plugin.org.uk.

* If you wish to use the following plugins, you will need to get the
	version 3 FFTW DLLs from http://www.fftw.org/ and place them in the Audacity
	plugin directory:

		imp_1199.dll
		mbeq_1197.dll
		morph_1917.dll
		pitch_scale_1193.dll
		pitch_scale_1194.dll

LADSPA_plugins-win has been proven with these versions:
	* swh-plugins-0.4.14 from http://plugin.org.uk
	* glibc-2.4


PROCEDURE: 
1. Put the LADSPA_plugins-win directory inside the latest distro 
	from http://plugin.org.uk.

2. Open "LADSPA_plugins-win.sln". Build. DLLs will appear in Debug and/or 
	Release subdirectories of LADSPA_plugins-win.
	
3. Move the DLLs to the Audacity\Plug-Ins folder.