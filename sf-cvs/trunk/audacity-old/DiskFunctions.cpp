/**********************************************************************

  Audacity: A Digital Audio Editor

  DiskFunctions.cpp

  Win: Mark Tomlinson
  Mac: Dominic Mazzoni

**********************************************************************/

#ifdef WIN32
#include <windows.h>		//the windows stuff
#include <tchar.h>
#endif

#ifdef linux
#include <sys/vfs.h>
#endif

#include <iostream.h>
#include <stdio.h>			//the std I/O stuff

//GetFreeSpace is an obsoleted Win-16 API name and cannot be used.
#ifdef WIN32
long GetFreeDiskSpace( TCHAR *path );
#else
long GetFreeDiskSpace( const char *path );
#endif
/**
  * <path> could be a drive letter, full path to a file or directory,
  * or local path (in which case the default drive and directory should be
  * grabbed).
  *
  * Since this API is specific only to a DRIVE the path information is moot.
  * If a UNC name (\\myhost\myshare\) is passed in then the trailing \ is 
  * required otherwise it is not needed. With a "" or NULL string for <path> 
  * the default  drive is used. If a 'local' path is used, it MUST include 
  * the leading `\' (i.e. \mydir1\mydir2).
  **/

#ifdef WIN32
long GetFreeDiskSpace( TCHAR *path )
{
	DWORD  dwSectorsPerCluster     = 0;
	DWORD  dwBytesPerSector        = 0;
	DWORD  dwNumberOfFreeClusters  = 0;
	DWORD  dwTotalNumberOfClusters = 0;
	DWORD  dwTotalFree             = 0;
	TCHAR *pszPath                 = path; 
	TCHAR  szPath[ 3 ];           

	memset( szPath, 0, sizeof( char ) );

	/**
	  *	 First some logic to sort out what is getting passed in.
	  *  if it is a local directory, we will use NULL as the
	  *  API will default to the current local drive.
	  *  If it is a UNC then we simply pass it along unchanged.
	  *  If it is neither of these, then we grab the drive letter
	  *  off and append a colon.
	  **/
	if( *pszPath == '\\' )	{	
		// local dir or UNC?
		pszPath = _tcsinc( path );
		if( *pszPath != '\\' )	{ 
			// it is a local dir
			pszPath = NULL;
		}
		//it is a UNC
		pszPath = path;
	}
	else	{
		//grab just the drive letter
		_tcsncpy( szPath, path, 1 );
		//append the :
		szPath[ 1 ] = _T( ':' );
		pszPath = szPath;
	}

	/**
	  *	Now we make the windows API call to get the free space
	  * information and do a little math to add up the free
	  * bytes.
	  **/
	if ( GetDiskFreeSpace( pszPath, &dwSectorsPerCluster,  &dwBytesPerSector, &dwNumberOfFreeClusters, &dwTotalNumberOfClusters))	{
		return (dwNumberOfFreeClusters * dwSectorsPerCluster * dwBytesPerSector);
	}
	else	{ 
		//normally you would call GetLastError here to parse the OS error
		return -1;
	}
}
#elif defined(__WXMAC__)
long GetFreeDiskSpace( const char *path )
{
	char *str = new char[strlen(path)+1];
	strcpy(str, wxUnix2MacFilename(path));
	char *p = str;
	
	/* We expect something like ":Macintosh HD:Audacity:"
	 * and we want to get rid of everything up to the second
	 * colon: "Macintosh HD"
	 */
	if (p[0]==0)
	    return -1;
	char *colon2 = &p[1];
	while(*colon2 && *colon2 != ':')
	    colon2++;
	*colon2 = 0;
	
	if (*p == ':')
	  *p++;
	   
	/* Mac routines want Pascal strings */
	c2pstr(p);
	
	HVolumeParam pb;
	
	pb.ioCompletion = NULL;
	pb.ioVolIndex = -1;
	pb.ioNamePtr = (unsigned char *)p;
	pb.ioVRefNum = 0;
	
	OSErr err = PBHGetVInfo((HParamBlockRec *)&pb, 0);
	
	if (err)
	    return -1;
	
	long freeBytes = ((long)pb.ioVFrBlk)*((long)pb.ioVAlBlkSiz);
	
	delete[] str;
	
	return freeBytes;
}
#elif defined(__WXGTK__)
#ifdef linux
long GetFreeDiskSpace( const char *path )
{
	struct statfs theStats;
	if(statfs(path, &theStats) != 0)
		return -1L;
	
	/* f_bsize is described in the man page as "optimal transfer block size."
	 * I'm not sure what they mean my that, but on my system at least, it
	 * correctly reports the block size of the filesystem. glibc >= 2.1
	 * offers a function "statvfs" which has a field for the actual block
	 * size, but I'd rather not create a dependency on glibc.
	 *
	 * f_bavail is "free blocks available to non-superuser." */
	return theStats.f_bavail * theStats.f_bsize;
}
#else
#warning GetFreeDiskSpace has not been implemented on this system...
long GetFreeDiskSpace( const char *path )
{
	return -1L;
}
#endif
#endif
