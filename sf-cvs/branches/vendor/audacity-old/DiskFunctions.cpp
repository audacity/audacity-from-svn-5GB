//headers and prototype, remove these to embed this into another module

#ifdef WIN32
#include <windows.h>		//the windows stuff
#include <tchar.h>
#endif
#include <iostream.h>
#include <stdio.h>			//the std I/O stuff

//GetFreeSpace is an obsoleted Win-16 API name and cannot be used.
#ifdef WIN32
long GetFreeDiskSpace( TCHAR *path );
#else
long GetFreeDiskSpace( char *path );
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
#else
long GetFreeDiskSpace( char *path )
{
	return -1L;
}
#endif