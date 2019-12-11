/* Eric.Developer: DevClub.idlagam.com */

#include 'hbclass.ch'
#include 'error.ch'

#ifndef _ACBRLIB_CH_
#define _ACBRLIB_CH_

#define STR_LEN 256
#define DC_CALL_CDECL          0x0010      // __cdecl
#define DC_CALL_STD            0x0020      // __stdcall

#if defined( __PLATFORM__Windows ) .AND. !defined( __PLATFORM__WINDOWS )
   #define __PLATFORM__WINDOWS
#endif
#if defined( __PLATFORM__Linux ) .AND. !defined( __PLATFORM__LINUX )
   #define __PLATFORM__LINUX
#endif

#ifdef __PLATFORM__LINUX
   #define DLL_OSAPI DC_CALL_CDECL
#else
   #define DLL_OSAPI DC_CALL_STD
#endif

#endif 