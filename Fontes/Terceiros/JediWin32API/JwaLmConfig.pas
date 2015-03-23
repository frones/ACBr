{******************************************************************************}
{                                                                              }
{ Lan Manager Config API interface Unit for Object Pascal                      }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: lmconfig.h, released November 2001. The original Pascal}
{ code is: LmConfig.pas, released Februari 2002. The initial developer of the  }
{ Pascal code is Marcel van Brakel (brakelm att chello dott nl).               }
{                                                                              }
{ Portions created by Marcel van Brakel are Copyright (C) 1999-2001            }
{ Marcel van Brakel. All Rights Reserved.                                      }
{                                                                              }
{ Obtained through: Joint Endeavour of Delphi Innovators (Project JEDI)        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project JEDI         }
{ APILIB home page, located at http://jedi-apilib.sourceforge.net              }
{                                                                              }
{ The contents of this file are used with permission, subject to the Mozilla   }
{ Public License Version 1.1 (the "License"); you may not use this file except }
{ in compliance with the License. You may obtain a copy of the License at      }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an "AS IS" basis,   }
{ WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for }
{ the specific language governing rights and limitations under the License.    }
{                                                                              }
{ Alternatively, the contents of this file may be used under the terms of the  }
{ GNU Lesser General Public License (the  "LGPL License"), in which case the   }
{ provisions of the LGPL License are applicable instead of those above.        }
{ If you wish to allow use of your version of this file only under the terms   }
{ of the LGPL License and not to allow others to use your version of this file }
{ under the MPL, indicate your decision by deleting  the provisions above and  }
{ replace  them with the notice and other provisions required by the LGPL      }
{ License.  If you do not delete the provisions above, a recipient may use     }
{ your version of this file under either the MPL or the LGPL License.          }
{                                                                              }
{ For more information about the LGPL: http://www.gnu.org/copyleft/lesser.html }
{                                                                              }
{******************************************************************************}

// $Id: JwaLmConfig.pas,v 1.11 2005/09/07 09:54:54 marquardt Exp $

{$IFNDEF JWA_INCLUDEMODE}

unit JwaLmConfig;

{$I jediapilib.inc}

{$WEAKPACKAGEUNIT}

interface

uses
  JwaWindows, JwaLmCons;

{$ENDIF !JWA_INCLUDEMODE}

{$IFDEF JWA_INTERFACESECTION}

{$HPPEMIT ''}
{$HPPEMIT '#include "lmconfig.h"'}
{$HPPEMIT ''}

{$DEFINE LM_REVISED_CONFIG_APIS}

//
// Function Prototypes - Config
//

{$IFDEF LM_REVISED_CONFIG_APIS}

function NetConfigGet(server, component, parameter: LPCWSTR; var bufptr: LPBYTE): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetConfigGet}

function NetConfigGetAll(server, component: LPCWSTR; var bufptr: LPBYTE): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetConfigGetAll}

{$ELSE}

function NetConfigGet(server, component, parameter: LPCWSTR; var bufptr: LPBYTE; totalavailable: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetConfigGet}

function NetConfigGetAll(server, component: LPCWSTR; var bufptr: LPBYTE; totalavailable: LPDWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetConfigGetAll}

{$ENDIF LM_REVISED_CONFIG_APIS}

function NetConfigSet(server, reserved1, component: LPCWSTR; level, reserved2: DWORD; buf: LPBYTE; reserved3: DWORD): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetConfigSet}

function NetRegisterDomainNameChangeNotification(NotificationEventHandle: PHANDLE): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetRegisterDomainNameChangeNotification}

function NetUnregisterDomainNameChangeNotification(NotificationEventHandle: HANDLE): NET_API_STATUS; stdcall;
{$EXTERNALSYM NetUnregisterDomainNameChangeNotification}

//
// Data Structures - Config
//

type
  _CONFIG_INFO_0 = record
    cfgi0_key: LPWSTR;
    cfgi0_data: LPWSTR;
  end;
  {$EXTERNALSYM _CONFIG_INFO_0}
  CONFIG_INFO_0 = _CONFIG_INFO_0;
  {$EXTERNALSYM CONFIG_INFO_0}
  PCONFIG_INFO_0 = ^CONFIG_INFO_0;
  {$EXTERNALSYM PCONFIG_INFO_0}
  LPCONFIG_INFO_0 = ^CONFIG_INFO_0;
  {$EXTERNALSYM LPCONFIG_INFO_0}
  TConfigInfo0 = CONFIG_INFO_0;
  PConfigInfo0 = PCONFIG_INFO_0;  

{$ENDIF JWA_INTERFACESECTION}

{$IFNDEF JWA_INCLUDEMODE}

implementation

uses
  JwaWinDLLNames;

{$ENDIF !JWA_INCLUDEMODE}

{$IFDEF JWA_IMPLEMENTATIONSECTION}

{$IFDEF DYNAMIC_LINK}

var
  _NetConfigGet: Pointer;

function NetConfigGet;
begin
  GetProcedureAddress(_NetConfigGet, netapi32, 'NetConfigGet');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NetConfigGet]
  end;
end;

var
  _NetConfigGetAll: Pointer;

function NetConfigGetAll;
begin
  GetProcedureAddress(_NetConfigGetAll, netapi32, 'NetConfigGetAll');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NetConfigGetAll]
  end;
end;

var
  _NetConfigSet: Pointer;

function NetConfigSet;
begin
  GetProcedureAddress(_NetConfigSet, netapi32, 'NetConfigSet');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NetConfigSet]
  end;
end;

var
  _NetRegisterDomainNameChangeNotification: Pointer;

function NetRegisterDomainNameChangeNotification;
begin
  GetProcedureAddress(_NetRegisterDomainNameChangeNotification, netapi32, 'NetRegisterDomainNameChangeNotification');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NetRegisterDomainNameChangeNotification]
  end;
end;

var
  _NetUnregisterDomainNameChangeNotification: Pointer;

function NetUnregisterDomainNameChangeNotification;
begin
  GetProcedureAddress(_NetUnregisterDomainNameChangeNotification, netapi32, 'NetUnregisterDomainNameChangeNotification');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_NetUnregisterDomainNameChangeNotification]
  end;
end;

{$ELSE}

function NetConfigGet; external netapi32 name 'NetConfigGet';
function NetConfigGetAll; external netapi32 name 'NetConfigGetAll';
function NetConfigSet; external netapi32 name 'NetConfigSet';
function NetRegisterDomainNameChangeNotification; external netapi32 name 'NetRegisterDomainNameChangeNotification';
function NetUnregisterDomainNameChangeNotification; external netapi32 name 'NetUnregisterDomainNameChangeNotification';

{$ENDIF DYNAMIC_LINK}

{$ENDIF JWA_IMPLEMENTATIONSECTION}

{$IFNDEF JWA_INCLUDEMODE}
end.
{$ENDIF !JWA_INCLUDEMODE}
