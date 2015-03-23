{******************************************************************************}
{                                                                              }
{ Windows Error Reporting API interface unit for Object Pascal                 }
{                                                                              }
{ Portions created by Microsoft are Copyright (C) 1995-2001 Microsoft          }
{ Corporation. All Rights Reserved.                                            }
{                                                                              }
{ The original file is: errorrep.h, released June 2000. The original Pascal    }
{ code is: ErrorRep.pas, released December 2000. The initial developer of the  }
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

// $Id: JwaErrorRep.pas,v 1.10 2005/09/06 16:36:50 marquardt Exp $

unit JwaErrorRep;

{$WEAKPACKAGEUNIT}

{$HPPEMIT ''}
{$HPPEMIT '#include "errorrep.h"'}
{$HPPEMIT ''}

{$I jediapilib.inc}

interface

uses
  JwaWindows;

type
  tagEFaultRepRetVal = (
    frrvOk,
    frrvOkManifest,
    frrvOkQueued,
    frrvErr,
    frrvErrNoDW,
    frrvErrTimeout,
    frrvLaunchDebugger,
    frrvOkHeadless);
  {$EXTERNALSYM tagEFaultRepRetVal}
  EFaultRepRetVal = tagEFaultRepRetVal;
  {$EXTERNALSYM EFaultRepRetVal}

function ReportFault(pep: LPEXCEPTION_POINTERS; dwOpt: DWORD): EFaultRepRetVal; stdcall;
{$EXTERNALSYM ReportFault}
function AddERExcludedApplicationA(szApplication: LPCSTR): BOOL; stdcall;
{$EXTERNALSYM AddERExcludedApplicationA}
function AddERExcludedApplicationW(wszApplication: LPCWSTR): BOOL; stdcall;
{$EXTERNALSYM AddERExcludedApplicationW}

type
  pfn_REPORTFAULT = function(pep: LPEXCEPTION_POINTERS; dwOpt: DWORD): EFaultRepRetVal; stdcall;
  {$EXTERNALSYM pfn_REPORTFAULT}
  pfn_ADDEREXCLUDEDAPPLICATIONA = function(szApplication: LPCSTR): BOOL; stdcall;
  {$EXTERNALSYM pfn_ADDEREXCLUDEDAPPLICATIONA}
  pfn_ADDEREXCLUDEDAPPLICATIONW = function(wszApplication: LPCWSTR): BOOL; stdcall;
  {$EXTERNALSYM pfn_ADDEREXCLUDEDAPPLICATIONW}

function AddERExcludedApplication(wszApplication: LPCTSTR): BOOL; stdcall;
{$EXTERNALSYM AddERExcludedApplication}
type
  pfn_ADDEREXCLUDEDAPPLICATION = pfn_ADDEREXCLUDEDAPPLICATIONW;
  {$EXTERNALSYM pfn_ADDEREXCLUDEDAPPLICATION}

implementation

uses
  JwaWinDLLNames;

{$IFDEF DYNAMIC_LINK}

var
  _ReportFault: Pointer;

function ReportFault;
begin
  GetProcedureAddress(_ReportFault, faultreplib, 'ReportFault');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_ReportFault]
  end;
end;

var
  _AddERExcludedApplicationA: Pointer;

function AddERExcludedApplicationA;
begin
  GetProcedureAddress(_AddERExcludedApplicationA, faultreplib, 'AddERExcludedApplicationA');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AddERExcludedApplicationA]
  end;
end;

var
  _AddERExcludedApplicationW: Pointer;

function AddERExcludedApplicationW;
begin
  GetProcedureAddress(_AddERExcludedApplicationW, faultreplib, 'AddERExcludedApplicationW');
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AddERExcludedApplicationW]
  end;
end;

var
  _AddERExcludedApplication: Pointer;

function AddERExcludedApplication;
begin
  GetProcedureAddress(_AddERExcludedApplication, faultreplib, 'AddERExcludedApplication' + AWSuffix);
  asm
        MOV     ESP, EBP
        POP     EBP
        JMP     [_AddERExcludedApplication]
  end;
end;

{$ELSE}

function ReportFault; external faultreplib name 'ReportFault';
function AddERExcludedApplicationA; external faultreplib name 'AddERExcludedApplicationA';
function AddERExcludedApplicationW; external faultreplib name 'AddERExcludedApplicationW';
function AddERExcludedApplication; external faultreplib name 'AddERExcludedApplication' + AWSuffix;

{$ENDIF DYNAMIC_LINK}

end.
