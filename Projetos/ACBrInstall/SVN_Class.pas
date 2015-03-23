{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2009   Isaque Pinheiro                      }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 29/03/2012: Isaque Pinheiro / Régys Borges da Silveira
|*  - Criação e distribuição da Primeira Versao
*******************************************************************************}
unit SVN_Class;

interface

uses
  SysUtils, Windows, Dialogs, Menus, Registry, ShellApi,
  Classes, Controls, Graphics, ImgList, ExtCtrls, ActnList;

var
  TSVNTortoisePath: string;
  TSVNCollabNetPath: string;
  TTortoiseMergePath: string;

type
  TSVN_Class = class
  private
    class procedure SVNCollabNetExec(Params: String); static;
    class procedure SVNTortoiseExec( Params: String ); static;
  public
    class constructor Create;

    // executar programa externo e aguardar o termino
    class function  WinExecAndWait32(CmdLine: AnsiString; Visibility: Integer = SW_SHOW): DWORD; static;

    // Métodos que utilizam o tortoise
    class function IsTortoiseInstalado: Boolean; static;
    class procedure SVNTortoise_CheckOut(const AUrl, APath: String;
      const AFecharAutomaticamente: Boolean); static;
    class procedure SVNTortoise_Update(const APath: String;
      const AFecharAutomaticamente: Boolean); static;

    // métodos que utilizam o CollabNet que é o mesmo utilizado pelo delphi XE2
    class function IsCollabNetInstalado: Boolean; static;
    class procedure SVNCollabNet_Checkout(const AUrl, APath: String); static;
    class procedure SVNCollabNet_Update(const AUrl, APath: String); static;

  end;

implementation

//******************************************************************************
//
//  Executar um aplicativo e aguardar o retorno do mesmo
//
//******************************************************************************
class function TSVN_Class.WinExecAndWait32(CmdLine: AnsiString; Visibility: Integer): DWORD;
var
  zAppName: array[0..512] of Char;
  zCurDir: array[0..255] of Char;
  WorkDir: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  StrPCopy(zAppName, String(CmdLine));
  GetDir(0, WorkDir);
  StrPCopy(zCurDir, WorkDir);
  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb := Sizeof(StartupInfo);

  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if not CreateProcess(nil,
           zAppName,               // pointer to command line string }
           nil,                    // pointer to process security attributes }
           nil,                    // pointer to thread security attributes }
           false,                  // handle inheritance flag }
           CREATE_NEW_CONSOLE or   // creation flags }
           NORMAL_PRIORITY_CLASS,
           nil,                    // pointer to new environment block }
           nil,                    // pointer to current directory name }
           StartupInfo,            // pointer to STARTUPINFO }
           ProcessInfo) then
  begin
    Result := 9; { pointer to PROCESS_INF }
  end
  else
  begin
    WaitforSingleObject(ProcessInfo.hProcess, INFINITE);

    GetExitCodeProcess(ProcessInfo.hProcess, Result);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end;
end;

//******************************************************************************
//
//  Setar paths dos aplicativos utilizados
//
//******************************************************************************

class constructor TSVN_Class.Create;
var
  Reg: TRegistry;
const
  KEY_WOW64_64KEY = $0100;
  KEY_WOW64_32KEY = $0200;

  function SetPathTortoise: Boolean;
  begin
    Result := Reg.OpenKeyReadOnly( '\SOFTWARE\TortoiseSVN' ) ;
    if Result then
    begin
      TSVNTortoisePath   := Reg.ReadString( 'ProcPath' );
      TTortoiseMergePath := Reg.ReadString( 'TMergePath' );
    end
    else
    begin
      TSVNTortoisePath   := '';
      TTortoiseMergePath := '';
    end;
  end;

  function SetPathCollabNet: Boolean;
  var
    CollabNetReg: String;
  begin
    Result := Reg.OpenKeyReadOnly( '\SOFTWARE\CollabNet\Subversion' ) ;
    if Result then
    begin
      CollabNetReg := Reg.ReadString( 'Client Version' );
      CollabNetReg := '\SOFTWARE\CollabNet\Subversion\' + CollabNetReg + '\Client';
      if Reg.OpenKeyReadOnly( CollabNetReg ) then
        TSVNCollabNetPath := Reg.ReadString( 'Install Location' ) + '\svn.exe';
    end
    else
    begin
      TSVNCollabNetPath := '';
    end;
  end;
begin
  Reg := TRegistry.Create;
  try
    Reg.RootKey := HKEY_LOCAL_MACHINE;

    // TORTOISE
    if not SetPathTortoise then
    begin
      //try 64 bit registry
      Reg.Access := Reg.Access or KEY_WOW64_64KEY;
      if not SetPathTortoise then
      begin
        //try WOW64 bit registry
        Reg.Access := Reg.Access or KEY_WOW64_32KEY;
        SetPathTortoise;
      end;
    end;

    // COLLABNET
    if not SetPathCollabNet then
    begin
      //try 64 bit registry
      Reg.Access := Reg.Access or KEY_WOW64_64KEY;
      if not SetPathCollabNet then
      begin
        //try WOW64 bit registry
        Reg.Access := Reg.Access or KEY_WOW64_32KEY;
        SetPathCollabNet;
      end;
    end;
  finally
    Reg.CloseKey;
    Reg.Free;
  end;
end;

//******************************************************************************
//
//  TORTOISE
//
//******************************************************************************

class procedure TSVN_Class.SVNTortoiseExec( Params: string );
var
  CmdLine: AnsiString;
begin
  CmdLine := AnsiString(TSVNTortoisePath + ' ' + Params );
  WinExecAndWait32(CmdLine, SW_SHOW);
end;

class function TSVN_Class.IsTortoiseInstalado: Boolean;
begin
  Result := FileExists(TSVNTortoisePath);
end;

class procedure TSVN_Class.SVNTortoise_CheckOut(const AUrl, APath: String;
  const AFecharAutomaticamente: Boolean);
var
  Comando: String;
begin
  Comando := '/command:checkout' +
               ' /blockpathadjustments ' +
               ' /path:' + AnsiQuotedStr( APath, '"' ) +
               ' /url:'  + AnsiQuotedStr( AUrl, '"' );

  if AFecharAutomaticamente then
    Comando := Comando + ' /closeonend:3'
  else
    Comando := Comando + ' /closeonend:0';

  TSVN_Class.SVNTortoiseExec( Comando );
end;

class procedure TSVN_Class.SVNTortoise_Update(const APath: String;
  const AFecharAutomaticamente: Boolean);
var
  Comando: String;
begin
  Comando := '/command:update' +
               ' /notempfile' +
               ' /path:' + AnsiQuotedStr( APath, '"' );

  if AFecharAutomaticamente then
    Comando := Comando + ' /closeonend:3'
  else
    Comando := Comando + ' /closeonend:0';

  TSVN_Class.SVNTortoiseExec( Comando );
end;

//******************************************************************************
//
//  TORTOISE
//
//******************************************************************************

class procedure TSVN_Class.SVNCollabNetExec( Params: string );
var
  CmdLine: AnsiString;
begin
  CmdLine := AnsiString(TSVNCollabNetPath + ' ' + Params );
  TSVN_Class.WinExecAndWait32(CmdLine, SW_SHOW);
end;

class function TSVN_Class.IsCollabNetInstalado: Boolean;
begin
  Result := FileExists(TSVNCollabNetPath);
end;

class procedure TSVN_Class.SVNCollabNet_Checkout(const AUrl, APath: String);
begin
  TSVN_Class.SVNCollabNetExec(
    Format('co %s %s', [
      AnsiQuotedStr( AUrl, '"' ),
      AnsiQuotedStr( APath, '"' )
    ])
  );
end;

class procedure TSVN_Class.SVNCollabNet_Update(const AUrl, APath: String);
begin
  //TSVN_Class.SVNCollabNetExec( 'info ' + AnsiQuotedStr( AUrl, '"' ) );
  TSVN_Class.SVNCollabNetExec(
    'up ' + AnsiQuotedStr( APath, '"' )
  );
end;

end.
