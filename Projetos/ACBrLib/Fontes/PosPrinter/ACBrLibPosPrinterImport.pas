{*******************************************************************************}
{ Projeto: Componentes ACBr                                                     }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa-  }
{ mentos de Automação Comercial utilizados no Brasil                            }
{                                                                               }
{ Direitos Autorais Reservados (c) 2018 Daniel Simoes de Almeida                }
{                                                                               }
{ Colaboradores nesse arquivo: Rafael Teno Dias                                 }
{                                                                               }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr       }
{                                                                               }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la  }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela   }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério)  }
{ qualquer versão posterior.                                                    }
{                                                                               }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM    }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU       }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor }
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)               }
{                                                                               }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto }
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,   }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.           }
{ Você também pode obter uma copia da licença em:                               }
{ http://www.opensource.org/licenses/gpl-license.php                            }
{                                                                               }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br }
{        Rua Cel.Aureliano de Camargo, 963 - Tatuí - SP - 18270-170             }
{                                                                               }
{*******************************************************************************}

{$I ACBr.inc}

unit ACBrLibPosPrinterImport;

interface

uses
  Classes, SysUtils, DynLibs,
  ACBrPosPrinter;

const
 {$IfDef MSWINDOWS}
  {$IfDef CPU64}
  CACBrPosPrinterLIBName = 'ACBrPosPrinter64.dll';
  {$Else}
  CACBrPosPrinterLIBName = 'ACBrPosPrinter32.dll';
  {$EndIf}
 {$Else}
  {$IfDef CPU64}
  CACBrPosPrinterLIBName = 'ACBrPosPrinter64.so';
  {$Else}
  CACBrPosPrinterLIBName = 'ACBrPosPrinter32.so';
  {$EndIf}
 {$EndIf}

type
  PACBrPosPrinter = ^TACBrPosPrinter;

  TPOSInicializar = function(const eArqConfig, eChaveCrypt: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  TPOSFinalizar = function: longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  TPOSConfigLer = function(const eArqConfig: PChar): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  TPOSInicializada = function: Boolean;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  TPOSUltimoRetorno = function(const sMensagem: PChar; var esTamanho: longint): longint;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};
  TPOSGetPosPrinter = function: Pointer;
    {$IfDef STDCALL} stdcall{$Else} cdecl{$EndIf};

  { TACBrLibPosPrinter }

  TACBrLibPosPrinter = class
  private
    FArqLib: String;
    FHandle: TLibHandle;
    FPOSInicializar: TPOSInicializar;
    FPOSFinalizar: TPOSFinalizar;
    FPOSConfigLer: TPOSConfigLer;
    FPOSInicializada: TPOSInicializada;
    FPOSUltimoRetorno: TPOSUltimoRetorno;
    FPOSGetPosPrinter: TPOSGetPosPrinter;

    FACBrPosPrinter: TACBrPosPrinter;

    procedure LoadLib;
    procedure UnLoadLib;
    procedure CheckResut(const resultado: longint);

  public
    constructor Create(ArqLib: String; ArqConfig: string = ''; ChaveCrypt: ansistring = '');
    destructor Destroy; override;

    property ACBrPosPrinter: TACBrPosPrinter read FACBrPosPrinter;

    procedure ConfigLer(const eArqConfig: string);
  end;

implementation

uses
  ACBrLibComum, ACBrLibConsts;

constructor TACBrLibPosPrinter.Create(ArqLib: String; ArqConfig: string;
  ChaveCrypt: ansistring);
Var
  ret: longint;
begin
  inherited Create();
  FArqLib := ArqLib;
  LoadLib;

  ret := FPOSInicializar(PChar(ArqConfig), PChar(ChaveCrypt));
  CheckResut(ret);
end;

destructor TACBrLibPosPrinter.Destroy;
Var
  ret: longint;
begin
  // Verificando se a Lib ainda está na memória
  if (FHandle > 0) and FPOSInicializada then
  begin
    ret := FPOSFinalizar;
    CheckResut(ret);
  end;

  UnLoadLib;
  inherited Destroy;
end;

procedure TACBrLibPosPrinter.LoadLib;
var
  APointer: Pointer;
begin
  if not FileExists(FArqLib) then
    Raise EACBrLibException.CreateFmt(SErrArquivoNaoExiste, [FArqLib]);

  FACBrPosPrinter := Nil;
  FHandle := SafeLoadLibrary(FArqLib);
  if (FHandle > 0) then
  begin
    FPOSInicializar := GetProcAddress(FHandle, 'POS_Inicializar');
    FPOSFinalizar := GetProcAddress(FHandle, 'POS_Finalizar');
    FPOSConfigLer := GetProcAddress(FHandle, 'POS_ConfigLer');
    FPOSInicializada := GetProcAddress(FHandle, 'POS_Inicializada');
    FPOSUltimoRetorno := GetProcAddress(FHandle, 'POS_UltimoRetorno');
    FPOSGetPosPrinter := GetProcAddress(FHandle, 'POS_GetPosPrinter');

    APointer := FPOSGetPosPrinter;
    if Assigned(APointer) then
      FACBrPosPrinter := TACBrPosPrinter(APointer)
  end;

  if not Assigned(FACBrPosPrinter) then
    Raise EACBrLibException.CreateFmt(SErrLibNaoCarregada, [FArqLib]);
end;

procedure TACBrLibPosPrinter.UnLoadLib;
begin
  FreeLibrary(FHandle);

  FPOSInicializar := nil;
  FPOSFinalizar := nil;
  FPOSConfigLer := nil;
  FPOSUltimoRetorno := nil;
  FPOSGetPosPrinter := nil;
  FHandle := 0;
  FACBrPosPrinter := Nil;
end;

procedure TACBrLibPosPrinter.ConfigLer(const eArqConfig: string);
Var
  ret: longint;
begin
  ret := FPOSConfigLer(PChar(eArqConfig));
  CheckResut(ret);
end;

procedure TACBrLibPosPrinter.CheckResut(const resultado: longint);
Var
  bufferLen: longint;
  sMensagem: string;
begin
  if resultado >= 0 then Exit;

  bufferLen := 256;
  sMensagem := Space(bufferLen);
  FPOSUltimoRetorno(PChar(sMensagem), bufferLen);

  if bufferLen > 256 then
  begin
    sMensagem := Space(bufferLen);
    FPOSUltimoRetorno(PChar(sMensagem), bufferLen);
  end;

  Raise Exception.Create(Trim(sMensagem));
end;

end.
