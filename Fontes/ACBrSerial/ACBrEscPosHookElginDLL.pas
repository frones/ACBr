{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2004 Daniel Simoes de Almeida               }

{ Colaboradores nesse arquivo:                                                 }

{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }

{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }

{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }

{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/gpl-license.php                           }

{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }

{******************************************************************************}

{******************************************************************************
|* Historico
|*
|* 20/07/2018:  Daniel Simões de Almeida
|*   Inicio do desenvolvimento
******************************************************************************}

{*
                 **** Como usar o ACBrDevice.Hook ? ****

- Configure a porta com "USB" ou "DLL"  (sem aspas)

- Você deve programas os eventos abaixo, na sua aplicação
     ACBrDevice.HookAtivar,
     ACBrDevice.HookDesativar,
     ACBrDevice.HookEnviaString
     ACBrDevice.HookLeString

- Instancie a classe abaixo na sua aplicação e use a mesma, nos eventos de Hook...
  Exemplos:

procedure TFrPosPrinterTeste.FormCreate(Sender: TObject);
begin
  FElginUSB := TElginUSBPrinter.Create;
end;

procedure TFrPosPrinterTeste.FormDestroy(Sender: TObject);
begin
  FElginUSB.Free;
end;

procedure TFrPosPrinterTeste.ACBrDeviceHookAtivar(const APort: String;
  Params: String);
begin
  FElginUSB.Open(APort);
end;

procedure TFrPosPrinterTeste.ACBrDeviceHookDesativar(const APort: String);
begin
  FElginUSB.Close;
end;

procedure TFrPosPrinterTeste.ACBrDeviceHookEnviaString(const cmd: AnsiString);
begin
  FElginUSB.WriteData(cmd);
end;

procedure TFrPosPrinterTeste.ACBrDeviceHookLeString(const NumBytes,
  ATimeOut: Integer; var Retorno: AnsiString);
begin
  Retorno := FElginUSB.ReadData;
end;

- Para acessar outras marcas/modelos por comunicação por DLL...
  Crie uma classe semelhante a dessa Unit...

*}

unit ACBrEscPosHookElginDLL;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils;

const
  LIB_NAME = 'HprtPrinter.dll' ;
  //Error Code
  E_SUCCESS = 0;
  E_BAD_HANDLE = -6;

type

  { TElginUSBPrinter }

  TElginUSBPrinter = class
  private
    xPrtPrinterCreatorW : function (printer: PPointer; model: WideString): Integer; cdecl;
    xPrtPortOpenW : function (printer: Pointer; portSetting: WideString): Integer; cdecl;
    xPrtPortClose : function (printer:Pointer): Integer; cdecl;
    xPrtDirectIO : function (printer:Pointer;
                             writeData:PByte; writeNum:integer;
                             readData:PByte; readNum:integer;
                             preadedNum:PInteger): Integer; cdecl;

    procedure UnLoadDLLFunctions;
  private
    FPrinter: Pointer;
    FPort: String;

  public
    procedure LoadDLLFunctions;

    procedure Init;
    procedure Shutdown;

    procedure Open(APort: String);
    procedure Close;
    procedure WriteData(AData: AnsiString);
    function ReadData: AnsiString;
    function Connected: Boolean;
  end;

implementation

uses
  ACBrUtil;

procedure FunctionDetectLib(FuncName : String ;
  var LibPointer : Pointer) ;
var
  sLibName: String;
begin
  if not FunctionDetect( LIB_NAME, FuncName, LibPointer) then
  begin
     LibPointer := NIL ;
     raise Exception.Create( Format(ACBrStr('Erro ao carregar a função: %s na Biblioteca: %s'), [FuncName,sLibName]) ) ;
  end ;
end ;

procedure TElginUSBPrinter.Init;
var
  errorNo: Integer;
  ModelName: AnsiString;
begin
  if (FPrinter <> Nil) then
    Exit;

  LoadDLLFunctions;

  FPrinter := Nil;
  FPort := '';
  ModelName := 'i7';

  errorNo := xPrtPrinterCreatorW(@FPrinter, WideString(ModelName));
  if (errorNo <> E_SUCCESS) then
    raise Exception.Create('Erro ao criar a impressora Elgin: '+IntToStr(errorNo));
end;

procedure TElginUSBPrinter.Shutdown;
begin
  UnLoadDLLFunctions;
  FPrinter := Nil;
end;

procedure TElginUSBPrinter.Open(APort: String);
var
  errorNo: Integer;
begin
  Init;

  if (UpperCase(APort) = 'DLL') then
    APort := 'USB';

  FPort := APort;
  errorNo := xPrtPortOpenW(FPrinter, WideString(FPort));
  if (errorNo <> E_SUCCESS) then
    raise Exception.Create('Erro ao Abrir a impressora Elgin em: '+FPort);
end;

procedure TElginUSBPrinter.Close;
var
  errorNo: Integer;
begin
  if (FPrinter = Nil) then
    Exit;

  errorNo := xPrtPortClose(FPrinter);
  if (errorNo <> E_SUCCESS) then
    raise Exception.Create('Erro ao Fechar a Porta da impressora Elgin');

  FPort := '';
end;

procedure TElginUSBPrinter.WriteData(AData: AnsiString);
begin
  xPrtDirectIO(FPrinter, PByte(AData), Length(AData), Nil, 0, Nil);
end;

function TElginUSBPrinter.ReadData: AnsiString;
var
  ReadNum: Integer;
begin
  Result := Space(1024);
  ReadNum := 0;
  xPrtDirectIO(FPrinter, Nil, 0, PByte(Result), 1024, @ReadNum);
  SetLength(Result, ReadNum);
end;

function TElginUSBPrinter.Connected: Boolean;
begin
  Result := (FPrinter <> Nil);
end;

procedure TElginUSBPrinter.LoadDLLFunctions;
begin
  FunctionDetectLib( 'PrtPrinterCreatorW', @xPrtPrinterCreatorW );
  FunctionDetectLib( 'PrtPortOpenW', @xPrtPortOpenW );
  FunctionDetectLib( 'PrtPortClose', @xPrtPortClose );
  FunctionDetectLib( 'PrtDirectIO', @xPrtDirectIO );
end;

procedure TElginUSBPrinter.UnLoadDLLFunctions;
begin
  UnLoadLibrary( LIB_NAME );

  xPrtPrinterCreatorW := Nil;
  xPrtPortOpenW := Nil;
end;

end.

