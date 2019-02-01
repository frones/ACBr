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
|* 29/01/2019:  Daniel Simões de Almeida
|*   Inicio do desenvolvimento
******************************************************************************}

unit ACBrEscPosHook;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils;

resourcestring
  CERROR_NOT_INIT = '%s não foi inicializada';
  CERROR_NOT_CONN = 'Não conectado a impressora %s';
  CERROR_LOAD_LIB = 'Erro ao carregar a função: %s na Biblioteca: %s';
  CERROR_OPEN = 'Erro ao Abrir porta %s da impressora %s';
  CERROR_CLOSE = 'Erro ao Fechar a porta %s da impressora %s';
  CERROR_WRITE = 'Erro ao Enviar Dados para porta %s da impressora %s';

type

  { EACBrPosPrinterHook }

  EACBrPosPrinterHook = class(Exception)
  public
    constructor Create(const msg : string);
    constructor CreateFmt(const msg : string; const args : array of const);
  end;

  { TACBrPosPrinterHook }

  TACBrPosPrinterHook = class
  protected
    fpInitialized: Boolean;
    fpConnected: Boolean;
    fpPrinterName: String;
    fpLibName: String;
    fpPort: String;

    function GetInitialized: Boolean; virtual;
    function GetConnected: Boolean; virtual;
    procedure CheckInitialized; virtual;
    procedure CheckConnected; virtual;

    procedure LoadLibFunctions; virtual;
    procedure UnLoadLibFunctions; virtual;

    procedure FunctionDetectLib(const FuncName: String; var LibPointer: Pointer);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Init; virtual;
    procedure Shutdown; virtual;

    procedure Open(const APort: String); virtual;
    procedure Close; virtual;
    procedure WriteData(const AData: AnsiString); virtual;
    function ReadData(const NumBytes, ATimeOut: Integer): AnsiString; virtual;

    property Initialized: Boolean read GetInitialized;
    property Connected: Boolean read GetConnected;

    class function CanInitilize: Boolean; virtual;
    class function Brand: String; virtual;
  end;

  TACBrPosPrinterHookClass = class of TACBrPosPrinterHook;

var
  HookList: array of TACBrPosPrinterHookClass;

procedure RegisterHook(AHookClass: TACBrPosPrinterHookClass);

implementation

uses
  ACBrUtil;

procedure RegisterHook(AHookClass: TACBrPosPrinterHookClass);
var
  LenList, i: Integer;
begin
  LenList := Length(HookList);
  For i := 0 to LenList-1 do
    if (HookList[i] = AHookClass) then
      Exit;

  SetLength(HookList, LenList + 1);
  HookList[LenList] := AHookClass;
end;

{ EACBrPosPrinterHook }

constructor EACBrPosPrinterHook.Create(const msg: string);
begin
  inherited Create(ACBrStr(msg));
end;

constructor EACBrPosPrinterHook.CreateFmt(const msg: string;
  const args: array of const);
begin
  inherited CreateFmt(ACBrStr(msg), args);
end;

{ TACBrPosPrinterHook }

constructor TACBrPosPrinterHook.Create;
begin
  inherited Create;

  // Setar valores corretos no CReate dos filhos
  fpPrinterName := 'NONE';
  fpLibName := '';

  fpPort := '';
  fpInitialized := False;
  fpConnected := False;
end;

destructor TACBrPosPrinterHook.Destroy;
begin
  Shutdown;
  inherited Destroy;
end;

procedure TACBrPosPrinterHook.Init;
begin
  if Initialized then
    Exit;

  LoadLibFunctions;

  fpInitialized := True;
  fpConnected := False;
  fpPort := '';
end;

function TACBrPosPrinterHook.GetInitialized: Boolean;
begin
  Result := fpInitialized;
end;

procedure TACBrPosPrinterHook.Open(const APort: String);
begin
  if Connected then
    Exit;

  if (pos(uppercase(copy(APort,1,3)),'USB|DLL') > 0) then
    fpPort := 'USB'
  else
    fpPort := APort;

  CheckInitialized;
  fpConnected := True;
end;

function TACBrPosPrinterHook.GetConnected: Boolean;
begin
  Result := fpConnected;
end;

procedure TACBrPosPrinterHook.CheckInitialized;
begin
  if not Initialized then
    raise EACBrPosPrinterHook.CreateFmt(CERROR_NOT_INIT, [ClassName]);
end;

procedure TACBrPosPrinterHook.CheckConnected;
begin
  if not Connected then
    raise EACBrPosPrinterHook.CreateFmt(CERROR_NOT_CONN, [fpPrinterName]);
end;

procedure TACBrPosPrinterHook.LoadLibFunctions;
begin
  // sobrescrever
end;

procedure TACBrPosPrinterHook.UnLoadLibFunctions;
begin
  if not Initialized then
    Exit;

  UnLoadLibrary( fpLibName );

  // sobrescrever, setando Nil para os métodos
end;

procedure TACBrPosPrinterHook.Close;
begin
  if not Connected then
    Exit;

  fpPort := '';
  fpConnected := False;
end;

procedure TACBrPosPrinterHook.WriteData(const AData: AnsiString);
begin
  CheckConnected;
end;

function TACBrPosPrinterHook.ReadData(const NumBytes, ATimeOut: Integer): AnsiString;
begin
  CheckConnected;
end;

class function TACBrPosPrinterHook.CanInitilize: Boolean;
begin
  // sobrescrever
  Result := True;
end;

class function TACBrPosPrinterHook.Brand: String;
begin
  // sobrescrever
  Result := 'NONE';
end;

procedure TACBrPosPrinterHook.Shutdown;
begin
  if not Initialized then
    Exit;

  Close;
  UnLoadLibFunctions;
  fpInitialized := False;
end;

procedure TACBrPosPrinterHook.FunctionDetectLib(const FuncName : String ;
  var LibPointer : Pointer) ;
begin
  if not FunctionDetect(fpLibName, FuncName, LibPointer) then
  begin
     LibPointer := NIL ;
     raise EACBrPosPrinterHook.CreateFmt(CERROR_LOAD_LIB, [FuncName, fpLibName]) ;
  end ;
end ;


end.

