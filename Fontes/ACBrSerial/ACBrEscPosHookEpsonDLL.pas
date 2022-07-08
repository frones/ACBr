{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

unit ACBrEscPosHookEpsonDLL;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils,
  ACBrEscPosHook;

const
  {$IfDef MSWINDOWS}
   EPSON_LIB_NAME = 'InterfaceEpsonNF.dll' ;
  {$Else}
   EPSON_LIB_NAME = 'InterfaceEpsonNF.so' ;
  {$EndIf}

  EPSON_NAME='EPSON';

  //Result codes
  FUNC_SUCESSO = 1;
  FUNC_ERRO = 0;

type

  { TEpsonUSBPrinter }

  TEpsonUSBPrinter = class(TACBrPosPrinterHook)
  private
    xEpsonIniciaPorta: function(pszPorta: PAnsiChar): Integer; StdCall;
    xEpsonFechaPorta: function(): Integer; StdCall;
    xEpsonComandoTX: function( pszComando: PAnsiChar; dwTamanho: Integer): Integer;StdCall;
    xEpsonLeStatus: function(): Integer; StdCall;
    xEpsonLeStatusGaveta: function(): Integer; StdCall;
    xEpsonLeStatusSlip: function(pszFlags: PAnsiChar): Integer; StdCall;
    xEpsonLeModelo: function (pszModelo: PAnsiChar): Integer; StdCall;
    xEpsonLeNumeroSerie: function(pszSerie: PAnsiChar): Integer; StdCall;
    xEpsonLeMICR: function(pszCodigo: PAnsiChar): Integer;StdCall;


    FStatusToRead: Integer;
    FInfoToRead: Char;
    FModelo: String;

    function LeModelo(): String;
  protected
    procedure LoadLibFunctions; override;
    procedure UnLoadLibFunctions; override;
  public
    constructor Create; override;
    procedure Init; override;

    procedure Open(const APort: String); override;
    procedure Close; override;
    procedure WriteData(const AData: AnsiString); override;
    function ReadData(const NumBytes, ATimeOut: Integer): AnsiString; override;

    class function CanInitilize: Boolean; override;
    class function Brand: String; override;
  end;

implementation

uses
  strutils,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.Math,
  ACBrConsts;

constructor TEpsonUSBPrinter.Create;
begin
  inherited Create;

  fpPrinterName := EPSON_NAME;
  fpLibName := EPSON_LIB_NAME;
end;

procedure TEpsonUSBPrinter.Init;
begin
  if Initialized then
    Exit;

  inherited Init;

  FStatusToRead := 0;
  FInfoToRead := ' ';
  FModelo := '';
end;

procedure TEpsonUSBPrinter.Open(const APort: String);
var
  errorNo: Integer;
begin
  if Connected then
    Exit;

  inherited Open(APort);

  try
    errorNo := xEpsonIniciaPorta(PAnsiChar(AnsiString(fpPort)));
    if (errorNo <> FUNC_SUCESSO) then
      raise Exception.CreateFmt(CERROR_OPEN, [fpPort, fpPrinterName]);
  except
    fpConnected := False;
    fpPort := '';
    raise;
  end;
end;

procedure TEpsonUSBPrinter.Close;
var
  errorNo: Integer;
begin
  if not Connected then
    Exit;

  errorNo := xEpsonFechaPorta();
  if (errorNo <> FUNC_SUCESSO) then
    raise Exception.CreateFmt(CERROR_CLOSE, [fpPort, fpPrinterName]);

  inherited Close;
end;

procedure TEpsonUSBPrinter.WriteData(const AData: AnsiString);
var
  errorNo: Integer;
begin
  CheckConnected;
  if (LeftStr(AData,2) = DLE + EOT) then   // Leitura de Status
  begin
    FStatusToRead := ord(PadLeft(copy(AData,3,1),1 ,#0)[1]);
    Exit;
  end;

  if (LeftStr(AData,2) = GS + 'I') then  // Leitura de Informações
  begin
    FInfoToRead := PadLeft(copy(AData,3,1),1)[1];
    Exit;
  end;

  if (LeftStr(AData,2) = FS + 'b') then  // Leitura de CMC7
  begin
    FInfoToRead := 'b';
    Exit;
  end;

  errorNo := xEpsonComandoTX(PAnsiChar(AData), Length(AData));
  if (errorNo <> FUNC_SUCESSO) then
    raise Exception.CreateFmt(CERROR_WRITE, [fpPort, fpPrinterName]);
end;

function TEpsonUSBPrinter.ReadData(const NumBytes, ATimeOut: Integer
  ): AnsiString;
var
  ret: Integer;
  AByte: Integer;
  Buffer: AnsiString;
begin
  CheckConnected;
  Result := '';  // TODO, achar método equivalente, na DLL, para LER dados da USB

  if (FStatusToRead > 0) then
  begin
    AByte := 0;
    case FStatusToRead of
      1:
        begin
          ret := xEpsonLeStatusGaveta();
          if (ret = 2) then
            SetBit(AByte, 2);
        end;

      2:
        begin
          ret := xEpsonLeStatus();
          case ret of
            0: SetBit(AByte, 6);
            9: SetBit(AByte, 2);
           32: SetBit(AByte, 5);
          end;
        end;

      4:
        begin
          ret := xEpsonLeStatus();
          case ret of
            5: SetBit(AByte, 2);
           32: SetBit(AByte, 6);
          end;
        end;

      5:
        begin
          Buffer := StringOfChar(' ', 4);
          ret := xEpsonLeStatusSlip( PAnsiChar(Buffer) );
          if (ret = FUNC_SUCESSO) then
            Result := Trim(Buffer);

          AByte := 6;  // Bits 1 e 2 Ligados
          if (copy(Buffer,1,1) = '1') then
            SetBit(AByte, 3);
          if not (copy(Buffer,2,1) = '1') then
            SetBit(AByte, 5);
          if not (copy(Buffer,3,1) = '1') then
            SetBit(AByte, 6);
        end;
    end;

    FStatusToRead := 0;
    Result := chr(AByte);
  end

  else if (FInfoToRead <> ' ') then
  begin
    case FInfoToRead of
      'A':
        Result := 'A';  // Firmware - Não há metodo compatível na DLL

      'B':
        Result := '_EPSON';

      'C':
        Result := LeModelo;

      'D':
        begin
          Buffer := StringOfChar(' ', 32);
          ret := xEpsonLeNumeroSerie( PAnsiChar(Buffer) );
          if (ret = FUNC_SUCESSO) then
            Result := Trim(Buffer);
        end;

      '2':
        begin
          if (pos('H6000', LeModelo) > 0) then
            Result := AnsiChr( 2 + 8 )  // Bit 1, 3 e 6 ligado: Guilhotina, Slip, MICR
          else
            Result := AnsiChr( 2 );     // Bit 1 ligado: Guilhotina
        end;

      //  Infelizmente não funciona, pois método "LeMICR", da DLL da Epson, fica aguardando novamente a inserção do cheque
      //'b':
      //  begin
      //    Buffer := StringOfChar(' ', 40);
      //    ret := xEpsonLeMICR( PAnsiChar(Buffer) );
      //    if (ret = FUNC_SUCESSO) then
      //      Result := Trim(Buffer);
      //  end;
    end;

    FInfoToRead := ' ';
  end;
end;

class function TEpsonUSBPrinter.CanInitilize: Boolean;
begin
  Result := FileExists(ApplicationPath + EPSON_LIB_NAME);
end;

class function TEpsonUSBPrinter.Brand: String;
begin
  Result := EPSON_NAME;
end;

function TEpsonUSBPrinter.LeModelo: String;
var
  Buffer: AnsiString;
  ret: Integer;
begin
  if (FModelo = '') then
  begin
    Buffer := StringOfChar(' ', 32);
    ret := xEpsonLeModelo( PAnsiChar(Buffer) );
    if (ret = FUNC_SUCESSO) then
      FModelo := Trim(Buffer);
  end;

  Result := FModelo;
end;

procedure TEpsonUSBPrinter.LoadLibFunctions;
begin
  if Initialized then
    Exit;

  FunctionDetectLib( 'IniciaPorta', @xEpsonIniciaPorta );
  FunctionDetectLib( 'FechaPorta', @xEpsonFechaPorta );
  FunctionDetectLib( 'ComandoTX', @xEpsonComandoTX );
  FunctionDetectLib( 'Le_Status', @xEpsonLeStatus );
  FunctionDetectLib( 'Le_Status_Slip', @xEpsonLeStatusSlip );
  FunctionDetectLib( 'Le_Status_Gaveta', @xEpsonLeStatusGaveta );
  FunctionDetectLib( 'LeModelo', @xEpsonLeModelo );
  FunctionDetectLib( 'LeNumeroSerie', @xEpsonLeNumeroSerie );
  FunctionDetectLib( 'LeMICR', @xEpsonLeMICR );
end;

procedure TEpsonUSBPrinter.UnLoadLibFunctions;
begin
  inherited UnLoadLibFunctions;

  xEpsonIniciaPorta := Nil;
  xEpsonFechaPorta := NIl;
  xEpsonComandoTX := Nil;
  xEpsonLeStatus := Nil;
  xEpsonLeStatusSlip := Nil;
  xEpsonLeStatusGaveta := Nil;
  xEpsonLeModelo := Nil;
  xEpsonLeNumeroSerie := Nil;
  xEpsonLeMICR := Nil;
end;

initialization
  RegisterHook(TEpsonUSBPrinter);

end.

