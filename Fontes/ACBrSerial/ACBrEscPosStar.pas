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

// Equipamentos compatíveis com esse Modelo:
// - D2 Mini - TecToy/Sunmi,
// - GPOS800 - Gertec

{$I ACBr.inc}

unit ACBrEscPosStar;

interface

uses
  Classes, SysUtils,
  ACBrPosPrinter, ACBrEscPosEpson
  {$IfDef NEXTGEN}
   ,ACBrBase
  {$EndIf};

type

  { TACBrEscPosStar }

  TACBrEscPosStar = class(TACBrEscPosEpson)
  public
    constructor Create(AOwner: TACBrPosPrinter);
    function ComandoGaveta(NumGaveta: Integer = 1): AnsiString; override;
    function ComandoPaginaCodigo(APagCodigo: TACBrPosPaginaCodigo): AnsiString; override;
    function ComandoQrCode(const ACodigo: AnsiString): AnsiString; override;
    function ComandoImprimirImagemRasterStr(const RasterStr: AnsiString; AWidth: Integer;
      AHeight: Integer): AnsiString; override;
    procedure LerStatus(var AStatus: TACBrPosPrinterStatus); override;
    function LerInfo: String; override;
  end;

implementation

uses
  ACBrConsts,
  ACBrUtil.Strings, ACBrUtil.Math;

{ TACBrEscPosStar }

constructor TACBrEscPosStar.Create(AOwner: TACBrPosPrinter);
begin
  inherited Create(AOwner);

  fpModeloStr := 'EscPosStar';

  with Cmd  do
  begin
    Beep := ESC + GS + BELL + #1 + #2 + #5;
  end;
end;

function TACBrEscPosStar.ComandoPaginaCodigo(APagCodigo: TACBrPosPaginaCodigo): AnsiString;
//var
//  CmdPag: Integer;
begin
  Result := FS +'.' +   // Cancel Chinese character mode
            inherited ComandoPaginaCodigo(APagCodigo);
  {
  //Nota: pc850, não funcinou corretamente no G800.
  case APagCodigo of
    pcUTF8: CmdPag := 0;
    pc437: CmdPag := 1;
    pc852: CmdPag := 5;
    pc860: CmdPag := 6;
    pc1252: CmdPag := 32;
  else
    CmdPag := -1;
  end;

  if (CmdPag < 0) then
    Result := inherited ComandoPaginaCodigo(APagCodigo)
  else
    Result := FS +'.' +   // Cancel Chinese character mode
              ESC + GS + 't' + AnsiChr( CmdPag );
  }
end;

function TACBrEscPosStar.ComandoQrCode(const ACodigo: AnsiString): AnsiString;
begin
  Result := inherited ComandoQrCode(ACodigo);

  // Comando padrão da Star, não funcinou corretamente no G800.
  //with fpPosPrinter.ConfigQRCode do
  //begin
  //  Result := ESC + GS + 'yS0'+ AnsiChr(Tipo) +
  //            ESC + GS + 'yS1'+ AnsiChr(ErrorLevel) +
  //            ESC + GS + 'yS2'+ AnsiChr(min(LarguraModulo,8)) +
  //            ESC + GS + 'yD10'+ IntToLEStr(length(ACodigo)) + ACodigo +  // Codifica
  //            ESC + GS + 'yP';  // Imprime
  //end;
end;

function TACBrEscPosStar.ComandoGaveta(NumGaveta: Integer): AnsiString;
var
  CharGav: AnsiChar;
begin
  with fpPosPrinter.ConfigGaveta do
  begin
    if SinalInvertido then
      CharGav := #1
    else
      CharGav := #0;

    Result := DLE + DC4 + #1 + CharGav + AnsiChar(Trunc(TempoOFF/31));
  end;
end;

function TACBrEscPosStar.ComandoImprimirImagemRasterStr(
  const RasterStr: AnsiString; AWidth: Integer; AHeight: Integer): AnsiString;
begin
  // Gerando RasterStr, sem LF, a cada fatia
  Result := ComandoImprimirImagemColumnStr(fpPosPrinter, RasterStr, AWidth, AHeight, False)
end;

procedure TACBrEscPosStar.LerStatus(var AStatus: TACBrPosPrinterStatus);
var
  b: Byte;
  c: AnsiString;
begin
  try
    fpPosPrinter.Ativo := True;

    c := fpPosPrinter.TxRx( GS + 'r' + #1 );
    if (Length(c) > 0) then
    begin
      b := Ord(c[1]);
      if TestBit(b, 0) or TestBit(b, 1) then
        AStatus := AStatus + [stPoucoPapel];
      if TestBit(b, 2) or TestBit(b, 3) then
        AStatus := AStatus + [stSemPapel];
    end;

    c := fpPosPrinter.TxRx( GS + 'r' + #2 );
    if (Length(c) > 0) then
    begin
      b := Ord(c[1]);
      if TestBit(b, 0) then
        AStatus := AStatus + [stGavetaAberta];
    end;
  except
    AStatus := AStatus + [stErroLeitura];
  end;
end;

function TACBrEscPosStar.LerInfo: String;
var
  Ret: AnsiString;
  b: Byte;
begin
  Result := '';
  Info.Clear;

  // Lendo a versão do Firmware
  Ret := fpPosPrinter.TxRx( GS + 'IA', 0, 0, True );
  if (Ret = '') then   // Nem todas impressoras suportam leitura de Info
    Exit;
  AddInfo(cKeyFirmware, Ret);

  // Lendo o Fabricante
  Ret := fpPosPrinter.TxRx( GS + 'IB', 0, 0, True );
  AddInfo(cKeyFabricante, Ret);

  // Lendo o Modelo
  Ret := fpPosPrinter.TxRx( GS + 'IC', 0, 0, True );
  AddInfo(cKeyModelo, Ret);

  // Lendo o Número Serial
  Ret := fpPosPrinter.TxRx( GS + 'ID', 0, 0, True );
  AddInfo(cKeySerial, Ret);

  // Lendo Bit de presença de Guilhotina
  Ret := fpPosPrinter.TxRx( GS + 'I2', 1, 0, False );
  if Length(Ret) > 0 then
  begin
    b := Ord(Ret[1]);
    AddInfo(cKeyGuilhotina, TestBit(b, 1) );
    AddInfo(cKeyCheque, TestBit(b, 3) );
    AddInfo(cKeyMICR, TestBit(b, 3) );
  end;

  Result := Info.Text;
end;

end.

