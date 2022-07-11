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
  Result := ComandoImprimirImagemColumnStr(fpPosPrinter, RasterStr, AWidth, AHeight, True)
end;

end.

