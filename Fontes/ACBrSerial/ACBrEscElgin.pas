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
|* 25/08/2013:  Daniel Simões de Almeida
|*   Inicio do desenvolvimento
******************************************************************************}

{$I ACBr.inc}

unit ACBrEscElgin;

interface

uses
  Classes, SysUtils,
  ACBrPosPrinter, ACBrEscBematech, ACBrConsts;

type

  { TACBrEscElgin }

  TACBrEscElgin = class(TACBrEscBematech)
  private
  public
    constructor Create(AOwner: TACBrPosPrinter);

    function ComandoQrCode(const ACodigo: AnsiString): AnsiString; override;
    procedure LerStatus(var AStatus: TACBrPosPrinterStatus); override;
  end;


implementation

Uses
  ACBrUtil;

{ TACBrEscElgin }

constructor TACBrEscElgin.Create(AOwner: TACBrPosPrinter);
begin
  inherited Create(AOwner);

  fpModeloStr := 'EscElgin';
end;

procedure TACBrEscElgin.LerStatus(var AStatus: TACBrPosPrinterStatus);
var
  B: Byte;
  Ret: AnsiString;
begin
  try
    Ret := fpPosPrinter.TxRx(ENQ, 1, 500);
    B := Ord(Ret[1]);

    if not TestBit(B, 0) then
      AStatus := AStatus + [stOffLine];

    if TestBit(B, 1) then
      AStatus := AStatus + [stSemPapel];

    if TestBit(B, 2) then
      AStatus := AStatus + [stGavetaAberta];

    if not TestBit(B, 3) then
      AStatus := AStatus + [stTampaAberta];

    if TestBit(B, 4) then
      AStatus := AStatus + [stPoucoPapel];
  except
    AStatus := AStatus + [stErroLeitura];
  end;
end;

function TACBrEscElgin.ComandoQrCode(const ACodigo: AnsiString): AnsiString;
var
  Quality: AnsiChar;
begin
  with fpPosPrinter.ConfigQRCode do
  begin
    Result := GS + 'o' + #0 +             // Set parameters of QRCODE barcode
              AnsiChr(LarguraModulo) +    // Basic element width
              #0 +                        // Language mode: 0:Chinese 1:Japanese
              AnsiChr(Tipo) ;             // Symbol type: 1:Original type 2:Enhanced type(Recommended)

    case ErrorLevel of
      1: Quality := 'M';
      2: Quality := 'Q';
      3: Quality := 'H';
    else
      Quality := 'L';
    end;

    Result := Result +
              GS  + 'k' + // Bar Code
              #11 +       // Type = QRCode. Number of Characters: 4-928
              Quality +
              'A,' +      // Data input mode Range: A-automatic (Recommended). M-manual
              ACodigo +
              #0;
  end;
end;

end.

