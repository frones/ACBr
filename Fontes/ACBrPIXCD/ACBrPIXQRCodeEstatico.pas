{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
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

(*

  Documentação:
  https://github.com/bacen/pix-api

*)

{$I ACBr.inc}

unit ACBrPIXQRCodeEstatico;

interface

uses
  Classes, SysUtils,
  ACBrPIXBase;

resourcestring
  sErroChaveInvalida = 'Chave Inválida: %s';
  sErroMCCForaDaFaixa = 'MCC fora da Faixa, 0100-9999';
  sErroPSSForaDaFaixa = 'Código ISPB fora da Faixa, 0-99999999';

const
  cMCCMinimo = 100;
  cMCCMaximo = 9999;
  cPSSMaximo = 99999999;

type

  { TACBrPIXQRCodeEstatico }

  TACBrPIXQRCodeEstatico = class
  private
    fChavePix: String;
    fCidadeRecebedor: String;
    finfoAdicional: String;
    fmcc: Integer;
    fNomeRecebedor: String;
    fpss: Integer;
    fTipoChavePix: TACBrPIXTipoChave;
    fTxId: String;
    fValor: Currency;
    function GetQRCode: String;
    procedure SetChavePix(AValue: String);
    procedure SetCidadeRecebedor(AValue: String);
    procedure SetinfoAdicional(AValue: String);
    procedure SetMcc(AValue: Integer);
    procedure SetNomeRecebedor(AValue: String);
    procedure SetPss(AValue: Integer);
    procedure SetTxId(AValue: String);

  protected
    function GetMerchantAccountInformation: String;
    function GetAdditionalDataField: String;
    function GetCRC(const AQRCode: String): String;

  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrPIXQRCodeEstatico);

    property NomeRecebedor: String read fNomeRecebedor write SetNomeRecebedor;
    property CidadeRecebedor: String read fCidadeRecebedor write SetCidadeRecebedor;
    property ChavePix: String read fChavePix write SetChavePix;
    property TipoChavePix: TACBrPIXTipoChave read fTipoChavePix;
    property Valor: Currency read fValor write fValor;
    property infoAdicional: String read finfoAdicional write SetinfoAdicional;
    property TxId: String read fTxId write SetTxId;  // Indentificador da Transação
    property pss: Integer read fpss write SetPss;    // Prestador de serviço de saque
    property mcc: Integer read fmcc write SetMcc;    // https://classification.codes/classifications/industry/mcc/

    property QRCode: String read GetQRCode;
  end;

implementation

uses
  ACBrPIXUtil, ACBrUtil;

{ TACBrPIXQRCodeEstatico }

constructor TACBrPIXQRCodeEstatico.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrPIXQRCodeEstatico.Clear;
begin
  fNomeRecebedor := '';
  fCidadeRecebedor := '';
  fChavePix := '';
  fTipoChavePix := tcNenhuma;
  finfoAdicional := '';
  fTxId := '';
  fValor := 0;
  fpss := -1;
  fmcc := 0;
end;

procedure TACBrPIXQRCodeEstatico.Assign(Source: TACBrPIXQRCodeEstatico);
begin
  fNomeRecebedor := Source.NomeRecebedor;
  fCidadeRecebedor := Source.CidadeRecebedor;
  ChavePix := Source.ChavePix;
  finfoAdicional := Source.infoAdicional;
  fTxId := Source.TxId;
  fValor := Source.Valor;
  fpss := Source.pss;
  fmcc := Source.mcc;
end;

function TACBrPIXQRCodeEstatico.GetQRCode: String;
var
  vs: String;
  l: Integer;
begin
  if (fTipoChavePix = tcNenhuma) then
    raise EACBrPixException.CreateFmt(sErroChaveInvalida, [fChavePix]);

  if (fValor > 0) then
    vs := FormatarValorPIX(fValor)
  else
    vs := '';

  Result := FormatarQRCodeId( 0, '01') +                          // Payload Format Indicator
            FormatarQRCodeId(26, GetMerchantAccountInformation) + // Merchant Account Information
            FormatarQRCodeId(52, IntToStrZero(fmcc, 4)) +         // Merchant Category Code
            FormatarQRCodeId(53, IntToStr(cBRCurrency)) +
            FormatarQRCodeId(54, vs) +
            FormatarQRCodeId(58, cBRCountryCode) +
            FormatarQRCodeId(59, NomeRecebedor) +
            FormatarQRCodeId(60, CidadeRecebedor) +
            FormatarQRCodeId(62, GetAdditionalDataField);

  Result := GetCRC(Result);
end;

function TACBrPIXQRCodeEstatico.GetMerchantAccountInformation: String;
var
  spss, id0, id1, id2, id3: String;
  l, lim: Integer;
begin
  if (pss >= 0) then
    spss := IntToStrZero(fpss, 8)
  else
    spss := '';

  id0 := FormatarQRCodeId(0, cGUIPIX);
  id1 := FormatarQRCodeId(1, ChavePix);
  id3 := FormatarQRCodeId(3, spss);

  if (infoAdicional <> '') then
  begin
    l := Length(id0) + Length(id1) + Length(id3);
    lim := cEMVLimit - 4 - l;
    id2 := FormatarQRCodeId(2, copy(infoAdicional, 1, lim));
  end
  else
    id2 := '';

  Result := id0 + id1 + id2 + id3;
end;

function TACBrPIXQRCodeEstatico.GetAdditionalDataField: String;
begin
  Result := FormatarQRCodeId(5, IfEmptyThen(fTxId, cMPMValueNotInformed));
end;

function TACBrPIXQRCodeEstatico.GetCRC(const AQRCode: String): String;
var
  s: String;
begin
  s := AQRCode +
       '6304';
  Result := s + UpperCase(Crc16PIX(s));
end;

procedure TACBrPIXQRCodeEstatico.SetChavePix(AValue: String);
var
  TipoChave: TACBrPIXTipoChave;
begin
  if (fChavePix = AValue) then
    Exit;

  TipoChave := DetectarTipoChave(AValue);
  if (TipoChave = tcNenhuma) then
    raise EACBrPixException.CreateFmt(sErroChaveInvalida, [AValue]);

  fChavePix := Trim(AValue);
  fTipoChavePix := TipoChave;
end;

procedure TACBrPIXQRCodeEstatico.SetMcc(AValue: Integer);
begin
  if (fmcc = AValue) then
    Exit;

  if (AValue <> 0) and (AValue < cMCCMinimo) or (AValue > cMCCMaximo) then
    raise EACBrPixException.Create(sErroMCCForaDaFaixa);

  fmcc := AValue;
end;

procedure TACBrPIXQRCodeEstatico.SetNomeRecebedor(AValue: String);
begin
  if (fNomeRecebedor = AValue) then
    Exit;

  fNomeRecebedor := copy(Trim(AValue),1,25);
end;

procedure TACBrPIXQRCodeEstatico.SetCidadeRecebedor(AValue: String);
begin
  if (fCidadeRecebedor = AValue) then
    Exit;

  fCidadeRecebedor := Trim(AValue);
end;

procedure TACBrPIXQRCodeEstatico.SetinfoAdicional(AValue: String);
begin
  if (finfoAdicional = AValue) then
    Exit;

  finfoAdicional := Trim(AValue);
end;

procedure TACBrPIXQRCodeEstatico.SetPss(AValue: Integer);
begin
  if (fpss = AValue) then
    Exit;

  if (AValue > cPSSMaximo) then
    raise EACBrPixException.Create(sErroPSSForaDaFaixa);

  fpss := AValue;
end;

procedure TACBrPIXQRCodeEstatico.SetTxId(AValue: String);
var
  e: String;
begin
  if (fTxId = AValue) then
    Exit;

  e := ValidarTxId(AValue);
  if (e <> '') then
    raise EACBrPixException.Create(e);

  fTxId := AValue;
end;

end.

