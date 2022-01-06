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
  sErroMCCForaDaFaixa = 'MCC fora da Faixa, 0100-9999';
  sErroPayloadIndicator = 'Indicador de Formato inválido';
  sErroCRCInvalido = 'CRC inválido';
  sErroSintaxe = 'Erro na sintaxe';
  sErroCurrency = 'Moeda %s não é aceita';
  sErroCountryCode = 'País deve ser BR';
  sErroGUIInvalido = 'GUI deve ser: '+cGUIPIX;

const
  cMCCMinimo = 100;
  cMCCMaximo = 9999;
  cCrcIdAndSize = '6304';

type

  { TACBrPIXQRCodeEstatico }

  TACBrPIXQRCodeEstatico = class
  private
    fCEPRecebedor: String;
    fChavePix: String;
    fCidadeRecebedor: String;
    fIgnorarErrosQRCode: Boolean;
    finfoAdicional: String;
    fmcc: Integer;
    fNomeRecebedor: String;
    fpss: Integer;
    fTipoChavePix: TACBrPIXTipoChave;
    fTxId: String;
    fValor: Currency;
    procedure DispararErroQRCode(const e: String);
    function GetQRCode: String;
    procedure SetCEPRecebedor(AValue: String);
    procedure SetChavePix(const AValue: String);
    procedure SetCidadeRecebedor(const AValue: String);
    procedure SetinfoAdicional(const AValue: String);
    procedure SetMcc(AValue: Integer);
    procedure SetNomeRecebedor(const AValue: String);
    procedure SetPss(AValue: Integer);
    procedure SetQRCode(const AValue: String);
    procedure SetTxId(const AValue: String);

  protected
    procedure ExtrairDadosMerchantAccountInformation(const ADados: String);
    procedure ExtrairDadosAdditionalDataField(const ADados: String);
    function GerarMerchantAccountInformation: String;
    function GerarAdditionalDataField: String;
    function AdicionarCRC(const ADados: String): String;

    procedure LerProximoEMV(const ADados: String; var p: Integer;
      out id: String; out TamId: Integer; out ValorId: String);
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrPIXQRCodeEstatico);
    function VerificarCRC(const ADados: String): Boolean;

    property NomeRecebedor: String read fNomeRecebedor write SetNomeRecebedor;
    property CidadeRecebedor: String read fCidadeRecebedor write SetCidadeRecebedor;
    property CEPRecebedor: String read fCEPRecebedor write SetCEPRecebedor;
    property ChavePix: String read fChavePix write SetChavePix;
    property TipoChavePix: TACBrPIXTipoChave read fTipoChavePix;
    property Valor: Currency read fValor write fValor;
    property infoAdicional: String read finfoAdicional write SetinfoAdicional;
    property TxId: String read fTxId write SetTxId;  // Indentificador da Transação
    property pss: Integer read fpss write SetPss;    // Prestador de serviço de saque
    property mcc: Integer read fmcc write SetMcc;    // https://classification.codes/classifications/industry/mcc/

    property QRCode: String read GetQRCode write SetQRCode;
    property IgnorarErrosQRCode: Boolean read fIgnorarErrosQRCode write fIgnorarErrosQRCode;
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
  fCEPRecebedor := '';
  fChavePix := '';
  fTipoChavePix := tchNenhuma;
  finfoAdicional := '';
  fTxId := '';
  fValor := 0;
  fpss := -1;
  fmcc := 0;
  fIgnorarErrosQRCode := False;
end;

procedure TACBrPIXQRCodeEstatico.Assign(Source: TACBrPIXQRCodeEstatico);
begin
  fNomeRecebedor := Source.NomeRecebedor;
  fCidadeRecebedor := Source.CidadeRecebedor;
  fCEPRecebedor := Source.CEPRecebedor;
  ChavePix := Source.ChavePix;
  finfoAdicional := Source.infoAdicional;
  fTxId := Source.TxId;
  fValor := Source.Valor;
  fpss := Source.pss;
  fmcc := Source.mcc;
  fIgnorarErrosQRCode := Source.IgnorarErrosQRCode;
end;

function TACBrPIXQRCodeEstatico.GetQRCode: String;
var
  vs: String;
begin
  if (fTipoChavePix = tchNenhuma) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroChaveInvalida), [fChavePix]);

  if (fValor > 0) then
    vs := FormatarValorPIX(fValor)
  else
    vs := '';

  Result := FormatarQRCodeId(0, '01') +                             // Payload Format Indicator
            FormatarQRCodeId(26, GerarMerchantAccountInformation) + // Merchant Account Information
            FormatarQRCodeId(52, IntToStrZero(fmcc, 4)) +           // Merchant Category Code
            FormatarQRCodeId(53, IntToStr(cBRCurrency)) +
            FormatarQRCodeId(54, vs) +
            FormatarQRCodeId(58, cBRCountryCode) +
            FormatarQRCodeId(59, NomeRecebedor) +
            FormatarQRCodeId(60, CidadeRecebedor) +
            FormatarQRCodeId(61, CEPRecebedor) +
            FormatarQRCodeId(62, GerarAdditionalDataField);

  Result := AdicionarCRC(Result);
end;

function TACBrPIXQRCodeEstatico.GerarMerchantAccountInformation: String;
var
  spss, id0, id1, id2, id3: String;
  lt, lim: Integer;
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
    lt := Length(id0) + Length(id1) + Length(id3);
    lim := cEMVLimit - 4 - lt;
    id2 := FormatarQRCodeId(2, copy(infoAdicional, 1, lim));
  end
  else
    id2 := '';

  Result := id0 + id1 + id2 + id3;
end;

function TACBrPIXQRCodeEstatico.GerarAdditionalDataField: String;
begin
  Result := FormatarQRCodeId(5, IfEmptyThen(fTxId, cMPMValueNotInformed));
end;

function TACBrPIXQRCodeEstatico.AdicionarCRC(const ADados: String): String;
var
  s: String;
begin
  s := ADados + cCrcIdAndSize;
  Result := s + UpperCase(Crc16PIX(s));
end;

procedure TACBrPIXQRCodeEstatico.SetChavePix(const AValue: String);
var
  TipoChave: TACBrPIXTipoChave;
begin
  if (fChavePix = AValue) then
    Exit;

  TipoChave := DetectarTipoChave(AValue);
  if (TipoChave = tchNenhuma) then
    raise EACBrPixException.CreateFmt(ACBrStr(sErroChaveInvalida), [AValue]);

  fChavePix := Trim(AValue);
  fTipoChavePix := TipoChave;
end;

procedure TACBrPIXQRCodeEstatico.SetMcc(AValue: Integer);
begin
  if (fmcc = AValue) then
    Exit;

  if (AValue <> 0) and (AValue < cMCCMinimo) or (AValue > cMCCMaximo) then
    raise EACBrPixException.Create(ACBrStr(sErroMCCForaDaFaixa));

  fmcc := AValue;
end;

procedure TACBrPIXQRCodeEstatico.SetNomeRecebedor(const AValue: String);
begin
  if (fNomeRecebedor = AValue) then
    Exit;

  fNomeRecebedor := copy(Trim(AValue),1,25);
end;

procedure TACBrPIXQRCodeEstatico.SetCidadeRecebedor(const AValue: String);
begin
  if (fCidadeRecebedor = AValue) then
    Exit;

  fCidadeRecebedor := copy(Trim(AValue),1,15);
end;

procedure TACBrPIXQRCodeEstatico.SetCEPRecebedor(AValue: String);
begin
  if (fCEPRecebedor = AValue) then
    Exit;

  fCEPRecebedor := OnlyNumber(AValue);
end;

procedure TACBrPIXQRCodeEstatico.SetinfoAdicional(const AValue: String);
begin
  if (finfoAdicional = AValue) then
    Exit;

  finfoAdicional := TiraAcentos(Trim(AValue));
end;

procedure TACBrPIXQRCodeEstatico.SetPss(AValue: Integer);
var
  e: String;
begin
  if (fpss = AValue) then
    Exit;

  e := ValidarPSS(AValue);
  if (e <> '') then
    raise EACBrPixException.Create(ACBrStr(e));

  fpss := AValue;
end;

procedure TACBrPIXQRCodeEstatico.SetTxId(const AValue: String);
var
  e: String;
begin
  if (fTxId = AValue) then
    Exit;

  if (AValue = cMPMValueNotInformed) or (Trim(AValue) = '') then
  begin
    fTxId := '';
    Exit;
  end;

  e := ValidarTxId(AValue, 25);
  if (e <> '') then
    raise EACBrPixException.Create(ACBrStr(e));

  fTxId := AValue;
end;

procedure TACBrPIXQRCodeEstatico.SetQRCode(const AValue: String);
var
  s, id, ValorId: String;
  p, l, TamId: Integer;
begin
  Clear;

  s := Trim(AValue);
  l := Length(s);
  if not VerificarCRC(s) then
    DispararErroQRCode(ACBrStr(sErroCRCInvalido));

  p := 1;
  while (p < l) do
  begin
    LerProximoEMV(s, p, id, TamId, ValorId);

    if (id = '00') then
    begin
      if (ValorId <> '01') then
        DispararErroQRCode(ACBrStr(sErroPayloadIndicator));
    end

    else if (id = '26') then
      ExtrairDadosMerchantAccountInformation(ValorId)

    else if (id = '52') then
    begin
      try
        mcc := StrToInt(ValorId);
      except
        On E: Exception do
          DispararErroQRCode(E.Message);
      end;
    end

    else if (id = '53') then
    begin
      if (ValorId <> IntToStr(cBRCurrency)) then
        DispararErroQRCode(Format(sErroCurrency, [ValorId]));
    end

    else if (id = '54') then
    begin
      try
        Valor := StringToFloat(ValorId);
      except
        On E: Exception do
          DispararErroQRCode(E.Message);
      end;
    end

    else if (id = '58') then
    begin
      if (ValorId <> cBRCountryCode) then
        DispararErroQRCode(sErroCountryCode);
    end

    else if (id = '59') then
      NomeRecebedor := ValorId

    else if (id = '60') then
      CidadeRecebedor := ValorId

    else if (id = '61') then
      CEPRecebedor := ValorId

    else if (id = '62') then
      ExtrairDadosAdditionalDataField(ValorId);

  end;
end;

procedure TACBrPIXQRCodeEstatico.ExtrairDadosMerchantAccountInformation(
  const ADados: String);
var
  p, l, TamId: Integer;
  id, ValorId: String;
begin
  p := 1;
  l := Length(ADados);
  while (p < l) do
  begin
    LerProximoEMV(ADados, p, id, TamId, ValorId);

    if (id = '00') then
    begin
      if (ValorId <> cGUIPIX) then
        DispararErroQRCode(sErroGUIInvalido);
    end

    else if (id = '01') then
    begin
      try
        ChavePix := ValorId;
      except
        On E: Exception do
          DispararErroQRCode(E.Message);
      end;
    end;
  end;
end;

procedure TACBrPIXQRCodeEstatico.ExtrairDadosAdditionalDataField(
  const ADados: String);
var
  p, l, TamId: Integer;
  id, ValorId: String;
begin
  p := 1;
  l := Length(ADados);
  while (p < l) do
  begin
    LerProximoEMV(ADados, p, id, TamId, ValorId);

    if (id = '05') then
    begin
      try
        TxId := ValorId;
      except
        On E: Exception do
          DispararErroQRCode(E.Message);
      end;
    end;
  end;
end;


procedure TACBrPIXQRCodeEstatico.LerProximoEMV(const ADados: String; var p: Integer;
  out id: String; out TamId: Integer; out ValorId: String);
begin
  id := copy(ADados, p, 2);
  TamId := StrToIntDef(copy(ADados, p+2, 2), 0);
  ValorId := '';
  if (TamId = 0) then
    if not fIgnorarErrosQRCode then
      raise EACBrPixException.Create(ACBrStr(sErroSintaxe));

  ValorId := copy(ADados, p+4, TamId);
  p := p + 4 + TamId;
end;

procedure TACBrPIXQRCodeEstatico.DispararErroQRCode(const e: String);
begin
  if fIgnorarErrosQRCode then
    Exit;

  raise EACBrPixException.Create(e);
end;

function TACBrPIXQRCodeEstatico.VerificarCRC(const ADados: String): Boolean;
var
  qr, crc1, crc2: String;
  l: Integer;
begin
  qr := Trim(ADados);
  l := Length(qr);
  crc1 := UpperCase(copy(qr, l-3, 4));
  qr := copy(qr, 1, l-4);
  Result := ((copy(qr, l-3, 4) <> cCrcIdAndSize));
  if Result then
  begin
    crc2 := UpperCase(Crc16PIX(qr));
    Result := (crc1 = crc2);
  end;
end;

end.

