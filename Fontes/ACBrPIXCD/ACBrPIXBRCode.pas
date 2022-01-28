{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }

{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }

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
{ http://www.opensource.org/licenses/lgpl-license.php                          }

{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

(*

  Documentação:
  https://github.com/bacen/pix-api

*)

{$I ACBr.inc}

unit ACBrPIXBRCode;

interface

uses
  Classes, SysUtils,
  ACBrPIXBase;

const
  cGUIPIX = 'br.gov.bcb.pix';
  cBRCurrency = 986;
  cMCCMinimo = 100;
  cMCCMaximo = 9999;
  cEMVMaxID = 99;
  cEMVMaxLen = 99;
  cCrcIdAndSize = '6304';

  cID_PayloadFormatIndicator = 0;
  cID_PointOfInformationMethod = 1;
  cID_MerchantAccountInformationCards = 14;
  cID_MerchantAccountInformation = 26;
    cID_GUI = 0;
    cID_PixKey = 1;
    cID_AdditionalInfo = 2;
    cID_Pss = 3;
    cID_URL = 25;
  cID_MerchantCategoryCode = 52;
  cID_TransactionCurrency = 53;
  cID_TransactionAmount = 54;
  cID_CountryCode = 58;
  cID_MerchantName = 59;
  cID_MerchantCity = 60;
  cID_PostalCode = 61;
  cID_AdditionalDataFieldTemplate = 62;
    cID_TxId = 5;
  cID_CRC = 63;

resourcestring
  sErrMCCOutOfRange = 'MCC fora da Faixa, 0100-9999';
  sErrPayloadIndicator = 'Indicador de Formato inválido';
  sErrCRCInvalid = 'CRC inválido';
  sErrSintax = 'Erro na sintaxe';
  sErrCurrency = 'Moeda %s não é aceita';
  sErrCountryCode = 'País deve ser BR';
  sErrGUIInvalid = 'GUI deve ser: ' + cGUIPIX;
  sErrTLVInvalid = 'String TLV inválida';
  sErrEMVInvalid = 'String EMV inválida';
  sErrEMVId = 'O ID máximo para EMV é 99';
  sErrEMVLen = 'O tamanho máximo para conteúdo EMV é 99';
  sErrMandatoryFieldNotInformed = 'Campo obrigatório, não informado: %s';

type

  { TACBrTLV }

  TACBrTLV = class   // TLV = Type, Length, Value
  private
    fT: byte;
    fV: String;
    function GetL: byte;
    function GetAsString: String;
    procedure SetAsString(AValue: String);
    procedure SetV(AValue: String);
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TACBrTLV);

    property AsString: String read GetAsString write SetAsString;
    property T: byte read fT write fT;
    property L: byte read GetL;
    property V: String read fV write SetV;
  end;

  { TACBrEMVList }

  TACBrEMVList = class(TStringList)
  private
    function GetAsString: String; virtual;
    procedure SetAsString(const AValue: String); virtual;
    function GetValueID(AId: byte): String;
    procedure SetValueID(AId: byte; const AValue: String);

    procedure ReadNextEMVChunk(const AData: String; var p: Integer;
      out AId: byte; out AValue: String);

  public
    property ID[AId: byte]: String read GetValueID write SetValueID; default;
    property AsString: String read GetAsString write SetAsString;
  end;

  { TACBrBRCode }

  TACBrBRCode = class(TACBrEMVList)
  private
    fIgnoreErrors: Boolean;
    fAdditionalDataFieldTemplate: TACBrEMVList;
    fMerchantAccountInformation: TACBrEMVList;

    procedure RaiseError(const e: String);

    function GetPayloadFormatIndicator: byte;
    function GetPointOfInformationMethod: byte;
    function GetMerchantAccountInformationCards: String;
    function GetMerchantCategoryCode: Integer;
    function GetTransactionCurrency: Integer;
    function GetTransactionAmount: currency;
    function GetCountryCode: String;
    function GetMerchantName: String;
    function GetMerchantCity: String;
    function GetPostalCode: String;
    function GetTxId: String;
    function GetCRC16: String;

    procedure SetPayloadFormatIndicator(AValue: byte);
    procedure SetPointOfInformationMethod(AValue: byte);
    procedure SetMerchantAccountInformationCards(AValue: String);
    procedure SetMerchantCategoryCode(AValue: Integer);
    procedure SetTransactionCurrency(AValue: Integer);
    procedure SetTransactionAmount(AValue: currency);
    procedure SetCountryCode(AValue: String);
    procedure SetMerchantName(const AValue: String);
    procedure SetMerchantCity(const AValue: String);
    procedure SetPostalCode(AValue: String);
    procedure SetTxId(AValue: String);
    procedure SetCRC16(AValue: String);

    function GetAsString: String; override;
    procedure SetAsString(const AValue: String); override;
  protected
    function ComputeCRC: String;
    procedure ValidateCRC;

    property MerchantAccountInformation: TACBrEMVList  // 26 - Template
      read fMerchantAccountInformation;
    property AdditionalDataFieldTemplate: TACBrEMVList  // 62 - Template
      read fAdditionalDataFieldTemplate;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear; override;
    procedure AddDefaultValues; virtual;
    procedure Assign(Source: TACBrBRCode); reintroduce;

    property IgnoreErrors: Boolean read fIgnoreErrors write fIgnoreErrors;

    property PayloadFormatIndicator: Byte read GetPayloadFormatIndicator  // 00
      write SetPayloadFormatIndicator;
    property PointOfInformationMethod: Byte read GetPointOfInformationMethod  // 01
      write SetPointOfInformationMethod;
    property MerchantAccountInformationCards: String  // 04
      read GetMerchantAccountInformationCards write SetMerchantAccountInformationCards;
    property MerchantCategoryCode: Integer  // 52 - https://classification.codes/classifications/industry/mcc/
      read GetMerchantCategoryCode write SetMerchantCategoryCode;
    property TransactionCurrency: Integer   // 53
      read GetTransactionCurrency write SetTransactionCurrency;
    property TransactionAmount: currency    // 54
      read GetTransactionAmount write SetTransactionAmount;
    property CountryCode: String read GetCountryCode write SetCountryCode;  // 58
    property MerchantName: String read GetMerchantName write SetMerchantName;  // 59
    property MerchantCity: String read GetMerchantCity write SetMerchantCity;  // 60
    property PostalCode: String read GetPostalCode write SetPostalCode;  // 61
    property TxId: String read GetTxId write SetTxId;  // 62-05 Indentificador da Transação
    property CRC16: String read GetCRC16 write SetCRC16; // 63
  end;

  { TACBrPIXQRCodeEstatico }

  TACBrPIXQRCodeEstatico = class(TACBrBRCode)
  private
    fPixKeyType: TACBrPIXTipoChave;

    function GetPixKey: String;
    function GetAdditionalInfo: String;
    function GetPss: Integer;
    procedure SetPixKey(const AValue: String);
    procedure SetAdditionalInfo(AValue: String);
    procedure SetPss(AValue: Integer);

    function GetAsString: String; override;
  protected

  public
    procedure Clear; override;
    procedure Assign(Source: TACBrPIXQRCodeEstatico);

    property PixKey: String read GetPixKey write SetPixKey;
    property PixKeyType: TACBrPIXTipoChave read fPixKeyType;  // 26-01
    property AdditionalInfo: String read GetAdditionalInfo write SetAdditionalInfo;  // 26-02
    property pss: Integer read GetPss write SetPss;  // 26-03 Prestador de serviço de saque
  end;

  { TACBrPIXQRCodeDinamico }

  TACBrPIXQRCodeDinamico = class(TACBrBRCode)
  private
    function GetAsString: String; override;
    function GetURL: String;
    procedure SetURL(AValue: String);
  public
    property URL: String read GetURL write SetURL;  // 26-25
  end;

implementation

uses
  ACBrPIXUtil, ACBrUtil;

{ TACBrTLV }

constructor TACBrTLV.Create;
begin
  inherited Create;
  Clear;
end;

procedure TACBrTLV.Clear;
begin
  fT := 0;
  fV := '';
end;

procedure TACBrTLV.Assign(Source: TACBrTLV);
begin
  fT := Source.T;
  fV := Source.V;
end;

function TACBrTLV.GetL: byte;
begin
  Result := Length(fV);
end;

procedure TACBrTLV.SetV(AValue: String);
begin
  if fV = AValue then Exit;
  fV := copy(AValue, 1, cEMVMaxLen);
end;

function TACBrTLV.GetAsString: String;
begin
  Result := IntToStrZero(T, 2) + IntToStrZero(L, 2) + V;
end;

procedure TACBrTLV.SetAsString(AValue: String);
var
  st, sl: String;
begin
  Clear;
  try
    st := copy(AValue, 1, 2);
    sl := copy(AValue, 3, 2);
    fT := StrToInt(st);
    fV := copy(AValue, 5, Length(AValue));
    if (Length(fV) <> StrToInt(sl)) then
      raise EACBrPixException.Create('');
  except
    Clear;
    raise EACBrPixException.Create(ACBrStr(sErrTLVInvalid));
  end;
end;

{ TACBrEMVList }

function TACBrEMVList.GetAsString: String;
var
  tlv: TACBrTLV;
  i, t: Integer;
  v, n: String;
begin
  Sort;
  Result := '';
  tlv := TACBrTLV.Create;
  try
    for i := 0 to Count - 1 do
    begin
      n := Names[i];
      t := StrToIntDef(n, -1);
      if (t > -1) and (t <= cEMVMaxID) then
      begin
        v := Trim(Values[n]);
        if (v <> '') then
        begin
          tlv.Clear;
          tlv.T := t;
          tlv.V := v;
          Result := Result + tlv.AsString;
        end;
      end;
    end;
  finally
    tlv.Free;
  end;
end;

procedure TACBrEMVList.SetAsString(const AValue: String);
var
  AId: byte;
  IdValue, s: String;
  l, p: Integer;
begin
  Clear;
  s := Trim(AValue);
  l := Length(s);
  p := 1;
  while (p < l) do
  begin
    ReadNextEMVChunk(s, p, AId, IdValue);
    ID[AId] := IdValue;
  end;
end;

function TACBrEMVList.GetValueID(AId: byte): String;
var
  s: String;
begin
  if (AId > cEMVMaxID) then
    raise EACBrPixException.Create(ACBrStr(sErrEMVId));
  s := IntToStrZero(AId, 2);

  Result := Values[s];
end;

procedure TACBrEMVList.SetValueID(AId: byte; const AValue: String);
begin
  if (AId > cEMVMaxID) then
    raise EACBrPixException.Create(ACBrStr(sErrEMVId));
  if (Length(AValue) > cEMVMaxLen) then
    raise EACBrPixException.Create(ACBrStr(sErrEMVLen));

  Values[IntToStrZero(AId,2)] := AValue;
end;

procedure TACBrEMVList.ReadNextEMVChunk(const AData: String; var p: Integer;
  out AId: byte; out AValue: String);
var
  l, t: Integer;
  tlv: TACBrTLV;
begin
  t := StrToIntDef(copy(AData, p, 2), -1);
  if (t < 0) then
    raise EACBrPixException.Create(ACBrStr(sErrEMVInvalid));

  AId := t;
  l := StrToIntDef(copy(AData, p + 2, 2), 0);
  if (l < 1) or (l > cEMVMaxLen) then
    raise EACBrPixException.Create(ACBrStr(sErrEMVInvalid));

  AValue := copy(AData, p + 4, l);
  p := p + 4 + l;
end;

{ TACBrBRCode }

constructor TACBrBRCode.Create;
begin
  inherited Create;
  fAdditionalDataFieldTemplate := TACBrEMVList.Create;
  fMerchantAccountInformation := TACBrEMVList.Create;
  fIgnoreErrors := False;
  Clear;
end;

destructor TACBrBRCode.Destroy;
begin
  fAdditionalDataFieldTemplate.Free;
  fMerchantAccountInformation.Free;
  inherited Destroy;
end;

procedure TACBrBRCode.Clear;
begin
  inherited Clear;
  fAdditionalDataFieldTemplate.Clear;
  fMerchantAccountInformation.Clear;
  AddDefaultValues;
end;

procedure TACBrBRCode.AddDefaultValues;
begin
  PayloadFormatIndicator := 1;  // version of payload QRCPS-MPM, fixed as “01”
  MerchantCategoryCode := 0;
  TransactionCurrency := cBRCurrency;
  CountryCode := cBRCountryCode;
  TxId := cMPMValueNotInformed;
end;

procedure TACBrBRCode.Assign(Source: TACBrBRCode);
begin
  inherited Assign(Source);
  fAdditionalDataFieldTemplate.Assign(Source.AdditionalDataFieldTemplate);
  fMerchantAccountInformation.Assign(Source.MerchantAccountInformation);
  fIgnoreErrors := Source.IgnoreErrors;
end;

function TACBrBRCode.ComputeCRC: String;
var
  s, crc: String;
begin
  ID[cID_CRC] := '';  // Remove CRC atual
  s := inherited GetAsString;
  s := s + cCrcIdAndSize;
  crc := Crc16BRCode(s);
  ID[cID_CRC] := crc;
  Result := s + crc;
end;

procedure TACBrBRCode.ValidateCRC;
var
  crc1, crc2: String;
begin
  crc1 := Trim(ID[cID_CRC]);
  ComputeCRC;
  crc2 := Trim(ID[cID_CRC]);
  if (crc1 <> crc2) then
    RaiseError(sErrCRCInvalid);
end;

procedure TACBrBRCode.RaiseError(const e: String);
begin
  if fIgnoreErrors then Exit;
  raise EACBrPixException.Create(e);
end;

function TACBrBRCode.GetPayloadFormatIndicator: byte;
begin
  Result := StrToIntDef(ID[cID_PayloadFormatIndicator], 0);
end;

function TACBrBRCode.GetPointOfInformationMethod: byte;
begin
  Result := StrToIntDef(ID[cID_PointOfInformationMethod], 0);
end;

function TACBrBRCode.GetMerchantAccountInformationCards: String;
begin
  Result := ID[cID_MerchantAccountInformationCards];
end;

function TACBrBRCode.GetMerchantCategoryCode: Integer;
begin
  Result := StrToIntDef(ID[cID_MerchantCategoryCode], 0);
end;

function TACBrBRCode.GetTransactionCurrency: Integer;
begin
  Result := StrToIntDef(ID[cID_TransactionCurrency], 0);
end;

function TACBrBRCode.GetTransactionAmount: currency;
begin
  Result := StrToIntDef(ID[cID_TransactionAmount], 0) / 100;
end;

function TACBrBRCode.GetCountryCode: String;
begin
  Result := ID[cID_CountryCode];
end;

function TACBrBRCode.GetMerchantName: String;
begin
  Result := ID[cID_MerchantName];
end;

function TACBrBRCode.GetMerchantCity: String;
begin
  Result := ID[cID_MerchantCity];
end;

function TACBrBRCode.GetPostalCode: String;
begin
  Result := ID[cID_PostalCode];
end;

function TACBrBRCode.GetTxId: String;
begin
  Result := fAdditionalDataFieldTemplate.ID[cID_TxId];
end;

function TACBrBRCode.GetCRC16: String;
begin
  Result := ID[cID_CRC];
end;


procedure TACBrBRCode.SetPayloadFormatIndicator(AValue: byte);
begin
  ID[cID_PayloadFormatIndicator] := IntToStrZero(AValue, 2);
end;

procedure TACBrBRCode.SetPointOfInformationMethod(AValue: byte);
begin
  ID[cID_PointOfInformationMethod] := IntToStrZero(AValue, 2);
end;

procedure TACBrBRCode.SetMerchantAccountInformationCards(AValue: String);
begin
  ID[cID_MerchantAccountInformationCards] := copy(AValue,1,14);
end;

procedure TACBrBRCode.SetMerchantCategoryCode(AValue: Integer);
begin
  if (AValue <> 0) and (AValue < cMCCMinimo) or (AValue > cMCCMaximo) then
    raise EACBrPixException.Create(ACBrStr(sErrMCCOutOfRange));

  ID[cID_MerchantCategoryCode] := IntToStrZero(AValue, 4);
end;

procedure TACBrBRCode.SetTransactionCurrency(AValue: Integer);
begin
  ID[cID_TransactionCurrency] := IntToStrZero(AValue, 3);
end;

procedure TACBrBRCode.SetTransactionAmount(AValue: currency);
begin
  if AValue <= 0 then
    ID[cID_TransactionAmount] := ''
  else
    ID[cID_TransactionAmount] := FormatarValorPIX(AValue);
end;

procedure TACBrBRCode.SetCountryCode(AValue: String);
begin
  ID[cID_CountryCode] := UpperCase(PadRight(AValue, 2));
end;

procedure TACBrBRCode.SetMerchantName(const AValue: String);
begin
  ID[cID_MerchantName] := TiraAcentos(copy(Trim(AValue), 1, 25));
end;

procedure TACBrBRCode.SetMerchantCity(const AValue: String);
begin
  ID[cID_MerchantCity] := TiraAcentos(copy(Trim(AValue), 1, 15));
end;

procedure TACBrBRCode.SetPostalCode(AValue: String);
begin
  ID[cID_PostalCode] := OnlyNumber(AValue);
end;

procedure TACBrBRCode.SetTxId(AValue: String);
var
  e, s: String;
begin
  s := Trim(AValue);
  if (s = cMPMValueNotInformed) or (s = '') then
    fAdditionalDataFieldTemplate.ID[cID_TxId] := cMPMValueNotInformed
  else
  begin
    e := ValidarTxId(s, 25);
    if (e <> '') then
      RaiseError(ACBrStr(e));

    fAdditionalDataFieldTemplate.ID[cID_TxId] := s;
  end;
end;

procedure TACBrBRCode.SetCRC16(AValue: String);
begin
  ID[cID_CRC] := PadLeft(AValue, 4, '0');
end;


function TACBrBRCode.GetAsString: String;
begin
  if (PayloadFormatIndicator = 0) then
    RaiseError(Format(ACBrStr(sErrMandatoryFieldNotInformed),['PayloadFormatIndicator']));

  if (ID[cID_MerchantCategoryCode] = '') then
    RaiseError(Format(ACBrStr(sErrMandatoryFieldNotInformed),['MerchantCategoryCode']));

  if (TransactionCurrency = 0) then
    RaiseError(Format(ACBrStr(sErrMandatoryFieldNotInformed),['TransactionCurrency']));

  if (CountryCode = '') then
    RaiseError(Format(ACBrStr(sErrMandatoryFieldNotInformed),['CountryCode']));

  if (MerchantName = '') then
    RaiseError(Format(ACBrStr(sErrMandatoryFieldNotInformed),['MerchantName']));

  if (MerchantCity = '') then
    RaiseError(Format(ACBrStr(sErrMandatoryFieldNotInformed),['MerchantCity']));

  if (fMerchantAccountInformation.Count > 0) then
  begin
    fMerchantAccountInformation.ID[cID_GUI] := cGUIPIX;
    ID[cID_MerchantAccountInformation] := fMerchantAccountInformation.AsString;
  end
  else
    ID[cID_MerchantAccountInformation] := '';

  if (fAdditionalDataFieldTemplate.Count > 0) then
    ID[cID_AdditionalDataFieldTemplate] := fAdditionalDataFieldTemplate.AsString
  else
    ID[cID_AdditionalDataFieldTemplate] := '';

  Result := ComputeCRC;
end;

procedure TACBrBRCode.SetAsString(const AValue: String);
begin
  inherited SetAsString(AValue);
  fMerchantAccountInformation.AsString := ID[cID_MerchantAccountInformation];
  fAdditionalDataFieldTemplate.AsString := ID[cID_AdditionalDataFieldTemplate];
end;

{ TACBrPIXQRCodeEstatico }

procedure TACBrPIXQRCodeEstatico.Clear;
begin
  inherited Clear;
  fPixKeyType := tchNenhuma;
end;

procedure TACBrPIXQRCodeEstatico.Assign(Source: TACBrPIXQRCodeEstatico);
begin
  inherited Assign(Source);
  fPixKeyType := Source.PixKeyType;
end;

function TACBrPIXQRCodeEstatico.GetPixKey: String;
begin
  Result := MerchantAccountInformation.ID[cID_PixKey];
end;

function TACBrPIXQRCodeEstatico.GetAdditionalInfo: String;
begin
  Result := MerchantAccountInformation.ID[cID_AdditionalInfo];
end;

function TACBrPIXQRCodeEstatico.GetPss: Integer;
begin
  Result := StrToIntDef(MerchantAccountInformation.ID[cID_Pss], 0);
end;

procedure TACBrPIXQRCodeEstatico.SetPixKey(const AValue: String);
var
  TipoChave: TACBrPIXTipoChave;
begin
  TipoChave := DetectarTipoChave(AValue);
  if (TipoChave = tchNenhuma) then
    RaiseError(Format(ACBrStr(sErroChaveInvalida), [AValue]));

  MerchantAccountInformation.ID[cID_PixKey] := Trim(AValue);
  fPixKeyType := TipoChave;
end;

procedure TACBrPIXQRCodeEstatico.SetAdditionalInfo(AValue: String);
begin
  MerchantAccountInformation.ID[cID_AdditionalInfo] := Trim(AValue);
end;

procedure TACBrPIXQRCodeEstatico.SetPss(AValue: Integer);
var
  e: String;
begin
  if AValue <= 0 then
    MerchantAccountInformation.ID[cID_Pss] := ''
  else
  begin
    e := ValidarPSS(AValue);
    if (e <> '') then
      RaiseError(ACBrStr(e));

    MerchantAccountInformation.ID[cID_Pss] := IntToStrZero(AValue, 8);
  end;
end;

function TACBrPIXQRCodeEstatico.GetAsString: String;
var
  s: String;
  l, d: Integer;
begin
  if (PixKey = '') then
    RaiseError(Format(ACBrStr(sErrMandatoryFieldNotInformed),['PixKey']));

  MerchantAccountInformation.ID[cID_GUI] := cGUIPIX;
  s := MerchantAccountInformation.AsString;
  l := Length(s);
  if (l > cEMVMaxLen) then
  begin
    d := cEMVMaxLen - l;
    s := AdditionalInfo;
    l := Length(s);
    AdditionalInfo := copy(s, 1, l-d);
  end;

  Result := inherited GetAsString;
end;

{ TACBrPIXQRCodeDinamico }

function TACBrPIXQRCodeDinamico.GetAsString: String;
begin
  if (URL = '') then
    RaiseError(Format(ACBrStr(sErrMandatoryFieldNotInformed),['URL']));

  Result := inherited GetAsString;
end;

function TACBrPIXQRCodeDinamico.GetURL: String;
begin
  Result := MerchantAccountInformation.ID[cID_URL];
end;

procedure TACBrPIXQRCodeDinamico.SetURL(AValue: String);
var
  s: String;
begin
  s := Trim(AValue);
  if (pos('https://', s) = 1) then
    s := copy(s, 9, Length(s));

  MerchantAccountInformation.ID[cID_URL] := s;
end;

end.

