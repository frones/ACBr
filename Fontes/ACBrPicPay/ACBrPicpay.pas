{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Felipe Baldin                                   }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{ Esse arquivo usa a classe  SynaSer   Copyright (c)2001-2003, Lukas Gebauer   }
{  Project : Ararat Synapse     (Found at URL: http://www.ararat.cz/synapse/)  }
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

{******************************************************************************}
{* Historico                                                                   }
{*                                                                             }
{* 24/03/2020: Primeira Versao - Felipe Baldin                                 }
{*    Doação para o projeto ACBR                                               }
{*    Criaçao do componente ACBrPicpay, que implementa a integração com a API  }
{*    do Picpay através de métodos HTTP usando a suite Synapse para envio      }
{*    e retorno de arquivos usando Json nativo do ACBr.                        }
{******************************************************************************}

unit ACBrPicpay;

{$I ACBr.inc}

interface

uses
  Classes, SysUtils,
  {$IFDEF FPC}
  base64,
  {$ELSE}
  EncdDecd,
  {$ENDIF}
//  {$IfDef USE_JSONDATAOBJECTS_UNIT}
//    JsonDataObjects_ACBr,
//  {$Else}
    Jsons,
//  {$EndIf}
  httpsend, synautil,
   ACBRBase, ACBRValidador, ACBrSocket
   , SyncObjs
   //, ACBrPicpayThreadCallback
   ;

const
  PICPAY_URL_REQUISICAO_PAG   = 'https://appws.picpay.com/ecommerce/public/payments';
  PICPAY_URL_CANCELAMENTO_REQ = 'https://appws.picpay.com/ecommerce/public/payments/{referenceId}/cancellations';
  PICPAY_URL_STATUS_REQ       = 'https://appws.picpay.com/ecommerce/public/payments/{referenceId}/status';

type
  EACBrPicpayError = class( Exception ) ;

  TWaitingPayment = procedure (const Status: String; const TempoRestante: Integer) of Object;

  TWaitingTimeout = procedure (var Retry: Boolean) of object;

  TStatusPayment = procedure (AuthorizationId, Status : String) of Object;

  TErrorPayment = procedure (Error : String) of Object;

  TACBrPicPayPessoa = (pFisica, pJuridica);

  TACBrTipoRetornoPicpay = (trNenhum, trThread, trCallback);

  TACBrPicPay = class;

  { TACBrThread }
  TACBrPicPayThread = class(TThread)
  private
    fevCancelarConsulta: TSimpleEvent;
    fAcordaEvt: TSimpleEvent;
    fPausado: Boolean;
    fACBrPicpay: TACBrPicPay;
    fTempoRestante: Integer;
    function getPausado: Boolean;
    procedure setPausado(const Value: Boolean);
    procedure StatusPayment;
    procedure FazWaitingPayment;
    procedure FazConsulta;
    procedure FazWaitingTimeout;
  protected
    procedure Execute; override;
  public
    constructor Create ( AOwner: TACBrPicPay );
    destructor Destroy; override;

    property Pausado: Boolean read getPausado write setPausado;
  end;

  { TACBrLojista }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrPicPayLojista = class(TComponent)
  private
    fURLCallBack: string;
    fURLReturn: string;
    fPicpayToken: string;
    fSellerToken: string;

    fACBrPicpay: TACBrPicPay;
  public
    constructor Create ( AOwner: TComponent ); override;
  published
    //URL do servidor que vai receber a callback
    property URLCallBack: string read fURLCallBack write fURLCallBack;
    property URLReturn: string read fURLReturn write fURLReturn;

    //Esses dados estão disponíveis no site do picpay quando abre a conta.
    property PicpayToken: string read fPicpayToken write fPicpayToken;
    property SellerToken: string read fSellerToken write fSellerToken;
  end;


  { TACBrComprador }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrPicPayComprador = class(TComponent)
  private
    fNome: string;
    fSobreNome: string;
    fDocumento: string;
    fEmail: string;
    fTelefone: string;
    fTipoInscricao: TACBrPicPayPessoa;

    fACBrPicpay: TACBrPicPay;

    procedure SetTipoInscricao ( const AValue: TACBrPicPayPessoa ) ;
    procedure SetDocumento(const AValue: String );
  public
    constructor Create ( AOwner: TComponent); override;
    property ACBrPicpay  : TACBrPicPay read fACBrPicpay;
  published
    property Nome: string read fNome write fNome;
    property SobreNome: string read fSobreNome write fSobreNome;
    property Documento: string read fDocumento write SetDocumento;
    property Telefone: string read fTelefone write fTelefone;
    property Email: string read fEmail write fEmail;

    property TipoInscricao: TACBrPicPayPessoa  read fTipoInscricao write  SetTipoInscricao;
  end;


  { TACBrPicpay }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TACBrPicPay = class(TACBrHTTP)
  private
    fReferenceId: string;
    fCancellationId: string;
    fAuthorizationId: string;
    fStatus: string;
    fProduto: string;
    fValor: Currency;
    fExpirar : TDateTime;
    fCancelarAguardoRetorno: Boolean;
    fQRCodeText : String;

    fTipoRetorno: TACBrTipoRetornoPicpay;
    fOnStatusPayment: TStatusPayment;
    fOnErrorPayment: TErrorPayment;
    fOnWaitingPayment: TWaitingPayment;

    fTempoRetorno: Integer;

    fQRCode: string;
    fUltimoQRCodeStream: TStringStream;

    fLojista: TACBrPicPayLojista;
    fComprador: TACBrPicPayComprador;
    fThreadAguardaRetorno: TACBrPicPayThread;
    fOnWaitingTimeout: TWaitingTimeout;

    procedure AguardarRetorno;

    procedure SetTipoRetorno ( const AValue: TACBrTipoRetornoPicpay ) ;
    procedure SetTempoRetorno(const AValue: Integer);
    procedure SetReferenceId(const AValue: String);
    function GetQRCode: TStringStream;
  protected
    { Protected declarations }
  public
    property QRCode: TStringStream read GetQRCode;
    property Status: string read fStatus;
    property QRCodeText: string read fQRCodeText;
    procedure Enviar;
    procedure Consultar;
    procedure CancelarAguardoRetorno;
    function Cancelar(const aAuthorizationId: string): Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published

    //Dados do pagamento
    property ReferenceId: string read fReferenceId write SetReferenceId;
    property CancellationId: string read fCancellationId;
    property AuthorizationId: string read fAuthorizationId;
    property Produto: string read fProduto write fProduto;
    property Valor: Currency read fValor write fValor;
    property Expirar : TDateTime read fExpirar write fExpirar;// expiresAt

    property Comprador: TACBrPicPayComprador read fComprador write fComprador;
    property Lojista: TACBrPicPayLojista read fLojista write fLojista;
    property TempoRetorno: Integer read fTempoRetorno write SetTempoRetorno;
    property TipoRetorno: TACBrTipoRetornoPicpay read fTipoRetorno write SetTipoRetorno;

    property OnStatusPayment: TStatusPayment read FOnStatusPayment write FOnStatusPayment;
    property OnWaitingPayment: TWaitingPayment read fOnWaitingPayment write fOnWaitingPayment;
    property OnErrorPayment: TErrorPayment read fOnErrorPayment write fOnErrorPayment;
    property OnWaitingTimeout: TWaitingTimeout read fOnWaitingTimeout write fOnWaitingTimeout;
  end;

implementation

uses
  {$IFDEF DELPHIXE8_UP}
    System.NetEncoding,
  {$ENDIF}
  StrUtils,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  DateUtils;

{ *** TACBrPicPayThread *** }

{ Private }


function TACBrPicPayThread.getPausado: Boolean;
begin
  Result := fPausado;
end;

procedure TACBrPicPayThread.setPausado(const Value: Boolean);
begin
  if (not Terminated) and (fPausado <> Value) then
  begin
    fPausado := Value;
    if fPausado then
      fAcordaEvt.ResetEvent
    else
      fAcordaEvt.SetEvent;
  end;
end;

procedure TACBrPicPayThread.StatusPayment;
begin
  if Assigned(fACBrPicpay.fOnStatusPayment) then
    fACBrPicpay.fOnStatusPayment(fACBrPicpay.fAuthorizationId, fACBrPicpay.fStatus);
end;


{ Protected }

procedure TACBrPicPayThread.Execute;
begin
  while (not Terminated) do
  begin
    Pausado := True;
    fAcordaEvt.WaitFor(Cardinal(-1));

    // Foi acordado para terminar?
    if not Terminated then
      FazConsulta;
  end;
end;

procedure TACBrPicPayThread.FazWaitingPayment;
begin
  if Assigned(fACBrPicpay.fOnWaitingPayment) then
    fACBrPicpay.fOnWaitingPayment(fACBrPicpay.Status, fTempoRestante);
end;

procedure TACBrPicPayThread.FazWaitingTimeout;
var
  Retry: Boolean;
begin
  Retry := False;
  if Assigned(fACBrPicpay.fOnWaitingTimeout) then
    fACBrPicpay.fOnWaitingTimeout(Retry);
  if Retry then
    fTempoRestante := fACBrPicpay.fTempoRetorno
  else
    fACBrPicpay.CancelarAguardoRetorno;
end;

{ Public }

constructor TACBrPicPayThread.Create(AOwner: TACBrPicPay);
begin
  fACBrPicpay := TACBrPicPay(AOwner);
  fPausado := True;
  fAcordaEvt := TSimpleEvent.Create;
  fevCancelarConsulta := TSimpleEvent.Create;
  inherited Create(False);
end;

destructor TACBrPicPayThread.Destroy;
begin
  fPausado := True;
  Terminate;
  fevCancelarConsulta.SetEvent;
  fAcordaEvt.SetEvent;
  if not Terminated then
    WaitFor;

  fAcordaEvt.Free;
  fevCancelarConsulta.Free;
  inherited;
end;

procedure TACBrPicPayThread.FazConsulta;
begin
  fTempoRestante  := fACBrPicpay.fTempoRetorno;

  while (fevCancelarConsulta.WaitFor(1000) <> wrSignaled) and (not Terminated) do
  begin
    Dec(fTempoRestante);
    if fACBrPicpay.fQRCode <> '' then
      Synchronize(fACBrPicpay.Consultar);

    if fACBrPicpay.fStatus = 'paid' then
    begin
      Synchronize(StatusPayment);
      Break;
    end;

    if (fTempoRestante <= 0) then
      Synchronize(FazWaitingTimeout)
    else
      Synchronize(FazWaitingPayment);
  end;
end;


{ *** TACBrLojista *** }

{ Public }

constructor TACBrPicPayLojista.Create ( AOwner: TComponent );
begin
  inherited Create(AOwner);
  fACBrPicpay := TACBrPicPay(AOwner);
end;

{ *** TACBrComprador *** }

{ Private }

procedure TACBrPicPayComprador.SetTipoInscricao ( const AValue: TACBrPicPayPessoa ) ;
begin
   if fTipoInscricao = AValue then
      exit;

   fTipoInscricao := AValue;
end;

procedure TACBrPicPayComprador.SetDocumento ( const AValue: String ) ;
var
  ACBrVal: TACBrValidador;
  ADocto: String;
begin
   if fDocumento = AValue then
     Exit;

   ADocto := OnlyNumber(AValue);
   if EstaVazio(ADocto) then
   begin
      fDocumento:= ADocto;
      Exit;
   end;

   if fDocumento = ADocto then
     Exit;

   ACBrVal := TACBrValidador.Create(Self);
   try
     if fTipoInscricao = pFisica then
     begin
       ACBrVal.TipoDocto := docCPF;
       ACBrVal.Documento := RightStr(ADocto,11);
     end
     else
     begin
       ACBrVal.TipoDocto := docCNPJ;
       ACBrVal.Documento := RightStr(ADocto,14);
     end;

     ACBrVal.IgnorarChar := './-';
     ACBrVal.RaiseExcept := True;
     ACBrVal.Validar;    // Dispara Exception se Documento estiver errado

     fDocumento := ACBrVal.Formatar;
   finally
     ACBrVal.Free;
   end;
end;

{ Public }

constructor TACBrPicPayComprador.Create ( AOwner: TComponent);
begin
  inherited Create(AOwner);
  fDocumento := '';

  fTipoInscricao := pFisica;
  fACBrPicpay := TACBrPicPay(AOwner);
end;

{ *** TACBrPicpay *** }

{ Private }

procedure TACBrPicPay.SetTipoRetorno ( const AValue: TACBrTipoRetornoPicpay ) ;
begin
  if fTipoRetorno = AValue then
    Exit;

  fTipoRetorno := AValue;
end;


procedure TACBrPicPay.SetTempoRetorno(const AValue: Integer);
begin
  if AValue = fTempoRetorno then
    Exit;

  if AValue <= 0  then
    fTempoRetorno := 1
  else
    fTempoRetorno := AValue;
end;

procedure TACBrPicPay.SetReferenceId(const AValue: String);
var
  aId: Integer;
begin
  if fReferenceId = AValue then
    Exit;

  fReferenceId := AValue;
  aId := StrToIntDef(Trim(AValue), 0);

  if aId = 0 then
    Exit;

  fReferenceId := IntToStrZero(aId, 6 );
end;

function TACBrPicPay.GetQRCode: TStringStream;

{$IFDEF FPC}
  procedure DecodeQRCodeLazarusFPC;
  var
    vData: Ansistring;
  begin
    vData := DecodeStringBase64(fQRCode);
    Result := TStringStream.Create(vData);
    Result.Position := 0;
  end;
{$ELSE}
  procedure DecodeQRCodeDelphi;
  var
    Input: TStringStream;
  begin
    Input := TStringStream.Create(fQRCode);
    try
      Result := TStringStream.Create(fQRCode);
      DecodeStream(Input, Result);
      Result.Position := 0;
    finally
      Input.Free;
    end;
  end;
{$ENDIF}

begin
  if fQRCode = ''  then
  begin
    Result := nil;
    EACBrPicpayError.Create('QRCode está vazio ou inválido.');
  end;

  fQRCode := StringReplace(fQRCode, 'data:image/png;base64,', '', [rfReplaceAll]);

  {$IFDEF FPC}
    DecodeQRCodeLazarusFPC;
  {$ELSE}
    DecodeQRCodeDelphi;
  {$ENDIF}

  if Assigned(fUltimoQRCodeStream) then
  begin
    FreeAndNil(fUltimoQRCodeStream);
  end;
  fUltimoQRCodeStream := Result;
end;

procedure TACBrPicPay.AguardarRetorno;
begin
  fCancelarAguardoRetorno := False;
  case fTipoRetorno of
    trThread:
    begin
      fThreadAguardaRetorno.fevCancelarConsulta.ResetEvent;
      fThreadAguardaRetorno.Pausado := False;
    end;
    trCallback:
    begin
      EACBrPicpayError.Create('Tem que implementar o servidor HTTP.');
    end;
  end;
end;

{ Public }

procedure TACBrPicPay.Consultar;
var
  JsonResponse: TJson;
  Url: string;
  I: Integer;
begin
  Url := StringReplace(PICPAY_URL_STATUS_REQ, '{referenceId}', fReferenceId, [rfReplaceAll]);

  Self.HTTPSend.Headers.Clear;
  Self.HTTPSend.UserAgent := 'Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV2';
  Self.HTTPSend.KeepAlive := True;
  Self.HTTPSend.MimeType := 'application/json';
  Self.HTTPSend.Headers.Add('Content-Type: application/json');
  Self.HTTPSend.Headers.Add('x-picpay-token: ' + fLojista.fPicpayToken);
  Self.HTTPSend.Protocol := '1.1';
  Self.HTTPSend.Status100 := False;

  Self.HTTPMethod('GET', Url);

  { Convertendo uma string para json object }
  JsonResponse := TJSon.Create;
  try
    JsonResponse.Parse(DecodeToString(HTTPResponse, RespIsUTF8));
    for I := 0 to JsonResponse.Count - 1 do
    begin

      fReferenceId := JsonResponse.Values['referenceId'].AsString;
      fStatus := JsonResponse.Values['status'].AsString;

      if SameText('paid', fStatus) then
        fAuthorizationId := JsonResponse.Values['authorizationId'].AsString;
    end;
  finally
    JsonResponse.Free;
  end;
end;

procedure TACBrPicPay.Enviar;
var
  data: AnsiString;
  DataToSend : TStringStream;
  JsonBuyer: TJsonPair;
  Json: TJSONObject;
  JsonCliente: TJSONObject;
  JsonResponse: TJson;
  I: Integer;
  J: Integer;
begin
  Json := TJsonObject.Create;
  try
    Json.Add('referenceId').Value.AsString := fReferenceId;
    Json.Add('callbackUrl').Value.AsString := fLojista.fURLCallBack;
    Json.Add('returnUrl').Value.AsString   := fLojista.fURLReturn;
    Json.Add('productName').Value.AsString := fProduto;
    Json.Add('value').Value.AsNumber       := fValor;
    if fExpirar <> 0 then
       Json.Add('expiresAt').Value.AsString := DateTimeToIso8601(fExpirar) ;
    JsonCliente := TJSONObject.Create;
    try
      JsonCliente.Add('firstName').Value.AsString := fComprador.fNome;
      JsonCliente.Add('lastName').Value.AsString  := fComprador.fSobreNome;
      JsonCliente.Add('document').Value.AsString  := fComprador.fDocumento;
      JsonCliente.Add('email').Value.AsString     := fComprador.fEmail;
      JsonCliente.Add('phone').Value.AsString     := fComprador.fTelefone;

      JsonBuyer := TJsonPair.Create(Json, 'buyer');
      try
        JsonBuyer.Value.AsObject := JsonCliente;
        Json.Add('buyer').Assign(JsonBuyer);
      finally
        JsonBuyer.Free;
      end;
    finally
      JsonCliente.Free;
    end;
    data := Json.Stringify;
  finally
    Json.Free;
  end;

  data := NativeStringToUTF8(data);

  DataToSend := TStringStream.Create(data);
  try
    Self.HTTPSend.Headers.Clear;
    Self.HTTPSend.UserAgent := 'Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV2';
    Self.HTTPSend.KeepAlive := True;
    Self.HTTPSend.MimeType := 'application/json';
    Self.HTTPSend.Headers.Add('Content-Type: application/json');
    Self.HTTPSend.Headers.Add('x-picpay-token: ' + fLojista.fPicpayToken);
    Self.HTTPSend.Protocol := '1.1';
    Self.HTTPSend.Status100 := True;
    Self.HTTPSend.Document.LoadFromStream(DataToSend);
  finally
    DataToSend.Free;
  end;

  Self.HTTPMethod('POST', PICPAY_URL_REQUISICAO_PAG);

  { Convertendo uma string para json object }
  JsonResponse := TJSon.Create;
  try
    JsonResponse.Parse(DecodeToString(HTTPResponse, RespIsUTF8));
    for I := 0 to JsonResponse.Count - 1 do
    begin

      fReferenceId := JsonResponse.Values['referenceId'].AsString;
      fQRCodeText := JsonResponse.Values['paymentUrl'].AsString;

      for J := 0 to TJsonObject(JsonResponse.Values['qrcode'].AsObject).Count - 1 do
      begin
        fQrCode := TJsonObject(JsonResponse.Values['qrcode'].AsObject).Values['base64'].AsString;
      end;
    end;
  finally
    JsonResponse.Free;
  end;

  if fQRCode <> '' then
  begin
    AguardarRetorno;
  end;

end;

function TACBrPicPay.Cancelar(const aAuthorizationId: String): Boolean;
var
  JsonPicPay: TJsonObject;
  JsonResponse: TJSONObject;
  Url: string;
  DataToSend: TStringStream;
  data: string;
  I: Integer;
begin
  Result := False;

  JsonPicPay := TJsonObject.Create;
  try
    JsonPicPay.Add('picpaytoken').Value.AsString := fLojista.PicpayToken;
    JsonPicPay.Add('sellertoken').Value.AsString := fLojista.SellerToken;
    JsonPicPay.Add('referenceId').Value.AsString := fReferenceId;
    JsonPicPay.Add('authorizationId').Value.AsString := aAuthorizationId;

    data := JsonPicPay.Stringify;

  finally
    JsonPicPay.Free;
  end;

  Url := StringReplace(PICPAY_URL_CANCELAMENTO_REQ, '{referenceId}', fReferenceId, [rfReplaceAll]);

  data := UTF8ToNativeString(data);

  DataToSend := TStringStream.Create(data);
  try
    Self.HTTPSend.Headers.Clear;
    Self.HTTPSend.UserAgent := 'Mozilla/4.0 (compatible; MSIE 6.0; Windows NT 5.1; SV2';
    Self.HTTPSend.KeepAlive := True;
    Self.HTTPSend.MimeType := 'application/json';
    Self.HTTPSend.Headers.Add('Content-Type: application/json');
    Self.HTTPSend.Headers.Add('x-picpay-token: ' + fLojista.fPicpayToken);
    Self.HTTPSend.Protocol := '1.1';
    Self.HTTPSend.Status100 := True;
    Self.HTTPSend.Document.LoadFromStream(DataToSend);

    Self.HTTPMethod('POST', Url);

    { Convertendo uma string para json object }

    if EstaVazio(HTTPResponse) then
      Exit;

    JsonResponse := TJsonObject.Create(nil);
    try
      JsonResponse.Parse(DecodeToString(HTTPResponse, RespIsUTF8));
      for I := 0 to JsonResponse.Count - 1 do
      begin
        fReferenceId := JsonResponse.Values['referenceId'].AsString;
        fCancellationId := JsonResponse.Values['cancellationId'].AsString;
      end;

    finally
      JsonResponse.Free;
    end;

    Result := fCancellationId <> '';
  finally
    DataToSend.Free;
  end;
end;

procedure TACBrPicPay.CancelarAguardoRetorno;
begin
  fCancelarAguardoRetorno := True;
  if TipoRetorno = trThread then
  begin
    fThreadAguardaRetorno.fevCancelarConsulta.SetEvent;
  end;

end;

constructor TACBrPicPay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fCancelarAguardoRetorno := False;

  fLojista      := TACBrPicPayLojista.Create(Self);
  fLojista.Name := 'Lojista';
  {$IFDEF COMPILER6_UP}
   fLojista.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}

  fComprador      := TACBrPicPayComprador.Create(self);
  fComprador.Name := 'Comprador';
  {$IFDEF COMPILER6_UP}
   fComprador.SetSubComponent(True);   // Ajustando como SubComponente para aparecer no ObjectInspector
  {$ENDIF}
  fExpirar := 0;

  fTipoRetorno := trThread;
  fThreadAguardaRetorno := TACBrPicPayThread.Create(Self);
end;

destructor TACBrPicPay.Destroy;
begin
  if Assigned(fUltimoQRCodeStream) then
    FreeAndNil(fUltimoQRCodeStream);
  fComprador.Free;
  fLojista.Free;
  fThreadAguardaRetorno.Terminate;
  fThreadAguardaRetorno.Free;
  inherited;
end;

end.

