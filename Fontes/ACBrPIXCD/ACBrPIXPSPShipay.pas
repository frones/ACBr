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

(*

  Documentação
  https://api-staging.shipay.com.br/docs.html

*)

{$I ACBr.inc}

unit ACBrPIXPSPShipay;

interface

uses
  Classes, SysUtils,
  ACBrPIXCD, ACBrShipaySchemas;

const
  cShipayURLStaging = 'https://api-staging.shipay.com.br';
  cShipayURLProducao = 'https://api.shipay.com.br';
  cShipayEndPointAuth = '/pdvauth';
  cShipayEndPointRefreshToken = '/refresh-token';
  cShipayEndPointWallets = '/v1/wallets';
  cShipayEndPointOrder = '/order';
  cShipayEndPointOrderV = '/orderv';
  cShipayWalletPix = 'pix';
  cItemTitleNotInformed = 'Item Vendido';

resourcestring
  sErrOrderIdDifferent = 'order_id diferente do informado';
  sErrOrderIdNotCancelled = 'order_id: %s, não cancelado.'+sLineBreak+'status: %s';
  sErrOrderRefNotInformed = 'order_ref não informado';
  sErrNoWallet = 'Nenhuma Carteira associada a essa conta';

type

  TShipayQuandoEnviarOrder = procedure(ShipayOrder: TShipayOrder) of object;

  { TACBrPSPShipay }

  TACBrPSPShipay = class(TACBrPSP)
  private
    fAccessKey: String;
    fOrder: TShipayOrder;
    fOrderCreated: TShipayOrderCreated;
    fOrderInfo: TShipayOrderInfo;
    fQuandoEnviarOrder: TShipayQuandoEnviarOrder;
    fRefreshToken: String;
    fWallets: TShipayWalletArray;
    function GetSecretKey: String;
    procedure SetSecretKey(AValue: String);

    procedure ProcessarAutenticacao(const AURL: String; ResultCode: Integer;
      const RespostaHttp: AnsiString);
    procedure QuandoAcessarEndPoint(const AEndPoint: String;
      var AURL: String; var AMethod: String);
    procedure QuandoReceberRespostaEndPoint(const AEndPoint, AMethod: String;
      var AResultCode: Integer; var RespostaHttp: AnsiString);

    function ConverterJSONCobSolicitadaParaShipayOrder(const CobSolicitadaJSON: String): String;
    function ConverterJSONOrderCreatedParaCobGerada(const OrderCreatedJSON: String): String;

  protected
    function ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;

    procedure Autenticar; override;
    procedure RenovarToken; override;

    procedure GetWallets;
    procedure PostOrder;
    procedure DeleteOrder(const order_id: String);

    property Wallets: TShipayWalletArray read fWallets;
    property Order: TShipayOrder read fOrder;
    property OrderCreated: TShipayOrderCreated read fOrderCreated;
    property OrderInfo: TShipayOrderInfo read fOrderInfo;
  published
    property ClientID;
    property SecretKey: String read GetSecretKey write SetSecretKey;
    property AccessKey: String read fAccessKey write fAccessKey;

    property QuandoEnviarOrder: TShipayQuandoEnviarOrder read fQuandoEnviarOrder
      write fQuandoEnviarOrder;
  end;

implementation

uses
  synautil,
  ACBrUtil,
  ACBrPIXBase, ACBrPIXSchemasCob, ACBrPIXBRCode,
  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   JsonDataObjects_ACBr
  {$Else}
   Jsons
  {$EndIf},
  DateUtils;

{ TACBrPSPShipay }

constructor TACBrPSPShipay.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fWallets := TShipayWalletArray.Create('wallets');
  fOrder := TShipayOrder.Create('');
  fOrderCreated := TShipayOrderCreated.Create('');
  fOrderInfo := TShipayOrderInfo.Create('');
  fRefreshToken := '';
  fAccessKey := '';
  fQuandoEnviarOrder := Nil;
  fpQuandoAcessarEndPoint := QuandoAcessarEndPoint;
  fpQuandoReceberRespostaEndPoint := QuandoReceberRespostaEndPoint;
end;

destructor TACBrPSPShipay.Destroy;
begin
  fWallets.Free;
  fOrder.Free;
  fOrderCreated.Free;
  fOrderInfo.Free;
  inherited Destroy;
end;

procedure TACBrPSPShipay.Clear;
begin
  inherited Clear;
  fOrder.Clear;
  fOrderCreated.Clear;
  fOrderInfo.Clear;
end;

procedure TACBrPSPShipay.Autenticar;
var
  AURL, Body: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
  js: TJsonObject;
begin
  LimparHTTP;
  AURL := ObterURLAmbiente( ACBrPixCD.Ambiente ) + cShipayEndPointAuth;

  {$IfDef USE_JSONDATAOBJECTS_UNIT}
   js := TJsonObject.Parse(RespostaHttp) as TJsonObject;
   try
     js.S['access_key'] := AccessKey;
     js.S['secret_key'] := SecretKey;
     js.S['client_id'] := ClientID;
     Body := js.ToJSON();
   finally
     js.Free;
   end;
  {$Else}
   js := TJsonObject.Create;
   try
     js['access_key'].AsString := AccessKey;
     js['secret_key'].AsString := SecretKey;
     js['client_id'].AsString := ClientID;
     Body := js.Stringify;
   finally
     js.Free;
   end;
  {$EndIf}

  WriteStrToStream(Http.Document, Body);
  Http.MimeType := CContentTypeApplicationJSon;
  TransmitirHttp(ChttpMethodPOST, AURL, ResultCode, RespostaHttp);
  ProcessarAutenticacao(AURL, ResultCode, RespostaHttp);
end;

procedure TACBrPSPShipay.RenovarToken;
var
  AURL: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  LimparHTTP;
  AURL := ObterURLAmbiente( ACBrPixCD.Ambiente ) + cShipayEndPointRefreshToken;

  Http.Headers.Insert(0, ChttpHeaderAuthorization + ChttpAuthorizationBearer+' '+fpRefereshToken);
  TransmitirHttp(ChttpMethodPOST, AURL, ResultCode, RespostaHttp);
  ProcessarAutenticacao(AURL, ResultCode, RespostaHttp);
end;

procedure TACBrPSPShipay.GetWallets;
var
  RespostaHttp: AnsiString;
  ResultCode: Integer;
  AURL: String;
  Ok: Boolean;
begin
  fWallets.Clear;
  PrepararHTTP;
  Ok := AcessarEndPoint(ChttpMethodGET, cShipayEndPointWallets, ResultCode, RespostaHttp);
  Ok := Ok and (ResultCode = HTTP_OK);

  if Ok then
  begin
    RespostaHttp := '{"wallets":'+RespostaHttp+'}';   // Transforma Array em Object
    fWallets.AsJSON := String(RespostaHttp)
  end
  else
  begin
    AURL := CalcularURLEndPoint(ChttpMethodGET, cShipayEndPointWallets);
    ACBrPixCD.DispararExcecao(EACBrPixHttpException.CreateFmt(
      sErroHttp,[Http.ResultCode, ChttpMethodPOST, AURL]));
  end;
end;

procedure TACBrPSPShipay.PostOrder;
var
  Body, AURL, ep: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
  Ok: Boolean;
begin
  Body := Trim(fOrder.AsJSON);
  if (Body = '') then
    ACBrPixCD.DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['Order']));

  if (LowerCase(fOrder.wallet) = cShipayWalletPix) then
  begin
    ep := cShipayEndPointOrderV;
    if (fOrder.expiration = 0) then
      fOrder.expiration := 3600;
  end
  else
    ep := cShipayEndPointOrder;

  fOrderCreated.Clear;
  PrepararHTTP;
  WriteStrToStream(Http.Document, Body);
  Http.MimeType := CContentTypeApplicationJSon;
  Ok := AcessarEndPoint(ChttpMethodPOST, ep, ResultCode, RespostaHttp);
  Ok := Ok and (ResultCode = HTTP_CREATED);

  if Ok then
    fOrderCreated.AsJSON := String(RespostaHttp)
  else
  begin
    AURL := CalcularURLEndPoint(ChttpMethodPOST, ep);
    ACBrPixCD.DispararExcecao(EACBrPixHttpException.CreateFmt(
      sErroHttp,[Http.ResultCode, ChttpMethodPOST, AURL]));
  end;
end;

procedure TACBrPSPShipay.DeleteOrder(const order_id: String);
var
  Body, AURL: String;
  RespostaHttp: AnsiString;
  ResultCode: Integer;
begin
  if (Trim(order_id) = '') then
    ACBrPixCD.DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroParametroInvalido), ['order_id']));

  Clear;
  PrepararHTTP;
  URLPathParams.Add(order_id);
  AcessarEndPoint(ChttpMethodDELETE, cShipayEndPointOrder, ResultCode, RespostaHttp);

  if (ResultCode = HTTP_OK) then
  begin
    fOrderInfo.AsJSON := String(RespostaHttp);
    if (fOrderInfo.order_id <> Trim(order_id)) then
        ACBrPixCD.DispararExcecao(EACBrPixException.Create(sErrOrderIdDifferent));

    if not (fOrderInfo.status in [spsCancelled, spsRefunded]) then
        ACBrPixCD.DispararExcecao(EACBrPixException.CreateFmt(
           ACBrStr(sErrOrderIdNotCancelled),
          [fOrderInfo.order_id, ShipayOrderStatusToString(fOrderInfo.status)]));
  end
  else
  begin
    AURL := CalcularURLEndPoint(ChttpMethodDELETE, cShipayEndPointOrder);
    ACBrPixCD.DispararExcecao(EACBrPixHttpException.CreateFmt(
      sErroHttp,[Http.ResultCode, ChttpMethodDELETE, AURL]));
  end;
end;

function TACBrPSPShipay.GetSecretKey: String;
begin
   Result := ClientSecret;
end;

procedure TACBrPSPShipay.SetSecretKey(AValue: String);
begin
  ClientSecret := AValue;
end;

procedure TACBrPSPShipay.ProcessarAutenticacao(const AURL: String;
  ResultCode: Integer; const RespostaHttp: AnsiString);
var
  js: TJsonObject;
begin
  Wallets.Clear;
  if (ResultCode = HTTP_OK) then
  begin
   {$IfDef USE_JSONDATAOBJECTS_UNIT}
    js := TJsonObject.Parse(RespostaHttp) as TJsonObject;
    try
      fpToken := js.S['access_token'];
      fRefreshToken := js.S['refresh_token'];
    finally
      js.Free;
    end;
   {$Else}
    js := TJsonObject.Create;
    try
      js.Parse(RespostaHttp);
      fpToken := js['access_token'].AsString;
      fRefreshToken := js['refresh_token'].AsString;
    finally
      js.Free;
    end;
   {$EndIf}

    if (Trim(fpToken) = '') then
      ACBrPixCD.DispararExcecao(EACBrPixHttpException.Create(ACBrStr(sErroAutenticacao)));

    fpValidadeToken := IncSecond(Now, 24);
    fpAutenticado := True;

    GetWallets;
    if (fWallets.Count < 1) then
      ACBrPixCD.DispararExcecao(EACBrPixHttpException.Create(sErrNoWallet));
  end
  else
    ACBrPixCD.DispararExcecao(EACBrPixHttpException.CreateFmt(
      sErroHttp,[Http.ResultCode, ChttpMethodPOST, AURL]));
end;

procedure TACBrPSPShipay.QuandoAcessarEndPoint(const AEndPoint: String;
  var AURL: String; var AMethod: String);
var
  Body, ep: String;
begin
  if (pos(UpperCase(AMethod), ChttpMethodPOST+','+ChttpMethodPUT) > 0) and (AEndPoint = cEndPointCob) then
  begin
    AMethod := ChttpMethodPOST;
    Body := ConverterJSONCobSolicitadaParaShipayOrder( String(StreamToAnsiString(Http.Document)) );
    if (LowerCase(fOrder.wallet) = cShipayWalletPix) then
      ep := cShipayEndPointOrderV
    else
      ep := cShipayEndPointOrder;

    AURL := StringReplace(AURL, cEndPointCob, ep, []);
    Http.Document.Clear;
    WriteStrToStream(Http.Document, Body);
  end;
end;

procedure TACBrPSPShipay.QuandoReceberRespostaEndPoint(const AEndPoint,
  AMethod: String; var AResultCode: Integer; var RespostaHttp: AnsiString);
var
  Body: String;
begin
  if (AResultCode = HTTP_OK) or (AResultCode = HTTP_CREATED) then
  begin
    if (UpperCase(AMethod) = ChttpMethodPOST) and (AEndPoint = cEndPointCob) then
    begin
      RespostaHttp := ConverterJSONOrderCreatedParaCobGerada( RespostaHttp );
      AResultCode := HTTP_CREATED;
    end;
  end;
end;

function TACBrPSPShipay.ConverterJSONCobSolicitadaParaShipayOrder(
  const CobSolicitadaJSON: String): String;
var
  Cob: TACBrPIXCobSolicitada;
  ia: TACBrPIXInfoAdicional;
  i: Integer;
  item: TShipayItem;
  s: String;
begin
  fOrder.Clear;
  Cob := TACBrPIXCobSolicitada.Create('');
  try
    Cob.AsJSON := CobSolicitadaJSON;

    fOrder.buyer.name := Cob.devedor.nome;
    fOrder.buyer.cpf_cnpj := IfEmptyThen(Cob.devedor.cnpj, Cob.devedor.cpf);
    fOrder.total := Cob.valor.original;
    fOrder.pix_dict_key := Cob.chave;
    fOrder.expiration := Cob.calendario.expiracao;

    ia := Cob.infoAdicionais.Find('order_ref');
    if (ia <> Nil) then
      fOrder.order_ref := ia.valor;
    ia := Cob.infoAdicionais.Find('wallet');
    if (ia <> Nil) then
      fOrder.wallet := ia.valor;

    i := 1;
    repeat
      s := 'item_'+IntToStr(i);
      ia := Cob.infoAdicionais.Find(s);
      if (ia <> Nil) then
      begin
        item := fOrder.items.New;
        try
          item.AsJSON := ia.valor;
        except
          On E: Exception do
          begin
            ACBrPixCD.RegistrarLog(Format('Erro na sintaxe de %s',[s]));
            ACBrPixCD.RegistrarLog(E.ClassName + ': ' + E.Message);
            fOrder.items.Remove(item);
          end;
        end;
        Inc(i);
      end;
    until (ia = Nil) ;

    // Chama Evento, para permitir ao usuário, informar a Wallet e os Items
    if Assigned(fQuandoEnviarOrder) then
      fQuandoEnviarOrder(fOrder);

    if (Trim(fOrder.order_ref) = '') then
      ACBrPixCD.DispararExcecao(EACBrPixException.Create(ACBrStr(sErrOrderRefNotInformed)));

    if (Trim(fOrder.wallet) = '') then
    begin
      // Não especificou Wallet, usando a única Wallet retornada ou "pix"
      if (fWallets.Count = 1) then
        fOrder.wallet := fWallets[0].wallet
      else
      begin
        for i := 0 to fWallets.Count-1 do
        begin
          if (fWallets[i].wallet = cShipayWalletPix) then  // Tem Pix ?
          begin
            fOrder.wallet := cShipayWalletPix;
            Break;
          end;
        end;
        if (Trim(fOrder.wallet) = '') then  // Não tem PIX, pegue a primeira da Lista
          fOrder.wallet := fWallets[0].wallet;
      end;
    end;

    if (fOrder.items.Count = 0) then
    begin
      // Não especificou Item em "QuandoEnviarOrder", criando um Item padrão
      with fOrder.items.New do
      begin
        sku := '00001';
        ean := '2'+ sku + '000000';  // 2 = in-store, Zeros = Total
        ean := ean + EAN13_DV(ean);
        item_title := cItemTitleNotInformed;
        quantity := 1;
        unit_price := fOrder.total;
      end;
    end;

    with fOrder.items.New do
    begin
      sku := 'ACBr';
      item_title := 'ACBrPIXCD';
      quantity := 0;
      unit_price := 0;
    end;

    if (fOrder.wallet <> cShipayWalletPix) then
      fOrder.expiration := 0
    else
    begin
      if (fOrder.expiration <= 0) then
        fOrder.expiration := 3600
    end;

    Result := fOrder.AsJSON;
  finally
    Cob.Free;
  end;
end;

function TACBrPSPShipay.ConverterJSONOrderCreatedParaCobGerada(
  const OrderCreatedJSON: String): String;
var
  Cob: TACBrPIXCobGerada;
  qrd: TACBrPIXQRCodeDinamico;
begin
  fOrderCreated.AsJSON := OrderCreatedJSON;
  Cob := TACBrPIXCobGerada.Create('');
  try
    Cob.calendario.criacao := Now;
    Cob.calendario.expiracao := SecondsBetween(Cob.calendario.criacao, fOrderCreated.expiration_date);
    Cob.chave := fOrderCreated.pix_dict_key;
    with Cob.infoAdicionais.New do
    begin
      nome := 'order_id';
      valor := fOrderCreated.order_id;
    end;
    with Cob.infoAdicionais.New do
    begin
      nome := 'wallet';
      valor := fOrderCreated.wallet;
    end;
    //with Cob.infoAdicionais.New do
    //begin
    //  nome := 'qr_code';
    //  valor := fOrderCreated.qr_code;   // infoAdicional.valor só aceita 200 chars;
    //end;

    case fOrderCreated.status of
      spsPending, spsPendingV:
        Cob.status := stcATIVA;
      spsApproved, spsRefunded, spsRefundPending:
        Cob.status := stcCONCLUIDA;
      spsCancelled, spsExpired:
        Cob.status := stcREMOVIDA_PELO_PSP
    else
      Cob.status := stcNENHUM;
    end;

    Cob.pixCopiaECola := fOrderCreated.qr_code_text;
    if (fOrderCreated.wallet = cShipayWalletPix) then
    begin
      with Cob.infoAdicionais.New do
      begin
        nome := 'pix_psp';
        valor := fOrderCreated.pix_psp;
      end;

      qrd := TACBrPIXQRCodeDinamico.Create;
      try
        qrd.AsString := fOrderCreated.qr_code_text;
        Cob.location := qrd.URL;
        Cob.loc.tipoCob := tcoCob;
        Cob.loc.location := qrd.URL;
      finally
        qrd.Free;
      end;
    end;

    Result := Cob.AsJSON;
  finally
    Cob.Free;
  end;
end;

function TACBrPSPShipay.ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): String;
begin
  if (ACBrPixCD.Ambiente = ambProducao) then
    Result := cShipayURLProducao
  else
    Result := cShipayURLStaging;
end;

end.


