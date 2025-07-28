{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César                                                                }
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
  https://doc.appless.com.br

*) 

{$I ACBr.inc}

unit ACBrPIXPSPAppLess;

interface

uses
  Classes, SysUtils,
  {$IFDEF RTL230_UP}ACBrBase,{$ENDIF RTL230_UP}
  ACBrPIXCD, ACBrPIXBase, ACBrOpenSSLUtils, ACBrSchemasAppLess;

const
  cAppLessURLProducao     = 'https://api.appless.dev/pay/prd';
  cAppLessURLHomologacao  = 'https://api.appless.dev/pay/hml';
  cAppLessPathAuthToken   = '/auth';
  cAppLessEndpointOrder   = '/order';
  cAppLessEndpointOrders  = '/orders';
  cAppLessEndpointCancel  = '/cancel';
  cAppLessURLAuthHomolog  = cAppLessURLHomologacao+ cAppLessPathAuthToken;
  cAppLessURLAuthProducao = cAppLessURLProducao+ cAppLessPathAuthToken;

type

  { TACBrPSPAppLess }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPSPAppLess = class(TACBrPSPCertificate)
  private
    fHMAC: String;
    fOrderRequest: TACBrAppLessOrder;
    fOrderResponse: TACBrAppLessOrderResponse;
    fOrdersResponse: TACBrAppLessOrdersResponse;
    fBankSlipOrderRequest: TACBrAppLessBankSlipOrderRequest;
    function GetBankSlipOrderRequest: TACBrAppLessBankSlipOrderRequest;
    function GetOrderRequest: TACBrAppLessOrder;
    function GetOrderResponse: TACBrAppLessOrderResponse;
    function GetOrdersResponse: TACBrAppLessOrdersResponse;

    function CobSolicitadaToOrderRequest(const aJsonCobSolicitada: String): String;
    function CobVSolicitadaToBankSlipOrderRequest(const aJsonCobSolicitada: String): String;

    function OrderResponseToCob(const aJsonOrderResponse: String): String;
    function OrdersResponseToCobs(const aJsonOrdersResponse: String): String;

    function OrderStatusToCobStatus(aOrderStatus: TAppLessOrderStatus): TACBrPIXStatusCobranca;
    function CobStatusToOrderStatus(aCobStatus: TACBrPIXStatusCobranca): TAppLessOrderStatus;
             
    procedure DoQuandoAcessarEndPoint(const aEndPoint: String; var aURL: String; var aMethod: String);
    procedure DoQuandoReceberRespostaEndPoint(const aEndPoint, aURL, aMethod: String; var aResultCode: Integer; var aRespostaHttp: AnsiString);
  protected
    function ObterURLAmbiente(const aAmbiente: TACBrPixCDAmbiente): String; override;
    function CalcularEndPointPath(const aMethod, aEndPoint: String): String; override;

    procedure ConfigurarHeaders(const Method, AURL: String); override;
    procedure ConfigurarBody(const aMethod, aEndPoint: String; var aBody: String); override;
    procedure ConfigurarQueryParameters(const Method, EndPoint: String); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Autenticar; override;

    function PostOrder: Boolean;
    function GetOrder(const aOrderId: String): Boolean;

    property OrderRequest: TACBrAppLessOrder read GetOrderRequest;
    property BankSlipOrderRequest: TACBrAppLessBankSlipOrderRequest read GetBankSlipOrderRequest;

    property OrderResponse: TACBrAppLessOrderResponse read GetOrderResponse;
    property OrdersResponse: TACBrAppLessOrdersResponse read GetOrdersResponse;

  published
    property SecretKeyHMAC: String read fHMAC write fHMAC;
  end;

implementation

uses
  synautil, synacode, DateUtils, StrUtils,
  ACBrJSON,
  ACBrPIXUtil,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrPIXSchemasCob,
  ACBrPIXSchemasCobV,
  ACBrPIXSchemasCobsConsultadas;

{ TACBrPSPAppLess }

function TACBrPSPAppLess.GetOrderRequest: TACBrAppLessOrder;
begin
  if (not Assigned(fOrderRequest)) then
    fOrderRequest := TACBrAppLessOrder.Create('order');
  Result := fOrderRequest;
end;

function TACBrPSPAppLess.GetBankSlipOrderRequest: TACBrAppLessBankSlipOrderRequest;
begin
  if (not Assigned(fBankSlipOrderRequest)) then
    fBankSlipOrderRequest := TACBrAppLessBankSlipOrderRequest.Create;
  Result := fBankSlipOrderRequest;
end;

function TACBrPSPAppLess.GetOrderResponse: TACBrAppLessOrderResponse;
begin
  if (not Assigned(fOrderResponse)) then
    fOrderResponse := TACBrAppLessOrderResponse.Create;
  Result := fOrderResponse;
end;

function TACBrPSPAppLess.GetOrdersResponse: TACBrAppLessOrdersResponse;
begin
  if (not Assigned(fOrdersResponse)) then
    fOrdersResponse := TACBrAppLessOrdersResponse.Create;
  Result := fOrdersResponse;
end;

function TACBrPSPAppLess.CobSolicitadaToOrderRequest(const aJsonCobSolicitada: String): String;
var
  wCob: TACBrPIXCobSolicitada;
begin
  wCob := TACBrPIXCobSolicitada.Create;
  try
    wCob.AsJSON := aJsonCobSolicitada;

    OrderRequest.amount := wCob.valor.original;
    OrderRequest.customerCPF := IfThen(EstaVazio(wCob.devedor.cpf), wCob.devedor.cnpj, wCob.devedor.cpf);
    OrderRequest.custumerSocialName := wCob.devedor.nome;

    if (URLPathParams.Count = 1) then
      OrderRequest.externalId := URLPathParams[0]
    else
      OrderRequest.externalId := CriarTxId;
    URLPathParams.Clear;

    Result := OrderRequest.AsJSON;
  finally
    wCob.Free;
  end;
end;

function TACBrPSPAppLess.CobVSolicitadaToBankSlipOrderRequest(const aJsonCobSolicitada: String): String;
var
  wCobV: TACBrPIXCobVSolicitada;
begin
  wCobV := TACBrPIXCobVSolicitada.Create;
  try
    wCobV.AsJSON := aJsonCobSolicitada;

    BankSlipOrderRequest.order.typeOrder := 'bankSlip';
    BankSlipOrderRequest.order.amount := wCobV.valor.original;
    
    BankSlipOrderRequest.bankSlip.dueDate := wCobV.calendario.dataDeVencimento;
    BankSlipOrderRequest.bankSlip.limitDate := IncDay(wCobV.calendario.dataDeVencimento, wCobV.calendario.validadeAposVencimento);
    BankSlipOrderRequest.bankSlip.type_ := 99;  // ?

    BankSlipOrderRequest.bankSlip.custumer.name := wCobV.devedor.nome;
    BankSlipOrderRequest.bankSlip.custumer.cpf := wCobV.devedor.cpf;
    BankSlipOrderRequest.bankSlip.custumer.cnpj := wCobV.devedor.cnpj;
    BankSlipOrderRequest.bankSlip.custumer.address.street := wCobV.devedor.logradouro;
    BankSlipOrderRequest.bankSlip.custumer.address.zipCode := wCobV.devedor.cep;
    BankSlipOrderRequest.bankSlip.custumer.address.city := wCobV.devedor.cidade;
    BankSlipOrderRequest.bankSlip.custumer.address.state := wCobV.devedor.uf;

    BankSlipOrderRequest.bankSlip.interest.amount := wCobV.valor.juros.valorPerc;
    BankSlipOrderRequest.bankSlip.interest.id := IntToStr(Ord(wCobV.valor.juros.modalidade));

    BankSlipOrderRequest.bankSlip.fine.amount := wCobV.valor.multa.valorPerc;
    BankSlipOrderRequest.bankSlip.fine.id := IntToStr(Ord(wCobV.valor.multa.modalidade));

    BankSlipOrderRequest.bankSlip.discount.amount := wCobV.valor.desconto.valorPerc;
    BankSlipOrderRequest.bankSlip.discount.id := IntToStr(Ord(wCobV.valor.desconto.modalidade));
    if (wCobV.valor.desconto.modalidade in [pdmValorFixo, pdmPercentual]) and
        NaoEstaZerado(wCobV.valor.desconto.descontosDataFixa.Count) then
    begin
      BankSlipOrderRequest.bankSlip.discount.amount := wCobV.valor.desconto.descontosDataFixa[0].valorPerc;
      BankSlipOrderRequest.bankSlip.discount.date := wCobV.valor.desconto.descontosDataFixa[0].data;
    end;

    if (URLPathParams.Count = 1) then
      BankSlipOrderRequest.order.externalId := URLPathParams[0]
    else
      BankSlipOrderRequest.order.externalId := CriarTxId;
    URLPathParams.Clear;

    Result := BankSlipOrderRequest.AsJSON;
  finally
    wCobV.Free;
  end;
end;

function TACBrPSPAppLess.OrderResponseToCob(const aJsonOrderResponse: String): String;
var
  wCob: TACBrPIXCobCompleta;
  i: Integer;
begin
  OrderResponse.AsJSON := aJsonOrderResponse;
  wCob := TACBrPIXCobCompleta.Create;
  try
    wCob.IsBacen := False;
    wCob.calendario.criacao := OrderResponse.createdAt;
    wCob.txId := OrderResponse.orderId;
    wCob.status := OrderStatusToCobStatus(OrderResponse.status);
    wCob.pixCopiaECola := OrderResponse.transaction.transactionPix.cobResponse.pixCopiaECola;
    wCob.valor.original := OrderResponse.amount;

    with wCob.infoAdicionais.New do
    begin
      nome := 'externalId';
      valor := OrderResponse.externalId;
    end;

    with wCob.infoAdicionais.New do
    begin
      nome := 'transactionTxid';
      valor := OrderResponse.transaction.transactionPix.txid;
    end;

    with wCob.infoAdicionais.New do
    begin
      nome := 'urlPix';
      valor := OrderResponse.transaction.transactionPix.cobResponse.urlPix;
    end;

    wCob.pix.Assign(OrderResponse.transaction.transactionPix.cobResponse.pix);

    wCob.calendario.expiracao := OrderResponse.transaction.transactionPix.cobResponse.calendario.expiracao;
    Result := wCob.AsJSON;
  finally
    wCob.Free;
  end;
end;

function TACBrPSPAppLess.OrdersResponseToCobs(const aJsonOrdersResponse: String): String;
var
  wCobs: TACBrPIXCobsConsultadas;
  i: Integer;
  wCob: TACBrPIXCobCompleta;
  jsonOrder: String;
begin
  OrdersResponse.AsJSON := aJsonOrdersResponse;
  wCobs := TACBrPIXCobsConsultadas.Create;
  try
    wCobs.IsBacen := False;
    wCobs.parametros.paginacao.paginaAtual := OrdersResponse.skip;
    wCobs.parametros.paginacao.itensPorPagina := OrdersResponse.range;
    wCobs.parametros.paginacao.quantidadeTotalDeItens := OrdersResponse.count;
    for i := 0 to (OrdersResponse.results.Count - 1) do
    begin
      jsonOrder := OrderResponseToCob(OrdersResponse.results[i].AsJSON);
      wCob := TACBrPIXCobCompleta.Create;
      wCob.AsJSON := jsonOrder;
      wCobs.cobs.Add(wCob);
    end;

    Result := wCobs.AsJSON;
  finally
    wCobs.Free;
  end;
end;

function TACBrPSPAppLess.OrderStatusToCobStatus(aOrderStatus: TAppLessOrderStatus): TACBrPIXStatusCobranca;
begin
  case aOrderStatus of
    aosWaiting: Result := stcATIVA;
    aosError: Result := stcREMOVIDA_PELO_PSP;
    aosPayed, aosRefunding, aosRefunded: Result := stcCONCLUIDA;
    aosCanceled: Result := stcREMOVIDA_PELO_USUARIO_RECEBEDOR;
  else
    Result := stcNENHUM;
  end;
end;

function TACBrPSPAppLess.CobStatusToOrderStatus(aCobStatus: TACBrPIXStatusCobranca): TAppLessOrderStatus;
begin
  case aCobStatus of
    stcATIVA: Result := aosWaiting;
    stcCONCLUIDA: Result := aosPayed;
    stcREMOVIDA_PELO_PSP: Result := aosCanceled;
    stcREMOVIDA_PELO_USUARIO_RECEBEDOR: Result := aosCanceled;
  else
    Result := aosNone;
  end;
end;

procedure TACBrPSPAppLess.DoQuandoAcessarEndPoint(const aEndPoint: String; var aURL: String; var aMethod: String);
begin
  if (aEndPoint = cEndPointCob) and (aMethod = ChttpMethodPATCH) or (aMethod = ChttpMethodPUT) then
    aMethod := ChttpMethodPOST;
end;

procedure TACBrPSPAppLess.DoQuandoReceberRespostaEndPoint(const aEndPoint, aURL, aMethod: String;
  var aResultCode: Integer; var aRespostaHttp: AnsiString);
var
  ja: TACBrJSONArray;
  Cancelamento: Boolean;
begin
  if (aResultCode <> HTTP_OK) and (aResultCode <> HTTP_CREATED) then
  begin
    if NaoEstaZerado(Pos('[', aRespostaHttp)) then
    begin
      ja := TACBrJSONArray.Parse(aRespostaHttp);
      try
        if NaoEstaZerado(ja.Count) then
          aRespostaHttp := ja.ItemAsJSONObject[0].ToJSON;
      finally
        ja.Free;
      end;
    end;
    Exit;
  end;

  if (AEndPoint = cEndPointCob) or (AEndPoint = cEndPointCobV) then
  begin
    Cancelamento := (Pos(cAppLessEndpointCancel, aURL) > 0);
    if (aMethod = ChttpMethodPOST) and (aResultCode = HTTP_OK) and (not Cancelamento) then
      aResultCode := HTTP_CREATED;

    if (aMethod = ChttpMethodPOST) or (aMethod = ChttpMethodGET) then
    begin
      if NaoEstaZerado(URLQueryParams.Count) then
        aRespostaHttp := OrdersResponseToCobs(aRespostaHttp)
      else
        aRespostaHttp := OrderResponseToCob(aRespostaHttp);
    end;
  end;
end;

function TACBrPSPAppLess.ObterURLAmbiente(const aAmbiente: TACBrPixCDAmbiente): String;
begin
  if (aAmbiente = ambProducao) then
    Result := cAppLessURLProducao
  else
    Result := cAppLessURLHomologacao;
end;

function TACBrPSPAppLess.CalcularEndPointPath(const aMethod, aEndPoint: String): String;
begin
  Result := Trim(aEndPoint);

  if (aEndPoint = cEndPointCob) or (aEndPoint = cEndPointCobV) then
  begin
    if NaoEstaZerado(URLQueryParams.Count) then
      Result := cAppLessEndpointOrders
    else
      Result := cAppLessEndpointOrder;

    if (aMethod = ChttpMethodPATCH) then
      Result := Result + cAppLessEndpointCancel;
  end;
end;

procedure TACBrPSPAppLess.ConfigurarHeaders(const Method, AURL: String);
var
  ts, s, wHash: String;
  wOpenSSL: TACBrOpenSSLUtils;
begin
  inherited ConfigurarHeaders(Method, AURL);
  //ts := IntToStr(DateTimeToUnix(ACBrUtil.DateTime.DateTimeUniversal(ACBrUtil.DateTime.GetUTCSistema, Now)) * 1000);
  ts := IntToStr(DateTimeToUnixMilliseconds(Now, False));

  s := ts + ClientID;
  wOpenSSL := TACBrOpenSSLUtils.Create(nil);
  try
    wHash := wOpenSSL.HMACFromString(s, fHMAC, algSHA256, sttBase64);
    Http.Headers.Add('X-Timestamp: ' + ts);
    Http.Headers.Add('X-Signature: ' + wHash);
    Http.Headers.Add('X-ClientId: ' + ClientID);
  finally
    wOpenSSL.Free;
  end;
end;

procedure TACBrPSPAppLess.ConfigurarBody(const aMethod, aEndPoint: String; var aBody: String);
begin
  if (aEndPoint = cEndPointCob) and ((aMethod = ChttpMethodPOST) or (aMethod = ChttpMethodPUT)) then
    aBody := CobSolicitadaToOrderRequest(aBody);

  if (aEndPoint = cEndPointCobV) and (aMethod = ChttpMethodPOST) then
    aBody := CobVSolicitadaToBankSlipOrderRequest(aBody);

  if (aEndPoint = cEndPointCob) and (aMethod = ChttpMethodPATCH) then
    aBody := EmptyStr;
end;

procedure TACBrPSPAppLess.ConfigurarQueryParameters(const Method, EndPoint: String);
var
  I: Integer;
  wSL: TStringList;
  wName, wValue: String;
  s: TAppLessOrderStatus;
begin
  if (URLQueryParams.Count <= 0) then
    Exit;

  wSL := TStringList.Create;
  try
    for I := 0 to URLQueryParams.Count - 1 do
    begin
      wName := LowerCase(URLQueryParams.Names[I]);
      wValue:= URLQueryParams.Values[wName];

      // Ignora parâmetros não utilizados pela AppLess
      if (Pos(wName, 'cpf,cnpj,locationPresente') > 0) then
        Continue;

      if (wName = 'inicio') then
        wSL.Values['initDTCreate'] := wValue
      else if (wName = 'fim') then
        wSL.Values['endDTCreate'] := wValue
      else if (wName = 'status') then
      begin
        s := CobStatusToOrderStatus(StringToPIXStatusCobranca(wValue));
        if (s <> aosNone) then
          wSL.Values['typeStatus'] := IntToStr(AppLessOrderStatusToInteger(s));
      end
      else if (wName = 'paginacao.itensporpagina') then
        wSL.Values['range'] := wValue
      else if (wName = 'paginacao.paginaatual') then
        wSL.Values['skip'] := wValue
      else
        Continue;

      RegistrarLog('Parametro(Query) alterado: ' + URLQueryParams[I] + ' => ' + wSL[wSL.Count-1]);
    end;

    if (wSL.Count > 0) then
      URLQueryParams.Text := wSL.Text;
  finally
    wSL.Free;
  end;
end;

constructor TACBrPSPAppLess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fpIsBacen := False;
  fpQuandoAcessarEndPoint := DoQuandoAcessarEndPoint;
  fpQuandoReceberRespostaEndPoint := DoQuandoReceberRespostaEndPoint;
end;

destructor TACBrPSPAppLess.Destroy;
begin
  if Assigned(fOrderRequest) then
    fOrderRequest.Free;
  if Assigned(fOrderResponse) then
    fOrderResponse.Free;
  if Assigned(fOrdersResponse) then
    fOrdersResponse.Free;
  if Assigned(fBankSlipOrderRequest) then
    fBankSlipOrderRequest.Free;
  inherited Destroy;
end;

procedure TACBrPSPAppLess.Autenticar;
var
  wURL: String;
  wRespostaHttp: AnsiString;
  wResultCode: Integer;
  wBody, js: TACBrJSONObject;
begin
  VerificarPIXCDAtribuido;
  LimparHTTP;

  if (ACBrPixCD.Ambiente = ambProducao) then
    wURL := cAppLessURLAuthProducao
  else
    wURL := cAppLessURLAuthHomolog;

  wBody := TACBrJSONObject.Create;
  try
    wBody
      .AddPair('clientId', ClientID)
      .AddPair('clientSecret', ClientSecret);
    WriteStrToStream(Http.Document, wBody.ToJSON);
  finally
    wBody.Free;
  end;

  TransmitirHttp(ChttpMethodPOST, wURL, wResultCode, wRespostaHttp);

  if (wResultCode = HTTP_OK) then
  begin
    js := TACBrJSONObject.Parse(wRespostaHttp);
    try
      fpToken := js.AsString['token'];
      fpValidadeToken := js.AsISODateTime['expires'];
    finally
      js.Free;
    end;

    if (Trim(fpToken) = EmptyStr) then
      DispararExcecao(EACBrPixHttpException.Create(ACBrStr(sErroAutenticacao)));

    fpAutenticado := True;
  end
  else
    DispararExcecao(EACBrPixHttpException.CreateFmt(sErroHttp, [Http.ResultCode, ChttpMethodPOST, wURL]));
end;

function TACBrPSPAppLess.PostOrder: Boolean;
var
  wBody: String;
  wResponse: AnsiString;
  wResultCode: Integer;
begin
  RegistrarLog('PostOrder');           
  if (not Assigned(fOrderRequest)) or fOrderRequest.IsEmpty then
    DispararExcecao(EACBrPixException.CreateFmt(ACBrStr(sErroObjetoNaoPrenchido), ['OrderRequest']));
        
  Clear;
  PrepararHTTP;
  wBody := Trim(fOrderRequest.AsJSON);
  WriteStrToStream(Http.Document, wBody);
  Http.MimeType := CContentTypeApplicationJSon;

  Result := AcessarEndPoint(ChttpMethodPOST, cAppLessEndpointOrder, wResultCode, wResponse);
  Result := Result and (wResultCode = HTTP_OK);

  if Result then
    OrderResponse.AsJSON := String(wResponse)
  else
    raise Exception.Create('Erro');
end;

function TACBrPSPAppLess.GetOrder(const aOrderId: String): Boolean;
var
  wResponse: AnsiString;
  wResultCode: Integer;
begin
  Result := False;
  RegistrarLog('GetOrder - OrderID: ' + aOrderId);
  if EstaVazio(aOrderId) then
    Exit;

  Clear;
  PrepararHTTP;
  Result := AcessarEndPoint(ChttpMethodGET, cAppLessEndpointOrder, wResultCode, wResponse);
  Result := Result and (wResultCode = HTTP_OK);

  if Result then
    OrderResponse.AsJSON := String(wResponse)
  else
    raise Exception.Create('Erro');
end;

end.

