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

unit ACBrPIXPSPMercadoPago;

interface

uses
  Classes, SysUtils,
  {$IFDEF RTL230_UP}ACBrBase,{$ENDIF RTL230_UP}
  ACBrPIXCD, ACBrPIXBase, ACBrPIXSchemasMercadoPago,
  ACBrPIXSchemasProblema, ACBrPIXSchemasCob, ACBrPIXSchemasDevolucao;

const
  cMercadoPagoURLProducao = ''; // URL removida a pedido do Mercado Pago
  cMercadoPagoEndPointPayment = '/v1/payments';
  cMercadoPagoEndPointRefund = '/refunds';
  cMercadoPagoEndPointSearch = '/search';
  cMercadoPagoXIdempotencyKey = 'X-Idempotency-Key: ';

type

  { TACBrPSPMercadoPago }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPSPMercadoPago = class(TACBrPSP)
  private
    FPagamentoAtualizado: TMercadoPagoPaymentUpdate;
    FPagamentoGerado: TMercadoPagoPayment;
    FPagamentosConsultados: TMercadoPagoConsultedPayments;
    FPagamentoSolicitado: TMercadoPagoPaymentRequest;
    FReembolsoGerado: TMercadoPagoRefund;
    FReembolsoSolicitado: TMercadoPagoRefundRequest;
    FResponseError: TMercadoPagoError;

    procedure QuandoReceberRespostaEndPoint(const AEndPoint, AURL, AMethod: string; var AResultCode: integer; var RespostaHttp: ansistring);
    procedure QuandoAcessarEndPoint(const aEndPoint: string; var aURL: string; var aMethod: string);

    function CobRevisadaParaPaymentUpdate: string;
    function CobSolicitadaParaPaymentRequest: string;
    function DevolucaoSolicitadaParaRefundRequest: string;
    function PaymentResponseParaCobGerada(const aPaymentResponseJSON: string): string;
    function PaymentResponseParaCobCompleta(const aPaymentResponseJSON: string): string;
    function PaymentResponseParaCobRevisada(const aPaymentResponseJSON: string): string;
    function PaymentResponseParaPixConsultado(const aPaymentResponseJSON: string): string;
    function ConsultedPaymentsParaCobsConsultadas(const aConsultedPaymentsJSON: string): string;
    function RefundResponseParaPixDevolvido(const aRefundResponseJSON: string): string;

    function CobStatusToPaymentStatus(aStatus: TACBrPIXStatusCobranca): TMercadoPagoPaymentStatus;
    function PaymentStatusToCobStatus(aStatus: TMercadoPagoPaymentStatus): TACBrPIXStatusCobranca;
    function RefundStatusToDevolucaoStatus(aStatus: TMercadoPagoRefundStatus): TACBrPIXStatusDevolucao;
  protected
    function ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): string; override;
    function CalcularEndPointPath(const {%H-}aMethod, aEndPoint: string): string; override;
    procedure ConfigurarBody(const aMethod, aEndPoint: string; var aBody: string); override;
    procedure ConfigurarHeaders(const Method, AURL: String); override;
    procedure ConfigurarPathParameters(const aMethod, aEndPoint: string); override;
    procedure ConfigurarQueryParameters(const aMethod, aEndPoint: string); override;
    procedure TratarRetornoComErro(ResultCode: integer; const RespostaHttp: ansistring; Problema: TACBrPIXProblema); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;

    property PagamentoSolicitado: TMercadoPagoPaymentRequest read FPagamentoSolicitado write FPagamentoSolicitado;
    property PagamentoGerado: TMercadoPagoPayment read FPagamentoGerado write FPagamentoGerado;
    property PagamentosConsultados: TMercadoPagoConsultedPayments read FPagamentosConsultados write FPagamentosConsultados;
    property PagamentoAtualizado: TMercadoPagoPaymentUpdate read FPagamentoAtualizado write FPagamentoAtualizado;

    property ReembolsoSolicitado: TMercadoPagoRefundRequest read FReembolsoSolicitado write FReembolsoSolicitado;
    property ReembolsoGerado: TMercadoPagoRefund read FReembolsoGerado write FReembolsoGerado;

    property ResponseError: TMercadoPagoError read FResponseError;
  published
    property AccessToken: string read fpToken write fpToken;
  end;

implementation

uses
  StrUtils, synautil, DateUtils, ACBrUtil.Strings, ACBrUtil.Base,
  ACBrUtil.FilesIO, ACBrPIXBRCode, ACBrPIXSchemasPix, ACBrOpenSSLUtils,
  ACBrPIXSchemasCobsConsultadas, ACBrPIXUtil;

{ TACBrPSPMercadoPago }

constructor TACBrPSPMercadoPago.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fpIsBacen := False;
  FResponseError := TMercadoPagoError.Create;

  FPagamentoGerado := TMercadoPagoPayment.Create;
  FPagamentoSolicitado := TMercadoPagoPaymentRequest.Create;
  FPagamentosConsultados := TMercadoPagoConsultedPayments.Create;
  FPagamentoAtualizado := TMercadoPagoPaymentUpdate.Create;

  FReembolsoGerado := TMercadoPagoRefund.Create;
  FReembolsoSolicitado := TMercadoPagoRefundRequest.Create;

  fpQuandoReceberRespostaEndPoint := QuandoReceberRespostaEndPoint;
  fpQuandoAcessarEndPoint := QuandoAcessarEndPoint;
end;

destructor TACBrPSPMercadoPago.Destroy;
begin
  FResponseError.Free;
  FPagamentoGerado.Free;
  FPagamentoSolicitado.Free;
  FPagamentosConsultados.Free;
  FPagamentoAtualizado.Free;
  FReembolsoGerado.Free;
  FReembolsoSolicitado.Free;
  inherited Destroy;
end;

procedure TACBrPSPMercadoPago.Clear;
begin
  inherited Clear;
  FResponseError.Clear;
  FPagamentoGerado.Clear;
  FPagamentoSolicitado.Clear;
  FPagamentosConsultados.Clear;
  FPagamentoAtualizado.Clear;
  FReembolsoGerado.Clear;
  FReembolsoSolicitado.Clear;
end;

procedure TACBrPSPMercadoPago.QuandoReceberRespostaEndPoint(
  const AEndPoint, AURL, AMethod: string; var AResultCode: integer;
  var RespostaHttp: ansistring);
var
  wMethod: string;
begin
  wMethod := UpperCase(AMethod);

  if (AResultCode <> HTTP_OK) and (AResultCode <> HTTP_CREATED) and
    (AResultCode <> HTTP_ACCEPTED) then
  begin
    ResponseError.AsJSON := RespostaHttp;
    Exit;
  end;

  if (AEndPoint = cEndPointPix) then
  begin
    if (wMethod = ChttpMethodGET) then
      RespostaHttp := PaymentResponseParaPixConsultado(RespostaHttp)
    else if (Pos(cMercadoPagoEndPointRefund, AURL) > 0) then
      RespostaHttp := RefundResponseParaPixDevolvido(RespostaHttp);
  end
  else if (AEndPoint = cEndPointCob) then
  begin 
    if (wMethod = ChttpMethodPUT) then
      RespostaHttp := PaymentResponseParaCobRevisada(RespostaHttp)
    else if (wMethod = ChttpMethodPOST) then
    begin
      RespostaHttp := PaymentResponseParaCobGerada(RespostaHttp);
      AResultCode := HTTP_CREATED;
    end
    else if (wMethod = ChttpMethodGET) then
    begin
      if (URLQueryParams.Count > 1) then
        RespostaHttp := ConsultedPaymentsParaCobsConsultadas(RespostaHttp)
      else
        RespostaHttp := PaymentResponseParaCobCompleta(RespostaHttp);
    end;
  end;
end;

procedure TACBrPSPMercadoPago.QuandoAcessarEndPoint(const aEndPoint: string;
  var aURL: string; var aMethod: string);
begin
  if ((aEndPoint = cEndPointPix) or (aEndPoint = cEndPointCob)) then
  begin
    if (aMethod = ChttpMethodPUT) then
      aMethod := ChttpMethodPOST
    else if (aMethod = ChttpMethodPATCH) then
      aMethod := ChttpMethodPUT
  end;
end;

function TACBrPSPMercadoPago.CobRevisadaParaPaymentUpdate: string;
begin
  FPagamentoGerado.Clear;
  FPagamentoSolicitado.Clear;
  FPagamentoAtualizado.Clear;
  FPagamentosConsultados.Clear;
  FPagamentoAtualizado.status := CobStatusToPaymentStatus(epCob.CobRevisada.status);
  FPagamentoAtualizado.transactionAmount := epCob.CobRevisada.valor.original;
  Result := FPagamentoAtualizado.AsJSON;
end;

function TACBrPSPMercadoPago.CobSolicitadaParaPaymentRequest: string;
begin
  FPagamentoGerado.Clear;
  FPagamentoSolicitado.Clear;
  FPagamentoAtualizado.Clear;
  FPagamentosConsultados.Clear;

  FPagamentoSolicitado.transactionAmount := epCob.CobSolicitada.valor.original;
  FPagamentoSolicitado.DateExpiration :=
    FormatDateTime('yyyy-mm-dd"T"hh:nn:ss.zzz"-"04:00',
    IncSecond(Now, epCob.CobSolicitada.calendario.expiracao));
  FPagamentoSolicitado.Payer.Email := 'teste@teste.com';
  // Mercado Pago obriga o payer.email mas teremos o email do pagador
  FPagamentoSolicitado.Payer.FirstName := epCob.CobSolicitada.devedor.nome;
  FPagamentoSolicitado.Payer.identification.number :=
    IfEmptyThen(epCob.CobSolicitada.devedor.cnpj, epCob.CobSolicitada.devedor.cpf);
  if NaoEstaVazio(epCob.CobSolicitada.devedor.cpf) then
    FPagamentoSolicitado.payer.identification.type_ := mitCPF
  else if NaoEstaVazio(epCob.CobSolicitada.devedor.cnpj) then
    FPagamentoSolicitado.payer.identification.type_ := mitCNPJ;
  Result := FPagamentoSolicitado.AsJSON;
end;

function TACBrPSPMercadoPago.DevolucaoSolicitadaParaRefundRequest: string;
begin
  FReembolsoGerado.Clear;
  FReembolsoSolicitado.Clear;

  if NaoEstaZerado(epPix.DevolucaoSolicitada.valor) then
    FReembolsoSolicitado.amount := epPix.DevolucaoSolicitada.valor;
  Result := FReembolsoSolicitado.AsJSON;
end;

function TACBrPSPMercadoPago.PaymentResponseParaCobGerada(const aPaymentResponseJSON: string): string;
var
  wCob: TACBrPIXCobGerada;
  wQRCode: TACBrPIXQRCodeDinamico;
begin
  FPagamentoGerado.AsJSON := aPaymentResponseJSON;
  wCob := TACBrPIXCobGerada.Create('');
  try
    wCob.calendario.criacao := FPagamentoGerado.dateCreated;
    wCob.calendario.expiracao :=
      SecondsBetween(wCob.calendario.criacao, FPagamentoGerado.dateExpiration);
    wCob.txId := FPagamentoGerado.id;
    wCob.status := PaymentStatusToCobStatus(FPagamentoGerado.status);
    wCob.pixCopiaECola := FPagamentoGerado.pointOfInteraction.transactionData.qrcode;
    wCob.valor.original := FPagamentoGerado.transactionAmount;

    wQRCode := TACBrPIXQRCodeDinamico.Create;
    try
      wQRCode.AsString := wCob.pixCopiaECola;
      wCob.location := wQRCode.URL;
      wCob.loc.tipoCob := tcoCob;
      wCob.loc.location := wQRCode.URL;
    finally
      wQRCode.Free;
    end;

    Result := wCob.AsJSON;
  finally
    wCob.Free;
  end;
end;

function TACBrPSPMercadoPago.PaymentResponseParaCobCompleta(const aPaymentResponseJSON: string): string;
var
  wCob: TACBrPIXCobCompleta;
  wQRCode: TACBrPIXQRCodeDinamico;
  I: Integer;
begin
  FPagamentoGerado.AsJSON := aPaymentResponseJSON;
  wCob := TACBrPIXCobCompleta.Create('');
  try
    wCob.calendario.criacao := FPagamentoGerado.dateCreated;
    wCob.calendario.expiracao :=
      SecondsBetween(wCob.calendario.criacao, FPagamentoGerado.dateExpiration);
    wCob.txId := FPagamentoGerado.id;
    wCob.status := PaymentStatusToCobStatus(FPagamentoGerado.status);
    wCob.pixCopiaECola := FPagamentoGerado.pointOfInteraction.transactionData.qrcode;
    wCob.valor.original := FPagamentoGerado.transactionAmount;

    if (FPagamentoGerado.status in [mpsApproved, mpsRefunded]) then
    with wCob.pix.New do
    begin
      txid := FPagamentoGerado.id;
      valor := FPagamentoGerado.transactionAmount;

      for I := 0 to FPagamentoGerado.refunds.Count-1 do
      with devolucoes.New do
      begin
        id := IntToStr(FPagamentoGerado.refunds[I].id);
        valor := FPagamentoGerado.refunds[I].amount;
        status := RefundStatusToDevolucaoStatus(FPagamentoGerado.refunds[I].status);
      end;
    end;

    wQRCode := TACBrPIXQRCodeDinamico.Create;
    try
      wQRCode.AsString := wCob.pixCopiaECola;
      wCob.location := wQRCode.URL;
      wCob.loc.tipoCob := tcoCob;
      wCob.loc.location := wQRCode.URL;
    finally
      wQRCode.Free;
    end;

    Result := wCob.AsJSON;
  finally
    wCob.Free;
  end;
end;

function TACBrPSPMercadoPago.PaymentResponseParaCobRevisada(const aPaymentResponseJSON: string): string;
var
  wCob: TACBrPIXCobRevisada;
begin
  FPagamentoGerado.AsJSON := aPaymentResponseJSON;
  wCob := TACBrPIXCobRevisada.Create('');
  try
    wCob.status := PaymentStatusToCobStatus(FPagamentoGerado.status);
    wCob.valor.original := FPagamentoGerado.transactionAmount;
    Result := wCob.AsJSON;
  finally
    wCob.Free;
  end;
end;

function TACBrPSPMercadoPago.PaymentResponseParaPixConsultado(const aPaymentResponseJSON: string): string;
var
  wPix: TACBrPIX;
  I: Integer;
begin
  FPagamentoGerado.AsJSON := aPaymentResponseJSON;
  wPix := TACBrPIX.Create('');
  try
    if (FPagamentoGerado.status in [mpsApproved, mpsRefunded]) then
    begin
      wPix.txid := FPagamentoGerado.id;
      wPix.valor := FPagamentoGerado.transactionAmount;

      for I := 0 to FPagamentoGerado.refunds.Count-1 do
      with wPix.devolucoes.New do
      begin
        id := IntToStr(FPagamentoGerado.refunds[I].id);
        valor := FPagamentoGerado.refunds[I].amount;
        status := RefundStatusToDevolucaoStatus(FPagamentoGerado.refunds[I].status);
      end;
    end;

    Result := wPix.AsJSON;
  finally
    wPix.Free;
  end;
end;

function TACBrPSPMercadoPago.ConsultedPaymentsParaCobsConsultadas(const aConsultedPaymentsJSON: string): string;
var
  wCobs: TACBrPIXCobsConsultadas;
  wQRCode: TACBrPIXQRCodeDinamico;
  I, J: Integer;
begin
  FPagamentosConsultados.AsJSON := aConsultedPaymentsJSON;
  wCobs := TACBrPIXCobsConsultadas.Create('');
  wQRCode := TACBrPIXQRCodeDinamico.Create;
  try
    for I := 0 to FPagamentosConsultados.results.Count-1 do
    begin
      with wCobs.cobs.New do
      begin
        calendario.criacao := FPagamentosConsultados.results[I].dateCreated;
        calendario.expiracao := SecondsBetween(calendario.criacao, FPagamentosConsultados.results[I].dateExpiration);
        txId := FPagamentosConsultados.results[I].id;
        status := PaymentStatusToCobStatus(FPagamentosConsultados.results[I].status);
        pixCopiaECola := FPagamentosConsultados.results[I].pointOfInteraction.transactionData.qrcode;
        valor.original := FPagamentosConsultados.results[I].transactionAmount; 

        if (FPagamentosConsultados.results[I].status in [mpsApproved, mpsRefunded]) then
        with pix.New do
        begin
          txid := FPagamentosConsultados.results[I].id;
          valor := FPagamentosConsultados.results[I].transactionAmount;

          for J := 0 to FPagamentosConsultados.results[I].refunds.Count-1 do
          with devolucoes.New do
          begin
            id := IntToStr(FPagamentosConsultados.results[I].refunds[J].id);
            valor := FPagamentosConsultados.results[I].refunds[J].amount;
            status := RefundStatusToDevolucaoStatus(FPagamentosConsultados.results[I].refunds[J].status);
          end;
        end;

        wQRCode.Clear;
        wQRCode.AsString := pixCopiaECola;
        location := wQRCode.URL;
        loc.tipoCob := tcoCob;
        loc.location := wQRCode.URL;
      end;
    end;

    Result := wCobs.AsJSON;
  finally
    wCobs.Free;
    wQRCode.Free;
  end;
end;

function TACBrPSPMercadoPago.RefundResponseParaPixDevolvido(const aRefundResponseJSON: string): string;
var
  wDevolucao: TACBrPIXDevolucao;
begin
  FReembolsoGerado.AsJSON := aRefundResponseJSON;
  wDevolucao := TACBrPIXDevolucao.Create('');
  try
    wDevolucao.id := IntToStr(FReembolsoGerado.id);
    wDevolucao.valor := FReembolsoGerado.amount;
    wDevolucao.horario.solicitacao := FReembolsoGerado.dataCreated;
    wDevolucao.status := RefundStatusToDevolucaoStatus(FReembolsoGerado.status);
    Result := wDevolucao.AsJSON;
  finally
    wDevolucao.Free;
  end;
end;

function TACBrPSPMercadoPago.CobStatusToPaymentStatus(aStatus: TACBrPIXStatusCobranca): TMercadoPagoPaymentStatus;
begin
  case aStatus of
    stcREMOVIDA_PELO_PSP, stcREMOVIDA_PELO_USUARIO_RECEBEDOR: Result := mpsCancelled;
    stcATIVA: Result := mpsPending;
    stcCONCLUIDA: Result := mpsApproved;
    else
      Result := mpsNone;
  end;
end;

function TACBrPSPMercadoPago.PaymentStatusToCobStatus(aStatus:
  TMercadoPagoPaymentStatus): TACBrPIXStatusCobranca;
begin
  case aStatus of
    mpsPending, mpsInProcess, mpsInMediation, mpsAuthorized: Result := stcATIVA;
    mpsApproved: Result := stcCONCLUIDA;
    mpsRejected: Result := stcREMOVIDA_PELO_PSP;
    mpsCancelled, mpsRefunded: Result := stcREMOVIDA_PELO_USUARIO_RECEBEDOR;
    else
      Result := stcNENHUM;
  end;
end;

function TACBrPSPMercadoPago.RefundStatusToDevolucaoStatus(aStatus:
  TMercadoPagoRefundStatus): TACBrPIXStatusDevolucao;
begin
  case aStatus of
    mrsApproved: Result := stdDEVOLVIDO;
    mrsInProcess: Result := stdEM_PROCESSAMENTO;
    mrsCancelled, mrsRejected: Result := stdNAO_REALIZADO;
  else
    Result := stdNENHUM;
  end;
end;

function TACBrPSPMercadoPago.ObterURLAmbiente(const Ambiente: TACBrPixCDAmbiente): string;
begin
  Result := cMercadoPagoURLProducao
end;

function TACBrPSPMercadoPago.CalcularEndPointPath(const aMethod, aEndPoint: string): string;
begin
  if (aEndPoint = cEndPointCob) or (aEndPoint = cEndPointPix) then
    Result := cMercadoPagoEndPointPayment;
end;

procedure TACBrPSPMercadoPago.ConfigurarBody(const aMethod, aEndPoint: string; var aBody: string);
begin
  if (aEndPoint = cEndPointPix) and (aMethod = ChttpMethodPUT) then
    aBody := DevolucaoSolicitadaParaRefundRequest
  else if (aEndPoint = cEndPointCob) and ((aMethod = ChttpMethodPUT) or (aMethod = ChttpMethodPOST)) then
    aBody := CobSolicitadaParaPaymentRequest
  else if (aEndPoint = cEndPointCob) and (aMethod = ChttpMethodPATCH) then
    aBody := CobRevisadaParaPaymentUpdate;
end;

procedure TACBrPSPMercadoPago.ConfigurarHeaders(const Method, AURL: String);
begin
  inherited ConfigurarHeaders(Method, AURL);

  if (Method = ChttpMethodPOST) and (Pos(cMercadoPagoEndPointPayment, AURL) > 0) then
    Http.Headers.Add(cMercadoPagoXIdempotencyKey + FormatarGUID(CriarTxId));
end;

procedure TACBrPSPMercadoPago.ConfigurarPathParameters(const aMethod, aEndPoint: string);
var
  wDevolucao: string;
begin
  if (aMethod = ChttpMethodPUT) and ((aEndPoint = cEndPointPix) or (aEndPoint = cEndPointCob)) then
  begin
    if (aEndPoint = cEndPointPix) then
    begin
      wDevolucao := URLPathParams[0];
      URLPathParams.Clear;
      URLPathParams.Add(wDevolucao);
      URLPathParams.Add(cMercadoPagoEndPointRefund);
    end
    else
      URLPathParams.Clear;
  end;

  if (aMethod = ChttpMethodGET) then
  begin
    URLPathParams.Text := StringReplace(URLPathParams.Text, 'devolucao', 'refunds', [rfReplaceAll]);

    if (URLQueryParams.Count > 1) then
      URLPathParams.Add(cMercadoPagoEndPointSearch);
  end;
end;

procedure TACBrPSPMercadoPago.ConfigurarQueryParameters(const aMethod, aEndPoint: string);
begin
  if (aEndPoint = cEndPointCob) and (aMethod = ChttpMethodGET) and (URLQueryParams.Count > 1) then
  begin
    while (URLQueryParams.Count > 2) do
      URLQueryParams.Delete(URLQueryParams.Count - 1);

    URLQueryParams.Text := StringReplace(URLQueryParams.Text, 'inicio', 'begin_date', [rfReplaceAll]);
    URLQueryParams.Text := StringReplace(URLQueryParams.Text, 'fim', 'end_date', [rfReplaceAll]);
    URLQueryParams.Insert(0, 'sort=date_created');
    URLQueryParams.Insert(1, 'criteria=asc');
  end;
end;

procedure TACBrPSPMercadoPago.TratarRetornoComErro(ResultCode: integer;
  const RespostaHttp: ansistring; Problema: TACBrPIXProblema);
begin
  if (NivelLog > 2) then
    RegistrarLog('TratarRetornoComErro( ' + IntToStr(ResultCode) + ' )');

  if (ResultCode = HTTP_UNAUTHORIZED) then
    fpAutenticado := False;

  Problema.Clear;
  if (Trim(RespostaHttp) = '') then
    AtribuirErroHTTPProblema(Problema)
  else
  begin
    try
      ResponseError.AsJSON := string(RespostaHttp);

      Problema.title := ResponseError.error;
      Problema.status := ResponseError.status;
      Problema.detail := ResponseError.message;
    except
    end;
  end;
end;

end.
