{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

// https://aditum.com.br
// https://developers.aditum.com.br/tef?section=instalacao

unit ACBrTEFAPIAditum;

interface

uses
  Classes, SysUtils,
  ACBrBase, ACBrJSON, ACBrSocket, httpsend,
  ACBrTEFComum, ACBrTEFAPI, ACBrTEFAPIComum;

const
  CPoolSleep = 1000;
  CEsperaMsg = 3000;

  CAditumAPIVersion = 'v2';
  CAditumAPIURL = 'https://localhost:4090/'+CAditumAPIVersion+'/';
  CAditumTEF = 'TEF Aditum';

  CPaymentType_CREDIT = 'Credit';
  CPaymentType_DEBIT = 'Debit';
  CPaymentType_VOUCHER = 'Voucher';

  CInstallmentType_Emissor = 'Issuer';
  CInstallmentType_Estabelecimento = 'Merchant';


resourcestring
  sACBrAditumNaoAtivo = CAditumTEF+' não está ativo';
  sACBrAditumSemActivationCode = '"Activation Code" deve ser informado em "DadosTerminal.CodTerminal"';
  sACBrAditumSemPartnerToken = '"Partner Token" deve ser informado em "DadosTerminal.CodEmpresa"';
  sACBrAditumSemNomeAplicacao = 'Nome da Aplicação deve ser informado em "DadosAutomacao.NomeAplicacao"';
  sACBrAditumSemVersaoAplicacao = 'Versão da Aplicação deve ser informado em "DadosAutomacao.VersaoAplicacao"';
  sACBrAditumErroModalidadeNaoSuportada = 'Modalidade %s não suportada em '+CAditumTEF;
  sACBrAditumCNPJEstabelecimentoDiferente = 'CNPJ informado em "DadosEstabelecimento.CNPJ": %s'+sLineBreak+'é diferente do retornado pelo '+CAditumTEF+': %s';
  sACBrAditumErroDadoNaoEJSon = 'Dado informado em %s não é um JSON válido';
  sACBrAditumErroEstabelecimentoNaoAtivo = 'Estabelecimento %s não está Ativo';
  sACBrAditumSemNSU = 'NSU não informado';
  sACBrAditumNaoAprovada = 'Transação %s não aprovada';
  sACBrAditumSemComunicacaoURL = 'Falha na comunicação. Erro: %d, URL: %s';


type

  { TACBrTEFRespAditum }

  TACBrTEFRespAditum = class( TACBrTEFResp )
  public
    procedure ConteudoToProperty; override;
  end;

  { TACBrTEFAPIClassAditum }

  TACBrTEFAPIClassAditum = class(TACBrTEFAPIClass)
  private
    FavailableBrands: TStringList;
    FHostApplicationVersion: String;
    FHTTP: THTTPSend;
    FURL: String;
    FHTTPResponse: AnsiString;
    FHTTPResultCode: Integer;

    procedure GravarLog(const AString: AnsiString);
    procedure DoException(const AErrorMsg: String);
    procedure LimparRespostaHTTP;
    procedure TransmitirHttp(const AMethod, AEndpoint: String; const ABody: AnsiString = '');
    procedure TransmitirHttpComEspera(const AMethod, AEndpoint: String; const ABody: AnsiString = '');
    procedure TratarRetornoComErro;

    function TransactionStatusDescription(const ATransactionStatus: String): String;
    function DadoPinPadToMessageType(TipoDado: TACBrTEFAPIDadoPinPad): String;
    function GetErrorCode(AJSonObject: TACBrJSONObject): Integer;

  protected
    procedure InterpretarRespostaAPI; override;
    procedure CarregarRespostasPendentes(const AListaRespostasTEF: TACBrTEFAPIRespostas); override;

  public
    constructor Create(AACBrTEFAPI: TACBrTEFAPIComum);
    destructor Destroy; override;

    procedure Inicializar; override;
    procedure DesInicializar; override;
    procedure Autenticar; override;

    function EfetuarPagamento(
      ValorPagto: Currency;
      Modalidade: TACBrTEFModalidadePagamento = tefmpNaoDefinido;
      CartoesAceitos: TACBrTEFTiposCartao = [];
      Financiamento: TACBrTEFModalidadeFinanciamento = tefmfNaoDefinido;
      Parcelas: Byte = 0;
      DataPreDatado: TDateTime = 0;
      DadosAdicionais: String = ''): Boolean; override;

    function EfetuarAdministrativa(
      OperacaoAdm: TACBrTEFOperacao = tefopAdministrativo): Boolean; overload; override;
    function EfetuarAdministrativa(
      const CodOperacaoAdm: string = ''): Boolean; overload; override;

    function CancelarTransacao(
      const NSU, CodigoAutorizacaoTransacao: string;
      DataHoraTransacao: TDateTime;
      Valor: Double;
      const CodigoFinalizacao: string = '';
      const Rede: string = ''): Boolean; override;
    procedure RecuperarTransacao(const NSU: String);

    procedure FinalizarTransacao(
      const Rede, NSU, CodigoFinalizacao: String;
      AStatus: TACBrTEFStatusTransacao = tefstsSucessoAutomatico); override;

    procedure ResolverTransacaoPendente(
      AStatus: TACBrTEFStatusTransacao = tefstsSucessoManual); override;
    procedure AbortarTransacaoEmAndamento; override;

    procedure ExibirMensagemPinPad(const MsgPinPad: String); override;
    function ObterDadoPinPad(TipoDado: TACBrTEFAPIDadoPinPad; TimeOut: integer = 30000;
      MinLen: SmallInt = 0; MaxLen: SmallInt = 0): String; override;
    function MenuPinPad(const Titulo: String; Opcoes: TStrings; TimeOut: Integer = 30000
      ): Integer; override;
    function VerificarPresencaPinPad: Byte; override;
    function VersaoAPI: String; override;

    procedure Deactivation;
    function Status: Boolean;

    property AvailableBrands: TStringList read FavailableBrands;
    property HostApplicationVersion: String read FHostApplicationVersion;
    property HTTPResultCode: Integer read FHTTPResultCode;
    property HTTPResponse: AnsiString read FHTTPResponse;
    property URL: String read FURL;
  end;


implementation

uses
  Math, DateUtils, TypInfo,
  synautil, synacode,
  ACBrUtil.Strings;

{ TACBrTEFRespAditum }

procedure TACBrTEFRespAditum.ConteudoToProperty;
var
  js, jsCharge: TACBrJSONObject;
  i: Integer;
  s: String;
  jsReceipt: TACBrJSONArray;
  isApproved, isCanceled: Boolean;
begin
  fpCNFEnviado := (UpperCase(Conteudo.LeInformacao(899,1).AsString) = 'S');
  fpHeader := Conteudo.LeInformacao(899,100).AsString;
  s := Conteudo.LeInformacao(899,200).AsString;
  js := TACBrJSONObject.Parse(s);
  try
    jsCharge := js.AsJSONObject['charge'];

    if not Assigned(jsCharge) then
      jsCharge := js; // fallback para raiz do JSON

    if Assigned(jsCharge) then
    begin
      Rede := jsCharge.AsString['acquirerName'];
      CodigoRedeAutorizada := jsCharge.AsString['aid'];
      ValorTotal := jsCharge.AsFloat['amount']/100;
      CodigoAutorizacaoTransacao := jsCharge.AsString['authorizationCode'];
      NSU_TEF := jsCharge.AsString['authorizationResponseCode'];
      ValorOriginal := jsCharge.AsFloat['balance']/100;
      NomeAdministradora := jsCharge.AsString['brand'];
      DataHoraTransacaoCancelada := jsCharge.AsISODateTime['cancelationDateTime'];
      DataHoraTransacaoHost := jsCharge.AsISODateTime['captureDateTime'];
      DataHoraTransacaoLocal := jsCharge.AsISODateTime['creationDateTime'];
      NFCeSAT.DonoCartao := jsCharge.AsString['cardHolderName'];
      BIN := jsCharge.AsString['cardNumber'];

      jsReceipt := jsCharge.AsJSONArray['cardholderReceipt'];
      ImagemComprovante1aVia.Clear;
      for i := 0 to jsReceipt.Count-1 do
        ImagemComprovante1aVia.Add(jsReceipt.Items[i]);

      jsReceipt := jsCharge.AsJSONArray['merchantReceipt'];
      ImagemComprovante2aVia.Clear;
      for i := 0 to jsReceipt.Count-1 do
        ImagemComprovante2aVia.Add(jsReceipt.Items[i]);

      QtdParcelas := jsCharge.AsInteger['installmentNumber'];
      isApproved := (LowerCase(jsCharge.AsString['isApproved']) = 'true');
      isCanceled := (LowerCase(jsCharge.AsString['isCanceled']) = 'true');
      Sucesso := isApproved or isCanceled;
      Confirmar := (fpHeader = 'CRT') and isApproved and (not isCanceled);

      s := LowerCase(jsCharge.AsString['installmentType']);
      if (s = 'merchant') then
        ParceladoPor := parcLoja
      else if (s = 'issuer') then
        ParceladoPor := parcADM
      else
        ParceladoPor := parcNenhum;

      Debito := False; Credito := False; Voucher := False;
      s := LowerCase(jsCharge.AsString['paymentType']);
      if (s = 'debit') then
        Debito := True
      else if (s = 'credit') then
        Credito := True
      else if (s = 'voucher') then
        Voucher := True;

      if Credito and (QtdParcelas > 1) then
        TipoOperacao := opParcelado
      else
        TipoOperacao := opAvista;

      Digitado := (LowerCase(jsCharge.AsString['isCaptured']) <> 'true');
      DocumentoVinculado := jsCharge.AsString['merchantChargeId'];
      NSU_TEF := jsCharge.AsString['merchantTransactionId'];
      NSU := jsCharge.AsString['nsu'];
      SerialPOS := jsCharge.AsString['origin'];

      NFCeSAT.Autorizacao := NSU;
      NFCeSAT.Bandeira := NomeAdministradora;
      NFCeSAT.UltimosQuatroDigitos := copy(BIN, Length(BIN)-3, 4);
    end;
  finally
    js.Free;
  end;
end;

{ TACBrTEFAPIClassAditum }

constructor TACBrTEFAPIClassAditum.Create(AACBrTEFAPI: TACBrTEFAPIComum);
begin
  inherited;
  FHTTP := THTTPSend.Create;
  FavailableBrands := TStringList.Create;
  fpTEFRespClass := TACBrTEFRespAditum;
  FHostApplicationVersion := '';
  LimparRespostaHTTP;
end;

destructor TACBrTEFAPIClassAditum.Destroy;
begin
  FHTTP.Free;
  FavailableBrands.Free;
  inherited Destroy;
end;

procedure TACBrTEFAPIClassAditum.Inicializar;
begin
  if (fpACBrTEFAPI.DadosTerminal.CodTerminal = '') then
    DoException(ACBrStr(sACBrAditumSemActivationCode));

  if (fpACBrTEFAPI.DadosTerminal.CodEmpresa = '') then
    DoException(ACBrStr(sACBrAditumSemPartnerToken));

  if (fpACBrTEFAPI.DadosAutomacao.NomeAplicacao = '') then
    DoException(ACBrStr(sACBrAditumSemNomeAplicacao));

  if (fpACBrTEFAPI.DadosAutomacao.VersaoAplicacao = '') then
    DoException(ACBrStr(sACBrAditumSemVersaoAplicacao));

  if (fpACBrTEFAPI.DadosTerminal.EnderecoServidor = '') then
    fpACBrTEFAPI.DadosTerminal.EnderecoServidor := CAditumAPIURL
  else
    fpACBrTEFAPI.DadosTerminal.EnderecoServidor := URLWithDelim(fpACBrTEFAPI.DadosTerminal.EnderecoServidor);

  AbortarTransacaoEmAndamento;
  inherited Inicializar;
end;

procedure TACBrTEFAPIClassAditum.DesInicializar;
begin
  try
    Deactivation;
  except
  end;

  inherited DesInicializar;
end;

procedure TACBrTEFAPIClassAditum.GravarLog(const AString: AnsiString);
begin
  fpACBrTEFAPI.GravarLog(AString);
end;

procedure TACBrTEFAPIClassAditum.DoException(const AErrorMsg: String);
begin
  fpACBrTEFAPI.DoException(AErrorMsg);
end;

procedure TACBrTEFAPIClassAditum.LimparRespostaHTTP;
begin
  FHTTP.Clear;
  FHTTPResultCode := 0;
  FHTTPResponse := '';
  FURL := '';
end;

procedure TACBrTEFAPIClassAditum.TransmitirHttp(const AMethod,
  AEndpoint: String; const ABody: AnsiString);
var
  ReTransmitir, ReAutenticar: Boolean;
  js: TACBrJSONObject;
  jserrors: TACBrJSONArray;
  i, t: Integer;
begin
  t := 0;
  ReTransmitir := True;
  while ReTransmitir do
  begin
    ReTransmitir := False;
    inc(t);
    LimparRespostaHTTP;

    FURL := EncodeURL( fpACBrTEFAPI.DadosTerminal.EnderecoServidor + AEndpoint );
    GravarLog('TransmitirHttp( '+AMethod+', '+FURL+', '+ABody+' )');

    FHTTP.Headers.Add('Authorization: '+fpACBrTEFAPI.DadosTerminal.CodEmpresa);
    if (ABody <> '') then
    begin
      FHTTP.MimeType := CContentTypeApplicationJSon;
      WriteStrToStream(FHTTP.Document, ABody);
    end;

    try
      FHTTP.HTTPMethod(AMethod, FURL);
    finally
      FHTTPResultCode := FHTTP.ResultCode;
      FHTTP.Document.Position := 0;
      FHTTPResponse := ReadStrFromStream(FHTTP.Document, FHTTP.Document.Size);
      GravarLog('  ResultCode: '+IntToStr(FHTTPResultCode)+', Response: '+FHTTPResponse);
    end;

    if (FHTTPResultCode <> HTTP_OK) and (pos('pinpad/init', FURL) = 0) and (t < 3) then
    begin
      ReAutenticar := false;

      try
        js := TACBrJSONObject.Parse(FHTTPResponse);
        try
          jserrors := js.AsJSONArray['errors'];
          if Assigned(jserrors) then
          begin
            for i := 0 to jserrors.Count-1 do
            begin
              if (GetErrorCode(jserrors.ItemAsJSONObject[i]) = -1000) then
              begin
                ReAutenticar := True;
                Break;
              end;
            end;
          end;
        finally
          js.free;
        end;
      except
      end;

      if ReAutenticar then
      begin
        Autenticar;
        ReTransmitir := True;
      end;
    end;
  end;
end;

procedure TACBrTEFAPIClassAditum.TransmitirHttpComEspera(const AMethod,
  AEndpoint: String; const ABody: AnsiString);
var
  Fim: Boolean;
  js, jsCharge: TACBrJSONObject;
  jsErrors: TACBrJSONArray;
  sMsg: String;
  TefAPI: TACBrTEFAPI;
  Espera: Integer;
begin
  TefAPI := TACBrTEFAPI(fpACBrTEFAPI);
  Fim := False;
  sMsg := '';
  while not Fim do
  begin
    TransmitirHttp(AMethod, AEndpoint, ABody);
    if (FHTTPResultCode <> HTTP_OK) then
      TratarRetornoComErro;

    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      jsCharge := js.AsJSONObject['charge'];
      jsErrors := js.AsJSONArray['errors'];
      Fim := Assigned(jsCharge) or (Assigned(jsErrors) and (jsErrors.Count > 0));

      sMsg := UTF8ToNativeString(js.AsString['operatorMessage']);
      if (not Fim) and (sMsg = '') then
        sMsg := ACBrStr(TransactionStatusDescription(js.AsString['status']));

      if Fim then
        Espera := CEsperaMsg
      else
        Espera := -1;

      if (sMsg <> '') then
        if Assigned(TefAPI.QuandoExibirMensagem) then
          TefAPI.QuandoExibirMensagem( sMsg, telaOperador, Espera );
    finally
      js.Free;
    end;

    if not Fim then
      Sleep(CPoolSleep);
  end;

  if (sMsg <> '') then
    if Assigned(TefAPI.QuandoExibirMensagem) then
      TefAPI.QuandoExibirMensagem( '', telaOperador, -1 );  // Remove Msg
end;

procedure TACBrTEFAPIClassAditum.TratarRetornoComErro;
var
  js, jserro: TACBrJSONObject;
  sMessage, eDescription, eMessage: String;
  jserrors: TACBrJSONArray;
  i, eCode: Integer;
begin
  sMessage := '';
  eCode := 0;
  try
    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      jserrors := js.AsJSONArray['errors'];
      if Assigned(jserrors) then
      begin
        for i := 0 to jserrors.Count-1 do
        begin
          jserro := jserrors.ItemAsJSONObject[i];
          eCode := GetErrorCode(jserrors.ItemAsJSONObject[i]);
          eDescription := jserro.AsString['description'];
          eMessage := jserro.AsString['message'];

          sMessage := sMessage + Format('%d - %s',[eCode, eDescription]);
          if (LowerCase(eMessage) <> 'null') then
            sMessage := sMessage + sLineBreak + eMessage;

          sMessage := sMessage + sLineBreak;
        end;
      end;
    finally
      js.free;
    end;
  except
  end;

  if (eCode = 0) then
    eCode := FHTTPResultCode;

  if (sMessage = '') then
    sMessage := Trim(FHTTPResponse)
  else
    sMessage := Trim(sMessage);

  if (eCode = 0) and (sMessage = '') then
    sMessage := Format(sACBrAditumSemComunicacaoURL, [0, FURL]);

  DoException(sMessage);
end;

function TACBrTEFAPIClassAditum.TransactionStatusDescription(
  const ATransactionStatus: String): String;
begin
  if ATransactionStatus = 'STARTING_TRANSACTION' then
    Result := 'Iniciando transação'
  else if ATransactionStatus = 'CHARGING_TABLES' then
    Result := 'Carregado parâmetros EMV no pinpad'
  else if ATransactionStatus = 'CHECK_CARD_EVENT' then
    Result := 'Aguardando inserção de cartão'
  else if ATransactionStatus = 'WAITING_TYPED_CARD' then
    Result := 'Aguardando entrada manual do cartão'
  else if ATransactionStatus = 'GETTING_PIN' then
    Result := 'Aguardando inserção de PIN'
  else if ATransactionStatus = 'PROCESSING_ONLINE' then
    Result := 'Enviando transação ao gateway Aditum'
  else if ATransactionStatus = 'PROCESSING_REVERSION' then
    Result := 'Transação negada pelo cartão, enviando reversão da transação'
  else if ATransactionStatus = 'WAITING_REMOVE_CARD' then
    Result := 'Aguardando remoção do cartão'
  else if ATransactionStatus = 'FINISHING_TRANSACTION' then
    Result := 'Encerrando fluxo transacional'
  else if ATransactionStatus = 'FINISHED' then
    Result := 'Fluxo completo'
  else if ATransactionStatus = 'VALIDATING_NSU' then
    Result := 'Validando NSU'
  else if ATransactionStatus = 'WAITING_CARD_EVENT' then
    Result := 'Aguardando Aproximação do Cartão'

  else
    Result := '';
end;

function TACBrTEFAPIClassAditum.DadoPinPadToMessageType(
  TipoDado: TACBrTEFAPIDadoPinPad): String;
begin
  case TipoDado of
    dpDDD: Result := 'DDD';
    dpRedDDD: Result := 'DDD_RETYPE';
    dpFone: Result := 'PHONE';
    dpRedFone: Result := 'PHONE_RETYPE';
    dpDDDeFone: Result := 'DDD_AND_PHONE';
    dpRedDDDeFone: Result := 'DDD_AND_PHONE_RETYPE';
    dpCPF: Result := 'CPF';
    dpRedCPF: Result := 'CPF_RETYPE';
    dpRG: Result := 'RG';
    dpRedRG: Result := 'RG_RETYPE';
    dp4UltDigitos: Result := 'LAST_FOUR_DIGITS';
    dpCodSeguranca: Result := 'CVV';
    dpCNPJ: Result := 'CNPJ';
    dpRedCNPJ: Result := 'CNPJ_RETYPE';
    dpMesMM: Result := 'MONTH';
    dpAnoAA: Result := 'YEAR';
    dpIdentificacao: Result := 'ENTER_ID';
    dpCodFidelidade: Result := 'LOYALTY_CODE';
    dpNumeroGuiche: Result := 'CHECKOUT_COUNTER';
    dpCodVendedor: Result := 'SELLER_CODE';
    dpNumeroNotaFiscal: Result := 'INVOICE_NUMBER';
    dpNumeroComanda: Result := 'ORDER_NUMBER';
    dpToken: Result := 'TOKEN';
    dpNumeroCartao: Result := 'CARD_NUMBER';
    dpCodigoPlano: Result := 'PLAN_CODE';
    dpCodigoProduto: Result := 'PRODUCT_CODE';
  else
    Result := '';
  end;
end;

function TACBrTEFAPIClassAditum.GetErrorCode(AJSonObject: TACBrJSONObject): Integer;
begin
  Result := AJSonObject.AsInteger['errorCode'];
  if (Result = 0) then
    Result := AJSonObject.AsInteger['code'];
end;

procedure TACBrTEFAPIClassAditum.Autenticar;
var
  js, jspinpadMessages, jsMerchantInfo, jsHostInfo: TACBrJSONObject;
  sBody, cnpjTEF: String;
  jsa: TACBrJSONArray;
  i: Integer;
begin
  GravarLog('Autenticar');
  LimparRespostaHTTP;
  try
    jspinpadMessages := TACBrJSONObject.Parse(fpACBrTEFAPI.DadosTerminal.ParamComunicacao);
  except
    DoException(Format(ACBrStr(sACBrAditumErroDadoNaoEJSon), ['DadosTerminal.ParamComunicacao']));
  end;

  js := TACBrJSONObject.Create;
  try
    js.AddPair('applicationName', fpACBrTEFAPI.DadosAutomacao.NomeAplicacao );
    js.AddPair('applicationVersion', fpACBrTEFAPI.DadosAutomacao.VersaoAplicacao );
    js.AddPair('activationCode', fpACBrTEFAPI.DadosTerminal.CodTerminal );
    if not jspinpadMessages.ValueExists('mainMessage') then
      jspinpadMessages.AddPair('mainMessage', fpACBrTEFAPI.DadosEstabelecimento.RazaoSocial);

    js.AddPair('pinpadMessages', jspinpadMessages);
    sBody := js.ToJSON;
  finally
    js.free;
  end;

  TransmitirHttp(cHTTPMethodPOST, 'pinpad/init', sBody);
  if (FHTTPResultCode = HTTP_OK) then
  begin
    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      if (LowerCase(js.AsString['success']) <> 'true') then
        TratarRetornoComErro;

      jsHostInfo := js.AsJSONObject['hostInfo'];
      if Assigned(jsHostInfo) then
        FHostApplicationVersion := jsHostInfo.AsString['applicationVersion'];

      jsMerchantInfo := js.AsJSONObject['merchantInfo'];
      if Assigned(jsMerchantInfo) then
      begin
        cnpjTEF := OnlyNumber(jsMerchantInfo.AsString['merchantDocument']);
        if (OnlyNumber(fpACBrTEFAPI.DadosEstabelecimento.CNPJ) <> cnpjTEF) then
          DoException(ACBrStr(Format(sACBrAditumCNPJEstabelecimentoDiferente,
                               [fpACBrTEFAPI.DadosEstabelecimento.CNPJ, cnpjTEF]) ));

        if (LowerCase(jsMerchantInfo.AsString['isActive']) <> 'true') then
          DoException(ACBrStr( Format(sACBrAditumErroEstabelecimentoNaoAtivo, [cnpjTEF])));
      end;

      FavailableBrands.Clear;
      jsa := js.AsJSONArray['availableBrands'];
      for i := 0 to jsa.Count-1 do
        FavailableBrands.Add(jsa.Items[i]);
    finally
      js.Free;
    end;
  end
  else
    TratarRetornoComErro;
end;

procedure TACBrTEFAPIClassAditum.Deactivation;
begin
  GravarLog('Deactivation');
  LimparRespostaHTTP;

  TransmitirHttp(cHTTPMethodGET, 'pinpad/deactivation');
  if (FHTTPResultCode = HTTP_OK) then
    FavailableBrands.Clear
  else
    TratarRetornoComErro;
end;

function TACBrTEFAPIClassAditum.Status: Boolean;
var
  js: TACBrJSONObject;
begin
  GravarLog('Status');
  LimparRespostaHTTP;

  TransmitirHttp(cHTTPMethodGET, 'status');
  if (FHTTPResultCode = HTTP_OK) then
  begin
    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      Result := (LowerCase(js.AsString['status']) = 'initialized');
    finally
      js.Free;
    end;
  end
  else
    TratarRetornoComErro;
end;

procedure TACBrTEFAPIClassAditum.InterpretarRespostaAPI;
begin
  with fpACBrTEFAPI.UltimaRespostaTEF do
  begin
    Clear;
    Conteudo.GravaInformacao(899,200,FHTTPResponse);
    DocumentoVinculado := fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao;
    AtualizarHeader;
    ConteudoToProperty;
  end;
end;

procedure TACBrTEFAPIClassAditum.CarregarRespostasPendentes(
  const AListaRespostasTEF: TACBrTEFAPIRespostas);
var
  js, jscharge: TACBrJSONObject;
  i: Integer;
  jsCharges: TACBrJSONArray;
  RespTEFPendente: TACBrTEFResp;
begin
  AListaRespostasTEF.CarregarRespostasDoDiretorioTrabalho;
  i := 0;
  while i < AListaRespostasTEF.Count do
  begin
    RespTEFPendente := AListaRespostasTEF[i];
    if not RespTEFPendente.CNFEnviado then   // Transações não confirmadas, serão carregadas abaixo, pelo "charge/pending"
      AListaRespostasTEF.ApagarRespostaTEF(i)
    else
      Inc(i);
  end;

  TransmitirHttp(cHTTPMethodGET, 'charge/pending?currentPage=1&pageSize=100');
  if (FHTTPResultCode = HTTP_OK) then
  begin
    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      jsCharges := js.AsJSONArray['charges'];
      if Assigned(jsCharges) then
      begin
        for i := 0 to jsCharges.Count-1 do
        begin
          RespTEFPendente := TACBrTEFRespAditum.Create;
          try
            jscharge := jsCharges.ItemAsJSONObject[i];
            RespTEFPendente.Conteudo.GravaInformacao(899,100,'CRT');
            RespTEFPendente.Conteudo.GravaInformacao(899,200, '{"charge": '+jscharge.ToJSON+'}');
            RespTEFPendente.ConteudoToProperty;
            AListaRespostasTEF.AdicionarRespostaTEF(RespTEFPendente); // Cria Clone interno
          finally
            RespTEFPendente.Free;
          end;
        end;
      end;
    finally
      js.Free;
    end;
  end
  else
    TratarRetornoComErro;
end;

function TACBrTEFAPIClassAditum.EfetuarPagamento(ValorPagto: Currency;
  Modalidade: TACBrTEFModalidadePagamento; CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento; Parcelas: Byte;
  DataPreDatado: TDateTime; DadosAdicionais: String): Boolean;
var
  js, jsCharge: TACBrJSONObject;
  jsErrors: TACBrJSONArray;
  sBody, sPaymentType, sInstallmentType, sMerchantChargeId: String;
  i: Integer;
  Fim: Boolean;
begin
  if not (Modalidade in [tefmpCartao]) then
    DoException(ACBrStr(Format(sACBrAditumErroModalidadeNaoSuportada,
       [GetEnumName(TypeInfo(TACBrTEFModalidadePagamento), integer(Modalidade) )])));

  if not Status then
    DoException(ACBrStr(sACBrAditumNaoAtivo));

  LimparRespostaHTTP;

  if CartoesAceitos = [] then
    CartoesAceitos := [teftcCredito];

  if (teftcCredito in CartoesAceitos) then
    sPaymentType := CPaymentType_CREDIT
  else if (teftcDebito in CartoesAceitos) then
    sPaymentType := CPaymentType_DEBIT
  else if (teftcVoucher in CartoesAceitos) then
    sPaymentType := CPaymentType_VOUCHER
  else
    sPaymentType := '';

  case Financiamento of
    tefmfParceladoEmissor: sInstallmentType := CInstallmentType_Emissor;
    tefmfParceladoEstabelecimento: sInstallmentType := CInstallmentType_Estabelecimento;
  else
    sInstallmentType := '';
  end;

  try
    js := TACBrJSONObject.Parse(DadosAdicionais);
  except
    DoException(Format(ACBrStr(sACBrAditumErroDadoNaoEJSon), ['DadosAdicionais']));
  end;

  try
    js.AddPair('amount', Trunc(ValorPagto*100) );
    if (sPaymentType <> '') then
     js.AddPair('paymentType', sPaymentType);
    if (sInstallmentType <> '') then
     js.AddPair('installmentType', sinstallmentType);
    if (Parcelas > 0) then
      js.AddPair('installmentNumber', Parcelas);

    if js.ValueExists('merchantChargeId') then
      sMerchantChargeId := js.AsString['merchantChargeId']
    else
    begin
      sMerchantChargeId := FormatDateTime('yymmddhhnnss', Now);
      js.AddPair('merchantChargeId', sMerchantChargeId);
    end;

    if not js.ValueExists('origin') then
      js.AddPair('origin', 'ACBr');

    sBody := js.ToJSON;
  finally
    js.free;
  end;

  Fim := False;
  while not Fim do
  begin
    TransmitirHttpComEspera(cHTTPMethodPOST, 'charge/authorization', sBody);

    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      jsCharge := js.AsJSONObject['charge'];
      jsErrors := js.AsJSONArray['errors'];
      Fim := Assigned(jsCharge) or (Assigned(jsErrors) and (jsErrors.Count > 0));

      if Fim then
      begin
        for i := 0 to jsErrors.Count-1 do
        begin
          if (GetErrorCode(jserrors.ItemAsJSONObject[i]) = 13) then  // OPERAÇÃO (anterior) CANCELADA
          begin
            Fim := True;
            Break;
          end;
        end;
      end;

      if Fim then
      begin
        if Assigned(jsCharge) then
          if (sMerchantChargeId <> Trim(jsCharge.AsString['merchantChargeId'])) then  // Resposta de Transação não é mesma que foi a enviada, ignore...
            Fim := False;
      end;
    finally
      js.Free;
    end;
  end;

  Result := (FHTTPResultCode = HTTP_OK);
  if Result then
  begin
    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      Result := (LowerCase(js.AsString['success']) = 'true');
    finally
      js.Free;
    end;
  end;

  if not Result then
    TratarRetornoComErro;
end;

function TACBrTEFAPIClassAditum.EfetuarAdministrativa(
  OperacaoAdm: TACBrTEFOperacao): Boolean;
var
  sl: TStringList;
  ItemSel: Integer;
begin
  LimparRespostaHTTP;
  Result := False;

  if (OperacaoAdm = tefopAdministrativo) then
  begin
    sl := TStringList.Create;
    try
      sl.Add('Teste PinPad');
      sl.Add(ACBrStr('Reimpressão'));
      ItemSel := -1;
      TACBrTEFAPI(fpACBrTEFAPI).QuandoPerguntarMenu( 'Menu Administrativo', sl, ItemSel );
      if (ItemSel = 0) then
        OperacaoAdm := tefopTesteComunicacao
      else if (ItemSel = 1) then
        OperacaoAdm := tefopReimpressao
      else
        Exit;
    finally
      sl.Free;
    end;
  end;

  if (OperacaoAdm = tefopTesteComunicacao) then
  begin
    VerificarPresencaPinPad;
    Result := True;
  end
  else if (OperacaoAdm = tefopReimpressao) then
  begin
    if (fpACBrTEFAPI.UltimaRespostaTEF.NSU = '') then
      DoException(ACBrStr(sACBrAditumSemNSU))
    else
      RecuperarTransacao(fpACBrTEFAPI.UltimaRespostaTEF.NSU);
    Result := True;
  end;

  if not Result then
    DoException(ACBrStr(Format(sACBrAditumErroModalidadeNaoSuportada,
      ['EfetuarAdministrativa( '+GetEnumName(TypeInfo(TACBrTEFOperacao), integer(OperacaoAdm) )+' )'])));
end;

function TACBrTEFAPIClassAditum.EfetuarAdministrativa(
  const CodOperacaoAdm: string): Boolean;
begin
  Result := EfetuarAdministrativa( TACBrTEFOperacao(StrToIntDef(CodOperacaoAdm, 0)) );
end;

function TACBrTEFAPIClassAditum.CancelarTransacao(const NSU,
  CodigoAutorizacaoTransacao: string; DataHoraTransacao: TDateTime;
  Valor: Double; const CodigoFinalizacao: string; const Rede: string): Boolean;
var
  js: TACBrJSONObject;
  sBody, sURL: String;
  jsErrors: TACBrJSONArray;
  i: Integer;
  jserro: TACBrJSONObject;
  sMsg : String;
begin
  LimparRespostaHTTP;
  if (Trim(NSU) = '') then
    DoException(ACBrStr(sACBrAditumSemNSU));

  js := TACBrJSONObject.Create;
  try
    js.AddPair('requireCard', True);
    sBody := js.ToJSON;
  finally
    js.free;
  end;

  sURL := 'charge/cancelation/by-nsu/'+Trim(NSU);
  TransmitirHttpComEspera(cHTTPMethodGET, sURL, sBody);
  Result := (FHTTPResultCode = HTTP_OK);
  if Result then
  begin
    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      Result := (LowerCase(js.AsString['canceled']) = 'true');
      if not Result then
      begin
        jsErrors := js.AsJSONArray['errors'];
        if Assigned(jsErrors) then
        begin
          for i := 0 to jsErrors.Count-1 do
          begin
            jserro := jserrors.ItemAsJSONObject[i];
            if Assigned(jserro) and (GetErrorCode(jserro) = -1950) then  // Transação já cancelada
            begin
              sMsg := UTF8ToNativeString(jserro.AsString['message']);
              TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirMensagem( sMsg, telaOperador, CEsperaMsg );
              Result := True;
              Break;
            end;
          end;
        end;
      end;
    finally
      js.Free;
    end;
  end;

  if not Result then
    TratarRetornoComErro;
end;

procedure TACBrTEFAPIClassAditum.RecuperarTransacao(const NSU: String);
var
  sURL, s: String;
  js: TACBrJSONObject;
  OK, sucesso, aprovado: Boolean;
begin
  LimparRespostaHTTP;
  sURL := 'charge/by-nsu/' + Trim(NSU);
  TransmitirHttp(cHTTPMethodGET, sURL );
  OK := (FHTTPResultCode = HTTP_OK);
  if OK then
  begin
    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      s := js.AsString['success'];
      sucesso := (LowerCase(s) = 'true');
      s := js.AsString['isApproved'];
      aprovado := (LowerCase(s) = 'true');

      if sucesso and aprovado then
        FinalizarChamadaAPI
      else if not aprovado then
        DoException(Format(ACBrStr(sACBrAditumNaoAprovada), [NSU]))
      else
        TratarRetornoComErro;
    finally
      js.Free;
    end;
  end;

  if not Ok then
    TratarRetornoComErro;
end;

procedure TACBrTEFAPIClassAditum.FinalizarTransacao(const Rede, NSU,
  CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
var
  sURL: String;
  js: TACBrJSONObject;
  Confirmar, Ok: Boolean;
  jsErrors: TACBrJSONArray;
  i: Integer;
begin
  LimparRespostaHTTP;
  Confirmar := (AStatus in [tefstsSucessoAutomatico, tefstsSucessoManual]);
  if Confirmar then
    sURL := 'charge/confirmation/'
  else
    sURL := 'charge/reversal/by-nsu/';

  sURL := sURL + Trim(NSU);
  if Confirmar then
    TransmitirHttp(cHTTPMethodGET, sURL)
  else
    TransmitirHttpComEspera(cHTTPMethodGET, sURL);

  OK := (FHTTPResultCode = HTTP_OK);
  if OK then
  begin
    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      if Confirmar then
        Ok := (LowerCase(js.AsString['confirmed']) = 'true') and (LowerCase(js.AsString['success']) = 'true')
      else
      begin
        Ok := (LowerCase(js.AsString['canceled']) = 'true');
        if not Ok then
        begin
          jsErrors := js.AsJSONArray['errors'];
          if Assigned(jsErrors) then
          begin
            for i := 0 to jsErrors.Count-1 do
            begin
              if (GetErrorCode(jserrors.ItemAsJSONObject[i]) = -1950) then  // Transação já cancelada
              begin
                Ok := True;
                Break;
              end;
            end;
          end;
        end;
      end;
    finally
      js.Free;
    end;
  end;

  if not Ok then
    TratarRetornoComErro;
end;

procedure TACBrTEFAPIClassAditum.ResolverTransacaoPendente(
  AStatus: TACBrTEFStatusTransacao);
begin
  if fpACBrTEFAPI.UltimaRespostaTEF.Confirmar then
  begin
    FinalizarTransacao( fpACBrTEFAPI.UltimaRespostaTEF.Rede,
                        fpACBrTEFAPI.UltimaRespostaTEF.NSU,
                        fpACBrTEFAPI.UltimaRespostaTEF.Finalizacao,
                        AStatus );
  end;
end;

procedure TACBrTEFAPIClassAditum.AbortarTransacaoEmAndamento;
var
  Ok: Boolean;
begin
  TransmitirHttp(cHTTPMethodGET, 'abort');
  Ok := (FHTTPResultCode = HTTP_OK);
  if not Ok then
    TratarRetornoComErro;
end;

procedure TACBrTEFAPIClassAditum.ExibirMensagemPinPad(
  const MsgPinPad: String);
const
  CPINPAD_COL = 16;
  CPINPAD_LIN = 2;
var
  Ok: Boolean;
  s: String;
begin
  LimparRespostaHTTP;
  s := StringReplace(MsgPinPad, '|', #10, [rfReplaceAll]);
  s := AjustaLinhas(s, CPINPAD_COL, CPINPAD_LIN, True);
  s := StringReplace(s, #10, '', [rfReplaceAll]);

  TransmitirHttp(cHTTPMethodGET, 'pinpad/display?message='+s);
  Ok := (FHTTPResultCode = HTTP_OK);
  if not Ok then
    TratarRetornoComErro;
end;

function TACBrTEFAPIClassAditum.ObterDadoPinPad(
  TipoDado: TACBrTEFAPIDadoPinPad; TimeOut: integer; MinLen: SmallInt;
  MaxLen: SmallInt): String;
var
  js, jsInput: TACBrJSONObject;
  sBody, sMessageType: String;
begin
  Result := '';
  sMessageType := DadoPinPadToMessageType(TipoDado);
  if (sMessageType = '') then
  begin
    fpACBrTEFAPI.DoException(Format(ACBrStr(sACBrTEFAPICapturaNaoSuportada),
      [GetEnumName(TypeInfo(TACBrTEFAPIDadoPinPad), integer(TipoDado) ), ClassName] ));
  end;

  if (MinLen = 0) and (MaxLen = 0) then
    CalcularTamanhosCampoDadoPinPad(TipoDado, MinLen, MaxLen);

  js := TACBrJSONObject.Create;
  try
    jsInput := TACBrJSONObject.Create;
    jsInput.AddPair('minSize', MinLen);
    jsInput.AddPair('maxSize', MaxLen);
    jsInput.AddPair('messageType',sMessageType);
    js.AddPair('input', jsInput);
    js.AddPair('timeout', Trunc(TimeOut/1000));
    sBody := js.ToJSON;
  finally
    js.free;
  end;

  TransmitirHttp(cHTTPMethodPOST, 'pinpad/input/getcleardata', sBody);
  if (FHTTPResultCode = HTTP_OK) then
  begin
    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      if (LowerCase(js.AsString['success']) = 'true') then
        Result := js.AsString['output'];
    finally
      js.Free;
    end;
  end
  else
    TratarRetornoComErro;
end;

function TACBrTEFAPIClassAditum.MenuPinPad(const Titulo: String;
  Opcoes: TStrings; TimeOut: Integer): Integer;
var
  js, jsMenu: TACBrJSONObject;
  s, sBody: String;
  jsoptions: TACBrJSONArray;
  i: Integer;
begin
  Result := 0;
  js := TACBrJSONObject.Create;
  try
    jsMenu := TACBrJSONObject.Create;
    jsMenu.AddPair('title', Titulo);
    jsoptions := TACBrJSONArray.Create;
    for i := 0 to Opcoes.Count-1 do
      jsoptions.AddElement(Opcoes[i]);
    jsMenu.AddPair('options', jsoptions);
    js.AddPair('menu', jsMenu);
    js.AddPair('timeout', Trunc(TimeOut/1000));
    sBody := js.ToJSON;
  finally
    js.free;
  end;

  TransmitirHttp(cHTTPMethodPOST, 'pinpad/input/datapicker', sBody);
  if (FHTTPResultCode = HTTP_OK) then
  begin
    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      if (LowerCase(js.AsString['success']) = 'true') then
      begin
        s := js.AsString['selected'];
        Result := StrToIntDef(s, -1);
      end;
    finally
      js.Free;
    end;
  end
  else
    TratarRetornoComErro;
end;

function TACBrTEFAPIClassAditum.VerificarPresencaPinPad: Byte;
var
  js, jsTerminalInfo: TACBrJSONObject;
begin
  Status;
  Result := 0;
  js := TACBrJSONObject.Parse(FHTTPResponse);
  try
    jsTerminalInfo := js.AsJSONObject['terminalInfo'];
    if Assigned(jsTerminalInfo) then
    begin
      if (LowerCase(js.AsString['connected']) = 'true') then
        Result := 99;   // TEF Aditum não retorna a Porta do PinPad, retornando algo diferente de 0
    end;
  finally
    js.Free;
  end;
end;

function TACBrTEFAPIClassAditum.VersaoAPI: String;
begin
  Result := FHostApplicationVersion;
end;

end.


