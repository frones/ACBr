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

type

  { TACBrTEFRespAditum }

  TACBrTEFRespAditum = class( TACBrTEFResp )
  public
    procedure ConteudoToProperty; override;
    procedure ProcessarTipoInterno(ALinha: TACBrTEFLinha); override;

  end;

  { TACBrTEFAPIClassAditum }

  TACBrTEFAPIClassAditum = class(TACBrTEFAPIClass)
  private
    FavailableBrands: TStringList;
    FHTTP: THTTPSend;
    FHTTPResponse: AnsiString;
    FHTTPResultCode: Integer;

    procedure GravarLog(const AString: AnsiString);
    procedure DoException(const AErrorMsg: String);
    procedure LimparRespostaHTTP;
    procedure TransmitirHttp(const AMethod, AEndpoint: String; const ABody: AnsiString = '');
    procedure TratarRetornoComErro;

    function TransactionStatusDescription(const ATransactionStatus: String): String;
    function DadoPinPadToMessageType(TipoDado: TACBrTEFAPIDadoPinPad): String;

  protected
    procedure InterpretarRespostaAPI; override;

  public
    constructor Create(AACBrTEFAPI: TACBrTEFAPIComum);
    destructor Destroy; override;

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

    procedure Deactivation;
    function Status: Boolean;

    property AvailableBrands: TStringList read FavailableBrands;
    property HTTPResultCode: Integer read FHTTPResultCode;
    property HTTPResponse: AnsiString read FHTTPResponse;
  end;


implementation

uses
  Math, DateUtils, TypInfo,
  synautil, synacode,
  ACBrUtil.Strings;

{ TACBrTEFRespAditum }

procedure TACBrTEFRespAditum.ConteudoToProperty;
var
  js, jsreceipt, jscard: TACBrJSONObject;
  i: Integer;
  s: String;
begin
  fpCNFEnviado := (UpperCase(Conteudo.LeInformacao(899,1).AsString) = 'S');
  fpHeader := Conteudo.LeInformacao(899,100).AsString;
  s := Conteudo.LeInformacao(899,200).AsString;
  js := TACBrJSONObject.Parse(s);
  try
    Rede := 'STONE';
    Confirmar := False;
    jsreceipt := js.AsJSONObject['receipt'];
    if Assigned(jsreceipt) then
    begin
      NSU := jsreceipt.AsString['acquirerTransactionKey'];
      //Finalizacao := jsreceipt.AsString['aid'];
      ValorTotal := jsreceipt.AsCurrency['amount'];
      Trailer := jsreceipt.AsString['arqc'];                         // estranho ?
      ValorEntradaCDC := jsreceipt.AsCurrency['availableBalance'];   // estranho ?
      Instituicao := jsreceipt.AsString['brandName'];
      NFCeSAT.DonoCartao := Trim(jsreceipt.AsString['cardholderName']);
      NSU_TEF := jsreceipt.AsString['initiatorTransactionKey'];
      TipoParcelamento := jsreceipt.AsInteger['installmentType'];
      case TipoParcelamento of
        2: ParceladoPor := parcLoja;
        3: ParceladoPor := parcADM;
      else
        ParceladoPor := parcNenhum;
      end;

      QtdParcelas := jsreceipt.AsInteger['totalNumberOfPayments'];
      BIN := Trim(jsreceipt.AsString['maskedPrimaryAccountNumber']);
      //SerialPOS := jsreceipt.AsString['systemSpecifications'];    // estranho ?
      DataHoraTransacaoHost := jsreceipt.AsISODateTime['transactionDateTime'];
      i := jsreceipt.AsInteger['transactionType'];
      Debito := (i = 1);
      Credito := (i = 2);

      s := jsreceipt.AsString['clientVia'];
      //D ImagemComprovante1aVia.Text := FormatarTextoCupom(s);
      s := jsreceipt.AsString['merchantVia'];
      //D ImagemComprovante2aVia.Text := FormatarTextoCupom(s);
      QtdLinhasComprovante := max(ImagemComprovante1aVia.Count, ImagemComprovante2aVia.Count);

      ModalidadePagto := jsreceipt.AsString['cardReadingType'];
      Digitado := jsreceipt.AsBoolean['cardNeedsPassword'];       // estranho ?
      CodigoAutorizacaoTransacao := jsreceipt.AsString['authorisationCode'];
      TextoEspecialOperador := jsreceipt.AsString['messageDisplay'];
      Voucher := jsreceipt.AsBoolean['isVoucher'];
      Debito := Debito and (not Voucher);

      Estabelecimento := jsreceipt.AsString['merchantDocument'];
      Estabelecimento := Estabelecimento + ', ' + jsreceipt.AsString['merchantName'];
      Estabelecimento := Estabelecimento + ', ' + jsreceipt.AsString['merchantAddress'];
      Estabelecimento := Estabelecimento + ', ' + jsreceipt.AsString['merchantCity'];
      Estabelecimento := Estabelecimento + ', ' + jsreceipt.AsString['merchantZipCode'];

      ValorOriginal := jsreceipt.AsCurrency['totalAmountReversal'];
      s := Trim(jsreceipt.AsString['cne']);
      if (s <> '') then
        NFCeSAT.DonoCartao := s;

      Autenticacao := jsreceipt.AsString['retrievalReferenceNumber'];
      CodigoBandeiraPadrao := Trim(jsreceipt.AsString['productBrandCard']);
      CodigoPSP := jsreceipt.AsString['stoneCode'];

      if Credito and (QtdParcelas > 1) then
        TipoOperacao := opParcelado
      else
        TipoOperacao := opAvista;

      NFCeSAT.CodCredenciadora := '999';  // ??
      NFCeSAT.CNPJCredenciadora := '16.501.555/0001-57';  // Stone
      NFCeSAT.Autorizacao := NSU;
      NFCeSAT.Bandeira := Instituicao;
      NFCeSAT.UltimosQuatroDigitos := BIN;
      NFCeSAT.Bandeira := CodigoBandeiraPadrao;
    end;

    jscard := js.AsJSONObject['card'];
    if Assigned(jscard) then
    begin
      ModalidadePagto := jscard.AsString['brandId'];
      ModalidadePagtoDescrita := jscard.AsString['brandName'];
      if (NFCeSAT.DonoCartao = '') then
        NFCeSAT.DonoCartao := Trim(jscard.AsString['cardholderName']);
      s := jscard.AsString['expirationDate'];
      NFCeSAT.DataExpiracao := copy(s,6,2) + copy(s,3,2);  //MMAA
      s := Trim(jscard.AsString['maskedPrimaryAccountNumber']);
      if (s <> '') then
        BIN := s;
      TipoTransacao := jscard.AsInteger['type'];
    end;

    Sucesso := (NSU <> '');
    if not Sucesso then
    begin
      StatusTransacao := js.AsString['code'];
      if (StatusTransacao = '') then
        StatusTransacao := js.AsString['reason'];
      if (StatusTransacao = '') then
        StatusTransacao := js.AsString['responseCode'];
      if (StatusTransacao = '') then
        StatusTransacao := js.AsString['ResponseCode'];

      if (TextoEspecialOperador = '') then
        TextoEspecialOperador := Trim(js.AsString['displayMessage']);
      if (TextoEspecialOperador = '') then
        TextoEspecialOperador := Trim(js.AsString['messageDisplay']);
      if (TextoEspecialOperador = '') then
        TextoEspecialOperador := Trim(js.AsString['responseReason']);
      if (TextoEspecialOperador = '') then
        TextoEspecialOperador := Trim(js.AsString['ResponseReason']);
    end
    else
    begin
      if (fpHeader = CHEADER_PAGAMENTO) then
      begin
        // Mapeia campos, necessários, para o efetuar Cancelamento
        Finalizacao := BIN;
        if Credito then
          Rede := CPaymentType_CREDIT
        else if Debito then
          Rede := CPaymentType_DEBIT
        else
          Rede := '';
      end;
    end;
  finally
    js.Free;
  end;
end;

procedure TACBrTEFRespAditum.ProcessarTipoInterno(ALinha: TACBrTEFLinha);
begin
  {}
end;


{ TACBrTEFAPIClassAditum }

constructor TACBrTEFAPIClassAditum.Create(AACBrTEFAPI: TACBrTEFAPIComum);
begin
  inherited;
  FHTTP := THTTPSend.Create;
  FavailableBrands := TStringList.Create;
  fpTEFRespClass := TACBrTEFRespAditum;
  LimparRespostaHTTP;
end;

destructor TACBrTEFAPIClassAditum.Destroy;
begin
  FHTTP.Free;
  FavailableBrands.Free;
  inherited Destroy;
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
end;

procedure TACBrTEFAPIClassAditum.TransmitirHttp(const AMethod,
  AEndpoint: String; const ABody: AnsiString);
var
  url: String;
  Transmitir: Boolean;
  js, jserro: TACBrJSONObject;
  jserrors: TACBrJSONArray;
  i: Integer;
begin
  Transmitir := True;
  while Transmitir do
  begin
    Transmitir := False;
    LimparRespostaHTTP;
    url := EncodeURL( fpACBrTEFAPI.DadosTerminal.EnderecoServidor + AEndpoint );
    GravarLog('TransmitirHttp( '+AMethod+', '+url+', '+ABody+' )');

    FHTTP.Headers.Add('Authorization: '+fpACBrTEFAPI.DadosTerminal.CodEmpresa);
    if (ABody <> '') then
    begin
      FHTTP.MimeType := CContentTypeApplicationJSon;
      WriteStrToStream(FHTTP.Document, ABody);
    end;

    try
      FHTTP.HTTPMethod(AMethod, url);
    finally
      FHTTPResultCode := FHTTP.ResultCode;
      FHTTP.Document.Position := 0;
      FHTTPResponse := ReadStrFromStream(FHTTP.Document, FHTTP.Document.Size);
      GravarLog('  ResultCode: '+IntToStr(FHTTPResultCode)+', Response: '+FHTTPResponse);
    end;

    if (FHTTPResultCode <> HTTP_OK) and (pos('pinpad/init', url) = 0) then
    try
      js := TACBrJSONObject.Parse(FHTTPResponse);
      try
        jserrors := js.AsJSONArray['errors'];
        if Assigned(jserrors) then
        begin
          for i := 0 to jserrors.Count-1 do
          begin
            jserro := jserrors.ItemAsJSONObject[i];
            if (jserro.AsInteger['errorCode'] = -1000) then
            begin
              Transmitir := True;
              Autenticar;
              Break;
            end;
          end;
        end;
      finally
        js.free;
      end;
    except
    end;
  end;
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
          eDescription := jserro.AsString['description'];
          eCode := jserro.AsInteger['errorCode'];
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
    sMessage := FHTTPResponse
  else
    sMessage := Trim(sMessage);

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

procedure TACBrTEFAPIClassAditum.Autenticar;
var
  js, jspinpadMessages, jsmerchantInfo: TACBrJSONObject;
  sBody, cnpjTEF: String;
  jsa: TACBrJSONArray;
  i: Integer;
begin
  GravarLog('Autenticar');
  LimparRespostaHTTP;
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
      if (LowerCase(js.AsString['sucess']) = 'true') then
        TratarRetornoComErro;

      jsmerchantInfo := js.AsJSONObject['merchantInfo'];
      if Assigned(jsmerchantInfo) then
      begin
        cnpjTEF := OnlyNumber(jsmerchantInfo.AsString['merchantDocument']);
        if (OnlyNumber(fpACBrTEFAPI.DadosEstabelecimento.CNPJ) <> cnpjTEF) then
          DoException(ACBrStr(Format(sACBrAditumCNPJEstabelecimentoDiferente,
                               [fpACBrTEFAPI.DadosEstabelecimento.CNPJ, cnpjTEF]) ));

        if (LowerCase(jsmerchantInfo.AsString['isActive']) <> 'true') then
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
      Result := (LowerCase(js.AsString['status']) = 'ok');
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
    if OperacaoEmAndamento in [tefmtdPagamento, tefmtdCancelamento] then
      CNFEnviado := True;
    AtualizarHeader;

    ConteudoToProperty;

    if (FHTTPResultCode <> HTTP_OK) then
    begin
      if (TextoEspecialOperador = '') then
        TratarRetornoComErro;
    end;
  end;
end;

function TACBrTEFAPIClassAditum.EfetuarPagamento(ValorPagto: Currency;
  Modalidade: TACBrTEFModalidadePagamento; CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento; Parcelas: Byte;
  DataPreDatado: TDateTime; DadosAdicionais: String): Boolean;
var
  js: TACBrJSONObject;
  sBody, sPaymentType, sInstallmentType, sStatus: String;
  Fim: Boolean;
  TefAPI: TACBrTEFAPI;
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
    sBody := js.ToJSON;
  finally
    js.free;
  end;

  TefAPI := TACBrTEFAPI(fpACBrTEFAPI);
  Fim := False;
  while not Fim do
  begin
    TransmitirHttp(cHTTPMethodPOST, 'charge/authorization', sBody);
    if (FHTTPResultCode <> HTTP_OK) then
      TratarRetornoComErro;

    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      Fim := js.ValueExists('charge') or js.ValueExists('errors');
      if not Fim then
      begin
        sStatus := js.AsString['status'];
        if Assigned(TefAPI.QuandoExibirMensagem) then
          TefAPI.QuandoExibirMensagem( TransactionStatusDescription(sStatus), telaOperador, -1 );
      end;
    finally
      js.Free;
    end;

    Sleep(CPoolSleep);
  end;
end;

function TACBrTEFAPIClassAditum.EfetuarAdministrativa(
  OperacaoAdm: TACBrTEFOperacao): Boolean;
begin
  LimparRespostaHTTP;
  Result := False;
  if (OperacaoAdm = tefopTesteComunicacao) then
  begin
    VerificarPresencaPinPad;
    Result := True;
    Exit;
  end;

  //DoException(ACBrStr(Format(sACBrStoneAutoTEFErroModalidadeNaoSuportada,
  //D  ['EfetuarAdministrativa( '+GetEnumName(TypeInfo(TACBrTEFOperacao), integer(OperacaoAdm) )+' )'])));
end;

function TACBrTEFAPIClassAditum.EfetuarAdministrativa(
  const CodOperacaoAdm: string): Boolean;
begin
  LimparRespostaHTTP;
  Result := False;
  //DoException(ACBrStr(Format(sACBrStoneAutoTEFErroModalidadeNaoSuportada,
  //D  ['EfetuarAdministrativa( '+CodOperacaoAdm+' )'])));
end;

function TACBrTEFAPIClassAditum.CancelarTransacao(const NSU,
  CodigoAutorizacaoTransacao: string; DataHoraTransacao: TDateTime;
  Valor: Double; const CodigoFinalizacao: string; const Rede: string): Boolean;
var
  js: TACBrJSONObject;
  sBody: String;
begin
  LimparRespostaHTTP;
  js := TACBrJSONObject.Create;
  try
    js.AddPair('acquirerTransactionKey', NSU);
    js.AddPair('amount', Valor);
    js.AddPair('transactionType', Rede);
    js.AddPair('panMask', CodigoFinalizacao);
    sBody := js.ToJSON;
  finally
    js.free;
  end;

  TransmitirHttp(cHTTPMethodPOST, 'api/cancel', sBody);
  Result := (FHTTPResultCode = HTTP_OK);
end;

procedure TACBrTEFAPIClassAditum.FinalizarTransacao(const Rede, NSU,
  CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
var
  sURL: String;
  js: TACBrJSONObject;
  Confirmar, Ok: Boolean;
begin
  LimparRespostaHTTP;
  Confirmar := (AStatus in [tefstsSucessoAutomatico, tefstsSucessoManual]);
  if Confirmar then
    sURL := 'charge/confirmation/'
  else
    sURL := 'charge/reversal/by-nsu/';

  sURL := sURL + Trim(NSU);
  TransmitirHttp(cHTTPMethodGET, sURL);
  OK := (FHTTPResultCode = HTTP_OK);
  if OK then
  begin
    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      Ok := (LowerCase(js.AsString['success']) = 'true');
      if Ok then
      begin
        if Confirmar then
          Ok := (LowerCase(js.AsString['confirmed']) = 'true')
        else
          Ok := (LowerCase(js.AsString['canceled']) = 'true');
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
  { não há o conceito de transações pendentes }
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

end.

