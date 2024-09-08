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

unit ACBrTEFAPIStoneAutoTEF;

interface

uses
  Classes, SysUtils,
  ACBrBase, ACBrJSON, ACBrSocket, httpsend,
  ACBrTEFComum, ACBrTEFAPI, ACBrTEFAPIComum;

const
  CAutoTEFSlimURL = 'http://localhost:8000/';
  CStoneAutoTEF = 'Stone AutoTEF Slim';

  CACCTYP_CREDIT = 'Credit';
  CACCTYP_DEBIT = 'Debit';
  CACCTYP_UNDEF = 'Undefined';

resourcestring
  sACBrStoneAutoTEFSemStoneCode = 'StoneCode deve ser informado em "DadosTerminal.CodTerminal"';
  sACBrStoneAutoTEFSemPortaPinPad = 'Porta do PinPad deve ser informada em "DadosTerminal.PortaPinPad"';
  sACBrStoneAutoTEFSemNomeAplicacao = 'Nome da Aplicação deve ser informado em "DadosAutomacao.NomeAplicacao"';
  sACBrStoneAutoTEFSemVersaoAplicacao = 'Versão da Aplicação deve ser informado em "DadosAutomacao.VersaoAplicacao"';
  sACBrStoneAutoTEFErroAtivacao = 'Erro ao Ativar '+CStoneAutoTEF;
  sACBrStoneAutoTEFErroModalidadeNaoSuportada = 'Modalidade %s não suportada em '+CStoneAutoTEF;
  sACBrStoneAutoTEFErroDadoNaoEJSon = 'Dado informado em %s não é um JSON válido';

  sACBrStoneAutoTEFN013 = 'Operação Cancelada pelo Operador';
  sACBrStoneAutoTEFN404 = 'Verifique se o Pinpad está bem conectado e se a porta indicada está correta %s.';

type

  { TACBrTEFRespStoneAutoTEF }

  TACBrTEFRespStoneAutoTEF = class( TACBrTEFResp )
  private
    function FormatarTextoCupom(const ATexto: String): String;

  public
    procedure ConteudoToProperty; override;
    procedure ProcessarTipoInterno(ALinha: TACBrTEFLinha); override;

  end;

  { TACBrTEFAPIClassStoneAutoTEF }

  TACBrTEFAPIClassStoneAutoTEF = class(TACBrTEFAPIClass)
  private
    FHTTP: THTTPSend;
    FHTTPResponse: AnsiString;
    FHTTPResultCode: Integer;
    FStarted: TDateTime;

    procedure GravarLog(const AString: AnsiString);
    procedure DoException(const AErrorMsg: String);
    procedure LimparRespostaHTTP;
    procedure TransmitirHttp(const AMethod, AEndpoint: String; const ABody: AnsiString = '');
    procedure TratarRetornoComErro;

    function PartnerName: String;
  protected
    procedure InterpretarRespostaAPI; override;

  public
    constructor Create(AACBrTEFAPI: TACBrTEFAPIComum);
    destructor Destroy; override;

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
    function VerificarPresencaPinPad: Byte; override;

    function MenuPinPad(const Titulo: String; Opcoes: TStrings): Integer;

    property Started: TDateTime read FStarted;
    property HTTPResultCode: Integer read FHTTPResultCode;
    property HTTPResponse: AnsiString read FHTTPResponse;
  end;


implementation

uses
  synautil, Math, DateUtils, TypInfo,
  ACBrUtil.Strings;

{ TACBrTEFRespStoneAutoTEF }

function TACBrTEFRespStoneAutoTEF.FormatarTextoCupom(const ATexto: String): String;
begin
  Result := StringReplace(ATexto, '\r\n', sLineBreak, [rfReplaceAll]);;
  Result := StringReplace(Result, '[[NSUPARCEIRO]]', DocumentoVinculado, [rfReplaceAll]);
  Result := StringReplace(Result, '[[PDVPARCEIRO]]', SerialPOS, [rfReplaceAll]);
end;

procedure TACBrTEFRespStoneAutoTEF.ConteudoToProperty;
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
      ImagemComprovante1aVia.Text := FormatarTextoCupom(s);
      s := jsreceipt.AsString['merchantVia'];
      ImagemComprovante2aVia.Text := FormatarTextoCupom(s);
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

      if (StatusTransacao = 'N013') then
        TextoEspecialOperador := ACBrStr(sACBrStoneAutoTEFN013)
      else if (StatusTransacao = 'N404') then
        TextoEspecialOperador := ACBrStr(sACBrStoneAutoTEFN404);

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
          Rede := CACCTYP_CREDIT
        else if Debito then
          Rede := CACCTYP_DEBIT
        else
          Rede := CACCTYP_UNDEF;
      end;
    end;
  finally
    js.Free;
  end;
end;

procedure TACBrTEFRespStoneAutoTEF.ProcessarTipoInterno(ALinha: TACBrTEFLinha);
begin
  {}
end;


{ TACBrTEFAPIClassStoneAutoTEF }

constructor TACBrTEFAPIClassStoneAutoTEF.Create(AACBrTEFAPI: TACBrTEFAPIComum);
begin
  inherited;
  FHTTP := THTTPSend.Create;
  FStarted := 0;
  fpTEFRespClass := TACBrTEFRespStoneAutoTEF;
  LimparRespostaHTTP;
end;

destructor TACBrTEFAPIClassStoneAutoTEF.Destroy;
begin
  FHTTP.Free;
  inherited Destroy;
end;

procedure TACBrTEFAPIClassStoneAutoTEF.GravarLog(const AString: AnsiString);
begin
  fpACBrTEFAPI.GravarLog(AString);
end;

procedure TACBrTEFAPIClassStoneAutoTEF.DoException(const AErrorMsg: String);
begin
  fpACBrTEFAPI.DoException(AErrorMsg);
end;

procedure TACBrTEFAPIClassStoneAutoTEF.LimparRespostaHTTP;
begin
  FHTTP.Clear;
  FHTTPResultCode := 0;
  FHTTPResponse := '';
end;

procedure TACBrTEFAPIClassStoneAutoTEF.TransmitirHttp(const AMethod,
  AEndpoint: String; const ABody: AnsiString);
var
  url: String;
begin
  LimparRespostaHTTP;
  url := fpACBrTEFAPI.DadosTerminal.EnderecoServidor + AEndpoint;
  GravarLog('TransmitirHttp( '+AMethod+', '+url+', '+ABody+' )');

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
end;

procedure TACBrTEFAPIClassStoneAutoTEF.TratarRetornoComErro;
var
  js, jserrors: TACBrJSONObject;
  sMessage, s: String;
  iStatusCode, i, j: Integer;
  //jserro: TACBrJSONValue;
begin
  iStatusCode := 0;
  try
    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      sMessage := js.AsString['Message'];
      if (sMessage = '') then
      begin
        sMessage := js.AsString['responseReason'];
        if (sMessage = '') then
          sMessage := js.AsString['ResponseReason'];

        s := js.AsString['responseCode'];
        if (s = '') then
          s := js.AsString['ResponseCode'];

        if (s <> '') then
          sMessage := s + ' - ' + sMessage;
      end;

      if (sMessage = '') then
      begin
        jserrors := js.AsJSONObject['errors'];
        if Assigned(jserrors) then
        begin
          sMessage := jserrors.ToJSON + sLineBreak;
          //for i := 0 to jserrors.Count-1 do
          //begin
          //  s := jserrors.ItemAsValue[i].AsJSON;
          //  jserro := jserrors.ItemAsValue[i];
          //  for j := 0 to jserro.Count-1 do
          //    sMessage := sMessage + jserro.Items[j].AsString + sLineBreak;
          //end;
        end;

        sMessage := Trim(js.AsString['title'] + sLineBreak + sMessage);
      end;

      iStatusCode := js.AsInteger['StatusCode'];
      if (iStatusCode = 0) then
        iStatusCode := js.AsInteger['Status'];
    finally
      js.free;
    end;
  except
  end;

  if (iStatusCode = 0) then
    iStatusCode := FHTTPResultCode;

  if (sMessage <> '') then
    sMessage := sMessage + ', '
  else
    sMessage := FHTTPResponse + sLineBreak;

  sMessage := sMessage + Format('StatusCode: %d', [iStatusCode]);
  DoException(sMessage);
end;

function TACBrTEFAPIClassStoneAutoTEF.PartnerName: String;
begin
  Result := fpACBrTEFAPI.DadosAutomacao.NomeAplicacao + '*' + fpACBrTEFAPI.DadosAutomacao.VersaoAplicacao;
end;

procedure TACBrTEFAPIClassStoneAutoTEF.Autenticar;
var
  js: TACBrJSONObject;
  sBody: String;
begin
  GravarLog('Autenticar');
  LimparRespostaHTTP;
  if (fpACBrTEFAPI.DadosTerminal.CodTerminal = '') then
    DoException(ACBrStr(sACBrStoneAutoTEFSemStoneCode));

  if (fpACBrTEFAPI.DadosTerminal.PortaPinPad = '') then
    DoException(ACBrStr(sACBrStoneAutoTEFSemPortaPinPad));

  if (fpACBrTEFAPI.DadosAutomacao.NomeAplicacao = '') then
    DoException(ACBrStr(sACBrStoneAutoTEFSemNomeAplicacao));

  if (fpACBrTEFAPI.DadosAutomacao.VersaoAplicacao = '') then
    DoException(ACBrStr(sACBrStoneAutoTEFSemVersaoAplicacao));

  if (fpACBrTEFAPI.DadosTerminal.EnderecoServidor = '') then
    fpACBrTEFAPI.DadosTerminal.EnderecoServidor := CAutoTEFSlimURL
  else
    fpACBrTEFAPI.DadosTerminal.EnderecoServidor := URLWithDelim(fpACBrTEFAPI.DadosTerminal.EnderecoServidor);

  js := TACBrJSONObject.Create;
  try
    js.AddPair('StoneCode', fpACBrTEFAPI.DadosTerminal.CodTerminal);
    js.AddPair('ConnectionName', fpACBrTEFAPI.DadosTerminal.PortaPinPad);
    js.AddPair('PartnerName', PartnerName());
    sBody := js.ToJSON+'}';
  finally
    js.free;
  end;

  TransmitirHttp(cHTTPMethodPOST, 'api/Activate', sBody);
  if (FHTTPResultCode = HTTP_OK) then
  begin
    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      FStarted := js.AsISODateTime['Started'];
    finally
      js.Free;
    end;
  end
  else
    TratarRetornoComErro;
end;

procedure TACBrTEFAPIClassStoneAutoTEF.InterpretarRespostaAPI;
begin
  with fpACBrTEFAPI.UltimaRespostaTEF do
  begin
    Clear;
    Conteudo.GravaInformacao(899,200,FHTTPResponse);
    DocumentoVinculado := fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao;
    SerialPOS := PartnerName();
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

function TACBrTEFAPIClassStoneAutoTEF.EfetuarPagamento(ValorPagto: Currency;
  Modalidade: TACBrTEFModalidadePagamento; CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento; Parcelas: Byte;
  DataPreDatado: TDateTime; DadosAdicionais: String): Boolean;
var
  js, jsinstallment: TACBrJSONObject;
  sBody, saccountType: String;
  iType: Integer;
begin
  LimparRespostaHTTP;
  if not (Modalidade in [tefmpCartao]) then
    DoException(ACBrStr(Format(sACBrStoneAutoTEFErroModalidadeNaoSuportada,
       [GetEnumName(TypeInfo(TACBrTEFModalidadePagamento), integer(Modalidade) )])));

  VerificarPresencaPinPad;

  if CartoesAceitos = [] then
    CartoesAceitos := [teftcCredito];

  if (teftcCredito in CartoesAceitos) then
    saccountType := CACCTYP_CREDIT
  else if (teftcDebito in CartoesAceitos) then
    saccountType := CACCTYP_DEBIT
  else
    saccountType := CACCTYP_UNDEF;

  if saccountType = CACCTYP_CREDIT then
    iType := 0
  else
  begin
    case Financiamento of
      tefmfParceladoEmissor: iType := 3;
      tefmfParceladoEstabelecimento: iType := 2;
    else
      iType := 1
    end;
  end;

  try
    js := TACBrJSONObject.Parse(DadosAdicionais);
  except
    DoException(Format(ACBrStr(sACBrStoneAutoTEFErroDadoNaoEJSon), ['DadosAdicionais']));
  end;

  try
    js.AddPair('amount', ValorPagto);
    if not js.ValueExists('hasAlcoholicDrink') then
      js.AddPair('hasAlcoholicDrink', False);  // Se necessário, Informe seu valor em DadosAdicionais
    jsinstallment := TACBrJSONObject.Create;
    jsinstallment.AddPair('number', Parcelas);
    jsinstallment.AddPair('type', iType);
    js.AddPair('installment', jsinstallment);
    js.AddPair('accountType', saccountType);

    sBody := js.ToJSON;
  finally
    js.free;
  end;

  TransmitirHttp(cHTTPMethodPOST, 'api/Pay', sBody);
  Result := (FHTTPResultCode = HTTP_OK);
end;

function TACBrTEFAPIClassStoneAutoTEF.EfetuarAdministrativa(
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

  DoException(ACBrStr(Format(sACBrStoneAutoTEFErroModalidadeNaoSuportada,
    ['EfetuarAdministrativa( '+GetEnumName(TypeInfo(TACBrTEFOperacao), integer(OperacaoAdm) )+' )'])));
end;

function TACBrTEFAPIClassStoneAutoTEF.EfetuarAdministrativa(
  const CodOperacaoAdm: string): Boolean;
begin
  LimparRespostaHTTP;
  Result := False;
  DoException(ACBrStr(Format(sACBrStoneAutoTEFErroModalidadeNaoSuportada,
    ['EfetuarAdministrativa( '+CodOperacaoAdm+' )'])));
end;

function TACBrTEFAPIClassStoneAutoTEF.CancelarTransacao(const NSU,
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

procedure TACBrTEFAPIClassStoneAutoTEF.FinalizarTransacao(const Rede, NSU,
  CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
begin
  { Em Stone AutoTEF, não há necessidade de enviar a 3a perna (CNF, NCF) }
end;

procedure TACBrTEFAPIClassStoneAutoTEF.ResolverTransacaoPendente(
  AStatus: TACBrTEFStatusTransacao);
begin
  { Em Stone AutoTEF, não há o conceito de transações pendentes }
end;

procedure TACBrTEFAPIClassStoneAutoTEF.AbortarTransacaoEmAndamento;
begin
  { Em Stone AutoTEF, não é possível abortar do lado do Operador }
end;

procedure TACBrTEFAPIClassStoneAutoTEF.ExibirMensagemPinPad(
  const MsgPinPad: String);
const
  CPINPAD_COL = 16;
  CPINPAD_LIN = 2;
var
  js: TACBrJSONObject;
  Ok: Boolean;
  s, sBody: String;
begin
  LimparRespostaHTTP;
  s := StringReplace(MsgPinPad, '|', #10, [rfReplaceAll]);
  s := AjustaLinhas(s, CPINPAD_COL, CPINPAD_LIN, True);
  s := StringReplace(s, #10, '', [rfReplaceAll]);
  js := TACBrJSONObject.Create;
  try
    js.AddPair('Message', Trim(copy(s, 1, CPINPAD_COL)));
    js.AddPair('SecondMessage', Trim(copy(s, CPINPAD_COL+1, CPINPAD_COL)));
    js.AddPair('FormatMessage', 'Center');
    sBody := js.ToJSON;
  finally
    js.free;
  end;

  TransmitirHttp(cHTTPMethodPOST, 'api/Pinpad/message', sBody);
  Ok := (FHTTPResultCode = HTTP_OK);
  if not Ok then
    TratarRetornoComErro;
end;

function TACBrTEFAPIClassStoneAutoTEF.ObterDadoPinPad(
  TipoDado: TACBrTEFAPIDadoPinPad; TimeOut: integer; MinLen: SmallInt;
  MaxLen: SmallInt): String;
begin
  LimparRespostaHTTP;
  DoException(ACBrStr(Format(sACBrStoneAutoTEFErroModalidadeNaoSuportada, ['ObterDadoPinPad'])));
end;

function TACBrTEFAPIClassStoneAutoTEF.VerificarPresencaPinPad: Byte;
var
  js: TACBrJSONObject;
  s: String;
  Ok: Boolean;
begin
  TransmitirHttp(cHTTPMethodGET, 'api/Healthcheck');
  Ok := (FHTTPResultCode = HTTP_OK);
  if Ok then
  begin
    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      s := js.AsString['connectionName'];
      Result := StrToIntDef(OnlyNumber(s), 0);
    finally
      js.Free;
    end;
  end
  else
    TratarRetornoComErro;
end;

function TACBrTEFAPIClassStoneAutoTEF.MenuPinPad(const Titulo: String;
  Opcoes: TStrings): Integer;
var
  js: TACBrJSONObject;
  s, sBody: String;
  Ok: Boolean;
  jsoptions: TACBrJSONArray;
  i: Integer;
begin
  Result := 0;
  js := TACBrJSONObject.Create;
  try
    js.AddPair('Header', Titulo);
    jsoptions := TACBrJSONArray.Create;
    for i := 0 to Opcoes.Count-1 do
      jsoptions.AddElement(Opcoes[i]);
    js.AddPair('Options', jsoptions);
    sBody := js.ToJSON;
  finally
    js.free;
  end;

  TransmitirHttp(cHTTPMethodPOST, 'api/Pinpad/selection', sBody);
  Ok := (FHTTPResultCode = HTTP_OK);
  if Ok then
  begin
    js := TACBrJSONObject.Parse(FHTTPResponse);
    try
      s := js.AsString['selectedItemNumber'];
      Result := StrToIntDef(s, -1) + 1;
    finally
      js.Free;
    end;
  end;
end;

end.

