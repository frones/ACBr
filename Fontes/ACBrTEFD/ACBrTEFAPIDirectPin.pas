{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2025 Daniel Simoes de Almeida               }
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

unit ACBrTEFAPIDirectPin;

interface

uses
  Classes, SysUtils,
  ACBrTEFComum, ACBrTEFAPI, ACBrTEFAPIComum,
  ACBrTEFDirectPinAPI;

type
  { TACBrTEFRespDirectPin }

  TACBrTEFRespDirectPin = class( TACBrTEFResp )
  public
    procedure ConteudoToProperty; override;
  end;

  { TACBrTEFAPIDirectPin }

  TACBrTEFAPIDirectPin = class(TACBrTEFAPIClass)
  private
    fDPAPI: TDPAPI;
    fTimeOut: Integer;
    fUltimaResposta: String;

    procedure GravarLogAPI(const ALogLine: String; var Tratado: Boolean);
    procedure TransacaoEmProgressoAPI( EstOp: TDPOperation; out Cancelar: Boolean);

  protected
    procedure InterpretarRespostaAPI; override;

  public
    constructor Create(AACBrTEFAPI: TACBrTEFAPIComum);
    destructor Destroy; override;

    procedure Inicializar; override;
    procedure DesInicializar; override;

    function EfetuarPagamento(
      ValorPagto: Currency;
      Modalidade: TACBrTEFModalidadePagamento = tefmpNaoDefinido;
      CartoesAceitos: TACBrTEFTiposCartao = [];
      Financiamento: TACBrTEFModalidadeFinanciamento = tefmfNaoDefinido;
      Parcelas: Byte = 0;
      DataPreDatado: TDateTime = 0;
      DadosAdicionais: String = ''): Boolean; override;

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

    property TimeOut: Integer read fTimeOut write fTimeOut default CDPTimeOut;

    property DPAPI: TDPAPI read fDPAPI;
    property UltimaResposta: String read fUltimaResposta;
  end;


implementation

uses
  DateUtils, Math,
  ACBrJSON,
  ACBrUtil.DateTime;

{ TACBrTEFRespDirectPin }

procedure TACBrTEFRespDirectPin.ConteudoToProperty;
var
  s, sjs: String;
  js: TACBrJSONObject;
begin
  fpCNFEnviado := (UpperCase(Conteudo.LeInformacao(899,1).AsString) = 'S');
  fpHeader := Conteudo.LeInformacao(899,100).AsString;
  Confirmar := False;
  sjs := Conteudo.LeInformacao(899,200).AsString;
  js := TACBrJSONObject.Parse(sjs);
  try
    Sucesso := js.AsBoolean['result'];
    TextoEspecialOperador := js.AsString['message'];

    s := js.AsString['receiptContent'];
    ImagemComprovante1aVia.Text := StringReplace(s, '@', sLineBreak, [rfReplaceAll]);
    ImagemComprovante2aVia.Text := ImagemComprovante1aVia.Text;

    DataHoraTransacaoHost := UnixMillisecondsToDateTime(js.AsInt64['date'], False);
    DataHoraTransacaoLocal := DataHoraTransacaoHost;

    ValorTotal := js.AsInteger['amount']/100;
    NSU := js.AsString['nsu'];
    CodigoAutorizacaoTransacao := js.AsString['nsuAcquirer'];
    PAN := js.AsString['panMasked'];
    Finalizacao := UpperCase(js.AsString['typeCard']);
    StatusTransacao := js.AsString['finalResult'];
    if (TextoEspecialOperador = '') and (StatusTransacao <> '') then
      TextoEspecialOperador := s;

    TipoTransacao := js.AsInteger['codeResult'];
  finally
    js.Free;
  end;
end;

{ TACBrTEFAPIDirectPin }

constructor TACBrTEFAPIDirectPin.Create(AACBrTEFAPI: TACBrTEFAPIComum);
begin
  inherited Create(AACBrTEFAPI);
  fpTEFRespClass := TACBrTEFRespDirectPin;

  fDPAPI := TDPAPI.Create;
  fDPAPI.OnWriteLog := GravarLogAPI;
  fDPAPI.TransactionInProgress := TransacaoEmProgressoAPI;
  fUltimaResposta := '';
  fTimeOut := CDPTimeOut;
end;

destructor TACBrTEFAPIDirectPin.Destroy;
begin
  fDPAPI.Free;
  inherited Destroy;
end;

procedure TACBrTEFAPIDirectPin.Inicializar;
begin
  if Inicializado then
    Exit;

  fDPAPI.TimeOut := fTimeOut;
  fDPAPI.Port := fpACBrTEFAPI.DadosTerminal.PortaPinPad;
  fDPAPI.Initialize;
  fUltimaResposta := '';

  inherited Inicializar;
end;

procedure TACBrTEFAPIDirectPin.DesInicializar;
begin
  fDPAPI.Deinitialize;

  inherited DesInicializar;
end;

procedure TACBrTEFAPIDirectPin.GravarLogAPI(const ALogLine: String;
  var Tratado: Boolean);
begin
  fpACBrTEFAPI.GravarLog(ALogLine);
  Tratado := True;
end;

procedure TACBrTEFAPIDirectPin.TransacaoEmProgressoAPI(EstOp: TDPOperation; out Cancelar: Boolean);
var
  op: TACBrTEFAPIOperacaoAPI;
begin
  Cancelar := False;
  with TACBrTEFAPI(fpACBrTEFAPI) do
  begin
    op := opapiAguardaUsuario;
    if Assigned( QuandoEsperarOperacao ) then
      QuandoEsperarOperacao( op, Cancelar );
  end;
end;

procedure TACBrTEFAPIDirectPin.InterpretarRespostaAPI;
begin
  with fpACBrTEFAPI.UltimaRespostaTEF do
  begin
    Clear;
    Conteudo.GravaInformacao(899,200, fUltimaResposta);
    DocumentoVinculado := fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao;
    if OperacaoEmAndamento in [tefmtdPagamento, tefmtdCancelamento] then
      CNFEnviado := True;
    AtualizarHeader;
    ConteudoToProperty;
  end;
end;

function TACBrTEFAPIDirectPin.EfetuarPagamento(ValorPagto: Currency;
  Modalidade: TACBrTEFModalidadePagamento; CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento; Parcelas: Byte;
  DataPreDatado: TDateTime; DadosAdicionais: String): Boolean;
var
  ReqTrans: TDPPayloadRequestTransaction;
  ResTrans: TDPPayloadResponseTransaction;
  ct: TDPcreditType;
  tt: TDPtypeTransaction;
  it: TDPinterestType;
  Payload: String;
begin
  fUltimaResposta := '';
  Parcelas := max(Parcelas, 1);

  if (Financiamento > tefmfAVista) then
    ct := dpcINSTALLMENT
  else
    ct := dpcNO_INSTALLMENT;

  if Modalidade = tefmpCarteiraVirtual then
    tt := dptPIX
  else if (teftcCredito in CartoesAceitos) then
    tt := dptCREDIT
  else if (teftcDebito in CartoesAceitos) then
    tt := dptDEBIT
  else if (teftcVoucher in CartoesAceitos) then
    tt := dptVOUCHER
  else
    tt := dptNONE;

  if (Financiamento in [tefmfParceladoEmissor, tefmfCreditoEmissor]) then
    it := dpiISSUER
  else
    it := dpiMERCHANT;

  ReqTrans := TDPPayloadRequestTransaction.Create;
  try
    try
      ReqTrans.AsJSON := DadosAdicionais;
    except
      DadosAdicionais := '';
    end;

    if (pos('amount', DadosAdicionais) = 0) then
      ReqTrans.amount := ValorPagto;
    if (pos('creditType', DadosAdicionais) = 0) then
      ReqTrans.creditType := ct;
    if (pos('installment', DadosAdicionais) = 0) then
      ReqTrans.installment := Parcelas;
    if (pos('typeTransaction', DadosAdicionais) = 0) then
      ReqTrans.typeTransaction := tt;
    if (pos('interestType', DadosAdicionais) = 0) then
      ReqTrans.interestType := it;

    Payload := ReqTrans.AsJSON;
  finally
    ReqTrans.Free;
  end;

  fUltimaResposta := fDPAPI.RequestTransaction(Payload);
  Result := (fUltimaResposta <> '');
end;

function TACBrTEFAPIDirectPin.CancelarTransacao(const NSU,
  CodigoAutorizacaoTransacao: string; DataHoraTransacao: TDateTime;
  Valor: Double; const CodigoFinalizacao: string; const Rede: string): Boolean;
var
  ReqRev: TDPPayLoadRequestReversal;
  ResRev: TDPPayLoadResponseReversal;
  Payload: String;
begin
  fUltimaResposta := '';

  ReqRev := TDPPayLoadRequestReversal.Create;
  try
    ReqRev.nsu := NSU;
    Payload := ReqRev.AsJSON;
  finally
    ReqRev.Free;
  end;

  fUltimaResposta := fDPAPI.RequestReversal(Payload);
  Result := (fUltimaResposta <> '');
end;

procedure TACBrTEFAPIDirectPin.FinalizarTransacao(const Rede, NSU,
  CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
begin
  { Em DirectPin, não há necessidade de enviar a 3a perna (CNF, NCF) }
end;

procedure TACBrTEFAPIDirectPin.ResolverTransacaoPendente(
  AStatus: TACBrTEFStatusTransacao);
begin
  { Em DirectPin, não há o conceito de transações pendentes }
end;

procedure TACBrTEFAPIDirectPin.AbortarTransacaoEmAndamento;
begin
  fDPAPI.AbortTransaction;
end;

end.

