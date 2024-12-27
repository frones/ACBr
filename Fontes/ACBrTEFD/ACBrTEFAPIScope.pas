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

unit ACBrTEFAPIScope;

interface

uses
  Classes, SysUtils,
  ACBrBase,
  ACBrTEFComum, ACBrTEFAPI, ACBrTEFAPIComum, ACBrTEFScopeAPI;

const
  CSUBDIRETORIO_SCOPE = 'scope';

type

  { TACBrTEFAPIClassScope }

  TACBrTEFAPIClassScope = class(TACBrTEFAPIClass)
  private
    fDiretorioTrabalho: String;
    fTEFScopeAPI: TACBrTEFScopeAPI;

    procedure QuandoGravarLogAPI(const ALogLine: String; var Tratado: Boolean);
    procedure QuandocopeTransacaoEmAndamentoAPI( EstadoOperacao: TACBrTEFScopeEstadoOperacao;
      out Cancelar: Boolean);

    procedure QuandoExibirMensagemAPI(const Mensagem: String;
       Terminal: TACBrTEFScopeTerminalMensagem; MilissegundosExibicao: Integer);
    procedure QuandoPerguntarMenuAPI(const Titulo: String; Opcoes: TStringList;
       var ItemSelecionado: Integer);
    procedure QuandoPerguntarCampoAPI( const MsgOperador: String;
       const MsgCliente: String; const TituloCampo: String; const AcoesPermitidas: Byte;
       var Resposta: String; var AcaoResposta: Byte);

    procedure SetDiretorioTrabalho(const AValue: String);

  protected
    procedure InicializarChamadaAPI(AMetodoOperacao: TACBrTEFAPIMetodo); override;
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

    property TEFScopeAPI: TACBrTEFScopeAPI read fTEFScopeAPI;
    property DiretorioTrabalho: String read fDiretorioTrabalho write SetDiretorioTrabalho;
  end;

implementation

uses
  math, TypInfo,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrUtil.FilesIO;

{ TACBrTEFAPIClassScope }

constructor TACBrTEFAPIClassScope.Create(AACBrTEFAPI: TACBrTEFAPIComum);
begin
  inherited;

  fpTEFRespClass := TACBrTEFRespScope;

  fTEFScopeAPI := TACBrTEFScopeAPI.Create;
  fTEFScopeAPI.OnTransacaoEmAndamento := QuandocopeTransacaoEmAndamentoAPI;
  fTEFScopeAPI.OnGravarLog := QuandoGravarLogAPI;
  fTEFScopeAPI.OnExibeMensagem := QuandoExibirMensagemAPI;
  fTEFScopeAPI.OnExibeMenu := QuandoPerguntarMenuAPI;
  fTEFScopeAPI.OnPerguntaCampo := QuandoPerguntarCampoAPI;
end;

destructor TACBrTEFAPIClassScope.Destroy;
begin
  fTEFScopeAPI.Free;
  inherited;
end;

procedure TACBrTEFAPIClassScope.Inicializar;
var
  i, P: Integer;
  ADir, IpStr, PortaStr: String;
begin
  if Inicializado then
    Exit;

  if (fDiretorioTrabalho = '') then
    ADir := PathWithDelim(fpACBrTEFAPI.DiretorioTrabalho) + CSUBDIRETORIO_SCOPE
  else
    ADir := fDiretorioTrabalho;

  IpStr := fpACBrTEFAPI.DadosTerminal.EnderecoServidor;
  PortaStr := '';
  p := pos(':', IpStr);
  if (p > 0) then
  begin
    PortaStr := copy(IpStr, p+1, Length(IpStr));
    IpStr := copy(IpStr, 1, p-1);
  end;

//D
  fTEFScopeAPI.DiretorioTrabalho := ADir;
  fTEFScopeAPI.EnderecoIP := IpStr;
  fTEFScopeAPI.PortaTCP := PortaStr;
  //fTEFScopeAPI.Aplicacao := fpACBrTEFAPI.DadosAutomacao.NomeAplicacao;
  fTEFScopeAPI.VersaoAutomacao := fpACBrTEFAPI.DadosAutomacao.VersaoAplicacao;
  //fTEFScopeAPI.SoftwareHouse := fpACBrTEFAPI.DadosAutomacao.NomeSoftwareHouse;
  //fTEFScopeAPI.NomeEstabelecimento := fpACBrTEFAPI.DadosEstabelecimento.RazaoSocial;
  //fTEFScopeAPI.SuportaSaque := fpACBrTEFAPI.DadosAutomacao.SuportaSaque;
  //fTEFScopeAPI.SuportaDesconto := fpACBrTEFAPI.DadosAutomacao.SuportaDesconto;
  //fTEFScopeAPI.SuportaViasDiferenciadas := fpACBrTEFAPI.DadosAutomacao.SuportaViasDiferenciadas;
  //fTEFScopeAPI.ImprimeViaClienteReduzida := fpACBrTEFAPI.DadosAutomacao.ImprimeViaClienteReduzida;
  //fTEFScopeAPI.UtilizaSaldoTotalVoucher := fpACBrTEFAPI.DadosAutomacao.UtilizaSaldoTotalVoucher;
  //i := Integer(TACBrTEFAPI(fpACBrTEFAPI).ExibicaoQRCode);
  //fTEFScopeAPI.ExibicaoQRCode := TACBrTEFPGWebAPIExibicaoQRCode(i);
  //
  //fTEFScopeAPI.ConfirmarTransacoesPendentesNoHost := (fpACBrTEFAPI.TratamentoTransacaoPendente = tefpenConfirmar);
  //if (fpACBrTEFAPI.TratamentoTransacaoPendente = tefpenPerguntar) then
  //  fTEFScopeAPI.OnAvaliarTransacaoPendente := QuandoAvaliarTransacaoPendenteAPI
  //else
  //  fTEFScopeAPI.OnAvaliarTransacaoPendente := Nil;
  //

//E
  fTEFScopeAPI.Empresa := fpACBrTEFAPI.DadosTerminal.CodEmpresa;
  fTEFScopeAPI.Filial := fpACBrTEFAPI.DadosTerminal.CodFilial;
  fTEFScopeAPI.PDV := fpACBrTEFAPI.DadosTerminal.CodTerminal;
  fTEFScopeAPI.PortaPinPad := fpACBrTEFAPI.DadosTerminal.PortaPinPad;

  fTEFScopeAPI.Inicializar;

  inherited;
end;

procedure TACBrTEFAPIClassScope.DesInicializar;
begin
  fTEFScopeAPI.DesInicializar;
  inherited;
end;

procedure TACBrTEFAPIClassScope.InicializarChamadaAPI(
  AMetodoOperacao: TACBrTEFAPIMetodo);
begin
  inherited;
//E?? - Poderia não confirmar as transações...
  fpACBrTEFAPI.ConfirmarTransacoesPendentes;
end;

procedure TACBrTEFAPIClassScope.InterpretarRespostaAPI;
begin
  //inherited;
  fpACBrTEFAPI.UltimaRespostaTEF.ViaClienteReduzida := fpACBrTEFAPI.DadosAutomacao.ImprimeViaClienteReduzida;
  //D
  DadosDaTransacaoToTEFResp( fTEFScopeAPI.DadosDaTransacao,
                             fpACBrTEFAPI.UltimaRespostaTEF );
end;

procedure TACBrTEFAPIClassScope.QuandoGravarLogAPI(const ALogLine: String;
  var Tratado: Boolean);
begin
  fpACBrTEFAPI.GravarLog(ALogLine);
  Tratado := True;
end;

procedure TACBrTEFAPIClassScope.QuandocopeTransacaoEmAndamentoAPI(
  EstadoOperacao: TACBrTEFScopeEstadoOperacao; out Cancelar: Boolean);
var
  i: Integer;
begin
  i := Integer(EstadoOperacao);
  Cancelar := False;
  TACBrTEFAPI(fpACBrTEFAPI).QuandoEsperarOperacao(TACBrTEFAPIOperacaoAPI(i), Cancelar);
end;

procedure TACBrTEFAPIClassScope.QuandoExibirMensagemAPI(const Mensagem: String;
  Terminal: TACBrTEFScopeTerminalMensagem; MilissegundosExibicao: Integer);
var
  TelaMsg: TACBrTEFAPITela;
begin
  case Terminal of
    tmOperador: TelaMsg := telaOperador;
    tmCliente: TelaMsg := telaCliente;
  else
    TelaMsg := telaTodas;
  end;

  TACBrTEFAPI(fpACBrTEFAPI).QuandoExibirMensagem(
    Mensagem,
    TelaMsg,
    MilissegundosExibicao );
end;

procedure TACBrTEFAPIClassScope.QuandoPerguntarMenuAPI(const Titulo: String;
  Opcoes: TStringList; var ItemSelecionado: Integer);
begin
  TACBrTEFAPI(fpACBrTEFAPI).QuandoPerguntarMenu( Titulo, Opcoes, ItemSelecionado );
end;

procedure TACBrTEFAPIClassScope.QuandoPerguntarCampoAPI(
  const MsgOperador: String; const MsgCliente: String;
  const TituloCampo: String; const AcoesPermitidas: Byte; var Resposta: String;
  var AcaoResposta: Byte);
var
  Validado, Cancelado: Boolean;
  DefCampo: TACBrTEFAPIDefinicaoCampo;
begin
  DefCampo.TituloPergunta := TituloCampo;
  DefCampo.MascaraDeCaptura := '';
  DefCampo.TipoDeEntrada := tedTodos;
  DefCampo.TamanhoMinimo := 0;
  DefCampo.TamanhoMaximo := 100;
  DefCampo.ValorMinimo := 0;
  DefCampo.ValorMaximo := 0;
  DefCampo.OcultarDadosDigitados := False;
  DefCampo.ValidacaoDado := valdNenhuma;
  DefCampo.ValorInicial := Resposta;
  DefCampo.MsgErroDeValidacao := '';
  DefCampo.MsgErroDadoMaior := '';
  DefCampo.MsgErroDadoMenor := '';
  DefCampo.MsgConfirmacaoDuplaDigitacao := '';
  DefCampo.TipoEntradaCodigoBarras := tbQualquer;
  DefCampo.TipoCampo := 0;

  TACBrTEFAPI(fpACBrTEFAPI).QuandoPerguntarCampo(DefCampo, Resposta, Validado, Cancelado);

  if Cancelado or (Resposta = ':-1') then
    AcaoResposta := ACAO_RESUME_CANCELAR
  else if (Resposta = ':-2') then
    AcaoResposta := ACAO_RESUME_ESTADO_ANTERIOR
  else
    AcaoResposta := ACAO_RESUME_PROXIMO_ESTADO;
end;


function TACBrTEFAPIClassScope.EfetuarAdministrativa(OperacaoAdm: TACBrTEFOperacao): Boolean;
begin
  EfetuarAdministrativa('');
end;

function TACBrTEFAPIClassScope.EfetuarAdministrativa(const CodOperacaoAdm: string): Boolean;
begin
  fTEFScopeAPI.IniciarTransacao(scoMenu);
  fTEFScopeAPI.ExecutarTransacao;
end;

function TACBrTEFAPIClassScope.EfetuarPagamento(ValorPagto: Currency;
  Modalidade: TACBrTEFModalidadePagamento; CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento; Parcelas: Byte;
  DataPreDatado: TDateTime; DadosAdicionais: String): Boolean;
var
  ValorTransacaoString, ValorTaxaServicoString: string;
begin
  VerificarIdentificadorVendaInformado;
  if (ValorPagto <= 0) then
    fpACBrTEFAPI.DoException(sACBrTEFAPIValorPagamentoInvalidoException);

  ValorTransacaoString := IntToStr(Trunc(RoundTo(ValorPagto * 100,-2)));
  ValorTaxaServicoString := '000';

  fTEFScopeAPI.IniciarTransacao(scoCredito, ValorTransacaoString, ValorTaxaServicoString);
  fTEFScopeAPI.ExecutarTransacao;
end;

procedure TACBrTEFAPIClassScope.FinalizarTransacao(const Rede, NSU,
  CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
var
  i: Integer;
  Resp: TACBrTEFResp;
  PGWebStatus: LongWord;
begin
  i := fpACBrTEFAPI.RespostasTEF.AcharTransacao(Rede, NSU, CodigoFinalizacao);
  if (i >= 0) then
    Resp := fpACBrTEFAPI.RespostasTEF[i]
  else
    fpACBrTEFAPI.DoException( Format(ACBrStr(sACBrTEFAPITransacaoNaoEncontradaException),
      [Rede+' '+ NSU+' '+CodigoFinalizacao]) );

  //D
  //PGWebStatus := StatusTransacaoToPWCNF_(AStatus);
  //fTEFPayGoAPI.FinalizarTransacao( PGWebStatus,
  //                                 IntToStr(Resp.NumeroLoteTransacao),
  //                                 Resp.Finalizacao,
  //                                 Resp.NSU,
  //                                 Resp.Estabelecimento,
  //                                 Resp.Rede );
end;

function TACBrTEFAPIClassScope.CancelarTransacao(const NSU,
  CodigoAutorizacaoTransacao: string; DataHoraTransacao: TDateTime;
  Valor: Double; const CodigoFinalizacao: string; const Rede: string): Boolean;
var
  PA: TACBrTEFParametros;
  i: Integer;
  Resp: TACBrTEFResp;

  procedure CopiarValorDaUltimaResposta(AInfo: Integer);
  var
    AStr: String;
  begin
    AStr := Resp.LeInformacao(AInfo).AsString;
    if (Trim(AStr) <> '') then
      PA.ValueInfo[AInfo] := AStr;
  end;

begin
  //D
  //PA := TACBrTEFParametros.Create;
  //try
  //  PA.ValueInfo[PWINFO_TRNORIGNSU] := NSU;
  //  PA.ValueInfo[PWINFO_TRNORIGDATE] := FormatDateTime('DDMMYY', DataHoraTransacao);
  //  PA.ValueInfo[PWINFO_TRNORIGTIME] := FormatDateTime('hhnnss', DataHoraTransacao);
  //  //PA.ValueInfo[PWINFO_TRNORIGDATETIME] := FormatDateTime('YYYYMMDDhhnnss', DataHoraTransacao);
  //  PA.ValueInfo[PWINFO_TRNORIGAMNT] :=  IntToStr(Trunc(RoundTo(Valor * 100,-2)));
  //  PA.ValueInfo[PWINFO_TRNORIGAUTH] := CodigoAutorizacaoTransacao;
  //  PA.ValueInfo[PWINFO_TRNORIGAUTHCODE] := CodigoAutorizacaoTransacao;
  //
  //  if (Rede <> '') and (CodigoFinalizacao <> '') then
  //  begin
  //    i := fpACBrTEFAPI.RespostasTEF.AcharTransacao(Rede, NSU, CodigoFinalizacao);
  //    if (i >= 0) then
  //    begin
  //      Resp := fpACBrTEFAPI.RespostasTEF[i];
  //      PA.ValueInfo[PWINFO_TRNORIGLOCREF] := Resp.Finalizacao;
  //      PA.ValueInfo[PWINFO_TRNORIGREQNUM] := IntToStr(Resp.NumeroLoteTransacao);
  //      //CopiarValorDaUltimaResposta(PWINFO_MERCHCNPJCPF);
  //      CopiarValorDaUltimaResposta(PWINFO_CARDTYPE);
  //      CopiarValorDaUltimaResposta(PWINFO_VIRTMERCH);
  //      CopiarValorDaUltimaResposta(PWINFO_AUTMERCHID);
  //      CopiarValorDaUltimaResposta(PWINFO_FINTYPE);
  //    end;
  //  end;
  //
  //  if (Rede <> '') then
  //    PA.ValueInfo[PWINFO_AUTHSYST] := Rede
  //  else if (fAutorizador <> '') then
  //    PA.ValueInfo[PWINFO_AUTHSYST] := fAutorizador;
  //
  //  fTEFPayGoAPI.IniciarTransacao(fOperacaoCancelamento, PA);
  //  Result := fTEFPayGoAPI.ExecutarTransacao;
  //finally
  //  PA.Free;
  //end;
end;

procedure TACBrTEFAPIClassScope.ResolverTransacaoPendente(
  AStatus: TACBrTEFStatusTransacao);
var
  PGWebStatus: LongWord;
begin
  //D
  // Se não foi disparado por QuandoAvaliarTransacaoPendenteAPI, pegue as
  //   informações da Transação atual, na memória
  //if (fpndReqNum = '') then
  //begin
  //  with fpACBrTEFAPI.UltimaRespostaTEF do
  //  begin
  //    fpndReqNum := LeInformacao(PWINFO_REQNUM,0).AsString;
  //    fPndLocRef := LeInformacao(PWINFO_AUTLOCREF,0).AsString;
  //    fPndExtRef := LeInformacao(PWINFO_AUTEXTREF,0).AsString;
  //    fPndVirtMerch := LeInformacao(PWINFO_VIRTMERCH,0).AsString;
  //    fPndAuthSyst := LeInformacao(PWINFO_AUTHSYST,0).AsString;
  //  end;
  //end;
  //
  //if (fpndReqNum = '') then
  //  fpACBrTEFAPI.DoException(sACBrTEFAPISemTransacaoPendenteException);
  //
  //PGWebStatus := StatusTransacaoToPWCNF_(AStatus);
  //fTEFPayGoAPI.FinalizarTransacao(PGWebStatus,
  //        fpndReqNum, fPndLocRef, fPndExtRef, fPndVirtMerch, fPndAuthSyst);
  //LimparUltimaTransacaoPendente;
end;

procedure TACBrTEFAPIClassScope.AbortarTransacaoEmAndamento;
begin
  fTEFScopeAPI.AbortarTransacao;
end;

procedure TACBrTEFAPIClassScope.ExibirMensagemPinPad(const MsgPinPad: String);
begin
  fTEFScopeAPI.ExibirMensagemPinPad(MsgPinPad);
end;

function TACBrTEFAPIClassScope.ObterDadoPinPad(
  TipoDado: TACBrTEFAPIDadoPinPad; TimeOut: integer; MinLen: SmallInt;
  MaxLen: SmallInt): String;
var
  TipoMsg: Word;
begin
  //D
  //TipoMsg := DadoPinPadToMsg(TipoDado);
  //if (TipoMsg < 1) then
  //begin
  //  fpACBrTEFAPI.DoException(Format(ACBrStr(sACBrTEFAPICapturaNaoSuportada),
  //    [GetEnumName(TypeInfo(TACBrTEFAPIDadoPinPad), integer(TipoDado) ), ClassName] ));
  //end;
  //
  //if (MinLen = 0) and (MaxLen = 0) then
  //  CalcularTamanhosCampoDadoPinPad(TipoDado, MinLen, MaxLen);
  //
  //Result := fTEFPayGoAPI.ObterDadoPinPad( TipoMsg,
  //                                        MinLen, MaxLen,
  //                                        Trunc(TimeOut/1000) );
end;

function TACBrTEFAPIClassScope.VerificarPresencaPinPad: Byte;
begin
  //D Result := fTEFPayGoAPI.VerificarPresencaPinPad;
end;

procedure TACBrTEFAPIClassScope.SetDiretorioTrabalho(const AValue: String);
begin
  if fDiretorioTrabalho = AValue then Exit;

  if Inicializado then
    fpACBrTEFAPI.DoException(Format(ACBrStr(sACBrTEFAPIComponenteInicializadoException),
                                     ['TACBrTEFAPIClassScope.DiretorioTrabalho']));

  fDiretorioTrabalho := AValue;
end;

end.
