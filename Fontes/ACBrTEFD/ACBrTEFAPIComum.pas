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
    * DEFINIÇÃO IMPORTANTE: OPERAÇAO vs TRANSAÇÃO *

    OPERACAO TEF: É o ato de iniciar uma chamada a API de TEF, gerando uma Transação TEF
    TRANSACAO TEF: É a resposta obtida ao Termino de uma Operação TEF.
                   Geralmente associada a uma Resposta, mapeada na classe "TACBrTEFResp"
*)

{$I ACBr.inc}

unit ACBrTEFAPIComum;

interface

uses
  Classes, SysUtils,
  ACBrBase, ACBrTEFComum;

resourcestring
  sACBrTEFAPIIdentificadorVendaVazioException = 'IdentificadorVenda não pode ser vazio';
  sACBrTEFAPIValorPagamentoInvalidoException = 'Valor do Pagamento inválido';

  sACBrTEFAPIClassDonoNaoDefinido = 'O componente TACBrTEFAPIComumClass, deve ser filho de TACBrTEFAPIComum';
  sACBrTEFAPIMetodoInvalidoException = 'Método: %s '+ sLineBreak +
                                       'não implementado para o TEF: %s';
  sACBrTEFAPIEventoInvalidoException = 'Evento %s não foi implementado na sua aplicação';
  sACBrTEFAPIComponenteInicializadoException = '%s não pode ser modificado após TEF Inicializado';
  sACBrTEFAPIArquivoNaoExistenteException = 'Arquivo %s não Encontrado';
  sACBrTEFAPIDiretorioInvalido = 'Diretorio %s não existe';
  sACBrTEFAPISemTransacaoPendenteException = 'Não há transação Pendente';
  sACBrTEFAPITransacaoNaoEncontradaException = 'Transação não encontrada [%s]';
  sACBrTEFAPICapturaNaoSuportada = 'Captura Tipo: %s não suportada por: %s';
  sACBrTEFAPIAdministrativaNaoSuportada = 'Função Administrativa: %s não suportada por: %s';
  sACBrTEFAPITransacaoPendente = 'Transação Pendente.' + sLineBreak +
                                 'Rede: %s' + sLineBreak +
                                 'NSU: %s';

const
  CPREFIXO_ARQUIVO_TEF = 'ACBr_';
  CEXTENSAO_ARQUIVO_TEF = '.tef';
  CDIRETORIO_TRABALHO_PADRAO = 'tef';
  CHEADER_PAGAMENTO = 'CRT';
  CHEADER_ADMINISTRATIVA = 'ADM';
  CHEADER_CANCELAMENTO = 'CNC';

type
  EACBrTEFAPIErro = class(EACBrTEFErro);

  TACBrTEFRespHack = class(TACBrTEFResp);

  { TACBrTEFAPIDadosAutomacao }

  TACBrTEFAPIDadosAutomacao = class( TPersistent )
  private
    fAutoAtendimento: Boolean;
    fNomeSoftwareHouse: String;
    fNomeAplicacao: String;
    fVersaoAplicacao: String;
    fSuportaSaque: Boolean;
    fSuportaDesconto: Boolean;
    fSuportaViasDiferenciadas: Boolean;
    fUtilizaSaldoTotalVoucher: Boolean;
    fImprimeViaClienteReduzida: Boolean;
    fCNPJSoftwareHouse: String;
    fMoedaISO4217: Integer;
    procedure SetNomeAplicacao(const AValue: String);
    procedure SetNomeSoftwareHouse(const AValue: String);
    procedure SetVersaoAplicacao(const AValue: String);

  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;

  published
    property NomeSoftwareHouse: String read fNomeSoftwareHouse write SetNomeSoftwareHouse;
    property CNPJSoftwareHouse: String read fCNPJSoftwareHouse write fCNPJSoftwareHouse;
    property NomeAplicacao: String read fNomeAplicacao write SetNomeAplicacao ;
    property VersaoAplicacao: String read fVersaoAplicacao write SetVersaoAplicacao ;

    Property SuportaSaque: Boolean read fSuportaSaque write fSuportaSaque default False;
    Property SuportaDesconto: Boolean read fSuportaDesconto write fSuportaDesconto default False;
    property ImprimeViaClienteReduzida: Boolean read fImprimeViaClienteReduzida
      write fImprimeViaClienteReduzida default False;
    property SuportaViasDiferenciadas: Boolean read fSuportaViasDiferenciadas
      write fSuportaViasDiferenciadas default True;
    property UtilizaSaldoTotalVoucher: Boolean read fUtilizaSaldoTotalVoucher
      write fUtilizaSaldoTotalVoucher default False;
    property MoedaISO4217: Integer read fMoedaISO4217 write fMoedaISO4217 default 986;
    property AutoAtendimento: Boolean read fAutoAtendimento write fAutoAtendimento;  // Ainda NÃO utilizado
  end;

  { TACBrTEFAPIDadosEstabelecimento }

  TACBrTEFAPIDadosEstabelecimento = class(TPersistent)
  private
    fCNPJ: String;
    fRazaoSocial: String;
    procedure SetRazaoSocial(const AValue: String);
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;

  published
    property CNPJ: String read fCNPJ write fCNPJ;
    property RazaoSocial: String read fRazaoSocial write SetRazaoSocial;
  end;

  { TACBrTEFDadosTerminal }

  { TACBrTEFAPIDadosTerminal }

  TACBrTEFAPIDadosTerminal = class(TPersistent)
  private
    fCodEmpresa: String;
    fCodFilial: String;
    fCodTerminal: String;
    fEnderecoServidor: String;
    fOperador: String;
    fPortaPinPad: String;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;

  published
    property EnderecoServidor: String read fEnderecoServidor write fEnderecoServidor;
    property CodEmpresa: String read fCodEmpresa write fCodEmpresa;
    property CodFilial: String read fCodFilial write fCodFilial;
    property CodTerminal: String read fCodTerminal write fCodTerminal;
    property Operador: String read fOperador write fOperador;
    property PortaPinPad: String read fPortaPinPad write fPortaPinPad;
  end;

  TACBrTEFAPIMetodo = (tefmtdNenhuma, tefmtdPagamento, tefmtdCancelamento, tefmtdAdministrativa);

  TACBrTEFAPIComum = class;

  { TACBrTEFAPIComumClass }

  TACBrTEFAPIComumClass = class
  protected
    fpACBrTEFAPI: TACBrTEFAPIComum;
    fpInicializado: Boolean;
    fpMetodoOperacao: TACBrTEFAPIMetodo;
    fpTEFRespClass: TACBrTEFRespClass;

    procedure ErroAbstract(const NomeProcedure: String);
    procedure VerificarIdentificadorVendaInformado;
    procedure AtualizarHeader;

    procedure InicializarChamadaAPI(AMetodoOperacao: TACBrTEFAPIMetodo); virtual;
    procedure FinalizarChamadaAPI; virtual;
    procedure InterpretarRespostaAPI; virtual;

    procedure ProcessarRespostaOperacaoTEF; virtual;

  public
    constructor Create(AACBrTEFAPI: TACBrTEFAPIComum);
    destructor Destroy; override;

    procedure Inicializar; virtual;
    procedure DesInicializar; virtual;
    procedure Autenticar; virtual;

//  function VerificarTEF: Boolean; virtual;

    function EfetuarPagamento(
      ValorPagto: Currency;
      Modalidade: TACBrTEFModalidadePagamento = tefmpNaoDefinido;
      CartoesAceitos: TACBrTEFTiposCartao = [];
      Financiamento: TACBrTEFModalidadeFinanciamento = tefmfNaoDefinido;
      Parcelas: Byte = 0;
      DataPreDatado: TDateTime = 0 ): Boolean; virtual;

    function EfetuarAdministrativa(
      OperacaoAdm: TACBrTEFOperacao = tefopAdministrativo): Boolean; overload; virtual;
    function EfetuarAdministrativa(const CodOperacaoAdm: string = ''):
      Boolean; overload; virtual;

    function CancelarTransacao(
      const NSU, CodigoAutorizacaoTransacao: string;
      DataHoraTransacao: TDateTime;
      Valor: Double;
      const CodigoFinalizacao: string = '';
      const Rede: string = ''): Boolean; virtual;

    procedure FinalizarTransacao(
      const Rede, NSU, CodigoFinalizacao: String;
      AStatus: TACBrTEFStatusTransacao = tefstsSucessoAutomatico); virtual;

    procedure ResolverTransacaoPendente(
      AStatus: TACBrTEFStatusTransacao = tefstsSucessoManual); virtual;

    procedure AbortarTransacaoEmAndamento; virtual;

    property Inicializado: Boolean read fpInicializado;
    property OperacaoEmAndamento: TACBrTEFAPIMetodo read fpMetodoOperacao;

    property TEFRespClass: TACBrTEFRespClass read fpTEFRespClass;
  end;

  TACBrTEFAPIEventoTransacao = procedure(RespostaTEF: TACBrTEFResp) of object;
  TACBrTEFAPIEventoTransacaoErro = procedure(RespostaTEF: TACBrTEFResp;
    const MsgErro: String) of object;
  TACBrTEFAPIEventoTransacaoStatus = procedure(RespostaTEF: TACBrTEFResp;
    AStatus: TACBrTEFStatusTransacao) of object;

  { TACBrTEFAPIRespostas }

  TACBrTEFAPIRespostas = class
  private
    fTEFRespList: TACBrTEFRespostas;
    fDiretorioTrabalho: String;
    fTEFRespClass: TACBrTEFRespClass;
    fIdentificadorTransacao: String;
    fGravarRespostas: Boolean;
    fLimparRespostasPorTransacao: Boolean;
    procedure SetIdentificadorTransacao(const AValue: String);
    function GetItem(Index: Integer): TACBrTEFResp;
    function GetCount: Integer;
    function ObterMascaraTodosArquivosDiretorioTrabalho: String;

  protected
    function AddClone(ATEFResp: TACBrTEFResp): Integer;
    procedure VerificarDiretorioTrabalho;
    procedure SalvarRespostaTEF(AIndex: Integer; Sobrescrever: Boolean = True);

  public
    constructor Create(TEFRespClass: TACBrTEFRespClass;
      const DirTrabalho: String; GravarRespostas: Boolean;
      LimparRespostasPorTransacao: Boolean);
    destructor Destroy; override;

    function AdicionarRespostaTEF(ATEFResp: TACBrTEFResp): Integer;
    procedure SalvarRespostasTEF;
    procedure LimparRespostasTEF;
    procedure ApagarRespostaTEF(AIndex: Integer); overload;
    procedure ApagarRespostaTEF(ATEFResp: TACBrTEFResp); overload;
    procedure GravarRespostaTEF(AIndex: Integer); overload;
    procedure GravarRespostaTEF(ATEFResp: TACBrTEFResp); overload;
    procedure CarregarRespostasDoDiretorioTrabalho;
    function AcharTransacao(const Rede, NSU: String; const CodigoFinalizacao: String = ''): Integer;
    procedure AtualizarTransacaoComTerceiraPerna(const Rede, NSU, CodigoFinalizacao: String); overload;
    procedure AtualizarTransacaoComTerceiraPerna(ATEFResp: TACBrTEFResp); overload;

    property IdentificadorTransacao: String read fIdentificadorTransacao
      write SetIdentificadorTransacao;
    property Items[Index: Integer]: TACBrTEFResp read GetItem; default;
    property Count: Integer read GetCount;
  end;

  { TACBrTEFAPIComum }

  TACBrTEFAPIComum = class( TACBrComponent )
  private
    fArqLOG: String;
    fRespostasTEF: TACBrTEFAPIRespostas;
    fDiretorioTrabalho: String;
    fGravarRespostas: Boolean;
    fLimparRespostasQuandoNovoIdentificador: Boolean;
    fQuandoGravarLog: TACBrGravarLog;
    fQuandoFinalizarOperacao: TACBrTEFAPIEventoTransacao;
    fQuandoFinalizarTransacao: TACBrTEFAPIEventoTransacaoStatus;
    fQuandoDetectarTransacaoPendente: TACBrTEFAPIEventoTransacaoErro;
    fConfirmarTransacaoAutomaticamente: Boolean;
    fDadosAutomacao: TACBrTEFAPIDadosAutomacao;
    fDadosEstabelecimento: TACBrTEFAPIDadosEstabelecimento;
    fDadosTerminal: TACBrTEFAPIDadosTerminal;
    fTratamentoTransacaoInicializacao: TACBrTEFTratamentoTransacaoInicializacao;
    fTratamentoTransacaoPendente: TACBrTEFTratamentoTransacaoPendente;
    fUltimaRespostaTEF: TACBrTEFResp;

    function GetEmTransacao: Boolean;
    function GetInicializado: Boolean;
    procedure SetInicializado(const AValue: Boolean);
    procedure CriarListaTEFResp;

    procedure GravarLogEmArquivo(const aString: AnsiString);
    function GetDiretorioTrabalho: String;
    procedure SetDiretorioTrabalho(const AValue: String);
    procedure SetDadosAutomacao(const AValue: TACBrTEFAPIDadosAutomacao);
    procedure SetDadosEstabelecimento(const AValue: TACBrTEFAPIDadosEstabelecimento);
    procedure SetDadosTerminal(const AValue: TACBrTEFAPIDadosTerminal);
    procedure SetTratamentoTransacaoInicializacao(AValue: TACBrTEFTratamentoTransacaoInicializacao);
    procedure SetTratamentoTransacaoPendente(const AValue: TACBrTEFTratamentoTransacaoPendente);
    procedure SetGravarRespostas(const AValue: Boolean);
    procedure SetLimparRespostasQuandoNovoIdentificador(const AValue: Boolean);
  protected
    fpTEFAPIClass: TACBrTEFAPIComumClass;
    fpInicializando: Boolean;

    procedure CriarTEFResp;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Inicializar; virtual;
    procedure DesInicializar; virtual;
    property Inicializado: Boolean read GetInicializado write SetInicializado;
    property Inicializando: Boolean read fpInicializando;
    procedure DoException(const AErrorMsg: String); virtual;

    function EfetuarAdministrativa(
      Operacao: TACBrTEFOperacao = tefopAdministrativo;
      const IdentificadorTransacao: string = ''): Boolean; overload; virtual;
    function EfetuarAdministrativa(
      const Operacao: string = '';
      const IdentificadorTransacao: string = ''): Boolean; overload; virtual;

    function EfetuarPagamento(
      const IdentificadorTransacao: String;
      ValorPagto: Currency;
      Modalidade: TACBrTEFModalidadePagamento = tefmpNaoDefinido;
      CartoesAceitos: TACBrTEFTiposCartao = [];
      Financiamento: TACBrTEFModalidadeFinanciamento = tefmfNaoDefinido;
      Parcelas: Byte = 0;
      DataPreDatado: TDateTime = 0): Boolean; virtual;

    function CancelarTransacao(
      const NSU, CodigoAutorizacaoTransacao: string;
      DataHoraTransacao: TDateTime;
      Valor: Double;
      const CodigoFinalizacao: string = '';
      const Rede: string = ''): Boolean; virtual;

    procedure FinalizarTransacao(
      const Rede, NSU, CodigoFinalizacao: String;
      AStatus: TACBrTEFStatusTransacao = tefstsSucessoAutomatico); overload; virtual;
    procedure FinalizarTransacao(
      AStatus: TACBrTEFStatusTransacao = tefstsSucessoAutomatico); overload; virtual;

    procedure ProcessarTransacaoPendente(const MsgErro: String); virtual;
    procedure ResolverTransacaoPendente(
      Status: TACBrTEFStatusTransacao = tefstsSucessoManual); virtual;

    //  function VerificarTEF: Boolean;
    procedure AbortarTransacaoEmAndamento;

    procedure LimparRespostasTEF;
    procedure CarregarRespostasDoDiretorioTrabalho;

    procedure VerificarTransacoesPendentes;
    procedure ConfirmarTransacoesPendentes;
    procedure EstornarTransacoesPendentes;
    procedure CancelarOuEstornarTransacoesDiretorioTrabalho;
    procedure FinalizarTransacoesPendentes(Status: TACBrTEFStatusTransacao = tefstsSucessoAutomatico);

    procedure GravarLog(const AString: AnsiString); virtual;

    property RespostasTEF: TACBrTEFAPIRespostas read fRespostasTEF;
    property UltimaRespostaTEF: TACBrTEFResp read fUltimaRespostaTEF;

    property EmTransacao: Boolean read GetEmTransacao;

  published
    property ArqLOG: String read fArqLOG write fArqLOG;

    property DiretorioTrabalho: String read GetDiretorioTrabalho
      write SetDiretorioTrabalho;
    property GravarRespostas: Boolean
      read fGravarRespostas write SetGravarRespostas default True;
    property LimparRespostasQuandoNovoIdentificador: Boolean
      read fLimparRespostasQuandoNovoIdentificador
      write SetLimparRespostasQuandoNovoIdentificador default True;

    property ConfirmarTransacaoAutomaticamente: Boolean
      read fConfirmarTransacaoAutomaticamente
      write fConfirmarTransacaoAutomaticamente default True;
    property TratamentoTransacaoPendente : TACBrTEFTratamentoTransacaoPendente
      read fTratamentoTransacaoPendente
      write SetTratamentoTransacaoPendente default tefpenConfirmar;
    property TratamentoTransacaoInicializacao: TACBrTEFTratamentoTransacaoInicializacao
      read fTratamentoTransacaoInicializacao write SetTratamentoTransacaoInicializacao
      default tefopiProcessarPendentes;

    property DadosAutomacao: TACBrTEFAPIDadosAutomacao
      read fDadosAutomacao write SetDadosAutomacao;
    property DadosEstabelecimento: TACBrTEFAPIDadosEstabelecimento
      read fDadosEstabelecimento write SetDadosEstabelecimento;
    property DadosTerminal: TACBrTEFAPIDadosTerminal
      read fDadosTerminal write SetDadosTerminal;

    property QuandoGravarLog: TACBrGravarLog read fQuandoGravarLog write fQuandoGravarLog;
    property QuandoFinalizarOperacao: TACBrTEFAPIEventoTransacao
      read fQuandoFinalizarOperacao write fQuandoFinalizarOperacao;
    property QuandoFinalizarTransacao: TACBrTEFAPIEventoTransacaoStatus
      read fQuandoFinalizarTransacao write fQuandoFinalizarTransacao;
    property QuandoDetectarTransacaoPendente: TACBrTEFAPIEventoTransacaoErro
      read fQuandoDetectarTransacaoPendente write fQuandoDetectarTransacaoPendente;
  end;

implementation

uses
  StrUtils, TypInfo,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.FilesIO;

{ TACBrTEFAPIDadosAutomacao }

constructor TACBrTEFAPIDadosAutomacao.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrTEFAPIDadosAutomacao.Clear;
begin
  fNomeSoftwareHouse := '';
  fNomeAplicacao := '';
  fVersaoAplicacao := '';
  fImprimeViaClienteReduzida := False;
  fSuportaDesconto := False;
  fSuportaSaque := False;
  fSuportaViasDiferenciadas := True;
  fUtilizaSaldoTotalVoucher := False;
  fMoedaISO4217 := 986;
  fAutoAtendimento := False;
end;

procedure TACBrTEFAPIDadosAutomacao.Assign(Source: TPersistent);
var
  DadosSource: TACBrTEFAPIDadosAutomacao;
begin
  if (Source is TACBrTEFAPIDadosAutomacao) then
  begin
    DadosSource := TACBrTEFAPIDadosAutomacao(Source);

    fNomeSoftwareHouse := DadosSource.NomeSoftwareHouse;
    fNomeAplicacao := DadosSource.NomeAplicacao;
    fVersaoAplicacao := DadosSource.VersaoAplicacao;
    fImprimeViaClienteReduzida := DadosSource.ImprimeViaClienteReduzida;
    fSuportaDesconto := DadosSource.SuportaDesconto;
    fSuportaSaque := DadosSource.SuportaSaque;
    fSuportaViasDiferenciadas := DadosSource.SuportaViasDiferenciadas;
    fUtilizaSaldoTotalVoucher := DadosSource.UtilizaSaldoTotalVoucher;
    fAutoAtendimento := DadosSource.AutoAtendimento;
  end;
end;

procedure TACBrTEFAPIDadosAutomacao.SetNomeAplicacao(const AValue: String);
begin
  if fNomeAplicacao = AValue then Exit;
  fNomeAplicacao := LeftStr(Trim(AValue),128);
end;

procedure TACBrTEFAPIDadosAutomacao.SetNomeSoftwareHouse(const AValue: String);
begin
  if fNomeSoftwareHouse = AValue then Exit;
  fNomeSoftwareHouse := LeftStr(Trim(AValue),128);
end;

procedure TACBrTEFAPIDadosAutomacao.SetVersaoAplicacao(const AValue: String);
begin
  if fVersaoAplicacao = AValue then Exit;
  fVersaoAplicacao := LeftStr(Trim(AValue),128);
end;

{ TACBrTEFAPIDadosEstabelecimento }

constructor TACBrTEFAPIDadosEstabelecimento.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrTEFAPIDadosEstabelecimento.Clear;
begin
  fCNPJ := '';
  fRazaoSocial := '';
end;

procedure TACBrTEFAPIDadosEstabelecimento.Assign(Source: TPersistent);
var
  DadosEstabelecimento: TACBrTEFAPIDadosEstabelecimento;
begin
  if (Source is TACBrTEFAPIDadosEstabelecimento) then
  begin
    DadosEstabelecimento := TACBrTEFAPIDadosEstabelecimento(Source);

    fCNPJ := DadosEstabelecimento.CNPJ;
    fRazaoSocial := DadosEstabelecimento.RazaoSocial;
  end;
end;

procedure TACBrTEFAPIDadosEstabelecimento.SetRazaoSocial(const AValue: String);
begin
  if fRazaoSocial = AValue then
    Exit;

  fRazaoSocial := Trim(AValue);
end;

{ TACBrTEFAPIDadosTerminal }

constructor TACBrTEFAPIDadosTerminal.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrTEFAPIDadosTerminal.Clear;
begin
  fCodEmpresa := '';
  fCodFilial := '';
  fCodTerminal := '';
  fOperador := '';
  fPortaPinPad := '';
  fEnderecoServidor := '';
end;

procedure TACBrTEFAPIDadosTerminal.Assign(Source: TPersistent);
var
  DadosTerminal: TACBrTEFAPIDadosTerminal;
begin
  if (Source is TACBrTEFAPIDadosTerminal) then
  begin
    DadosTerminal := TACBrTEFAPIDadosTerminal(Source);

    fCodEmpresa := DadosTerminal.CodEmpresa;
    fCodFilial := DadosTerminal.CodFilial;
    fCodTerminal := DadosTerminal.CodTerminal;
    fOperador := DadosTerminal.Operador;
    fPortaPinPad := DadosTerminal.PortaPinPad;
    fEnderecoServidor := DadosTerminal.EnderecoServidor;
  end;
end;

{ TACBrTEFAPIRespostas }

constructor TACBrTEFAPIRespostas.Create(TEFRespClass: TACBrTEFRespClass;
  const DirTrabalho: String; GravarRespostas: Boolean;
  LimparRespostasPorTransacao: Boolean);
begin
  inherited Create;

  fTEFRespClass := TEFRespClass;
  fLimparRespostasPorTransacao := LimparRespostasPorTransacao;
  fGravarRespostas := GravarRespostas;
  fDiretorioTrabalho := PathWithDelim(DirTrabalho);

  fTEFRespList := TACBrTEFRespostas.Create(True);
end;

destructor TACBrTEFAPIRespostas.Destroy;
begin
  fTEFRespList.Free;
  inherited;
end;

function TACBrTEFAPIRespostas.AcharTransacao(const Rede, NSU: String;
  const CodigoFinalizacao: String): Integer;
var
  i: Integer;
  ATEFResp: TACBrTEFResp;
begin
  Result := -1;
  for i := 0 to Count-1 do
  begin
    ATEFResp := Items[i];
    if (ATEFResp.Rede = Rede) and
       (ATEFResp.NSU = NSU) and
       ( (CodigoFinalizacao = '') or (ATEFResp.Finalizacao = CodigoFinalizacao) ) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TACBrTEFAPIRespostas.AtualizarTransacaoComTerceiraPerna(const Rede,
  NSU, CodigoFinalizacao: String);
var
  i: Integer;
begin
  i := AcharTransacao(Rede, NSU, CodigoFinalizacao);
  if (i >= 0) then
    AtualizarTransacaoComTerceiraPerna(Items[i]);
end;

procedure TACBrTEFAPIRespostas.AtualizarTransacaoComTerceiraPerna(ATEFResp: TACBrTEFResp);
begin
  if not Assigned(ATEFResp) then
    Exit;

  ATEFResp.CNFEnviado := True;
  // OK. "CNFEnviado" não é um bom nome para essa propriedade... :(
  // O Nome é mantido por motivo de compatibilidade com as antigas implementações de TEF do ACBr.
  // Quando informamos "ATEFResp.CNFEnviado := True", isso significa que já enviamos a Terceira Perna,
  //  para essa transação, independente se ela foi Confirmada ou Estornada...
  GravarRespostaTEF(ATEFResp);
end;

function TACBrTEFAPIRespostas.ObterMascaraTodosArquivosDiretorioTrabalho: String;
begin
  if (fDiretorioTrabalho = '') then
    VerificarDiretorioTrabalho;

  Result := fDiretorioTrabalho + CPREFIXO_ARQUIVO_TEF + '*' +CEXTENSAO_ARQUIVO_TEF;
end;

function TACBrTEFAPIRespostas.GetCount: Integer;
begin
  Result := fTEFRespList.Count;
end;

function TACBrTEFAPIRespostas.GetItem(Index: Integer): TACBrTEFResp;
begin
  Result := fTEFRespList[Index];
end;

procedure TACBrTEFAPIRespostas.CarregarRespostasDoDiretorioTrabalho;
var
  SL: TStringList;
  Arquivo: String;
  NovaResp: TACBrTEFResp;
  i: Integer;
begin
  VerificarDiretorioTrabalho;

  fTEFRespList.Clear;
  SL := TStringList.Create;
  try
    FindFiles(ObterMascaraTodosArquivosDiretorioTrabalho, SL, True, fstFileName, fsdAscending );
    for i := 0 to SL.Count-1 do
    begin
      Arquivo := SL[i];
      NovaResp := fTEFRespClass.Create;
      NovaResp.LeArquivo(Arquivo);
      NovaResp.ArqBackup := Arquivo;

      fTEFRespList.Add(NovaResp);
      fIdentificadorTransacao := NovaResp.DocumentoVinculado;
    end;
  finally
    SL.Free;
  end;
end;

procedure TACBrTEFAPIRespostas.LimparRespostasTEF;
//var
//  i: Integer;
begin
  while (fTEFRespList.Count > 0) do
    ApagarRespostaTEF(0);
end;

procedure TACBrTEFAPIRespostas.ApagarRespostaTEF(AIndex: Integer);
var
  ArqBkp: String;
begin
  if (AIndex < 0) or (AIndex >= fTEFRespList.Count) then
    Exit;

  ArqBkp := fTEFRespList[AIndex].ArqBackup;
  if (ArqBkp <> '') and FileExists(ArqBkp) then
    DeleteFile(ArqBkp);

  fTEFRespList.Delete(AIndex);
end;

procedure TACBrTEFAPIRespostas.ApagarRespostaTEF(ATEFResp: TACBrTEFResp);
begin
  ApagarRespostaTEF( fTEFRespList.IndexOf(ATEFResp) );
end;

procedure TACBrTEFAPIRespostas.GravarRespostaTEF(AIndex: Integer);
begin
  if (AIndex >= 0) and (AIndex < fTEFRespList.Count) then
    GravarRespostaTEF(fTEFRespList[AIndex]);
end;

procedure TACBrTEFAPIRespostas.GravarRespostaTEF(ATEFResp: TACBrTEFResp);
begin
  if (ATEFResp.ArqBackup <> '') then
    ATEFResp.Conteudo.GravarArquivo(ATEFResp.ArqBackup);
end;

procedure TACBrTEFAPIRespostas.SalvarRespostasTEF;
var
  i: Integer;
begin
  for i := 0 to fTEFRespList.Count-1 do
    SalvarRespostaTEF(i);
end;

procedure TACBrTEFAPIRespostas.SalvarRespostaTEF(AIndex: Integer; Sobrescrever: Boolean);
var
  ArqResposta: string;
  ATEFResp: TACBrTEFResp;
  Salvar: Boolean;
begin
  if (AIndex < 0) or (AIndex >= fTEFRespList.Count) then
    Exit;

  ATEFResp := fTEFRespList[AIndex];
  // Apenas salva Transações que precisem de Confirmação (Terceira Perna)
  Salvar := ATEFResp.Confirmar or
            (ATEFResp.ImagemComprovante1aVia.Count > 0) or
            (ATEFResp.ImagemComprovante2aVia.Count > 0);
  if not Salvar then
    Exit;

  VerificarDiretorioTrabalho;
  if (ATEFResp.ArqBackup = '') then
    ArqResposta := fDiretorioTrabalho +
                   CPREFIXO_ARQUIVO_TEF + IntToStrZero(AIndex,3) +
                   CEXTENSAO_ARQUIVO_TEF
  else
    ArqResposta := ATEFResp.ArqBackup;

  if Sobrescrever or (not FileExists(ArqResposta)) then
  begin
    ATEFResp.ArqBackup := ArqResposta;
    ATEFResp.Conteudo.GravarArquivo(ArqResposta);
  end;
end;

procedure TACBrTEFAPIRespostas.VerificarDiretorioTrabalho;
begin
  if (fDiretorioTrabalho = '') then
    fDiretorioTrabalho := ApplicationPath + CDIRETORIO_TRABALHO_PADRAO + PathDelim;

  if not DirectoryExists(fDiretorioTrabalho) then
  begin
    ForceDirectories(fDiretorioTrabalho);

    if not DirectoryExists(fDiretorioTrabalho) then
      raise EACBrTEFAPIErro.CreateFmt( ACBrStr(sACBrTEFAPIDiretorioInvalido),
                                       [fDiretorioTrabalho] ) ;
  end;
end;

procedure TACBrTEFAPIRespostas.SetIdentificadorTransacao(const AValue: String);
begin
  if (fIdentificadorTransacao = AValue) then
    Exit;

  if fLimparRespostasPorTransacao then
    LimparRespostasTEF;

  fIdentificadorTransacao := AValue;
end;

function TACBrTEFAPIRespostas.AddClone(ATEFResp: TACBrTEFResp): Integer;
var
  NovaResp: TACBrTEFResp;
begin
  NovaResp := fTEFRespClass.Create;
  NovaResp.Assign(ATEFResp);
  Result := fTEFRespList.Add(NovaResp);
end;

function TACBrTEFAPIRespostas.AdicionarRespostaTEF(ATEFResp: TACBrTEFResp): Integer;
begin
  Result := -1;
  if ( (Trim(ATEFResp.Rede) <> '') and
       (Trim(ATEFResp.NSU) <> '') ) then
  begin
    // Verificando se essa transação já está na lista
    Result := AcharTransacao(ATEFResp.Rede, ATEFResp.NSU, ATEFResp.Finalizacao);
  end;

  if (Result < 0) then  // Se não estiver, crie um clone e adicione na lista
  begin
    Result := AddClone(ATEFResp);
    if fGravarRespostas then
      SalvarRespostaTEF(Result);
  end;
end;

{ TACBrTEFAPIComumClass }

constructor TACBrTEFAPIComumClass.Create(AACBrTEFAPI: TACBrTEFAPIComum);
begin
  if not Assigned(AACBrTEFAPI) then
    raise EACBrTEFAPIErro.Create( sACBrTEFAPIClassDonoNaoDefinido );

  inherited Create;
  fpACBrTEFAPI := AACBrTEFAPI;
  fpInicializado := False;
  fpMetodoOperacao := tefmtdNenhuma;
  fpTEFRespClass := TACBrTEFResp;
end;

destructor TACBrTEFAPIComumClass.Destroy;
begin
  inherited;
end;

procedure TACBrTEFAPIComumClass.Inicializar;
begin
  Autenticar;
  fpInicializado := True;
end;

procedure TACBrTEFAPIComumClass.DesInicializar;
begin
  fpInicializado := False;
end;

procedure TACBrTEFAPIComumClass.Autenticar;
begin
  { Nada a fazer, sobreescrever se necessário }
end;

procedure TACBrTEFAPIComumClass.AbortarTransacaoEmAndamento;
begin
  ErroAbstract('AbortarTransacaoEmAndamento');
end;

procedure TACBrTEFAPIComumClass.InicializarChamadaAPI(
  AMetodoOperacao: TACBrTEFAPIMetodo);
begin
  fpMetodoOperacao := AMetodoOperacao;
end;

procedure TACBrTEFAPIComumClass.FinalizarChamadaAPI;
begin
  fpMetodoOperacao := tefmtdNenhuma;
end;

function TACBrTEFAPIComumClass.EfetuarAdministrativa(const CodOperacaoAdm: string): Boolean;
begin
  Result := False;
end;

function TACBrTEFAPIComumClass.EfetuarAdministrativa(
  OperacaoAdm: TACBrTEFOperacao): Boolean;
begin
  Result := False;
end;

function TACBrTEFAPIComumClass.CancelarTransacao(const NSU,
  CodigoAutorizacaoTransacao: string; DataHoraTransacao: TDateTime;
  Valor: Double; const CodigoFinalizacao: string; const Rede: string): Boolean;
begin
  Result := False;
end;

function TACBrTEFAPIComumClass.EfetuarPagamento(ValorPagto: Currency;
  Modalidade: TACBrTEFModalidadePagamento; CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento; Parcelas: Byte;
  DataPreDatado: TDateTime): Boolean;
begin
  Result := False;
end;

procedure TACBrTEFAPIComumClass.FinalizarTransacao(const Rede, NSU,
  CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
begin
  ErroAbstract('FecharOperacao');
end;

procedure TACBrTEFAPIComumClass.ProcessarRespostaOperacaoTEF;
var
  NumRespostas, i: Integer;
begin
  InterpretarRespostaAPI;  // Mapeia valores da resposta em fpACBrTEFAPI
  AtualizarHeader;

  with fpACBrTEFAPI do     // Chama métodos do Componente
  begin
    if UltimaRespostaTEF.Sucesso then
    begin
      NumRespostas := RespostasTEF.Count;
      //HOMOLOGAÇÃO: Insira um Break Point na linha abaixo, se deseja interromper a
      //             aplicação, antes de Componente Criar um Arquivo de Backup da Ultima Transação
      //             (simulação de transação pendente, sem recuperação por arquivo de Backup)
      i := RespostasTEF.AdicionarRespostaTEF(UltimaRespostaTEF);

      if (i < 0) or (NumRespostas < RespostasTEF.Count) then  // Adicionou uma nova resposta ?
      begin
        // Se transação foi OK, e precisa de confirmação, vamos salvar um Backup dela em disco
        if UltimaRespostaTEF.Sucesso and
           ConfirmarTransacaoAutomaticamente and
           UltimaRespostaTEF.Confirmar then
        begin
          FinalizarTransacao(tefstsSucessoAutomatico);
        end;
      end;
    end;

    if Assigned(QuandoFinalizarOperacao) then
    begin
      GravarLog('  QuandoFinalizarOperacao');
      QuandoFinalizarOperacao(UltimaRespostaTEF);
    end;
  end;
end;

procedure TACBrTEFAPIComumClass.ResolverTransacaoPendente(
  AStatus: TACBrTEFStatusTransacao);
begin
  ErroAbstract('ResolverOperacaoPendente');
end;

procedure TACBrTEFAPIComumClass.InterpretarRespostaAPI;
begin
  with fpACBrTEFAPI do
  begin
    UltimaRespostaTEF.Clear;
    UltimaRespostaTEF.TextoEspecialOperador :=
      Format(ACBrStr('%s.InterpretarDadosDaTransacao não implementado.'), [ClassName]);
    UltimaRespostaTEF.Sucesso := False;
  end;
end;

//function TACBrTEFAPIComumClass.VerificarTEF: Boolean;
//begin
//  Result := fpInicializado;
//  { Sobreescrever se houver alguma maneira de Verificar a comunicação com o TEF }
//end;

procedure TACBrTEFAPIComumClass.AtualizarHeader;
var
  AHeader: String;
begin
  // Traduzindo Tipo de Opertação, com Header compatível com ACBrTEFD
  with fpACBrTEFAPI do
  begin
    case fpMetodoOperacao of
      tefmtdPagamento: AHeader := CHEADER_PAGAMENTO;
      tefmtdCancelamento: AHeader := CHEADER_CANCELAMENTO;
    else
      AHeader := CHEADER_ADMINISTRATIVA;
    end;

    TACBrTEFRespHack(UltimaRespostaTEF).fpHeader := AHeader;
    UltimaRespostaTEF.Conteudo.GravaInformacao(899,100, AHeader);
    if (UltimaRespostaTEF.ArqBackup <> '') then
      UltimaRespostaTEF.Conteudo.GravarArquivo(UltimaRespostaTEF.ArqBackup);
  end;
end;

procedure TACBrTEFAPIComumClass.ErroAbstract(const NomeProcedure : String) ;
begin
  fpACBrTEFAPI.DoException(Format( ACBrStr(sACBrTEFAPIMetodoInvalidoException),
                                   [NomeProcedure, ClassName] ));
end ;

procedure TACBrTEFAPIComumClass.VerificarIdentificadorVendaInformado;
begin
  if (Trim(fpACBrTEFAPI.RespostasTEF.IdentificadorTransacao) = '') then
    fpACBrTEFAPI.DoException(sACBrTEFAPIIdentificadorVendaVazioException);
end;

{ TACBrTEFAPIComum }

constructor TACBrTEFAPIComum.Create(AOwner: TComponent);
begin
  inherited;
  fArqLOG := '';
  fConfirmarTransacaoAutomaticamente := True;
  fTratamentoTransacaoPendente := tefpenConfirmar;
  fTratamentoTransacaoInicializacao := tefopiProcessarPendentes;
  fQuandoGravarLog := Nil;
  fQuandoFinalizarOperacao := Nil;
  fQuandoDetectarTransacaoPendente := Nil;

  fDadosAutomacao := TACBrTEFAPIDadosAutomacao.Create;
  fDadosEstabelecimento := TACBrTEFAPIDadosEstabelecimento.Create;
  fDadosTerminal := TACBrTEFAPIDadosTerminal.Create;

  fpTEFAPIClass := TACBrTEFAPIComumClass.Create( Self );
  fpInicializando := False;

  fDiretorioTrabalho := '';
  fGravarRespostas := True;
  fLimparRespostasQuandoNovoIdentificador := True;
  fRespostasTEF := Nil;

  CriarListaTEFResp;
end;

procedure TACBrTEFAPIComum.CriarListaTEFResp;
begin
  if Assigned(fRespostasTEF) then
    fRespostasTEF.Free;

  fRespostasTEF := TACBrTEFAPIRespostas.Create( fpTEFAPIClass.TEFRespClass,
                                                fDiretorioTrabalho,
                                                fGravarRespostas,
                                                fLimparRespostasQuandoNovoIdentificador);
  CriarTEFResp;
end;

procedure TACBrTEFAPIComum.CriarTEFResp;
begin
  if Assigned(fUltimaRespostaTEF) then
    FreeAndNil(fUltimaRespostaTEF);

  fUltimaRespostaTEF := fpTEFAPIClass.TEFRespClass.Create;
end;

destructor TACBrTEFAPIComum.Destroy;
begin
  fpTEFAPIClass.Free;
  fDadosAutomacao.Free;
  fDadosEstabelecimento.Free;
  fDadosTerminal.Free;
  if Assigned(fRespostasTEF) then
    fRespostasTEF.Free;
  if Assigned(fUltimaRespostaTEF) then
    fUltimaRespostaTEF.Free;

  inherited;
end;

procedure TACBrTEFAPIComum.DoException(const AErrorMsg: String);
begin
  if (Trim(AErrorMsg) = '') then
    Exit;

  GravarLog('EACBrTEFAPIErro: '+AErrorMsg);
  raise EACBrTEFAPIErro.Create(AErrorMsg);
end;

procedure TACBrTEFAPIComum.Inicializar;
begin
  GravarLog('Inicializar');

  if not Assigned( fQuandoFinalizarOperacao )  then
    DoException( Format( ACBrStr(sACBrTEFAPIEventoInvalidoException),
                         ['QuandoFinalizarOperacao']) );

  if (TratamentoTransacaoPendente = tefpenPerguntar) and
     (not (Assigned( fQuandoDetectarTransacaoPendente ))) then
    DoException( Format( ACBrStr(sACBrTEFAPIEventoInvalidoException),
                         ['QuandoDetectarTransacaoPendente']) );

  if (fArqLOG <> '') then
  begin
    if (ExtractFilePath(fArqLOG) = '') then
      fArqLOG := ApplicationPath + fArqLOG;
  end;

  fpInicializando := True;
  try
    fpTEFAPIClass.Inicializar;
    CriarListaTEFResp;

    // Verificando se ficou alguma Transação Pendente, no Diretório de Trabalho
    if (TratamentoTransacaoInicializacao = tefopiCancelarOuEstornar) then
      CancelarOuEstornarTransacoesDiretorioTrabalho
    else
    begin
      fRespostasTEF.CarregarRespostasDoDiretorioTrabalho;

      if (TratamentoTransacaoInicializacao = tefopiProcessarPendentes) then
        VerificarTransacoesPendentes;
    end;
  finally
    fpInicializando := False;
  end;
end;

procedure TACBrTEFAPIComum.DesInicializar;
begin
  GravarLog('DesInicializar');
  CriarListaTEFResp;
  fpTEFAPIClass.DesInicializar;
end;

function TACBrTEFAPIComum.GetInicializado: Boolean;
begin
  Result := fpTEFAPIClass.Inicializado;
end;

procedure TACBrTEFAPIComum.SetInicializado(const AValue: Boolean);
begin
  if AValue then
    Inicializar
  else
    DesInicializar;
end;

procedure TACBrTEFAPIComum.SetLimparRespostasQuandoNovoIdentificador(const AValue: Boolean);
begin
  if fLimparRespostasQuandoNovoIdentificador = AValue then
    Exit;

  if Inicializado then
    DoException( Format( ACBrStr(sACBrTEFAPIComponenteInicializadoException),
                         ['LimparRespostasQuandoNovoIdentificador']) );

  fLimparRespostasQuandoNovoIdentificador := AValue;
end;

procedure TACBrTEFAPIComum.SetTratamentoTransacaoPendente(
  const AValue: TACBrTEFTratamentoTransacaoPendente);
begin
  if fTratamentoTransacaoPendente = AValue then
    Exit;

  if Inicializado then
    DoException( Format( ACBrStr(sACBrTEFAPIComponenteInicializadoException),
                         ['TratamentoTransacaoPendente']) );

  fTratamentoTransacaoPendente := AValue;
end;

procedure TACBrTEFAPIComum.SetDadosAutomacao(const AValue: TACBrTEFAPIDadosAutomacao);
begin
  if (fDadosAutomacao <> AValue) then
    fDadosAutomacao.Assign(AValue);
end;

procedure TACBrTEFAPIComum.SetDadosEstabelecimento(const AValue: TACBrTEFAPIDadosEstabelecimento);
begin
  if (fDadosEstabelecimento <> AValue) then
    fDadosEstabelecimento.Assign(AValue);
end;

procedure TACBrTEFAPIComum.SetDadosTerminal(const AValue: TACBrTEFAPIDadosTerminal);
begin
  if (fDadosTerminal <> AValue) then
    fDadosTerminal.Assign(AValue);
end;

procedure TACBrTEFAPIComum.SetTratamentoTransacaoInicializacao(
  AValue: TACBrTEFTratamentoTransacaoInicializacao);
begin
  if (fTratamentoTransacaoInicializacao = AValue) then
    Exit;

  if Inicializado then
    DoException( Format( ACBrStr(sACBrTEFAPIComponenteInicializadoException),
                         ['TratamentoTransacaoInicalizacao']) );

  fTratamentoTransacaoInicializacao := AValue;
end;

procedure TACBrTEFAPIComum.SetGravarRespostas(const AValue: Boolean);
begin
  if fGravarRespostas = AValue then
    Exit;

  if Inicializado then
    DoException( Format( ACBrStr(sACBrTEFAPIComponenteInicializadoException),
                         ['GravarRespostas']) );

  fGravarRespostas := AValue;
end;

function TACBrTEFAPIComum.GetEmTransacao: Boolean;
begin
  Result := (fpTEFAPIClass.OperacaoEmAndamento <> tefmtdNenhuma);
end;

function TACBrTEFAPIComum.EfetuarAdministrativa(const Operacao: string;
  const IdentificadorTransacao: string): Boolean;
begin
  GravarLog('EfetuarAdministrativa( '+Operacao+', '+IdentificadorTransacao+' )');

  fRespostasTEF.IdentificadorTransacao := IdentificadorTransacao;

  fpTEFAPIClass.InicializarChamadaAPI(tefmtdAdministrativa);
  try
    Result := fpTEFAPIClass.EfetuarAdministrativa(Operacao);
  finally
    fpTEFAPIClass.FinalizarChamadaAPI;
  end;
end;

function TACBrTEFAPIComum.EfetuarAdministrativa(Operacao: TACBrTEFOperacao;
  const IdentificadorTransacao: string): Boolean;
begin
  GravarLog('EfetuarAdministrativa( '+
            GetEnumName(TypeInfo(TACBrTEFOperacao), integer(Operacao) )+', '+
            IdentificadorTransacao+' )');

  fRespostasTEF.IdentificadorTransacao := IdentificadorTransacao;

  fpTEFAPIClass.InicializarChamadaAPI(tefmtdAdministrativa);
  try
    Result := fpTEFAPIClass.EfetuarAdministrativa(Operacao);
  finally
    fpTEFAPIClass.FinalizarChamadaAPI;
  end;
end;

function TACBrTEFAPIComum.EfetuarPagamento(
  const IdentificadorTransacao: String; ValorPagto: Currency;
  Modalidade: TACBrTEFModalidadePagamento; CartoesAceitos: TACBrTEFTiposCartao;
  Financiamento: TACBrTEFModalidadeFinanciamento; Parcelas: Byte;
  DataPreDatado: TDateTime): Boolean;
var
  StrCartoesAceitos: String;
  i: TACBrTEFTipoCartao;
begin
  StrCartoesAceitos := '';
  for i := Low(TACBrTEFTipoCartao) to High(TACBrTEFTipoCartao) do
  begin
    if (i in CartoesAceitos) then
      StrCartoesAceitos := StrCartoesAceitos + GetEnumName(TypeInfo(TACBrTEFTipoCartao), Integer(i))+', ';
  end;
  if (StrCartoesAceitos <> '') then
    StrCartoesAceitos := '[ '+copy(StrCartoesAceitos, 1, Length(StrCartoesAceitos)-2) +' ]'
  else
    StrCartoesAceitos := '[]';

  GravarLog('EfetuarPagamento( '+
            IdentificadorTransacao+', '+
            FormatFloatBr(ValorPagto)+', '+
            GetEnumName(TypeInfo(TACBrTEFModalidadePagamento), integer(Modalidade) )+', '+
            GetEnumName(TypeInfo(TACBrTEFModalidadeFinanciamento), integer(Financiamento) )+', '+
            StrCartoesAceitos+
            IfThen(Parcelas = 0, '', ', '+IntToStr(Parcelas))+
            IfThen(DataPreDatado = 0, '', ', '+FormatDateBr(DataPreDatado))+
            ' )');

  fRespostasTEF.IdentificadorTransacao := IdentificadorTransacao;

  fpTEFAPIClass.InicializarChamadaAPI(tefmtdPagamento);
  try
    Result := fpTEFAPIClass.EfetuarPagamento( ValorPagto, Modalidade,
                                              CartoesAceitos, Financiamento,
                                              Parcelas, DataPreDatado );
  finally
    fpTEFAPIClass.FinalizarChamadaAPI;
  end;
end;

function TACBrTEFAPIComum.CancelarTransacao(const NSU,
  CodigoAutorizacaoTransacao: string; DataHoraTransacao: TDateTime;
  Valor: Double; const CodigoFinalizacao: string; const Rede: string): Boolean;
begin
  GravarLog( 'CancelarTransacao( '+
             NSU+', '+
             CodigoAutorizacaoTransacao+', '+
             FormatDateTimeBr(DataHoraTransacao)+', '+
             FormatFloatBr(Valor)+', '+
             CodigoFinalizacao+', '+
             Rede+' )' );

  fpTEFAPIClass.InicializarChamadaAPI(tefmtdCancelamento);
  try
    Result := fpTEFAPIClass.CancelarTransacao( NSU, CodigoAutorizacaoTransacao,
                                               DataHoraTransacao, Valor,
                                               CodigoFinalizacao, Rede );
  finally
    fpTEFAPIClass.FinalizarChamadaAPI;
  end;
end;

procedure TACBrTEFAPIComum.FinalizarTransacao(const Rede, NSU,
  CodigoFinalizacao: String; AStatus: TACBrTEFStatusTransacao);
var
  i: Integer;
  ATEFResp: TACBrTEFResp;
begin
  GravarLog( '    FinalizarTransacao( '+
             Rede+', '+
             NSU+', '+
             CodigoFinalizacao+', '+
             GetEnumName(TypeInfo(TACBrTEFStatusTransacao), integer(AStatus))+' )');

  fpTEFAPIClass.FinalizarTransacao(Rede, NSU, CodigoFinalizacao, AStatus);

  i := RespostasTEF.AcharTransacao(Rede, NSU, CodigoFinalizacao);
  if (i >= 0) then
  begin
    ATEFResp := fRespostasTEF[i];
    fRespostasTEF.AtualizarTransacaoComTerceiraPerna(ATEFResp);

    if Assigned(fQuandoFinalizarTransacao) then
    begin
      GravarLog('      QuandoFinalizarTransacao');
      fQuandoFinalizarTransacao(ATEFResp, AStatus);
    end;
  end;
end;

procedure TACBrTEFAPIComum.FinalizarTransacao(AStatus: TACBrTEFStatusTransacao);
begin
  GravarLog( '  FinalizarTransacao( '+
             GetEnumName(TypeInfo(TACBrTEFStatusTransacao), integer(AStatus))+' )');

  //HOMOLOGAÇÃO: Insira um Break Point na linha abaixo, se deseja interromper a
  //             aplicação, antes de Confirmar a Ultima Transação
  //             (simulação de transação pendente)
  FinalizarTransacao( UltimaRespostaTEF.Rede,
                      UltimaRespostaTEF.NSU,
                      UltimaRespostaTEF.Finalizacao,
                      AStatus );
end;

procedure TACBrTEFAPIComum.ProcessarTransacaoPendente(const MsgErro: String);
var
  AStatus: TACBrTEFStatusTransacao;
begin
  GravarLog( 'ProcessarTransacaoPendente( '+ MsgErro+' )' );
  if (TratamentoTransacaoPendente = tefpenPerguntar) then
  begin
    GravarLog( '  QuandoDetectarTransacaoPendente' );
    QuandoDetectarTransacaoPendente(UltimaRespostaTEF, MsgErro)
  end
  else
  begin
    if (TratamentoTransacaoPendente = tefpenEstornar) then
    begin
      if fpInicializando then
        AStatus := tefstsErroEnergia
      else
        AStatus := tefstsErroDiverso;
    end
    else
    begin
      if fpInicializando then
        AStatus := tefstsSucessoManual
      else
        AStatus := tefstsSucessoAutomatico;
    end;

    ResolverTransacaoPendente( AStatus );
  end;
end;

procedure TACBrTEFAPIComum.ResolverTransacaoPendente(
  Status: TACBrTEFStatusTransacao);
begin
  GravarLog( 'ResolverOperacaoPendente( '+
             GetEnumName(TypeInfo(TACBrTEFStatusTransacao), integer(Status))+' )');

  fpTEFAPIClass.ResolverTransacaoPendente(Status);
  fRespostasTEF.AtualizarTransacaoComTerceiraPerna(UltimaRespostaTEF);
end;

procedure TACBrTEFAPIComum.AbortarTransacaoEmAndamento;
begin
  GravarLog( 'AbortarTransacaoEmAndamento');
  fpTEFAPIClass.AbortarTransacaoEmAndamento;
end;

procedure TACBrTEFAPIComum.LimparRespostasTEF;
begin
  GravarLog('LimparRespostasTEF');
  RespostasTEF.LimparRespostasTEF;
end;

procedure TACBrTEFAPIComum.CarregarRespostasDoDiretorioTrabalho;
begin
  GravarLog('CarregarRespostasDoDiretorioTrabalho');
  RespostasTEF.CarregarRespostasDoDiretorioTrabalho;
end;

procedure TACBrTEFAPIComum.VerificarTransacoesPendentes;
var
  i: Integer;
  MsgErro: String;
begin
  GravarLog('VerificarTransacoesPendentes');
  for i := 0 to RespostasTEF.Count-1 do
  begin
    UltimaRespostaTEF.Assign(RespostasTEF[i]);
    if UltimaRespostaTEF.Confirmar then    // Transação requer Confirmação ?
    begin
      if not UltimaRespostaTEF.CNFEnviado then    // Ainda não enviou a Terceira Perna ?
      begin
        MsgErro := Format( ACBrStr(sACBrTEFAPITransacaoPendente),
                           [UltimaRespostaTEF.Rede, UltimaRespostaTEF.NSU] );
        ProcessarTransacaoPendente( MsgErro );
      end;
    end
  end;
end;

procedure TACBrTEFAPIComum.ConfirmarTransacoesPendentes;
var
  AStatus: TACBrTEFStatusTransacao;
begin
  GravarLog('ConfirmarTransacoesPendentes');
  if fpInicializando then
    AStatus := tefstsSucessoManual
  else
    AStatus := tefstsSucessoAutomatico;

  FinalizarTransacoesPendentes( AStatus );
end;

procedure TACBrTEFAPIComum.EstornarTransacoesPendentes;
var
  AStatus: TACBrTEFStatusTransacao;
begin
  GravarLog('EstornarTransacoesPendentes');
  if fpInicializando then
    AStatus := tefstsErroEnergia
  else
    AStatus := tefstsErroDiverso;

  FinalizarTransacoesPendentes( AStatus );
end;

procedure TACBrTEFAPIComum.CancelarOuEstornarTransacoesDiretorioTrabalho;
var
  RespostasTEFAtuais: TACBrTEFAPIRespostas;
  i, p: Integer;
  ATEFResp: TACBrTEFResp;
  Processar, EhPagamento: Boolean;
  AStatus: TACBrTEFStatusTransacao;
begin
  GravarLog('CancelarOuEstornarTransacoesDiretorioTrabalho( '+fDiretorioTrabalho+' )');

  fRespostasTEF.CarregarRespostasDoDiretorioTrabalho;

  // Cria uma nova Lista com as respostas da Pasta de Trabalho, pois a operação
  // de Cancelamento, pode gerar novas entradas na lista padrão (fRespostasTEF)
  RespostasTEFAtuais := TACBrTEFAPIRespostas.Create( fpTEFAPIClass.TEFRespClass,
                                                     fDiretorioTrabalho,
                                                     False, False);
  try
    RespostasTEFAtuais.CarregarRespostasDoDiretorioTrabalho;

    p := 0;  // No Passo 0, vamos processar primeiro os CNCs e ADMs, e as Não Confirmadas, depois Todas
    i := 0;
    while (i < RespostasTEFAtuais.Count) and (p < 2) do
    begin
      while (i < RespostasTEFAtuais.Count) do
      begin
        ATEFResp := RespostasTEFAtuais[i];
        EhPagamento := (ATEFResp.Header = CHEADER_PAGAMENTO);

        Processar := (p > 0) or
                     ( (not EhPagamento) or (not ATEFResp.CNFEnviado) );

        if Processar then
        begin
          if ATEFResp.CNFEnviado then    // Já enviou Terceira Perna ?
          begin
            if EhPagamento then      // Não tente Cancelar os Cancelamentos...
              CancelarTransacao( ATEFResp.NSU,
                                 ATEFResp.CodigoAutorizacaoTransacao,
                                 ATEFResp.DataHoraTransacaoLocal,
                                 ATEFResp.ValorTotal,
                                 ATEFResp.Finalizacao,
                                 ATEFResp.Rede )
          end

          else if ATEFResp.Confirmar then    // Transação requer Confirmação ?
          begin
            if EhPagamento then
            begin
              if fpInicializando then
                AStatus := tefstsErroEnergia
              else
                AStatus := tefstsErroDiverso;
            end
            else
              AStatus := tefstsSucessoManual; // Queremos Confirmar os Cancelamentos...

            FinalizarTransacao( ATEFResp.Rede,
                                ATEFResp.NSU,
                                ATEFResp.Finalizacao,
                                AStatus );
          end;

          RespostasTEFAtuais.ApagarRespostaTEF(i);  // Apaga arquivo de Backup
          RespostasTEF.ApagarRespostaTEF(i);        // Apaga também, da lista oficial
        end
        else
          Inc(i);
      end;

      Inc(p);
      i := 0;
    end;
  finally
    RespostasTEFAtuais.Free;
  end;
end;

procedure TACBrTEFAPIComum.FinalizarTransacoesPendentes(Status: TACBrTEFStatusTransacao);
var
  i: Integer;
  ATEFResp: TACBrTEFResp;
begin
  GravarLog('FinalizarTransacoes( '+
            GetEnumName(TypeInfo(TACBrTEFStatusTransacao), integer(Status) )+' )');

  for i := 0 to RespostasTEF.Count-1 do
  begin
    ATEFResp := RespostasTEF[i];
    if ATEFResp.Confirmar and            // Transação requer Confirmação ?
       (not ATEFResp.CNFEnviado) then    // Ainda não enviou Terceira Perna ?
    begin
      FinalizarTransacao( ATEFResp.Rede,
                          ATEFResp.NSU,
                          ATEFResp.Finalizacao,
                          Status );
    end;
  end;
end;


//function TACBrTEFAPIComum.VerificarTEF: Boolean;
//begin
//  Result := fTEFAndroidClass.VerificarTEF;
//end;

procedure TACBrTEFAPIComum.GravarLog(const AString: AnsiString);
var
  Tratado: Boolean;
begin
  Tratado := False;
  if Assigned( fQuandoGravarLog ) then
    fQuandoGravarLog( AString, Tratado );

  if not Tratado then
    GravarLogEmArquivo( AString );
end;

procedure TACBrTEFAPIComum.GravarLogEmArquivo(const aString: AnsiString);
begin
  if (fArqLOG <> '') then
    WriteLog( fArqLOG, FormatDateTime('dd/mm/yy hh:nn:ss:zzz',now) + ' - ' + AString );
end;

function TACBrTEFAPIComum.GetDiretorioTrabalho: String;
begin
  if (fDiretorioTrabalho = '') then
    if not (csDesigning in Self.ComponentState) then
      fDiretorioTrabalho := ApplicationPath + CDIRETORIO_TRABALHO_PADRAO + PathDelim;

  Result := fDiretorioTrabalho;
end;

procedure TACBrTEFAPIComum.SetDiretorioTrabalho(const AValue: String);
begin
  if (fDiretorioTrabalho = AValue) then
    Exit;

  if Inicializado then
    DoException( Format( ACBrStr(sACBrTEFAPIComponenteInicializadoException),
                         ['DiretorioTrabalho']) );

  fDiretorioTrabalho := PathWithDelim(AValue);
end;

end.

