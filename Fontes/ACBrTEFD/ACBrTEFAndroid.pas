{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2021 Daniel Simoes de Almeida               }
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

unit ACBrTEFAndroid;

interface

uses
  Classes, SysUtils,
  System.UITypes,
  System.Generics.Collections,
  {$IfDef ANDROID}
  Androidapi.JNI.GraphicsContentViewText,
  {$EndIf}
  ACBrBase, ACBrTEFComum;

resourcestring
  sACBrTEFAndroidMetodoInvalidoException = 'Método: %s '+ sLineBreak +
                                           'não implementado para o TEF: %s';
  sACBrTEFAndroidEventoInvalidoException = 'Evento %s não foi implementado na sua aplicação';
  sACBrTEFAndroidComponenteInicializadoException = '%s não pode ser modificado com o ACBrTEFAndroid Inicializado';
  sACBrTEFAndroidArquivoNaoExistenteException = 'Arquivo %s não Encontrado';
  sACBrTEFAndroidPlataformException = '%s disponível apenas para Android';
  sACBrTEFAndroidDiretorioInvalido = 'Diretorio %s não existe';
  sACBrTEFAndroidSemTransacaoPendenteException = 'Não há transação Pendente';
  sACBrTEFAndroidTransacaoNaoEncontradaException = 'Transação não encontrada [%s]';


type
  TACBrTEFAndroidModelo = (tefNenhum, tefPayGo);

  EACBrTEFAndroid = class(EACBrTEFErro);

  TACBrTEFAndroid = class;

  { TACBrTEFAndroidClass }

  TACBrTEFAndroidClass = class
  private
    procedure ErroAbstract(const NomeProcedure: String);

  protected
    fpACBrTEFAndroid: TACBrTEFAndroid;
    fpInicializado: Boolean;
    fpEmTransacao: Boolean;

    fpTransacaoOK: Boolean;
    fpMensagemResultado: String;

    procedure InicializarOperacaoTEF; virtual;
    procedure FinalizarOperacaoTEF; virtual;
    procedure InterpretarDadosDaTransacao; virtual;

    procedure ProcessarRespostaOperacaoTEF; virtual;
    procedure ProcessarTransacaoPendente(MsgErro: String); virtual;

  public
    constructor Create(AACBrTEFAndroid: TACBrTEFAndroid);
    destructor Destroy; override;

    procedure Inicializar; virtual;
    procedure DesInicializar; virtual;
    procedure Autenticar; virtual;

//  function VerificarTEF: Boolean; virtual;

    procedure EfetuarPagamento(
      IdentificadorVenda: String;
      ValorPagto: Currency;
      CartoesAceitos: TACBrTEFTiposCartao = [];
      Modalidade: TACBrTEFModalidadePagamento = TACBrTEFModalidadePagamento.tefmpNaoDefinido;
      Financiamento: TACBrTEFModalidadeFinanciamento = TACBrTEFModalidadeFinanciamento.tefmfNaoDefinido;
      Parcelas: Byte = 0;
      DataPreDatado: TDate = 0 ); virtual;

    procedure EfetuarAdministrativa(
      Operacao: string = '';
      IdentificadorVenda: string = ''); virtual;

    procedure CancelarTransacao(
      NSU, CodigoAutorizacaoTransacao: string;
      DataHoraTransacao: TDateTime;
      Valor: Double;
      CodigoFinalizacao: string = '';
      Rede: string = ''); virtual;

    procedure FinalizarOperacao(
      Rede, NSU, CodigoFinalizacao: String;
      Status: TACBrTEFStatusTransacao = TACBrTEFStatusTransacao.tefstsSucessoAutomatico); virtual;

    procedure ResolverOperacaoPendente(
      Status: TACBrTEFStatusTransacao = TACBrTEFStatusTransacao.tefstsSucessoManual); virtual;

    procedure AbortarTransacaoEmAndamento; virtual;

    property Inicializado: Boolean read fpInicializado;
    property EmTransacao: Boolean read fpEmTransacao;
  end;


  { TACBrTEFAndroidDadosAutomacao }

  TACBrTEFAndroidDadosAutomacao = class( TPersistent )
  private
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
      write fImprimeViaClienteReduzida default True;
    property SuportaViasDiferenciadas: Boolean read fSuportaViasDiferenciadas
      write fSuportaViasDiferenciadas default True;
    property UtilizaSaldoTotalVoucher: Boolean read fUtilizaSaldoTotalVoucher
      write fUtilizaSaldoTotalVoucher default False;
    property MoedaISO4217: Integer read fMoedaISO4217 write fMoedaISO4217 default 986;
  end;


  { TACBrTEFAndroidDadosEstabelecimento }

  TACBrTEFAndroidDadosEstabelecimento = class(TPersistent)
  private
    fCNPJ: String;
    fRazaoSocial: String;
    procedure SetRazaoSocial(AValue: String);
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;

  published
    property CNPJ: String read fCNPJ write fCNPJ;
    property RazaoSocial: String read fRazaoSocial write SetRazaoSocial;
  end;


  { TACBrTEFAPIConfigTerminal }

  TACBrTEFAndroidDadosTerminal = class(TPersistent)
  private
    fCodEmpresa: String;
    fCodFilial: String;
    fCodTerminal: String;
    fOperador: String;
    fPortaPinPad: String;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;

  published
    property CodEmpresa: String read fCodEmpresa write fCodEmpresa;
    property CodFilial: String read fCodFilial write fCodFilial;
    property CodTerminal: String read fCodTerminal write fCodTerminal;
    property Operador: String read fOperador write fOperador;
    property PortaPinPad: String read fPortaPinPad write fPortaPinPad;
  end;

  {$IfDef ANDROID}
  TACBrTEFAndroidEstadoTransacao = procedure(AIntent: JIntent) of object;
  {$EndIf}

  TACBrTEFAndroidTerminoDeTransacao = procedure(Sucesso: Boolean; MsgFinal: String;
    RespostaTEF: TACBrTEFResp) of object;

  TACBrTEFAndroidTransacaoPendente = procedure(MsgErro: String;
    RespostaTEF: TACBrTEFResp) of object;


  { TACBrTEFAndroidPersonalizacao }

  TACBrTEFAndroidPersonalizacao = class( TPersistent )
  private
    fcorFundoTela: TAlphaColor;
    fcorFonte: TAlphaColor;
    fcorTextoCaixaEdicao: TAlphaColor;
    fcorFundoCaixaEdicao: TAlphaColor;
    fcorSeparadorMenu: TAlphaColor;
    fcorTeclaLiberadaTeclado: TAlphaColor;
    fcorTeclaPressionadaTeclado: TAlphaColor;
    fcorFundoToolbar: TAlphaColor;
    fcorFonteTeclado: TAlphaColor;
    fcorFundoTeclado: TAlphaColor;
    fArquivoIcone: String;
    fArquivoFonte: String;
  public
    constructor Create;
    procedure Clear;
    procedure Assign(Source: TPersistent); override;

  published
    property corFundoTela: TAlphaColor read fcorFundoTela write fcorFundoTela default 0;
    property corFundoToolbar: TAlphaColor read fcorFundoToolbar write fcorFundoToolbar default 0;
    property corFundoTeclado: TAlphaColor read fcorFundoTeclado write fcorFundoTeclado default 0;
    property corFonte: TAlphaColor read fcorFonte write fcorFonte default 0;
    property corFundoCaixaEdicao: TAlphaColor read fcorFundoCaixaEdicao write fcorFundoCaixaEdicao default 0;
    property corTextoCaixaEdicao: TAlphaColor read fcorTextoCaixaEdicao write fcorTextoCaixaEdicao default 0;
    property corTeclaLiberadaTeclado: TAlphaColor read fcorTeclaLiberadaTeclado write fcorTeclaLiberadaTeclado default 0;
    property corTeclaPressionadaTeclado: TAlphaColor read fcorTeclaPressionadaTeclado write fcorTeclaPressionadaTeclado default 0;
    property corFonteTeclado: TAlphaColor read fcorFonteTeclado write fcorFonteTeclado default 0;
    property corSeparadorMenu: TAlphaColor read fcorSeparadorMenu write fcorSeparadorMenu default 0;
    property ArquivoIcone: String read fArquivoIcone write fArquivoIcone;
    property ArquivoFonte: String read fArquivoFonte write fArquivoFonte;
  end;

{ TRespostasTEFAndroid }

  TACBrTEFAndroidRespostas = class
  private
    fTEFRespList: TObjectList<TACBrTEFResp>;
    fPathBackup: String;
    fIdentificadorTransacao: String;
    fGravarRespostas: Boolean;
    fLimparRespostasPorTransacao: Boolean;
    fPathBakup: String;
    procedure SetIdentificadorTransacao(const Value: String);
    function GetItem(Index: Integer): TACBrTEFResp;
    function GetCount: Integer;
    function GetAllBakupFiles: String;

    function AddClone(ATEFResp: TACBrTEFResp): Integer;
    procedure VerificarPathBackup;
    procedure SalvarRespostaTEF(AIndex: Integer; Sobrescrever: Boolean = True);
  protected
  public
    constructor Create(PathBakup: String; GravarRespostas: Boolean;
      LimparRespostasPorTransacao: Boolean);
    destructor Destroy; override;

    function AdicionarRespostaTEF(ATEFResp: TACBrTEFResp): Integer;
    procedure ApagarRespostasTEF;
    procedure SalvarRespostasTEF;
    procedure LerRespostasTEF;
    function AcharTransacao(Rede, NSU, CodigoFinalizacao: String): Integer;
    procedure ConfirmarTransacao(Rede, NSU, CodigoFinalizacao: String); overload;
    procedure ConfirmarTransacao(ATEFResp: TACBrTEFResp); overload;

    property IdentificadorTransacao: String read fIdentificadorTransacao
      write SetIdentificadorTransacao;
    property Items[Index: Integer]: TACBrTEFResp read GetItem; default;
    property Count: Integer read GetCount;
  end;


  { TACBrTEFAndroid }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllAndroidPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrTEFAndroid = class( TACBrComponent )
  private
    fTEFAndroidClass: TACBrTEFAndroidClass;
    fArqLOG: String;
    fRespostasTEF: TACBrTEFAndroidRespostas;
    fPathBackup: String;
    fGravarRespostas: Boolean;
    fLimparRespostasPorTransacao: Boolean;
    fQuandoGravarLog: TACBrGravarLog;
    fTEFModelo: TACBrTEFAndroidModelo;
    {$IfDef ANDROID}
    fQuandoIniciarTransacao: TACBrTEFAndroidEstadoTransacao;
    {$EndIf}
    fQuandoFinalizarTransacao: TACBrTEFAndroidTerminoDeTransacao;
    fQuandoDetectarTransacaoPendente: TACBrTEFAndroidTransacaoPendente;
    fConfirmarTransacoesAutomaticamente: Boolean;
    fDadosAutomacao: TACBrTEFAndroidDadosAutomacao;
    fPersonalizacao: TACBrTEFAndroidPersonalizacao;
    fDadosEstabelecimento: TACBrTEFAndroidDadosEstabelecimento;
    fDadosTerminal: TACBrTEFAndroidDadosTerminal;
    fAutoConfirmarTransacoesPendente: Boolean;
    fUltimaRespostaTEF: TACBrTEFResp;

    function GetEmTransacao: Boolean;
    function GetInicializado: Boolean;
    procedure SetInicializado(const AValue: Boolean);
    procedure SetModelo(const AValue: TACBrTEFAndroidModelo);
    procedure CreateTEFRespList;

    procedure GravarLogEmArquivo(aString: AnsiString);
    function GetPathBackup: String;
    procedure SetPathBackup(const AValue: String);
    procedure SetDadosAutomacao(const Value: TACBrTEFAndroidDadosAutomacao);
    procedure SetDadosEstabelecimento(const Value: TACBrTEFAndroidDadosEstabelecimento);
    procedure SetDadosTerminal(const Value: TACBrTEFAndroidDadosTerminal);
    procedure SetPersonalizacao(const Value: TACBrTEFAndroidPersonalizacao);
    procedure SetAutoConfirmarTransacoesPendente(const Value: Boolean);
    procedure SetGravarRespostas(const Value: Boolean);
    procedure SetLimparRespostasPorTransacao(const Value: Boolean);
  protected

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Inicializar;
    procedure DesInicializar;
    property Inicializado: Boolean read GetInicializado write SetInicializado;

    procedure EfetuarAdministrativa(
      Operacao: string = '';
      IdentificadorTransacao: string = '');

    procedure EfetuarPagamento(
      IdentificadorTransacao: String;
      ValorPagto: Currency;
      CartoesAceitos: TACBrTEFTiposCartao = [];
      Modalidade: TACBrTEFModalidadePagamento = TACBrTEFModalidadePagamento.tefmpNaoDefinido;
      Financiamento: TACBrTEFModalidadeFinanciamento = TACBrTEFModalidadeFinanciamento.tefmfNaoDefinido;
      Parcelas: Byte = 0;
      DataPreDatado: TDate = 0);

    procedure CancelarTransacao(
      NSU, CodigoAutorizacaoTransacao: string;
      DataHoraTransacao: TDateTime;
      Valor: Double;
      CodigoFinalizacao: string = '';
      Rede: string = '');

    procedure FinalizarOperacao(
      Rede, NSU, CodigoFinalizacao: String;
      Status: TACBrTEFStatusTransacao = TACBrTEFStatusTransacao.tefstsSucessoAutomatico); overload;
    procedure FinalizarOperacao(
      Status: TACBrTEFStatusTransacao = TACBrTEFStatusTransacao.tefstsSucessoAutomatico); overload;

    procedure ResolverOperacaoPendente(
      Status: TACBrTEFStatusTransacao = TACBrTEFStatusTransacao.tefstsSucessoManual);

//  function VerificarTEF: Boolean;
//  procedure AbortarTransacaoEmAndamento;

    procedure GravarLog(AString: AnsiString);

    property RespostasTEF: TACBrTEFAndroidRespostas read fRespostasTEF;
    property UltimaRespostaTEF: TACBrTEFResp read fUltimaRespostaTEF;

    property TEF: TACBrTEFAndroidClass read fTEFAndroidClass;

    property EmTransacao: Boolean read GetEmTransacao;

  published
    property Modelo: TACBrTEFAndroidModelo
      read fTEFModelo write SetModelo default tefNenhum;
    property ArqLOG: String read fArqLOG write fArqLOG;

    property PathBackup: String read GetPathBackup write SetPathBackup ;
    property GravarRespostas: Boolean 
      read fGravarRespostas write SetGravarRespostas default True;
    property LimparRespostasPorTransacao: Boolean
      read fLimparRespostasPorTransacao 
      write SetLimparRespostasPorTransacao default True;

    property ConfirmarTransacoesAutomaticamente: Boolean
      read fConfirmarTransacoesAutomaticamente
      write fConfirmarTransacoesAutomaticamente default True;
    property AutoConfirmarTransacoesPendente: Boolean
      read fAutoConfirmarTransacoesPendente
      write SetAutoConfirmarTransacoesPendente default False;

    property DadosAutomacao: TACBrTEFAndroidDadosAutomacao
      read fDadosAutomacao write SetDadosAutomacao;
    property Personalizacao: TACBrTEFAndroidPersonalizacao
      read fPersonalizacao write SetPersonalizacao;
    property DadosEstabelecimento: TACBrTEFAndroidDadosEstabelecimento
      read fDadosEstabelecimento write SetDadosEstabelecimento;
    property DadosTerminal: TACBrTEFAndroidDadosTerminal
      read fDadosTerminal write SetDadosTerminal;

    property QuandoGravarLog: TACBrGravarLog read fQuandoGravarLog write fQuandoGravarLog;
    {$IfDef ANDROID}
    property QuandoIniciarTransacao: TACBrTEFAndroidEstadoTransacao
      read fQuandoIniciarTransacao
      write fQuandoIniciarTransacao;
    {$EndIf}
    property QuandoFinalizarTransacao: TACBrTEFAndroidTerminoDeTransacao
      read fQuandoFinalizarTransacao
      write fQuandoFinalizarTransacao;
    property QuandoDetectarTransacaoPendente: TACBrTEFAndroidTransacaoPendente
      read fQuandoDetectarTransacaoPendente
      write fQuandoDetectarTransacaoPendente;
  end;

implementation

uses
  StrUtils,
  ACBrUtil
  {$IfDef ANDROID}
  ,ACBrTEFAndroidPayGo
  {$EndIf}
  ;

{ TRespostasTEFAndroid }

constructor TACBrTEFAndroidRespostas.Create(PathBakup: String; GravarRespostas: Boolean; LimparRespostasPorTransacao: Boolean);
begin
  inherited Create;

  fLimparRespostasPorTransacao := LimparRespostasPorTransacao;
  fGravarRespostas := GravarRespostas;
  fPathBackup := PathWithDelim(Trim(PathBakup));

  fTEFRespList := TObjectList<TACBrTEFResp>.Create(True);
end;

destructor TACBrTEFAndroidRespostas.Destroy;
begin
  fTEFRespList.Free;
  inherited;
end;

function TACBrTEFAndroidRespostas.AcharTransacao(Rede, NSU,
  CodigoFinalizacao: String): Integer;
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
       (ATEFResp.Finalizacao = CodigoFinalizacao) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TACBrTEFAndroidRespostas.ConfirmarTransacao(Rede, NSU,
  CodigoFinalizacao: String);
var
  i: Integer;
begin
  i := AcharTransacao(Rede, NSU, CodigoFinalizacao);
  if (i >= 0) then
    ConfirmarTransacao(Items[i]);
end;

procedure TACBrTEFAndroidRespostas.ConfirmarTransacao(ATEFResp: TACBrTEFResp);
begin
  if not Assigned(ATEFResp) then
    Exit;

  ATEFResp.CNFEnviado := True;
  if (ATEFResp.ArqBackup <> '') then
    ATEFResp.Conteudo.GravarArquivo(ATEFResp.ArqBackup);
end;

function TACBrTEFAndroidRespostas.GetAllBakupFiles: String;
begin
  Result := fPathBackup + 'ACBr_*.tef' ;
end;

function TACBrTEFAndroidRespostas.GetCount: Integer;
begin
  Result := fTEFRespList.Count;
end;

function TACBrTEFAndroidRespostas.GetItem(Index: Integer): TACBrTEFResp;
begin
  Result := fTEFRespList[Index];
end;

procedure TACBrTEFAndroidRespostas.LerRespostasTEF;
var
  SL: TStringList;
  Arquivo: String;
  NovaResp: TACBrTEFResp;
begin
  fTEFRespList.Clear;
  SL := TStringList.Create;
  try
    FindFiles(GetAllBakupFiles, SL, True, fstFileName, fsdAscending );
    for Arquivo in SL do
    begin
      NovaResp := TACBrTEFResp.Create;
      NovaResp.LeArquivo(Arquivo);
      NovaResp.ArqBackup := Arquivo;

      fTEFRespList.Add(NovaResp);
      fIdentificadorTransacao := NovaResp.DocumentoVinculado;
    end;
  finally
    SL.Free;
  end;
end;

procedure TACBrTEFAndroidRespostas.ApagarRespostasTEF;
begin
  fTEFRespList.Clear;
  DeleteFiles(GetAllBakupFiles, False);
end;

procedure TACBrTEFAndroidRespostas.SalvarRespostasTEF;
var
  i: Integer;
begin
  for i := 0 to fTEFRespList.Count-1 do
    SalvarRespostaTEF(i);
end;

procedure TACBrTEFAndroidRespostas.VerificarPathBackup;
begin
  if (fPathBackup = '') then
    fPathBackup := ApplicationPath + 'tef' + PathDelim;

  if not DirectoryExists(fPathBackup) then
    ForceDirectories(fPathBackup);

  if not DirectoryExists(fPathBackup) then
    raise EACBrTEFAndroid.CreateFmt( sACBrTEFAndroidDiretorioInvalido, [fPathBackup] ) ;
end;

procedure TACBrTEFAndroidRespostas.SalvarRespostaTEF(AIndex: Integer; Sobrescrever: Boolean);
var
  ArqBackup: string;
begin
  if (AIndex < 0) or (AIndex >= fTEFRespList.Count) then
    Exit;

  if not fTEFRespList[AIndex].Confirmar then  // Transação TEF, precisa confirmação da 3a perna ?
    Exit;

  VerificarPathBackup;
  ArqBackup := fPathBackup + 'ACBr_'+IntToStrZero(AIndex,3) +'.tef' ;
  if Sobrescrever or (not FileExists(ArqBackup)) then
  begin
    fTEFRespList[AIndex].ArqBackup := ArqBackup;
    fTEFRespList[AIndex].Conteudo.GravarArquivo(ArqBackup);
  end;
end;

procedure TACBrTEFAndroidRespostas.SetIdentificadorTransacao(const Value: String);
begin
  if (fIdentificadorTransacao = Value) then
    Exit;

  if fLimparRespostasPorTransacao then
    ApagarRespostasTEF;
    
  fIdentificadorTransacao := Value;
end;

function TACBrTEFAndroidRespostas.AddClone(ATEFResp: TACBrTEFResp): Integer;
var
  NovaResp: TACBrTEFResp;
begin
  NovaResp := TACBrTEFResp.Create;
  NovaResp.Assign(ATEFResp);
  Result := fTEFRespList.Add(NovaResp);
end;

function TACBrTEFAndroidRespostas.AdicionarRespostaTEF(ATEFResp: TACBrTEFResp): Integer;
begin

  if ATEFResp.Rede.Trim.IsEmpty and
     ATEFResp.NSU.Trim.IsEmpty and
     ATEFResp.Finalizacao.Trim.IsEmpty then
  begin
    Result := -1;
    Exit;
  end;

  // Verificando se essa transação já está na lista
  Result := AcharTransacao(ATEFResp.Rede, ATEFResp.NSU, ATEFResp.Finalizacao);

  if (Result < 0) then  // Se não estiver, crie um clone e adicione na lista
  begin
    Result := AddClone(ATEFResp);
    if fGravarRespostas then
      SalvarRespostaTEF(Result);
  end;
end;

{ TACBrTEFAndroidClass }

constructor TACBrTEFAndroidClass.Create(AACBrTEFAndroid: TACBrTEFAndroid);
begin
  inherited Create;
  fpACBrTEFAndroid := AACBrTEFAndroid;
  fpInicializado := False;
  fpEmTransacao := False;
  fpMensagemResultado := '';
  fpTransacaoOK := False;
end;

destructor TACBrTEFAndroidClass.Destroy;
begin
  inherited;
end;

procedure TACBrTEFAndroidClass.Inicializar;
begin
  fpInicializado := True;
end;

procedure TACBrTEFAndroidClass.DesInicializar;
begin
  fpInicializado := False;
end;

procedure TACBrTEFAndroidClass.AbortarTransacaoEmAndamento;
begin
  ErroAbstract('AbortarTransacaoEmAndamento');
end;

procedure TACBrTEFAndroidClass.Autenticar;
begin
  { Nada a fazer, sobreescrever se necessário }
end;

procedure TACBrTEFAndroidClass.InicializarOperacaoTEF;
begin
  { Nada a fazer, sobreescrever se necessário }
end;

procedure TACBrTEFAndroidClass.EfetuarAdministrativa(Operacao: string = '';
  IdentificadorVenda: string = '');
begin
  ErroAbstract('EfetuarAdministrativa');
end;

procedure TACBrTEFAndroidClass.CancelarTransacao(NSU,
  CodigoAutorizacaoTransacao: string; DataHoraTransacao: TDateTime;
  Valor: Double; CodigoFinalizacao, Rede: string);
begin
  ErroAbstract('CancelarTransacao');
end;

procedure TACBrTEFAndroidClass.EfetuarPagamento(IdentificadorVenda: String;
  ValorPagto: Currency; CartoesAceitos: TACBrTEFTiposCartao;
  Modalidade: TACBrTEFModalidadePagamento;
  Financiamento: TACBrTEFModalidadeFinanciamento;
  Parcelas: Byte; DataPreDatado: TDate);
begin
  ErroAbstract('EfetuarPagamento');
end;

procedure TACBrTEFAndroidClass.FinalizarOperacao(Rede, NSU,
  CodigoFinalizacao: String; Status: TACBrTEFStatusTransacao);
begin
  ErroAbstract('FecharOperacao');
end;

procedure TACBrTEFAndroidClass.FinalizarOperacaoTEF;
begin
  ErroAbstract('FinalizarOperação');
end;

procedure TACBrTEFAndroidClass.ProcessarRespostaOperacaoTEF;
var
  NumRespostas, i: Integer;
begin
  InterpretarDadosDaTransacao;  // Mapeia valores da resposta em UltimaRespostaTEF, fpTransacaoOK, fpMensagemResultado

  with fpACBrTEFAndroid do
  begin
    NumRespostas := RespostasTEF.Count;
    i := RespostasTEF.AdicionarRespostaTEF(UltimaRespostaTEF);

    if (i < 0) or (NumRespostas < RespostasTEF.Count) then  // Adicionou uma nova resposta ?
    begin
      // Se transação foi OK, e precisa de confirmação, vamos salvar um Backup dela em disco
      if fpTransacaoOK and
         ConfirmarTransacoesAutomaticamente and
         UltimaRespostaTEF.Confirmar then
        FinalizarOperacao(tefstsSucessoAutomatico);

      QuandoFinalizarTransacao( fpTransacaoOK, fpMensagemResultado,
                                UltimaRespostaTEF);
    end;
  end;
end;

procedure TACBrTEFAndroidClass.ProcessarTransacaoPendente(MsgErro: String);
begin
  if fpACBrTEFAndroid.AutoConfirmarTransacoesPendente then
    ResolverOperacaoPendente(TACBrTEFStatusTransacao.tefstsSucessoAutomatico)
  else
    fpACBrTEFAndroid.QuandoDetectarTransacaoPendente(MsgErro, fpACBrTEFAndroid.UltimaRespostaTEF);
end;

procedure TACBrTEFAndroidClass.ResolverOperacaoPendente(
  Status: TACBrTEFStatusTransacao);
begin
  ErroAbstract('ResolverOperacaoPendente');
end;

procedure TACBrTEFAndroidClass.InterpretarDadosDaTransacao;
begin
  fpACBrTEFAndroid.UltimaRespostaTEF.Clear;
  fpMensagemResultado := Format('%s.InterpretarDadosDaTransacao não implementado.', [ClassName]);
  fpTransacaoOK := False;
end;

//function TACBrTEFAndroidClass.VerificarTEF: Boolean;
//begin
//  Result := fpInicializado;
//  { Sobreescrever se houver alguma maneira de Verificar a comunicação com o TEF }
//end;

procedure TACBrTEFAndroidClass.ErroAbstract(const NomeProcedure : String) ;
begin
  raise EACBrTEFAndroid.CreateFmt( sACBrTEFAndroidMetodoInvalidoException,
                                   [NomeProcedure, ClassName] ) ;
end ;


{ TACBrTEFAndroid }

constructor TACBrTEFAndroid.Create(AOwner: TComponent);
begin
  inherited;
  fArqLOG := '';
  fConfirmarTransacoesAutomaticamente := True;
  fAutoConfirmarTransacoesPendente := False;
  fQuandoGravarLog := Nil;
  {$IfDef ANDROID}
  fQuandoIniciarTransacao := Nil;
  {$EndIf}
  fQuandoFinalizarTransacao := Nil;
  fQuandoDetectarTransacaoPendente := Nil;

  fDadosAutomacao := TACBrTEFAndroidDadosAutomacao.Create;
  fPersonalizacao := TACBrTEFAndroidPersonalizacao.Create;
  fDadosEstabelecimento := TACBrTEFAndroidDadosEstabelecimento.Create;
  fDadosTerminal := TACBrTEFAndroidDadosTerminal.Create;

  fTEFModelo := tefNenhum;
  fTEFAndroidClass := TACBrTEFAndroidClass.Create( Self );

  fUltimaRespostaTEF := TACBrTEFResp.Create;

  fPathBackup := '';
  fGravarRespostas := True;
  fLimparRespostasPorTransacao:= True;
  fRespostasTEF := Nil;
  CreateTEFRespList;
end;

procedure TACBrTEFAndroid.CreateTEFRespList;
begin
  if Assigned(fRespostasTEF) then
    fRespostasTEF.Free;
    
  fRespostasTEF := TACBrTEFAndroidRespostas.Create(fPathBackup, fGravarRespostas, fLimparRespostasPorTransacao);
end;

destructor TACBrTEFAndroid.Destroy;
begin
  fUltimaRespostaTEF.Free;
  fTEFAndroidClass.Free;
  fDadosAutomacao.Free;
  fDadosEstabelecimento.Free;
  fDadosTerminal.Free;
  fPersonalizacao.Free;
  if Assigned(fRespostasTEF) then
    fRespostasTEF.Free;

  inherited;
end;

procedure TACBrTEFAndroid.Inicializar;
begin
  if not Assigned( fQuandoFinalizarTransacao )  then
    raise EACBrTEFAndroid.CreateFmt( sACBrTEFAndroidEventoInvalidoException,
                                     ['QuandoFinalizarTransacao']);

  if (not fAutoConfirmarTransacoesPendente) and
     (not Assigned( fQuandoDetectarTransacaoPendente ))  then
    raise EACBrTEFAndroid.CreateFmt( sACBrTEFAndroidEventoInvalidoException,
                                     ['QuandoDetectarTransacaoPendente']);

  if not fPersonalizacao.ArquivoIcone.IsEmpty then
    if not FileExists(fPersonalizacao.ArquivoIcone) then
      raise EACBrTEFAndroid.CreateFmt( sACBrTEFAndroidArquivoNaoExistenteException,
                                       [fPersonalizacao.ArquivoIcone]);

  if not fPersonalizacao.ArquivoFonte.IsEmpty then
    if not FileExists(fPersonalizacao.ArquivoFonte) then
      raise EACBrTEFAndroid.CreateFmt( sACBrTEFAndroidArquivoNaoExistenteException,
                                       [fPersonalizacao.ArquivoFonte]);

  if not fArqLOG.IsEmpty then
  begin
    if ExtractFilePath(fArqLOG).IsEmpty then
      fArqLOG := ApplicationPath + fArqLOG;
  end;
  fTEFAndroidClass.Inicializar;
  CreateTEFRespList;
end;

procedure TACBrTEFAndroid.DesInicializar;
begin
  CreateTEFRespList;
  fTEFAndroidClass.DesInicializar;
end;

function TACBrTEFAndroid.GetInicializado: Boolean;
begin
  Result := fTEFAndroidClass.Inicializado;
end;


procedure TACBrTEFAndroid.SetInicializado(const AValue: Boolean);
begin
  if AValue then
    Inicializar
  else
    DesInicializar;
end;

procedure TACBrTEFAndroid.SetLimparRespostasPorTransacao(const Value: Boolean);
begin
  if fLimparRespostasPorTransacao = Value then
    Exit;

  if Inicializado then
    raise EACBrTEFAndroid.CreateFmt(sACBrTEFAndroidComponenteInicializadoException, ['LimparRespostasPorTransacao']);

  fLimparRespostasPorTransacao := Value;
end;

procedure TACBrTEFAndroid.SetAutoConfirmarTransacoesPendente(const Value: Boolean);
begin
  if fAutoConfirmarTransacoesPendente = Value then
    Exit;

  if Inicializado then
    raise EACBrTEFAndroid.CreateFmt(sACBrTEFAndroidComponenteInicializadoException, ['AutoConfirmarTransacoesPendente']);

  fAutoConfirmarTransacoesPendente := Value;
end;

procedure TACBrTEFAndroid.SetDadosAutomacao(const Value: TACBrTEFAndroidDadosAutomacao);
begin
  if (fDadosAutomacao <> Value) then
    fDadosAutomacao.Assign(Value);
end;

procedure TACBrTEFAndroid.SetDadosEstabelecimento(const Value: TACBrTEFAndroidDadosEstabelecimento);
begin
  if (fDadosEstabelecimento <> Value) then
    fDadosEstabelecimento.Assign(Value);
end;

procedure TACBrTEFAndroid.SetDadosTerminal(const Value: TACBrTEFAndroidDadosTerminal);
begin
  if (fDadosTerminal <> Value) then
    fDadosTerminal.Assign(Value);
end;

procedure TACBrTEFAndroid.SetGravarRespostas(const Value: Boolean);
begin
  if fGravarRespostas = Value then
    Exit;

  if Inicializado then
    raise EACBrTEFAndroid.CreateFmt(sACBrTEFAndroidComponenteInicializadoException, ['GravarRespostas']);

  fGravarRespostas := Value;
end;

procedure TACBrTEFAndroid.SetPersonalizacao(const Value: TACBrTEFAndroidPersonalizacao);
begin
  if (fPersonalizacao <> Value) then
    fPersonalizacao.Assign(Value);
end;

function TACBrTEFAndroid.GetEmTransacao: Boolean;
begin
  Result := fTEFAndroidClass.EmTransacao;
end;

procedure TACBrTEFAndroid.EfetuarAdministrativa(Operacao: string = '';
  IdentificadorTransacao: string = '');
begin
  fRespostasTEF.IdentificadorTransacao := IdentificadorTransacao;

  fTEFAndroidClass.InicializarOperacaoTEF;
  try
    fTEFAndroidClass.EfetuarAdministrativa(Operacao, IdentificadorTransacao);
  finally
    fTEFAndroidClass.FinalizarOperacaoTEF;
  end;
end;

procedure TACBrTEFAndroid.EfetuarPagamento(IdentificadorTransacao: String;
  ValorPagto: Currency; CartoesAceitos: TACBrTEFTiposCartao;
  Modalidade: TACBrTEFModalidadePagamento;
  Financiamento: TACBrTEFModalidadeFinanciamento;
  Parcelas: Byte; DataPreDatado: TDate);
begin
  fRespostasTEF.IdentificadorTransacao := IdentificadorTransacao;

  fTEFAndroidClass.InicializarOperacaoTEF;
  try
    fTEFAndroidClass.EfetuarPagamento( IdentificadorTransacao, ValorPagto,
                                       CartoesAceitos, Modalidade, Financiamento,
                                       Parcelas, DataPreDatado );
  finally
    fTEFAndroidClass.FinalizarOperacaoTEF;
  end;
end;

procedure TACBrTEFAndroid.CancelarTransacao(NSU,
  CodigoAutorizacaoTransacao: string; DataHoraTransacao: TDateTime;
  Valor: Double; CodigoFinalizacao: string = ''; Rede: string = '');
begin
  fTEFAndroidClass.InicializarOperacaoTEF;
  try
    fTEFAndroidClass.CancelarTransacao( NSU, CodigoAutorizacaoTransacao,
                                        DataHoraTransacao, Valor,
                                        CodigoFinalizacao, Rede );
  finally
    fTEFAndroidClass.FinalizarOperacaoTEF;
  end;
end;

procedure TACBrTEFAndroid.FinalizarOperacao(Rede, NSU, CodigoFinalizacao: String;
  Status: TACBrTEFStatusTransacao);
begin
  fTEFAndroidClass.FinalizarOperacao(Rede, NSU, CodigoFinalizacao, Status);
  fRespostasTEF.ConfirmarTransacao(Rede, NSU, CodigoFinalizacao);
end;

procedure TACBrTEFAndroid.FinalizarOperacao(Status: TACBrTEFStatusTransacao);
begin
  FinalizarOperacao( UltimaRespostaTEF.Rede,
                     UltimaRespostaTEF.NSU,
                     UltimaRespostaTEF.Finalizacao,
                     Status);
end;

procedure TACBrTEFAndroid.ResolverOperacaoPendente(
  Status: TACBrTEFStatusTransacao);
begin
  fTEFAndroidClass.ResolverOperacaoPendente(Status);
end;

//function TACBrTEFAndroid.VerificarTEF: Boolean;
//begin
//  Result := fTEFAndroidClass.VerificarTEF;
//end;

//procedure TACBrTEFAndroid.AbortarTransacaoEmAndamento;
//begin
//  fTEFAndroidClass.AbortarTransacaoEmAndamento;
//end;

procedure TACBrTEFAndroid.GravarLog(AString: AnsiString);
var
  Tratado: Boolean;
begin
  Tratado := False;
  if Assigned( fQuandoGravarLog ) then
    fQuandoGravarLog( AString, Tratado );

  if not Tratado then
    GravarLogEmArquivo( AString );
end;

procedure TACBrTEFAndroid.GravarLogEmArquivo(aString: AnsiString);
begin
  if (fArqLOG <> '') then
    WriteLog( fArqLOG, FormatDateTime('dd/mm/yy hh:nn:ss:zzz',now) + ' - ' + AString );
end;

procedure TACBrTEFAndroid.SetModelo(const AValue: TACBrTEFAndroidModelo);
begin
  if fTEFModelo = AValue then exit ;

  if Inicializado then
    raise EACBrTEFAndroid.CreateFmt(sACBrTEFAndroidComponenteInicializadoException, ['Modelo']);

  FreeAndNil( fTEFAndroidClass ) ;

  { Instanciando uma nova classe de acordo com AValue }
  case AValue of
    tefPayGo :
      begin
       {$IfDef ANDROID}
        fTEFAndroidClass := TACBrTEFAndroidPayGoClass.Create( Self );
       {$Else}
        fTEFAndroidClass := TACBrTEFAndroidClass.Create( Self );
        raise EACBrTEFAndroid.CreateFmt( sACBrTEFAndroidPlataformException, ['tefPayGo']);
       {$EndIf}
      end
  else
    fTEFAndroidClass := TACBrTEFAndroidClass.Create( Self );
  end;

  fTEFModelo := AValue;
end;

function TACBrTEFAndroid.GetPathBackup: String;
begin
  if fPathBackup.IsEmpty then
    if not (csDesigning in Self.ComponentState) then
      fPathBackup := ApplicationPath + 'tef';

  Result := fPathBackup;
end;

procedure TACBrTEFAndroid.SetPathBackup(const AValue: String);
begin
  if fPathBackup = AValue then
    Exit;

  if Inicializado then
    raise EACBrTEFAndroid.CreateFmt(sACBrTEFAndroidComponenteInicializadoException,['PathBackup']);

  fPathBackup := PathWithDelim(AValue);
end;

{ TACBrTEFAndroidDadosAutomacao }

constructor TACBrTEFAndroidDadosAutomacao.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrTEFAndroidDadosAutomacao.Clear;
begin
  fNomeSoftwareHouse := '';
  fNomeAplicacao := '';
  fVersaoAplicacao := '';
  fImprimeViaClienteReduzida := True;
  fSuportaDesconto := False;
  fSuportaSaque := False;
  fSuportaViasDiferenciadas := True;
  fUtilizaSaldoTotalVoucher := False;
  fMoedaISO4217 := 986;
end;

procedure TACBrTEFAndroidDadosAutomacao.Assign(Source: TPersistent);
begin
  if (Source is TACBrTEFAndroidDadosAutomacao) then
  begin
    fNomeSoftwareHouse := TACBrTEFAndroidDadosAutomacao(Source).NomeSoftwareHouse;
    fNomeAplicacao := TACBrTEFAndroidDadosAutomacao(Source).NomeAplicacao;
    fVersaoAplicacao := TACBrTEFAndroidDadosAutomacao(Source).VersaoAplicacao;
    fImprimeViaClienteReduzida := TACBrTEFAndroidDadosAutomacao(Source).ImprimeViaClienteReduzida;
    fSuportaDesconto := TACBrTEFAndroidDadosAutomacao(Source).SuportaDesconto;
    fSuportaSaque := TACBrTEFAndroidDadosAutomacao(Source).SuportaSaque;
    fSuportaViasDiferenciadas := TACBrTEFAndroidDadosAutomacao(Source).SuportaViasDiferenciadas;
    fUtilizaSaldoTotalVoucher := TACBrTEFAndroidDadosAutomacao(Source).UtilizaSaldoTotalVoucher;
  end;

end;

procedure TACBrTEFAndroidDadosAutomacao.SetNomeAplicacao(const AValue: String);
begin
  if fNomeAplicacao = AValue then Exit;
  fNomeAplicacao := LeftStr(Trim(AValue),128);
end;

procedure TACBrTEFAndroidDadosAutomacao.SetNomeSoftwareHouse(const AValue: String);
begin
  if fNomeSoftwareHouse = AValue then Exit;
  fNomeSoftwareHouse := LeftStr(Trim(AValue),128);
end;

procedure TACBrTEFAndroidDadosAutomacao.SetVersaoAplicacao(const AValue: String);
begin
  if fVersaoAplicacao = AValue then Exit;
  fVersaoAplicacao := LeftStr(Trim(AValue),128);
end;

{ TACBrTEFAndroidPersonalizacao }

constructor TACBrTEFAndroidPersonalizacao.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrTEFAndroidPersonalizacao.Clear;
begin
  fcorFundoTela := 0;
  fcorFonte := 0;
  fcorTextoCaixaEdicao := 0;
  fcorFundoCaixaEdicao := 0;
  fcorSeparadorMenu := 0;
  fcorTeclaLiberadaTeclado := 0;
  fcorTeclaPressionadaTeclado := 0;
  fcorFundoToolbar := 0;
  fcorFonteTeclado := 0;
  fcorFundoTeclado := 0;
  fArquivoIcone := '';
  fArquivoFonte := '';
end;

procedure TACBrTEFAndroidPersonalizacao.Assign(Source: TPersistent);
begin
  if (Source is TACBrTEFAndroidPersonalizacao) then
  begin
    fcorFundoTela := TACBrTEFAndroidPersonalizacao(Source).corFundoTela;
    fcorFonte := TACBrTEFAndroidPersonalizacao(Source).corFonte;
    fcorTextoCaixaEdicao := TACBrTEFAndroidPersonalizacao(Source).corTextoCaixaEdicao;
    fcorFundoCaixaEdicao := TACBrTEFAndroidPersonalizacao(Source).corFundoCaixaEdicao;
    fcorSeparadorMenu := TACBrTEFAndroidPersonalizacao(Source).corSeparadorMenu;
    fcorTeclaLiberadaTeclado := TACBrTEFAndroidPersonalizacao(Source).corTeclaLiberadaTeclado;
    fcorTeclaPressionadaTeclado := TACBrTEFAndroidPersonalizacao(Source).corTeclaPressionadaTeclado;
    fcorFundoToolbar := TACBrTEFAndroidPersonalizacao(Source).corFundoToolbar;
    fcorFonteTeclado := TACBrTEFAndroidPersonalizacao(Source).corFonteTeclado;
    fcorFundoTeclado := TACBrTEFAndroidPersonalizacao(Source).corFundoTeclado;
    fArquivoIcone := TACBrTEFAndroidPersonalizacao(Source).ArquivoIcone;
    fArquivoFonte := TACBrTEFAndroidPersonalizacao(Source).ArquivoFonte;
  end;
end;

{ TACBrTEFAndroidDadosEstabelecimento }

constructor TACBrTEFAndroidDadosEstabelecimento.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrTEFAndroidDadosEstabelecimento.Clear;
begin
  fCNPJ := '';
  fRazaoSocial := '';
end;

procedure TACBrTEFAndroidDadosEstabelecimento.Assign(Source: TPersistent);
begin
  if (Source is TACBrTEFAndroidDadosEstabelecimento) then
  begin
    fCNPJ := TACBrTEFAndroidDadosEstabelecimento(Source).CNPJ;
    fRazaoSocial := TACBrTEFAndroidDadosEstabelecimento(Source).RazaoSocial;
  end;
end;

procedure TACBrTEFAndroidDadosEstabelecimento.SetRazaoSocial(AValue: String);
begin
  if fRazaoSocial = AValue then
    Exit;
  fRazaoSocial := Trim(AValue);
end;

{ TACBrTEFAndroidDadosTerminal }

constructor TACBrTEFAndroidDadosTerminal.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrTEFAndroidDadosTerminal.Clear;
begin
  fCodEmpresa := '';
  fCodFilial := '';
  fCodTerminal := '';
  fOperador := '';
  fPortaPinPad := '';
end;

procedure TACBrTEFAndroidDadosTerminal.Assign(Source: TPersistent);
begin
  if (Source is TACBrTEFAndroidDadosTerminal) then
  begin
    fCodEmpresa := TACBrTEFAndroidDadosTerminal(Source).CodEmpresa;
    fCodFilial := TACBrTEFAndroidDadosTerminal(Source).CodFilial;
    fCodTerminal := TACBrTEFAndroidDadosTerminal(Source).CodTerminal;
    fOperador := TACBrTEFAndroidDadosTerminal(Source).Operador;
    fPortaPinPad := TACBrTEFAndroidDadosTerminal(Source).PortaPinPad;
  end;
end;

end.

