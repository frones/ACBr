{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrPOSPGWebAPI;

interface

uses
  Classes, SysUtils, syncobjs,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrTEFComum, ACBrTEFPayGoComum, ACBrBase;

resourcestring
  sErrTransacaoNaoIniciada = 'Não foi iniciada uma Transação TEF no Temrinal %s';
  sErrTransacaoJaIniciada = 'Já foi iniciada uma Transação TEF no Temrinal %s';
  sErrLibJaInicializada = 'Biblioteca PTI_DLL já foi inicializada';
  sErrEventoNaoAtribuido = 'Evento %s não atribuido';
  sErrSemComprovante = 'Não há Comprovante a ser impresso';
  sErrPTIRET_UNKNOWN = 'Erro %d';
  sErrPTIRET_INVPARAM = 'Parâmetro inválido informado à função';
  sErrPTIRET_NOCONN = 'O terminal %s está Desconectado.';
  sErrPTIRET_SOCKETERR = 'Erro ao iniciar a escuta da porta TCP %d.';
  sErrPTIRET_WRITEERR = 'Falha de gravação no diretório %s';
  sErrPTIRET_BUSY = 'O terminal %s está ocupado processando outro comando.';
  sErrPTIRET_SECURITYERR = 'A função foi rejeitada por questões de segurança.';
  sErrPTIRET_NOTSUPPORTED = 'Função não suportada pelo terminal';
  sErrPTIRET_PRINTERR = 'Erro na impressora.';
  sErrPTIRET_NOPAPER = 'Impressora sem papel.';
  sErrPTIRET_INTERNALERR = 'Erro Interno';
  sErrPTIRET_EFTERR = 'Falha na execução da Transação TEF';
  sErrPTIRET_BUFOVRFLW = 'O tamanho do dado é maior que o Buffer alocado';


const
  CACBrPOSPGWebAPIName = 'ACBrPOSPGWebAPI';
  CACBrPOSPGWebAPIVersao = '1.1.0';
  CACBrPOSPGWebSubDir = 'POSPGWeb';
  CACBrPOSPGWebBkpFile = 'ACBr_POSTEF_';
  CACBrPOSPGWebBkpExt = '.tef';
  CACBrPOSPGWebColunasDisplay = 20;
  CACBrPOSPGWebColunasImpressora = 40;
  CACBrPOSPGWebPortaTCP = 3433;
  CACBrPOSPGWebMaxTerm = 100;
  CACBrPOSPGWebTempoDesconexao = 60;  // Segundos
  CACBrPOSPGWebEsperaLoop = 500;  // Milissegundos
  CACBrPOSPGWebBoasVindas = 'CONECTADO A '+CACBrPOSPGWebAPIName + ' '+ CACBrPOSPGWebAPIVersao;

  {$IFDEF MSWINDOWS}
   CACBrPOSPGWebLib = 'PTI_DLL.dll';
  {$ELSE}
   CACBrPOSPGWebLib = 'PTI_DLL.so';
  {$ENDIF}

//==========================================================================================
//  Tabela de Códigos de Erro de Retorno da Biblioteca
//==========================================================================================
  PTIRET_OK           = 0;      // Operação bem-sucedida
  PTIRET_INVPARAM     = -2001;  // Parâmetro inválido informado à função.
  PTIRET_NOCONN       = -2002;  // O terminal está offline
  PTIRET_BUSY         = -2003;  // O terminal está ocupado processando outro comando.
  PTIRET_TIMEOUT      = -2004;  // Usuário falhou ao pressionar uma tecla durante o tempo especificado
  PTIRET_CANCEL       = -2005;  // Usuário pressionou a tecla [CANCELA].
  PTIRET_NODATA       = -2006;  // Informação requerida não disponível
  PTIRET_BUFOVRFLW    = -2007;  // Dados maiores que o tamanho do buffer fornecido
  PTIRET_SOCKETERR    = -2008;  // Impossibilitado de iniciar escuta das portas TCP especificadas
  PTIRET_WRITEERR     = -2009;  // Impossibilitado de utilizar o diretório especificado
  PTIRET_EFTERR       = -2010;  // A operação financeira foi completada, porém falhou
  PTIRET_INTERNALERR  = -2011;  // Erro interno da biblioteca de integração
  PTIRET_PROTOCOLERR  = -2012;  // Erro de comunicação entre a biblioteca de integração e o terminal
  PTIRET_SECURITYERR  = -2013;  // A função falhou por questões de segurança
  PTIRET_PRINTERR     = -2014;  // Erro na impressora
  PTIRET_NOPAPER      = -2015;  // Impressora sem papel
  PTIRET_NEWCONN      = -2016;  // Novo terminal conectado
  PTIRET_NONEWCONN    = -2017;  // Sem recebimento de novas conexões.
  PTIRET_NOTSUPPORTED = -2057;  // Função não suportada pelo terminal.
  PTIRET_CRYPTERR     = -2058;  // Erro na criptografia de dados (comunicação entre a biblioteca de integração e o terminal).

//==========================================================================================
// Definicoes das teclas do POS
//==========================================================================================
  PTIKEY_BACKSP  = 8;
  PTIKEY_OK      = 13;
  PTIKEY_CANCEL  = 27;
  PTIKEY_HASH    = 35; // '#'
  PTIKEY_STAR    = 42; // '*'
  PTIKEY_DOT     = 46; // '.'
  PTIKEY_0       = 48;
  PTIKEY_1       = 49;
  PTIKEY_2       = 50;
  PTIKEY_3       = 51;
  PTIKEY_4       = 52;
  PTIKEY_5       = 53;
  PTIKEY_6       = 54;
  PTIKEY_7       = 55;
  PTIKEY_8       = 56;
  PTIKEY_9       = 57;
  PTIKEY_00      = 37; // '00'
  PTIKEY_FUNC0   = 97;
  PTIKEY_FUNC1   = 98;
  PTIKEY_FUNC2   = 99;
  PTIKEY_FUNC3   = 100;
  PTIKEY_FUNC4   = 101;
  PTIKEY_FUNC5	 = 102;
  PTIKEY_FUNC6	 = 103;
  PTIKEY_FUNC7	 = 104;
  PTIKEY_FUNC8	 = 105;
  PTIKEY_FUNC9	 = 106;
  PTIKEY_FUNC10	 = 107;
  PTIKEY_TOUCH   = 126;
  PTIKEY_ALPHA   = 38;

type
  EACBrPOSPGWeb = class(Exception);

  TACBrPOSPGWebOperacao = (
    operVenda = 33,          // Pagamento de mercadorias ou serviços.
    operAdmin = 32,          // Qualquer transação que não seja um pagamento (estorno, pré-autorização, consulta, relatório, reimpressão de recibo, etc.).
    operCancelarVenda = 34); // Estorna uma transação de venda que foi previamente realizada e confirmada.

  TACBrPOSPGWebBeep = (
     beepOK,      // 0 Sucesso
     beepAlerta,  // 1 Alerta
     beepErro);   // 2 Erro

  TACBrPOSPGWebEstadoTerminal = (
     statConectado,      // 0 Terminal está on-line e aguardando por comandos.
     statOcupado,        // 1 Terminal está on-line, porém ocupado processando um comando.
     statDesconectado,   // 2 Terminal está offline.
     statEsperaConexao,  // 3 Terminal está off-line. A transação continua sendo executada e após sua finalização, o terminal tentará efetuar a reconexão automaticamente
     statTEF);           // 4 Terminal está executando uma Transação TEF

  TACBrPOSPGWebCodBarras = (
     code128 = 2,     // Código de barras padrão 128. Pode-se utilizar aproximadamente 31 caracteres alfanuméricos.
     codeITF = 3,     // Código de barras padrão ITF. Pode-se utilizar aproximadamente 30 caracteres alfanuméricos.
     codeQRCODE = 4); // QR Code. Com aceitação de aproximadamente 600 caracteres alfanuméricos.

  TACBrPOSPGWebComprovantes = (
     prnNaoImprimir,      // 0 - Não imprimir
     prnEstabalecimento,  // 1 - Via do estabelecimento
     prnCliente,          // 2 - Via do portador do cartão
     prnAmbas);           // 3 - Ambas as Vias

  TACBrPOSPGWebStatusTransacao = (
     cnfSucesso = 1,        // Transação confirmada.
     cnfErroImpressao = 2,  // Erro na impressora, desfazer a transação.
     cnfErroDispesador = 3, // Erro com o mecanismo dispensador, desfazer a transação.
     cnfErroDiverso = 4);   // Outro erro, desfazer a transação.

  TACBrPOSPGWebAPI = class;
  TACBrPOSPGWebConexao = class;

  TACBrPOSPGWebNovaConexao = procedure(const TerminalId: String;
     const Model: String; const MAC: String; const SerNo: String) of object;

  TACBrPOSPGWebNovoEstadoTerminal = procedure(const TerminalId: String;
     EstadoAtual, EstadoAnterior: TACBrPOSPGWebEstadoTerminal) of object;

  TACBrPOSPGWebAposFinalizarTransacao = procedure(const TerminalId: String;
    Status: TACBrPOSPGWebStatusTransacao) of object ;

  TACBrPOSPGWebAvaliarTransacaoPendente = procedure(const TerminalId: String;
    var Status: TACBrPOSPGWebStatusTransacao;
    const AuthSyst, VirtMerch, AutLocRef, AutExtRef: String) of object;

  { TACBrPOSPGWebConexao }

  TACBrPOSPGWebConexao = class(TThread)
    fPOSPGWeb: TACBrPOSPGWebAPI;
    fTerminalId: String;
    fModel: String;
    fMAC: String;
    fSerNo: String;
    fUltimaLeituraEstado: TDateTime;
    fPndAuthSyst, fPndVirtMerch, fPndAutLocRef, fPndAutExtRef: String;
    fStatus: TACBrPOSPGWebStatusTransacao;
  private
    function GetEstado: TACBrPOSPGWebEstadoTerminal;
    procedure SetEstado(AValue: TACBrPOSPGWebEstadoTerminal);
  protected
    fpEstado, fpEstadoAnterior: TACBrPOSPGWebEstadoTerminal;

    procedure Execute; override;
    procedure Terminate;
    procedure ChamarEventoMudancaDeEstado;
    procedure ChamarEventoTransacaoPendente;
    procedure ChamarEventoFinalizarTransacao;

  public
    constructor Create(POSPGWeb: TACBrPOSPGWebAPI; const TerminalId: String;
      const Model: String; const MAC: String; const SerNo: String);
    destructor Destroy; override;

    procedure AvaliarTransacaoPendente( var AStatus: TACBrPOSPGWebStatusTransacao;
      const PndAuthSyst, PndVirtMerch, PndAutLocRef, PndAutExtRef: String);
    procedure AvisarFinalizarTransacao( AStatus: TACBrPOSPGWebStatusTransacao );

    property TerminalId: String read fTerminalId;
    property Model: String read fModel;
    property MAC: String read fMAC;
    property SerNo: String read fSerNo;

    property Estado: TACBrPOSPGWebEstadoTerminal read GetEstado write SetEstado;
  end;

  { TACBrPOSPGWebAPIParametros }

  TACBrPOSPGWebAPIParametros = class(TACBrTEFParametros)
  private
    fTerminalId: String;
  public
    constructor Create(const TerminalId: String);
    property TerminalId: String read fTerminalId;
  end;

  { TACBrTEFPGWebAPIListaParametros }

  TACBrTEFPGWebAPIListaParametros = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TObject>{$EndIf})
  private
    function GetParametrosAdicionaisPorTerminal(const TerminalId: String
      ): TACBrPOSPGWebAPIParametros;
  protected
    procedure SetObject(Index: Integer; Item: TACBrPOSPGWebAPIParametros);
    function GetObject(Index: Integer): TACBrPOSPGWebAPIParametros;
  public
    function Add(Obj: TACBrPOSPGWebAPIParametros): Integer;
    procedure Insert(Index: Integer; Obj: TACBrPOSPGWebAPIParametros);
    property Objects[Index: Integer]: TACBrPOSPGWebAPIParametros read GetObject write SetObject; default;
    property Terminal[const TerminalId: String]: TACBrPOSPGWebAPIParametros read GetParametrosAdicionaisPorTerminal;
  end;

  { TACBrPOSPGWebAPI }

  TACBrPOSPGWebAPI = class
  private
    fOnAposFinalizarTransacao: TACBrPOSPGWebAposFinalizarTransacao;
    fOnMudaEstadoTerminal: TACBrPOSPGWebNovoEstadoTerminal;
    fOnNovaConexao: TACBrPOSPGWebNovaConexao;
    fOnAvaliarTransacaoPendente: TACBrPOSPGWebAvaliarTransacaoPendente;
    fTimerConexao: TACBrThreadTimer;
    fListaConexoes: TThreadList;
    fDadosTransacaoList: TACBrTEFPGWebAPIListaParametros;
    fACBrTEFPGWebAPIListaParametros: TACBrTEFPGWebAPIListaParametros;
    fLogCriticalSection: TCriticalSection;
    fConfirmarTransacoesPendentes: Boolean;
    fConfirmarAntesImpressao: Boolean;
    fpszTerminalId, fpszModel, fpszMAC, fpszSerNo: PAnsiChar;
    fInicializada: Boolean;
    fEmConnectionLoop: Boolean;
    fCNPJEstabelecimento: String;
    fDiretorioTrabalho: String;
    fImprimirViaClienteReduzida: Boolean;
    fNomeAplicacao: String;
    fOnGravarLog: TACBrGravarLog;
    fPathDLL: String;
    fPortaTCP: Word;
    fMaximoTerminaisConectados: Word;
    fTempoDesconexaoAutomatica: Word;
    fMensagemBoasVindas: String;
    fSoftwareHouse: String;
    fSuportaDesconto: Boolean;
    fSuportaSaque: Boolean;
    fSuportaViasDiferenciadas: Boolean;
    fUtilizaSaldoTotalVoucher: Boolean;
    fVersaoAplicacao: String;

    xPTI_Init: procedure( pszPOS_Company: AnsiString; pszPOS_Version: AnsiString;
      pszPOS_Capabilities: AnsiString; pszDataFolder: AnsiString; uiTCP_Port: Word;
      uiMaxTerminals: Word; pszWaitMsg: AnsiString; uiAutoDiscSec: Word;
      var piRet: SmallInt); cdecl;
    xPTI_End: procedure(); cdecl;
    xPTI_ConnectionLoop: procedure(pszTerminalId: PAnsiChar; pszModel: PAnsiChar;
      pszMAC: PAnsiChar; pszSerNo: PAnsiChar; out piRet: SmallInt); cdecl;
    xPTI_CheckStatus: procedure(pszTerminalId: PAnsiChar; out piStatus: SmallInt;
      pszModel: PAnsiChar; pszMAC: PAnsiChar; pszSerNo: PAnsiChar;
      out piRet: SmallInt); cdecl;
    xPTI_Disconnect: procedure(pszTerminalId: AnsiString; uiPwrDelay: Word;
      out piRet: SmallInt); cdecl;
    xPTI_Display: procedure(pszTerminalId: AnsiString; pszMsg: AnsiString;
      out piRet: SmallInt); cdecl;
    xPTI_WaitKey: procedure(pszTerminalId: AnsiString; uiTimeOutSec: Word;
      out piKey: SmallInt; out piRet: SmallInt); cdecl;
    xPTI_ClearKey: procedure(pszTerminalId: AnsiString; out piRet: SmallInt); cdecl;
    xPTI_GetData: procedure(pszTerminalId: AnsiString; pszPrompt: AnsiString;
      pszFormat: AnsiString; uiLenMin: Word; uiLenMax: Word; fFromLeft: ByteBool;
      fAlpha: ByteBool; fMask: ByteBool; uiTimeOutSec: Word; pszData: PAnsiChar;
      uiCaptureLine: Word; out piRet: SmallInt); cdecl;
    xPTI_StartMenu: procedure(pszTerminalId: AnsiString; out piRet: SmallInt); cdecl;
    xPTI_AddMenuOption: procedure(pszTerminalId: AnsiString; pszOption: AnsiString;
      out piRet: SmallInt); cdecl;
    xPTI_ExecMenu: procedure(pszTerminalId: AnsiString; pszPrompt: AnsiString;
      uiTimeOutSec: Word; var puiSelection: SmallInt; out piRet: SmallInt); cdecl;
    xPTI_Beep: procedure(pszTerminalId: AnsiString; iType: SmallInt;
      out piRet: SmallInt); cdecl;
    xPTI_Print: procedure(pszTerminalId: AnsiString; pszText: AnsiString;
      out piRet: SmallInt); cdecl;
    xPTI_PrnFeed: procedure(pszTerminalId: AnsiString; out piRet: SmallInt); cdecl;
    xPTI_PrnSymbolCode: procedure(pszTerminalId: AnsiString; pszMsg: AnsiString;
      iSymbology: SmallInt; out piRet: SmallInt); cdecl;
    xPTI_EFT_Start: procedure(pszTerminalId: AnsiString; iOper: SmallInt;
      out piRet: SmallInt); cdecl;
    xPTI_EFT_AddParam: procedure(pszTerminalId: AnsiString; iParam: Word;
      pszValue: AnsiString; out piRet: SmallInt); cdecl;
    xPTI_EFT_Exec: procedure(pszTerminalId: AnsiString; out piRet: SmallInt); cdecl;
    xPTI_EFT_GetInfo: procedure(pszTerminalId: AnsiString; iInfo: Word; uiBufLen: Word;
      pszValue: PAnsiChar; out piRet: SmallInt); cdecl;
    xPTI_EFT_PrintReceipt: procedure(pszTerminalId: AnsiString; iCopies: Word;
      out piRet: SmallInt); cdecl;
    xPTI_EFT_Confirm: procedure(pszTerminalId: AnsiString; iStatus: SmallInt;
      out piRet: SmallInt) cdecl;

    function GetDadosTransacao(const TerminalId: String): TACBrPOSPGWebAPIParametros;
    function GetParametrosAdicionais(const TerminalId: String): TACBrPOSPGWebAPIParametros;
    procedure SetDiretorioTrabalho(AValue: String);
    procedure SetInicializada(AValue: Boolean);
    procedure SetNomeAplicacao(AValue: String);
    procedure SetPathDLL(AValue: String);
    procedure SetSoftwareHouse(AValue: String);
    procedure SetVersaoAplicacao(AValue: String);

    procedure OnAguardaConexao(Sender: TObject);

  protected
    procedure LoadDLLFunctions;
    procedure UnLoadDLLFunctions;
    procedure ClearMethodPointers;

    procedure DoException( AErrorMsg: String );

    function CalcularCapacidadesDaAutomacao: Integer;
    function FormatarMensagem(const AMsg: String; Colunas: Word): AnsiString;
    procedure AvaliarErro(iRet: SmallInt; const TerminalId: String);
    procedure VerificarTransacaoFoiIniciada(const TerminalId: String);
    procedure AjustarEstadoConexao(const TerminalId: String; NovoEstado: TACBrPOSPGWebEstadoTerminal);

    property ListaConexoes: TThreadList read fListaConexoes;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Inicializar;
    procedure DesInicializar;

    procedure GravarLog(const AString: AnsiString; Traduz: Boolean = False);

    procedure ExibirMensagem(const TerminalId: String; const AMensagem: String;
      TempoEspera: Word = 0);
    procedure LimparTeclado(const TerminalId: String);
    function AguardarTecla(const TerminalId: String; Espera: Word = 0): Integer;
    function ObterDado(const TerminalId: String; const Titulo: String;
      const Mascara: String = ''; uiLenMin: Word = 1; uiLenMax: Word = 20;
      AlinhaAEsqueda: Boolean = False; PermiteAlfa: Boolean = False;
      OcultarDigitacao: Boolean = False; IntervaloMaxTeclas: Word = 0;
      const ValorInicial: String = ''; LinhaCaptura: Word = 2): String;
    function ExecutarMenu(const TerminalId: String; Opcoes: TStrings;
      const Titulo: String = ''; IntervaloMaxTeclas: Word = 0;
      Opcao: SmallInt = 0): SmallInt;
    procedure Beep(const TerminalId: String; TipoBeep: TACBrPOSPGWebBeep = beepOK);
    procedure ImprimirTexto(const TerminalId: String; const ATexto: String);
    procedure AvancarPapel(const TerminalId: String);
    procedure ImprimirCodBarras(const TerminalId: String; const Codigo: String;
      Tipo: TACBrPOSPGWebCodBarras);
    procedure ImprimirComprovantesTEF(const TerminalId: String;
      Tipo: TACBrPOSPGWebComprovantes = prnAmbas;
      IgnoraErroSemComprovante: Boolean = True);
    procedure ObterEstado(const TerminalId: String;
      out EstadoAtual: TACBrPOSPGWebEstadoTerminal;
      out Modelo: String; out MAC: String; out Serial: String); overload;
    function ObterEstado(const TerminalId: String): TACBrPOSPGWebEstadoTerminal; overload;

    procedure ExecutarTransacaoTEF(const TerminalId: String;
      Operacao: TACBrPOSPGWebOperacao;
      Comprovantes: TACBrPOSPGWebComprovantes = prnAmbas;
      ParametrosAdicionaisTransacao: TStrings = nil);

    procedure IniciarTransacao(const TerminalId: String;
      Operacao: TACBrPOSPGWebOperacao;
      ParametrosAdicionaisTransacao: TStrings = nil);
    procedure AdicionarParametro(const TerminalId: String; iINFO: Word;
      const AValor: AnsiString); overload;
    procedure AdicionarParametro(const TerminalId: String; AKeyValueStr: String);
      overload;
    function ExecutarTransacao(const TerminalId: String): SmallInt;
    function ObterInfo(const TerminalId: String; iINFO: Word): String;
    function ObterUltimoRetorno(const TerminalId: String): String;
    procedure ObterDadosDaTransacao(const TerminalId: String);
    function ObterDadoTransacao(const TerminalId: String; iINFO: Word): String;
    procedure FinalizarTrancao(const TerminalId: String; Status: TACBrPOSPGWebStatusTransacao);
    procedure TratarTransacaoPendente(const TerminalId: String); overload;
    procedure TratarTransacaoPendente(const TerminalId, PndAuthSyst, PndVirtMerch, PndAutLocRef, PndAutExtRef: String); overload;

    function BackupFileName(const TerminalId: String): String;
    procedure GravarBackupDaTransacao(const TerminalId: String);
    procedure ApagarBackupDaTransacao(const TerminalId: String);
    procedure VerificarTransacoesBackup;
    procedure TratarTransacaoBackup(const TerminalId: String);

    function Desconectar(const TerminalId: String; Segundos: Word = 0): Boolean;
    procedure TerminarConexao(const TerminalId: String);
    function TerminarTodasConexoes: Integer;
    function TerminalEstaConectado(const TerminalId: String): Boolean;
    procedure VerificarConexao(const TerminalId: String);

    property PathDLL: String read fPathDLL write SetPathDLL;
    property DiretorioTrabalho: String read fDiretorioTrabalho write SetDiretorioTrabalho;
    property Inicializada: Boolean read fInicializada write SetInicializada;

    property DadosDaTransacao[const TerminalId: String]: TACBrPOSPGWebAPIParametros read GetDadosTransacao;

    property SoftwareHouse: String read fSoftwareHouse write SetSoftwareHouse;
    property NomeAplicacao: String read fNomeAplicacao write SetNomeAplicacao ;
    property VersaoAplicacao: String read fVersaoAplicacao write SetVersaoAplicacao ;

    property PortaTCP: Word read fPortaTCP write fPortaTCP;
    property MaximoTerminaisConectados: Word read fMaximoTerminaisConectados write fMaximoTerminaisConectados;
    property TempoDesconexaoAutomatica: Word read fTempoDesconexaoAutomatica write fTempoDesconexaoAutomatica;
    property MensagemBoasVindas: String read fMensagemBoasVindas write fMensagemBoasVindas;
    property ParametrosAdicionais[const TerminalId: String]: TACBrPOSPGWebAPIParametros read GetParametrosAdicionais;

    Property SuportaSaque: Boolean read fSuportaSaque write fSuportaSaque;
    Property SuportaDesconto: Boolean read fSuportaDesconto write fSuportaDesconto;
    property ImprimirViaClienteReduzida : Boolean read fImprimirViaClienteReduzida
      write fImprimirViaClienteReduzida;
    property SuportaViasDiferenciadas: Boolean read fSuportaViasDiferenciadas
      write fSuportaViasDiferenciadas;
    property UtilizaSaldoTotalVoucher: Boolean read fUtilizaSaldoTotalVoucher
      write fUtilizaSaldoTotalVoucher;
    property ConfirmarTransacoesPendentes: Boolean read fConfirmarTransacoesPendentes
      write fConfirmarTransacoesPendentes;
    property ConfirmarAntesImpressao: Boolean read fConfirmarAntesImpressao
      write fConfirmarAntesImpressao;

    property OnGravarLog: TACBrGravarLog read fOnGravarLog write fOnGravarLog;
    property OnNovaConexao: TACBrPOSPGWebNovaConexao read fOnNovaConexao write fOnNovaConexao;
    property OnMudaEstadoTerminal: TACBrPOSPGWebNovoEstadoTerminal read fOnMudaEstadoTerminal
      write fOnMudaEstadoTerminal;
    property OnAposFinalizarTransacao: TACBrPOSPGWebAposFinalizarTransacao
      read fOnAposFinalizarTransacao write fOnAposFinalizarTransacao;
    property OnAvaliarTransacaoPendente: TACBrPOSPGWebAvaliarTransacaoPendente
      read fOnAvaliarTransacaoPendente write fOnAvaliarTransacaoPendente;
  end;

function PTIRETToString(iRET: SmallInt): String;

implementation

uses
  strutils, math, dateutils,
  ACBrUtil.Math,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrConsts;

function PTIRETToString(iRET: SmallInt): String;
begin
  case iRET of
    PTIRET_OK:           Result := 'PTIRET_OK';
    PTIRET_INVPARAM:     Result := 'PTIRET_INVPARAM';
    PTIRET_NOCONN:       Result := 'PTIRET_NOCONN';
    PTIRET_BUSY:         Result := 'PTIRET_BUSY';
    PTIRET_TIMEOUT:      Result := 'PTIRET_TIMEOUT';
    PTIRET_CANCEL:       Result := 'PTIRET_CANCEL';
    PTIRET_NODATA:       Result := 'PTIRET_NODATA';
    PTIRET_BUFOVRFLW:    Result := 'PTIRET_BUFOVRFLW';
    PTIRET_SOCKETERR:    Result := 'PTIRET_SOCKETERR';
    PTIRET_WRITEERR:     Result := 'PTIRET_WRITEERR';
    PTIRET_EFTERR:       Result := 'PTIRET_EFTERR';
    PTIRET_INTERNALERR:  Result := 'PTIRET_INTERNALERR';
    PTIRET_PROTOCOLERR:  Result := 'PTIRET_PROTOCOLERR';
    PTIRET_SECURITYERR:  Result := 'PTIRET_SECURITYERR';
    PTIRET_PRINTERR:     Result := 'PTIRET_PRINTERR';
    PTIRET_NOPAPER:      Result := 'PTIRET_NOPAPER';
    PTIRET_NEWCONN:      Result := 'PTIRET_NEWCONN';
    PTIRET_NONEWCONN:    Result := 'PTIRET_NONEWCONN';
    PTIRET_NOTSUPPORTED: Result := 'PTIRET_NOTSUPPORTED';
    PTIRET_CRYPTERR:     Result := 'PTIRET_CRYPTERR';
  else
    Result := 'PTIRET_'+IntToStr(iRET);
  end;
end;

{ TACBrPOSPGWebConexao }

constructor TACBrPOSPGWebConexao.Create(POSPGWeb: TACBrPOSPGWebAPI;
  const TerminalId: String; const Model: String; const MAC: String;
  const SerNo: String);
begin
  FreeOnTerminate := True;
  inherited Create(False);

  fPOSPGWeb := POSPGWeb;
  fTerminalId := TerminalId;
  fModel := Model;
  fMAC := MAC;
  fSerNo := SerNo;

  fpEstado := statDesconectado;
  fpEstadoAnterior := statDesconectado;
  fUltimaLeituraEstado := 0;

  fPOSPGWeb.ListaConexoes.Add(Self);
  SetEstado(statConectado);

  // Verificando se há transações pendentes para esse Terminal
  fPOSPGWeb.TratarTransacaoBackup(fTerminalId);
end;

destructor TACBrPOSPGWebConexao.Destroy;
begin
  if not Terminated then
  begin
    Terminate;
    WaitFor;
  end;

  fPOSPGWeb.ListaConexoes.Remove(Self);
  inherited Destroy;
end;

procedure TACBrPOSPGWebConexao.Execute;
begin
  // ATENÇÃO: Todo o código do Evento OnNovaConexao deve ser Thread Safe
  try
    try
      fPOSPGWeb.GravarLog( 'TACBrPOSPGWebConexao.Execute: '+fTerminalId );
      fPOSPGWeb.OnNovaConexao(fTerminalId, fModel, fMAC, fSerNo);
      fPOSPGWeb.GravarLog( 'TACBrPOSPGWebConexao.Done: '+fTerminalId );
    except
      On E: Exception do
      begin
        if fPOSPGWeb.Inicializada then
          fPOSPGWeb.GravarLog( Format( 'TACBrPOSPGWebConexao.Exception: %s, %s - %s',
                                       [fTerminalId, E.ClassName, E.Message] ));
      end;
    end;
  finally
    Terminate;
    fPOSPGWeb.GravarLog( 'TACBrPOSPGWebConexao.End: '+fTerminalId );
  end;
end;

procedure TACBrPOSPGWebConexao.Terminate;
begin
  fPOSPGWeb.GravarLog( 'TACBrPOSPGWebConexao.Terminate: '+fTerminalId );
  inherited Terminate;

  try
    if fPOSPGWeb.Inicializada then
      fPOSPGWeb.Desconectar(TerminalId);
  except
    { Ignora erros }
  end;
end;

function TACBrPOSPGWebConexao.GetEstado: TACBrPOSPGWebEstadoTerminal;
var
  NovoEstado: TACBrPOSPGWebEstadoTerminal;
begin
  // Força a leitura do Estado, se não for Idle ou a cada 5 segundos
  if (fpEstado <> statTEF) then
  begin
    if (fpEstado <> statConectado) or (SecondsBetween(fUltimaLeituraEstado, Now) > 5) then
    begin
      fPOSPGWeb.ObterEstado(fTerminalId, NovoEstado, fModel, fMAC, fSerNo);
      SetEstado(NovoEstado);
    end;
  end;

  Result := fpEstado;
end;

procedure TACBrPOSPGWebConexao.SetEstado(AValue: TACBrPOSPGWebEstadoTerminal);
begin
  if (AValue <> fpEstado) then
  begin
    fpEstadoAnterior := fpEstado;
    fpEstado := AValue;
    fUltimaLeituraEstado := Now;
    if Assigned(fPOSPGWeb.OnMudaEstadoTerminal) then
      Synchronize(ChamarEventoMudancaDeEstado);
  end;
end;

procedure TACBrPOSPGWebConexao.AvaliarTransacaoPendente(
  var AStatus: TACBrPOSPGWebStatusTransacao; const PndAuthSyst, PndVirtMerch,
  PndAutLocRef, PndAutExtRef: String);
begin
  fStatus := AStatus;
  fPndAuthSyst := PndAuthSyst;
  fPndVirtMerch := PndVirtMerch;
  fPndAutLocRef := PndAutLocRef;
  fPndAutExtRef := PndAutExtRef;

  if Assigned(fPOSPGWeb.OnAvaliarTransacaoPendente) then
    Synchronize(ChamarEventoTransacaoPendente);

  AStatus := fStatus;
end;

procedure TACBrPOSPGWebConexao.ChamarEventoMudancaDeEstado;
begin
  if Assigned(fPOSPGWeb.OnMudaEstadoTerminal) then
    fPOSPGWeb.OnMudaEstadoTerminal(fTerminalId, fpEstado, fpEstadoAnterior);
end;

procedure TACBrPOSPGWebConexao.ChamarEventoTransacaoPendente;
begin
  if Assigned(fPOSPGWeb.OnAvaliarTransacaoPendente) then
    fPOSPGWeb.OnAvaliarTransacaoPendente( fTerminalId,
                                          fStatus,
                                          fPndAuthSyst,
                                          fPndVirtMerch,
                                          fPndAutLocRef,
                                          fPndAutExtRef);

end;

procedure TACBrPOSPGWebConexao.AvisarFinalizarTransacao(
  AStatus: TACBrPOSPGWebStatusTransacao);
begin
  fStatus := AStatus;
  if Assigned(fPOSPGWeb.OnAposFinalizarTransacao) then
    Synchronize(ChamarEventoFinalizarTransacao);
end;

procedure TACBrPOSPGWebConexao.ChamarEventoFinalizarTransacao;
begin
  if Assigned(fPOSPGWeb.OnAposFinalizarTransacao) then
    fPOSPGWeb.OnAposFinalizarTransacao(fTerminalId, fStatus);
end;

{ TACBrPOSPGWebAPIParametros }

constructor TACBrPOSPGWebAPIParametros.Create(const TerminalId: String);
begin
  inherited Create;
  fTerminalId := TerminalId;
end;

{ TACBrTEFPGWebAPIListaParametros }

function TACBrTEFPGWebAPIListaParametros.GetParametrosAdicionaisPorTerminal(
  const TerminalId: String): TACBrPOSPGWebAPIParametros;
var
  i: Integer;
begin
  Result := Nil;
  for i := 0 to Count-1 do
  begin
    if Objects[i].TerminalId = TerminalId then
    begin
      Result := Objects[i];
      Break;
    end;
  end;

  if Result = Nil then // Ainda não existe uma Lista para esse Terminal, vamos criá-la
  begin
    i := Add( TACBrPOSPGWebAPIParametros.Create(TerminalId) );
    Result := Objects[i];
  end;
end;

procedure TACBrTEFPGWebAPIListaParametros.SetObject(Index: Integer;
  Item: TACBrPOSPGWebAPIParametros);
begin
  inherited Items[Index] := Item;
end;

function TACBrTEFPGWebAPIListaParametros.GetObject(Index: Integer
  ): TACBrPOSPGWebAPIParametros;
begin
  Result := TACBrPOSPGWebAPIParametros(inherited Items[Index]);
end;

function TACBrTEFPGWebAPIListaParametros.Add(Obj: TACBrPOSPGWebAPIParametros
  ): Integer;
begin
  Result := inherited Add(Obj);
end;

procedure TACBrTEFPGWebAPIListaParametros.Insert(Index: Integer;
  Obj: TACBrPOSPGWebAPIParametros);
begin
  inherited Insert(Index, Obj);
end;

{ TACBrPOSPGWebAPI }

constructor TACBrPOSPGWebAPI.Create;
begin
  inherited Create;
  ClearMethodPointers;

  fSuportaViasDiferenciadas := True;
  fUtilizaSaldoTotalVoucher := False;
  fSuportaDesconto := True;
  fSuportaSaque := False;
  fImprimirViaClienteReduzida := True;

  fCNPJEstabelecimento := '';
  fNomeAplicacao := '';
  fSoftwareHouse := '';
  fVersaoAplicacao := '';

  fDiretorioTrabalho := '';
  fPortaTCP := CACBrPOSPGWebPortaTCP;
  fMaximoTerminaisConectados := CACBrPOSPGWebMaxTerm;
  fTempoDesconexaoAutomatica := CACBrPOSPGWebTempoDesconexao;
  fMensagemBoasVindas := CACBrPOSPGWebBoasVindas;
  fPathDLL := '';

  fpszTerminalId := AllocMem(50);
  fpszModel := AllocMem(50);
  fpszMAC := AllocMem(50);
  fpszSerNo := AllocMem(50);
  fEmConnectionLoop := False;
  fInicializada := False;
  fConfirmarTransacoesPendentes := True;
  fConfirmarAntesImpressao := True;

  fDadosTransacaoList := TACBrTEFPGWebAPIListaParametros.Create(True);  // FreeObjects;
  fACBrTEFPGWebAPIListaParametros := TACBrTEFPGWebAPIListaParametros.Create(True);  // FreeObjects;
  fListaConexoes := TThreadList.Create;
  fTimerConexao := TACBrThreadTimer.Create;
  fTimerConexao.Interval := CACBrPOSPGWebEsperaLoop;
  fTimerConexao.Enabled := False;

  fLogCriticalSection := TCriticalSection.Create;
  fOnGravarLog := Nil;
  fOnNovaConexao := Nil;
  fOnMudaEstadoTerminal := Nil;
  fOnAposFinalizarTransacao := Nil;
  fOnAvaliarTransacaoPendente := Nil;
end;

function TACBrPOSPGWebAPI.TerminarTodasConexoes: Integer;
var
  i: Integer;
  Alist: TList;
begin
  Result := 0;
  Alist := fListaConexoes.LockList;
  try
    Result := Alist.Count;
    GravarLog('TACBrPOSPGWebAPI.TerminarTodasConexoes: '+IntToStr(Result));
    for i := 0 to Alist.Count - 1 do
      TACBrPOSPGWebConexao(Alist[i]).Terminate;
  finally
    fListaConexoes.UnlockList;
  end;
end;

destructor TACBrPOSPGWebAPI.Destroy;
begin
  fOnGravarLog := nil;
  DesInicializar;
  Sleep(CACBrPOSPGWebEsperaLoop);  // Aguarda, caso esteja dentro do Evento "OnAguardaConexao"

  Freemem(fpszTerminalId);
  Freemem(fpszModel);
  Freemem(fpszMAC);
  Freemem(fpszSerNo);

  fLogCriticalSection.Free;
  fDadosTransacaoList.Free;
  fACBrTEFPGWebAPIListaParametros.Free;
  fListaConexoes.Free;
  fTimerConexao.Free;

  inherited Destroy;
end;

procedure TACBrPOSPGWebAPI.DoException(AErrorMsg: String);
begin
  if (Trim(AErrorMsg) = '') then
    Exit;

  GravarLog('TACBrPOSPGWebAPI.EACBrPOSPGWeb: '+AErrorMsg);
  raise EACBrPOSPGWeb.Create(AErrorMsg);
end;

procedure TACBrPOSPGWebAPI.GravarLog(const AString: AnsiString;
  Traduz: Boolean);
Var
  Tratado: Boolean;
  AStringLog: AnsiString;
begin
  if not Assigned(fOnGravarLog) then
    Exit;

  fLogCriticalSection.Acquire;
  try
    if Traduz then
      AStringLog := TranslateUnprintable(AString)
    else
      AStringLog := AString;

    Tratado := False;
    fOnGravarLog(AStringLog, Tratado);
  finally
    fLogCriticalSection.Release;
  end;
end;

procedure TACBrPOSPGWebAPI.Inicializar;
var
  iRet: SmallInt;
  pszWaitMsg, pszPOS_Version: AnsiString;
  MsgError: String;
  POSCapabilities: Integer;
begin
  if fInicializada then
    Exit;

  GravarLog('TACBrPOSPGWebAPI.Inicializar');

  if not Assigned(fOnNovaConexao) then
    DoException(Format(ACBrStr(sErrEventoNaoAtribuido), ['OnNovaConexao']));

  if (fDiretorioTrabalho = '') then
    fDiretorioTrabalho := ApplicationPath  + CACBrPOSPGWebSubDir;

  LoadDLLFunctions;
  if not DirectoryExists(fDiretorioTrabalho) then
    ForceDirectories(fDiretorioTrabalho);

  POSCapabilities := CalcularCapacidadesDaAutomacao;
  pszPOS_Version := Trim(fNomeAplicacao + ' ' + fVersaoAplicacao);
  pszPOS_Version := Trim(pszPOS_Version+ ' ' + CACBrPOSPGWebAPIName + ' '+ CACBrPOSPGWebAPIVersao);

  pszWaitMsg := FormatarMensagem(fMensagemBoasVindas, CACBrPOSPGWebColunasDisplay);

  GravarLog('PTI_Init( '+fSoftwareHouse+', '+
                         pszPOS_Version+', '+
                         IntToStr(POSCapabilities)+', '+
                         fDiretorioTrabalho+', '+
                         IntToStr(fPortaTCP)+', '+
                         IntToStr(fMaximoTerminaisConectados)+', '+
                         pszWaitMsg+', '+
                         IntToStr(fTempoDesconexaoAutomatica)+' )', True);

  iRet := 0;
  xPTI_Init( AnsiString(fSoftwareHouse),
             pszPOS_Version,
             AnsiString(IntToStr(POSCapabilities)),
             AnsiString(fDiretorioTrabalho),
             fPortaTCP, fMaximoTerminaisConectados,
             pszWaitMsg,
             fTempoDesconexaoAutomatica,
             iRet );
  GravarLog('  '+PTIRETToString(iRet));
  case iRet of
    PTIRET_OK: MsgError := '';
    PTIRET_INVPARAM: MsgError := sErrPTIRET_INVPARAM;
    PTIRET_SOCKETERR: MsgError := Format(sErrPTIRET_SOCKETERR, [fPortaTCP]);
    PTIRET_WRITEERR: MsgError := Format(sErrPTIRET_WRITEERR, [fDiretorioTrabalho]);
  else
    MsgError := Format(sErrPTIRET_UNKNOWN, [iRet, 'PTI_Init']);
  end;

  if (MsgError <> '') then
    DoException(ACBrStr(MsgError));

  fInicializada := True;

  fTimerConexao.OnTimer := OnAguardaConexao;
  fTimerConexao.Enabled := True;
end;

procedure TACBrPOSPGWebAPI.DesInicializar;
begin
  if not fInicializada then
    Exit;

  //GravarLog('TACBrPOSPGWebAPI.DesInicializar');
  fTimerConexao.Enabled := False;
  fTimerConexao.OnTimer := Nil;

  TerminarTodasConexoes;
  UnLoadDLLFunctions;
  fInicializada := False;
end;

procedure TACBrPOSPGWebAPI.SetInicializada(AValue: Boolean);
begin
  if (fInicializada = AValue) then
    Exit;

  GravarLog('TACBrPOSPGWebAPI.SetInicializada( '+BoolToStr(AValue, True)+' )');

  if AValue then
    Inicializar
  else
    DesInicializar;
end;

procedure TACBrPOSPGWebAPI.ExibirMensagem(const TerminalId: String;
  const AMensagem: String; TempoEspera: Word);
var
  iRet: SmallInt;
  MsgFormatada: AnsiString;
begin

  MsgFormatada := FormatarMensagem(AMensagem, CACBrPOSPGWebColunasDisplay);
  GravarLog('PTI_Display( '+TerminalId+', '+MsgFormatada+' )', True);
  VerificarConexao(TerminalId);
  xPTI_Display( AnsiString(TerminalId),
                MsgFormatada,
                iRet );
  GravarLog('  '+PTIRETToString(iRet));
  AvaliarErro(iRet, TerminalId);

  if (TempoEspera > 0) then
    AguardarTecla(TerminalId, TempoEspera);
end;

procedure TACBrPOSPGWebAPI.LimparTeclado(const TerminalId: String);
var
  iRet: SmallInt;
begin
  GravarLog('PTI_ClearKey( '+TerminalId+' )');
  VerificarConexao(TerminalId);
  xPTI_ClearKey( AnsiString(TerminalId),
                 iRet );
  GravarLog('  '+PTIRETToString(iRet));
  AvaliarErro(iRet, TerminalId);
end;

function TACBrPOSPGWebAPI.AguardarTecla(const TerminalId: String; Espera: Word
  ): Integer;
var
  iRet, iKey: SmallInt;
begin
  GravarLog('PTI_WaitKey( '+TerminalId+', '+IntToStr(Integer(Espera))+' )');
  VerificarConexao(TerminalId);
  iKey := 0; iRet := 0;
  xPTI_WaitKey( AnsiString(TerminalId),
                Espera,
                iKey,
                iRet );
  GravarLog('  '+PTIRETToString(iRet)+', '+IntToStr(iKey));
  Result := iKey
end;

function TACBrPOSPGWebAPI.ObterDado(const TerminalId: String;
  const Titulo: String; const Mascara: String; uiLenMin: Word; uiLenMax: Word;
  AlinhaAEsqueda: Boolean; PermiteAlfa: Boolean; OcultarDigitacao: Boolean;
  IntervaloMaxTeclas: Word; const ValorInicial: String; LinhaCaptura: Word
  ): String;
var
  iRet: SmallInt;
  pszData: PAnsiChar;
  AValorInicial: AnsiString;
begin
  if (IntervaloMaxTeclas = 0) then
    IntervaloMaxTeclas := fTempoDesconexaoAutomatica;

  GravarLog('PTI_GetData( '+TerminalId+', Titulo:'+Titulo+', Mascara:'+Mascara+', '+
                          'TamMin: '+IntToStr(uiLenMin)+', TamMax:'+IntToStr(uiLenMax)+', '+
                          'AlinhaAEsqueda: '+ifthen(AlinhaAEsqueda,'S','N')+', '+
                          'PermiteAlfa: '+ifthen(PermiteAlfa,'S','N')+', '+
                          'OcultarDigitacao: '+ifthen(OcultarDigitacao,'S','N')+', '+
                          'Intervalo: '+IntToStr(IntervaloMaxTeclas)+', '+
                          'Valor: '+ValorInicial+', Linha:'+IntToStr(LinhaCaptura)+' )');

  VerificarConexao(TerminalId);
  Result := '';
  AValorInicial := AnsiString(ValorInicial);
  pszData := AllocMem(50);
  try
    if (Length(ValorInicial) > 0) then
      Move(AValorInicial[1], pszData^, Length(AValorInicial)+1 );

    xPTI_GetData( AnsiString(TerminalId),
                  AnsiString(Titulo),
                  AnsiString(Mascara),
                  uiLenMin, uiLenMax,
                  AlinhaAEsqueda, PermiteAlfa, OcultarDigitacao,
                  IntervaloMaxTeclas,
                  pszData,
                  LinhaCaptura,
                  iRet );
    GravarLog('  '+PTIRETToString(iRet)+', '+Result);
    if (iRet = PTIRET_OK) then
      Result := String(pszData)
    else if (iRet <> PTIRET_TIMEOUT) and (iRet <> PTIRET_CANCEL) then
      AvaliarErro(iRet, TerminalId);
  finally
    Freemem(pszData);
  end;
end;

function TACBrPOSPGWebAPI.ExecutarMenu(const TerminalId: String;
  Opcoes: TStrings; const Titulo: String; IntervaloMaxTeclas: Word;
  Opcao: SmallInt): SmallInt;
var
  iRet: SmallInt;
  i: Integer;
  pszPrompt, pszOption: AnsiString;
begin
  if (IntervaloMaxTeclas = 0) then
    IntervaloMaxTeclas := fTempoDesconexaoAutomatica;

  Result := -1;
  if (not Assigned(Opcoes)) or (Opcoes.Count < 1) then
    Exit;

  GravarLog('PTI_StartMenu( '+TerminalId+' )');
  VerificarConexao(TerminalId);
  xPTI_StartMenu( AnsiString(TerminalId),
                  iRet );
  GravarLog('  '+PTIRETToString(iRet));

  i := 0;
  while (iRet = PTIRET_OK)  and (i < Opcoes.Count) do
  begin
    pszOption := LeftStr(Opcoes[i], 18);
    GravarLog('PTI_AddMenuOption( '+TerminalId+', '+pszOption+' )');
    xPTI_AddMenuOption( AnsiString(TerminalId),
                        pszOption,
                        iRet );
    GravarLog('  '+PTIRETToString(iRet));
    Inc(i);
  end;

  if (iRet = PTIRET_OK) then
  begin
    Opcao := min( max(0, Opcao), Opcoes.Count-1);
    pszPrompt := LeftStr(Titulo,20);
    GravarLog('PTI_ExecMenu( '+TerminalId+', '+pszPrompt+', '+
                              IntToStr(IntervaloMaxTeclas)+', '+
                              IntToStr(Opcao)+' )');
    xPTI_ExecMenu( AnsiString(TerminalId),
                   pszPrompt,
                   IntervaloMaxTeclas,
                   Opcao,
                   iRet );
    GravarLog('  '+PTIRETToString(iRet)+', Opcao: '+IntToStr(Opcao));
  end;

  if (iRet = PTIRET_OK) then
    Result := Opcao
  else if (iRet = PTIRET_TIMEOUT) or (iRet = PTIRET_CANCEL) then
    Result := -1
  else
    AvaliarErro(iRet, TerminalId);
end;

procedure TACBrPOSPGWebAPI.Beep(const TerminalId: String;
  TipoBeep: TACBrPOSPGWebBeep);
var
  iRet: SmallInt;
begin
  GravarLog('PTI_Beep( '+TerminalId+', '+IntToStr(Integer(TipoBeep))+' )');
  VerificarConexao(TerminalId);
  xPTI_Beep( AnsiString(TerminalId),
             SmallInt(TipoBeep),
             iRet );
  GravarLog('  '+PTIRETToString(iRet));
  AvaliarErro(iRet, TerminalId);
end;

procedure TACBrPOSPGWebAPI.ImprimirTexto(const TerminalId: String;
  const ATexto: String);
var
  iRet: SmallInt;
  TextoFormatado: AnsiString;
begin
  // Trocando comando #11 por Tag <e>, para Expandido. Pois FormatarMensagem() irá remover Acentos e Não caracteres
  TextoFormatado := StringReplace(ATexto, #11, '<e>', [rfReplaceAll]);
  // Formatando no limite de Colunas
  TextoFormatado := FormatarMensagem(TextoFormatado, CACBrPOSPGWebColunasImpressora);
  // Trocando Tag <e> por #11 ( \v ), para Expandido. NOTA: Deve ser o Primeiro caracter da Linha
  TextoFormatado := StringReplace(TextoFormatado, '<e>', #11, [rfReplaceAll]);
  TextoFormatado := StringReplace(TextoFormatado, '<E>', #11, [rfReplaceAll]);

  GravarLog('PTI_Print( '+TerminalId+', '+TextoFormatado+' )', True);
  VerificarConexao(TerminalId);
  xPTI_Print( AnsiString(TerminalId),
              TextoFormatado,
              iRet );
  GravarLog('  '+PTIRETToString(iRet));
  AvaliarErro(iRet, TerminalId);
end;

procedure TACBrPOSPGWebAPI.AvancarPapel(const TerminalId: String);
var
  iRet: SmallInt;
begin
  GravarLog('PTI_PrnFeed( '+TerminalId+' )');
  VerificarConexao(TerminalId);
  xPTI_PrnFeed( AnsiString(TerminalId),
                iRet );
  GravarLog('  '+PTIRETToString(iRet));
  AvaliarErro(iRet, TerminalId);
end;

procedure TACBrPOSPGWebAPI.ImprimirCodBarras(const TerminalId: String;
  const Codigo: String; Tipo: TACBrPOSPGWebCodBarras);
var
  iRet: SmallInt;
begin
  GravarLog('PTI_PrnSymbolCode( '+TerminalId+', '+Codigo+', '+IntToStr(SmallInt(Tipo))+' )', True);
  VerificarConexao(TerminalId);
  xPTI_PrnSymbolCode( AnsiString(TerminalId),
                      AnsiString(Codigo),
                      SmallInt(Tipo),
                      iRet );
  GravarLog('  '+PTIRETToString(iRet));
  AvaliarErro(iRet, TerminalId);
end;

procedure TACBrPOSPGWebAPI.ImprimirComprovantesTEF(const TerminalId: String;
  Tipo: TACBrPOSPGWebComprovantes; IgnoraErroSemComprovante: Boolean);
var
  iRet: SmallInt;
begin
  if Tipo = prnNaoImprimir then
    Exit;

  GravarLog('PTI_EFT_PrintReceipt( '+TerminalId+', '+IntToStr(Word(Tipo))+' )');
  VerificarConexao(TerminalId);
  xPTI_EFT_PrintReceipt( AnsiString(TerminalId),
                         Word(Tipo),
                         iRet );
  GravarLog('  '+PTIRETToString(iRet));
  if (iRet = PTIRET_NODATA) then
  begin
    if not IgnoraErroSemComprovante then
      DoException( ACBrStr(sErrSemComprovante) );
  end
  else
    AvaliarErro(iRet, TerminalId);
end;

procedure TACBrPOSPGWebAPI.ObterEstado(const TerminalId: String; out
  EstadoAtual: TACBrPOSPGWebEstadoTerminal; out Modelo: String; out
  MAC: String; out Serial: String);
var
  iRet, iStatus: SmallInt;
  AAnsiStr: AnsiString;
  pszTerminalId, pszModel, pszMAC, pszSerNo: PAnsiChar;
begin
  pszTerminalId := AllocMem(50);
  pszModel := AllocMem(50);
  pszMAC := AllocMem(50);
  pszSerNo := AllocMem(50);
  try
    GravarLog('PTI_CheckStatus( '+TerminalId+' )');
    AAnsiStr := AnsiString(TerminalId);
    Move( AAnsiStr[1], pszTerminalId^, Length(TerminalId)+1 );
    xPTI_CheckStatus( pszTerminalId,
                      iStatus,
                      pszModel,
                      pszMAC,
                      pszSerNo,
                      iRet );
    if (iRet = PTIRET_OK) then
    begin
      EstadoAtual := TACBrPOSPGWebEstadoTerminal(iStatus);
      Modelo := String(pszModel);
      MAC := String(pszMAC);
      Serial := String(pszSerNo);

      GravarLog('  '+PTIRETToString(iRet)+', Estado:'+IntToStr(iStatus)+', Model:'+Modelo+', MAC:'+MAC+', SerNo:'+Serial);
    end
    else
      GravarLog('  '+PTIRETToString(iRet));
  finally
    Freemem(pszTerminalId);
    Freemem(pszModel);
    Freemem(pszMAC);
    Freemem(pszSerNo);
  end;
end;

function TACBrPOSPGWebAPI.ObterEstado(const TerminalId: String): TACBrPOSPGWebEstadoTerminal;
var
  Alist: TList;
  i: Integer;
  AConexao: TACBrPOSPGWebConexao;
begin
  Result := statDesconectado;
  Alist := fListaConexoes.LockList;
  try
    for i := 0 to Alist.Count-1 do
    begin
      AConexao := TACBrPOSPGWebConexao(Alist[i]);
      if (AConexao.TerminalId = TerminalId) then
      begin
        Result := AConexao.Estado;
        Break;
      end;
    end;
  finally
    fListaConexoes.UnlockList;
  end;
end;

procedure TACBrPOSPGWebAPI.ExecutarTransacaoTEF(const TerminalId: String;
  Operacao: TACBrPOSPGWebOperacao; Comprovantes: TACBrPOSPGWebComprovantes;
  ParametrosAdicionaisTransacao: TStrings);
var
  iRet: SmallInt;
  PndAutLocRef, PndAutExtRef: String;
begin
  GravarLog('TACBrPOSPGWebAPI.ExecutarTransacaoTEF( '+TerminalId+' )');
  try
    VerificarConexao(TerminalId);
    AjustarEstadoConexao(TerminalId, statOcupado);
    IniciarTransacao( TerminalId, Operacao, ParametrosAdicionaisTransacao );
    iRet := ExecutarTransacao( TerminalId );
    ObterDadosDaTransacao(TerminalId);

    if (iRet = PTIRET_OK) then
    begin
      GravarBackupDaTransacao(TerminalId);
      if ConfirmarAntesImpressao then
        FinalizarTrancao( TerminalId, cnfSucesso );

      if (Comprovantes <> prnNaoImprimir) then
      begin
        try
          ImprimirComprovantesTEF( TerminalId, Comprovantes );
        except
          if not ConfirmarAntesImpressao then
            FinalizarTrancao( TerminalId, cnfErroImpressao );
          raise;
        end;
      end;

      if not ConfirmarAntesImpressao then
        FinalizarTrancao( TerminalId, cnfSucesso );
    end
    else
    begin
      PndAutLocRef := ObterDadoTransacao(TerminalId, PWINFO_PNDAUTLOCREF);
      PndAutExtRef := ObterDadoTransacao(TerminalId, PWINFO_PNDAUTEXTREF);
      if (PndAutLocRef <> '') or (PndAutExtRef <> '') then
        TratarTransacaoPendente(TerminalId);

      AvaliarErro(iRet, TerminalId);
    end;
  finally
    AjustarEstadoConexao(TerminalId, statConectado);
  end;
end;

procedure TACBrPOSPGWebAPI.IniciarTransacao(const TerminalId: String;
  Operacao: TACBrPOSPGWebOperacao; ParametrosAdicionaisTransacao: TStrings);
var
  iRet: SmallInt;
  i: Integer;
  ParametrosAdicionaisTerminal: TACBrPOSPGWebAPIParametros;
begin
  if (ObterEstado(TerminalId) = statTEF) then
    DoException(Format(ACBrStr(sErrTransacaoJaIniciada), [TerminalId]));

  GravarLog('PTI_EFT_Start( '+TerminalId+', '+PWOPERToString(SmallInt(Operacao))+' )');
  VerificarConexao(TerminalId);
  AjustarEstadoConexao(TerminalId, statTEF);
  xPTI_EFT_Start( AnsiString(TerminalId),
                  SmallInt(Operacao),
                  iRet );
  GravarLog('  '+PTIRETToString(iRet));
  AvaliarErro(iRet, TerminalId);

  ParametrosAdicionaisTerminal := ParametrosAdicionais[TerminalId];
  For i := 0 to ParametrosAdicionaisTerminal.Count-1 do
    AdicionarParametro(TerminalId, ParametrosAdicionaisTerminal[i]);

  if Assigned(ParametrosAdicionaisTransacao) then
  begin
    For i := 0 to ParametrosAdicionaisTransacao.Count-1 do
      AdicionarParametro(TerminalId, ParametrosAdicionaisTransacao[i]);
  end;
end;

procedure TACBrPOSPGWebAPI.AdicionarParametro(const TerminalId: String;
  iINFO: Word; const AValor: AnsiString);
var
  iRet: SmallInt;
begin
  VerificarTransacaoFoiIniciada(TerminalId);
  GravarLog('PTI_EFT_AddParam( '+TerminalId+', '+PWINFOToString(iINFO)+', '+AValor+' )', True);
  VerificarConexao(TerminalId);
  xPTI_EFT_AddParam( AnsiString(TerminalId),
                     iINFO,
                     AValor,
                     iRet );
  GravarLog('  '+PTIRETToString(iRet));
  AvaliarErro(iRet, TerminalId);
end;

procedure TACBrPOSPGWebAPI.AdicionarParametro(const TerminalId: String;
  AKeyValueStr: String);
var
  AInfo: Integer;
  AInfoStr, AValue: String;
begin
  if ParseKeyValue(AKeyValueStr, AInfoStr, AValue) then
  begin
    AInfo := StrToIntDef(AInfoStr, -1);
    if (AInfo >= 0) then
      AdicionarParametro(TerminalId, AInfo, AValue);
  end;
end;

function TACBrPOSPGWebAPI.ExecutarTransacao(const TerminalId: String): SmallInt;
var
  iRet: SmallInt;
begin
  VerificarTransacaoFoiIniciada(TerminalId);
  GravarLog('PTI_EFT_Exec( '+TerminalId+' )');
  VerificarConexao(TerminalId);
  xPTI_EFT_Exec( AnsiString(TerminalId),
                 iRet );
  GravarLog('  '+PTIRETToString(iRet));
  Result := iRet;
end;

function TACBrPOSPGWebAPI.ObterInfo(const TerminalId: String; iINFO: Word
  ): String;
var
  pszValue: PAnsiChar;
  uiBuffLen: Word;
  iRet: SmallInt;
begin
  Result := #0;
  uiBuffLen := 10240;   // 10K
  pszValue := AllocMem(uiBuffLen);
  try
    GravarLog('PTI_EFT_GetInfo ( '+TerminalId+', '+ PWINFOToString(iINFO)+' )');
    xPTI_EFT_GetInfo( AnsiString(TerminalId),
                      iINFO,
                      uiBuffLen,
                      pszValue,
                      iRet );
    if (iRet = PTIRET_OK) then
    begin
      Result := String(pszValue);
      GravarLog('  '+Result, True);
    end
    else
    begin
      GravarLog('  '+PTIRETToString(iRet));
      if (iRet <> PTIRET_NODATA) then
        AvaliarErro(iRet, TerminalId);
    end;
  finally
    Freemem(pszValue);
  end;
end;

function TACBrPOSPGWebAPI.ObterUltimoRetorno(const TerminalId: String): String;
begin
  Result := ObterInfo(TerminalId, PWINFO_RESULTMSG);
end;

procedure TACBrPOSPGWebAPI.ObterDadosDaTransacao(const TerminalId: String);
var
  pszValue: PAnsiChar;
  uiBuffLen: Word;
  iRet: SmallInt;
  i: Word;
  AData, InfoStr: String;
  ADadosDaTransacao: TACBrPOSPGWebAPIParametros;
begin
  GravarLog('TACBrPOSPGWebAPI.ObterDadosDaTransacao( '+TerminalId+' )');
  ADadosDaTransacao := DadosDaTransacao[TerminalId];
  ADadosDaTransacao.Clear;
  uiBuffLen := 10240;   // 10K
  pszValue := AllocMem(uiBuffLen);
  try
    For i := MIN_PWINFO to MAX_PWINFO do
    begin
      InfoStr := PWINFOToString(i);
      if (i <> PWINFO_PPINFO) and  // Ler PWINFO_PPINFO é lento (e desnecessário)
         (pos(IntToStr(i), InfoStr) = 0) then  // i equivale a um PWINFO_ conhecido ?
      begin
        xPTI_EFT_GetInfo( AnsiString(TerminalId),
                          i,
                          uiBuffLen,
                          pszValue,
                          iRet );
        if (iRet = PTIRET_OK) then
        begin
          AData := BinaryStringToString(AnsiString(pszValue));
          ADadosDaTransacao.Add(Format('%d=%s', [i, Adata]));  // Add é mais rápido que usar "ValueInfo[i]"
          GravarLog('  '+Format('%s=%s', [InfoStr, AData]));
        end;
      end;
    end;
  finally
    Freemem(pszValue);
  end;
end;

function TACBrPOSPGWebAPI.ObterDadoTransacao(const TerminalId: String; iINFO: Word): String;
begin
  Result := DadosDaTransacao[TerminalId].ValueInfo[iINFO];
  if (Result = '') then
    Result := Trim(ObterInfo(TerminalId, iINFO));
end;

procedure TACBrPOSPGWebAPI.FinalizarTrancao(const TerminalId: String;
  Status: TACBrPOSPGWebStatusTransacao);
var
  iRet: SmallInt;
  Alist: TList;
  i: Integer;
  AConexao: TACBrPOSPGWebConexao;
begin
  //VerificarTransacaoFoiIniciada(TerminalId);
  GravarLog('PTI_EFT_Confirm( '+TerminalId+', '+IntToStr(SmallInt(Status))+' )');
  //VerificarConexao(TerminalId);
  xPTI_EFT_Confirm( AnsiString(TerminalId),
                    SmallInt(Status),
                    iRet );
  GravarLog('  '+PTIRETToString(iRet));

  AjustarEstadoConexao(TerminalId, statConectado);
  AvaliarErro(iRet, TerminalId);
  ApagarBackupDaTransacao(TerminalId);

  if Assigned(fOnAposFinalizarTransacao) then
  begin
    Alist := fListaConexoes.LockList;
    try
      for i := 0 to Alist.Count-1 do
      begin
        AConexao := TACBrPOSPGWebConexao(Alist[i]);
        if (AConexao.TerminalId = TerminalId) then
        begin
          AConexao.AvisarFinalizarTransacao(Status);
          Break;
        end;
      end;
    finally
      fListaConexoes.UnlockList;
    end;
  end;
end;

procedure TACBrPOSPGWebAPI.TratarTransacaoPendente(const TerminalId: String);
var
  PndAuthSyst, PndVirtMerch, PndAutLocRef, PndAutExtRef: String;
begin
  GravarLog('TACBrPOSPGWebAPI.TratarTransacaoPendente( '+TerminalId+' )');

  PndAutLocRef := ObterDadoTransacao(TerminalId, PWINFO_PNDAUTLOCREF);
  PndAutExtRef := ObterDadoTransacao(TerminalId, PWINFO_PNDAUTEXTREF);
  if (PndAutLocRef = '') and (PndAutExtRef = '') then
    Exit;

  PndAuthSyst := ObterDadoTransacao(TerminalId, PWINFO_PNDAUTHSYST);
  PndVirtMerch := ObterDadoTransacao(TerminalId, PWINFO_PNDVIRTMERCH);

  TratarTransacaoPendente(TerminalId, PndAuthSyst, PndVirtMerch, PndAutLocRef, PndAutExtRef);
end;

procedure TACBrPOSPGWebAPI.TratarTransacaoPendente(const TerminalId,
  PndAuthSyst, PndVirtMerch, PndAutLocRef, PndAutExtRef: String);
var
  AStatus: TACBrPOSPGWebStatusTransacao;
  Alist: TList;
  i: Integer;
  AConexao: TACBrPOSPGWebConexao;
begin
  if (TerminalId = '') or ((PndAutLocRef = '') and (PndAutExtRef = '')) then
    Exit;

  GravarLog('TACBrPOSPGWebAPI.TratarTransacaoPendente( '+TerminalId+', '+
                                                         PndAuthSyst+', '+
                                                         PndVirtMerch+', '+
                                                         PndAutLocRef+', '+
                                                         PndAutExtRef+' )');

  if fConfirmarTransacoesPendentes then
    AStatus := cnfSucesso
  else
    AStatus := cnfErroDiverso;

  if Assigned(fOnAvaliarTransacaoPendente) then
  begin
    Alist := fListaConexoes.LockList;
    try
      for i := 0 to Alist.Count-1 do
      begin
        AConexao := TACBrPOSPGWebConexao(Alist[i]);
        if (AConexao.TerminalId = TerminalId) then
        begin
          AConexao.AvaliarTransacaoPendente( AStatus,
                                             PndAuthSyst,
                                             PndVirtMerch,
                                             PndAutLocRef,
                                             PndAutExtRef);
          Break;
        end;
      end;
    finally
      fListaConexoes.UnlockList;
    end;
  end;

  FinalizarTrancao(TerminalId, AStatus);
end;


procedure TACBrPOSPGWebAPI.VerificarTransacoesBackup;
var
  BkpFileName, ATerminalId: String;
  slFiles: TStringList;
  i, p1, p2: Integer;
begin
  BkpFileName := BackupFileName('*');
  slFiles := TStringList.Create;
  try
    FindFiles(BkpFileName, slFiles);
    for i := 0 to slFiles.Count-1 do
    begin
      ATerminalId := '';
      p1 := pos(CACBrPOSPGWebBkpFile, slFiles[i]);
      if (p1 > 0) then
      begin
        p1 := p1 + Length(CACBrPOSPGWebBkpFile);
        p2 := PosEx('.', slFiles[i], p1);
        if (p2 < 0) then
          p2 := Length(slFiles[i]);

        ATerminalId := copy(slFiles[i], p1, p2-p1)
      end;

      if (ATerminalId <> '') then
        TratarTransacaoBackup(ATerminalId);
    end;
  finally
    slFiles.Free;
  end;
end;

function TACBrPOSPGWebAPI.BackupFileName(const TerminalId: String): String;
begin
  Result := fDiretorioTrabalho + PathDelim +
            CACBrPOSPGWebBkpFile + Trim(TerminalId) + CACBrPOSPGWebBkpExt;
end;

procedure TACBrPOSPGWebAPI.GravarBackupDaTransacao(const TerminalId: String);
var
  ADadosDaTransacao: TACBrPOSPGWebAPIParametros;
  BkpFileName: String;
begin
  BkpFileName := BackupFileName(TerminalId);
  GravarLog('TACBrPOSPGWebAPI.GravarBackupDaTransacao( '+BkpFileName+' )');
  ADadosDaTransacao := DadosDaTransacao[TerminalId];
  if ADadosDaTransacao.Count < 1 then
  begin
    GravarLog('  Vazio');
    exit;
  end;

  if FileExists(BkpFileName) then
    DeleteFile(BkpFileName);

  ADadosDaTransacao.SaveToFile(BkpFileName);
end;

procedure TACBrPOSPGWebAPI.ApagarBackupDaTransacao(const TerminalId: String);
var
  BkpFileName: String;
begin
  BkpFileName := BackupFileName(TerminalId);
  if FileExists(BkpFileName) then
  begin
    GravarLog('TACBrPOSPGWebAPI.ApagarBackupDaTransacao( '+BkpFileName+' )');
    DeleteFile(BkpFileName);
  end;
end;

procedure TACBrPOSPGWebAPI.TratarTransacaoBackup(const TerminalId: String);
var
  PndAuthSyst, PndVirtMerch, PndAutLocRef, PndAutExtRef, BkpFileName: String;
begin
  BkpFileName := BackupFileName(TerminalId);
  if not FileExists(BkpFileName) then
    Exit;

  GravarLog('TACBrPOSPGWebAPI.TratarTransacaoBackup( '+BkpFileName+' )');
  DadosDaTransacao[TerminalId].LoadFromFile(BkpFileName);

  PndAutLocRef := ObterDadoTransacao(TerminalId, PWINFO_AUTLOCREF);
  PndAutExtRef := ObterDadoTransacao(TerminalId, PWINFO_AUTEXTREF);
  if (PndAutLocRef = '') and (PndAutExtRef = '') then
    Exit;

  PndAuthSyst := ObterDadoTransacao(TerminalId, PWINFO_AUTHSYST);
  PndVirtMerch := ObterDadoTransacao(TerminalId, PWINFO_VIRTMERCH);

  TratarTransacaoPendente(TerminalId, PndAuthSyst, PndVirtMerch, PndAutLocRef, PndAutExtRef);
end;

function TACBrPOSPGWebAPI.Desconectar(const TerminalId: String; Segundos: Word
  ): Boolean;
var
  iRet: SmallInt;
begin
  GravarLog('PTI_Disconnect( '+TerminalId+', '+IntToStr(Segundos)+' )');
  xPTI_Disconnect( AnsiString(TerminalId),
                   Segundos,
                   iRet );
  GravarLog('  '+PTIRETToString(iRet));
  Result := (iRet = PTIRET_OK) or (iRet = PTIRET_NOCONN);
  if not Result then
    AvaliarErro(iRet, TerminalId)
  else
    AjustarEstadoConexao(TerminalId, statDesconectado);
end;

procedure TACBrPOSPGWebAPI.OnAguardaConexao(Sender: TObject);
var
  iRet: SmallInt;
  TerminalId, Model, MAC, SerNo: String;
begin
  fTimerConexao.Enabled := False;
  fEmConnectionLoop := True;

  try
    //GravarLog('PTI_ConnectionLoop');
    xPTI_ConnectionLoop( fpszTerminalId,
                         fpszModel,
                         fpszMAC,
                         fpszSerNo,
                         iRet );
    if (iRet = PTIRET_NEWCONN) then
    begin
      TerminalId := String(fpszTerminalId);
      Model := String(fpszModel);
      MAC := String(fpszMAC);
      SerNo := String(fpszSerNo);

      GravarLog('PTI_ConnectionLoop: '+PTIRETToString(iRet)+', TerminalId:'+TerminalId+', Model:'+Model+', MAC:'+MAC+', SerNo:'+SerNo);

      // Garante que não tem outra Conexão pendente, com o mesmo Terminal //
      TerminarConexao(TerminalId);

      // Cria Thread para receber Nova Conexão //
      TACBrPOSPGWebConexao.Create(Self, TerminalId, Model, MAC, SerNo);
    end
    else if (iRet <> PTIRET_NONEWCONN) then
      GravarLog('PTI_ConnectionLoop: '+PTIRETToString(iRet));
  finally
    fEmConnectionLoop := False;
    fTimerConexao.Enabled := True;
  end;
end;

function TACBrPOSPGWebAPI.FormatarMensagem(const AMsg: String; Colunas: Word
  ): AnsiString;
var
  MsgFormatada: String;
  l: Integer;
begin
  l := Length(AMsg);
  if (l > Colunas) then
  begin
    MsgFormatada := StringReplace(AMsg, CRLF, LF, [rfReplaceAll]) ;
    MsgFormatada := StringReplace(MsgFormatada, CR, LF, [rfReplaceAll]) ;
    MsgFormatada := TiraAcentos(MsgFormatada);
    MsgFormatada := AjustaLinhas(MsgFormatada, Colunas);
    MsgFormatada := StringReplace(MsgFormatada, LF, CR, [rfReplaceAll]);

    Result := AnsiString(ACBrStrToAnsi(MsgFormatada));
  end
  else if (l < 1) then
    Result := CR
  else
    Result := AnsiString(AMsg);
end;

procedure TACBrPOSPGWebAPI.AvaliarErro(iRet: SmallInt; const TerminalId: String
  );
var
  MsgError: String;
begin
  case iRet of
    PTIRET_OK:
    begin
      MsgError := '';
      if not (ObterEstado(TerminalId) in [statConectado, statTEF]) then
        AjustarEstadoConexao(TerminalId, statConectado);
    end;

    PTIRET_NOCONN:
    begin
      MsgError := Format(sErrPTIRET_NOCONN, [TerminalId]);
      AjustarEstadoConexao(TerminalId, statDesconectado);
    end;

    PTIRET_BUSY:
    begin
      MsgError := Format(sErrPTIRET_BUSY, [TerminalId]);
      AjustarEstadoConexao(TerminalId, statOcupado);
    end;

    PTIRET_INVPARAM: MsgError := sErrPTIRET_INVPARAM;
    PTIRET_SECURITYERR: MsgError := sErrPTIRET_SECURITYERR;
    PTIRET_NOTSUPPORTED: MsgError := sErrPTIRET_NOTSUPPORTED;
    PTIRET_PRINTERR: MsgError := sErrPTIRET_PRINTERR;
    PTIRET_NOPAPER: MsgError := sErrPTIRET_NOPAPER;
    PTIRET_INTERNALERR: MsgError := sErrPTIRET_INTERNALERR;
    PTIRET_EFTERR: MsgError := sErrPTIRET_EFTERR;
    PTIRET_BUFOVRFLW: MsgError := sErrPTIRET_BUFOVRFLW;
  else
    MsgError := Format(sErrPTIRET_UNKNOWN, [iRet]);
  end;

  if (MsgError <> '') then
    DoException( ACBrStr(MsgError) );
end;

procedure TACBrPOSPGWebAPI.VerificarTransacaoFoiIniciada(
  const TerminalId: String);
var
  EstadoTerminal: TACBrPOSPGWebEstadoTerminal;
begin
  EstadoTerminal := ObterEstado(TerminalId);
  if (EstadoTerminal <> statTEF) then
    DoException(Format(ACBrStr(sErrTransacaoNaoIniciada), [TerminalId]));
end;

procedure TACBrPOSPGWebAPI.AjustarEstadoConexao(const TerminalId: String;
  NovoEstado: TACBrPOSPGWebEstadoTerminal);
var
  Alist: TList;
  i: Integer;
  AConexao: TACBrPOSPGWebConexao;
begin
  Alist := fListaConexoes.LockList;
  try
    for i := 0 to Alist.Count-1 do
    begin
      AConexao := TACBrPOSPGWebConexao(Alist[i]);
      if (AConexao.TerminalId = TerminalId) then
      begin
        AConexao.Estado := NovoEstado;
        Break;
      end;
    end;
  finally
    fListaConexoes.UnlockList;
  end;
end;

procedure TACBrPOSPGWebAPI.TerminarConexao(const TerminalId: String);
var
  Alist: TList;
  i: Integer;
  AConexao: TACBrPOSPGWebConexao;
begin
  GravarLog('TACBrPOSPGWebAPI.TerminarConexao( '+TerminalId+ ' )');

  Alist := fListaConexoes.LockList;
  try
    for i := 0 to Alist.Count-1 do
    begin
      AConexao := TACBrPOSPGWebConexao(Alist[i]);
      if (AConexao.TerminalId = TerminalId) then
      begin
        AConexao.Terminate;
        Break;
      end;
    end;
  finally
    fListaConexoes.UnlockList;
  end;
end;

function TACBrPOSPGWebAPI.TerminalEstaConectado(const TerminalId: String): Boolean;
var
  EstadoTerminal: TACBrPOSPGWebEstadoTerminal;
begin
  //GravarLog('  TerminalEstaConectado( '+TerminalId+ ' )');
  EstadoTerminal := ObterEstado(TerminalId);
  Result := not (EstadoTerminal in [statDesconectado, statEsperaConexao]);
end;

procedure TACBrPOSPGWebAPI.VerificarConexao(const TerminalId: String);
begin
  if not TerminalEstaConectado(TerminalId) then
    raise EACBrPOSPGWeb.CreateFmt(ACBrStr(sErrPTIRET_NOCONN), [TerminalId]);
end;

function TACBrPOSPGWebAPI.CalcularCapacidadesDaAutomacao: Integer;
begin
  Result := 4;            // 4: valor fixo, sempre incluir;
  if fSuportaSaque then
    Inc(Result, 1);       // 1: funcionalidade de troco/saque;
  if fSuportaDesconto then
    Inc(Result, 2);       // 2: funcionalidade de desconto;
  if fSuportaViasDiferenciadas then
    Inc(Result, 8);       // 8: impressão das vias diferenciadas do comprovante para Cliente/Estabelecimento;
  if fImprimirViaClienteReduzida then
    Inc(Result, 16);      // 16: impressão do cupom reduzido
  if fUtilizaSaldoTotalVoucher then
    Inc(Result, 32);      // 32: utilização de saldo total do voucher para abatimento do valor da compra
end;

procedure TACBrPOSPGWebAPI.SetDiretorioTrabalho(AValue: String);
begin
  if fDiretorioTrabalho = AValue then
    Exit;

  GravarLog('TACBrPOSPGWebAPI.SetDiretorioTrabalho( '+AValue+' )');

  if fInicializada then
    DoException(ACBrStr(sErrLibJaInicializada));

  fDiretorioTrabalho := AValue;
end;

function TACBrPOSPGWebAPI.GetParametrosAdicionais(const TerminalId: String
  ): TACBrPOSPGWebAPIParametros;
begin
  Result := fACBrTEFPGWebAPIListaParametros.Terminal[TerminalId];
end;

function TACBrPOSPGWebAPI.GetDadosTransacao(const TerminalId: String
  ): TACBrPOSPGWebAPIParametros;
begin
  Result := fDadosTransacaoList.Terminal[TerminalId];
end;

procedure TACBrPOSPGWebAPI.SetNomeAplicacao(AValue: String);
begin
  if fNomeAplicacao = AValue then Exit;
  fNomeAplicacao := LeftStr(Trim(AValue),29);
end;

procedure TACBrPOSPGWebAPI.SetPathDLL(AValue: String);
begin
  if fPathDLL = AValue then
    Exit;

  GravarLog('TACBrPOSPGWebAPI.SetPathDLL( '+AValue+' )');

  if fInicializada then
    DoException(ACBrStr(sErrLibJaInicializada));

  fPathDLL := AValue;
end;

procedure TACBrPOSPGWebAPI.SetSoftwareHouse(AValue: String);
begin
  if fSoftwareHouse = AValue then Exit;
  fSoftwareHouse := LeftStr(Trim(AValue),40);
end;

procedure TACBrPOSPGWebAPI.SetVersaoAplicacao(AValue: String);
begin
  if fVersaoAplicacao = AValue then Exit;
  fVersaoAplicacao := LeftStr(Trim(AValue),10);
end;

procedure TACBrPOSPGWebAPI.LoadDLLFunctions;

  procedure PGWebFunctionDetect( FuncName: AnsiString; var LibPointer: Pointer ) ;
  var
    sLibName: string;
  begin
    if not Assigned( LibPointer )  then
    begin
      GravarLog('   '+FuncName);

      // Verifica se exite o caminho das DLLs
      sLibName := '';
      if Length(PathDLL) > 0 then
        sLibName := PathWithDelim(PathDLL);

      // Concatena o caminho se exitir mais o nome da DLL.
      sLibName := sLibName + CACBrPOSPGWebLib;

      if not FunctionDetect( sLibName, FuncName, LibPointer) then
      begin
        LibPointer := NIL ;
        DoException(Format(ACBrStr('Erro ao carregar a função: %s de: %s'),[FuncName, sLibName]));
      end ;
    end ;
  end;

 begin
   if fInicializada then
     Exit;

   GravarLog('TACBrPOSPGWebAPI.LoadDLLFunctions');

   PGWebFunctionDetect('PTI_Init', @xPTI_Init);
   PGWebFunctionDetect('PTI_End', @xPTI_End);
   PGWebFunctionDetect('PTI_ConnectionLoop', @xPTI_ConnectionLoop);
   PGWebFunctionDetect('PTI_CheckStatus', @xPTI_CheckStatus);
   PGWebFunctionDetect('PTI_Disconnect', @xPTI_Disconnect);
   PGWebFunctionDetect('PTI_Display', @xPTI_Display);
   PGWebFunctionDetect('PTI_WaitKey', @xPTI_WaitKey);
   PGWebFunctionDetect('PTI_ClearKey', @xPTI_ClearKey);
   PGWebFunctionDetect('PTI_GetData', @xPTI_GetData);
   PGWebFunctionDetect('PTI_StartMenu', @xPTI_StartMenu);
   PGWebFunctionDetect('PTI_AddMenuOption', @xPTI_AddMenuOption);
   PGWebFunctionDetect('PTI_StartMenu', @xPTI_StartMenu);
   PGWebFunctionDetect('PTI_ExecMenu', @xPTI_ExecMenu);
   PGWebFunctionDetect('PTI_Beep', @xPTI_Beep);
   PGWebFunctionDetect('PTI_Print', @xPTI_Print);
   PGWebFunctionDetect('PTI_PrnFeed', @xPTI_PrnFeed);
   PGWebFunctionDetect('PTI_PrnSymbolCode', @xPTI_PrnSymbolCode);
   PGWebFunctionDetect('PTI_EFT_Start', @xPTI_EFT_Start);
   PGWebFunctionDetect('PTI_EFT_AddParam', @xPTI_EFT_AddParam);
   PGWebFunctionDetect('PTI_EFT_Exec', @xPTI_EFT_Exec);
   PGWebFunctionDetect('PTI_EFT_GetInfo', @xPTI_EFT_GetInfo);
   PGWebFunctionDetect('PTI_EFT_PrintReceipt', @xPTI_EFT_PrintReceipt);
   PGWebFunctionDetect('PTI_EFT_Confirm', @xPTI_EFT_Confirm);
end;

procedure TACBrPOSPGWebAPI.UnLoadDLLFunctions;
var
  sLibName: String;
begin
  if not fInicializada then
    Exit;

  xPTI_End;

  //GravarLog('TACBrPOSPGWebAPI.UnLoadDLLFunctions');

  sLibName := '';
  if Length(PathDLL) > 0 then
     sLibName := PathWithDelim(PathDLL);

  UnLoadLibrary( sLibName + CACBrPOSPGWebLib );
  ClearMethodPointers;
end;

procedure TACBrPOSPGWebAPI.ClearMethodPointers;
begin
  xPTI_Init := Nil;
  xPTI_End := Nil;
  xPTI_ConnectionLoop := Nil;
  xPTI_CheckStatus := Nil;
  xPTI_Disconnect := Nil;
  xPTI_Display := Nil;
  xPTI_WaitKey := Nil;
  xPTI_ClearKey := Nil;
  xPTI_GetData := Nil;
  xPTI_StartMenu := Nil;
  xPTI_AddMenuOption := Nil;
  xPTI_StartMenu := Nil;
  xPTI_ExecMenu := Nil;
  xPTI_Beep := Nil;
  xPTI_Print := Nil;
  xPTI_PrnFeed := Nil;
  xPTI_PrnSymbolCode := Nil;
  xPTI_EFT_Start := Nil;
  xPTI_EFT_AddParam := Nil;
  xPTI_EFT_Exec := Nil;
  xPTI_EFT_GetInfo := Nil;
  xPTI_EFT_PrintReceipt := Nil;
  xPTI_EFT_Confirm := Nil;
end;

end.

