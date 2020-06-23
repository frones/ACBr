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
  ACBrTEFPayGoComum, ACBrBase;

resourcestring
  sErrLibJaInicializda = 'Biblioteca PTI_DLL já foi inicializada';
  sErrEventoNaoAtribuido = 'Evento %s não atribuido';
  sErrPTIRET_UNKNOWN = 'Erro %d executando %s';
  sErrPTIRET_INVPARAM = 'Parâmetro inválido informado à função';
  sErrPTIRET_NOCONN = 'O terminal %s está offline.';
  sErrPTIRET_SOCKETERR = 'Erro ao iniciar a escuta da porta TCP %d.';
  sErrPTIRET_WRITEERR = 'Falha de gravação no diretório %s';
  sErrPTIRET_BUSY = 'O terminal %s está ocupado processando outro comando.';
  sErrPTIRET_SECURITYERR = 'A função foi rejeitada por questões de segurança.';
  sErrPTIRET_NOTSUPPORTED = 'Função não suportada pelo terminal';
  sErrPTIRET_NOPAPER = 'Impressora sem papel.';
  sErrPTIRET_INTERNALERR = 'Erro interno da biblioteca de integração.';

const
  CACBrPOSPGWebAPIName = 'ACBrPOSPGWebAPI';
  CACBrPOSPGWebAPIVersao = '1.0.0';
  CACBrPOSPGWebSubDir = 'POSPGWeb';
  CACBrPOSPGWebColunasDisplay = 20;
  CACBrPOSPGWebColunasImpressora = 40;
  CACBrPOSPGWebPortaTCP = 3433;
  CACBrPOSPGWebMaxTerm = 100;
  CACBrPOSPGWebTempoDesconexao = 30;  // Segundos
  CACBrPOSPGWebEsperaLoop = 300;  // Milissegundos
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
  PTIRET_NODATA       = 2006;   // Informação requerida não disponível
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

  TACBrPOSPGWebBeep = (PTIBEEP_OK,      // 0 Sucesso
                       PTIBEEP_WARNING, // 1 Alerta
                       PTIBEEP_ERROR);  // 2 Erro


  TACBrPOSPGWebStatus = (PTISTAT_IDLE,       // 0 Terminal está on-line e aguardando por comandos.
                         PTISTAT_BUSY,       // 1 Terminal está on-line, porém ocupado processando um comando.
                         PTISTAT_NOCONN,     // 2 Terminal está offline.
                         PTISTAT_WAITRECON); // 3 Terminal está off-line. A transação continua sendo executada e após sua finalização, o terminal tentará efetuar a reconexão automaticamente

  TACBrPOSPGWebCodBarras = (CODESYMB_128 = 2, // Código de barras padrão 128. Pode-se utilizar aproximadamente 31 caracteres alfanuméricos.
                            CODESYMB_ITF = 3, // Código de barras padrão ITF. Pode-se utilizar aproximadamente 30 caracteres alfanuméricos.
                            CODESYMB_QRCODE = 4); // QR Code. Com aceitação de aproximadamente 600 caracteres alfanuméricos.

  TACBrPOSPGWebAPI = class;

  TACBrPOSPGWebNovaConexao = procedure(const TerminalId: String; const Model: String;
      const MAC: String; const SerNo: String);

  { TACBrPOSPGWebConexao }

  TACBrPOSPGWebConexao = class(TThread)
    fPOSPGWeb: TACBrPOSPGWebAPI;
    fTerminalId: String;
    fModel: String;
    fMAC: String;
    fSerNo: String;
  protected
    procedure Execute; override;

  public
    constructor Create(POSPGWeb: TACBrPOSPGWebAPI; const TerminalId: String;
      const Model: String; const MAC: String; const SerNo: String);

    property TerminalId: String read fTerminalId;
    property Model: String read fModel;
    property MAC: String read fMAC;
    property SerNo: String read fSerNo;
  end;

  { TACBrPOSPGWebAPI }

  TACBrPOSPGWebAPI = class
  private
    fOnNovaConexao: TACBrPOSPGWebNovaConexao;
    fTimerConexao: TACBrThreadTimer;
    fListaConexoes: TThreadList;
    fDadosTransacao: TACBrTEFPGWebAPIParametrosAdicionais;
    fParametrosAdicionais: TACBrTEFPGWebAPIParametrosAdicionais;
    fLogCriticalSection: TCriticalSection;
    fpszTerminalId, fpszModel, fpszMAC, fpszSerNo: PAnsiChar;
    fCNPJEstabelecimento: String;
    fConfirmarTransacoesPendentesNoHost: Boolean;
    fDiretorioTrabalho: String;
    ffMensagemBoasVindas: String;
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

    xPTI_Init: procedure( pszPOS_Company: PAnsiChar; pszPOS_Version: PAnsiChar;
      pszPOS_Capabilities: PAnsiChar; pszDataFolder: PAnsiChar; uiTCP_Port: Word;
      uiMaxTerminals: Word; pszWaitMsg: PAnsiChar; uiAutoDiscSec: Word;
      out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_End: procedure();
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_ConnectionLoop: procedure(pszTerminalId: PAnsiChar; pszModel: PAnsiChar;
      pszMAC: PAnsiChar; pszSerNo: PAnsiChar; out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_CheckStatus: procedure(pszTerminalId: PAnsiChar; out piStatus: SmallInt;
      pszModel: PAnsiChar; pszMAC: PAnsiChar; pszSerNo: PAnsiChar; out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_Disconnect: procedure(pszTerminalId: PAnsiChar; uiPwrDelay: Word;
      out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_Display: procedure(pszTerminalId: PAnsiChar; pszMsg: PAnsiChar;
      out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_WaitKey: procedure(pszTerminalId: PAnsiChar; uiTimeOutSec: Word;
      out piKey: SmallInt; out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_ClearKey: procedure(pszTerminalId: PAnsiChar; out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_GetData: procedure(pszTerminalId: PAnsiChar; pszPrompt: PAnsiChar;
      pszFormat: PAnsiChar; uiLenMin: Word; uiLenMax: Word; fFromLeft: Boolean;
      fAlpha: Boolean; fMask: Boolean; uiTimeOutSec: Word; pszData: PAnsiChar;
      uiCaptureLine: Word; out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_StartMenu: procedure(pszTerminalId: PAnsiChar; out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_AddMenuOption: procedure(pszTerminalId: PAnsiChar; pszOption: PAnsiChar;
      out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_ExecMenu: procedure(pszTerminalId: PAnsiChar; pszPrompt: PAnsiChar;
      uiTimeOutSec: Word; out puiSelection: SmallInt; out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_Beep: procedure(pszTerminalId: PAnsiChar; iType: SmallInt; out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_Print: procedure(pszTerminalId: PAnsiChar; pszText: PAnsiChar; out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_PrnFeed: procedure(pszTerminalId: PAnsiChar; out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_PrnSymbolCode: procedure(pszTerminalId: PAnsiChar; pszMsg: PAnsiChar;
      iSymbology: SmallInt; out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_EFT_Start: procedure(pszTerminalId: PAnsiChar; iOper: SmallInt; out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_EFT_AddParam: procedure(pszTerminalId: PAnsiChar; iParam: Word;
      pszValue: PAnsiChar; out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_EFT_Exec: procedure(pszTerminalId: PAnsiChar; out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_EFT_GetInfo: procedure(pszTerminalId: PAnsiChar; iInfo: Word; uiBufLen: Word;
      pszValue: PAnsiChar; out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_EFT_PrintReceipt: procedure(pszTerminalId: PAnsiChar; iCopies: Word;
      out piRet: SmallInt);
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};
    xPTI_EFT_Confirm: procedure(pszTerminalId: PAnsiChar; iStatus: SmallInt;
      out piRet: SmallInt)
      {$IfDef MSWINDOWS}stdcall{$Else}cdecl{$EndIf};

    function GetInicializada: Boolean;
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
    function FormatarMensagem(const AMsg: String; Colunas: Word): String;
    procedure AvaliarErro(iRet: SmallInt; const TerminalId: String);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Inicializar;
    procedure DesInicializar;

    procedure GravarLog(const AString: AnsiString; Traduz: Boolean = False);

    procedure ExibirMensagem(const TerminalId: String; const AMensagem: String);
    procedure LimparTeclado(const TerminalId: String);
    function AguardarTecla(const TerminalId: String; Espera: Word = 0): Integer;
    function ObterDado(const TerminalId: String; const Titulo: String;
      const Mascara: String = ''; uiLenMin: Word = 1; uiLenMax: Word = 20;
      AlinhaAEsqueda: Boolean = False; PermiteAlfa: Boolean = False;
      OcultarDigitacao: Boolean = False; IntervaloMaxTeclas: Word = 30;
      const ValorInicial: String = ''; LinhaCaptura: Word = 2): String;
    function ExecutarMenu(const TerminalId: String; Opcoes: TStrings;
      const Titulo: String = ''; IntervaloMaxTeclas: Word = 30;
      Opcao: Word = 0): Integer;
    procedure Beep(const TerminalId: String; TipoBeep: TACBrPOSPGWebBeep = PTIBEEP_OK);
    procedure ImprimirTexto(const TerminalId: String; const ATexto: String);
    procedure AvancarPapel(const TerminalId: String);
    procedure ImprimirCodBarras(const TerminalId: String; const Codigo: String;
      Tipo: TACBrPOSPGWebCodBarras);
    procedure ImprimirQRCode(const TerminalId: String; const Conteudo: String);
    function Desconectar(const TerminalId: String; Segundos: Word = 0): Boolean;


    property PathDLL: String read fPathDLL write SetPathDLL;
    property DiretorioTrabalho: String read fDiretorioTrabalho write SetDiretorioTrabalho;
    property Inicializada: Boolean read GetInicializada write SetInicializada;

    property DadosDaTransacao: TACBrTEFPGWebAPIParametrosAdicionais read fDadosTransacao;

    property SoftwareHouse: String read fSoftwareHouse write SetSoftwareHouse;
    property NomeAplicacao: String read fNomeAplicacao write SetNomeAplicacao ;
    property VersaoAplicacao: String read fVersaoAplicacao write SetVersaoAplicacao ;

    property PortaTCP: Word read fPortaTCP write fPortaTCP;
    property MaximoTerminaisConectados: Word read fMaximoTerminaisConectados write fMaximoTerminaisConectados;
    property TempoDesconexaoAutomatica: Word read fTempoDesconexaoAutomatica write fTempoDesconexaoAutomatica;
    property MensagemBoasVindas: String read fMensagemBoasVindas write ffMensagemBoasVindas;
    property ParametrosAdicionais: TACBrTEFPGWebAPIParametrosAdicionais read fParametrosAdicionais;

    Property SuportaSaque: Boolean read fSuportaSaque write fSuportaSaque;
    Property SuportaDesconto: Boolean read fSuportaDesconto write fSuportaDesconto;
    property ImprimirViaClienteReduzida : Boolean read fImprimirViaClienteReduzida
      write fImprimirViaClienteReduzida;
    property SuportaViasDiferenciadas: Boolean read fSuportaViasDiferenciadas
      write fSuportaViasDiferenciadas;
    property UtilizaSaldoTotalVoucher: Boolean read fUtilizaSaldoTotalVoucher
      write fUtilizaSaldoTotalVoucher;
    property ConfirmarTransacoesPendentesNoHost: Boolean read fConfirmarTransacoesPendentesNoHost
      write fConfirmarTransacoesPendentesNoHost;

    property OnGravarLog: TACBrGravarLog read fOnGravarLog write fOnGravarLog;
    property OnNovaConexao: TACBrPOSPGWebNovaConexao read fOnNovaConexao write fOnNovaConexao;
  end;

function PTIRETToString(iRET: SmallInt): String;

implementation

uses
  strutils, math,
  ACBrUtil, ACBrConsts;

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
  inherited Create(True); // CreateSuspended;

  fPOSPGWeb := POSPGWeb;
  fTerminalId := TerminalId;
  fModel := Model;
  fMAC := MAC;
  fSerNo := SerNo;
end;

procedure TACBrPOSPGWebConexao.Execute;
begin
  // ATENÇÃO: Todo o código do Evento OnNovaConexao deve ser Thread Safe
  fPOSPGWeb.OnNovaConexao(fTerminalId, fModel, fMAC, fSerNo);

  try
    fPOSPGWeb.Desconectar(TerminalId);
  except
    { Ignora erros }
  end;

  Terminate;
end;


{ TACBrPOSPGWebAPI }

constructor TACBrPOSPGWebAPI.Create;
begin
  inherited Create;
  ClearMethodPointers;

  fSuportaViasDiferenciadas := True;
  fUtilizaSaldoTotalVoucher := False;
  fSuportaDesconto := False;
  fSuportaSaque := False;
  fImprimirViaClienteReduzida := False;
  fConfirmarTransacoesPendentesNoHost := True;

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

  fDadosTransacao := TACBrTEFPGWebAPIParametrosAdicionais.Create;
  fParametrosAdicionais := TACBrTEFPGWebAPIParametrosAdicionais.Create;
  fListaConexoes := TThreadList.Create;
  fTimerConexao := TACBrThreadTimer.Create;
  fTimerConexao.OnTimer := OnAguardaConexao;
  fTimerConexao.Interval := CACBrPOSPGWebEsperaLoop;
  fTimerConexao.Enabled := False;

  fLogCriticalSection := TCriticalSection.Create;
  fOnGravarLog := Nil;
  fOnNovaConexao := Nil;
end;

destructor TACBrPOSPGWebAPI.Destroy;
begin
  fTimerConexao.Enabled := False;

  Freemem(fpszTerminalId);
  Freemem(fpszModel);
  Freemem(fpszMAC);
  Freemem(fpszSerNo);

  fLogCriticalSection.Free;
  fTimerConexao.Free;
  fDadosTransacao.Free;
  fParametrosAdicionais.Free;
  fListaConexoes.Free;

  UnLoadDLLFunctions;

  inherited Destroy;
end;

procedure TACBrPOSPGWebAPI.DoException(AErrorMsg: String);
begin
  if (Trim(AErrorMsg) = '') then
    Exit;

  GravarLog('EACBrPOSPGWeb: '+AErrorMsg);
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
  MsgError, AMsgBoasVindas: String;
  POSCapabilities: Integer;
begin
  if Inicializada then
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
  AMsgBoasVindas := FormatarMensagem(fMensagemBoasVindas, CACBrPOSPGWebColunasDisplay);

  GravarLog('PTI_Init( '+fSoftwareHouse+', '+
                         fNomeAplicacao+' '+fVersaoAplicacao+', '+
                         IntToStr(POSCapabilities)+', '+
                         fDiretorioTrabalho+', '+
                         IntToStr(fPortaTCP)+', '+
                         IntToStr(fMaximoTerminaisConectados)+', '+
                         AMsgBoasVindas+', '+
                         IntToStr(fTempoDesconexaoAutomatica)+' )');

  xPTI_Init( PAnsiChar(AnsiString(fSoftwareHouse)),
             PAnsiChar(AnsiString(fNomeAplicacao + ' ' + fVersaoAplicacao)),
             PAnsiChar(AnsiString(IntToStr(POSCapabilities))),
             PAnsiChar(AnsiString(fDiretorioTrabalho)),
             fPortaTCP, fMaximoTerminaisConectados,
             PAnsiChar(AnsiString(AMsgBoasVindas)),
             fTempoDesconexaoAutomatica,
             iRet);
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

  fTimerConexao.Enabled := True;
end;

procedure TACBrPOSPGWebAPI.DesInicializar;
begin
  GravarLog('TACBrPOSPGWebAPI.DesInicializar');
  fTimerConexao.Enabled := False;
  UnLoadDLLFunctions;
end;

procedure TACBrPOSPGWebAPI.SetInicializada(AValue: Boolean);
begin
  if Inicializada = AValue then
    Exit;

  GravarLog('TACBrPOSPGWebAPI.SetInicializada( '+BoolToStr(AValue, True)+' )');

  if AValue then
    Inicializar
  else
    DesInicializar;
end;

procedure TACBrPOSPGWebAPI.ExibirMensagem(const TerminalId: String;
  const AMensagem: String);
var
  iRet: SmallInt;
  MsgFormatada: String;
begin
  MsgFormatada := FormatarMensagem(AMensagem, CACBrPOSPGWebColunasDisplay);
  GravarLog('PTI_Display( '+TerminalId+', '+MsgFormatada+' )', True);
  xPTI_Display( PAnsiChar(AnsiString(TerminalId)), PAnsiChar(AnsiString(MsgFormatada)), iRet);
  GravarLog('  '+PTIRETToString(iRet));
  AvaliarErro(iRet, TerminalId);
end;

procedure TACBrPOSPGWebAPI.LimparTeclado(const TerminalId: String);
var
  iRet: SmallInt;
begin
  GravarLog('PTI_ClearKey( '+TerminalId+' )');
  xPTI_ClearKey( PAnsiChar(AnsiString(TerminalId)), iRet);
  GravarLog('  '+PTIRETToString(iRet));
  AvaliarErro(iRet, TerminalId);
end;

function TACBrPOSPGWebAPI.AguardarTecla(const TerminalId: String; Espera: Word
  ): Integer;
var
  iRet, iKet: SmallInt;
begin
  GravarLog('PTI_WaitKey( '+TerminalId+', '+IntToStr(Integer(Espera))+' )');
  xPTI_WaitKey( PAnsiChar(AnsiString(TerminalId)), Espera, iKet, iRet);
  GravarLog('  '+PTIRETToString(iRet)+', '+IntToStr(Result));
  if (iRet = PTIRET_OK) then
    Result := iKet
  else if (iRet = PTIRET_TIMEOUT) then
    Result := 0
  else
    AvaliarErro(iRet, TerminalId);
end;

function TACBrPOSPGWebAPI.ObterDado(const TerminalId: String;
  const Titulo: String; const Mascara: String; uiLenMin: Word; uiLenMax: Word;
  AlinhaAEsqueda: Boolean; PermiteAlfa: Boolean; OcultarDigitacao: Boolean;
  IntervaloMaxTeclas: Word; const ValorInicial: String; LinhaCaptura: Word
  ): String;
var
  iRet: SmallInt;
  pszData: PAnsiChar;
begin
  GravarLog('PTI_GetData( '+TerminalId+', Titulo:'+Titulo+', Mascara:'+Mascara+', '+
                          'TamMin: '+IntToStr(uiLenMin)+', TamMax:'+IntToStr(uiLenMax)+', '+
                          'AlinhaAEsqueda: '+ifthen(AlinhaAEsqueda,'S','N')+', '+
                          'PermiteAlfa: '+ifthen(PermiteAlfa,'S','N')+', '+
                          'OcultarDigitacao: '+ifthen(OcultarDigitacao,'S','N')+', '+
                          'Intervalo: '+IntToStr(IntervaloMaxTeclas)+', '+
                          'Valor: '+ValorInicial+', Linha:'+IntToStr(LinhaCaptura)+' )');

  Result := '';
  pszData := AllocMem(50);
  try
    Move(ValorInicial[1], pszData, Length(ValorInicial)+1 );
    xPTI_GetData( PAnsiChar(AnsiString(TerminalId)),
                  PAnsiChar(AnsiString(Titulo)),
                  PAnsiChar(AnsiString(Mascara)),
                  uiLenMin, uiLenMax,
                  AlinhaAEsqueda, PermiteAlfa, OcultarDigitacao,
                  IntervaloMaxTeclas,
                  pszData,
                  LinhaCaptura,
                  iRet);
    GravarLog('  '+PTIRETToString(iRet)+', '+Result);
    if (iRet = PTIRET_OK) then
      Result := String(pszData)
    else if (iRet <> PTIRET_TIMEOUT) or (iRet <> PTIRET_CANCEL) then
      AvaliarErro(iRet, TerminalId);
  finally
    Freemem(pszData);
  end;
end;

function TACBrPOSPGWebAPI.ExecutarMenu(const TerminalId: String;
  Opcoes: TStrings; const Titulo: String; IntervaloMaxTeclas: Word; Opcao: Word
  ): Integer;
var
  iRet: SmallInt;
  i: Integer;
begin
  Result := -1;
  if (not Assigned(Opcoes)) or (Opcoes.Count < 1) then
    Exit;

  GravarLog('PTI_StartMenu( '+TerminalId+' )');
  xPTI_StartMenu( PAnsiChar(AnsiString(TerminalId)), iRet );
  GravarLog('  '+PTIRETToString(iRet));

  i := 0;
  while (iRet = PTIRET_OK)  and (i < Opcoes.Count) do
  begin
    GravarLog('PTI_AddMenuOption( '+TerminalId+', '+Opcoes[i]+' )');
    xPTI_AddMenuOption( PAnsiChar(AnsiString(TerminalId)),
                        PAnsiChar(AnsiString(Opcoes[i])),
                        iRet );
    GravarLog('  '+PTIRETToString(iRet));
  end;

  if (iRet = PTIRET_OK) then
  begin
    Opcao := min( max(0, Opcao), Opcoes.Count-1);
    GravarLog('PTI_ExecMenu( '+TerminalId+', '+Titulo+', '+
                              IntToStr(IntervaloMaxTeclas)+', '+
                              IntToStr(Opcao)+' )');
    xPTI_ExecMenu( PAnsiChar(AnsiString(TerminalId)),
                   PAnsiChar(AnsiString(Titulo)),
                   IntervaloMaxTeclas,
                   Opcao, iRet );
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
  xPTI_Beep( PAnsiChar(AnsiString(TerminalId)), SmallInt(TipoBeep), iRet);
  GravarLog('  '+PTIRETToString(iRet));
  AvaliarErro(iRet, TerminalId);
end;

procedure TACBrPOSPGWebAPI.ImprimirTexto(const TerminalId: String;
  const ATexto: String);
var
  iRet: SmallInt;
  TextoFormatado: String;
begin
  // Trocando Tag <e> por \v ou #11, para Expandido. Deve ser o Primeiro caracter da Linha
  TextoFormatado := StringReplace(ATexto, '<e>', #11, [rfReplaceAll]);
  TextoFormatado := StringReplace(ATexto, '<E>', #11, [rfReplaceAll]);
  // Formatando no limite de Colunas
  TextoFormatado := FormatarMensagem(TextoFormatado, CACBrPOSPGWebColunasImpressora);

  GravarLog('PTI_Print( '+TerminalId+', '+TextoFormatado+' )', True);
  xPTI_Print( PAnsiChar(AnsiString(TerminalId)),
              PAnsiChar(AnsiString(TextoFormatado)),
              iRet);
  GravarLog('  '+PTIRETToString(iRet));
  AvaliarErro(iRet, TerminalId);
end;

procedure TACBrPOSPGWebAPI.AvancarPapel(const TerminalId: String);
var
  iRet: SmallInt;
begin
  GravarLog('PTI_PrnFeed( '+TerminalId+' )');
  xPTI_PrnFeed( PAnsiChar(AnsiString(TerminalId)), iRet);
  GravarLog('  '+PTIRETToString(iRet));
  AvaliarErro(iRet, TerminalId);
end;

procedure TACBrPOSPGWebAPI.ImprimirCodBarras(const TerminalId: String;
  const Codigo: String; Tipo: TACBrPOSPGWebCodBarras);
var
  iRet: SmallInt;
begin
  GravarLog('PTI_PrnSymbolCode( '+TerminalId+', '+Codigo+', '+IntToStr(Tipo)+' )', True);
  xPTI_PrnSymbolCode( PAnsiChar(AnsiString(TerminalId)),
                      PAnsiChar(AnsiString(Codigo)),
                      Tipo, iRet);
  GravarLog('  '+PTIRETToString(iRet));
  AvaliarErro(iRet, TerminalId);
end;

procedure TACBrPOSPGWebAPI.ImprimirQRCode(const TerminalId: String;
  const Conteudo: String);
begin
  ImprimirCodBarras(TerminalId, Conteudo, CODESYMB_QRCODE);
end;

function TACBrPOSPGWebAPI.Desconectar(const TerminalId: String; Segundos: Word
  ): Boolean;
var
  iRet: SmallInt;
begin
  GravarLog('PTI_Disconnect( '+TerminalId+', '+IntToStr(Segundos)+' )');
  xPTI_Disconnect( PAnsiChar(AnsiString(TerminalId)), Segundos, iRet);
  GravarLog('  '+PTIRETToString(iRet));
  Result := (iRet = PTIRET_OK) or (iRet = PTIRET_NOCONN);
  if not Result then
    AvaliarErro(iRet, TerminalId);
end;

procedure TACBrPOSPGWebAPI.OnAguardaConexao(Sender: TObject);
var
  iRet: SmallInt;
  NovaConexao: TACBrPOSPGWebConexao;
  TerminalId, Model, MAC, SerNo: String;
begin
  xPTI_ConnectionLoop(fpszTerminalId, fpszModel, fpszMAC, fpszSerNo, iRet);
  if (iRet = PTIRET_NEWCONN) then
  begin
    TerminalId := String(fpszTerminalId);
    Model := String(fpszModel);
    MAC := String(fpszMAC);
    SerNo := String(fpszSerNo);

    GravarLog('  '+PTIRETToString(iRet)+', TerminalId:'+TerminalId+', Model:'+Model+', MAC:'+MAC+', SerNo:'+SerNo);
    NovaConexao := TACBrPOSPGWebConexao.Create(Self, TerminalId, Model, MAC, SerNo);
    fListaConexoes.Add(NovaConexao);
    NovaConexao.Start;
  end
  else if (iRet <> PTIRET_NOCONN) then
    GravarLog('  PTI_ConnectionLoop: '+PTIRETToString(iRet));
end;

function TACBrPOSPGWebAPI.FormatarMensagem(const AMsg: String; Colunas: Word): String;
var
  LB: String;
begin
  Result := AMsg;
  if (Length(AMsg) > Colunas) then
  begin
    LB := sLineBreak;
    if (LB <> LF) then
      Result := StringReplace(Result, sLineBreak, LF, [rfReplaceAll]);

    if (LB <> CR) then
      Result := StringReplace(Result, CR, LF, [rfReplaceAll]);

    Result := QuebraLinhas(Result, Colunas);
    if (LB <> CR) then
      Result := StringReplace(Result, LB, CR, [rfReplaceAll]);
  end;
end;

procedure TACBrPOSPGWebAPI.AvaliarErro(iRet: SmallInt; const TerminalId: String
  );
var
  MsgError: String;
begin
  case iRet of
    PTIRET_OK: MsgError := '';
    PTIRET_INVPARAM: MsgError := sErrPTIRET_INVPARAM;
    PTIRET_NOCONN: MsgError := Format(sErrPTIRET_NOCONN, [TerminalId]);
    PTIRET_BUSY: MsgError := Format(sErrPTIRET_BUSY, [TerminalId]);
    PTIRET_SECURITYERR: MsgError := sErrPTIRET_SECURITYERR;
    PTIRET_NOTSUPPORTED: MsgError := sErrPTIRET_NOTSUPPORTED;
    PTIRET_NOPAPER: MsgError := sErrPTIRET_NOPAPER;
    PTIRET_INTERNALERR: MsgError := sErrPTIRET_INTERNALERR;
  else
    MsgError := Format(sErrPTIRET_UNKNOWN, [iRet]);
  end;

  if (MsgError <> '') then
    DoException( ACBrStr(MsgError) );
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

function TACBrPOSPGWebAPI.GetInicializada: Boolean;
begin
  Result := fTimerConexao.Enabled;
end;

procedure TACBrPOSPGWebAPI.SetDiretorioTrabalho(AValue: String);
begin
  if fDiretorioTrabalho = AValue then
    Exit;

  GravarLog('TACBrPOSPGWebAPI.SetDiretorioTrabalho( '+AValue+' )');

  if Inicializada then
    DoException(ACBrStr(sErrLibJaInicializda));

  fDiretorioTrabalho := AValue;
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

  if Inicializada then
    DoException(ACBrStr(sErrLibJaInicializda));

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
   if Inicializada then
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
  if not Inicializada then
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

