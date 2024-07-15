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

unit ACBrPosPrinter;

interface

uses
  Classes, SysUtils,
  {$IfDef MSWINDOWS}
   windows,
  {$EndIf}
  Types,
  ACBrDevice, ACBrBase,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs
  {$Else}
   Contnrs
  {$IfEnd}
  {$IfDef MSWINDOWS}
   ,ACBrEscPosHook, ACBrWinUSBDevice
  {$EndIf};

const
  CACBrPosChequeINI = 'ACBrPosCheque.ini';
  CACBrPosChequeResource = 'ACBrPosCheque';
  cKeyFabricante = 'Fabricante';
  cKeySerial = 'Serial';
  cKeyFirmware = 'Firmware';
  cKeyModelo = 'Modelo';
  cKeyGuilhotina = 'Guilhotina';
  cKeyColunas = 'Colunas';
  cKeyCodPage = 'CodPage';
  cKeyCheque = 'Cheque';
  cKeyAutenticacao = 'Autenticacao';
  cKeyMICR = ' MICR';

  cTimeOutTxRx = 300;
  cTimeOutTxRxTCP = 1500;

type

  EPosPrinterException = class(Exception);

  { TACBrPosComandos }

  TACBrPosComandos = class
  private
    FBeep: AnsiString;
    FAlinhadoCentro: AnsiString;
    FAlinhadoDireita: AnsiString;
    FAlinhadoEsquerda: AnsiString;
    FCorteParcial: AnsiString;
    FDesligaAlturaDupla: AnsiString;
    FDesligaInvertido: AnsiString;
    FDesligaModoPagina: AnsiString;
    FEspacoEntreLinhasPadrao: AnsiString;
    FImprimePagina: AnsiString;
    FLigaAlturaDupla: AnsiString;
    FLigaInvertido: AnsiString;
    FFonteNormal: AnsiString;
    FLigaCondensado: AnsiString;
    FCorteTotal: AnsiString;
    FEspacoEntreLinhas: AnsiString;
    FLigaExpandido: AnsiString;
    FDesligaCondensado: AnsiString;
    FDesligaExpandido: AnsiString;
    FDesligaItalico: AnsiString;
    FDesligaNegrito: AnsiString;
    FDesligaSublinhado: AnsiString;
    FFonteA: AnsiString;
    FFonteB: AnsiString;
    FLigaItalico: AnsiString;
    FLigaModoPagina: AnsiString;
    FLigaNegrito: AnsiString;
    FLigaSublinhado: AnsiString;
    FPuloDeLinha: AnsiString;
    FPuloDePagina: AnsiString;
    FZera: AnsiString;
  public
    constructor Create;
    procedure Clear;

    property Zera: AnsiString read FZera write FZera;
    property EspacoEntreLinhas: AnsiString read FEspacoEntreLinhas
      write FEspacoEntreLinhas;
    property EspacoEntreLinhasPadrao: AnsiString
      read FEspacoEntreLinhasPadrao write FEspacoEntreLinhasPadrao;

    property LigaNegrito: AnsiString read FLigaNegrito write FLigaNegrito;
    property DesligaNegrito: AnsiString read FDesligaNegrito write FDesligaNegrito;
    property LigaExpandido: AnsiString read FLigaExpandido write FLigaExpandido;
    property DesligaExpandido: AnsiString read FDesligaExpandido write FDesligaExpandido;
    property LigaAlturaDupla: AnsiString read FLigaAlturaDupla write FLigaAlturaDupla;
    property DesligaAlturaDupla: AnsiString read FDesligaAlturaDupla write FDesligaAlturaDupla;
    property LigaSublinhado: AnsiString read FLigaSublinhado write FLigaSublinhado;
    property DesligaSublinhado: AnsiString read FDesligaSublinhado
      write FDesligaSublinhado;
    property LigaItalico: AnsiString read FLigaItalico write FLigaItalico;
    property DesligaItalico: AnsiString read FDesligaItalico write FDesligaItalico;
    property LigaCondensado: AnsiString read FLigaCondensado write FLigaCondensado;
    property DesligaCondensado: AnsiString read FDesligaCondensado
      write FDesligaCondensado;
    property LigaInvertido: AnsiString read FLigaInvertido write FLigaInvertido;
    property DesligaInvertido: AnsiString read FDesligaInvertido write FDesligaInvertido;

    property FonteNormal: AnsiString read FFonteNormal write FFonteNormal;
    property FonteA: AnsiString read FFonteA write FFonteA;
    property FonteB: AnsiString read FFonteB write FFonteB;

    property AlinhadoEsquerda: AnsiString read FAlinhadoEsquerda write FAlinhadoEsquerda;
    property AlinhadoDireita: AnsiString read FAlinhadoDireita write FAlinhadoDireita;
    property AlinhadoCentro: AnsiString read FAlinhadoCentro write FAlinhadoCentro;

    property Beep: AnsiString read FBeep write FBeep;
    property CorteTotal: AnsiString read FCorteTotal write FCorteTotal;
    property CorteParcial: AnsiString read FCorteParcial write FCorteParcial;
    property PuloDeLinha: AnsiString read FPuloDeLinha write FPuloDeLinha;
    property PuloDePagina: AnsiString read FPuloDePagina write FPuloDePagina;

    property LigaModoPagina: AnsiString read FLigaModoPagina write FLigaModoPagina;
    property DesligaModoPagina: AnsiString read FDesligaModoPagina write FDesligaModoPagina;
    property ImprimePagina: AnsiString read FImprimePagina write FImprimePagina;
  end;

  TACBrPosTipoFonte = (ftNormal, ftCondensado, ftExpandido, ftNegrito,
    ftSublinhado, ftInvertido, ftItalico, ftFonteB, ftAlturaDupla);
  TACBrPosFonte = set of TACBrPosTipoFonte;

  TACBrPosTipoAlinhamento = (alEsquerda, alCentro, alDireita);
  TACBrPosPaginaCodigo = (pcNone, pc437, pc850, pc852, pc860, pcUTF8, pc1252);
  TACBrPosDirecao = (dirEsquerdaParaDireita, dirTopoParaBaixo, dirDireitaParaEsquerda, dirBaixoParaTopo);

  TACBrPosTipoStatus = (stErro, stApenasEscrita, stPoucoPapel, stSemPapel,
                        stGavetaAberta, stImprimindo, stOffLine, stTampaAberta,
                        stErroLeitura, stSlip, stMICR, stAguardandoSlip, stTOF, stBOF);
  TACBrPosPrinterStatus = set of TACBrPosTipoStatus;
const
  TACBrPosTipoStatusArrayStrings:array[TACBrPosTipoStatus] of string =
                       ('Erro', 'ApenasEscrita', 'PoucoPapel', 'SemPapel',
                        'GavetaAberta', 'Imprimindo', 'OffLine', 'TampaAberta',
                        'ErroLeitura', 'Slip', 'MICR', 'AguardandoSlip', 'TOF', 'BOF');
type
  { TACBrPosRazaoColunaFonte }
  {$M+}
  TACBrPosRazaoColunaFonte = class
  private
    FCondensada: Double;
    FExpandida: Double;
  public
    constructor Create;
  published
    property Condensada: Double read FCondensada write FCondensada;
    property Expandida: Double read FExpandida write FExpandida;
  end;
  {$M-}

  TACBrPosPrinter = class;

  TACBrPosPrinterModelo = (ppTexto, ppEscPosEpson, ppEscBematech, ppEscDaruma,
                           ppEscVox, ppEscDiebold, ppEscEpsonP2, ppCustomPos,
                           ppEscPosStar, ppEscZJiang, ppEscGPrinter, ppEscDatecs,
                           ppEscSunmi, ppExterno);

  { TACBrPosPrinterClass }

  TACBrPosPrinterClass = class
  private
    FCmd: TACBrPosComandos;
    FRazaoColunaFonte: TACBrPosRazaoColunaFonte;
    FTagsNaoSuportadas: TStringList;
    FInfo: TStringList;

  protected
    fpModeloStr: String;
    fpPosPrinter: TACBrPosPrinter;

    procedure AddInfo(const ATitulo: String; const AStr: AnsiString); overload;
    procedure AddInfo(const ATitulo: String; const ABool: Boolean); overload;
  public
    procedure AntesDecodificar(var ABinaryString: AnsiString); virtual;
    procedure AdicionarBlocoResposta(const ConteudoBloco: AnsiString); virtual;
    procedure DepoisDecodificar(var ABinaryString: AnsiString); virtual;
    function TraduzirTag(const ATag: AnsiString; var TagTraduzida: AnsiString): Boolean; virtual;
    function TraduzirTagBloco(const ATag, ConteudoBloco: AnsiString;
      var BlocoTraduzido: AnsiString): Boolean; virtual;

    function ComandoCodBarras(const ATag: String; const ACodigo: AnsiString): AnsiString; virtual;
    function ComandoQrCode(const ACodigo: AnsiString): AnsiString; virtual;
    function ComandoEspacoEntreLinhas(Espacos: byte): AnsiString; virtual;
    function ComandoPaginaCodigo(APagCodigo: TACBrPosPaginaCodigo): AnsiString; virtual;
    function ComandoGaveta(NumGaveta: Integer = 1): AnsiString; virtual;
    function ComandoInicializa: AnsiString; virtual;
    function ComandoPuloLinhas(NLinhas: Integer): AnsiString; virtual;
    function ComandoFonte(TipoFonte: TACBrPosTipoFonte; Ligar: Boolean): AnsiString; virtual;
    function ComandoConfiguraModoPagina: AnsiString; virtual;
    function ComandoPosicionaModoPagina(APoint: TPoint): AnsiString; virtual;
    function ComandoCarregaCheque(LerCMC7: Boolean = True; SegundosEspera: Byte = 5): AnsiString; virtual;
    function ComandoEjetarCheque: AnsiString; virtual;

    procedure Configurar; virtual;
    procedure LerStatus(var AStatus: TACBrPosPrinterStatus); virtual;
    function LerInfo: String; virtual;
    function ComandoLerCMC7(SegundosEspera: Byte = 0): AnsiString; virtual;
    function LeituraCheque: AnsiString; virtual;

    function ComandoImprimirImagemRasterStr(const RasterStr: AnsiString; AWidth: Integer;
      AHeight: Integer): AnsiString; virtual;
    function ComandoImprimirImagemArquivo(ArquivoBMP: String): AnsiString;
    function ComandoImprimirImagemStream(ABMPStream: TStream): AnsiString;

    function ComandoLogo: AnsiString; virtual;
    function ComandoGravarLogoRasterStr(const RasterStr: AnsiString; AWidth: Integer;
      AHeight: Integer): AnsiString; virtual;
    function ComandoGravarLogoArquivo(ArquivoBMP: String): AnsiString;
    function ComandoGravarLogoStream(ABMPStream: TStream): AnsiString;
    function ComandoApagarLogo: AnsiString; virtual;


    procedure ArquivoImagemToRasterStr(ArquivoImagem: String; out AWidth: Integer;
      out AHeight: Integer; out ARasterStr: AnsiString);

    constructor Create(AOwner: TACBrPosPrinter);
    destructor Destroy; override;

    property RazaoColunaFonte: TACBrPosRazaoColunaFonte read FRazaoColunaFonte;
    property Cmd: TACBrPosComandos read FCmd;
    property ModeloStr: String read fpModeloStr;
    property Info: TStringList read FInfo;
    property PosPrinter: TACBrPosPrinter read fpPosPrinter;

    property TagsNaoSuportadas: TStringList read FTagsNaoSuportadas;
  end;

  { TACBrConfigQRCode }

  TACBrConfigQRCode = class(TPersistent)
    private
      FErrorLevel: Integer;
      FLarguraModulo: Integer;
      FTipo: Integer;
      procedure SetErrorLevel(AValue: Integer);
      procedure SetLarguraModulo(AValue: Integer);
      procedure SetTipo(AValue: Integer);
    public
      constructor Create;

    published
      property Tipo: Integer read FTipo write SetTipo;
      property LarguraModulo: Integer read FLarguraModulo write SetLarguraModulo;
      property ErrorLevel: Integer read FErrorLevel write SetErrorLevel;
  end;

  { TACBrConfigLogo }

  TACBrConfigLogo = class(TPersistent)
    private
      FFatorX: Byte;
      FFatorY: Byte;
      FIgnorarLogo: Boolean;
      FKeyCode1: Byte;
      FKeyCode2: Byte;
    public
      constructor Create;

    published
      property IgnorarLogo: Boolean read FIgnorarLogo write FIgnorarLogo default False;
      property KeyCode1: Byte read FKeyCode1 write FKeyCode1 default 32;
      property KeyCode2: Byte read FKeyCode2 write FKeyCode2 default 32;
      property FatorX: Byte read FFatorX write FFatorX default 1;
      property FatorY: Byte read FFatorY write FFatorY default 1;
  end;

  { TACBrConfigGaveta }

  TACBrConfigGaveta = class(TPersistent)
    private
      FSinalInvertido: Boolean;
      FTempoOFF: Byte;
      FTempoON: Byte;

    public
      constructor Create;

    published
      property SinalInvertido: Boolean read FSinalInvertido
        write FSinalInvertido default False;
      property TempoON: Byte read FTempoON write FTempoON default 50;
      property TempoOFF: Byte read FTempoOFF write FTempoOFF default 200;
  end;

  { TACBrConfigModoPagina }

  TACBrConfigModoPagina = class(TPersistent)
    private
      FDirecao: TACBrPosDirecao;
      FEspacoEntreLinhas: Byte;
      FLargura: Integer;
      FAltura: Integer;
      FEsquerda: Integer;
      FTopo: Integer;
    public
      constructor Create;

    published
      property Largura: Integer read FLargura write FLargura default 0;
      property Altura: Integer read FAltura write FAltura default 0;
      property Esquerda: Integer read FEsquerda write FEsquerda default 0;
      property Topo: Integer read FTopo write FTopo default 0;
      property Direcao: TACBrPosDirecao read FDirecao write FDirecao default dirEsquerdaParaDireita;
      property EspacoEntreLinhas: Byte read FEspacoEntreLinhas write FEspacoEntreLinhas default 0;
  end;

  TACBrPosTipoCorte = (ctTotal, ctParcial);

  { TACBrPosCheque }

  TACBrPosCheque = class
  private
    FAno: TPoint;
    FCidade: TPoint;
    FCodBanco: Integer;
    FComplemento: TPoint;
    FDia: TPoint;
    FExtenso: TPoint;
    FFavorecido: TPoint;
    FMes: TPoint;
    FNome: String;
    FValor: TPoint;
  public
    constructor Create;
    procedure Clear;

    property CodBanco: Integer read FCodBanco write FCodBanco;
    property Nome: String read FNome write FNome;
    property Valor: TPoint read FValor write FValor;
    property Extenso: TPoint read FExtenso write FExtenso;
    property Favorecido: TPoint read FFavorecido write FFavorecido;
    property Cidade: TPoint read FCidade write FCidade;
    property Dia: TPoint read FDia write FDia;
    property Mes: TPoint read FMes write FMes;
    property Ano: TPoint read FAno write FAno;
    property Complemento: TPoint read FComplemento write FComplemento;
  end;

  { TACBrPosCheques }

  TACBrPosCheques = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrPosCheque>{$EndIf})
  private
    FBancoPadrao: Integer;
    FDimensao: TPoint;
    procedure LoadFromStringList(AStringList: TStrings);

  protected
    FFileName: String;
    FResourceName: String;
    FLoaded: Boolean;
    function LoadFromFile: Boolean;
    function LoadFromResource: Boolean;
    procedure SetFileName(AValue: String);

    procedure SetObject (Index: Integer; Item: TACBrPosCheque);
    function GetObject (Index: Integer): TACBrPosCheque;
    procedure Insert (Index: Integer; Obj: TACBrPosCheque);
  public
    constructor Create(APosChequeIni: String = ''; AResourceName: String = '');
    destructor Destroy; override;
    procedure Clear; {$IfNDef HAS_SYSTEM_GENERICS}override;{$EndIf}

    procedure Load;
    function FindPosCheque(ACodBanco: Integer; UsaPadraoSeNaoAchar: Boolean = True): TACBrPosCheque;

    property FileName: String read FFileName write SetFileName;
    property ResourceName: String read FResourceName;

    function Add(Obj: TACBrPosCheque): Integer;
    property Objects [Index: Integer]: TACBrPosCheque
      read GetObject write SetObject; default;
    property Dimensao: TPoint read FDimensao;
    property BancoPadrao: Integer read FBancoPadrao;
  end;

  TACBrPosOperacaoCheque = (agcCancelado, agcConcluido, agcSleep, agcImpressao, agcLerCMC7);
  TACBrPosOnAguardarCheque = procedure(const AStatus: TACBrPosPrinterStatus;
    Operacao: TACBrPosOperacaoCheque; var CancelarEspera: Boolean) of object ;

  { TACBrPosPrinter }

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrPosPrinter = class(TACBrComponent)
  private
    FColunasFonteNormal: Integer;
    FConfigBarras: TACBrECFConfigBarras;
    FConfigLogo: TACBrConfigLogo;
    FConfigQRCode: TACBrConfigQRCode;
    FConfigModoPagina: TACBrConfigModoPagina;
    FControlePorta: Boolean;
    FDevice: TACBrDevice;
    FAtivo: Boolean;
    FEspacoEntreLinhas: Byte;
    FConfigGaveta: TACBrConfigGaveta;
    FModelo: TACBrPosPrinterModelo;
    FModeloExterno: TACBrPosPrinterClass;
    FOnAguardarCheque: TACBrPosOnAguardarCheque;
    FCancelarEsperaCheque: Boolean;
    FOnGravarLog: TACBrGravarLog;
    FOnEnviarStringDevice: TACBrGravarLog;
    FOnEnviarStringDeviceDefault: TACBrGravarLog;
    FTagProcessor: TACBrTagProcessor;
    FPosCheques: TACBrPosCheques;
    FTemGuilhotina: Integer;
    FTemCheque: Integer;
    FTemAutenticacao: Integer;
    FTemMICR: Integer;

    FCortaPapel: Boolean;
    FTipoCorte: TACBrPosTipoCorte;
    FLinhasBuffer: Integer;
    FLinhasEntreCupons: Integer;
    FPaginaDeCodigo: TACBrPosPaginaCodigo;
    FArqLog: String;

    FBuffer: TStringList;
    FTipoAlinhamento: TACBrPosTipoAlinhamento;
    FFonteStatus: TACBrPosFonte;
    FModoPaginaLigado: Boolean;
    FInicializada: Boolean;
    FVerificarImpressora: Boolean;

    function ComandoConfigurarModoPaginaCheque: AnsiString;
    function ComandoTextoPosCheque(const AText: AnsiString; APos, ADimensao: TPoint): String;
    function GetColunasFonteCondensada: Integer;
    function GetColunasFonteExpandida: Integer;
    function GetLeituraCheque: AnsiString;
    function GetNumeroPaginaDeCodigo(APagCod: TACBrPosPaginaCodigo): word;
    function CodificarPaginaDeCodigo(const ATexto: AnsiString): AnsiString;

    procedure DoLinesChange(Sender: TObject);
    function GetColunas: Integer;
    function GetIgnorarTags: Boolean;
    function GetPorta: String;
    function GetTagsNaoSuportadas: TStringList;
    function GetTemAutenticacao: Integer;
    function GetTemCheque: Integer;
    function GetTemGuilhotina: Integer;
    function GetTemMICR: Integer;
    function GetTraduzirTags: Boolean;
    procedure SetAtivo(AValue: Boolean);
    procedure SetIgnorarTags(AValue: Boolean);
    procedure SetPorta(const AValue: String);
    procedure SetTraduzirTags(AValue: Boolean);
    procedure SetModelo(AValue: TACBrPosPrinterModelo);
    procedure SetModeloExterno(APosPrinterClass: TACBrPosPrinterClass);
    procedure VerificarParametrosLogo(const AKC2: Integer = -1; const AKC1: Integer = -1);
    function ProcessarComandoBMP(const ConteudoBloco: String): AnsiString;
    function ProcessarPosicionaXY(const ConteudoBloco: String): AnsiString;
    procedure LerInfoDispositivos;

  protected
    FPosPrinterClass: TACBrPosPrinterClass;

    {$IfDef MSWINDOWS}
     FHook: TACBrPosPrinterHook;
    {$EndIf}

    procedure Loaded; override;
    procedure EnviarStringDevice(AString: AnsiString);

    function DecodificarTagsFormatacao(ABinaryString: AnsiString): AnsiString;
    procedure DoTraduzirTag(const ATag: AnsiString; var TagTraduzida: AnsiString);
    procedure DoAdicionarBlocoResposta(const ConteudoBloco: AnsiString);
    procedure DoTraduzirTagBloco(const ATag, ConteudoBloco: AnsiString;
      var BlocoTraduzido: AnsiString);

    procedure AjustarEstadoFonteAlinhamento(const ATag: AnsiString);

    procedure AtivarPorta;
    procedure DesativarPorta;
    procedure VerificaPodeLerDaPorta;

    {$IfDef MSWINDOWS}
    procedure DetectarECriarHook;
    procedure LiberarHook;
    procedure PosPrinterHookAtivar(const APort: String; Params: String);
    procedure PosPrinterHookDesativar(const APort: String);
    procedure PosPrinterHookEnviaString(const cmd: AnsiString);
    procedure PosPrinterHookLeString(const NumBytes, ATimeOut: Integer; var Retorno: AnsiString);
    {$EndIf}

    function AguardarCheque(Operacao: TACBrPosOperacaoCheque): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property PosPrinter: TACBrPosPrinterClass read FPosPrinterClass;
    {$IfDef MSWINDOWS}
     property Hook: TACBrPosPrinterHook read FHook;
    {$EndIf}

    procedure Ativar;
    procedure Desativar;
    property Ativo: Boolean read FAtivo write SetAtivo;

    procedure Imprimir(const AString: AnsiString = ''; PulaLinha: Boolean = False;
      DecodificarTags: Boolean = True; CodificarPagina: Boolean = True;
      Copias: Integer = 1);
    procedure ImprimirLinha(const AString: AnsiString);
    procedure ImprimirCmd(const AString: AnsiString);
    procedure GravarLog(AString: AnsiString; Traduz: Boolean = False;
      AdicionaTempo: Boolean = True);
    procedure ImprimirCheque(CodBanco: Integer; AValor: Double; ADataEmissao: TDateTime;
      const AFavorecido, ACidade, AComplemento: String; LerCMC7: Boolean = False;
      SegundosEspera: Integer = 5);
    procedure ImprimirTextoCheque(const X, Y: Integer; const AString: AnsiString;
      AguardaCheque: Boolean = False; SegundosEspera: Integer = 5);
    procedure EjetarCheque;
    procedure LerCMC7(AguardaCheque: Boolean = False; SegundosEspera: Integer = 5);

    function TxRx(const ACmd: AnsiString; BytesToRead: Byte = 1;
      ATimeOut: Integer = 0; WaitForTerminator: Boolean = False): AnsiString;

    property TagProcessor: TACBrTagProcessor read FTagProcessor;
    property TagsNaoSuportadas: TStringList read GetTagsNaoSuportadas;
    procedure RetornarTags(AStringList: TStrings; IncluiAjuda: Boolean = True);
    procedure ImprimirTags;

    procedure Zerar;
    procedure Inicializar;
    procedure Reset;

    procedure PularLinhas(NumLinhas: Integer = 0);
    procedure CortarPapel(Parcial: Boolean = False);
    procedure AbrirGaveta(NumGaveta: Integer = 1);

    function LerStatusImpressora( Tentativas: Integer = 1): TACBrPosPrinterStatus;
    function LerInfoImpressora: String;

    procedure ImprimirImagemStream(ABMPStream: TStream);
    procedure ImprimirImagemArquivo(ArquivoBMP: String);
    procedure ImprimirImagemRasterStr(const ARasterStr: AnsiString; AWidth, AHeight: Integer);
    function  ValidaLogoBmp(const APathLogo: string): Boolean;

    procedure ImprimirLogo(AKC1: Integer = -1; AKC2: Integer = -1;
      AFatorX: Integer = -1; AFatorY: Integer = -1);
    procedure GravarLogoStream(ABMPStream: TStream; AKC1: Integer = -1;
      AKC2: Integer = -1);
    procedure GravarLogoArquivo(ArquivoBMP: String; AKC1: Integer = -1;
      AKC2: Integer = -1);
    procedure ApagarLogo(AKC1: Integer = -1; AKC2: Integer = -1);

    function CalcularAlturaTexto(ALinhas: Integer): Integer;
    function CalcularLinhasAltura(AAltura: Integer): Integer;
    function CalcularAlturaQRCodeAlfaNumM(const QRCodeData: String): Integer;
    function ConfigurarRegiaoModoPagina(AEsquerda, ATopo, AAltura, ALargura: Integer;
      ADirecao: TACBrPosDirecao = dirEsquerdaParaDireita): String;

    function AjustarCodBarras(const ABarCode, ABarCodeTag: AnsiString): AnsiString;

    property Buffer: TStringList read FBuffer;

    property Colunas: Integer read GetColunas;
    property ColunasFonteExpandida: Integer read GetColunasFonteExpandida;
    property ColunasFonteCondensada: Integer read GetColunasFonteCondensada;

    property FonteStatus: TACBrPosFonte read FFonteStatus;
    property Alinhamento: TACBrPosTipoAlinhamento read FTipoAlinhamento;
    property Inicializada: Boolean read FInicializada;
    property ModoPagina: Boolean read FModoPaginaLigado;

    function PodeLerDaPorta: Boolean;
    property TemGuilhotina: Integer read GetTemGuilhotina;
    property TemCheque: Integer read GetTemCheque;
    property TemAutenticacao: Integer read GetTemAutenticacao;
    property TemMICR: Integer read GetTemMICR;
    property LeituraCheque: AnsiString read GetLeituraCheque;

    property ModeloExterno: TACBrPosPrinterClass read FModeloExterno write SetModeloExterno;
  published
    property Modelo: TACBrPosPrinterModelo read FModelo write SetModelo default ppTexto;
    property Porta: String read GetPorta write SetPorta;
    property Device: TACBrDevice read FDevice;

    property PaginaDeCodigo: TACBrPosPaginaCodigo
      read FPaginaDeCodigo write FPaginaDeCodigo default pc850;
    property ColunasFonteNormal: Integer read FColunasFonteNormal
      write FColunasFonteNormal default 48;
    property EspacoEntreLinhas: byte read FEspacoEntreLinhas
      write FEspacoEntreLinhas default 0;

    property ConfigBarras: TACBrECFConfigBarras read FConfigBarras write FConfigBarras;
    property ConfigQRCode: TACBrConfigQRCode read FConfigQRCode write FConfigQRCode;
    property ConfigLogo: TACBrConfigLogo read FConfigLogo write FConfigLogo;
    property ConfigGaveta: TACBrConfigGaveta read FConfigGaveta write FConfigGaveta;
    property ConfigModoPagina: TACBrConfigModoPagina read FConfigModoPagina write FConfigModoPagina;

    property LinhasEntreCupons: Integer read FLinhasEntreCupons
      write FLinhasEntreCupons default 21;
    property CortaPapel: Boolean read FCortaPapel write FCortaPapel default True;
    property TipoCorte: TACBrPosTipoCorte read FTipoCorte write FTipoCorte default ctTotal;
    property TraduzirTags: Boolean read GetTraduzirTags
      write SetTraduzirTags default True;
    property IgnorarTags: Boolean read GetIgnorarTags write SetIgnorarTags default False;
    property LinhasBuffer: Integer read FLinhasBuffer write FLinhasBuffer default 0;
    property ControlePorta: Boolean read FControlePorta write FControlePorta default False;
    property VerificarImpressora: Boolean read FVerificarImpressora write FVerificarImpressora default False;

    property OnGravarLog: TACBrGravarLog read FOnGravarLog write FOnGravarLog;
    property ArqLOG: String read FArqLog write FArqLog;
    property OnEnviarStringDevice: TACBrGravarLog read FOnEnviarStringDevice write FOnEnviarStringDevice;
    property OnAguardarCheque: TACBrPosOnAguardarCheque read FOnAguardarCheque write FOnAguardarCheque;
  end;

  function FormataPrinterStatus(ConjuntoDeStatus: TACBrPosPrinterStatus): string;

implementation

{$IFDEF FPC}
 {$R ACBrPosCheque.rc}
{$ELSE}
 {$R ACBrPosCheque.res}
{$ENDIF}

uses
  strutils, Math, typinfo, IniFiles, dateutils,
  {$IfNDef NOGUI}
    {$IfDef FMX}
      FMX.Graphics,
    {$Else}
      Graphics,
    {$EndIf}
  {$EndIf}
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.FilesIO,
  ACBrUtil.Compatibilidade,
  ACBrImage, ACBrConsts, ACBrExtenso, ACBrCMC7,
  synacode, synautil,
  ACBrEscPosEpson, ACBrEscEpsonP2, ACBrEscBematech, ACBrEscDaruma,
  ACBrEscElgin, ACBrEscDiebold, ACBrEscCustomPos, ACBrEscPosStar,
  ACBrEscZJiang, ACBrEscGPrinter, ACBrEscDatecs, ACBrEscSunmi
  {$IfDef MSWINDOWS}
  ,ACBrEscPosHookElginDLL, ACBrEscPosHookEpsonDLL
  {$EndIf};



function FormataPrinterStatus(ConjuntoDeStatus: TACBrPosPrinterStatus): string;
var
  UmStatus: TACBrPosTipoStatus;
begin
  Result := '';
  for UmStatus := High(TACBrPosTipoStatus) downto Low(TACBrPosTipoStatus) do
  begin
    if Result <> '' then
    begin
      Result := '|' + Result;
    end;

    if UmStatus in ConjuntoDeStatus then
      Result := '1' + Result
    else
      Result := '0' + Result;
  end;
end;


{ TACBrPosCheque }

constructor TACBrPosCheque.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrPosCheque.Clear;
begin
  FCodBanco := 0;
  FNome := '';
  FAno := Point(0,0);
  FCidade := Point(0,0);
  FComplemento := Point(0,0);
  FDia := Point(0,0);
  FExtenso := Point(0,0);
  FFavorecido := Point(0,0);
  FMes := Point(0,0);
  FValor := Point(0,0);
end;

{ TACBrPosCheques }

constructor TACBrPosCheques.Create(APosChequeIni: String;
  AResourceName: String);
begin
  inherited Create(True);

  if APosChequeIni = '' then
    FFileName := ApplicationPath + CACBrPosChequeINI
  else
    FFileName := APosChequeIni;

  if AResourceName = '' then
    FResourceName := CACBrPosChequeResource
  else
    FResourceName := AResourceName;

  Clear;
end;

destructor TACBrPosCheques.Destroy;
begin
  inherited Destroy;
end;

procedure TACBrPosCheques.Clear;
begin
  inherited Clear;
  FDimensao := Point(154,93);
  FBancoPadrao := 0;
end;

procedure TACBrPosCheques.Load;
var
  APosCheque: TACBrPosCheque;
begin
  Clear;
  FLoaded := LoadFromFile or LoadFromResource;

  if not FLoaded then
    raise Exception.Create(ACBrStr('Erro ao carregar Posições de Cheques'));

  if (FindPosCheque(FBancoPadrao, False) = Nil) then  // Garante que o Banco Padrão existe..
  begin
    if (Count = 0) then
    begin
      FBancoPadrao := 0;
      APosCheque := TACBrPosCheque.Create;
      APosCheque.CodBanco    := 0;
      APosCheque.Nome        := 'PADRAO';
      APosCheque.Valor       := Point( 43,63);
      APosCheque.Extenso     := Point(145,60);
      APosCheque.Favorecido  := Point(165,45);
      APosCheque.Cidade      := Point(100,38);
      APosCheque.Dia         := Point( 70,38);
      APosCheque.Mes         := Point( 55,38);
      APosCheque.Ano         := Point( 17,38);
      APosCheque.Complemento := Point(165,10);
      Insert(0, APosCheque);
    end
    else
      FBancoPadrao := Objects[0].CodBanco;    // Primeiro Banco definido no INI
  end;
end;

function TACBrPosCheques.FindPosCheque(ACodBanco: Integer;
  UsaPadraoSeNaoAchar: Boolean): TACBrPosCheque;

  function DoFindPosCheque(ACodBanco: Integer): TACBrPosCheque;
  var
    i: Integer;
  begin
    Result := Nil;
    for i := 0 to Count-1 do
    begin
      if Objects[i].CodBanco = ACodBanco then
      begin
        Result := Objects[i];
        Break;
      end;
    end;
  end;

begin
  if not FLoaded then
    Load;

  Result := DoFindPosCheque(ACodBanco);
  if (Result = Nil) and UsaPadraoSeNaoAchar then
    Result := DoFindPosCheque(FBancoPadrao);
end;

procedure TACBrPosCheques.LoadFromStringList(AStringList: TStrings);
var
  Ini: TMemIniFile;
  Sections: TStringList;
  APosCheque: TACBrPosCheque;
  CodBanco, i: Integer;

  Function StrToPoint(const AValue: String): TPoint;
  var
    L, P: Integer;
  begin
    L := Length(AValue);
    P := pos(',', AValue);
    if (P = 0) then
      P := L + 1;

    Result.x := StrToIntDef(copy(AValue, 1, P-1), 0);
    Result.y := StrToIntDef(copy(AValue, P+1, L), 0);
  end;

begin
  Sections := TStringList.Create;
  Ini := TMemIniFile.Create(FFileName);
  try
    Ini.SetStrings(AStringList);

    FDimensao := StrToPoint( Ini.ReadString('Cheque', 'Dimensao', '') );
    FBancoPadrao := Ini.ReadInteger('Cheque', 'BancoPadrao', 0);

    Sections.Clear;
    Ini.ReadSections(Sections);
    For i := 0 to Sections.Count-1 do
    begin
      CodBanco := StrToIntDef(Sections[i], 0);
      if (CodBanco > 0) then
      begin
        APosCheque := TACBrPosCheque.Create;
        APosCheque.CodBanco    := CodBanco;
        APosCheque.Nome        := Ini.ReadString(Sections[i], 'Nome', '');
        APosCheque.Valor       := StrToPoint( Ini.ReadString(Sections[i], 'Valor', '') );
        APosCheque.Extenso     := StrToPoint( Ini.ReadString(Sections[i], 'Extenso', '') );
        APosCheque.Favorecido  := StrToPoint( Ini.ReadString(Sections[i], 'Favorecido', '') );
        APosCheque.Cidade      := StrToPoint( Ini.ReadString(Sections[i], 'Cidade', '') );
        APosCheque.Dia         := StrToPoint( Ini.ReadString(Sections[i], 'Dia', '') );
        APosCheque.Mes         := StrToPoint( Ini.ReadString(Sections[i], 'Mes', '') );
        APosCheque.Ano         := StrToPoint( Ini.ReadString(Sections[i], 'Ano', '') );
        APosCheque.Complemento := StrToPoint( Ini.ReadString(Sections[i], 'Complemento', '') );
        Add(APosCheque);
      end;
    end;
  finally
    Ini.Free;
    Sections.Free;
  end;
end;

function TACBrPosCheques.LoadFromFile: Boolean;
var
  SL: TStringList;
begin
  Result := False;
  if (FFileName <> '') and FileExists(FFileName) then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(FFileName);
      LoadFromStringList(SL);
      Result := True;
    finally
      SL.Free;
    end;
  end;
end;

function TACBrPosCheques.LoadFromResource: Boolean;
var
  RS: TResourceStream;
  SL: TStringList;
begin
  {$IfDef FPC}Result := False;{$EndIf}

  RS := TResourceStream.Create(HInstance, FResourceName, RT_RCDATA);
  SL := TStringList.Create;
  try
    // Leitura do Resource pode falhar
    RS.Position := 0;
    SL.LoadFromStream(RS);
    LoadFromStringList(SL);
    Result := True;
  finally
    RS.Free;
    SL.Free;
  end;
end;

procedure TACBrPosCheques.SetFileName(AValue: String);
begin
  if (FFileName = AValue) then
    Exit;

  if (AValue <> '') and (not FileExists(AValue)) then
    raise Exception.CreateFmt(ACBrStr(cACBrArquivoNaoEncontrado), [AValue]);

  FFileName := AValue;
  FLoaded := False;
end;

procedure TACBrPosCheques.SetObject(Index: Integer; Item: TACBrPosCheque);
begin
  inherited Items[Index] := Item;
end;

function TACBrPosCheques.GetObject(Index: Integer): TACBrPosCheque;
begin
  Result := TACBrPosCheque(inherited Items[Index]);
end;

procedure TACBrPosCheques.Insert(Index: Integer; Obj: TACBrPosCheque);
begin
  inherited Insert(Index, Obj);
end;

function TACBrPosCheques.Add(Obj: TACBrPosCheque): Integer;
begin
  Result := inherited Add(Obj) ;
end;

{ TACBrConfigModoPagina }

constructor TACBrConfigModoPagina.Create;
begin
  inherited;

  FLargura := 0;
  FAltura := 0;
  FEsquerda := 0;
  FTopo := 0;
  FDirecao := dirEsquerdaParaDireita;
  FEspacoEntreLinhas := 0;
end;

{ TACBrPosComandos }

constructor TACBrPosComandos.Create;
begin
  inherited;
  Clear;
end;

procedure TACBrPosComandos.Clear;
begin
  FPuloDeLinha := LF;
  FPuloDePagina := FF;
  FBeep := '';
  FAlinhadoCentro := '';
  FAlinhadoDireita := '';
  FAlinhadoEsquerda := '';
  FCorteParcial := '';
  FDesligaAlturaDupla := '';
  FDesligaInvertido := '';
  FDesligaModoPagina := '';
  FEspacoEntreLinhasPadrao := '';
  FImprimePagina := '';
  FLigaAlturaDupla := '';
  FLigaInvertido := '';
  FFonteNormal := '';
  FLigaCondensado := '';
  FCorteTotal := '';
  FEspacoEntreLinhas := '';
  FLigaExpandido := '';
  FDesligaCondensado := '';
  FDesligaExpandido := '';
  FDesligaItalico := '';
  FDesligaNegrito := '';
  FDesligaSublinhado := '';
  FFonteA := '';
  FFonteB := '';
  FLigaItalico := '';
  FLigaModoPagina := '';
  FLigaNegrito := '';
  FLigaSublinhado := '';
  FZera := '';
end;

{ TACBrConfigGaveta }

constructor TACBrConfigGaveta.Create;
begin
  inherited;
  FTempoON := 50;
  FTempoOFF := 200;
  FSinalInvertido := False;
end;

{ TACBrConfigLogo }

constructor TACBrConfigLogo.Create;
begin
  inherited;
  FKeyCode1 := 32;
  FKeyCode2 := 32;
  FFatorX := 1;
  FFatorY := 1;
  FIgnorarLogo := False;
end;

{ TACBrConfigQRCode }

constructor TACBrConfigQRCode.Create;
begin
  inherited;

  FTipo := 2;
  FLarguraModulo := 4;
  FErrorLevel := 0;
end;

procedure TACBrConfigQRCode.SetLarguraModulo(AValue: Integer);
begin
  FLarguraModulo := max(min(AValue,16),1);
end;

procedure TACBrConfigQRCode.SetErrorLevel(AValue: Integer);
begin
  FErrorLevel := max(min(AValue,3),0);
end;

procedure TACBrConfigQRCode.SetTipo(AValue: Integer);
begin
  FTipo := max(min(AValue,2),1);
end;

{ TACBrPosRazaoColunaFonte }

constructor TACBrPosRazaoColunaFonte.Create;
begin
  inherited Create;

  FCondensada := 0.75;
  FExpandida := 2;
end;

{ TACBrPosPrinterClass }

constructor TACBrPosPrinterClass.Create(AOwner: TACBrPosPrinter);
begin
  inherited Create;

  fpModeloStr := 'Texto';
  fpPosPrinter := AOwner;

  FCmd := TACBrPosComandos.Create;
  FRazaoColunaFonte := TACBrPosRazaoColunaFonte.Create;
  FTagsNaoSuportadas := TStringList.Create;
  FInfo := TStringList.Create;
end;

destructor TACBrPosPrinterClass.Destroy;
begin
  FCmd.Free;
  FRazaoColunaFonte.Free;
  FTagsNaoSuportadas.Free;
  FInfo.Free;

  inherited;
end;

Procedure TACBrPosPrinterClass.AddInfo(const ATitulo: String; const AStr: AnsiString);
var
  InfoStr: String;
begin
  InfoStr := Trim(AStr);
  if (InfoStr = '') then
    Exit;

  if CharInSet(InfoStr[1], ['_',':']) then
    Delete(InfoStr, 1, 1);

  FInfo.Values[ATitulo] := InfoStr;
end;

procedure TACBrPosPrinterClass.AddInfo(const ATitulo: String; const ABool: Boolean);
var
  c: AnsiChar;
begin
  if ABool then
    c := '1'
  else
    c := '0';

  AddInfo(ATitulo, c);
end;

procedure TACBrPosPrinterClass.AntesDecodificar(var ABinaryString: AnsiString);
begin
  { não altera ABinaryString aqui, sobreescrever se necessário }
end;

procedure TACBrPosPrinterClass.AdicionarBlocoResposta(
  const ConteudoBloco: AnsiString);
begin
  { não faz nada aqui, sobreescrever se necessário }
end;

procedure TACBrPosPrinterClass.DepoisDecodificar(var ABinaryString: AnsiString);
begin
  { não altera ABinaryString aqui, sobreescrever se necessário }
end;

function TACBrPosPrinterClass.TraduzirTag(const ATag: AnsiString;
  var TagTraduzida: AnsiString): Boolean;
begin
  Result := False;  // Não traduziu aqui, sobreescrever se necessário;
end;

function TACBrPosPrinterClass.TraduzirTagBloco(const ATag,
  ConteudoBloco: AnsiString; var BlocoTraduzido: AnsiString): Boolean;
begin
  Result := False;  // Não traduziu aqui, sobreescrever se necessário
end;

function TACBrPosPrinter.ValidaLogoBmp(const APathLogo: string): Boolean;
var
  LFileStream : TFileStream;
  LPathLogo : String;
begin
   Result := False;

   LPathLogo := Trim(APathLogo);

   if (LPathLogo = '') or not FileExists(LPathLogo) then
     Exit;

   LFileStream := TFileStream.Create(LPathLogo, fmOpenRead or fmShareDenyWrite);
   try
     if not IsBMP(LFileStream, True) then
       Exit;
       //if LFileStream.Size <= 5000 then // Menor ou igual a 5 KB -- se necessário validar, ficar para futuro
       Result := True;
   finally
     LFileStream.Free;
   end;
end;

function TACBrPosPrinterClass.ComandoCodBarras(const ATag: String;
  const ACodigo: AnsiString): AnsiString;
begin
  Result := ACodigo;
end;

function TACBrPosPrinterClass.ComandoQrCode(const ACodigo: AnsiString): AnsiString;
begin
  Result := ACodigo;
end;

function TACBrPosPrinterClass.ComandoEspacoEntreLinhas(Espacos: byte): AnsiString;
begin
  if Espacos = 0 then
    Result := Cmd.EspacoEntreLinhasPadrao
  else
  begin
    if Length(Cmd.EspacoEntreLinhas) > 0 then
      Result := Cmd.EspacoEntreLinhas + AnsiChr(Espacos)
    else
      Result := '';
  end;
end;

function TACBrPosPrinterClass.ComandoPaginaCodigo(
  APagCodigo: TACBrPosPaginaCodigo): AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoGaveta(NumGaveta: Integer): AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoInicializa: AnsiString;
begin
  Result := ComandoEspacoEntreLinhas(fpPosPrinter.EspacoEntreLinhas) +
            ComandoPaginaCodigo(fpPosPrinter.PaginaDeCodigo);
end;

function TACBrPosPrinterClass.ComandoPuloLinhas(NLinhas: Integer): AnsiString;
begin
  Result := AnsiString( DupeString(' '+Cmd.PuloDeLinha,NLinhas) );
end;

function TACBrPosPrinterClass.ComandoFonte(TipoFonte: TACBrPosTipoFonte;
  Ligar: Boolean): AnsiString;
begin
  Result := '';

  case TipoFonte of
    ftExpandido:
      if Ligar then
        Result := Cmd.LigaExpandido
      else
        Result := Cmd.DesligaExpandido;

    ftAlturaDupla:
      if Ligar then
        Result := Cmd.LigaAlturaDupla
      else
        Result := Cmd.DesligaAlturaDupla;

    ftCondensado:
      if Ligar then
        Result := Cmd.LigaCondensado
      else
        Result :=  Cmd.DesligaCondensado;

    ftNegrito:
      if Ligar then
        Result := Cmd.LigaNegrito
      else
        Result := Cmd.DesligaNegrito;

    ftItalico:
      if Ligar then
        Result := Cmd.LigaItalico
      else
         Result := Cmd.DesligaItalico;

    ftInvertido:
       if Ligar then
         Result := Cmd.LigaInvertido
       else
         Result := Cmd.DesligaInvertido;

    ftSublinhado:
       if Ligar then
         Result := Cmd.LigaSublinhado
       else
         Result := Cmd.DesligaSublinhado;

    ftFonteB:
      if Ligar then
        Result := Cmd.FonteB
      else
        Result := Cmd.FonteA;
  end;
end;

function TACBrPosPrinterClass.ComandoConfiguraModoPagina: AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoPosicionaModoPagina(APoint: TPoint): AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoCarregaCheque(LerCMC7: Boolean; SegundosEspera: Byte): AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoEjetarCheque: AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoImprimirImagemRasterStr(const RasterStr: AnsiString;
  AWidth: Integer; AHeight: Integer): AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoImprimirImagemArquivo(ArquivoBMP: String): AnsiString;
var
  AWidth, AHeight: Integer;
  ARasterStr: AnsiString;
begin
  AWidth := 0; AHeight := 0; ARasterStr := '';
  ArquivoImagemToRasterStr(ArquivoBMP, AWidth, AHeight, ARasterStr);
  Result := ComandoImprimirImagemRasterStr(ARasterStr, AWidth, AHeight);
end;

function TACBrPosPrinterClass.ComandoImprimirImagemStream(ABMPStream: TStream
  ): AnsiString;
var
  AWidth, AHeight: Integer;
  ARasterStr: AnsiString;
begin
  AWidth := 0; AHeight := 0; ARasterStr := '';
  BMPMonoToRasterStr(ABMPStream, True, AWidth, AHeight, ARasterStr );

  Result := ComandoImprimirImagemRasterStr(ARasterStr, AWidth, AHeight);
end;

function TACBrPosPrinterClass.ComandoLogo: AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoGravarLogoRasterStr(const RasterStr: AnsiString;
  AWidth: Integer; AHeight: Integer): AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoGravarLogoArquivo(ArquivoBMP: String): AnsiString;
var
  AWidth, AHeight: Integer;
  ARasterStr: AnsiString;
begin
  AWidth := 0; AHeight := 0; ARasterStr := '';
  ArquivoImagemToRasterStr(ArquivoBMP, AWidth, AHeight, ARasterStr);
  Result := ComandoGravarLogoRasterStr(ARasterStr, AWidth, AHeight);
end;

function TACBrPosPrinterClass.ComandoGravarLogoStream(ABMPStream: TStream
  ): AnsiString;
var
  AWidth, AHeight: Integer;
  ARasterStr: AnsiString;
begin
  AWidth := 0; AHeight := 0; ARasterStr := '';
  BMPMonoToRasterStr(ABMPStream, True, AWidth, AHeight, ARasterStr );

  Result := ComandoGravarLogoRasterStr(ARasterStr, AWidth, AHeight);
end;

function TACBrPosPrinterClass.ComandoApagarLogo: AnsiString;
begin
  Result := '';
end;

procedure TACBrPosPrinterClass.ArquivoImagemToRasterStr(ArquivoImagem: String; out
  AWidth: Integer; out AHeight: Integer; out ARasterStr: AnsiString);
var
  {$IfNDef NOGUI}
   ABitMap: TBitmap;
  {$Else}
   MS: TMemoryStream;
  {$EndIf}
begin
  AWidth := 0; AHeight := 0; ARasterStr := '';
  if (Trim(ArquivoImagem) = '') then
    Exit;

  if not FileExists(ArquivoImagem) then
    raise EPosPrinterException.Create(ACBrStr(Format(cACBrArquivoNaoEncontrado,[ArquivoImagem])));

  {$IfNDef NOGUI}
   ABitMap := TBitmap.Create;
   try
     ABitMap.LoadFromFile(ArquivoImagem);
     BitmapToRasterStr(ABitMap, True, AWidth, AHeight, ARasterStr);
   finally
     ABitMap.Free;
   end;
  {$Else}
   MS := TMemoryStream.Create;
   try
     MS.LoadFromFile(ArquivoImagem);
     BMPMonoToRasterStr(MS, True, AWidth, AHeight, ARasterStr );
   finally
     MS.Free;
   end;
  {$EndIf}
end;

procedure TACBrPosPrinterClass.Configurar;
begin
  {nada aqui, método virtual}
end;

procedure TACBrPosPrinterClass.LerStatus(var AStatus: TACBrPosPrinterStatus);
begin
  {nada aqui, método virtual}
end;

function TACBrPosPrinterClass.LerInfo: String;
begin
  Result := '';
end;

function TACBrPosPrinterClass.ComandoLerCMC7(SegundosEspera: Byte): AnsiString;
begin
  Result := '';
end;

function TACBrPosPrinterClass.LeituraCheque: AnsiString;
begin
  Result := '';
end;

{ TACBrPosPrinter }

constructor TACBrPosPrinter.Create(AOwner: TComponent);
const
  CTAGS_TIPOFONTE: array[0..6] of String =
    (cTagFonteA, cTagFonteB, cTagLigaInvertido, cTagDesligaInvertido,
     cTagFonteAlinhadaEsquerda, cTagfonteAlinhadaCentro, cTagFonteAlinhadaDireita);
  CTAGS_TIPOFONTE_HELP: array[0..6] of String =
    ('Liga Fonte Tipo A (normal)',
     'Liga Fonte Tipo B (condensada)',
     'Liga Fonte Invertida (Fundo Preto)', 'Desliga Fonte Invertida',
     'Liga Alinhamento a Esquerda',
     'Liga Alinhamento ao Centro',
     'Liga Alinhamento a Direita');
begin
  inherited Create(AOwner);

  FDevice := TACBrDevice.Create(Self);
  FDevice.Name := 'ACBrDevice' ;      { Apenas para aparecer no Object Inspector}
  FDevice.TimeOutMilissegundos := cTimeOutTxRx;
  {$IFDEF COMPILER6_UP}
  FDevice.SetSubComponent( true );{ para gravar no DFM/XFM }
  {$ENDIF}
  {$IfDef MSWINDOWS}
  FDevice.WinUSB.HardwareType := htPOSPrinter;
  FHook := Nil;
  {$EndIf}

  FPosPrinterClass := TACBrPosPrinterClass.Create(Self);
  FPosPrinterClass.RazaoColunaFonte.Condensada := 1;
  FPosPrinterClass.RazaoColunaFonte.Expandida := 1;
  FModelo := ppTexto;

  FTipoAlinhamento := alEsquerda;
  FFonteStatus := [ftNormal];
  FInicializada := False;
  FModoPaginaLigado := False;

  FConfigBarras := TACBrECFConfigBarras.Create;
  FConfigQRCode := TACBrConfigQRCode.Create;
  FConfigLogo   := TACBrConfigLogo.Create;
  FConfigGaveta := TACBrConfigGaveta.Create;
  FConfigModoPagina := TACBrConfigModoPagina.Create;

  FPosCheques := TACBrPosCheques.Create;

  FTemGuilhotina := -2;
  FTemCheque := -2;
  FTemAutenticacao := -2;
  FTemMICR := -2;

  FTagProcessor := TACBrTagProcessor.Create;
  FTagProcessor.AddTags(cTAGS_CARACTER, cTAGS_CARACTER_HELP, False);
  FTagProcessor.AddTags(CTAGS_TIPOFONTE, CTAGS_TIPOFONTE_HELP, False);
  FTagProcessor.AddTags(cTAGS_LINHAS, cTAGS_LINHAS_HELP, False);
  FTagProcessor.AddTags(cTAGS_FUNCOES, cTAGS_FUNCOES_HELP, False);
  FTagProcessor.AddTags(cTAGS_ALINHAMENTO, cTAGS_ALINHAMENTO_HELP, True);
  FTagProcessor.AddTags(cTAGS_BARRAS, cTAGS_BARRAS_HELP, True);

  // Abertura de Gaveta específica //
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagAbreGavetaEsp;
    Ajuda := 'Abertura de Gaveta Específica (1 ou 2)';
    EhBloco := True;
  end;

  // Tags de Configurações do Cod.Barras //
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagBarraMostrar;
    Ajuda := 'Configura se deve exibir conteudo abaixo do Cod.Barras: 0-NAO; 1-SIM';
    EhBloco := True;
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagBarraLargura;
    Ajuda := 'Configura a Largura das Barras do Cod.Barras: 0 a 5. (0=default)';
    EhBloco := True;
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagBarraAltura;
    Ajuda := 'Configura a Altura do Cod.Barras: 0 a 255. (0=default)';
    EhBloco := True;
  end;

  // Tags de QRCode e configuração do QRCode //
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagQRCode;
    Ajuda := 'Imprime QRCode de acordo com "ConfigQRCode"';
    EhBloco := True;
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagQRCodeTipo;
    Ajuda := 'Configura o Tipo de QRCode: 1,2';
    EhBloco := True;
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagQRCodeLargura;
    Ajuda := 'Configura a Largura do QRCode: 1 a 16';
    EhBloco := True;
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagQRCodeError;
    Ajuda := 'Configura o Error Level do QRCode: 0 a 3';
    EhBloco := True;
  end;

  // Tag de Impressão de Imagem
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagBMP;
    Ajuda := 'Imprime Imagem BMP monocromática. Conteúdo pode ser: Path da Imagem, Stream em Base64 ou AscII Art (0 e 1)';
    EhBloco := True;
  end;

  // Tags de Região e configuração do Região //
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagModoPaginaLiga;
    Ajuda := 'Liga Modo de Impressão em Página (em memória)';
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagModoPaginaDesliga;
    Ajuda := 'Desliga Modo de Impressão Página (em memória)';
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagModoPaginaImprimir;
    Ajuda := 'Comanda a Impressão da Página na memória';
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagModoPaginaDirecao;
    Ajuda := 'Direção Texto no Modo Página: 0-Esquerda/Direta, 1-Topo/Baixo, 2-Direita/Esquerda, 3-Baixo/Topo';
    EhBloco := True;
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagModoPaginaPosEsquerda;
    Ajuda := 'Posição Inicial Horizontal Modo Página (Esquerda)';
    EhBloco := True;
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagModoPaginaPosTopo;
    Ajuda := 'Posição Inicial Vertical Modo Página (Topo)';
    EhBloco := True;
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagModoPaginaLargura;
    Ajuda := 'Largura da Região no Modo Página';
    EhBloco := True;
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagModoPaginaAltura;
    Ajuda := 'Altura da Região no Modo Página';
    EhBloco := True;
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagModoPaginaEspaco;
    Ajuda := 'Espaço entre Linhas na Região no Modo Página';
    EhBloco := True;
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagModoPaginaConfigurar;
    Ajuda := 'Envia a configuração de Coordenadas da Região e direção do Modo Página';
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagModoPaginaPosiciona;
    Ajuda := 'Define Posição X,Y no Modo Página';
    EhBloco := True;
  end;

  // Tags de configuração do LogoTipo //
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagLogoImprimir;
    Ajuda := 'Configura a Impressão ou não do Logo Tipo: 0-NÃO, 1-SIM (default = 1)';
    EhBloco := True;
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagLogoKC1;
    Ajuda := 'Configura a posição KC1 do Logo a ser impresso. Ex: 0=48';
    EhBloco := True;
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagLogoKC2;
    Ajuda := 'Configura a posição KC2 do Logo a ser impresso. Ex: 1=49';
    EhBloco := True;
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagLogoFatorX;
    Ajuda := 'Configura o aumento Horizonal do Logo, de 1 a 4';
    EhBloco := True;
  end;
  with FTagProcessor.Tags.New do
  begin
    Nome := cTagLogoFatorY;
    Ajuda := 'Configura o aumento Vertical do Logo, de 1 a 4';
    EhBloco := True;
  end;

  with FTagProcessor.Tags.New do
  begin
    Nome := cTagIgnorarTags;
    Ajuda := 'Ignora todas as Tags contidas no Bloco';
    EhBloco := True;
  end;

  FTagProcessor.OnTraduzirTag := DoTraduzirTag;
  FTagProcessor.OnAdicionarBlocoResposta := DoAdicionarBlocoResposta;
  FTagProcessor.OnTraduzirTagBloco := DoTraduzirTagBloco;

  FBuffer := TStringList.Create;
  FBuffer.OnChange := DoLinesChange;

  FColunasFonteNormal := 48;
  FPaginaDeCodigo := pc850;
  FEspacoEntreLinhas := 0;
  FControlePorta := False;
  FCortaPapel := True;
  FVerificarImpressora := False;

  FArqLog := '';
  FOnGravarLog := nil;
  FOnEnviarStringDevice := nil;
  FOnEnviarStringDeviceDefault := nil;
  FOnAguardarCheque := nil;
  FCancelarEsperaCheque := False;

  FTipoCorte := ctTotal;
end;

procedure TACBrPosPrinter.Loaded;
begin
  inherited Loaded;
  FOnEnviarStringDeviceDefault := FOnEnviarStringDevice;
end;

destructor TACBrPosPrinter.Destroy;
begin
  {$IfDef MSWINDOWS}
  LiberarHook;
  {$EndIf}
  if (FPosPrinterClass <> FModeloExterno) then
    FPosPrinterClass.Free;
  FBuffer.Free;
  FPosCheques.Free;
  FTagProcessor.Free;
  FConfigBarras.Free;
  FConfigQRCode.Free;
  FConfigLogo.Free;
  FConfigGaveta.Free;
  FConfigModoPagina.Free;
  FreeAndNil(FDevice);

  inherited Destroy;
end;

procedure TACBrPosPrinter.AbrirGaveta(NumGaveta: Integer);
begin
  GravarLog('AbrirGaveta');
  ImprimirCmd(FPosPrinterClass.ComandoGaveta(NumGaveta));
  Sleep(500);
end;

procedure TACBrPosPrinter.Ativar;
var
  {$IfDef MSWINDOWS}
  TipoHardware: TACBrUSBHardwareType;
  ProtocoloACBr: Integer;
  {$EndIf}
  DadosDevice: String;
begin
  if FAtivo then
    Exit;

{(*}
  DadosDevice := '  - Porta..: '+FDevice.Porta + sLineBreak +
                 '  - Tipo...: '+copy(GetEnumName(TypeInfo(TACBrDeviceType), integer(FDevice.DeviceType)),3,20);

  if FDevice.IsSerialPort then
    DadosDevice := DadosDevice + sLineBreak +
                   '  - Params.: '+FDevice.DeviceToString(False);

  GravarLog(AnsiString(sLineBreak + StringOfChar('-', 80) + sLineBreak +
            'ATIVAR - ' + FormatDateTime('dd/mm/yy hh:nn:ss:zzz', now) + sLineBreak +
            '  - Modelo.: ' + FPosPrinterClass.ModeloStr + sLineBreak +
            '  - TimeOut: ' + IntToStr(FDevice.TimeOutMilissegundos) + ' milissegundos' + sLineBreak +
            DadosDevice + sLineBreak +
            StringOfChar('-', 80) + sLineBreak),
            False, False);
  {*)}

  {$IfDef MSWINDOWS}
   DetectarECriarHook;
   ProtocoloACBr := 0;
   TipoHardware := htPOSPrinter;
   FDevice.DetectarTipoEProtocoloDispositivoUSB(TipoHardware, ProtocoloACBr);

   if not (TipoHardware in [htUnknown, htPOSPrinter]) then
     raise EPosPrinterException.Create(ACBrStr('Porta: '+FDevice.Porta+' não está conectada a uma Impressora'));

   if (ProtocoloACBr > 0) then
     Modelo := TACBrPosPrinterModelo(ProtocoloACBr);
  {$EndIf}

  FDevice.Ativar;
  FAtivo := True;
  FPosPrinterClass.Configurar;
  FInicializada := False;
  FTemGuilhotina := -2;
  FTemCheque := -2;
  FTemAutenticacao := -2;
  FTemMICR := -2;
end;

procedure TACBrPosPrinter.Desativar;
begin
  if not FAtivo then
    Exit;

  GravarLog(AnsiString(sLineBreak + StringOfChar('-', 80) + sLineBreak +
    'DESATIVAR - ' + FormatDateTime('dd/mm/yy hh:nn:ss:zzz', now) +
    sLineBreak + StringOfChar('-', 80) + sLineBreak),
    False, False);

  FDevice.Desativar;
  FAtivo := False;
  FInicializada := False;
  FTemGuilhotina := -2;
  FTemCheque := -2;
  FTemAutenticacao := -2;
  FTemMICR := -2;
end;

procedure TACBrPosPrinter.SetModelo(AValue: TACBrPosPrinterModelo);
begin
  if FModelo = AValue then
    Exit;

  if Ativo then
    Desativar;

  GravarLog('SetModelo(' + AnsiString(GetEnumName(TypeInfo(TACBrPosPrinterModelo),
    integer(AValue))) + ')');

  if (AValue = ppExterno) and (not Assigned(FModeloExterno)) then
    raise EPosPrinterException.Create(ACBrStr('Nenhum Modelo Externo atribuído'));

  if (FPosPrinterClass <> FModeloExterno) then
    FPosPrinterClass.Free
  else
    FOnEnviarStringDevice := FOnEnviarStringDeviceDefault;

  case AValue of
    ppEscPosEpson: FPosPrinterClass := TACBrEscPosEpson.Create(Self);
    ppEscBematech: FPosPrinterClass := TACBrEscBematech.Create(Self);
    ppEscDaruma : FPosPrinterClass := TACBrEscDaruma.Create(Self);
    ppEscVox : FPosPrinterClass := TACBrEscElgin.Create(Self);
    ppEscDiebold : FPosPrinterClass := TACBrEscDiebold.Create(Self);
    ppEscEpsonP2 : FPosPrinterClass := TACBrEscEpsonP2.Create(self);
    ppCustomPos : FPosPrinterClass := TACBrEscCustomPos.Create(self);
    ppEscPosStar: FPosPrinterClass := TACBrEscPosStar.Create(Self);
    ppEscZJiang: FPosPrinterClass := TACBrEscZJiang.Create(Self);
    ppEscGPrinter: FPosPrinterClass := TACBrEscGPrinter.Create(Self);
    ppEscDatecs: FPosPrinterClass := TACBrEscDatecs.Create(Self);
    ppEscSunmi:  FPosPrinterClass := TACBrEscSunmi.Create(Self);
    ppExterno: FPosPrinterClass := FModeloExterno;
  else
    FPosPrinterClass := TACBrPosPrinterClass.Create(Self);
    FPosPrinterClass.RazaoColunaFonte.Condensada := 1;
    FPosPrinterClass.RazaoColunaFonte.Expandida := 1;
  end;

  FModelo := AValue;
end;

procedure TACBrPosPrinter.SetModeloExterno(
  APosPrinterClass: TACBrPosPrinterClass);
begin
  if (FModeloExterno = APosPrinterClass) then
    Exit;

  GravarLog('SetModeloExterno()');

  if Assigned(APosPrinterClass) then
  begin
    if (FPosPrinterClass <> FModeloExterno) then
      FPosPrinterClass.Free;

    FModeloExterno := APosPrinterClass;
    FPosPrinterClass := APosPrinterClass;
    FModelo := ppExterno;

    FModeloExterno.Configurar;
  end
  else
    Modelo := ppTexto;
end;

procedure TACBrPosPrinter.VerificarParametrosLogo(const AKC2: Integer;
  const AKC1: Integer);
begin
  if AKC1 >= 0 then
    FConfigLogo.KeyCode1 := AKC1;

  if AKC2 >= 0 then
    FConfigLogo.KeyCode2 := AKC2;
end;

function TACBrPosPrinter.ProcessarComandoBMP(const ConteudoBloco: String
  ): AnsiString;
var
  ARasterStr: AnsiString;
  AHeight, AWidth: Integer;
  SL: TStringList;
  AData: String;
  SS: TStringStream;
begin
  AData := Trim(ConteudoBloco);

  Result := '';
  if (AData = '') then
    Exit;

  if StrIsBinary(LeftStr(AData,10)) then           // AscII Art
  begin
    SL := TStringList.Create;
    try
      SL.Text := AData;
      AWidth := 0; AHeight := 0; ARasterStr := '';
      AscIIToRasterStr(SL, AWidth, AHeight, ARasterStr);
    finally
      SL.Free;
    end;

    if (Length(ARasterStr) > 0) and (AWidth > 0) and (AHeight > 0) then
      Result := FPosPrinterClass.ComandoImprimirImagemRasterStr(ARasterStr, AWidth, AHeight);
  end

  else if StrIsBase64(AData) then
  begin
    SS := TStringStream.Create(DecodeBase64(AData));
    try
      Result := FPosPrinterClass.ComandoImprimirImagemStream(SS);
    finally
      SS.Free;
    end;
  end

  else
    Result := FPosPrinterClass.ComandoImprimirImagemArquivo(AData);
end;

function TACBrPosPrinter.ProcessarPosicionaXY(const ConteudoBloco: String): AnsiString;
var
  L, P, X, Y: Integer;
begin
  L := Length(ConteudoBloco);
  P := pos(',', ConteudoBloco);
  if (P = 0) then
    P := L+1;

  X := StrToIntDef(copy(ConteudoBloco, 1, P-1), 0);
  Y := StrToIntDef(copy(ConteudoBloco, P+1, L), 0);

  Result := FPosPrinterClass.ComandoPosicionaModoPagina(Point(x,y));
end;

procedure TACBrPosPrinter.LerInfoDispositivos;
var
  SL: TStringList;
begin
  if not PodeLerDaPorta then
  begin
    FTemGuilhotina := -1;
    FTemCheque := -1;
    FTemAutenticacao := -1;
    FTemMICR := -1;
    Exit;
  end;

  SL := TStringList.Create;
  try
    SL.Text := LerInfoImpressora;
    FTemGuilhotina := StrToIntDef(SL.Values[cKeyGuilhotina], 0);
    FTemCheque := StrToIntDef(SL.Values[cKeyCheque], 0);
    FTemAutenticacao := StrToIntDef(SL.Values[cKeyAutenticacao], 0);
    FTemMICR := StrToIntDef(SL.Values[cKeyMICR], 0);
  finally
    SL.Free;
  end;
end;

procedure TACBrPosPrinter.DoLinesChange(Sender: TObject);
begin
  if (FLinhasBuffer > 0) and (FBuffer.Count > FLinhasBuffer) then
  begin
    GravarLog(AnsiString('Esvaziando Buffer: ' + IntToStr(FBuffer.Count) + ' linhas'));
    Imprimir;
  end;
end;

function TACBrPosPrinter.GetColunas: Integer;
begin
  if (ftCondensado in FFonteStatus) then
    Result := ColunasFonteCondensada
  else if (ftExpandido in FFonteStatus) then
    Result := ColunasFonteExpandida
  else
    Result := ColunasFonteNormal;
end;

function TACBrPosPrinter.GetColunasFonteCondensada: Integer;
begin
  Result := trunc(ColunasFonteNormal / FPosPrinterClass.RazaoColunaFonte.Condensada)
end;

function TACBrPosPrinter.ComandoConfigurarModoPaginaCheque: AnsiString;
begin
  Result := FPosPrinterClass.Cmd.LigaModoPagina;

  with FConfigModoPagina do
  begin
    EspacoEntreLinhas := 0;
    Direcao := dirBaixoParaTopo;
    Esquerda := 0;
    Topo := 0;
    Largura := 540;
    Altura := 858;
  end;

  Result := Result + FPosPrinterClass.ComandoConfiguraModoPagina;
end;

function TACBrPosPrinter.ComandoTextoPosCheque(const AText: AnsiString; APos, ADimensao: TPoint): String;
var
  RelPos: TPoint;
begin
  RelPos.x := trunc(max(ADimensao.x-APos.x,1) * 5.65);
  RelPos.y := trunc(max(ADimensao.y-APos.y,1) * 6.2);

  Result := FPosPrinterClass.ComandoPosicionaModoPagina(RelPos) +
            DecodificarTagsFormatacao( CodificarPaginaDeCodigo(AText) ) +
            FPosPrinterClass.Cmd.PuloDeLinha;
end;

function TACBrPosPrinter.GetColunasFonteExpandida: Integer;
begin
  Result := trunc(ColunasFonteNormal / FPosPrinterClass.RazaoColunaFonte.Expandida)
end;

function TACBrPosPrinter.GetLeituraCheque: AnsiString;
begin
  GravarLog('GetLeituraCheque');
  Result := FPosPrinterClass.LeituraCheque;
end;

procedure TACBrPosPrinter.DoTraduzirTag(const ATag: AnsiString;
  var TagTraduzida: AnsiString);
begin
  // GravarLog(AnsiString('TraduzirTag(' + ATag + ')'));   // DEBUG - permite medir de tradução de cada Tag
  TagTraduzida := '';
  if (TagsNaoSuportadas.IndexOf(ATag) >= 0) then
    Exit;

  if not FPosPrinterClass.TraduzirTag(ATag, TagTraduzida) then
  begin
    TagTraduzida := '';
    if (ATag = cTagLigaExpandido) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftExpandido, True)
    else if (ATag = cTagDesligaExpandido) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftExpandido, False)
    else if (ATag = cTagLigaAlturaDupla) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftAlturaDupla, True)
    else if (ATag = cTagDesligaAlturaDupla) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftAlturaDupla, False)
    else if (ATag = cTagLigaNegrito) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftNegrito, True)
    else if (ATag = cTagDesligaNegrito) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftNegrito, False)
    else if (ATag = cTagLigaSublinhado) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftSublinhado, True)
    else if (ATag = cTagDesligaSublinhado) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftSublinhado, False)
    else if (ATag = cTagLigaCondensado) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftCondensado, True)
    else if (ATag = cTagDesligaCondensado) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftCondensado, False)
    else if (ATag = cTagLigaItalico) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftItalico, True)
    else if (ATag = cTagDesligaItalico) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftItalico, False)
    else if (ATag = cTagLigaInvertido) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftInvertido, True)
    else if (ATag = cTagDesligaInvertido) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftInvertido, False)
    else if (ATag = cTagFonteA) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftFonteB, False)
    else if (ATag = cTagFonteB) then
      TagTraduzida := FPosPrinterClass.ComandoFonte(ftFonteB, True)
    else if (ATag = cTagFonteAlinhadaEsquerda) then
      TagTraduzida := FPosPrinterClass.Cmd.AlinhadoEsquerda
    else if (ATag = cTagFonteAlinhadaDireita) then
      TagTraduzida := FPosPrinterClass.Cmd.AlinhadoDireita
    else if (ATag = cTagfonteAlinhadaCentro) then
      TagTraduzida := FPosPrinterClass.Cmd.AlinhadoCentro
    else if (ATag = cTagFonteNormal) then
      TagTraduzida := FPosPrinterClass.Cmd.FonteNormal
    else if (ATag = cTagZera) then
      TagTraduzida := FPosPrinterClass.Cmd.Zera + FPosPrinterClass.ComandoInicializa
    else if (ATag = cTagReset) then
      TagTraduzida := FPosPrinterClass.Cmd.Zera
    else if (ATag = cTagLinhaSimples) then
      TagTraduzida := AnsiString(StringOfChar('-', Colunas))
    else if (ATag = cTagLinhaDupla) then
      TagTraduzida := AnsiString(StringOfChar('=', Colunas))
    else if (ATag = cTagPuloDeLinhas) then
      TagTraduzida := FPosPrinterClass.ComandoPuloLinhas(LinhasEntreCupons)
    else if (ATag = cTagCorteParcial) or ( (ATag = cTagCorte) and (FTipoCorte = ctParcial) ) then
    begin
      TagTraduzida := FPosPrinterClass.ComandoPuloLinhas(LinhasEntreCupons);
      if CortaPapel then
        TagTraduzida := TagTraduzida + FPosPrinterClass.Cmd.CorteParcial;
    end
    else if (ATag = cTagCorteTotal) or ( (ATag = cTagCorte) and (FTipoCorte = ctTotal) ) then
    begin
      TagTraduzida := FPosPrinterClass.ComandoPuloLinhas(LinhasEntreCupons);
      if CortaPapel then
        TagTraduzida := TagTraduzida + FPosPrinterClass.Cmd.CorteTotal;
    end
    else if (ATag = cTagAbreGaveta) then
      TagTraduzida := FPosPrinterClass.ComandoGaveta()
    else if (ATag = cTagBeep) then
      TagTraduzida := FPosPrinterClass.Cmd.Beep
    else if (ATag = cTagLogotipo) then
    begin
      if FConfigLogo.IgnorarLogo then
        TagTraduzida := ''
      else
        TagTraduzida := FPosPrinterClass.ComandoLogo;
    end
    else if (ATag = cTagPulodeLinha) then
      TagTraduzida := FPosPrinterClass.ComandoPuloLinhas(1)
    else if (ATag = cTagPulodePagina) then
      TagTraduzida := FPosPrinterClass.Cmd.PuloDePagina
    else if (ATag = cTagRetornoDeCarro) then
      TagTraduzida := CR
    else if (ATag = cTagModoPaginaLiga) then
      TagTraduzida := FPosPrinterClass.Cmd.LigaModoPagina
    else if (ATag = cTagModoPaginaDesliga) then
    begin
      TagTraduzida := FPosPrinterClass.Cmd.ImprimePagina +
                      FPosPrinterClass.Cmd.DesligaModoPagina;
    end
    else if (ATag = cTagModoPaginaImprimir) then
      TagTraduzida := FPosPrinterClass.Cmd.ImprimePagina
    else if (ATag = cTagModoPaginaConfigurar) then
      TagTraduzida := FPosPrinterClass.ComandoConfiguraModoPagina;
  end;

  AjustarEstadoFonteAlinhamento(ATag);

  GravarLog(AnsiString('TraduzirTag(' + ATag + ') -> ') + TagTraduzida, True);
end;

procedure TACBrPosPrinter.DoAdicionarBlocoResposta(
  const ConteudoBloco: AnsiString);
begin
  FPosPrinterClass.AdicionarBlocoResposta(ConteudoBloco);
end;

procedure TACBrPosPrinter.DoTraduzirTagBloco(const ATag, ConteudoBloco: AnsiString;
  var BlocoTraduzido: AnsiString);
var
  ACodBar: AnsiString;
begin
  BlocoTraduzido := ConteudoBloco;
  if (TagsNaoSuportadas.IndexOf(ATag) >= 0) then
    Exit;

  if FPosPrinterClass.TraduzirTagBloco(ATag, ConteudoBloco, BlocoTraduzido) then
    Exit;

  BlocoTraduzido := ConteudoBloco;
  if ATag = cTagAlinhadoEsquerda then
    BlocoTraduzido := PadRightA(ConteudoBloco, Colunas)

  else if ATag = cTagAlinhadoDireita then
    BlocoTraduzido := PadLeftA(ConteudoBloco, Colunas)

  else if ATag = cTagAlinhadoCentro then
    BlocoTraduzido := PadCenterA(ConteudoBloco, Colunas)

  else if ATag = cTagAbreGavetaEsp then
    BlocoTraduzido := FPosPrinterClass.ComandoGaveta( StrToIntDef( ConteudoBloco, 1) )

  else if ATag = cTagQRCodeTipo then
  begin
    BlocoTraduzido := '';
    ConfigQRCode.Tipo := StrToIntDef( ConteudoBloco, ConfigQRCode.Tipo);
  end

  else if ATag = cTagQRCodeLargura then
  begin
    BlocoTraduzido := '';
    ConfigQRCode.LarguraModulo := StrToIntDef( ConteudoBloco, ConfigQRCode.LarguraModulo);
  end

  else if ATag = cTagQRCodeError then
  begin
    BlocoTraduzido := '';
    ConfigQRCode.ErrorLevel := StrToIntDef( ConteudoBloco, ConfigQRCode.ErrorLevel);
  end

  else if ATag = cTagQRCode then
  begin
    BlocoTraduzido := FPosPrinterClass.ComandoQrCode(ConteudoBloco);
  end

  else if ATag = cTagBMP then
  begin
    BlocoTraduzido := ProcessarComandoBMP(ConteudoBloco);
  end

  else if ATag = cTagModoPaginaDirecao then
  begin
    BlocoTraduzido := '';
    ConfigModoPagina.Direcao := TACBrPosDirecao(StrToIntDef(ConteudoBloco, 0));
  end

  else if ATag = cTagModoPaginaPosEsquerda then
  begin
    BlocoTraduzido := '';
    ConfigModoPagina.Esquerda := StrToIntDef(ConteudoBloco, ConfigModoPagina.Esquerda);
  end

  else if ATag = cTagModoPaginaPosTopo then
  begin
    BlocoTraduzido := '';
    ConfigModoPagina.Topo := StrToIntDef(ConteudoBloco, ConfigModoPagina.Topo);
  end

  else if ATag = cTagModoPaginaAltura then
  begin
    BlocoTraduzido := '';
    ConfigModoPagina.Altura := StrToIntDef(ConteudoBloco, ConfigModoPagina.Altura);
  end

  else if ATag = cTagModoPaginaEspaco then
  begin
    BlocoTraduzido := '';
    ConfigModoPagina.EspacoEntreLinhas := StrToIntDef(ConteudoBloco, ConfigModoPagina.EspacoEntreLinhas);
  end

  else if ATag = cTagModoPaginaLargura then
  begin
    BlocoTraduzido := '';
    ConfigModoPagina.Largura := StrToIntDef(ConteudoBloco, ConfigModoPagina.Largura);
  end

  else if ATag = cTagModoPaginaPosiciona then
  begin
    BlocoTraduzido := ProcessarPosicionaXY(ConteudoBloco);
  end

  else if ATag = cTagBarraMostrar then
  begin
    BlocoTraduzido := '';
    ConfigBarras.MostrarCodigo := StrToBoolDef( ConteudoBloco, ConfigBarras.MostrarCodigo);
  end

  else if ATag = cTagBarraLargura then
  begin
    BlocoTraduzido := '';
    ConfigBarras.LarguraLinha := StrToIntDef( ConteudoBloco, ConfigBarras.LarguraLinha);
  end

  else if ATag = cTagBarraAltura then
  begin
    BlocoTraduzido := '';
    ConfigBarras.Altura := StrToIntDef( ConteudoBloco, ConfigBarras.Altura);
  end

  else if ATag = cTagLogoImprimir then
  begin
    BlocoTraduzido := '';
    ConfigLogo.IgnorarLogo := not StrToBoolDef( ConteudoBloco, not ConfigLogo.IgnorarLogo);
  end

  else if ATag = cTagLogoKC1 then
  begin
    BlocoTraduzido := '';
    ConfigLogo.KeyCode1 := StrToIntDef( ConteudoBloco, ConfigLogo.KeyCode1);
  end

  else if ATag = cTagLogoKC2 then
  begin
    BlocoTraduzido := '';
    ConfigLogo.KeyCode2 := StrToIntDef( ConteudoBloco, ConfigLogo.KeyCode2);
  end

  else if ATag = cTagLogoFatorX then
  begin
    BlocoTraduzido := '';
    ConfigLogo.FatorX := StrToIntDef( ConteudoBloco, ConfigLogo.FatorX);
  end

  else if ATag = cTagLogoFatorY then
  begin
    BlocoTraduzido := '';
    ConfigLogo.FatorY := StrToIntDef( ConteudoBloco, ConfigLogo.FatorY);
  end

  else if (AnsiIndexText(ATag, cTAGS_BARRAS) >= 0) then
  begin
    ACodBar := AjustarCodBarras(ConteudoBloco, ATag);
    BlocoTraduzido := FPosPrinterClass.ComandoCodBarras(ATag, ACodBar);
  end;

  GravarLog('TraduzirTagBloco(' + ATag + ', ' + ConteudoBloco + ') -> ' + BlocoTraduzido, True);
end;

procedure TACBrPosPrinter.AjustarEstadoFonteAlinhamento(const ATag: AnsiString);
begin
  if (ATag = cTagLigaExpandido) then
    FFonteStatus := FFonteStatus + [ftExpandido]
  else if (ATag = cTagDesligaExpandido) then
    FFonteStatus := FFonteStatus - [ftExpandido]
  else if (ATag = cTagLigaAlturaDupla) then
    FFonteStatus := FFonteStatus + [ftAlturaDupla]
  else if (ATag = cTagDesligaAlturaDupla) then
    FFonteStatus := FFonteStatus - [ftAlturaDupla]
  else if (ATag = cTagLigaNegrito) then
    FFonteStatus := FFonteStatus + [ftNegrito]
  else if (ATag = cTagDesligaNegrito) then
    FFonteStatus := FFonteStatus - [ftNegrito]
  else if (ATag = cTagLigaSublinhado) then
    FFonteStatus := FFonteStatus + [ftSublinhado]
  else if (ATag = cTagDesligaSublinhado) then
    FFonteStatus := FFonteStatus - [ftSublinhado]
  else if (ATag = cTagLigaCondensado) then
    FFonteStatus := FFonteStatus + [ftCondensado]
  else if (ATag = cTagDesligaCondensado) then
    FFonteStatus := FFonteStatus - [ftCondensado]
  else if (ATag = cTagLigaItalico) then
    FFonteStatus := FFonteStatus + [ftItalico]
  else if (ATag = cTagDesligaItalico) then
    FFonteStatus := FFonteStatus - [ftItalico]
  else if (ATag = cTagLigaInvertido) then
    FFonteStatus := FFonteStatus + [ftInvertido]
  else if (ATag = cTagDesligaInvertido) then
    FFonteStatus := FFonteStatus - [ftInvertido]
  else if (ATag = cTagFonteA) then
    FFonteStatus := FFonteStatus - [ftFonteB]
  else if (ATag = cTagFonteB) then
    FFonteStatus := FFonteStatus + [ftFonteB]
  else if (ATag = cTagFonteAlinhadaEsquerda) then
    FTipoAlinhamento := alEsquerda
  else if (ATag = cTagFonteAlinhadaDireita) then
    FTipoAlinhamento := alDireita
  else if (ATag = cTagfonteAlinhadaCentro) then
    FTipoAlinhamento := alCentro
  else if (ATag = cTagModoPaginaLiga) then
    FModoPaginaLigado := True
  else if (ATag = cTagModoPaginaDesliga) then
    FModoPaginaLigado := False
  else if (ATag = cTagFonteNormal) then
  begin
    FFonteStatus := FFonteStatus - [ftCondensado, ftExpandido, ftAlturaDupla,
                                    ftNegrito, ftSublinhado, ftItalico, ftInvertido,
                                    ftFonteB]
  end
  else if (ATag = cTagZera) then
  begin
    FInicializada := True;
    FFonteStatus := FFonteStatus - [ftCondensado, ftExpandido, ftAlturaDupla,
                                    ftNegrito, ftSublinhado, ftItalico, ftInvertido];
  end
  else if (ATag = cTagReset) then
  begin
    FInicializada := False;
    FFonteStatus := FFonteStatus - [ftCondensado, ftExpandido, ftAlturaDupla,
                                    ftNegrito, ftSublinhado, ftItalico, ftInvertido];
  end
end;

procedure TACBrPosPrinter.AtivarPorta;
begin
  if not FDevice.Ativo then
  begin
    GravarLog('Ativando a porta: ' + FDevice.Porta);
    FDevice.Ativar;
  end;
end;

procedure TACBrPosPrinter.DesativarPorta;
begin
  if FDevice.Ativo then
  begin
    GravarLog('Desativando a porta: ' + FDevice.Porta);

    FDevice.Desativar;

    if not FDevice.IsSerialPort then
      FInicializada := False;
  end;
end;

procedure TACBrPosPrinter.VerificaPodeLerDaPorta;
begin
  if not PodeLerDaPorta then
    raise EPosPrinterException.Create(ACBrStr('Leitura de Informações não disponível na porta: ')+
                                      GetEnumName(TypeInfo(TACBrDeviceType), Integer(FDevice.DeviceType)));
end;


{$IfDef MSWINDOWS}
procedure TACBrPosPrinter.DetectarECriarHook;
var
  uPorta, uMarca: String;
  P, i: Integer;
  HookClass: TACBrPosPrinterHookClass;
begin
  if (FDevice.DeviceType <> dtHook) then
  begin
    LiberarHook;
    Exit;
  end;

  uPorta := UpperCase(Porta);
  uMarca := '';
  P := pos(':',uPorta);
  if (P > 0) then
    uMarca := Trim(copy(uPorta, P+1, Length(uPorta)));

  if (uMarca = '') then
  begin
    for i := 0 to Length(HookList)-1 do
    begin
      if HookList[i].CanInitilize then
      begin
        uMarca := HookList[i].Brand;
        Break;
      end;
    end;

    if (uMarca = '') then
      raise EPosPrinterException.Create('Nenhuma biblioteca de modo USB encontrada');
  end;

  if Assigned(FHook) then
  begin
    if (uMarca <> FHook.Brand) then
      LiberarHook
    else
      Exit;   // Hook já existe, e é da mesma marca... tudo ok, caia fora...
  end;

  if not Assigned(FHook) then
  begin
    HookClass := Nil;

    for i := 0 to Length(HookList)-1 do
    begin
      if (HookList[i].Brand = uMarca) then
      begin
        HookClass := HookList[i];
        Break;
      end;
    end;

    if Assigned(HookClass) then
      FHook := HookClass.Create
    else
      raise EPosPrinterException.Create(ACBrStr('Marca '+uMarca+', não tem suporte em modo USB'));
  end;

  Modelo := TACBrPosPrinterModelo( FHook.PosPrinterModel );

  FHook.Init;
  FDevice.HookAtivar := PosPrinterHookAtivar;
  FDevice.HookDesativar := PosPrinterHookDesativar;
  FDevice.HookEnviaString := PosPrinterHookEnviaString;
  FDevice.HookLeString := PosPrinterHookLeString;
end;

procedure TACBrPosPrinter.LiberarHook;
begin
  if Assigned(FDevice) then
  begin
    FDevice.HookAtivar := Nil;
    FDevice.HookDesativar :=  Nil;
    FDevice.HookEnviaString := Nil;
    FDevice.HookLeString := Nil;
  end;

  if Assigned(FHook) then
    FreeAndNil(FHook);
end;

procedure TACBrPosPrinter.PosPrinterHookAtivar(const APort: String; Params: String);
begin
  if Assigned(FHook) then
    FHook.Open(APort);
end;

procedure TACBrPosPrinter.PosPrinterHookDesativar(const APort: String);
begin
  if Assigned(FHook) then
    FHook.Close;
end;

procedure TACBrPosPrinter.PosPrinterHookEnviaString(const cmd: AnsiString);
begin
  if Assigned(FHook) then
    FHook.WriteData(cmd);
end;

procedure TACBrPosPrinter.PosPrinterHookLeString(const NumBytes, ATimeOut: Integer;
  var Retorno: AnsiString);
begin
  Retorno := '';
  if Assigned(FHook) then
     Retorno := FHook.ReadData(NumBytes, ATimeOut);
end;
{$EndIf}

function TACBrPosPrinter.AguardarCheque(Operacao: TACBrPosOperacaoCheque): Boolean;
var
  AStatus: TACBrPosPrinterStatus;
  ProxLeitura: TDateTime;
begin
  Result := False;
  if not (PodeLerDaPorta and (TemCheque = 1)) then
    Exit;

  AStatus := [];
  FPosPrinterClass.LerStatus( AStatus );
  if not (stAguardandoSlip in AStatus) then   // Não está aguardando inserção de Cheques
    Exit;

  FCancelarEsperaCheque := False;
  while not (FCancelarEsperaCheque or (stTOF in AStatus)) do    // Aguada presença de sensor de Topo
  begin
    ProxLeitura := IncMilliSecond(Now, 500);
    if Assigned(FOnAguardarCheque) then
      FOnAguardarCheque(AStatus, Operacao, FCancelarEsperaCheque);

    if not FCancelarEsperaCheque then
    begin
      while (ProxLeitura > Now) do
      begin
        if Assigned(FOnAguardarCheque) then
          FOnAguardarCheque(AStatus, agcSleep, FCancelarEsperaCheque);
        Sleep( 50 );
      end;

      FPosPrinterClass.LerStatus( AStatus )
    end;
  end;

  Result := (not FCancelarEsperaCheque);
  if FCancelarEsperaCheque then
  begin
    if Assigned(FOnAguardarCheque) then
      FOnAguardarCheque(AStatus, agcCancelado, FCancelarEsperaCheque);

    EjetarCheque;
  end
  else
  begin
    if Assigned(FOnAguardarCheque) then
      FOnAguardarCheque(AStatus, agcConcluido, FCancelarEsperaCheque);
  end;

  FCancelarEsperaCheque := False ;
end;

procedure TACBrPosPrinter.EnviarStringDevice(AString: AnsiString);
var
  CmdInit: AnsiString;
  Tratado:boolean;
begin
  if AString = '' then
    exit;

  AtivarPorta;

  if not FInicializada then
  begin
    CmdInit := FPosPrinterClass.ComandoInicializa;
    FInicializada := (CmdInit = '') or (pos( CmdInit, AString ) > 0);

    if (not FInicializada) and (AString <> FPosPrinterClass.Cmd.Zera) then
    begin
      GravarLog('EnviarStringDevice - Inicializando: '+CmdInit, True);
      AString := CmdInit + AString;
      FInicializada := True;
    end;
  end;

  Tratado := False;
  if Assigned(FOnEnviarStringDevice) then
     FOnEnviarStringDevice(AString, Tratado);

  try
    if not Tratado then
    begin
      GravarLog('EnviarStringDevice(' + AString + ')', True);
      FDevice.EnviaString(AString);
    end
    else
      GravarLog('OnEnviarStringDevice(' + AString + ')', True);
  finally
    if ControlePorta then
      DesativarPorta;
  end;
end;

function TACBrPosPrinter.DecodificarTagsFormatacao(ABinaryString: AnsiString
  ): AnsiString;
begin
  FPosPrinterClass.AntesDecodificar( ABinaryString );
  Result := FTagProcessor.DecodificarTagsFormatacao(ABinaryString);
  FPosPrinterClass.DepoisDecodificar( Result );
end;

procedure TACBrPosPrinter.GravarLog(AString: AnsiString; Traduz: Boolean;
  AdicionaTempo: Boolean);
var
  Tratado: Boolean;
begin
  Tratado := False;

  if Traduz then
    AString := TranslateUnprintable(AString);

  if Assigned(FOnGravarLog) then
    FOnGravarLog(AString, Tratado);

  if (not Tratado) and (FArqLog <> '') then
  begin
    if AdicionaTempo then
      AString := '-- ' + FormatDateTime('dd/mm hh:nn:ss:zzz', now) + ' - ' + AString;

    WriteLog(FArqLog, AString);
  end;
end;

// Rotina inspirada em Código Fonte, gentilmente cedido pela Epson do Brasil,
procedure TACBrPosPrinter.ImprimirCheque(CodBanco: Integer; AValor: Double;
  ADataEmissao: TDateTime; const AFavorecido, ACidade, AComplemento: String;
  LerCMC7: Boolean; SegundosEspera: Integer);
var
  APosCheque: TACBrPosCheque;
  ValorFormatado, ValorExtenso, sDia, sMes, sAno: String;
  AACBrExtenso: TACBrExtenso;
  AACBrCMC7: TACBrCMC7;
  CMC7Banco: Integer;
  BufCmd: AnsiString;
begin
  GravarLog( 'ImprimirCheque( '+IntToStr(CodBanco) + ', '+ FloatToStr(AValor) + ', '+
             DateToStr(ADataEmissao)+', '+AFavorecido+', '+ACidade+', '+AComplemento+', '+
             IfThen(LerCMC7,'LerCMC7','NaoLerCMC7')+', '+IntToStr(SegundosEspera)+ ' )');

  if (TemCheque = 0) then
    raise EPosPrinterException.Create('Sem Impressora de Cheques');

  LerCMC7 := LerCMC7 and PodeLerDaPorta and (TemMICR = 1);

  Buffer.Clear;
  ImprimirCmd(FPosPrinterClass.ComandoCarregaCheque(LerCMC7, SegundosEspera));

  if LerCMC7 then  // Ok, se temos o CMC7, então podemos saber o Cod.Banco com certeza
  begin
    if not AguardarCheque(agcImpressao) then
      Exit;

    try
      // Lendo o CMC7 do Cheque, e achando CodBanco
      AACBrCMC7 := TACBrCMC7.Create(nil);
      try
        AACBrCMC7.CMC7 := LeituraCheque;
        CMC7Banco := StrToIntDef(AACBrCMC7.Banco, 0);
        if (CodBanco <> CMC7Banco) then
        begin
          GravarLog(Format('   Usando CodBanco %d, lido do CMC7',[CMC7Banco]));
          CodBanco := CMC7Banco;
        end;
      finally
        AACBrCMC7.Free;
      end;
    except
      { ignore erros }
    end;
  end;

  APosCheque := FPosCheques.FindPosCheque(CodBanco);
  if (APosCheque = Nil) then
    raise EPosPrinterException.Create('Erro ao ler coordenadas do Banco: '+IntToStr(CodBanco));

  if (CodBanco <> APosCheque.CodBanco) then
    GravarLog(Format('   Banco %d nao encontrado, usando Banco: %d',[CodBanco, APosCheque.CodBanco]));

  BufCmd := ComandoConfigurarModoPaginaCheque;

  ValorFormatado := '#'+FormatFloatBr(AValor)+'#';
  AACBrExtenso := TACBrExtenso.Create(nil);
  try
    AACBrExtenso.Valor := AValor;
    ValorExtenso := '#'+AACBrExtenso.Texto+'#';
  finally
    AACBrExtenso.Free;
  end;
  sDia := IntToStrZero(DayOf(ADataEmissao),2);
  sMes := cMesDescricao[MonthOf(ADataEmissao)];
  sAno := IntToStr(YearOf(ADataEmissao));

  BufCmd := BufCmd + ComandoTextoPosCheque(ValorFormatado, APosCheque.Valor, FPosCheques.Dimensao);
  BufCmd := BufCmd + ComandoTextoPosCheque(ValorExtenso, APosCheque.Extenso, FPosCheques.Dimensao);
  BufCmd := BufCmd + ComandoTextoPosCheque(AFavorecido, APosCheque.Favorecido, FPosCheques.Dimensao);
  BufCmd := BufCmd + ComandoTextoPosCheque(ACidade, APosCheque.Cidade, FPosCheques.Dimensao);
  BufCmd := BufCmd + ComandoTextoPosCheque(sDia, APosCheque.Dia, FPosCheques.Dimensao);
  BufCmd := BufCmd + ComandoTextoPosCheque(sMes, APosCheque.Mes, FPosCheques.Dimensao);
  BufCmd := BufCmd + ComandoTextoPosCheque(sAno, APosCheque.Ano, FPosCheques.Dimensao);
  BufCmd := BufCmd + ComandoTextoPosCheque(AComplemento, APosCheque.Complemento, FPosCheques.Dimensao);

  BufCmd := BufCmd + FPosPrinterClass.Cmd.ImprimePagina +
                     FPosPrinterClass.Cmd.DesligaModoPagina +
                     FPosPrinterClass.Cmd.PuloDePagina;
  ImprimirCmd(BufCmd);
end;

procedure TACBrPosPrinter.ImprimirTextoCheque(const X, Y: Integer;
  const AString: AnsiString; AguardaCheque: Boolean; SegundosEspera: Integer);
var
  BufCmd: AnsiString;
begin
  GravarLog( 'ImprimirTextoCheque( '+IntToStr(X)+', '+IntToStr(Y)+', '+AString+' )');

  if (TemCheque = 0) then
    raise EPosPrinterException.Create('Sem Impressora de Cheques');

  AguardaCheque := AguardaCheque and PodeLerDaPorta;

  Buffer.Clear;
  ImprimirCmd(FPosPrinterClass.ComandoCarregaCheque(False, SegundosEspera));
  if AguardaCheque then
    if not AguardarCheque(agcImpressao) then
      Exit;

  BufCmd := ComandoConfigurarModoPaginaCheque +
            ComandoTextoPosCheque(AString, Point(X,Y), FPosCheques.Dimensao) +
            FPosPrinterClass.Cmd.ImprimePagina +
            FPosPrinterClass.Cmd.DesligaModoPagina +
            FPosPrinterClass.Cmd.PuloDePagina;
  ImprimirCmd(BufCmd);
end;

procedure TACBrPosPrinter.EjetarCheque;
begin
  GravarLog('EjetarCheque');
  ImprimirCmd(FPosPrinterClass.ComandoEjetarCheque);
  FCancelarEsperaCheque := True;
end;

procedure TACBrPosPrinter.LerCMC7(AguardaCheque: Boolean;
  SegundosEspera: Integer);
begin
  GravarLog('LerCMC7');
  VerificaPodeLerDaPorta;
  if (TemMICR = 0) then
    raise EPosPrinterException.Create('Sem leitor MICR');

  ImprimirCmd(FPosPrinterClass.ComandoLerCMC7);

  if AguardaCheque then
    AguardarCheque(agcLerCMC7);
end;

procedure TACBrPosPrinter.RetornarTags(AStringList: TStrings;
  IncluiAjuda: Boolean);
begin
  FTagProcessor.RetornarTags(AStringList, IncluiAjuda);
end;

procedure TACBrPosPrinter.ImprimirTags;
begin
  GravarLog('ImprimirTags');
  FTagProcessor.RetornarTags(FBuffer);
  FBuffer.Insert(0,'</zera><c><ignorar_tags>');
  FBuffer.Add('</ignorar_tags>');
  FBuffer.Add('</corte_total>');

  Imprimir;
end;

function TACBrPosPrinter.TxRx(const ACmd: AnsiString; BytesToRead: Byte;
  ATimeOut: Integer; WaitForTerminator: Boolean): AnsiString;
var
  OldTimeOut, OldSendBytesInterval: Integer;
begin
  FDevice.Limpar;

  GravarLog('TX -> '+ACmd, True);
  if (ATimeOut = 0) then
  begin
    ATimeOut := FDevice.TimeOutMilissegundos;
    if FDevice.IsTCPPort then
      ATimeOut := max(ATimeOut, cTimeOutTxRxTCP);
  end;

  OldTimeOut := FDevice.TimeOutMilissegundos;
  OldSendBytesInterval := FDevice.SendBytesInterval;
  try
    FDevice.TimeOutMilissegundos := ATimeOut;

    if (Length(ACmd) > 0) then
    begin
      FDevice.SendBytesInterval := 0;
      FDevice.EnviaString( ACmd );
      Sleep(10);  // Aguarda equipamento ficar pronto para responder
    end;

    if WaitForTerminator then
      Result := FDevice.LeString(ATimeOut, 0, chr(BytesToRead))
    else
      Result := FDevice.LeString(ATimeOut, BytesToRead);
  finally
    FDevice.TimeOutMilissegundos := OldTimeOut;
    FDevice.SendBytesInterval := OldSendBytesInterval;
  end;

  GravarLog('RX <- '+Result, True);
end;

function TACBrPosPrinter.GetIgnorarTags: Boolean;
begin
  Result := FTagProcessor.IgnorarTags;
end;

function TACBrPosPrinter.GetPorta: String;
begin
  Result := FDevice.Porta;
end;

function TACBrPosPrinter.GetTagsNaoSuportadas: TStringList;
begin
  Result := FPosPrinterClass.TagsNaoSuportadas;
end;

function TACBrPosPrinter.GetTemAutenticacao: Integer;
begin
  if FTemAutenticacao < -1 then
    LerInfoDispositivos;

  Result := FTemAutenticacao;
end;

function TACBrPosPrinter.GetTemCheque: Integer;
begin
  if FTemCheque < -1 then
    LerInfoDispositivos;

  Result := FTemCheque;
end;

function TACBrPosPrinter.GetTemGuilhotina: Integer;
begin
  if FTemGuilhotina < -1 then
    LerInfoDispositivos;

  Result := FTemGuilhotina;
end;

function TACBrPosPrinter.GetTemMICR: Integer;
begin
  if FTemMICR < -1 then
    LerInfoDispositivos;

  Result := FTemMICR;
end;

function TACBrPosPrinter.LerStatusImpressora(Tentativas: Integer
  ): TACBrPosPrinterStatus;
var
  OldAtivo: Boolean;
  Falhas: Integer;
  tpStatus: TACBrPosTipoStatus;
  AStr: String;
begin
  Tentativas := max(Tentativas, 1);
  GravarLog('LerStatusImpressora( '+IntToStr(Tentativas)+' )');

  try
    if not PodeLerDaPorta then
    begin
      Result := [stApenasEscrita];
      Exit;
    end;

    OldAtivo := Ativo;
    try
      Ativar;
      AtivarPorta;

      Falhas := 0;
      while Falhas < Tentativas do
      begin
        Result := [];
        FPosPrinterClass.LerStatus( Result );

        if stErroLeitura in Result then
          Inc( Falhas )
        else
          Break;
      end;

      if ConfigGaveta.SinalInvertido then
      begin
        if (stGavetaAberta in Result) then
          Result := Result - [stGavetaAberta]
        else
          Result := Result + [stGavetaAberta];
      end;
    finally
      Ativo := OldAtivo;
      if ControlePorta then
        DesativarPorta;
    end;
  finally
    AStr := '';
    For tpStatus := Low(TACBrPosTipoStatus) to High(TACBrPosTipoStatus) do
    begin
      if tpStatus in Result then
        AStr := AStr + GetEnumName(TypeInfo(TACBrPosTipoStatus), integer(tpStatus) )+ ', ';
    end;

    if (AStr <> '') then
      GravarLog('   '+AStr);
  end;
end;

function TACBrPosPrinter.LerInfoImpressora: String;
var
  OldAtivo: Boolean;
begin
  GravarLog('LerInfoImpressora');
  Result := '';

  OldAtivo := Ativo;
  try
    VerificaPodeLerDaPorta;
    Ativar;
    AtivarPorta;

    Result := FPosPrinterClass.LerInfo;
    if (Result <> '') then
      GravarLog('   '+Result, True);
  finally
    Ativo := OldAtivo;
    if ControlePorta then
      DesativarPorta;
  end;
end;

procedure TACBrPosPrinter.ImprimirImagemStream(ABMPStream: TStream);
begin
  GravarLog('ImprimirImagemStream( '+IntToStr(ABMPStream.Size)+' bytes )');
  ImprimirCmd(FPosPrinterClass.ComandoImprimirImagemStream(ABMPStream));
end;

procedure TACBrPosPrinter.ImprimirImagemArquivo(ArquivoBMP: String);
begin
  if ValidaLogoBmp(ArquivoBMP) then
  begin
    GravarLog('ImprimirImagemArquivo( '+ArquivoBMP+' )');
    ImprimirCmd(FPosPrinterClass.ComandoImprimirImagemArquivo(ArquivoBMP))
  end
  else
    GravarLog('ImprimirImagemArquivo( '+ArquivoBMP+' ) é inválido' );
end;

procedure TACBrPosPrinter.ImprimirImagemRasterStr(const ARasterStr: AnsiString; AWidth,
  AHeight: Integer);
begin
  GravarLog('ImprimirImagemRasterStr( '+IntToStr(Length(ARasterStr))+' bytes, '+
            IntToStr(AWidth)+', '+IntToStr(AHeight)+' )');
  ImprimirCmd(FPosPrinterClass.ComandoImprimirImagemRasterStr(ARasterStr, AWidth, AHeight));
end;

procedure TACBrPosPrinter.ImprimirLogo(AKC1: Integer; AKC2: Integer;
  AFatorX: Integer; AFatorY: Integer);
begin
  GravarLog('ImprimirLogo( '+IntToStr(AKC1)+', '+IntToStr(AKC2)+', '+
    IntToStr(AFatorX)+', '+IntToStr(AFatorY)+' )');

  VerificarParametrosLogo(AKC2, AKC1);

  if AFatorX >= 0 then
    FConfigLogo.FatorX := AFatorX;

  if AFatorY >= 0 then
    FConfigLogo.FatorY := AFatorY;

  ImprimirCmd(FPosPrinterClass.ComandoLogo)
end;

procedure TACBrPosPrinter.GravarLogoStream(ABMPStream: TStream; AKC1: Integer;
  AKC2: Integer);
begin
  GravarLog('GravarLogoStream( '+IntToStr(ABMPStream.Size)+' bytes, '+IntToStr(AKC1)+', '+IntToStr(AKC2)+' )');
  VerificarParametrosLogo(AKC2, AKC1);
  ImprimirCmd(FPosPrinterClass.ComandoGravarLogoStream(ABMPStream));
end;

procedure TACBrPosPrinter.GravarLogoArquivo(ArquivoBMP: String; AKC1: Integer;
  AKC2: Integer);
begin
  GravarLog('GravarLogoArquivo( '+ArquivoBMP+', '+IntToStr(AKC1)+', '+IntToStr(AKC2)+' )');
  VerificarParametrosLogo(AKC2, AKC1);
  ImprimirCmd(FPosPrinterClass.ComandoGravarLogoArquivo(ArquivoBMP))
end;

procedure TACBrPosPrinter.ApagarLogo(AKC1: Integer; AKC2: Integer);
begin
  GravarLog('ApagarLogo( '+IntToStr(AKC1)+', '+IntToStr(AKC2)+' )');
  VerificarParametrosLogo(AKC2, AKC1);
  ImprimirCmd(FPosPrinterClass.ComandoApagarLogo)
end;

function TACBrPosPrinter.CalcularAlturaTexto(ALinhas: Integer): Integer;
var
  EspLin: Byte;
begin
  EspLin := FEspacoEntreLinhas;
  if EspLin = 0 then
    EspLin := 60;

  Result := (EspLin+2) * ALinhas;
end;

function TACBrPosPrinter.CalcularLinhasAltura(AAltura: Integer): Integer;
begin
  Result := round(AAltura / (FEspacoEntreLinhas+2));
end;

function TACBrPosPrinter.CalcularAlturaQRCodeAlfaNumM(const QRCodeData: String
  ): Integer;
var
  QRCodeModules: Integer;
  LenData: Integer;
begin
  // http://www.qrcode.com/en/about/version.html
  LenData := Length(QRCodeData);

  if LenData < 20 then
    QRCodeModules := 21
  else if LenData < 38 then
    QRCodeModules := 25
  else if LenData < 61 then
    QRCodeModules := 29
  else if LenData < 90 then
    QRCodeModules := 33
  else if LenData < 122 then
    QRCodeModules := 37
  else if LenData < 154 then
    QRCodeModules := 41
  else if LenData < 178 then
    QRCodeModules := 45
  else if LenData < 221 then
    QRCodeModules := 49
  else if LenData < 262 then
    QRCodeModules := 53
  else if LenData < 311 then
    QRCodeModules := 57
  else if LenData < 366 then
    QRCodeModules := 61
  else if LenData < 419 then
    QRCodeModules := 65
  else if LenData < 483 then
    QRCodeModules := 69
  else if LenData < 528 then
    QRCodeModules := 73
  else if LenData < 600 then
    QRCodeModules := 77
  else
    raise EPosPrinterException.Create('QRCode muito grande');

  // http://www.qrcode.com/en/howto/code.html
  Result := (QRCodeModules + 10) * CDotsMM;
end;

function TACBrPosPrinter.AjustarCodBarras(const ABarCode, ABarCodeTag: AnsiString): AnsiString;
var
  ACodBar: AnsiString;
begin
  // Ajustando os Códigos de Barras, conforme regras do Tipo do Código //
  if (ABarCodeTag = cTagBarraUPCA) then
    // Apenas números, sempre 11 digitos, e 1 digito verificador
    ACodBar := PadLeftA(OnlyNumber(ABarCode), 11, '0')

  else if (ABarCodeTag = cTagBarraUPCE) then
    // EPC-A compactado, Apenas números, 6 ou 11 dígitos
    ACodBar := AnsiString(OnlyNumber(ABarCode))

  else if ABarCodeTag = cTagBarraEAN13 then
    // Apenas números, sempre 12 digitos, e 1 digito verificador
    ACodBar := PadLeftA(OnlyNumber(ABarCode), 12, '0')

  else if ABarCodeTag = cTagBarraEAN8 then
    // Apenas números, sempre 7 digitos, e 1 digito verificador
    ACodBar := PadLeftA(OnlyNumber(ABarCode), 7, '0')

  else if ABarCodeTag = cTagBarraCode128c then
    // Apenas números,
    ACodBar := AnsiString(OnlyNumber(ABarCode))

  else if ABarCodeTag = cTagBarraCode39 then
    // Qualquer tamanho.. Aceita: 0~9, A~Z, ' ', '$', '%', '*', '+', '-', '.', '/'
    ACodBar := AnsiString(OnlyCharsInSet(ABarCode,
      ['0'..'9', 'A'..'Z', ' ', '$', '%', '*', '+', '-', '.', '/']))

  else if ABarCodeTag = cTagBarraCode93 then
    // Qualquer tamanho.. Aceita: #0~#127
    ACodBar := AnsiString(OnlyCharsInSet(ABarCode, [#0..#127]))

  else if ABarCodeTag = cTagBarraInter then
  begin
    // Interleaved 2of5. Somente números, Tamanho deve ser PAR
    ACodBar := AnsiString(OnlyNumber(ABarCode));

    if (Length(ACodBar) mod 2) <> 0 then  // Tamanho é Par ?
      ACodBar := '0' + ACodBar;
  end

  else if ABarCodeTag = cTagBarraStd then
    // Apenas números, Sem dígito verificador
    ACodBar := AnsiString(OnlyNumber(ABarCode))

  else if ABarCodeTag = cTagBarraCodaBar then
    // Qualquer tamanho.. Aceita: 0~9, A~D, a~d, $, +, -, ., /, :
    ACodBar := AnsiString(OnlyCharsInSet(ABarCode,
      ['0'..'9', 'A'..'D', 'a'..'d', '$', '+', '-', '.', '/', ':']))

  else if ABarCodeTag = cTagBarraCode11 then
    // Apenas números, Qualquer tamanho, dois dígitos verificador
    ACodBar := AnsiString(OnlyNumber(ABarCode))

  else if ABarCodeTag = cTagBarraMSI then
    // Apenas números, 1 dígito verificador
    ACodBar := AnsiString(OnlyNumber(ABarCode))

  else
    ACodBar := ABarCode;

  Result := AnsiString(LeftStr(ACodBar, 255));  // Tamanho máximo para Cod.Barras é 255 caracteres
end;

function TACBrPosPrinter.ConfigurarRegiaoModoPagina(AEsquerda, ATopo, AAltura,
  ALargura: Integer; ADirecao: TACBrPosDirecao): String;

  Function MontarTag(const ATag, AConteudo: String): String;
  begin
     Result := ATag + AConteudo + StringReplace(ATag, '<', '</', [rfReplaceAll]);
  end;

begin
  Result := MontarTag( cTagModoPaginaDirecao, IntToStr(Integer(ADirecao)) ) +
            MontarTag( cTagModoPaginaPosEsquerda, IntToStr(AEsquerda) ) +
            MontarTag( cTagModoPaginaPosTopo, IntToStr(ATopo) ) +
            MontarTag( cTagModoPaginaAltura, IntToStr(AAltura) ) +
            MontarTag( cTagModoPaginaLargura, IntToStr(ALargura) ) +
            MontarTag( cTagModoPaginaEspaco, IntToStr(EspacoEntreLinhas) ) +
            cTagModoPaginaConfigurar;
end;

function TACBrPosPrinter.GetTraduzirTags: Boolean;
begin
  Result := FTagProcessor.TraduzirTags;
end;

function TACBrPosPrinter.PodeLerDaPorta: Boolean;
begin
   Result := (FDevice.DeviceType in [dtNenhum, dtSerial, dtTCP, dtHook, dtUSB, dtBlueTooth] )
end;

procedure TACBrPosPrinter.SetAtivo(AValue: Boolean);
begin
  if (AValue = FAtivo) then
    Exit;

  if AValue then
    Ativar
  else
    Desativar;
end;

procedure TACBrPosPrinter.SetIgnorarTags(AValue: Boolean);
begin
  FTagProcessor.IgnorarTags := AValue;
end;

procedure TACBrPosPrinter.SetPorta(const AValue: String);
begin
  FDevice.Porta := AValue;
end;

procedure TACBrPosPrinter.SetTraduzirTags(AValue: Boolean);
begin
  FTagProcessor.TraduzirTags := AValue;
end;

procedure TACBrPosPrinter.Imprimir(const AString: AnsiString; PulaLinha: Boolean;
  DecodificarTags: Boolean; CodificarPagina: Boolean; Copias: Integer);
var
  i: Integer;
  StrToPrint: AnsiString;
  PrnStatus: TACBrPosPrinterStatus;
  MsgErro: String;
begin
  try
    if not (ControlePorta or Ativo) then
      raise EPosPrinterException.Create(ACBrStr('Não está Ativo'));

    if not Ativo then
      Ativar;

    if VerificarImpressora then
    begin
      MsgErro := '';
      PrnStatus := LerStatusImpressora;

      if stTampaAberta in PrnStatus then
        MsgErro := 'com Tampa Aberta'
      else if stSemPapel in PrnStatus then
        MsgErro := 'Sem Papel'
      else if stOffLine in PrnStatus then
        MsgErro := 'Desligada'
      else if stErro in PrnStatus then
        MsgErro := 'em Erro';

      if (MsgErro <> '') then
        raise EPosPrinterException.Create('Impressora '+MsgErro);
    end;

    StrToPrint := '';
    if (FBuffer.Count > 0) then
    begin
      For i := 0 to FBuffer.Count-1 do
      begin
        StrToPrint := StrToPrint + FBuffer[i];
        if (i < FBuffer.Count-1) then
          StrToPrint := StrToPrint + FPosPrinterClass.Cmd.PuloDeLinha;
      end;
    end;
  finally
    FBuffer.Clear;
  end;

  StrToPrint := StrToPrint + AString;

  GravarLog('Imprimir, Copias:' + IntToStr(Copias)+
            ', DecodificarTags:'+IfThen(DecodificarTags,'SIM','NAO')+
            ', TraduzirTags:'+IfThen(TraduzirTags,'SIM','NAO') );
  GravarLog( TranslateUnprintable(StrToPrint) );

  if CodificarPagina then
    StrToPrint := CodificarPaginaDeCodigo(StrToPrint);

  //DEBUG
  //WriteLog('c:\temp\teste2.txt', StrToPrint, True);

  // Ajustando a Quebra de Linha, para a usada pela Impressora
  if (FPosPrinterClass.Cmd.PuloDeLinha <> CRLF) then
    StrToPrint := ReplaceString(StrToPrint, CRLF, FPosPrinterClass.Cmd.PuloDeLinha);
  if (FPosPrinterClass.Cmd.PuloDeLinha <> CR) then
    StrToPrint := ReplaceString(StrToPrint, CR, FPosPrinterClass.Cmd.PuloDeLinha);
  if (FPosPrinterClass.Cmd.PuloDeLinha <> LF) then
    StrToPrint := ReplaceString(StrToPrint, LF, FPosPrinterClass.Cmd.PuloDeLinha);

  if PulaLinha then
    StrToPrint := StrToPrint + FPosPrinterClass.Cmd.PuloDeLinha;

  if DecodificarTags then
    StrToPrint := DecodificarTagsFormatacao(StrToPrint);

  //DEBUG
  //WriteLog('c:\temp\teste3.txt', StrToPrint, True);

  For i := 1 to Copias do
    EnviarStringDevice(StrToPrint);
end;

procedure TACBrPosPrinter.ImprimirLinha(const AString: AnsiString);
begin
  Imprimir(AString, True);
end;

procedure TACBrPosPrinter.ImprimirCmd(const AString: AnsiString);
begin
  if FBuffer.Count > 0 then
    Imprimir;

  if AString = '' then
    exit;

  EnviarStringDevice(AString);
end;

procedure TACBrPosPrinter.Zerar;
begin
  GravarLog('Zerar');
  ImprimirCmd(FPosPrinterClass.Cmd.Zera);

  FInicializada := False;
  FFonteStatus := FFonteStatus - [ftCondensado, ftExpandido, ftAlturaDupla,
      ftNegrito, ftSublinhado, ftItalico, ftInvertido, ftFonteB];

  Inicializar;
end;

procedure TACBrPosPrinter.Inicializar;
begin
  GravarLog('Inicializar');
  ImprimirCmd(FPosPrinterClass.ComandoInicializa);

  FInicializada := True;
end;

procedure TACBrPosPrinter.Reset;
begin
  GravarLog('Reset');
  ImprimirCmd(FPosPrinterClass.Cmd.Zera);

  FInicializada := False;
  FFonteStatus := FFonteStatus - [ftCondensado, ftExpandido, ftAlturaDupla,
      ftNegrito, ftSublinhado, ftItalico, ftInvertido];
end;

function TACBrPosPrinter.GetNumeroPaginaDeCodigo(APagCod: TACBrPosPaginaCodigo): word;
begin
  case APagCod of
    pc437: Result := 437;
    pc850: Result := 850;
    pc852: Result := 852;
    pc860: Result := 860;
    pc1252: Result := 1252;
    pcUTF8: Result := 65001;
    else
      Result := 0;
  end;
end;

function TACBrPosPrinter.CodificarPaginaDeCodigo(const ATexto: AnsiString
  ): AnsiString;
var
  NumPagCod: word;
begin
  NumPagCod := GetNumeroPaginaDeCodigo(FPaginaDeCodigo);
  //GravarLog('CodificarPaginaDeCodigo: '+IntToStr(NumPagCod) );

  if NumPagCod > 0 then
  begin
    {$IfDef MSWINDOWS}
    Result := TranslateString(ACBrStrToAnsi(ATexto), NumPagCod)
    {$Else}
    Result := TranslateString(ATexto, NumPagCod)
    {$EndIf}
  end
  else
    Result := TiraAcentos(ATexto);
end;

procedure TACBrPosPrinter.PularLinhas(NumLinhas: Integer);
begin
  GravarLog('PularLinhas(' + IntToStr(NumLinhas) + ')');

  if NumLinhas = 0 then
    NumLinhas := LinhasEntreCupons;

  ImprimirCmd( FPosPrinterClass.ComandoPuloLinhas(NumLinhas) );
end;

procedure TACBrPosPrinter.CortarPapel(Parcial: Boolean);
begin
  GravarLog('CortarPapel(' + IfThen(Parcial, 'Parcial', 'Total') + ')');

  if Parcial then
    ImprimirCmd(FPosPrinterClass.Cmd.CorteParcial)
  else
    ImprimirCmd(FPosPrinterClass.Cmd.CorteTotal);

  Sleep(500);
end;

end.

