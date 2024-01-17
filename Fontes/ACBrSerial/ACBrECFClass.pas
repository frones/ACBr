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

Unit ACBrECFClass ;

interface
uses
  SysUtils, Classes,
  ACBrDevice, ACBrConsts, ACBrBase
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   ,System.Generics.Collections, System.Generics.Defaults
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   ,System.Contnrs
  {$Else}
   ,Contnrs
  {$IfEnd}
  {$IFNDEF NOGUI}
   {$IFDEF VisualCLX},
     {$IFDEF QT3CLX} QtLibrary, QtSignalHooks {$ELSE} Qt {$ENDIF},
      QControls, QForms, QGraphics, QDialogs, QExtCtrls
   {$ENDIF}
   {$IF DEFINED(FMX)}
      , FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ExtCtrls
      , System.UITypes, System.Character, System.Types, FMX.Types, FMX.TextLayout, FMX.Objects, System.UIConsts
   {$ELSEIF DEFINED(VCL)}
      , Controls, Forms, Graphics, Dialogs, ExtCtrls
      {$IFDEF DELPHIXE2_UP}
       , System.UITypes
      {$ENDIF}
   {$IFEND}
   {$IFDEF MSWINDOWS}
     , Windows, messages
   {$ENDIF}
  {$ENDIF} ;

type

  EACBrECFErro            = class(Exception) ;
  EACBrECFCMDInvalido     = class(EACBrECFErro) ;
  EACBrECFSemResposta     = class(EACBrECFErro) ;
  EACBrECFSemPapel        = class(EACBrECFErro) ;
  EACBrECFTimeOut         = class(EACBrECFErro) ;
  EACBrECFNaoInicializado = class(EACBrECFErro) ;
  EACBrECFOcupado         = class(EACBrECFErro) ;

{ Definindo novo tipo para armazenar os dados que irão compor o rodapé }

{ TACBrECFRodape }

  { TACBRAbastecimento }

  TACBRRodapeAbastecimento = class( TPersistent )
  private
    FEI: Double;
    FBico: Integer;
    FVolume: Double;
    FEF: Double;
    FAutomatico: Boolean;
    FManual: Boolean;
  public
    property Bico: Integer read FBico write FBico;
    property EI: Double read FEI write FEI;
    property EF: Double read FEF write FEF;
    property Volume: Double read FVolume write FVolume;
    property Automatico: Boolean read FAutomatico write FAutomatico;
    property Manual: Boolean read FManual write FManual;
  end;

  { TACBRAbastecimentos }

  TACBRRodapeAbastecimentos = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBRRodapeAbastecimento>{$EndIf})
  private
    fsImprimir: Boolean;
    procedure SetObject(Index: Integer; Item: TACBRRodapeAbastecimento);
    function GetObject(Index: Integer): TACBRRodapeAbastecimento;
  public
     property Imprimir: Boolean read fsImprimir write fsImprimir;
    function New: TACBRRodapeAbastecimento;
    function Add(Obj: TACBRRodapeAbastecimento): Integer;
    procedure Insert(Index: Integer; Obj: TACBRRodapeAbastecimento);
    property Objects[Index: Integer]: TACBRRodapeAbastecimento read GetObject write SetObject; default;
  end;

TACBrECFRodapeImposto = class( TPersistent )
  private
    fsValorAproximado: Double;
    fsValorAproximadoFederal: Double;
    fsValorAproximadoEstadual: Double;
    fsValorAproximadoMunicipal: Double;
    fsFonte: String;
    fsTexto: String;
    FModoCompacto: Boolean;
    fsChave: String;
  published
    property Texto: String read fsTexto write fsTexto;
    property ValorAproximado: Double read fsValorAproximado write fsValorAproximado stored false;
    property ValorAproximadoFederal: Double read fsValorAproximadoFederal write fsValorAproximadoFederal stored False;
    property ValorAproximadoEstadual: Double read fsValorAproximadoEstadual write fsValorAproximadoEstadual stored False;
    property ValorAproximadoMunicipal: Double read fsValorAproximadoMunicipal write fsValorAproximadoMunicipal stored False;
    property ModoCompacto: Boolean read FModoCompacto write FModoCompacto;
    property Fonte: String read fsFonte write fsFonte stored false;
    property Chave: String read fsChave write fsChave;
end;

TACBrECFRodapeNotaLegalDF = class( TPersistent )
  private
    fsProgramaDeCredito: Boolean;
    fsValorICMS: Double;
    fsValorISS: Double;
    fsImprimir: Boolean;
  public
  published
    property Imprimir: Boolean read fsImprimir write fsImprimir default False;
    property ProgramaDeCredito: Boolean read fsProgramaDeCredito
       write fsProgramaDeCredito default False;
    property ValorICMS: Double read fsValorICMS write fsValorICMS stored false;
    property ValorISS : Double read fsValorISS  write fsValorISS  stored false;
end;

TACBrECFRodapeRestaurante = class( TPersistent )
  private
    fsECF : Integer;
    fsCER:  Integer;
    fsCOO:  Integer;
    fsMesa: String;
    fsImprimir: Boolean;
    fsContaCliente: Boolean;
  public
  published
    property Imprimir : Boolean read fsImprimir write fsImprimir default False;
    property ECF      : Integer read fsECF      write fsECF stored false;
    property CER      : Integer read fsCER      write fsCER stored false;
    property COO      : Integer read fsCOO      write fsCOO stored false;
    property Mesa     : String  read fsMesa     write fsMesa stored false;
    property ContaCliente: Boolean read fsContaCliente write fsContaCliente
       stored false default False;
end;

TACBrECFRodape = class( TPersistent )
  private
    fsPreVenda: String;
    fsDavOs: String;
    fsMD5: String;
    fsDav: String;
    fsDavFarm: String;
    fsNF: String;
    fsPlaca: String;
    fsQtdeKM: String;
    fsRestaurante: TACBrECFRodapeRestaurante;
    fsMinasLegal: Boolean;
    fsCupomMania: Boolean;
    fsNotaLegalDF: TACBrECFRodapeNotaLegalDF;
    fsParaibaLegal: Boolean;
    fsImposto: TACBrECFRodapeImposto;
    fsPostoComustivel: TACBRRodapeAbastecimentos;
    procedure SetMD5(AValue : String) ;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
  published
    property MD5         : String  read fsMD5         write SetMD5;
    property Dav         : String  read fsDav         write fsDav   stored False;
    property DavFarm     : String  read fsDavFarm     write fsDavFarm stored False;
    property NF          : String  read fsNF          write fsNF stored False;
    property DavOs       : String  read fsDavOs       write fsDavOs stored False;
    property PreVenda    : String  read fsPreVenda    write fsPreVenda stored False;
    property Placa       : String  read fsPlaca       write fsPlaca stored false;
    property QtdeKM      : String  read fsQtdeKM      write fsQtdeKM stored false;
    property Restaurante : TACBrECFRodapeRestaurante read fsRestaurante write fsRestaurante;
    property CupomMania  : Boolean read fsCupomMania  write fsCupomMania default False;
    property MinasLegal  : Boolean read fsMinasLegal  write fsMinasLegal default False;
    property ParaibaLegal: Boolean read fsParaibaLegal write fsParaibaLegal default False;
    property NotaLegalDF : TACBrECFRodapeNotaLegalDF read fsNotaLegalDF write fsNotaLegalDF;
    property Imposto     : TACBrECFRodapeImposto read fsImposto write fsImposto;
    property PostoCombustivel: TACBRRodapeAbastecimentos read fsPostoComustivel write fsPostoComustivel;
end;

{ Definindo novo tipo para armazenar Aliquota de ICMS }

{ TACBrECFAliquota }

TACBrECFAliquota = class
 private
    fsSequencia: Byte;
    fsIndice: String;
    fsAliquota: Double ;
    fsTipo: Char;
    fsTotal: Double;
    function GetAsString: String;
    procedure SetAsString(const AValue: String);
    procedure SetTipo(const AValue: Char);
 public
    constructor create ;
    procedure Assign( AAliquota : TACBrECFAliquota ) ;

    property Sequencia : Byte   read fsSequencia write fsSequencia ;
    property Indice    : String read fsIndice    write fsIndice ;
    property Aliquota  : Double read fsAliquota  write fsAliquota ;
    property Tipo      : Char read fsTipo write SetTipo ;
    property Total     : Double read fsTotal write fsTotal ;

    property AsString : String read GetAsString write SetAsString;
end;

{ Lista de Objetos do tipo TACBrECFAliquota }

TACBrECFAliquotas = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrECFAliquota>{$EndIf})
  protected
    procedure SetObject (Index: Integer; Item: TACBrECFAliquota);
    function GetObject (Index: Integer): TACBrECFAliquota;
    procedure Insert (Index: Integer; Obj: TACBrECFAliquota);
  public
    function New: TACBrECFAliquota;
    function Add (Obj: TACBrECFAliquota): Integer;
    property Objects [Index: Integer]: TACBrECFAliquota
      read GetObject write SetObject; default;
  end;

{ TACBrECFTotalizadorNaoTributado }

TACBrECFTotalizadorNaoTributado = class
 private
   fsIndice: String;
   fsTipo: Char;
   fsTotal: Double;
   function GetAsString: String;
   procedure SetAsString(const AValue: String);
   procedure SetTipo(const AValue: Char);
 public
   constructor create ;
   procedure Assign(ATotalizadorNaoTributado: TACBrECFTotalizadorNaoTributado);

   property Indice : String read fsIndice write fsIndice ;
   property Tipo   : Char   read fsTipo   write SetTipo ;
   property Total  : Double read fsTotal  write fsTotal ;

   property AsString : String read GetAsString write SetAsString;
end;

{ Lista de Objetos do tipo TACBrECFAliquota }

{ TACBrECFTotalizadoresNaoTributados }

TACBrECFTotalizadoresNaoTributados = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrECFTotalizadorNaoTributado>{$EndIf})
  protected
    procedure SetObject (Index: Integer; Item: TACBrECFTotalizadorNaoTributado);
    function GetObject (Index: Integer): TACBrECFTotalizadorNaoTributado;
    procedure Insert (Index: Integer; Obj: TACBrECFTotalizadorNaoTributado);
  public
    function New: TACBrECFTotalizadorNaoTributado;
    function Add (Obj: TACBrECFTotalizadorNaoTributado): Integer;
    property Objects [Index: Integer]: TACBrECFTotalizadorNaoTributado
      read GetObject write SetObject; default;
  end;

{ Definindo novo tipo para armazenar as Formas de Pagamento }

{ TACBrECFFormaPagamento }

TACBrECFFormaPagamento = class
 private
    fsIndice: String;
    fsDescricao: String;
    fsPermiteVinculado: Boolean;
    fsTotal: Double;
    fsData: TDateTime;
    fsTipoDoc: String;
    function GetAsString: String;
    procedure SetAsString(const AValue: String);
 public
    constructor create ;
    procedure Assign( AFormaPagamento : TACBrECFFormaPagamento ) ;

    property Indice    : String read fsIndice    write fsIndice ;
    property Descricao : String read fsDescricao write fsDescricao ;
    property PermiteVinculado : Boolean read fsPermiteVinculado
                                       write fsPermiteVinculado ;
    property Total : Double read fsTotal write fsTotal ;
    property Data: TDateTime read fsData write fsData;
    property TipoDoc: String read fsTipoDoc write fsTipoDoc;

    property AsString : String read GetAsString write SetAsString;
end;

{ Lista de Objetos do tipo TACBrECFFormaPagamento }
TACBrECFFormasPagamento = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrECFFormaPagamento>{$EndIf})
  protected
    procedure SetObject (Index: Integer; Item: TACBrECFFormaPagamento);
    function GetObject (Index: Integer): TACBrECFFormaPagamento;
  public
    procedure Ordenar;
    function New: TACBrECFFormaPagamento;
    function Add (Obj: TACBrECFFormaPagamento): Integer;
    procedure Insert (Index: Integer; Obj: TACBrECFFormaPagamento);
    property Objects [Index: Integer]: TACBrECFFormaPagamento
      read GetObject write SetObject; default;
  end;

{ Definindo novo tipo para armazenar as unidades de Medida }
TACBrECFUnidadeMedida = class
 private
    fsIndice: String;
    fsDescricao: String;
 public
    constructor create ;
    property Indice    : String read fsIndice    write fsIndice ;
    property Descricao : String read fsDescricao write fsDescricao ;
end;

{ Lista de Objetos do tipo TACBrECFunidadeMedida }
TACBrECFUnidadesMedida = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrECFUnidadeMedida>{$EndIf})
  protected
    procedure SetObject (Index: Integer; Item: TACBrECFUnidadeMedida);
    function GetObject (Index: Integer): TACBrECFUnidadeMedida;
  public
    function Add (Obj: TACBrECFUnidadeMedida): Integer;
    procedure Insert (Index: Integer; Obj: TACBrECFUnidadeMedida);
    property Objects [Index: Integer]: TACBrECFUnidadeMedida
      read GetObject write SetObject; default;
  end;

{ Definindo novo tipo para armazenar os Reletórios Gerenciais (RG) }

{ TACBrECFRelatorioGerencial }

TACBrECFRelatorioGerencial = class
 private
    fsIndice: String;
    fsDescricao: String;
    fsContador: Integer;
    function GetAsString: String;
    procedure SetAsString(const AValue: String);
 public
    constructor create ;
    procedure Assign( ARelatorioGerencial : TACBrECFRelatorioGerencial ) ;

    property Indice    : String read fsIndice    write fsIndice ;
    property Descricao : String read fsDescricao write fsDescricao ;
    property Contador : Integer read fsContador write fsContador;

    property AsString : String read GetAsString write SetAsString;
 end;

{ Lista de Objetos do tipo TACBrECFRelatoriosGerencial }
TACBrECFRelatoriosGerenciais = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrECFRelatorioGerencial>{$EndIf})
  protected
    procedure SetObject (Index: Integer; Item: TACBrECFRelatorioGerencial);
    function GetObject (Index: Integer): TACBrECFRelatorioGerencial;
  public
    function Add (Obj: TACBrECFRelatorioGerencial): Integer;
    procedure Insert (Index: Integer; Obj: TACBrECFRelatorioGerencial);
    property Objects [Index: Integer]: TACBrECFRelatorioGerencial
      read GetObject write SetObject; default;
  end;

{ Definindo novo tipo para armazenar os Comprovantes NAO Fiscais (CNF) }

{ TACBrECFComprovanteNaoFiscal }

TACBrECFComprovanteNaoFiscal = class
 private
    fsIndice: String;
    fsDescricao: String;
    fsPermiteVinculado: Boolean;
    fsFormaPagamento: String;
    fsTotal: Double ;
    fsContador: Integer;
    function GetAsString: String;
    procedure SetAsString(const AValue: String);
 public
    constructor create ;
    procedure Assign( AComprovanteNaoFiscal : TACBrECFComprovanteNaoFiscal ) ;

    property Indice    : String read fsIndice    write fsIndice ;
    property Descricao : String read fsDescricao write fsDescricao ;
    property PermiteVinculado : Boolean read fsPermiteVinculado
                                       write fsPermiteVinculado ;
    property FormaPagamento : String read fsFormaPagamento
                                    write fsFormaPagamento ;
    property Total    : Double  read fsTotal    write fsTotal ;
    property Contador : Integer read fsContador write fsContador;

    property AsString : String read GetAsString write SetAsString;
 end;


{ Definindo novo tipo para armazenar o Consumidor que será impresso no CF }
TACBrECFConsumidor = class
 private
    fsNome      : String;
    fsEndereco  : String;
    fsDocumento : String;
    fsEnviado   : Boolean;
    function GetEnviado: Boolean;
    function GetAtribuido: Boolean;
 public
    constructor create ;
    property Enviado   : Boolean read GetEnviado write fsEnviado ;
    property Nome      : String  read fsNome ;
    property Endereco  : String  read fsEndereco ;
    property Documento : String  read fsDocumento  ;
    property Atribuido : Boolean read GetAtribuido ;

    procedure AtribuiConsumidor(CPF_CNPJ, Nome, Endereco: String);
    procedure Zera;
end ;

{ Lista de Objetos do tipo TACBrECFComprovanteNaoFiscal }
TACBrECFComprovantesNaoFiscais = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrECFComprovanteNaoFiscal>{$EndIf})
  protected
    procedure SetObject (Index: Integer; Item: TACBrECFComprovanteNaoFiscal);
    function GetObject (Index: Integer): TACBrECFComprovanteNaoFiscal;
  public
    function Add (Obj: TACBrECFComprovanteNaoFiscal): Integer;
    procedure Insert (Index: Integer; Obj: TACBrECFComprovanteNaoFiscal);
    property Objects [Index: Integer]: TACBrECFComprovanteNaoFiscal
      read GetObject write SetObject; default;
  end;

{ Dados da atual ou última redução Z }

{ TACBrECFDadosRZ }

TACBrECFDadosRZ = class
  private
    fsCOO: integer;
    fsCFD: integer;
    fsCancelamentoISSQN: Double;
    fsGNFC: integer;
    fsCRO: integer;
    fsValorVendaBruta: Double;
    fsTotalizadoresNaoFiscais: TACBrECFComprovantesNaoFiscais;
    fsICMS: TACBrECFAliquotas;
    fsTodasAliquotas: TACBrECFAliquotas;
    fsAcrescimoICMS: Double;
    fsDescontoICMS: Double;
    fsNaoTributadoICMS: Double;
    fsRelatorioGerencial: TACBrECFRelatoriosGerenciais;
    fsCRZ: integer;
    fsISSQN: TACBrECFAliquotas;
    fsGRG: integer;
    fsValorGrandeTotal: Double;
    fsAcrescimoISSQN: Double;
    fsNaoTributadoISSQN: Double;
    fsIsentoICMS: Double;
    fsSubstituicaoTributariaICMS: Double;
    fsDataDaImpressora: TDateTime;
    fsDataHoraEmissao : TDateTime ;
    fsTotalOperacaoNaoFiscal: Double;
    fsDescontoISSQN: Double;
    fsCancelamentoOPNF: Double;
    fsAcrescimoOPNF: Double;
    fsDescontoOPNF: Double;
    fsCancelamentoICMS: Double;
    fsGNF: integer;
    fsIsentoISSQN: Double;
    fsSubstituicaoTributariaISSQN: Double;
    fsVendaLiquida: Double;
    fsCFC: integer;
    fsCCF: integer;
    fsTotalISSQN: Double;
    fsTotalICMS: Double;
    fsCDC: integer;
    fsCCDC: integer;
    fsNCN: integer;
    fsDataDoMovimento: TDateTime;
    fsMeiosDePagamento: TACBrECFFormasPagamento;
    fsNumeroCOOInicial: AnsiString;
    fsNumeroDoECF: AnsiString;
    fsNumeroDeSerie: AnsiString;
    fsNumeroDeSerieMFD: AnsiString;
    fsNumeroDaLoja: AnsiString;
    fsTotalTroco: Double;
    procedure SetDataDoMovimento(AValue: TDateTime);
  public
    constructor Create;
    destructor Destroy; override ;
    procedure Clear;
    Procedure CalculaValoresVirtuais;

    Function MontaDadosReducaoZ : String;
    procedure AdicionaAliquota( AliqZ: TACBrECFAliquota );

    property DataDaImpressora: TDateTime read fsDataDaImpressora write fsDataDaImpressora;
    property DataHoraEmissao: TDateTime read fsDataHoraEmissao write fsDataHoraEmissao;
    property NumeroDeSerie: AnsiString read fsNumeroDeSerie write fsNumeroDeSerie;
    property NumeroDeSerieMFD: AnsiString read fsNumeroDeSerieMFD write fsNumeroDeSerieMFD;
    property NumeroDoECF: AnsiString read fsNumeroDoECF write fsNumeroDoECF;
    property NumeroDaLoja: AnsiString read fsNumeroDaLoja write fsNumeroDaLoja;
    property NumeroCOOInicial: AnsiString read fsNumeroCOOInicial write fsNumeroCOOInicial;
    // REDUÇÃO Z
    property DataDoMovimento: TDateTime read fsDataDoMovimento write SetDataDoMovimento;
    // CONTADORES
    property COO: integer read fsCOO write fsCOO;
    property GNF: integer read fsGNF write fsGNF;
    property CRO: integer read fsCRO write fsCRO;
    property CRZ: integer read fsCRZ write fsCRZ;
    property CCF: integer read fsCCF write fsCCF;
    property CFD: integer read fsCFD write fsCFD;   //Contador Fita Detalhe
    property CDC: integer read fsCDC write fsCDC;
    property NCN: integer read fsNCN write fsNCN;
    property GRG: integer read fsGRG write fsGRG;
    property GNFC: integer read fsGNFC write fsGNFC;
    property CCDC: integer read fsCCDC write fsCCDC;
    property CFC: integer read fsCFC write fsCFC;
    // TOTALIZADORES
    property ValorGrandeTotal: Double read fsValorGrandeTotal write fsValorGrandeTotal;
    property ValorVendaBruta: Double read fsValorVendaBruta write fsValorVendaBruta;
    property CancelamentoICMS: Double read fsCancelamentoICMS write fsCancelamentoICMS;
    property DescontoICMS: Double read fsDescontoICMS write fsDescontoICMS;
    property TotalISSQN: Double read fsTotalISSQN write fsTotalISSQN;
    property TotalICMS: Double read fsTotalICMS write fsTotalICMS;
    property CancelamentoISSQN: Double read fsCancelamentoISSQN write fsCancelamentoISSQN;
    property CancelamentoOPNF: Double read fsCancelamentoOPNF write fsCancelamentoOPNF;
    property DescontoISSQN: Double read fsDescontoISSQN write fsDescontoISSQN;
    property DescontoOPNF: Double read fsDescontoOPNF write fsDescontoOPNF;
    property VendaLiquida: Double read fsVendaLiquida write fsVendaLiquida;
    property AcrescimoICMS: Double read fsAcrescimoICMS write fsAcrescimoICMS;
    property AcrescimoISSQN: Double read fsAcrescimoISSQN write fsAcrescimoISSQN;
    property AcrescimoOPNF: Double read fsAcrescimoOPNF write fsAcrescimoOPNF;

    // Todas as Aliquotas, de ICMS e ISSQN na ordem original de programação no ECF
    property TodasAliquotas: TACBrECFAliquotas read fsTodasAliquotas;
    // ICMS
    property ICMS: TACBrECFAliquotas read fsICMS;
    property SubstituicaoTributariaICMS: Double read fsSubstituicaoTributariaICMS write fsSubstituicaoTributariaICMS;
    property IsentoICMS: Double read fsIsentoICMS write fsIsentoICMS;
    property NaoTributadoICMS: Double read fsNaoTributadoICMS write fsNaoTributadoICMS;
    // ISSQN
    property ISSQN: TACBrECFAliquotas read fsISSQN;
    property SubstituicaoTributariaISSQN: Double read fsSubstituicaoTributariaISSQN write fsSubstituicaoTributariaISSQN;
    property IsentoISSQN: Double read fsIsentoISSQN write fsIsentoISSQN;
    property NaoTributadoISSQN: Double read fsNaoTributadoISSQN write fsNaoTributadoISSQN;
    // TOTALIZADORES NÃO FISCAIS
    property TotalizadoresNaoFiscais: TACBrECFComprovantesNaoFiscais read fsTotalizadoresNaoFiscais ;
    property TotalOperacaoNaoFiscal: Double read fsTotalOperacaoNaoFiscal write fsTotalOperacaoNaoFiscal;
    // RELATÓRIO GERENCIAL
    property RelatorioGerencial: TACBrECFRelatoriosGerenciais read fsRelatorioGerencial;
    // MEIOS DE PAGAMENTO
    property MeiosDePagamento: TACBrECFFormasPagamento read fsMeiosDePagamento;
    property TotalTroco: Double read fsTotalTroco write fsTotalTroco;
  end;

{ Evento para o usuário exibir os erros encontrados pela classe TACBrECFClass.
  Se o evento OnMsgErro NAO for programado a Classe TACBrECFClass exibirá as
  Msg de erro através de Exceçoes. Se o evento OnMsgErro for programado a Classe
  nao exibe nenhuma msg de erro, que deverao ser tratados dentro deste evento }
TACBrECFExibeErroEvent = procedure(Erro : Exception) of object ;

{ Evento para enviar as msg de Aguarde para o Componente  }
TACBrECFMsgAguarde = procedure(const Mensagem : String) of object ;

{ Evento para enviar mensagem de Retentar para o Componente }
TACBrECFMsgRetentar = procedure(const Mensagem : String;
   const Situacao : String; var Result : Boolean) of object ;

{ Evento disparado quando o componente adicionar algo em MemoBobina  }
TACBrECFBobinaAdicionaLinhas = procedure(const Linhas : String;
   const Operacao : String) of object ;

TACBrECFOnChequeEstado = procedure(const EstadoAtual: TACBrECFCHQEstado;
  var Continuar: Boolean) of object;

TACBrFormMsgProcedure = procedure of object ;

TACBrFormMsgEstado = (fmsNenhum, fmsProcessando, fmsConcluido, fmsAbortado) ;

{$IFDEF FMX}
  TACBrFMXCustomForm = procedure(var CustomForm: TForm) of Object;
{$ENDIF}

{ Classe generica de ECF, nao implementa nenhum modelo especifico, apenas
  declara a Classe. NAO DEVE SER INSTANCIADA. Usada apenas como base para
  as demais Classes de ECF como por exemplo a classe TACBrECFBematech  }

{ Nota sobre procimentos e funções VIRTUAL. Essas funçoes/procedimentos PODEM
  ou NAO ser implementados nas Classes filhas com a clausula OVERRIDE. Se não
  forem implementadas nas classes filhas, a funçao/procedimento definida aqui
  nessa classe (TACBrECFClass) e que será executada }

{ TACBrECFClass }

TACBrECFClass = class
 private
    fsRetentar     : Boolean;
    fsControlePorta: Boolean;
    fsBloqueiaMouseTeclado: Boolean;
    fsExibeMensagem: Boolean;
    fsTempoInicioMsg: Integer;
    fsIntervaloAposComando: Integer ;
    fsMsgAguarde: String;
    fsMsgTrabalhando: String;
    fsMsgRelatorio : String;
    fsPausaRelatorio : Integer ;
    fsLinhasEntreCupons : Integer ;
    fsMaxLinhasBuffer : Integer ;
    fsMsgPausaRelatorio : String ;
    fsMsgPoucoPapel: Integer;
    fsDescricaoGrande: Boolean;
    fsOnMsgErro    : TACBrECFExibeErroEvent ;
    fsOnMsgAguarde : TACBrECFMsgAguarde ;
    fsAguardandoResposta: Boolean;
    fsOnAguardandoRespostaChange: TNotifyEvent;
    fsOnMsgPoucoPapel: TNotifyEvent;
    fsOnErrorSemPapel : TNotifyEvent ;
    fsOnMsgRetentar : TACBrECFMsgRetentar ;
    fsOnChequeEstado: TACBrECFOnChequeEstado;
    fsOperador: String;
    fsBytesRec : Integer ;
    fsAguardaImpressao: Boolean;

    {$IFNDEF NOGUI}
      fsFormMsg: TForm ;           { Form para exibir Msgs de Aguarde... }
      fsFormMsgProcedureAExecutar : TACBrFormMsgProcedure ;
      fsFormMsgTeclaParaFechar    : Word ;
      fsFormMsgEstado             : TACBrFormMsgEstado ;
      fsFormMsgControla : Boolean ;
      fsFormMsgException: String  ;
      fsUsandoBlockInput : Boolean ;
      {$IFDEF FMX}
        fsOnCriarFormMsg: TACBrFMXCustomForm; //Variavel do Evento de Formulário Personalizado.
        fsOnDrawFormMsg: TACBrECFMsgAguarde; //Variavel para o usuário desenhar ou enviar para qualquer componente.
      {$ENDIF}
    {$ENDIF}

    fsRelatorio : TStrings ;
    fsVias      : Word ;
    fsIndiceRG  : Integer;

    fsPathDLL: string;
    fpInfoRodapeCupom: TACBrECFRodape;
    fpRespostasComando: TACBrInformacoes;

    procedure AtivarPorta;
    procedure DesativarPorta;
    function GetNumMaxLinhasRodape: Integer;
    function GetPathDLL : string ;
    function GetTotalizadoresNaoTributados: TACBrECFTotalizadoresNaoTributados;
    procedure SetAtivo(const Value: Boolean);
    procedure SetTimeOut(const Value: Integer);
    function GetTimeOut: Integer;

    procedure ErroAbstract( NomeProcedure : String ) ;
    function GetAliquotas: TACBrECFAliquotas;
    function GetFormasPagamentos: TACBrECFFormasPagamento;
    procedure SetAguardandoResposta(const Value: Boolean);
    function GetComprovantesNaoFiscais: TACBrECFComprovantesNaoFiscais;
    function GetUnidadesMedida: TACBrECFUnidadesMedida;
    function GetRelatoriosGerenciais: TACBrECFRelatoriosGerenciais;

    {$IFNDEF NOGUI}
      procedure FormMsgTimer(Sender: TObject);
      procedure FormMsgCloseQuery(Sender: TObject; var CanClose: Boolean);
      {$IFDEF FMX}
      procedure FormMsgKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char;
        Shift: TShiftState);
      {$ELSE}
      procedure FormMsgKeyPress(Sender: TObject; var Key: Char);
      {$ENDIF}

      {$IFDEF VisualCLX}
       procedure FormMsgEvent(Sender: QObjectH; Event: QEventH;
         var Handled: Boolean);
      {$ENDIF}
      {$IFDEF LINUX}
       {$IFNDEF FPC}
        procedure FormShow(Sender: TObject);
       {$ENDIF}
      {$ENDIF}
      function FormMsgExibe : Boolean;

      {$IFDEF MSWINDOWS}
        procedure BlockInput(const Block, ClearTypeAhead: Boolean);
      {$ENDIF}
    {$ENDIF}

    procedure DoLeResposta ;
    procedure DoRelatorioGerencial ;
    procedure DoCupomVinculado ;

 Protected
    fpDevice : TACBrDevice ;
    fpOwner  : TComponent ;   { Componente ACBrECF }
    fpAtivo  : Boolean ;
    fpColunas: Integer;
    fpNumMaxLinhasRodape: Integer;
    fpPaginaDeCodigo : Word ;
    fpRFDID  : String;
    fpModeloStr: String;
    fpComandoEnviado: AnsiString;
    fpRespostaComando: AnsiString;
    fpUltimaMsgPoucoPapel : TDateTime ;
    fpEstado: TACBrECFEstado;
    fpArredondaPorQtd: Boolean;
    fpArredondaItemMFD : Boolean ;
    fpIgnorarErroSemPapel : Boolean ;
    fpIgnorarTagsFormatacao : Boolean ;
    fpDecimaisPreco: Integer;
    fpDecimaisQtd: Integer;
    fpArqLOG: String;
    fpOnGravarLog: TACBrGravarLog;
    fpComandoLOG: AnsiString;
    fpMFD: Boolean;
    fpTermica: Boolean;
    fpIdentificaConsumidorRodape: Boolean;
    fpModoPreVenda: Boolean;

    { Coleçao de objetos TACBrECFAliquota }
    fpAliquotas: TACBrECFAliquotas;
    { Coleção de objetos TACBrECFTotalizadorNaoTributado }
    fpTotalizadoresNaoTributados: TACBrECFTotalizadoresNaoTributados;
    { Coleçao de objetos TACBrECFFormasPagamento }
    fpFormasPagamentos : TACBrECFFormasPagamento;
    { Coleçao de objetos TACBrECFRelatórios Gerenciais }
    fpRelatoriosGerenciais : TACBrECFRelatoriosGerenciais;
    { Coleçao de objetos TACBrECFComprovantesNaoFiscais }
    fpComprovantesNaoFiscais : TACBrECFComprovantesNaoFiscais;
    { Coleçao de objetos TACBrECFUnidadesMedida}
    fpUnidadesMedida : TACBrECFUnidadesMedida;

    fpConsumidor : TACBrECFConsumidor ;

    { Class com instacia para armazenar dados da RZ }
    fpDadosReducaoZClass: TACBrECFDadosRZ;

    procedure GeraErro( E : Exception ) ;

    function GetModeloStr: String; virtual ;
    function GetDataHora: TDateTime; virtual ;
    function GetNumCupom: String; virtual ;
    function GetNumECF: String; virtual ;
    function GetNumLoja: String; virtual ;
    function GetNumSerie: String; virtual ;
    function GetNumSerieMFD: String; virtual ;
    function GetNumVersao: String ; virtual ;
    function GetSubTotal: Double; virtual ;
    function GetTotalPago: Double; virtual ;
    function GetNumReducoesZRestantes: String; virtual;

    function GetCNPJ: String; virtual ;
    function GetIE: String; virtual ;
    function GetIM: String; virtual ;
    function GetCliche: AnsiString; virtual ;
    function GetUsuarioAtual: String; virtual ;
    function GetDataHoraSB: TDateTime; virtual ;
    function GetSubModeloECF: String ; virtual ;

    function GetPAF: String; virtual ;
    function GetDataMovimento: TDateTime; virtual ;
    function GetDataHoraUltimaReducaoZ : TDateTime ; virtual ;
    function GetGrandeTotal: Double; virtual ;
    function GetNumCRO: String; virtual ;
    function GetNumCCF: String; virtual ;
    function GetNumGNF: String; virtual ;
    function GetNumGRG: String; virtual ;
    function GetNumCDC: String; virtual ;
    function GetNumCFC: String; virtual ;
    function GetNumGNFC: String; virtual ;
    function GetNumCRZ: String; virtual ;
    function GetNumCFD: String; virtual ;
    function GetNumNCN: String; virtual ;
    function GetNumCCDC: String; virtual ;
    function GetVendaBruta: Double; virtual ;
    function GetTotalTroco: Double; virtual ;

    function GetTotalAcrescimos: Double; virtual ;
    function GetTotalCancelamentos: Double; virtual ;
    function GetTotalCancelamentosEmAberto: Double; virtual ;
    function GetTotalDescontos: Double; virtual ;
    function GetTotalSubstituicaoTributaria: Double; virtual ;
    function GetTotalNaoTributado: Double; virtual ;
    function GetTotalIsencao: Double; virtual ;
    function GetTotalNaoFiscal: Double; virtual ;

    function GetTotalAcrescimosISSQN: Double; virtual ;
    function GetTotalCancelamentosISSQN: Double; virtual ;
    function GetTotalCancelamentosEmAbertoISSQN: Double; virtual;
    function GetTotalDescontosISSQN: Double; virtual ;
    function GetTotalIsencaoISSQN: Double; virtual ;
    function GetTotalNaoTributadoISSQN: Double; virtual ;
    function GetTotalSubstituicaoTributariaISSQN: Double; virtual ;

    function GetTotalAcrescimosOPNF: Double; virtual ;
    function GetTotalCancelamentosOPNF: Double; virtual ;
    function GetTotalDescontosOPNF: Double; virtual ;

    function GetNumCOOInicial: String; virtual ;
    function GetNumUltimoItem: Integer; virtual ;

    function GetDadosUltimaReducaoZ: String; Virtual ;
    function GetDadosReducaoZ: String; Virtual ;
    Procedure InitDadosUltimaReducaoZ;

    function GetEstado: TACBrECFEstado; virtual ;
    function GetGavetaAberta: Boolean; virtual ;
    function GetPoucoPapel : Boolean; virtual ;
    function GetHorarioVerao: Boolean; virtual ;
    function GetArredonda: Boolean; virtual ;
    function GetChequePronto: Boolean; virtual ;
    function GetParamDescontoISSQN: Boolean; virtual;

    function GetTipoUltimoDocumento : TACBrECFTipoDocumento ; virtual ;

    procedure SetDecimaisPreco(AValue: Integer); virtual;
    procedure SetDecimaisQtd(AValue: Integer); virtual;

    Function EnviaComando_ECF( cmd : AnsiString ) : AnsiString ; virtual ;

    procedure LeResposta ; virtual ;
    function TransmiteComando( const Cmd : AnsiString ) : Boolean ; virtual ;

    function VerificaFimLeitura(var Retorno: AnsiString; var TempoLimite: TDateTime) : Boolean ; virtual ;
    function VerificaFimImpressao(var TempoLimite: TDateTime) : Boolean ; virtual ;
    Procedure VerificaEmLinha( TimeOut : Integer = 3) ; virtual ;

    procedure ListaRelatorioGerencial(Relatorio : TStrings; Vias : Integer = 1; Indice: Integer = 0);
       virtual ;
    Procedure ListaCupomVinculado( Relatorio : TStrings; Vias : Integer = 1) ;
       virtual ;
    procedure PausarRelatorio( Via : Integer) ;

    procedure DoOnMsgPoucoPapel( Mensagem : String = '') ;
    procedure DoOnErrorSemPapel ;
    Function DoOnMsgRetentar( const Mensagem : String;
       const Situacao : String = '') : Boolean ;
    procedure DoOnChequeEstado(const Estado: TACBrECFCHQEstado;
       var Continuar: Boolean);

    procedure ImprimirLinhaALinha( Texto, Cmd : AnsiString ) ;
 public
    Constructor create( AOwner : TComponent ) ;
    Destructor Destroy  ; override ;

    property Owner : TComponent read fpOwner ;
    property Device: TACBrDevice read fpDevice;
    Property Ativo : Boolean read fpAtivo write SetAtivo ;
    procedure Ativar ; virtual ;
    procedure Desativar ; virtual ;

    procedure GravaLog(AString: AnsiString; Traduz :Boolean = False);

    property ArredondaPorQtd : Boolean read fpArredondaPorQtd
       write fpArredondaPorQtd ;
    property ArredondaItemMFD : Boolean read fpArredondaItemMFD
       write fpArredondaItemMFD ;
    property IgnorarErroSemPapel : Boolean read fpIgnorarErroSemPapel
       write fpIgnorarErroSemPapel;
    property IgnorarTagsFormatacao : Boolean read fpIgnorarTagsFormatacao
                write fpIgnorarTagsFormatacao default false ;

    property DecimaisPreco : Integer read fpDecimaisPreco
       write SetDecimaisPreco default 3 ;
    property DecimaisQtd : Integer read fpDecimaisQtd
       write SetDecimaisQtd default 3 ;
    property ArqLOG : String read fpArqLOG write fpArqLOG ;
    property OnGravarLog : TACBrGravarLog read fpOnGravarLog write fpOnGravarLog;
    property ComandoLOG : AnsiString read fpComandoLOG write fpComandoLOG ;
    property AguardaImpressao : Boolean read fsAguardaImpressao
       write fsAguardaImpressao ;

    {$IFNDEF NOGUI}
      function FormMsgDoProcedure( AProcedure : TACBrFormMsgProcedure;
         TeclaParaFechar : Word) : Boolean ;
      procedure FormMsgPinta( Texto : String ) ;
      property FormMsgEstado   : TACBrFormMsgEstado read fsFormMsgEstado ;
      property FormMsgControla : Boolean read fsFormMsgControla write fsFormMsgControla ;
        {$IFDEF FMX}
          property OnCriarFormMsg: TACBrFMXCustomForm read fsOnCriarFormMsg write fsOnCriarFormMsg;
          property OnDrawFormMsg: TACBrECFMsgAguarde read fsOnDrawFormMsg write fsOnDrawFormMsg;
        {$ENDIF}
    {$ENDIF}

    { Proriedades de uso interno, configurando o funcionamento da classe,
      atribuidas pelo Objeto TACBrECF dono dessa classe }
//    property OnMsgErro : TACBrECFExibeErroEvent read  fpOnMsgErro
//                write fpOnMsgErro ;
    property OnMsgAguarde : TACBrECFMsgAguarde
        read  fsOnMsgAguarde write fsOnMsgAguarde ;
    property OnAguardandoRespostaChange : TNotifyEvent
        read fsOnAguardandoRespostaChange write fsOnAguardandoRespostaChange ;
    property OnMsgPoucoPapel : TNotifyEvent
        read fsOnMsgPoucoPapel write fsOnMsgPoucoPapel ;
    property OnMsgRetentar : TACBrECFMsgRetentar
        read  fsOnMsgRetentar write fsOnMsgRetentar ;
    property OnErrorSemPapel : TNotifyEvent
        read fsOnErrorSemPapel write fsOnErrorSemPapel ;
    property OnChequeEstado: TACBrECFOnChequeEstado
        read fsOnChequeEstado write fsOnChequeEstado;

    Property TimeOut       : Integer read GetTimeOut      write SetTimeOut ;
    Property Retentar      : Boolean read fsRetentar      write fsRetentar ;
    Property ControlePorta : Boolean read fsControlePorta write fsControlePorta ;

    property BloqueiaMouseTeclado : Boolean read  fsBloqueiaMouseTeclado
                                            write fsBloqueiaMouseTeclado ;
    property Operador   : String  read fsOperador   write fsOperador ;
    property MsgAguarde : String  read fsMsgAguarde write fsMsgAguarde ;
    property MsgTrabalhando : String read fsMsgTrabalhando write fsMsgTrabalhando ;
    property MsgRelatorio : String  read fsMsgRelatorio write fsMsgRelatorio ;
    property PausaRelatorio : Integer read fsPausaRelatorio
                write fsPausaRelatorio ;
    property LinhasEntreCupons : Integer read fsLinhasEntreCupons
                write fsLinhasEntreCupons ;
    property MaxLinhasBuffer : Integer read fsMaxLinhasBuffer
                write fsMaxLinhasBuffer ;
    property MsgPausaRelatorio : String  read fsMsgPausaRelatorio
                write fsMsgPausaRelatorio ;
    property ExibeMensagem : Boolean read fsExibeMensagem write fsExibeMensagem ;
    property TempoInicioMsg : Integer read  fsTempoInicioMsg
                                      write fsTempoInicioMsg ;
    Property IntervaloAposComando : Integer read  fsIntervaloAposComando
                                            write fsIntervaloAposComando ;
    property MsgPoucoPapel : Integer read  fsMsgPoucoPapel
                                     write fsMsgPoucoPapel  ;
    property DescricaoGrande : Boolean read fsDescricaoGrande
                                      write fsDescricaoGrande ;

    property PaginaDeCodigo : Word read fpPaginaDeCodigo write fpPaginaDeCodigo ;

    property InfoRodapeCupom: TACBrECFRodape read fpInfoRodapeCupom write fpInfoRodapeCupom;

    { Proriedades ReadOnly }
    Property Colunas  : Integer read fpColunas  ;
    Property ModeloStr: String  read GetModeloStr ;
    Property RFDID    : String  read fpRFDID ;
    Property AguardandoResposta : Boolean read fsAguardandoResposta
       write SetAguardandoResposta ;
    { String com Comando exatamente como foi enviado para impressora }
    Property ComandoEnviado : AnsiString read fpComandoEnviado
       write fpComandoEnviado ;
    { String com a Resposta Completa da Impressora (sem tratamentos) }
    Property RespostaComando : AnsiString read fpRespostaComando
       write fpRespostaComando ;
    { lista com as resposta de comando tratadas }
    property RespostasComando: TACBrInformacoes read fpRespostasComando
       write fpRespostasComando;

    { Propriedades relacionadas aos dados do ECF }
    { ECF - Variaveis }
    Property DataHora  : TDateTime read GetDataHora  ;
    Property NumCupom  : String    read GetNumCupom  ;
    Property NumLoja   : String    read GetNumLoja   ;
    Property NumECF    : String    read GetNumECF    ;
    Property NumSerie  : String    read GetNumSerie  ;
    Property NumSerieMFD  : String read GetNumSerieMFD  ;
    Property NumVersao : String    read GetNumVersao ;
    Property NumReducoesZRestantes: String read GetNumReducoesZRestantes ;
    Property NumMaxLinhasRodape: Integer read GetNumMaxLinhasRodape;


    { Dados da Reducao Z - Registro 60M }
    Property DataMovimento      : TDateTime  read GetDataMovimento ;
    Property DataHoraUltimaReducaoZ : TDateTime read GetDataHoraUltimaReducaoZ ;
    Property CNPJ               : String     read GetCNPJ ;
    Property IE                 : String     read GetIE ;
    Property IM                 : String     read GetIM ;
    Property Cliche             : AnsiString read GetCliche ;
    Property UsuarioAtual       : String     read GetUsuarioAtual ;
    Property DataHoraSB         : TDateTime  read GetDataHoraSB ;
    Property SubModeloECF       : String     read GetSubModeloECF ;

    Property PAF                : String     read GetPAF ;
    Property NumCRZ             : String     read GetNumCRZ ;
    Property NumCRO             : String     read GetNumCRO ;
    Property NumCCF             : String     read GetNumCCF ;
    Property NumGNF             : String     read GetNumGNF ;
    Property NumGRG             : String     read GetNumGRG ;
    Property NumCDC             : String     read GetNumCDC ;
    Property NumCFC             : String     read GetNumCFC ;
    Property NumGNFC            : String     read GetNumGNFC ;
    Property NumCFD             : String     read GetNumCFD ;
    Property NumNCN             : String     read GetNumNCN ;
    Property NumCCDC            : String     read GetNumCCDC ;
    Property NumCOOInicial      : String     read GetNumCOOInicial ;
    Property VendaBruta         : Double     read GetVendaBruta ;
    Property GrandeTotal        : Double     read GetGrandeTotal ;
    Property TotalCancelamentos : Double     read GetTotalCancelamentos ;
    Property TotalCancelamentosEmAberto : Double     read GetTotalCancelamentosEmAberto ;
    Property TotalDescontos     : Double     read GetTotalDescontos ;
    Property TotalAcrescimos    : Double     read GetTotalAcrescimos ;
    Property TotalTroco         : Double     read GetTotalTroco ;
    Property TotalSubstituicaoTributaria : Double
       read GetTotalSubstituicaoTributaria ;
    Property TotalNaoTributado  : Double     read GetTotalNaoTributado ;
    Property TotalIsencao       : Double     read GetTotalIsencao ;

    Property TotalCancelamentosISSQN          : Double read GetTotalCancelamentosISSQN;
    Property TotalCancelamentosEmAbertoISSQN  : Double read GetTotalCancelamentosEmAbertoISSQN;
    Property TotalDescontosISSQN              : Double read GetTotalDescontosISSQN;
    Property TotalAcrescimosISSQN             : Double read GetTotalAcrescimosISSQN;
    Property TotalSubstituicaoTributariaISSQN : Double read GetTotalSubstituicaoTributariaISSQN;
    Property TotalNaoTributadoISSQN           : Double read GetTotalNaoTributadoISSQN;
    Property TotalIsencaoISSQN                : Double read GetTotalIsencaoISSQN;

    Property TotalCancelamentosOPNF           : Double read GetTotalCancelamentosOPNF;
    Property TotalDescontosOPNF               : Double read GetTotalDescontosOPNF;
    Property TotalAcrescimosOPNF              : Double read GetTotalAcrescimosOPNF;

    Property NumUltItem         : Integer    read GetNumUltimoItem ;
    Property TotalNaoFiscal     : Double     read GetTotalNaoFiscal ;

    Property DadosReducaoZ : String  read GetDadosReducaoZ ;
    Property DadosUltimaReducaoZ : String read GetDadosUltimaReducaoZ ;
    Property DadosReducaoZClass: TACBrECFDadosRZ read fpDadosReducaoZClass;

    { Aliquotas de ICMS }
    procedure CarregaAliquotas ; virtual ;
    procedure LerTotaisAliquota ; virtual ;
    Property Aliquotas : TACBrECFAliquotas read GetAliquotas ;
    function AchaICMSAliquota( Aliquota : Double; Tipo : Char = ' ' ) :
       TACBrECFAliquota ;  overload ; virtual;
    function AchaICMSAliquota( var AliquotaICMS : String ) :
       TACBrECFAliquota ;  overload ; virtual;
    function AchaICMSIndice( Indice : String ) : TACBrECFAliquota ; virtual ;
    Procedure ProgramaAliquota( Aliquota : Double; Tipo : Char = 'T';
       Posicao : String = '') ; virtual ;

    { TotalizadoresNaoTributados, F1, N1, I1, FS1, NS1, IS1 }
    procedure CarregaTotalizadoresNaoTributados ; virtual;
    procedure LerTotaisTotalizadoresNaoTributados ; virtual;
    Property TotalizadoresNaoTributados : TACBrECFTotalizadoresNaoTributados
       read GetTotalizadoresNaoTributados ;
    function AchaTotalizadorNaoTributadoIndice( Indice : String ) :
       TACBrECFTotalizadorNaoTributado ; virtual;
    function SomaTotalizadorNaoTributadoIndice( Indice : String ) : Double;

    { Formas de Pagamento }
    procedure CarregaFormasPagamento ; virtual ;
    procedure LerTotaisFormaPagamento ; virtual ;
    Property FormasPagamento : TACBrECFFormasPagamento read GetFormasPagamentos;
    function AchaFPGDescricao( Descricao : String;
       BuscaExata : Boolean = False;
       IgnorarCase : Boolean = True;
       IgnorarAcentos : Boolean = False) : TACBrECFFormaPagamento ; virtual ;
    function AchaFPGIndice( Indice : String ) : TACBrECFFormaPagamento ;
       virtual ;
    Procedure ProgramaFormaPagamento( var Descricao: String;
       PermiteVinculado : Boolean = true; Posicao : String = '' ) ; virtual ;

    { Relatório Gerencial (RG) }
    procedure CarregaRelatoriosGerenciais ; virtual ;
    Property RelatoriosGerenciais : TACBrECFRelatoriosGerenciais
       read GetRelatoriosGerenciais ;
    procedure LerTotaisRelatoriosGerenciais ; virtual ;
    function AchaRGDescricao( Descricao : String;
       BuscaExata : Boolean = False; IgnorarCase : Boolean = True ) :
       TACBrECFRelatorioGerencial ; virtual ;
    function AchaRGIndice( Indice : String ) : TACBrECFRelatorioGerencial ;
       virtual ;
    Procedure ProgramaRelatorioGerencial( var Descricao: String;
       Posicao : String = '') ; virtual ;

    { Comprovantes Nao Fiscais (CNF) }
    procedure CarregaComprovantesNaoFiscais ; virtual ;
    procedure LerTotaisComprovanteNaoFiscal ; virtual ;
    Property ComprovantesNaoFiscais : TACBrECFComprovantesNaoFiscais
       read GetComprovantesNaoFiscais ;
    function AchaCNFDescricao( Descricao : String;
       BuscaExata : Boolean = False; IgnorarCase : Boolean = True ) :
       TACBrECFComprovanteNaoFiscal ; virtual ;
    function AchaCNFIndice( Indice : String ) : TACBrECFComprovanteNaoFiscal ;
       virtual ;
    function AchaCNFFormaPagamento( CodFPG : String ) :
       TACBrECFComprovanteNaoFiscal ; virtual ;
    Procedure ProgramaComprovanteNaoFiscal( var Descricao: String;
       Tipo : String = ''; Posicao : String = '') ; virtual ;

    { Unidades de Medida (UMD) }
    procedure CarregaUnidadesMedida ; virtual ;
    Property UnidadesMedida : TACBrECFUnidadesMedida read GetUnidadesMedida;
    function AchaUMDDescricao( Descricao : String ) : TACBrECFUnidadeMedida ;
       virtual ;
    function AchaUMDIndice( Indice : String ) : TACBrECFUnidadeMedida ;
       virtual ;
    Procedure ProgramaUnidadeMedida( var Descricao: String) ; virtual ;

    { ECF - Flags }
    Function EmLinha( lTimeOut : Integer = 1) : Boolean ; virtual ;
    Property PoucoPapel   : Boolean read GetPoucoPapel ;
    Property Estado       : TACBrECFEstado read GetEstado ;
    Property HorarioVerao : Boolean read GetHorarioVerao ;
    Property Arredonda    : Boolean read GetArredonda ;
    Property Termica      : Boolean read fpTermica ;
    Property MFD          : Boolean read fpMFD ;
    Property ParamDescontoISSQN : Boolean read GetParamDescontoISSQN ;
    Property IdentificaConsumidorRodape : Boolean read fpIdentificaConsumidorRodape ;
    Property ModoPreVenda: Boolean read fpModoPreVenda write fpModoPreVenda ;

    Property TipoUltimoDocumento: TACBrECFTipoDocumento read GetTipoUltimoDocumento ;

    { Procedimentos de Cupom Fiscal }
    property Consumidor : TACBrECFConsumidor read fpConsumidor ;
    Procedure AbreCupom ; virtual ;
    Procedure AbreBilhetePassagem( Origem: String; Destino: String;
      Linha: String; Agencia: String; DataHora: TDateTime;
      Poltrona: String; Plataforma: String; Tipo: TACBrECFTipoBilhete; UFDestino: String;
      PassageiroRG: String; PassageiroNome: String; PassageiroEnd: String); virtual;
    procedure LegendaInmetroProximoItem ; Virtual ;
    Procedure VendeItem( Codigo, Descricao : String; AliquotaECF : String;
       Qtd : Double ; ValorUnitario : Double; ValorDescontoAcrescimo : Double = 0;
       Unidade : String = ''; TipoDescontoAcrescimo : String = '%';
       DescontoAcrescimo : String = 'D'; CodDepartamento: Integer = -1) ; virtual ;

    Procedure VendeItemEx( Codigo, Descricao : String; AliquotaICMS : String;
           Qtd : Double ; ValorUnitario : Double; ValorDescontoAcrescimo : Double = 0;
           Unidade : String = 'UN'; TipoDescontoAcrescimo : String = '%';
           DescontoAcrescimo : String = 'D'; CodDepartamento: Integer = -1;
           EAN13: String = '';              // Código Barras do Produto (GTIN-13)
           CasasDecimaisQtde: Integer = 0;  // Se 0 assume o valor de DecimaisQtd
           CasasDecimaisValor: Integer = 0; // Se 0 assume o valor de DecimaisPreco
           ArredondaTrunca: Char = 'A';     // Se diferente de 'A' ou 'T' assume o valor de "Arredonda"
           NCM: String = '';                // Código da Nomenclatura Comum do MERCOSUL
           CFOP: String = '';               // Código Fiscal de Operações e Prestações
           InformacaoAdicional: String = '';// Texto Livro, até 500 caracteres
           TotalDosTributos: Double = 0;    // Valor da lei "De olho no Imposto)
           OrigemProduto: Integer = 0;      // 0–Nacional; 1–Estrangeira Import.direta; 2–Estrangeira–Mercado interno

           CST_ICMS: String = '';           // ICMS: Código de Situação Tributária
           ModalidadeBCICMS: Integer = 0;   // ICMS: Modalidade Base de Calculo: 0 – Margem do valor agregado (%)
                                            //                                   1 – Pauta (Valor)
                                            //                                   2 – Preço tabelado máx. (Valor)
                                            //                                   3 – Valor da operação
           PercentualReducaoBCICMS: Double = 0; // ICMS:
           CSOSN: String = '';                  // Simples Nacional: Código de Situação da Operação
           ValorBaseCalculoSN: Double = 0;      // Simples Nacional: Base de Calculo
           ValorICMSRetidoSN: Double = 0;       // Simples Nacional: Valor Retido para ICMS
           AliquotaCalculoCreditoSN: Double = 0;// Simples Nacional:
           ValorCreditoICMSSN: Double = 0;      // Simples Nacional:
           ItemListaServico: String = '';   // Serviço apenas: código do serviço prestado: lista de serviços anexa à Lei Complementar nº 116,
           CodigoISS: String = '';          // Serviço apenas: Código do Imposto Sobre Serviço
           NaturezaOperacaoISS: String = '';// Serviço apenas: com os seguintes valores possíveis: '00' até '08',
           IndicadorIncentivoFiscalISS: Integer = 1;  // Serviço apenas: para indicar se o estado é participante ou não da (Lei do Incentivo Fiscal – ISS), valores: 1 (participante) ou 2 (não participante)
           CodigoIBGE: String = '';         // Serviço apenas: Código do município
           ModalidadeBCICMSST: Integer = 0; // ICMS ST: Modalidade Base de Calculo, 0 – Preço tabelado ou máximo sugerido
                                            //       Substituição Tributária        1 – Lista negativa (valor)
                                            //                                      2 – Lista positiva (valor)
                                            //                                      3 – Lista neutra (valor)
                                            //                                      4 – Margem do valor agregado (%)
                                            //                                      5 – Pauta (valor)
           PercentualMargemICMSST: Double = 0;    // ICMS ST:
           PercentualReducaoBCICMSST: Double = 0; // ICMS ST:
           ValorReducaoBCICMSST: Double = 0;      // ICMS ST:
           AliquotaICMSST: Double = 0;            // ICMS ST:
           ValorICMSST: Double = 0;               // ICMS ST:
           ValorICMSDesonerado: Double = 0;
           MotivoDesoneracaoICMS: Integer = 9;    // 3 – Uso na agropecuária; 9 – Outros; 12 – Órgão de fomento e desenvolvimento agropecuário
           CST_PIS: String = '';
           BaseCalculoPIS: Double = 0;
           AliquotaPIS: Double = 0;
           ValorPIS: Double = 0;
           QuantidadeVendidaPIS: Double = 0;
           ValorAliquotaPIS: Double = 0;
           CST_COFINS: String = '';
           BaseCalculoCOFINS: Double = 0;
           AliquotaCOFINS: Double = 0;
           ValorCOFINS: Double = 0;
           QuantidadeVendidaCOFINS: Double = 0;
           ValorAliquotaCOFINS: Double = 0;
           CEST: String = '');   // Código do CEST para esse produto (7 dígitos)
           virtual;

    Procedure DescontoAcrescimoItemAnterior( ValorDescontoAcrescimo : Double = 0;
       DescontoAcrescimo : String = 'D'; TipoDescontoAcrescimo : String = '%';
       NumItem : Integer = 0 ) ;  virtual ;
    Procedure SubtotalizaCupom( DescontoAcrescimo : Double = 0;
       MensagemRodape : AnsiString = '' ) ;  virtual ;
    procedure CancelaDescontoAcrescimoSubTotal(TipoAcrescimoDesconto: Char) ;
       Virtual ;{ A -> Acrescimo D -> Desconto }
    Procedure EfetuaPagamento( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false;
       CodMeioPagamento: Integer = 0) ; virtual ;
    procedure EstornaPagamento(const CodFormaPagtoEstornar,
      CodFormaPagtoEfetivar : String; const Valor: Double;
      Observacao : AnsiString = '') ; Virtual ;

    { Para quebrar linhas nos parametros Observacao use #10 ou chr(10),
      Geralmente o ECF aceita no máximo 8 linhas }
    Procedure FechaCupom( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; virtual ;
    Procedure CancelaCupom( NumCOOCancelar: Integer = 0 ) ; virtual ;
    Procedure CancelaItemVendido( NumItem : Integer ) ; virtual ;
    procedure CancelaItemVendidoParcial( NumItem : Integer;
      Quantidade : Double) ; Virtual ;
    procedure CancelaDescontoAcrescimoItem( NumItem : Integer;
      TipoAcrescimoDesconto: String = 'D') ; Virtual ;
    Property Subtotal  : Double read GetSubTotal ;
    Property TotalPago : Double read GetTotalPago ;

    { Procedimentos de Cupom Não Fiscal }
    Procedure NaoFiscalCompleto( CodCNF : String; Valor : Double;
       CodFormaPagto  : String; Obs : AnsiString = ''; IndiceBMP : Integer = 0 ) ; virtual ;
    Procedure AbreNaoFiscal( CPF_CNPJ: String = ''; Nome: String = '';
       Endereco: String = '' ) ; virtual ;
    Procedure RegistraItemNaoFiscal( CodCNF : String; Valor : Double;
       Obs : AnsiString = '') ; virtual ;
    Procedure SubtotalizaNaoFiscal( DescontoAcrescimo : Double = 0;
       MensagemRodape: AnsiString = '') ; virtual ;
    Procedure EfetuaPagamentoNaoFiscal( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false) ; virtual ;
    Procedure FechaNaoFiscal( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; virtual ;
    Procedure CancelaNaoFiscal ; virtual ;
    Procedure CancelaItemNaoFiscal(const AItem: Integer); virtual;

    procedure Sangria( const Valor: Double;  Obs : AnsiString;
       DescricaoCNF: String; DescricaoFPG: String; IndiceBMP: Integer ) ; virtual ;
    procedure Suprimento( const Valor: Double; Obs : AnsiString;
       DescricaoCNF: String; DescricaoFPG: String; IndiceBMP: Integer ) ; virtual ;

    Function EstornaCCD( const Todos: Boolean = True) : Integer; virtual ;

    { Gaveta de dinheiro }
    Procedure AbreGaveta  ; virtual ;
    Property GavetaAberta : Boolean read GetGavetaAberta ;

    { Relatorios }
    Procedure LeituraX ; virtual ;
    Procedure LeituraXSerial( Linhas : TStringList) ; virtual ;
    Procedure ReducaoZ( DataHora : TDateTime = 0 ) ; virtual ;
    Procedure RelatorioGerencial(Relatorio : TStrings; Vias : Integer = 1; Indice: Integer = 0) ;
    Procedure AbreRelatorioGerencial(Indice: Integer = 0) ; virtual ;
    Procedure LinhaRelatorioGerencial( Linha : AnsiString; IndiceBMP: Integer = 0 ) ; virtual ;

    Procedure CupomVinculado(COO, CodFormaPagto, CodComprovanteNaoFiscal :
       String; Valor : Double;  Relatorio : TStrings;
       Vias : Integer = 1) ;
    Procedure AbreCupomVinculado(COO, CodFormaPagto, CodComprovanteNaoFiscal :
       String; Valor : Double) ; virtual ;
    Procedure LinhaCupomVinculado( Linha : AnsiString ) ; virtual ;

    Procedure SegundaViaVinculado; virtual;
    procedure ReimpressaoVinculado; virtual;

    Procedure FechaRelatorio ; virtual ;
    Procedure PulaLinhas( NumLinhas : Integer = 0 ) ; virtual ;
    Procedure CortaPapel( const CorteParcial : Boolean = false) ; virtual ;

    { Cheques }
    Procedure ImprimeCheque(Banco : String; Valor : Double ; Favorecido,
       Cidade : String; Data : TDateTime ;Observacao : String = '') ; virtual ;
    Procedure CancelaImpressaoCheque ; virtual ;
    Function LeituraCMC7 : AnsiString ; virtual ;
    Property ChequePronto : Boolean read GetChequePronto ;

    { Utilitarios e Diversos }
    Procedure MudaHorarioVerao ; overload ; virtual ;
    Procedure MudaHorarioVerao( EHorarioVerao : Boolean ) ; overload ; virtual ;
    Procedure MudaArredondamento( Arredondar : Boolean ) ; virtual ;
    Procedure PreparaTEF ; virtual ; { Carrega as Formas, de Pagamento e CNF,
                            verifica por Vinculos, etc Particular de cada ECF }
    Procedure CorrigeEstadoErro( Reducao: Boolean = True ) ; virtual ; { Verifica o estado da impressora e
                                              tenta deixar em estado Livre }
    Procedure ImpactoAgulhas( NivelForca : Integer = 2) ; virtual ;
    Procedure LeituraMemoriaFiscal( DataInicial, DataFinal : TDateTime;
       Simplificada : Boolean = False ) ; overload ; virtual ;
    Procedure LeituraMemoriaFiscal( ReducaoInicial, ReducaoFinal : Integer;
       Simplificada : Boolean = False ); overload ; virtual ;
    Procedure LeituraMemoriaFiscalSerial( DataInicial, DataFinal : TDateTime;
       Linhas : TStringList; Simplificada : Boolean = False ) ;
       overload ; virtual ;
    Procedure LeituraMemoriaFiscalSerial( ReducaoInicial, ReducaoFinal: Integer;
       Linhas : TStringList; Simplificada : Boolean = False ) ;
       overload ; virtual ;

    Procedure LeituraMFDSerial( DataInicial, DataFinal : TDateTime;
       Linhas : TStringList; Documentos : TACBrECFTipoDocumentoSet = [docTodos] ) ; overload ; virtual ;
    Procedure LeituraMFDSerial( COOInicial, COOFinal : Integer;
       Linhas : TStringList; Documentos : TACBrECFTipoDocumentoSet = [docTodos] ) ; overload ; virtual ;

    Procedure EspelhoMFD_DLL( DataInicial, DataFinal : TDateTime;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; overload ; virtual ;
    Procedure EspelhoMFD_DLL( COOInicial, COOFinal : Integer;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos]  ) ; overload ; virtual ;
    Procedure ArquivoMFD_DLL( DataInicial, DataFinal : TDateTime;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos];
       Finalidade: TACBrECFFinalizaArqMFD = finMFD  ) ; overload ; virtual ;
    Procedure ArquivoMFD_DLL( ContInicial, ContFinal : Integer;
       const NomeArquivo : AnsiString; Documentos : TACBrECFTipoDocumentoSet = [docTodos];
       Finalidade: TACBrECFFinalizaArqMFD = finMFD;
       TipoContador: TACBrECFTipoContador = tpcCOO ) ; overload ; virtual ;

    Procedure ArquivoMF_Binario_DLL(const NomeArquivo: AnsiString); virtual;
    Procedure ArquivoMFD_Binario_DLL(Tipo:TACBrECFTipoDownloadMFD; const NomeArquivo: AnsiString;
       StrInicial, StrFinal: AnsiString); virtual;

    procedure PafMF_GerarCAT52(const DataInicial, DataFinal: TDateTime;
      const DirArquivos: String; NumeroSerie: String = ''); virtual;

    Procedure IdentificaOperador(Nome : String); virtual;
    Procedure IdentificaPAF( NomeVersao, MD5 : String) ; virtual ;
    Function RetornaInfoECF( Registrador: String) : AnsiString; Virtual ;

    { Retorna a Resposta do ECF }
    Function EnviaComando( const cmd : AnsiString = '') : AnsiString ; overload ;
    { Versao que Permite mudar o TimeOut padrao }
    Function EnviaComando( const cmd : AnsiString; lTimeOut : Integer): AnsiString; overload ;
    { Versao que Permite mudar o TimeOut padrao e o TempoInicioMsg }
    Function EnviaComando( const cmd : AnsiString; lTimeOut, lTempoInicioMsg : Integer):
       AnsiString; overload ;

    { Gera erro se nao puder abrir Cupom, informando o motivo }
    Function TestaPodeAbrirCupom : Boolean ; virtual ;

    { Obs: De/Codifica e Verifica Textos C-->  Codifica D--> Decodifica V--> Verifica }
    function DecodificaTexto(Operacao: Char; Texto: String; var Resposta: String): Boolean; virtual;

    property PathDLL: string read GetPathDLL write fsPathDLL;

    procedure ProgramarBitmapPromocional(const AIndice: Integer;
      const APathArquivo: AnsiString;
      const AAlinhamento: TACBrAlinhamento = alCentro); virtual;

    function ConfigBarras: TACBrECFConfigBarras;

    function TraduzirTag(const ATag: AnsiString): AnsiString; virtual;
    function TraduzirTagBloco(const ATag, Conteudo: AnsiString): AnsiString; virtual;
    function PossuiTagCodBarra(const ATexto: String): Boolean; virtual;
    function CodificarPaginaDeCodigoECF(ATexto: String): AnsiString; virtual;
    function DecodificarPaginaDeCodigoECF(ATexto: AnsiString): String; virtual;
end ;

implementation
Uses ACBrECF, ACBrECFVirtual,
     ACBrUtil.Strings,
     ACBrUtil.FilesIO,
     ACBrUtil.DateTime,
     ACBrUtil.Base,
     ACBrUtil.Math,
     ACBrUtil.Compatibilidade,
     Math,
     {$IFDEF COMPILER6_UP} DateUtils, StrUtils {$ELSE} ACBrD5, Windows {$ENDIF},
     TypInfo ;

{ TACBrECFTotalizadorNaoTributado }

constructor TACBrECFTotalizadorNaoTributado.create;
begin
  fsIndice := ''  ;
  fsTipo   := 'T' ;
  fsTotal  := 0 ;
end;

function TACBrECFTotalizadorNaoTributado.GetAsString: String;
begin
  Result := Indice + '|' +
            Tipo   + '|' +
            FloatToStr( RoundTo(Total, -2) ) + '|' ;
end;

procedure TACBrECFTotalizadorNaoTributado.SetAsString(const AValue: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := StringReplace(AValue,'|',sLineBreak,[rfReplaceAll]);

    if SL.Count < 3 then exit ;

    Indice := SL[0];
    Tipo   := SL[1][1];
    Total  := StrToFloat( SL[2] );
  finally
    SL.Free;
  end;
end;

procedure TACBrECFTotalizadorNaoTributado.SetTipo(const AValue: Char);
begin
  if not CharInSet(AValue , ['T','S']) then
    raise EACBrECFErro.create( ACBrStr(cACBrECFAliquotaSetTipoException));

  fsTipo := AValue;
end;

procedure TACBrECFTotalizadorNaoTributado.Assign(
  ATotalizadorNaoTributado: TACBrECFTotalizadorNaoTributado);
begin
  fsIndice := ATotalizadorNaoTributado.Indice ;
  fsTipo   := ATotalizadorNaoTributado.Tipo ;
  fsTotal  := ATotalizadorNaoTributado.Total ;
end;

{ TACBrECFTotalizadoresNaoTributados }

procedure TACBrECFTotalizadoresNaoTributados.SetObject(Index: Integer;
  Item: TACBrECFTotalizadorNaoTributado);
begin
  inherited Items[Index] := Item;
end;

function TACBrECFTotalizadoresNaoTributados.GetObject(Index: Integer
  ): TACBrECFTotalizadorNaoTributado;
begin
  Result := TACBrECFTotalizadorNaoTributado(inherited Items[Index]);
end;

procedure TACBrECFTotalizadoresNaoTributados.Insert(Index: Integer;
  Obj: TACBrECFTotalizadorNaoTributado);
begin
  inherited Insert(Index, Obj);
end;

function TACBrECFTotalizadoresNaoTributados.New: TACBrECFTotalizadorNaoTributado;
begin
  Result := TACBrECFTotalizadorNaoTributado.create;
  Add(Result);
end;

function TACBrECFTotalizadoresNaoTributados.Add(
  Obj: TACBrECFTotalizadorNaoTributado): Integer;
begin
  Result := inherited Add(Obj) ;
end;

{ ---------------------------- TACBrECFAliquotas -------------------------- }

{ TACBrECFAliquota }
constructor TACBrECFAliquota.create;
begin
  fsSequencia := 0 ;
  fsIndice    := ''  ;
  fsAliquota  := 0   ;
  fsTipo      := 'T' ;
  fsTotal     := 0 ;
end;

procedure TACBrECFAliquota.Assign(AAliquota: TACBrECFAliquota);
begin
  fsSequencia := AAliquota.Sequencia ;
  fsIndice    := AAliquota.Indice ;
  fsAliquota  := AAliquota.Aliquota ;
  fsTipo      := AAliquota.Tipo ;
  fsTotal     := AAliquota.Total ;
end;

procedure TACBrECFAliquota.SetTipo(const AValue: Char);
Var
  NewVar : Char ;
begin
  NewVar := UpCase(AValue) ;
  if NewVar = ' ' then
     NewVar := 'T' ;

  if not CharInSet(NewVar , ['T','S']) then
     raise EACBrECFErro.create( ACBrStr(cACBrECFAliquotaSetTipoException));

  fsTipo := NewVar;
end;

function TACBrECFAliquota.GetAsString: String;
begin
  Result := IntToStr( Sequencia )   + '|' +
            Indice                  + '|' +
            FloatToStr( Aliquota )  + '|' +
            Tipo                    + '|' +
            FloatToStr( RoundTo(Total, -2) )+ '|' ;
end;

procedure TACBrECFAliquota.SetAsString(const AValue: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := StringReplace(AValue,'|',sLineBreak,[rfReplaceAll]);

    if SL.Count < 5 then exit ;

    Sequencia := StrToInt( SL[0] );
    Indice    := SL[1];
    Aliquota  := StrToFloat( SL[2] );
    Tipo      := SL[3][1];
    Total     := StrToFloat( SL[4] );
  finally
    SL.Free;
  end;
end;

function TACBrECFAliquotas.Add(Obj: TACBrECFAliquota): Integer;
begin
  Result := inherited Add(Obj) ;
  Obj.Sequencia := Count ;
end;

function TACBrECFAliquotas.GetObject(Index: Integer): TACBrECFAliquota;
begin
  Result := TACBrECFAliquota(inherited Items[Index]);
end;

procedure TACBrECFAliquotas.Insert(Index: Integer; Obj: TACBrECFAliquota);
begin
  inherited Insert(Index, Obj);
end;

function TACBrECFAliquotas.New: TACBrECFAliquota;
begin
  Result := TACBrECFAliquota.create;
  Add(Result);
end;

procedure TACBrECFAliquotas.SetObject(Index: Integer; Item: TACBrECFAliquota);
begin
  inherited Items[Index] := Item;
end;

{ --------------------------- TACBrECFFormasPagamento ---------------------- }

// método de comparação utilizado para ordenar a lista por data
function CompararCamposOrdenacao(const FormaPagto1,
  FormaPagto2: TACBrECFFormaPagamento): Integer;
var
  sCampo1, sCampo2: String;
begin
  sCampo1 := FormatDateTime('YYYYMMDD', FormaPagto1.Data) + Trim(FormaPagto1.Descricao);
  sCampo2 := FormatDateTime('YYYYMMDD', FormaPagto2.Data) + Trim(FormaPagto2.Descricao);

  Result := AnsiCompareText(sCampo1, sCampo2);
end;

function TACBrECFFormaPagamento.GetAsString: String;
begin
  Result := Indice                           + '|' +
            Descricao                        + '|' +
            BoolToStr(PermiteVinculado)      + '|' +
            FloatToStr( RoundTo(Total, -2) ) + '|' +
            DateTimeToStr( Data )            + '|' +
            TipoDoc                          + '|' ;
end;

procedure TACBrECFFormaPagamento.SetAsString(const AValue: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := StringReplace(AValue,'|',sLineBreak,[rfReplaceAll]);

    if SL.Count < 6 then exit ;

    Indice           := SL[0];
    Descricao        := SL[1];
    PermiteVinculado := StrToBool( SL[2] );
    Total            := StrToFloat( SL[3] );
    Data             := StrToDateTime( SL[4] );
    TipoDoc          := SL[5];
  finally
    SL.Free;
  end;
end;

{ TACBrECFFormaPagamento }
constructor TACBrECFFormaPagamento.create;
begin
  fsIndice           := '' ;
  fsDescricao        := '' ;
  fsPermiteVinculado := true ;
  fsTotal            := 0.00 ;
  fsData             := 0.00;
end;

procedure TACBrECFFormaPagamento.Assign(
  AFormaPagamento: TACBrECFFormaPagamento);
begin
  fsIndice           := AFormaPagamento.Indice ;
  fsDescricao        := AFormaPagamento.Descricao ;
  fsPermiteVinculado := AFormaPagamento.PermiteVinculado ;
  fsTotal            := AFormaPagamento.Total ;
  fsData             := AFormaPagamento.Data ;
end;

function TACBrECFFormasPagamento.Add(Obj: TACBrECFFormaPagamento): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TACBrECFFormasPagamento.GetObject( Index: Integer):
   TACBrECFFormaPagamento;
begin
  Result := TACBrECFFormaPagamento(inherited Items[Index]);
end;

procedure TACBrECFFormasPagamento.Insert(Index: Integer;
  Obj: TACBrECFFormaPagamento);
begin
  inherited Insert(Index, Obj);
end;

function TACBrECFFormasPagamento.New: TACBrECFFormaPagamento;
begin
  Result := TACBrECFFormaPagamento.Create;
  Add(Result);
end;

procedure TACBrECFFormasPagamento.Ordenar;
begin
  {$IfDef HAS_SYSTEM_GENERICS}
  Self.Sort( TComparer<TACBrECFFormaPagamento>.Construct( CompararCamposOrdenacao ) );
  {$Else}
  Self.Sort(@CompararCamposOrdenacao);
  {$EndIf}
end;

procedure TACBrECFFormasPagamento.SetObject(Index: Integer;
  Item: TACBrECFFormaPagamento);
begin
  inherited Items[Index] := Item;
end;

{ ----------------------------- TACBrECFConsumidor -------------------------- }
constructor TACBrECFConsumidor.create;
begin
   Zera ;
end;

procedure TACBrECFConsumidor.Zera ;
begin
  fsNome      := '' ;
  fsEndereco  := '' ;
  fsDocumento := '' ;
  fsEnviado   := False ;
end ;

procedure TACBrECFConsumidor.AtribuiConsumidor(CPF_CNPJ, Nome, Endereco: String);
begin
  CPF_CNPJ  := Trim( CPF_CNPJ );
  Nome      := Trim( Nome );
  Endereco  := Trim( Endereco );

  if CPF_CNPJ = '' then
     raise EACBrECFErro.Create( ACBrStr(cACBrECFConsumidorCPFCNPJException)) ;

  if (Nome = '') and (Endereco <> '') then
     raise EACBrECFErro.Create( ACBrStr(cACBrECFConsumidorNomeException) ) ;

  fsDocumento := CPF_CNPJ ;
  fsNome      := Nome ;
  fsEndereco  := Endereco ;
  fsEnviado   := False ;
end;

function TACBrECFConsumidor.GetEnviado: Boolean;
begin
  Result := fsEnviado or (not Atribuido) ;
end;

function TACBrECFConsumidor.GetAtribuido: Boolean;
begin
  Result := (fsNome <> '') or (fsEndereco <> '') or (fsDocumento <> '') ;
end;


{------------------------- TACBrECFRelatoriosGerenciais -----------------------}

constructor TACBrECFRelatorioGerencial.create;
begin
  fsIndice    := '' ;
  fsDescricao := '' ;
  fsContador  := 0 ;
end;

procedure TACBrECFRelatorioGerencial.Assign(
  ARelatorioGerencial: TACBrECFRelatorioGerencial);
begin
  fsIndice    := ARelatorioGerencial.Indice ;
  fsDescricao := ARelatorioGerencial.Descricao ;
  fsContador  := ARelatorioGerencial.Contador ;
end;

function TACBrECFRelatorioGerencial.GetAsString: String;
begin
  Result := Indice               + '|' +
            Descricao            + '|' +
            IntToStr( Contador ) ;
end;

procedure TACBrECFRelatorioGerencial.SetAsString(const AValue: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := StringReplace(AValue,'|',sLineBreak,[rfReplaceAll]);

    if SL.Count < 3 then exit ;

    Indice           := SL[0];
    Descricao        := SL[1];
    Contador         := StrToInt( SL[2] );
  finally
    SL.Free;
  end;
end;



function TACBrECFRelatoriosGerenciais.Add(
  Obj: TACBrECFRelatorioGerencial): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TACBrECFRelatoriosGerenciais.GetObject(
  Index: Integer): TACBrECFRelatorioGerencial;
begin
  Result := TACBrECFRelatorioGerencial(inherited Items[Index]);
end;

procedure TACBrECFRelatoriosGerenciais.Insert(Index: Integer;
  Obj: TACBrECFRelatorioGerencial);
begin
  inherited Insert(Index, Obj);
end;

procedure TACBrECFRelatoriosGerenciais.SetObject(Index: Integer;
  Item: TACBrECFRelatorioGerencial);
begin
  inherited Items[Index] := Item;
end;


{ ---------------------- TACBrECFComprovantesNaoFiscais --------------------- }

function TACBrECFComprovanteNaoFiscal.GetAsString: String;
begin
  Result := Indice                           + '|' +
            Descricao                        + '|' +
            BoolToStr( PermiteVinculado )    + '|' +
            FormaPagamento                   + '|' +
            FloatToStr( RoundTo(Total, -2) ) + '|' +
            IntToStr( Contador )             + '|' ;
end;

procedure TACBrECFComprovanteNaoFiscal.SetAsString(const AValue: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := StringReplace(AValue,'|',sLineBreak,[rfReplaceAll]);

    if SL.Count < 6 then exit ;

    Indice           := SL[0];
    Descricao        := SL[1];
    PermiteVinculado := StrToBool( SL[2] );
    FormaPagamento   := SL[3];
    Total            := StrToFloat( SL[4] );
    Contador         := StrToInt( SL[5] );
  finally
    SL.Free;
  end;
end;

{ TACBrECFComprovanteNaoFiscal }
constructor TACBrECFComprovanteNaoFiscal.create;
begin
  fsIndice           := '' ;
  fsDescricao        := '' ;
  fsPermiteVinculado := true ;
  fsFormaPagamento   := '' ;
  fsTotal            := 0 ;
  fsContador         := 0 ;
end;

procedure TACBrECFComprovanteNaoFiscal.Assign(
  AComprovanteNaoFiscal: TACBrECFComprovanteNaoFiscal);
begin
  fsIndice           := AComprovanteNaoFiscal.Indice ;
  fsDescricao        := AComprovanteNaoFiscal.Descricao ;
  fsPermiteVinculado := AComprovanteNaoFiscal.PermiteVinculado ;
  fsFormaPagamento   := AComprovanteNaoFiscal.FormaPagamento ;
  fsTotal            := AComprovanteNaoFiscal.Total ;
  fsContador         := AComprovanteNaoFiscal.Contador ;
end;

function TACBrECFComprovantesNaoFiscais.Add(
  Obj: TACBrECFComprovanteNaoFiscal): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TACBrECFComprovantesNaoFiscais.GetObject(
  Index: Integer): TACBrECFComprovanteNaoFiscal;
begin
  Result := TACBrECFComprovanteNaoFiscal(inherited Items[Index]);
end;

procedure TACBrECFComprovantesNaoFiscais.Insert(Index: Integer;
  Obj: TACBrECFComprovanteNaoFiscal);
begin
  inherited Insert(Index, Obj);
end;

procedure TACBrECFComprovantesNaoFiscais.SetObject(Index: Integer;
  Item: TACBrECFComprovanteNaoFiscal);
begin
  inherited Items[Index] := Item;
end;

{-------------------------- TACBrECFUnidadesMedida ---------------------------}
Constructor TACBrECFUnidadeMedida.create;
begin
  fsIndice           := '' ;
  fsDescricao        := '' ;
end;

function TACBrECFUnidadesMedida.Add(Obj: TACBrECFUnidadeMedida): Integer;
begin
  Result := inherited Add(Obj) ;
end;

function TACBrECFUnidadesMedida.GetObject(
  Index: Integer): TACBrECFUnidadeMedida;
begin
  Result := TACBrECFUnidadeMedida(inherited Items[Index]);
end;

procedure TACBrECFUnidadesMedida.Insert(Index: Integer;
  Obj: TACBrECFUnidadeMedida);
begin
  inherited Insert(Index, Obj);
end;

procedure TACBrECFUnidadesMedida.SetObject(Index: Integer;
  Item: TACBrECFUnidadeMedida);
begin
  inherited Items[Index] := Item;
end;

{ ---------------------------- TACBrECFClass ------------------------------ }

constructor TACBrECFClass.create( AOwner : TComponent ) ;
begin
  if (AOwner is TACBrECF) then
   begin
     { Criando ponteiro para as propriedade de FormMsg ficarem visiveis nessa Unit }
     with (AOwner as TACBrECF) do
     begin
        { Criando ponteiro interno para as Propriedade SERIAL e FORMMSG de ACBrECF,
          para permitir as Classes Filhas o acesso a essas propriedades do Componente}
        fpDevice := Device ;
        fpDevice.SetDefaultValues ;
     end ;
   end
  else if(AOwner is TACBrECFVirtual) then
     fpDevice := nil
  else
     raise EACBrECFErro.create( ACBrStr(cACBrECFClassCreateException) );

  fpOwner := AOwner ;

  { Ajustando variaveis Internas }
  fsRetentar             := True ;
  fsControlePorta        := False ;
  fsOperador             := '' ;
  fsMsgAguarde           := cACBrMsgAguardando ;
  fsMsgTrabalhando       := cACBrMsgTrabalhando ;
  fsMsgRelatorio         := cACBrMsgRelatorio ;
  fsPausaRelatorio       := cACBrPausaRelatorio ;
  fsLinhasEntreCupons    := cACBrLinhasEntreCupons ;
  fsMaxLinhasBuffer      := cACBrMaxLinhasBuffer ;
  fsMsgPausaRelatorio    := cACBrMsgPausaRelatorio ;
  fsTempoInicioMsg       := cACBrTempoInicioMsg ;
  fsIntervaloAposComando := cACBrIntervaloAposComando ;
  fsExibeMensagem        := true ;
  fsBloqueiaMouseTeclado := true ;
  fsMsgPoucoPapel        := cACBrMsgPoucoPapel ;
  fsDescricaoGrande      := false ;
  fsVias                 := 0 ;
  fsRelatorio            := nil ;
  fsBytesRec             := 0 ;
  fsOnAguardandoRespostaChange := nil ;
  fsOnMsgPoucoPapel            := nil ;
  fsAguardandoResposta         := false ;
  fsOnMsgErro                  := nil ;
  fsOnMsgAguarde               := nil ;
  fsOnMsgRetentar              := nil ;
  {$IFNDEF NOGUI}
   fsUsandoBlockInput          := False ;
  {$ENDIF}

  { Variaveis Protected fp___ acessiveis pelas Classes filhas }
  fpAtivo                 := false ;
  fpEstado                := estNaoInicializada ;
  fpPaginaDeCodigo        := 0 ;
  fpColunas               := 48 ;
  fpNumMaxLinhasRodape    := 8 ;
  fpRFDID                 := '' ;
  fpModeloStr             := 'Não Definido' ;
  fpComandoEnviado        := '' ;
  fpRespostaComando       := '' ;
  fpUltimaMsgPoucoPapel   := 0 ;
  fpArredondaPorQtd       := False ;
  fpArredondaItemMFD      := False ;
  fpIgnorarErroSemPapel   := False ;
  fpIgnorarTagsFormatacao := False ;
  fpDecimaisPreco         := 3 ;
  fpDecimaisQtd           := 3 ;
  fpAliquotas             := nil ;
  fpTotalizadoresNaoTributados := nil;
  fpFormasPagamentos      := nil ;
  fpRelatoriosGerenciais  := nil ;
  fpComprovantesNaoFiscais:= nil ;
  fpTermica               := False ;
  fpMFD                   := False ;
  fpIdentificaConsumidorRodape := False ;
  fpModoPreVenda          := False;
  fpArqLOG                := '' ;
  fpComandoLOG            := '' ;
  fpOnGravarLog           := nil ;

  fpDadosReducaoZClass    := TACBrECFDadosRZ.Create ;
  fpConsumidor            := TACBrECFConsumidor.create ;
  fpInfoRodapeCupom       := TACBrECFRodape.Create;
  fpRespostasComando      := TACBrInformacoes.Create;

  {$IFNDEF NOGUI}
    fsFormMsg                   := nil ;
    fsFormMsgProcedureAExecutar := nil ;
    fsFormMsgTeclaParaFechar    := 0 ;
    fsFormMsgEstado             := fmsNenhum ;
    fsFormMsgControla           := true ;
  {$ENDIF}
end;

destructor TACBrECFClass.Destroy;
begin
  Desativar ;
  fpDevice := nil ; { Apenas remove referencia (ponteiros internos) }

  if Assigned( fpAliquotas ) then
     fpAliquotas.Free ;

  if Assigned( fpTotalizadoresNaoTributados ) then
     fpTotalizadoresNaoTributados.Free ;

  if Assigned( fpFormasPagamentos ) then
     fpFormasPagamentos.Free ;

  if Assigned( fpRelatoriosGerenciais ) then
     fpRelatoriosGerenciais.Free ;

  if Assigned( fpComprovantesNaoFiscais ) then
     fpComprovantesNaoFiscais.Free ;

  if Assigned( fpUnidadesMedida ) then
     fpUnidadesMedida.Free;

  fpDadosReducaoZClass.Free;
  fpConsumidor.Free ;
  fpInfoRodapeCupom.Free ;
  fpRespostasComando.Free ;

  {$IFNDEF NOGUI}
    if Assigned( fsFormMsg ) then
       FreeAndNil( fsFormMsg ) ;
  {$ENDIF}

  inherited Destroy ;
end;

procedure TACBrECFClass.SetAtivo(const Value: Boolean);
begin
  if Value then
     Ativar
  else
     Desativar ;
end;

procedure TACBrECFClass.SetDecimaisPreco(AValue: Integer);
begin
  fpDecimaisPreco := AValue;
end;

procedure TACBrECFClass.SetDecimaisQtd(AValue: Integer);
begin
  fpDecimaisQtd := AValue;
end;

function TACBrECFClass.GetPathDLL : string ;
begin
  Result := PathWithDelim(fsPathDLL);
end;

procedure TACBrECFClass.AtivarPorta;
begin
  if not Assigned(fpDevice) then exit;

  if not fpDevice.Ativo then
  begin
     GravaLog('   Ativando a porta: ' + fpDevice.Porta);
     fpDevice.Ativar;
  end;
end;

procedure TACBrECFClass.DesativarPorta;
begin
  if not Assigned(fpDevice) then exit;

  GravaLog('   Desativando a porta: ' + fpDevice.Porta);
  fpDevice.Desativar;
end;

function TACBrECFClass.GetNumMaxLinhasRodape: Integer;
begin
  Result := fpNumMaxLinhasRodape;
end;

function TACBrECFClass.GetDataHoraUltimaReducaoZ : TDateTime ;
begin
  Result := 0;
end;

procedure TACBrECFClass.Ativar;
begin
  if fpAtivo then exit ;

  if Assigned(fpDevice) then
  begin
     GravaLog( sLineBreak +
               StringOfChar('-',80)+ sLineBreak +
               'ATIVAR - '+FormatDateTime('dd/mm/yy hh:nn:ss:zzz',now)+
               ' - Modelo: '+ModeloStr+
               ' - Porta: '+fpDevice.Porta+
               ' - TimeOut: '+IntToStr(TimeOut)+ sLineBreak +
               '         Device: '+fpDevice.DeviceToString(False) + sLineBreak +
               StringOfChar('-',80) + sLineBreak );

     fpDevice.Ativar ;
  end;

  fpEstado := estDesconhecido ;
  fpAtivo  := true ;
end;

procedure TACBrECFClass.Desativar;
begin
  AguardandoResposta := False;
  AguardaImpressao   := False;

  fpEstado := estNaoInicializada ;

  if not fpAtivo then exit ;

  if Assigned(fpDevice) then
     fpDevice.Desativar ;

  fpAtivo := false ;
end;


{------------------------------------------------------------------------------}
function TACBrECFClass.EnviaComando(const cmd: AnsiString; lTimeOut: Integer): AnsiString;
Var wTimeOut : Integer ;
begin
  wTimeOut := TimeOut ;                      { Salvando os valores antigos }
  TimeOut  := max(lTimeOut,wTimeOut) ;       { Novo Valor recebido pelo metodo }

  try
     result := EnviaComando( cmd ) ;
  finally
     TimeOut := wTimeOut ;     { Restaurando valor antigo }
  end ;
end;

function TACBrECFClass.EnviaComando(const cmd: AnsiString; lTimeOut,
  lTempoInicioMsg: Integer): AnsiString;
Var wTimeOut, wTempoInicioMsg : Integer ;
begin
  wTimeOut        := TimeOut ;          { Salvando os valores antigos }
  wTempoInicioMsg := TempoInicioMsg ;

  TimeOut         := max(lTimeOut,wTimeOut) ;   { Novos Valores recebidos pelo metodo }
  TempoInicioMsg  := max(lTempoInicioMsg,wTempoInicioMsg) ;

  try
     result := EnviaComando( cmd ) ;
  finally
     TimeOut        := wTimeOut ;      { Restaurando valores antigos }
     TempoInicioMsg := wTempoInicioMsg ;
  end ;
end;

function TACBrECFClass.EnviaComando(const cmd: AnsiString = ''): AnsiString;
begin
  try
    try
       AtivarPorta;
       GravaLog(fpComandoLOG,True );

       if Assigned(fpDevice) then
          if (not fpDevice.Ativo) then
             raise EACBrECFNaoInicializado.create( ACBrStr(cACBrECFNaoInicializadoException) );

       if AguardandoResposta then
          raise EACBrECFOcupado.create( ACBrStr(cACBrECFOcupadoException) ) ;

       VerificaEmLinha ;

       fsBytesRec         := 0 ;
       AguardandoResposta := True ;
       try
          Result := EnviaComando_ECF( Cmd ) ;
       finally
          AguardandoResposta  := False ;
          IgnorarErroSemPapel := False;
          GravaLog('    RX <- '+fpRespostaComando, True);

          if ControlePorta then
             DesativarPorta;
       end ;
    except
       On E: Exception do
       begin
          GravaLog(sLineBreak+
                   '----------------- ERRO -----------------' + sLineBreak +
                   ACBrStrToAnsi( E.Message ) + sLineBreak +
                   '----------------------------------------' + sLineBreak );
          raise ;
       end ;
    end ;
  finally
     fpComandoLOG := '' ;
  end ;
end;

procedure TACBrECFClass.GravaLog(AString : AnsiString ; Traduz : Boolean) ;
var
  Tratado: Boolean;
begin
  Tratado := False;

  if Traduz then
    AString := TranslateUnprintable(AString);

  if Assigned( fpOnGravarLog ) then
    fpOnGravarLog( AString, Tratado);

  if not Tratado then
    WriteLog(fpArqLOG, '-- '+FormatDateTime('dd/mm hh:nn:ss:zzz',now)+' '+ AString);
end ;

function TACBrECFClass.EnviaComando_ECF( cmd: AnsiString): AnsiString;
begin
  Result := '';
  ErroAbstract( 'EnviaComando_ECF' );
end;

{- LE RESPOSTA - Rotina de Leitura da Resposta do ECF com Bloqueio de Teclado -}

procedure TACBrECFClass.LeResposta;
begin
  {$IFNDEF NOGUI}
    if FormMsgExibe then
     begin
       {$IFDEF MSWINDOWS}
        LoadBlockInput;
        if (not ExibeMensagem) and Assigned( xBlockInput ) then
         begin
           BlockInput(True, False);
           try
              DoLeResposta ;
           finally
              BlockInput(False, True);
           end ;
         end
        else
       {$ENDIF}
          FormMsgDoProcedure( DoLeResposta, 0 ) ;
     end
    else
  {$ENDIF}
     DoLeResposta ;

  if (pos('ACBrErro:',fpRespostaComando) = 1)  then
  begin
     fpRespostaComando := copy( fpRespostaComando, 11, Length( fpRespostaComando ) );
     GravaLog( 'RespostaComando: '+fpRespostaComando, True );
     raise EACBrECFTimeOut.create( Format(ACBrStr(cACBrECFSemRespostaException), [ModeloStr]) ) ;
  end ;
end;

procedure TACBrECFClass.DoLeResposta;
Var Fim : Boolean ;
    TempoInicio, TempoLimite : TDateTime ;
    TempoRestante, LenResp : Integer ;
    Texto : AnsiString ;
    ProcessaFormMsg, FimLeitura : Boolean ;
begin
  if not Assigned(fpDevice) then exit ;

  {$IFNDEF FPC}
  {$IFNDEF DELPHIXE8_UP}
  // A linha abaixo remove Warning do Delphi (W1036 Variable 'Fim' might not have been initialized)
  // Isso é um bug do compilador Win32 presente pelo menos da versão Delphi 6 até a XE7 (http://stackoverflow.com/a/25905266/460775)
  Fim := True;
  {$ENDIF}
  {$ENDIF}
  try
     fpRespostaComando := '' ;
     {$IFNDEF NOGUI}
       ProcessaFormMsg := (Assigned( fsFormMsg ) and fsFormMsgControla) ;
     {$ENDIF}

     { Calcula Tempo Limite. Espera resposta até Tempo Limite. Se a resposta
       for Lida antes, já encerra. Se nao chegar até TempoLimite, gera erro.}
     TempoLimite := IncSecond( now, TimeOut) ;
     TempoInicio := IncSecond( now, TempoInicioMsg) ;
     FimLeitura := False ;

     { - Le até atingir a condiçao descrita na funçao VerificaFimLeitura que
         é particular de cada Classe Filha (override)
       - VerificaFimImpressao é necessário apenas nos ECFs que respondem
         antes do termino da impressao (Sweda, Bematech, Daruma) }
     repeat
        { Atualizando a Msg no Form }
        {$IFNDEF NOGUI}
          if ProcessaFormMsg and (now >= TempoInicio) then
          begin
             TempoRestante := SecondsBetween( now, TempoLimite) ;

             if (TimeOut - TempoRestante) > 1 then
              begin
                try
                   Texto := Format(MsgAguarde, [ TempoRestante ]) ;
                except
                   Texto := MsgAguarde ;
                end ;
              end
             else
                Texto := MsgTrabalhando ;

             FormMsgPinta( Texto );
          end ;
        {$ENDIF}

        if now > TempoLimite then       { TimeOut }
        begin
           if Retentar then
           begin
             {$IFNDEF NOGUI}
              if ProcessaFormMsg then
              begin
                 fsFormMsg.Width  := 0 ;  { Escondendo o Form da Msg }
                 fsFormMsg.Height := 0 ;
              end ;
              {$ENDIF}
              if DoOnMsgRetentar( Format(cACBrECFDoOnMsgSemRespostaRetentar,
                                        [ ModeloStr ]),
                  'LerResposta') then
                 TempoLimite := IncSecond( now, TimeOut)  ;
           end ;

           if now > TempoLimite then      { Respondeu Nao a Retentar }
           begin
              fpRespostaComando := 'ACBrErro: '+fpRespostaComando ;
              break ;
           end ;
        end ;

        Fim := True ;
        if not FimLeitura then
         begin
           Fim  := False ;
           try
              fpRespostaComando := fpRespostaComando + { Le conteudo da porta }
                                   fpDevice.LeString(100) ;

              LenResp := Length( fpRespostaComando ) ;
              if LenResp <> fsBytesRec then
              begin
                 // ECF está respondendo, portanto está trabalhando //
                 TempoLimite := IncSecond(now, TimeOut);
                 fsBytesRec  := LenResp ;
              end ;
           except
              sleep(10) ;
           end ;

           FimLeitura := VerificaFimLeitura(fpRespostaComando, TempoLimite) ;
         end
        else
           if AguardaImpressao then
           begin
              Fim := VerificaFimImpressao( TempoLimite ) ;

              if not Fim then
                 sleep(200) ;
           end ;

        {$IFNDEF NOGUI}
          if fpDevice.ProcessMessages then
	     Application.ProcessMessages;
        {$ENDIF}
     until Fim ;
  finally
     AguardaImpressao := False ;

     if Assigned( fsOnMsgAguarde ) then
        fsOnMsgAguarde( '' ) ;
  end ;
end;

{ Essa função PODE ser override por cada Classe Filha criada
 - Transmite a string do Comando.
   - Se conseguiu retorna True.
   - Se não conseguiu e a propriedade Retentar, estiver ligada retorna False,
   - Se Retentar estiver desligada ou respondeu NAO ao Retentar, dispara Excecao}
function TACBrECFClass.TransmiteComando(const Cmd: AnsiString): Boolean;
begin
  Result := True ;
  if not Assigned(fpDevice) then exit;

  try
     AtivarPorta;
     GravaLog('                TX -> '+Cmd, True);
     fpDevice.EnviaString( Cmd );   { Eviando o comando }
  except
     if not DoOnMsgRetentar(Format(cACBrECFCmdSemRespostaException, [ ModeloStr ]),
       'TransmitirComando') then
       raise EACBrECFSemResposta.create(Format(ACBrStr(cACBrECFEnviaCmdSemRespostaException), [ ModeloStr ]))
     else
        Result := False ;
  end ;
end;


{ Essa função DEVE ser override por cada Classe Filha criada }
function TACBrECFClass.VerificaFimLeitura(var Retorno: AnsiString;
   var TempoLimite: TDateTime) : Boolean ;
begin
{$IFDEF FPC}
  Result := False;
{$ENDIF}
  raise EACBrECFErro.Create( ACBrStr(Format(cACBrECFVerificaFimLeituraException, [ ModeloStr ]))) ;
end;

function TACBrECFClass.VerificaFimImpressao(var TempoLimite: TDateTime): Boolean;
begin
{ Essa função PODE ser override por cada Classe Filha criada
  - Ela é necessária apenas para ECFs que respondem antes do termino da
    Impressao como a Sweda, Bematech, Daruma, etc.
  - Substitui a antiga função "EnviaComandoEspera"
  - Para usa-la ative a Propriedade "AguardarImpressao" entes de chamar
    "EnviaComando"
  - IMPORTANTE: Não é permitido o uso de chamadas EnviaComando dentra dessa
    função, caso constrário ela entrará em chamada Recursiva Infinita }

  Result := EmLinha(1) ;  //  Result := True ;
end;

procedure TACBrECFClass.VerificaEmLinha(TimeOut : Integer) ;
begin
  while not EmLinha( TimeOut ) do  { Impressora está em-linha ? }
  begin
     if Retentar and
        DoOnMsgRetentar(Format(cACBrECFVerificaEmLinhaMsgRetentar,
                        [ ModeloStr ]), 'VerEmLinha') then
        Continue ;

     raise EACBrECFSemResposta.create(Format(ACBrStr(cACBrECFVerificaEmLinhaException),
                                     [ ModeloStr ])) ;
  end ;
end;


function TACBrECFClass.GetTimeOut: Integer;
begin
  if Assigned(fpDevice) then
     Result := fpDevice.TimeOut
  else
     Result := 0;
end;

procedure TACBrECFClass.SetTimeOut(const Value: Integer);
begin
  if Assigned(fpDevice) then
     fpDevice.TimeOut := Value ;
end;

{ Essa função PODE ser override por cada Classe Filha criada }
function TACBrECFClass.TestaPodeAbrirCupom : Boolean ;
Var Msg : String ;
    Est : TACBrECFEstado ;
begin
  Result := true ;
  Est    := Estado ;

  case Est of
     estRequerX :
        Msg := Format(cACBrECFPodeAbrirCupomRequerX, [ ModeloStr ]);
     estRequerZ :
        Msg := cACBrECFPodeAbrirCupomRequerZ;
     estBloqueada :
        Msg := cACBrECFPodeAbrirCupomBloqueada;
     estVenda :
        Msg := cACBrECFPodeAbrirCupomCFAberto;
     estNaoInicializada :
        Msg := cACBrECFPodeAbrirCupomNaoAtivada;
     estLivre :
        Msg := '' ;
  else ;
     Msg := Format(cACBrECFPodeAbrirCupomEstado,
                     [ ModeloStr, GetEnumName(TypeInfo(TACBrECFEstado), integer(Est)) ]);
  end;

  if Msg <> '' then
  begin
     result := false ;
     GeraErro( EACBrECFCMDInvalido.Create( ACBrStr(Msg) ) );
  end ;
end;

procedure TACBrECFClass.SetAguardandoResposta(const Value: Boolean);
begin
  if Value = fsAguardandoResposta then exit ;

  fsAguardandoResposta := Value;
  if Assigned( fsOnAguardandoRespostaChange ) then
     fsOnAguardandoRespostaChange( self ) ;
end;

procedure TACBrECFClass.GeraErro( E : Exception ) ;
begin
  if Assigned( fsOnMsgErro ) then
     fsOnMsgErro( E )
  else
     raise E ;
end;

function TACBrECFClass.EmLinha( lTimeOut: Integer): Boolean;
begin
  if Assigned(fpDevice) then
     Result := fpDevice.EmLinha( lTimeOut )
  else
     Result := True;
end;

function TACBrECFClass.GetDataHora: TDateTime;
begin
  Result := now ;
end;

function TACBrECFClass.GetNumECF: String;
begin
  Result := '001' ;
end;

function TACBrECFClass.GetNumCRO: String;
begin
  Result := '001' ;
end;

function TACBrECFClass.GetNumCCF: String;
begin
  Result := '' ;
end;

function TACBrECFClass.GetNumGNF: String;
begin
  Result := '' ;
end;

function TACBrECFClass.GetNumGNFC: String;
begin
  Result := '' ;
end;

function TACBrECFClass.GetNumGRG: String;
begin
  Result := '' ;
end;

function TACBrECFClass.GetNumCDC: String;
begin
  Result := '' ;
end;

function TACBrECFClass.GetNumCFC: String;
begin
  Result := '' ;
end;

function TACBrECFClass.GetNumCFD: String;
begin
  Result := '' ;
end;

function TACBrECFClass.GetNumLoja: String;
begin
  Result := '001' ;
end;

function TACBrECFClass.GetNumNCN: String;
begin
  Result := '' ;
end;

function TACBrECFClass.GetNumCCDC: String;
begin
  Result := '' ;
end;

function TACBrECFClass.GetNumSerie: String;
begin
  Result := '' ;
end;

function TACBrECFClass.GetNumSerieMFD: String;
begin
  Result := '' ;
end;

{ Essa função DEVE ser override por cada Classe Filha criada }
procedure TACBrECFClass.AbreGaveta ;
begin
  GeraErro( EACBrECFCMDInvalido.Create( Format(cACBrECFAbreGavetaException, [ModeloStr] ))) ;
end;

{ Essa função DEVE ser override por cada Classe Filha criada }
function TACBrECFClass.GetEstado: TACBrECFEstado;
begin
  Result := fpEstado ;
end;

{ Essa função PODE ser override por cada Classe Filha criada }
function TACBrECFClass.GetGavetaAberta: Boolean;
begin
  Result := false ;
end;

{ Essa função DEVE ser override por cada Classe Filha criada }
function TACBrECFClass.GetPoucoPapel: Boolean;
begin
  Result := false ;
end;

{ Essa função DEVE ser override por cada Classe Filha criada }
function TACBrECFClass.GetArredonda: Boolean;
begin
  Result := false ;
end;

{ Essa função DEVE ser override por cada Classe Filha criada }
function TACBrECFClass.GetHorarioVerao: Boolean;
begin
  Result := false ;
end;

{ Essa função PODE ser override por cada Classe Filha criada }
procedure TACBrECFClass.ImpactoAgulhas( NivelForca : Integer = 2);
begin
  GeraErro( EACBrECFCMDInvalido.Create( Format(cACBrECFImpactoAgulhasException,
                                               [ ModeloStr ] ))) ;
end;

{ Essa função PODE ser override por cada Classe Filha criada }
procedure TACBrECFClass.LeituraMemoriaFiscal(ReducaoInicial,
   ReducaoFinal: Integer; Simplificada : Boolean);
begin
  ErroAbstract('LeituraMemoriaFiscal');
end;

procedure TACBrECFClass.LeituraMemoriaFiscal(DataInicial, DataFinal: TDateTime;
   Simplificada : Boolean);
begin
  ErroAbstract('LeituraMemoriaFiscal');
end;

procedure TACBrECFClass.LeituraMemoriaFiscalSerial(ReducaoInicial,
   ReducaoFinal: Integer; Linhas : TStringList; Simplificada : Boolean);
begin
  ErroAbstract('LeituraMemoriaFiscalSerial');
end;

procedure TACBrECFClass.LeituraMemoriaFiscalSerial(DataInicial,
   DataFinal: TDateTime; Linhas : TStringList; Simplificada : Boolean);
begin
  ErroAbstract('LeituraMemoriaFiscalSerial');
end;


procedure TACBrECFClass.LeituraMFDSerial(DataInicial, DataFinal: TDateTime;
  Linhas: TStringList; Documentos : TACBrECFTipoDocumentoSet );
begin
  ErroAbstract('LeituraMFDSerial');
end;

procedure TACBrECFClass.LeituraMFDSerial(COOInicial,
  COOFinal: Integer; Linhas: TStringList; Documentos : TACBrECFTipoDocumentoSet);
begin
  ErroAbstract('LeituraMFDSerial');
end;

procedure TACBrECFClass.EspelhoMFD_DLL(DataInicial,
  DataFinal: TDateTime; const NomeArquivo: AnsiString;
  Documentos: TACBrECFTipoDocumentoSet);
begin
  ErroAbstract('EspelhoMFD_DLL');
end;

procedure TACBrECFClass.EspelhoMFD_DLL(COOInicial, COOFinal: Integer;
  const NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet);
begin
  ErroAbstract('EspelhoMFD_DLL');
end;

procedure TACBrECFClass.ArquivoMFD_DLL(DataInicial, DataFinal: TDateTime;
  const NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet;
  Finalidade: TACBrECFFinalizaArqMFD);
begin
  ErroAbstract('ArquivoMFD_DLL');
end;

procedure TACBrECFClass.ArquivoMFD_DLL(ContInicial, ContFinal: Integer;
  const NomeArquivo: AnsiString; Documentos: TACBrECFTipoDocumentoSet;
  Finalidade: TACBrECFFinalizaArqMFD; TipoContador: TACBrECFTipoContador);
begin
  ErroAbstract('ArquivoMFD_DLL');
end;

procedure TACBrECFClass.ArquivoMF_Binario_DLL(const NomeArquivo: AnsiString);
begin
  ErroAbstract('ArquivoMF_Binario_DLL');
end;

procedure TACBrECFClass.ArquivoMFD_Binario_DLL(Tipo: TACBrECFTipoDownloadMFD;
  const NomeArquivo: AnsiString; StrInicial, StrFinal: AnsiString);
begin
  ErroAbstract('ArquivoMFD_Binario_DLL');
end;


{ Essa função PODE ser override por cada Classe Filha criada }
procedure TACBrECFClass.ImprimeCheque(Banco: String; Valor: Double; Favorecido,
  Cidade: String; Data: TDateTime; Observacao: String);
begin
  GeraErro( EACBrECFCMDInvalido.Create( Format(cACBrECFImprimeChequeException,
                                               [ ModeloStr ] ))) ;
end;

{ Essa função PODE ser override por cada Classe Filha criada }
function TACBrECFClass.LeituraCMC7 : AnsiString ;
begin
  Result := '';
  GeraErro( EACBrECFCMDInvalido.Create( Format(cACBrECFLeituraCMC7Exception,
                                               [ ModeloStr ] ))) ;
end;

{ Essa função PODE ser override por cada Classe Filha criada }
procedure TACBrECFClass.CancelaImpressaoCheque;
begin
  ErroAbstract('CancelaImpressaoCheque');
end;

{ Essa função PODE ser override por cada Classe Filha criada }
function TACBrECFClass.GetChequePronto: Boolean;
begin
  result := True ;
end;

function TACBrECFClass.ConfigBarras: TACBrECFConfigBarras;
var
  ECF: TACBrECF;
begin
  ECF := GetECFComponente(Self);
  Result := ECF.ConfigBarras;
end;

procedure TACBrECFClass.CorrigeEstadoErro( Reducao: Boolean );
begin
  case Estado of
     estRequerX : LeituraX ;

     estRequerZ :
       if Reducao then
         try
           ReducaoZ(now);
         except
           try
             CancelaCupom;
             ReducaoZ(now);
           except
           end;
         end;

     estRelatorio : FechaRelatorio ;

     estVenda, estPagamento :  CancelaCupom ;

     estNaoFiscal : CancelaNaoFiscal ;

     estBloqueada : GeraErro( EACBrECFCMDInvalido.Create(
                               ACBrStr(cACBrECFPodeAbrirCupomBloqueada) ) );
  end;

  if Estado <> estLivre then
     try
        FechaNaoFiscal ;
     except
     end
  else
     exit ;

  if Estado <> estLivre then
     try
        CancelaNaoFiscal ;
     except
     end
  else
     exit ;

  if Estado <> estLivre then
     try
        FechaRelatorio ;
     except
     end ;
end;

procedure TACBrECFClass.AbreBilhetePassagem(Origem, Destino, Linha,
  Agencia: String; DataHora: TDateTime; Poltrona, Plataforma: String;
  Tipo: TACBrECFTipoBilhete; UFDestino, PassageiroRG, PassageiroNome,
  PassageiroEnd: String);
begin
  ErroAbstract('AbreBilhetePassagem');
end;

procedure TACBrECFClass.AbreCupom ;
begin
  ErroAbstract('AbreCupom');
end;

procedure TACBrECFClass.CancelaCupom(NumCOOCancelar: Integer);
begin
  ErroAbstract('CancelaCupom');
end;

procedure TACBrECFClass.CancelaItemVendido(NumItem: Integer);
begin
  ErroAbstract('CancelaItemVendido');
end;

procedure TACBrECFClass.EfetuaPagamento(CodFormaPagto : String ;
   Valor : Double ; Observacao : AnsiString ; ImprimeVinculado : Boolean ;
   CodMeioPagamento : Integer) ;
begin
  ErroAbstract('EfetuaPagamento');
end;

procedure TACBrECFClass.EstornaPagamento(const CodFormaPagtoEstornar,
  CodFormaPagtoEfetivar : String; const Valor: Double;
   Observacao : AnsiString = '') ;
begin
  ErroAbstract('EstornaPagamento');
end;

procedure TACBrECFClass.FechaCupom(Observacao: AnsiString; IndiceBMP : Integer);
begin
  ErroAbstract('FechaCupom');
end;

procedure TACBrECFClass.FechaRelatorio;
begin
  ErroAbstract('FechaRelatorio');
end;

procedure TACBrECFClass.AbreNaoFiscal(CPF_CNPJ : String ; Nome : String ;
   Endereco : String) ;
begin
  ErroAbstract('AbreNaoFiscal');
end;

procedure TACBrECFClass.CancelaNaoFiscal;
begin
  CancelaCupom ;
end;

procedure TACBrECFClass.CancelaItemNaoFiscal(const AItem : Integer) ;
begin
  ErroAbstract('AbreNaoFiscal');
end;

procedure TACBrECFClass.EfetuaPagamentoNaoFiscal(CodFormaPagto: String;
  Valor: Double; Observacao: AnsiString; ImprimeVinculado: Boolean);
begin
  EfetuaPagamento( CodFormaPagto, Valor, Observacao, ImprimeVinculado );
end;

procedure TACBrECFClass.FechaNaoFiscal(Observacao: AnsiString; IndiceBMP : Integer);
begin
  FechaCupom(Observacao, IndiceBMP);
end;

procedure TACBrECFClass.NaoFiscalCompleto(CodCNF: String; Valor: Double;
  CodFormaPagto: String; Obs: AnsiString; IndiceBMP : Integer);
var
  ECF: TACBrECF;
begin
  { Chama rotinas da classe Pai para atualizar os Memos }
  ECF := GetECFComponente(Self);

  with ECF do
  begin
     AbreNaoFiscal ;
     try
        RegistraItemNaoFiscal(CodCNF, Valor);
        SubtotalizaNaoFiscal(0);
        EfetuaPagamentoNaoFiscal(CodFormaPagto, Valor );
        FechaNaoFiscal( Obs, IndiceBMP);
     except
        try
           CancelaNaoFiscal
        except
        end;

        raise ;
     end ;
  end ;
end;

procedure TACBrECFClass.RegistraItemNaoFiscal(CodCNF: String;
  Valor: Double; Obs: AnsiString = '');
begin
  ErroAbstract('RegistraItemNaoFiscal');
end;

procedure TACBrECFClass.SubtotalizaNaoFiscal(DescontoAcrescimo: Double;
   MensagemRodape: AnsiString);
begin
  SubtotalizaCupom(DescontoAcrescimo, MensagemRodape);
end;

procedure TACBrECFClass.Sangria(const Valor : Double ; Obs : AnsiString ;
  DescricaoCNF : String ; DescricaoFPG : String; IndiceBMP: Integer) ;
Var
  CNF : TACBrECFComprovanteNaoFiscal ;
  FPG : TACBrECFFormaPagamento ;
begin
  CNF := AchaCNFDescricao(DescricaoCNF, True) ;
  if CNF = nil then
     raise EACBrECFErro.Create( ACBrStr(Format(cACBrECFAchaCNFException,
                                   [ DescricaoCNF ] ))) ;

  FPG := AchaFPGDescricao(DescricaoFPG, True) ;
  if FPG = nil then
     raise EACBrECFErro.Create( ACBrStr(Format(cACBrECFAchaFPGException,
                                   [ DescricaoFPG ]))) ;

  NaoFiscalCompleto( CNF.Indice, Valor, FPG.Indice, Obs);
end;

procedure TACBrECFClass.Suprimento(const Valor : Double ; Obs : AnsiString ;
  DescricaoCNF : String ; DescricaoFPG : String; IndiceBMP: Integer) ;
begin
  Sangria( Valor, Obs, DescricaoCNF, DescricaoFPG, IndiceBMP);
end;

function TACBrECFClass.EstornaCCD(const Todos : Boolean) : Integer ;
begin
  Result := 0;
  ErroAbstract('EstornaCCD');
end ;

procedure TACBrECFClass.PulaLinhas(NumLinhas: Integer);
begin
  if NumLinhas = 0 then
     NumLinhas := LinhasEntreCupons ;

  LinhaRelatorioGerencial( StringOfChar(#10, NumLinhas ) ) ;
end;

procedure TACBrECFClass.CortaPapel(const CorteParcial : Boolean ) ;
begin
  LinhaRelatorioGerencial( #10+#10+#10 ) ;
end;

function TACBrECFClass.GetNumCupom: String;
begin
  Result := '' ;
end;

function TACBrECFClass.GetNumVersao: String ;
begin
  Result := ''
end;

function TACBrECFClass.GetNumReducoesZRestantes: String;
var
  CRZR: String;
  I: Integer;
  LeituraXsl: TStringList;
  Linha: String;
begin
  // implementada a leitura do contador pela leitura X para suprir
  // as impressoras que não possuem retorno do contador
  // nas impressoras que possuem, sobrescrever o método e utilizar o
  // comando apropriado
  CRZR := '';
  LeituraXsl := TStringList.Create;
  try
    LeituraXSerial(LeituraXsl);

    for I := LeituraXsl.Count - 1 downto 0 do
    begin
      Linha := AnsiUpperCase(LeituraXsl[I]);
      if pos('REDUÇÕES RESTANTES:', Linha) > 0 then
      begin
        CRZR := Trim(Copy(Linha, 30, 40));
        CRZR := ACBrUtil.Strings.OnlyNumber(CRZR);
        Break;
      end;
    end;
  finally
    LeituraXsl.Free;
  end;

  Result := Trim( CRZR ) ;
end;

function TACBrECFClass.GetSubTotal: Double;
begin
  Result := 0 ;
end;

function TACBrECFClass.GetTotalPago: Double;
begin
  Result := 0 ;
end;

function TACBrECFClass.GetCNPJ: String;
begin
  Result := '' ;
end;

function TACBrECFClass.GetIE: String;
begin
  Result := '' ;
end;

function TACBrECFClass.GetIM: String;
begin
  Result := '' ;
end;
function TACBrECFClass.GetCliche: AnsiString;
begin
  Result := '' ;
end;

function TACBrECFClass.GetUsuarioAtual: String;
begin
  Result := '' ;
end;

function TACBrECFClass.GetDataHoraSB: TDateTime;
Var
  Linha, LinhaVer, DtHrStr : AnsiString ;
  Linhas : TStringList;
  I, CRZ :Integer;
  AchouBlocoSB : Boolean ;
begin
  Result := 0;
  Linhas := TStringList.Create;
  try
    CRZ := StrToIntDef(NumCRZ, 1) ;
    LeituraMemoriaFiscalSerial(CRZ, CRZ, Linhas);

    I := 0 ;
    AchouBlocoSB := False;
    while (not AchouBlocoSB) and (I < Linhas.Count) do
    begin
       Linha := Linhas[I] ;
       AchouBlocoSB := (pos('SOFTWARE B', Linha ) > 0) ;
       Inc( I ) ;
    end ;

    Linha    := '';
    LinhaVer := '';
    while AchouBlocoSB and (I < Linhas.Count) and (Linha = LinhaVer) do
    begin
       Linha := Trim(Linhas[I]) ;
       if (Linha <> '') then
       begin
          if ( StrIsNumber( copy(Linha,1,2) ) and ( copy(Linha,3,1) = '.' ) and
               StrIsNumber( copy(Linha,4,2) ) and ( copy(Linha,6,1) = '.' ) and
               StrIsNumber( copy(Linha,7,2) ) ) then
             LinhaVer := Linha;
       end ;

       Inc( I ) ;
    end ;

    if LinhaVer <> '' then
    begin
      //EPSON: 01.00.01                    25/06/2009 21:07:40
      //FISCNET:03.00.00 09/10/2002   12:18:47
      //FISCNET:03.03.00 06/03/2006   10:57:23
      //BEMATECH: 01.00.01                    25/06/2009 21:07:40

      I := pos('/', LinhaVer ) ;
      DtHrStr := copy(LinhaVer, I-2, 10 ) ;
      I := pos(':', LinhaVer ) ;
      DtHrStr := DtHrStr + ' ' + copy(LinhaVer, I-2, 8 ) ;

      Result := StringToDateTime( DtHrStr, 'dd/mm/yyyy hh:nn:ss' ) ;
    end
  finally
    Linhas.Free ;
  end ;
end;

function TACBrECFClass.GetSubModeloECF: String;
begin
  Result := '' ;
end;

function TACBrECFClass.GetPAF: String;
begin
  Result := '' ;
end;

{ Essa função DEVE ser override por cada Classe Filha criada }
function TACBrECFClass.GetParamDescontoISSQN: Boolean;
begin
  Result := (Trim(IM) <> '') ;
end;

function TACBrECFClass.GetTipoUltimoDocumento : TACBrECFTipoDocumento ;
begin
   Result := docNenhum;
end ;

function TACBrECFClass.GetDataMovimento: TDateTime;
begin
  Result := now ;
end;

function TACBrECFClass.GetGrandeTotal: Double;
begin
  Result := 0 ;
end;

function TACBrECFClass.GetNumCRZ: String ;
begin
  Result := '00000'
end;

function TACBrECFClass.GetTotalAcrescimos: Double;
begin
  Result := 0 ;
end;

function TACBrECFClass.GetTotalCancelamentos: Double;
begin
  Result := 0 ;
end;

function TACBrECFClass.GetTotalDescontos: Double;
begin
  Result := 0 ;
end;

function TACBrECFClass.GetTotalTroco: Double;
begin
  Result := 0 ;
end;

function TACBrECFClass.GetTotalSubstituicaoTributaria: Double;
begin
  Result := 0 ;
end;

function TACBrECFClass.GetTotalIsencao: Double;
begin
  Result := 0 ;
end;

function TACBrECFClass.GetTotalNaoFiscal: Double;
  Var I : Integer ;
begin
  Result := 0 ;
  try
     LerTotaisComprovanteNaoFiscal ;

     For I := 0 to ComprovantesNaoFiscais.Count-1 do
        Result := Result + ComprovantesNaoFiscais[I].Total ;
  except
  end ;
end;

function TACBrECFClass.GetTotalAcrescimosISSQN: Double;
begin
  Result := 0;
end;

function TACBrECFClass.GetTotalCancelamentosISSQN: Double;
begin
  Result := 0;
end;

function TACBrECFClass.GetTotalCancelamentosEmAbertoISSQN: Double;
begin
  Result := 0;
end;

function TACBrECFClass.GetTotalCancelamentosEmAberto: Double;
begin
  Result := 0;
end;

function TACBrECFClass.GetTotalDescontosISSQN: Double;
begin
  Result := 0;
end;

function TACBrECFClass.GetTotalAcrescimosOPNF: Double;
begin
  Result := 0;
end;

function TACBrECFClass.GetTotalCancelamentosOPNF: Double;
begin
  Result := 0;
end;

function TACBrECFClass.GetTotalDescontosOPNF: Double;
begin
  Result := 0;
end;

function TACBrECFClass.GetTotalIsencaoISSQN: Double;
begin
  Result := 0;
end;

function TACBrECFClass.GetTotalNaoTributadoISSQN: Double;
begin
  Result := 0;
end;

function TACBrECFClass.GetTotalSubstituicaoTributariaISSQN: Double;
begin
  Result := 0;
end;

function TACBrECFClass.GetNumUltimoItem: Integer;
begin
  Result := 0 ;
end;

function TACBrECFClass.GetTotalNaoTributado: Double;
begin
  Result := 0 ;
end;

function TACBrECFClass.GetVendaBruta: Double;
begin
  Result := 0 ;
end;

function TACBrECFClass.GetNumCOOInicial: String;
begin
  Result := ''
end;

function TACBrECFClass.GetDadosUltimaReducaoZ: String;
begin
  Result := '';
  ErroAbstract('DadosUltimaReducaoZ');
end;

function TACBrECFClass.GetDadosReducaoZ: String;
Var
  I     : Integer ;
  AliqZ : TACBrECFAliquota ;
  FPGZ  : TACBrECFFormaPagamento ;
  CNFZ  : TACBrECFComprovanteNaoFiscal ;
  RGZ   : TACBrECFRelatorioGerencial ;
  ECF   : TACBrECF;
begin
  ECF := GetECFComponente(Self);

  { Alimenta a class com os dados atuais do ECF }
  with fpDadosReducaoZClass do
  begin
    { Zerar variaveis e inicializa Dados do ECF }
    InitDadosUltimaReducaoZ;

    with ECF do
    begin
      { REDUÇÃO Z }
      try DataDoMovimento  := DataMovimento; except end ;
      try NumeroCOOInicial := NumCOOInicial; except end ;

      { CONTADORES }
      try COO  := StrToIntDef(NumCOO,0);  except end ;
      try GNF  := StrToIntDef(NumGNF,0);  except end ;
      try CRO  := StrToIntDef(NumCRO,0);  except end ;
      try CRZ  := StrToIntDef(NumCRZ,0);  except end ;
      try CCF  := StrToIntDef(NumCCF,0);  except end ;
      try CDC  := StrToIntDef(NumCDC,0);  except end ;
      try CFC  := StrToIntDef(NumCFC,0);  except end ;
      try GRG  := StrToIntDef(NumGRG,0);  except end ;
      try GNFC := StrToIntDef(NumGNFC,0); except end ;
      try CFD  := StrToIntDef(NumCFD,0);  except end ;
      try NCN  := StrToIntDef(NumNCN,0);  except end ;
      try CCDC := StrToIntDef(NumCCDC,0); except end ;

      { TOTALIZADORES }
      try ValorGrandeTotal  := GrandeTotal;             except end ;
      try ValorVendaBruta   := VendaBruta;              except end ;
      try CancelamentoICMS  := TotalCancelamentos + TotalCancelamentosEmAberto; except end ;
      try DescontoICMS      := TotalDescontos;          except end ;
      try AcrescimoICMS     := TotalAcrescimos;         except end ;
      try CancelamentoISSQN := TotalCancelamentosISSQN + TotalCancelamentosEmAbertoISSQN; except end ;
      try DescontoISSQN     := TotalDescontosISSQN;     except end ;
      try AcrescimoISSQN    := TotalAcrescimosISSQN;    except end ;
      try CancelamentoOPNF  := TotalCancelamentosOPNF;  except end ;
      try DescontoOPNF      := TotalDescontosOPNF;      except end ;
      try AcrescimoOPNF     := TotalAcrescimosOPNF;     except end ;

      { Copiando objetos de ICMS e ISS}
      try
        CarregaAliquotas;
        LerTotaisAliquota;

        for I := 0 to fpAliquotas.Count - 1 do
        begin
          { Deve desconsiderar alíquotas zeradas e índice FF, II, NN (ECFVirtual) }
          if (fpAliquotas[I].Aliquota <= 0) and (not StrIsNumber(fpAliquotas[I].Indice)) then
            Continue;

          AliqZ := TACBrECFAliquota.Create;
          AliqZ.Assign( fpAliquotas[I] );

          AdicionaAliquota( AliqZ );
        end;
      except
      end;

      { ICMS }
      try SubstituicaoTributariaICMS  := TotalSubstituicaoTributaria; except end ;
      try IsentoICMS                  := TotalIsencao;                except end ;
      try NaoTributadoICMS            := TotalNaoTributado;           except end ;

      { ISSQN }
      try SubstituicaoTributariaISSQN := TotalSubstituicaoTributariaISSQN; except end ;
      try IsentoISSQN                 := TotalIsencaoISSQN;                except end ;
      try NaoTributadoISSQN           := TotalNaoTributadoISSQN;           except end ;

      { TOTALIZADORES NÃO FISCAIS }
      try
        CarregaComprovantesNaoFiscais ;
        LerTotaisComprovanteNaoFiscal ;

        For I := 0 to fpComprovantesNaoFiscais.Count-1 do
        begin
          CNFZ := TACBrECFComprovanteNaoFiscal.Create ;
          CNFZ.Assign( fpComprovantesNaoFiscais[I] );

          TotalizadoresNaoFiscais.Add( CNFZ ) ;
        end ;

        TotalOperacaoNaoFiscal := TotalNaoFiscal;
      except
      end ;

      { RELATÓRIO GERENCIAL }
      try
        CarregaRelatoriosGerenciais ;

        For I := 0 to fpRelatoriosGerenciais.Count-1 do
        begin
           RGZ := TACBrECFRelatorioGerencial.Create ;
           RGZ.Assign( fpRelatoriosGerenciais[I] );

           fpDadosReducaoZClass.RelatorioGerencial.Add( RGZ ) ;
        end ;
      except
      end ;

      { MEIOS DE PAGAMENTO }
      try
        CarregaFormasPagamento ;
        LerTotaisFormaPagamento ;

        For I := 0 to fpFormasPagamentos.Count-1 do
        begin
          FPGZ := TACBrECFFormaPagamento.Create ;
          FPGZ.Assign( fpFormasPagamentos[I] );

          MeiosDePagamento.Add( FPGZ ) ;
        end ;

        fpDadosReducaoZClass.TotalTroco := ECF.TotalTroco;
      except
      end ;
    end;

    CalculaValoresVirtuais;
    Result := MontaDadosReducaoZ;
  end;
end;

procedure TACBrECFClass.InitDadosUltimaReducaoZ;
var
  ECF: TACBrECF;
begin
  with fpDadosReducaoZClass do
  begin
    Clear ;

    ECF := GetECFComponente(Self);

    { DADOS DO ECF }
    with ECF do
    begin
      try DataDaImpressora := DataHora;    except end ;
      try NumeroDeSerie    := NumSerie;    except end ;
      try NumeroDeSerieMFD := NumSerieMFD; except end ;
      try NumeroDoECF      := NumECF;      except end ;
      try NumeroDaLoja     := NumLoja;     except end ;
    end;
  end;
end;

procedure TACBrECFClass.LeituraX;
begin
  ErroAbstract('LeituraX');
end;

procedure TACBrECFClass.LeituraXSerial(Linhas: TStringList);
begin
  ErroAbstract('LeituraXSerial');
end;

procedure TACBrECFClass.MudaHorarioVerao;
begin
  ErroAbstract('MudaHorarioVerao');
end;

procedure TACBrECFClass.MudaHorarioVerao(EHorarioVerao: Boolean);
begin
  ErroAbstract('MudaHorarioVerao(EHorarioVerao: Boolean)');
end;

procedure TACBrECFClass.MudaArredondamento(Arredondar: Boolean);
begin
  ErroAbstract('MudaArredondamento');
end;

procedure TACBrECFClass.IdentificaOperador(Nome: String);
begin
//  ErroAbstract('IdentificaOperador');
end;

procedure TACBrECFClass.IdentificaPAF(NomeVersao, MD5: String);
begin
  ErroAbstract('IdentificaPAF');
end;

function TACBrECFClass.RetornaInfoECF(Registrador : String) : AnsiString ;
begin
  Result := '';
  ErroAbstract('RetornaInfoECF');
end;

procedure TACBrECFClass.PreparaTEF;
var
  ECF: TACBrECF;
begin
  ECF := GetECFComponente(Self);

  try
     ECF.FechaRelatorio ;
  except
  end ;

  ECF.CarregaAliquotas ;
  ECF.CarregaFormasPagamento ;
  ECF.CarregaComprovantesNaoFiscais ;
end;

procedure TACBrECFClass.ReducaoZ(DataHora: TDateTime);
begin
  ErroAbstract('ReducaoZ');
end;

procedure TACBrECFClass.SubtotalizaCupom(DescontoAcrescimo: Double;
       MensagemRodape : AnsiString);
begin
  ErroAbstract('SubtotalizaCupom');
end;

procedure TACBrECFClass.CancelaDescontoAcrescimoSubTotal(
  TipoAcrescimoDesconto: Char);
begin
  ErroAbstract('CancelaDescontoAcrescimoSubTotal');
end;

procedure TACBrECFClass.LegendaInmetroProximoItem;
begin
  ErroAbstract('LegendaInmetroProximoItem');
end;

procedure TACBrECFClass.VendeItem(Codigo, Descricao : String ;
  AliquotaECF : String ; Qtd : Double ; ValorUnitario : Double ;
  ValorDescontoAcrescimo : Double ; Unidade : String ;
  TipoDescontoAcrescimo : String ; DescontoAcrescimo : String ;
  CodDepartamento : Integer) ;
begin
  ErroAbstract('VendeItem');
end;

procedure TACBrECFClass.VendeItemEx(Codigo, Descricao: String;
  AliquotaICMS: String; Qtd: Double; ValorUnitario: Double;
  ValorDescontoAcrescimo: Double; Unidade: String;
  TipoDescontoAcrescimo: String; DescontoAcrescimo: String;
  CodDepartamento: Integer; EAN13: String; CasasDecimaisQtde: Integer;
  CasasDecimaisValor: Integer; ArredondaTrunca: Char; NCM: String;
  CFOP: String; InformacaoAdicional: String; TotalDosTributos: Double;
  OrigemProduto: Integer; CST_ICMS: String; ModalidadeBCICMS: Integer;
  PercentualReducaoBCICMS: Double; CSOSN: String; ValorBaseCalculoSN: Double;
  ValorICMSRetidoSN: Double; AliquotaCalculoCreditoSN: Double;
  ValorCreditoICMSSN: Double; ItemListaServico: String; CodigoISS: String;
  NaturezaOperacaoISS: String; IndicadorIncentivoFiscalISS: Integer;
  CodigoIBGE: String; ModalidadeBCICMSST: Integer;
  PercentualMargemICMSST: Double; PercentualReducaoBCICMSST: Double;
  ValorReducaoBCICMSST: Double; AliquotaICMSST: Double; ValorICMSST: Double;
  ValorICMSDesonerado: Double; MotivoDesoneracaoICMS: Integer; CST_PIS: String;
  BaseCalculoPIS: Double; AliquotaPIS: Double; ValorPIS: Double;
  QuantidadeVendidaPIS: Double; ValorAliquotaPIS: Double; CST_COFINS: String;
  BaseCalculoCOFINS: Double; AliquotaCOFINS: Double; ValorCOFINS: Double;
  QuantidadeVendidaCOFINS: Double; ValorAliquotaCOFINS: Double; CEST: String);
begin
  ErroAbstract('VendeItemEx');
end;

procedure TACBrECFClass.DescontoAcrescimoItemAnterior(
  ValorDescontoAcrescimo : Double ; DescontoAcrescimo : String ;
  TipoDescontoAcrescimo : String ; NumItem : Integer) ;
begin
  ErroAbstract('DescontoAcrescimoItemAnterior');
end ;

procedure TACBrECFClass.CancelaDescontoAcrescimoItem(NumItem: Integer;
  TipoAcrescimoDesconto: String);
begin
  ErroAbstract('CancelaDescontoAcrescimoItem');
end;

procedure TACBrECFClass.CancelaItemVendidoParcial(NumItem: Integer;
  Quantidade: Double);
begin
  ErroAbstract('CancelaItemVendidoParcial');
end;

procedure TACBrECFClass.ErroAbstract(NomeProcedure: String);
begin
  raise EACBrECFCMDInvalido.create(ACBrStr(Format(cACBrECFCMDInvalidoException,
                                          [NomeProcedure, ModeloStr] ))) ;
end;

function TACBrECFClass.GetModeloStr: String;
begin
  Result := fpModeloStr ;
  //if fpMFD then
  //   Result := Result + ' MFD' ;
end;

procedure TACBrECFClass.DoOnMsgPoucoPapel( Mensagem : String ) ;
begin
  if MsgPoucoPapel < 0 then exit;

  if now > IncSecond(fpUltimaMsgPoucoPapel, MsgPoucoPapel) then { Avisa ? }
  begin
     if Assigned( fsOnMsgPoucoPapel ) then
        fsOnMsgPoucoPapel( self )
     else
      begin
        if Mensagem = '' then
           Mensagem := cACBrECFDoOnMsgPoucoPapel;

        Mensagem := ACBrStr( Mensagem ) ;
        {$IFNDEF NOGUI}
          {$IFDEF FMX}
          MessageDlg(Mensagem, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK],0);
          {$ELSE}
          MessageDlg( Mensagem ,mtError,[mbOk],0)  ;
          {$ENDIF}
        {$ELSE}
          writeln( Mensagem ) ;
        {$ENDIF}
      end ;

     fpUltimaMsgPoucoPapel := now ;
  end ;
end;

procedure TACBrECFClass.DoOnChequeEstado(const Estado: TACBrECFCHQEstado;
  var Continuar: Boolean);
begin
  if Assigned( fsOnChequeEstado ) then
    fsOnChequeEstado( Estado, Continuar );
end;

procedure TACBrECFClass.DoOnErrorSemPapel ;
begin
   if fpIgnorarErroSemPapel then exit;

   if Assigned( fsOnErrorSemPapel ) then
      fsOnErrorSemPapel( self )
   else
      raise EACBrECFSemPapel.Create( cACBrECFSemPapelException );
end ;

function TACBrECFClass.DoOnMsgRetentar( const Mensagem : String;
   const Situacao : String = ''): Boolean;
{$IFDEF MSWINDOWS}
 Var
   UsandoBlockInput : Boolean ;
{$ENDIF}
begin
  Result := False ;

  {$IFNDEF NOGUI}
    {$IFDEF MSWINDOWS}
      UsandoBlockInput := False ;

      if fsUsandoBlockInput then
      begin
         UsandoBlockInput := True ;
         BlockInput(False,True);
      end ;
    {$ENDIF}
  {$ENDIF}

  if Assigned( fsOnMsgRetentar ) then
     fsOnMsgRetentar( ACBrStr(Mensagem), Situacao, Result )
  else
   begin
     {$IFNDEF NOGUI}
      {$IFDEF FMX}
      if MessageDlg(ACBrStr( Mensagem+sLineBreak+sLineBreak + cACBrECFDoOnMsgRetentar ),
                    TMsgDlgType.mtConfirmation, [TMsgDlgBtn.mbYes,TMsgDlgBtn.mbNo],0) = mrYes then
        Result := True ;
      {$ELSE}
      if Retentar and
        (MessageDlg( ACBrStr( Mensagem+sLineBreak+sLineBreak + cACBrECFDoOnMsgRetentar ),
                     mtConfirmation,[mbYes,mbNo],0) = mrYes) then
        Result := True ;
      {$ENDIF}
     {$else}
       Result := Retentar;
     {$ENDIF}
   end ;

  {$IFNDEF NOGUI}
    {$IFDEF MSWINDOWS}
      if UsandoBlockInput then
         BlockInput(True,False);
    {$ENDIF}
  {$ENDIF}
end;

{ Esta rotina é usada por Impressoras que não permitem enviar várias
  linhas de uma só vez }
procedure TACBrECFClass.ImprimirLinhaALinha(Texto, Cmd: AnsiString );
Var Linhas : TStringList ;
    I : Integer ;
begin
  if Texto = '' then
     Texto := PadRight(Texto,Colunas) ;

  Texto := AjustaLinhas( Texto, Colunas );

  Linhas := TStringList.Create ;
  try
     Linhas.Text := Texto ;

     for I := 0 to Linhas.Count-1 do
     begin
        EnviaComando( Cmd + PadRight( Linhas[I], Colunas), 6 ) ;
        if not fpTermica then
           sleep(100) ;

        { Aguarda 1 segundo ou até o ECF ficar Em linha novamente }
        { Semelhante ao "AguardaImpressao := True", porém é mais rápido, pois no
          método "VerificaFimImpressao" alem de verificado o "EmLinha" também é
          solicitado o Status do ECF }
        try
           EmLinha( 1 ) ;
        except
        end ;
     end ;
  finally
     Linhas.Free ;
  end ;
end;

{-------------------------------- ALIQUOTAS ----------------------------------}
procedure TACBrECFClass.CarregaAliquotas;
begin
  { Apenas instancia um ObjectList de Aliquotas (TACBrECFAliquotas) vazia }
  if Assigned( fpAliquotas ) then
     fpAliquotas.Free ;

  fpAliquotas := TACBrECFAliquotas.create( true ) ;
end;

procedure TACBrECFClass.LerTotaisAliquota;
begin
  ErroAbstract('LerTotaisAliquota');
end;

procedure TACBrECFClass.ProgramaAliquota(Aliquota: Double; Tipo: Char;
   Posicao : String);
begin
  ErroAbstract('ProgramaAliquota');
end;

function TACBrECFClass.GetAliquotas: TACBrECFAliquotas;
var
  ECF: TACBrECF;
begin
  if not Assigned( fpAliquotas ) then
  begin
     ECF := GetECFComponente(Self);
     ECF.CarregaAliquotas ;
  end;

  result := fpAliquotas ;
end;

function TACBrECFClass.AchaICMSAliquota( var AliquotaICMS: String):
   TACBrECFAliquota;
{   AliquotaICMS, Formatos válidos:
  - Por Valor da aliquota, Ex: '18', '12,00' '2,56'
  - Por Valor da Aliquota especificando o Tipo como sufixo
    (T = ICMS, ou S = ISS),      Ex: '18T',  '2,5S'
  - Por Indice, adicione o prefixo 'T', Ex: 'T01', 'T03', 'TA', 'TT01'
    ( o indice deve ser no mesmo formato retornado pela propriedade
      "Aliquotas[n].Indice" (que varia para cada modelo de ECF) )
}
   procedure VerificaTipoAliquota(var AliquotaICMS: String;
     var Tipo: char);
   Var UltDigito : String ;
   begin
     UltDigito := RightStr(AliquotaICMS,1) ;
     case UltDigito[1] of
        'T','S' :
          begin
            Tipo := UltDigito[1] ;
            AliquotaICMS := copy( AliquotaICMS,1,Length(AliquotaICMS)-1 ) ;
          end ;
     else
        Tipo := ' ' ;
     end ;
   end;

Var
  AliquotaStr : String ;
  Tipo        : Char ;
  ValAliquota : Double ;
  N: Integer;
begin
  GetAliquotas;

  Result := nil ;

  case AliquotaICMS[1] of
    'I','N','F' :
      begin
        if copy(AliquotaICMS,2,1) = 'S' then
        begin
          N := min(max(StrToIntDef(copy(AliquotaICMS,3,1),1),1),3);
          AliquotaICMS := copy(AliquotaICMS,1,2) + IntToStr(N);
        end
        else
        begin
          N := min(max(StrToIntDef(copy(AliquotaICMS,2,1),1),1),3);
          AliquotaICMS := AliquotaICMS[1] + IntToStr(N);
        end;

        if (AchaTotalizadorNaoTributadoIndice(AliquotaICMS) = nil) then
          raise EACBrECFCMDInvalido.Create(ACBrStr(cACBrECFAchaICMSAliquotaInvalida) + AliquotaICMS);
      end;

    'T' :
      begin
        AliquotaICMS := copy(AliquotaICMS,2,Length(AliquotaICMS)) ; {Remove o "T"}
        Result := AchaICMSIndice( AliquotaICMS ) ;
      end;

    'S' : {ISSQN}
      begin
        if pos(copy(AliquotaICMS,2,1),'INF') > 0 then
        begin
          N := min(max(StrToIntDef(copy(AliquotaICMS,3,1),1),1),3);
          AliquotaICMS  := copy(AliquotaICMS,2,1)+'S'+ IntToStr(N);

          if (AchaTotalizadorNaoTributadoIndice(AliquotaICMS) = nil) then
            raise EACBrECFCMDInvalido.Create(ACBrStr(cACBrECFAchaICMSAliquotaInvalida) + AliquotaICMS);
        end
        else
        begin
          AliquotaICMS := copy(AliquotaICMS,2,Length(AliquotaICMS)) ; {Remove o "S"}
          Result := AchaICMSIndice( AliquotaICMS ) ;
        end;
      end;
  else
    { Verificando se informou T ou S no final da Aliquota (Tipo) }
    AliquotaStr := AliquotaICMS ;
    Tipo        := ' ' ;
    VerificaTipoAliquota( AliquotaStr, Tipo) ;

    try
      ValAliquota := StringToFloat( AliquotaStr ) ;
    except
      raise EACBrECFCMDInvalido.Create(ACBrStr(cACBrECFAchaICMSAliquotaInvalida) + AliquotaICMS);
    end ;

    Result := AchaICMSAliquota( ValAliquota, Tipo ) ;

    if Result = Nil then
      raise EACBrECFCMDInvalido.Create(ACBrStr(cACBrECFAchaICMSCMDInvalido) + AliquotaICMS)
  end;

  if Result <> nil then
     AliquotaICMS := Result.Indice ;
end;

function TACBrECFClass.AchaICMSAliquota( Aliquota: Double; Tipo : Char ) :
   TACBrECFAliquota;
var
  A : Integer ;
  ftAliq : Double ;
  cTipo  : Char ;
begin
  GetAliquotas;

  if not CharInSet(Tipo , ['S','T']) then
     Tipo := ' ' ;

  Aliquota := SimpleRoundTo(Aliquota,-2) ;
  Result   := nil ;

  with fpAliquotas do
  begin
     For A := 0 to Count -1 do
     begin
        ftAliq := SimpleRoundTo(Objects[A].Aliquota,-2) ;
        cTipo  := Objects[A].Tipo ;
        if ( ftAliq = Aliquota) and CharInSet(Tipo , [' ',cTipo]) then
        begin
           result := Objects[A] ;
           Break ;
        end ;
     end ;
  end ;
end;

function TACBrECFClass.AchaICMSIndice(Indice: String): TACBrECFAliquota;
var
  A : Integer ;
begin
  GetAliquotas;

  Result := nil ;
  Indice := UpperCase(Indice) ;

  with fpAliquotas do
  begin
     For A := 0 to Count -1 do
     begin
        if UpperCase( Objects[A].Indice ) = Indice then
        begin
           result := Objects[A] ;
           Break ;
        end ;
     end ;
  end ;
end;

{------------------------ TOTALIZADORESNAOTRIBUTADOS --------------------------}
procedure TACBrECFClass.CarregaTotalizadoresNaoTributados;
begin
  if Assigned( fpTotalizadoresNaoTributados ) then
     fpTotalizadoresNaoTributados.Free ;

  fpTotalizadoresNaoTributados := TACBrECFTotalizadoresNaoTributados.create( true ) ;

  with fpTotalizadoresNaoTributados.New do
  begin
    Indice := 'F1';
  end;
  with fpTotalizadoresNaoTributados.New do
  begin
    Indice := 'I1';
  end;
  with fpTotalizadoresNaoTributados.New do
  begin
    Indice := 'N1';
  end;

  if fpMFD then
  begin
    with fpTotalizadoresNaoTributados.New do
    begin
      Indice := 'FS1';
      Tipo := 'S';
    end;

    with fpTotalizadoresNaoTributados.New do
    begin
      Indice := 'IS1';
      Tipo := 'S';
    end;

    with fpTotalizadoresNaoTributados.New do
    begin
      Indice := 'NS1';
      Tipo := 'S';
    end;
  end;
end;

function TACBrECFClass.GetTotalizadoresNaoTributados: TACBrECFTotalizadoresNaoTributados;
var
  ECF: TACBrECF;
begin
  if not Assigned( fpTotalizadoresNaoTributados ) then
  begin
     ECF := GetECFComponente(Self);
     ECF.CarregaTotalizadoresNaoTributados ;
  end;

  Result := fpTotalizadoresNaoTributados ;
end;


procedure TACBrECFClass.LerTotaisTotalizadoresNaoTributados;
var
  A: Integer;
begin
  GetTotalizadoresNaoTributados ;

  with fpTotalizadoresNaoTributados do
  begin
     For A := 0 to Count -1 do
     begin
        with Objects[A] do
        begin
           if Indice = 'F1' then
             Total := TotalSubstituicaoTributaria
           else if Indice = 'I1' then
             Total := TotalIsencao
           else if Indice = 'N1' then
             Total := TotalNaoTributado
           else if Indice = 'FS1' then
             Total := TotalSubstituicaoTributariaISSQN
           else if Indice = 'IS1' then
             Total := TotalIsencaoISSQN
           else if Indice = 'NS1' then
             Total := TotalNaoTributadoISSQN;
        end;
     end;
  end ;
end;

function TACBrECFClass.AchaTotalizadorNaoTributadoIndice(Indice: String
  ): TACBrECFTotalizadorNaoTributado;
var
  A : Integer ;
begin
  GetTotalizadoresNaoTributados;

  Result := nil ;
  Indice := UpperCase(Indice) ;

  with fpTotalizadoresNaoTributados do
  begin
     For A := 0 to Count -1 do
     begin
        if UpperCase( Objects[A].Indice ) = Indice then
        begin
           result := Objects[A] ;
           Break ;
        end ;
     end ;
  end ;
end;

function TACBrECFClass.SomaTotalizadorNaoTributadoIndice(Indice: String
  ): Double;
var
  A : Integer ;
begin
  GetTotalizadoresNaoTributados;

  Result := 0;
  Indice := UpperCase(Indice) ;

  For A := 0 to fpTotalizadoresNaoTributados.Count -1 do
  begin
    if (UpperCase( copy(fpTotalizadoresNaoTributados[A].Indice,1,Length(Indice)) ) = Indice) and
       (StrToIntDef(copy(fpTotalizadoresNaoTributados[A].Indice, Length(Indice)+1, 1),0) > 0) then
      Result := Result + fpTotalizadoresNaoTributados[A].Total;
  end ;

  Result := RoundTo(Result, -2);
end;


{--------------------------- FORMAS DE PAGAMENTO ------------------------------}
procedure TACBrECFClass.CarregaFormasPagamento;
begin
  if Assigned( fpFormasPagamentos ) then
     fpFormasPagamentos.Free ;

  fpFormasPagamentos := TACBrECFFormasPagamento.Create( true ) ;
end;

procedure TACBrECFClass.LerTotaisFormaPagamento;
begin
  ErroAbstract('LerTotaisFormaPagamento');
end;

procedure TACBrECFClass.ProgramaFormaPagamento(var Descricao: String;
  PermiteVinculado: Boolean; Posicao : String);
begin
  ErroAbstract('ProgramaFormaPagamento');
end;

function TACBrECFClass.GetFormasPagamentos: TACBrECFFormasPagamento;
var
  ECF: TACBrECF;
begin
  if not Assigned( fpFormasPagamentos ) then
  begin
    ECF := GetECFComponente(Self);
    ECF.CarregaFormasPagamento ;
  end;

  Result := fpFormasPagamentos ;
end;

function TACBrECFClass.AchaFPGDescricao(Descricao: String; BuscaExata: Boolean;
  IgnorarCase: Boolean; IgnorarAcentos: Boolean): TACBrECFFormaPagamento;
 var Tamanho, A : Integer ;
     DescrECF : String ;
begin
  GetFormasPagamentos;

  result := nil ;
  with fpFormasPagamentos do
  begin
     Descricao := TrimRight( Descricao ) ;
     if IgnorarAcentos then
        Descricao := TiraAcentos( Descricao );
     if IgnorarCase then
        Descricao := UpperCase( Descricao ) ;

     Tamanho := Length( Descricao ) ;
     For A := 0 to Count -1 do
     begin
        DescrECF := TrimRight( Objects[A].Descricao ) ;
        if not BuscaExata then
           DescrECF := LeftStr( DescrECF, Tamanho) ;
        if IgnorarAcentos then
           DescrECF := TiraAcentos( DescrECF );
        if IgnorarCase then
           DescrECF := UpperCase( DescrECF ) ;

        if DescrECF = Descricao then
        begin
           result := Objects[A] ;
           Break ;
        end ;
     end ;
  end ;
end;

function TACBrECFClass.AchaFPGIndice( Indice: String) :
   TACBrECFFormaPagamento;
var A : Integer ;
begin
  GetFormasPagamentos;

  result := nil ;
  with fpFormasPagamentos do
  begin
     For A := 0 to Count -1 do
     begin
        if Objects[A].Indice = Indice then
        begin
           result := Objects[A] ;
           Break ;
        end ;
     end ;
  end ;
end;

{---------------------------- Relatórios Gerenciais ---------------------------}
procedure TACBrECFClass.CarregaRelatoriosGerenciais;
begin
  if Assigned( fpRelatoriosGerenciais ) then
     fpRelatoriosGerenciais.Free ;

  fpRelatoriosGerenciais := TACBrECFRelatoriosGerenciais.Create( true ) ;
end;

procedure TACBrECFClass.LerTotaisRelatoriosGerenciais ;
begin
  ErroAbstract('LerTotaisRelatoriosGerenciais');
end ;

procedure TACBrECFClass.ProgramaRelatorioGerencial(var Descricao: String;
  Posicao: String);
begin
  ErroAbstract('ProgramaRelatóriosGerenciais');
end;

function TACBrECFClass.GetRelatoriosGerenciais: TACBrECFRelatoriosGerenciais;
var
  ECF: TACBrECF;
begin
  if not Assigned( fpRelatoriosGerenciais ) then
  begin
     ECF := GetECFComponente(Self);
     ECF.CarregaRelatoriosGerenciais ;
  end;

  Result := fpRelatoriosGerenciais ;
end;

function TACBrECFClass.AchaRGDescricao(Descricao: String;
  BuscaExata: Boolean; IgnorarCase : Boolean): TACBrECFRelatorioGerencial;
var Tamanho, A : Integer ;
     DescrECF : String ;
begin
  GetRelatoriosGerenciais;

  result := nil ;
  with fpRelatoriosGerenciais do
  begin
     Descricao := TrimRight( Descricao ) ;
     if IgnorarCase then
        Descricao := UpperCase( Descricao ) ;
     Tamanho := Length( Descricao ) ;
     For A := 0 to Count -1 do
     begin
        DescrECF := TrimRight( Objects[A].Descricao ) ;
        if not BuscaExata then
           DescrECF := LeftStr( DescrECF, Tamanho) ;
        if IgnorarCase then
           DescrECF := UpperCase(DescrECF) ;

        if DescrECF = Descricao then
        begin
           result := Objects[A] ;
           Break ;
        end ;
     end ;
  end ;
end;

function TACBrECFClass.AchaRGIndice(
  Indice: String): TACBrECFRelatorioGerencial;
var A : Integer ;
begin
  GetRelatoriosGerenciais;

  result := nil ;
  with fpRelatoriosGerenciais do
  begin
     For A := 0 to Count -1 do
     begin
        if Objects[A].Indice = Indice then
        begin
           result := Objects[A] ;
           Break ;
        end ;
     end ;
  end ;

end;


{------------------------- COMPROVANTES NAO FISCAIS ---------------------------}
procedure TACBrECFClass.CarregaComprovantesNaoFiscais;
begin
  if Assigned( fpComprovantesNaoFiscais ) then
     fpComprovantesNaoFiscais.Free ;

  fpComprovantesNaoFiscais := TACBrECFComprovantesNaoFiscais.Create( true ) ;
end;

procedure TACBrECFClass.LerTotaisComprovanteNaoFiscal;
begin
  ErroAbstract('LerTotaisComprovanteNaoFiscal');
end;

procedure TACBrECFClass.ProgramaComprovanteNaoFiscal(var Descricao: String;
  Tipo: String; Posicao : String);
begin
  ErroAbstract('ProgramaComprovanteNaoFiscal');
end;

function TACBrECFClass.GetComprovantesNaoFiscais: TACBrECFComprovantesNaoFiscais;
var
  ECF: TACBrECF;
begin
  if not Assigned( fpComprovantesNaoFiscais ) then
  begin
     ECF := GetECFComponente(Self);
     ECF.CarregaComprovantesNaoFiscais ;
  end;

  result := fpComprovantesNaoFiscais ;
end;

function TACBrECFClass.AchaCNFDescricao( Descricao: String;
       BuscaExata : Boolean; IgnorarCase : Boolean ): TACBrECFComprovanteNaoFiscal;
 var Tamanho, A : Integer ;
     DescrECF : String ;
begin
  GetComprovantesNaoFiscais;

  result := nil ;
  with fpComprovantesNaoFiscais do
  begin
     Descricao := TrimRight( Descricao ) ;
     if IgnorarCase then
        Descricao := UpperCase( Descricao ) ;
     Tamanho := Length( Descricao ) ;
     For A := 0 to Count -1 do
     begin
        DescrECF := TrimRight( Objects[A].Descricao ) ;
        if not BuscaExata then
           DescrECF := LeftStr( DescrECF, Tamanho) ;
        if IgnorarCase then
           DescrECF := UpperCase(DescrECF) ;

        if DescrECF = Descricao then
        begin
           result := Objects[A] ;
           Break ;
        end ;
     end ;
  end ;
end;

function TACBrECFClass.AchaCNFIndice(
  Indice: String): TACBrECFComprovanteNaoFiscal;
var A : Integer ;
begin
  GetComprovantesNaoFiscais;

  result := nil ;
  with fpComprovantesNaoFiscais do
  begin
     For A := 0 to Count -1 do
     begin
        if Objects[A].Indice = Indice then
        begin
           result := Objects[A] ;
           Break ;
        end ;
     end ;
  end ;
end;

function TACBrECFClass.AchaCNFFormaPagamento(
  CodFPG: String): TACBrECFComprovanteNaoFiscal;
var A : Integer ;
begin
  GetComprovantesNaoFiscais;

  result := nil ;
  with fpComprovantesNaoFiscais do
  begin
     For A := 0 to Count -1 do
     begin
        if Objects[A].FormaPagamento = CodFPG then
        begin
           result := Objects[A] ;
           Break ;
        end ;
     end ;
  end ;
end;

{---------------------------- UNIDADES DE MEDIDA ------------------------------}
procedure TACBrECFClass.CarregaUnidadesMedida;
begin
  if Assigned( fpUnidadesMedida ) then
     fpUnidadesMedida.Free ;

  fpUnidadesMedida := TACBrECFUnidadesMedida.Create( true ) ;
end;

procedure TACBrECFClass.ProgramaUnidadeMedida(var Descricao: String);
begin
  ErroAbstract('ProgramaUnidadeMedida');
end;

function TACBrECFClass.GetUnidadesMedida: TACBrECFUnidadesMedida;
var
  ECF: TACBrECF;
begin
  if not Assigned( fpUnidadesMedida ) then
  begin
     ECF := GetECFComponente(Self);
     ECF.CarregaUnidadesMedida ;
  end;

  result := fpUnidadesMedida ;
end;

function TACBrECFClass.AchaUMDDescricao(
  Descricao: String): TACBrECFUnidadeMedida;
var A : Integer ;
begin
  GetUnidadesMedida;

  result := nil ;
  with fpUnidadesMedida do
  begin
     Descricao := Trim(UpperCase( Descricao )) ;
     For A := 0 to Count -1 do
     begin
        if Trim(UpperCase( Objects[A].Descricao )) = Descricao then
        begin
           result := Objects[A] ;
           Break ;
        end ;
     end ;
  end ;
end;

function TACBrECFClass.AchaUMDIndice( Indice: String): TACBrECFUnidadeMedida;
var A : Integer ;
begin
  GetUnidadesMedida;

  result := nil ;
  with fpUnidadesMedida do
  begin
     For A := 0 to Count -1 do
     begin
        if Objects[A].Indice = Indice then
        begin
           result := Objects[A] ;
           Break ;
        end ;
     end ;
  end ;
end;

{ ------------------------------ Cupom Vinculado -----------------------------}
procedure TACBrECFClass.CupomVinculado(COO, CodFormaPagto,
  CodComprovanteNaoFiscal: String; Valor: Double; Relatorio: TStrings;
  Vias: Integer);
Var
  wRetentar : Boolean ;
  ECF: TACBrECF;
begin
  { Chama rotinas do Componente ECF para atualizar os Memos }
  ECF := GetECFComponente(Self);
  ECF.AbreCupomVinculado( COO, CodFormaPagto, CodComprovanteNaoFiscal, Valor) ;

  wRetentar := Retentar ;
  try
     Retentar    := false ;
     fsVias      := Vias ;
     fsRelatorio := Relatorio ;

     {$IFNDEF NOGUI}
       if not ExibeMensagem  then
          DoCupomVinculado
       else
        begin
          { Isso fará a procedure LeResposta nao pintar o FormMsgAguarde }
          fsFormMsgControla := false ;
          FormMsgDoProcedure( DoCupomVinculado, 0)
        end ;
     {$ELSE}
       DoCupomVinculado
     {$ENDIF}
  finally
     {$IFNDEF NOGUI}
       fsFormMsgControla := true ;
     {$ENDIF}
     Retentar := wRetentar ;
  end ;
end;

procedure TACBrECFClass.DoCupomVinculado;
begin
  ListaCupomVinculado( fsRelatorio, fsVias );
end;

procedure TACBrECFClass.AbreCupomVinculado(COO, CodFormaPagto,
  CodComprovanteNaoFiscal: String; Valor: Double);
begin
  ErroAbstract('AbreCupomVinculado');
end;

procedure TACBrECFClass.LinhaCupomVinculado(Linha: AnsiString);
begin
  ErroAbstract('LinhaCupomVinculado');
end;

procedure TACBrECFClass.SegundaViaVinculado;
begin
  ErroAbstract('SegundaViaVinculado');
end;

procedure TACBrECFClass.ReimpressaoVinculado;
begin
  ErroAbstract('ReimpressãoVinculado');
end;

procedure TACBrECFClass.ListaCupomVinculado( Relatorio: TStrings;
  Vias: Integer);
Var
  Imp   : Integer ;
  Texto : String ;
  ECF: TACBrECF;
begin
  Imp := 0 ;
  ECF := GetECFComponente(Self);

  while Imp < Vias do
  begin
     {$IFNDEF NOGUI}
       try
          Texto := Format(MsgRelatorio,['Cupom Vinculado',Imp+1 ]) ;
       except
          Texto := MsgRelatorio ;
       end ;

       FormMsgPinta( Texto );
     {$ENDIF}

     ECF.LinhaCupomVinculado( Relatorio.Text ) ;

     Inc(Imp) ;
     if Imp < Vias then
     begin
        ECF.PulaLinhas  ;
        ECF.CortaPapel  ;
        PausarRelatorio( Imp ) ;
     end ;
  end ;

  {$IFNDEF NOGUI}
    FormMsgPinta( 'Fechando Cupom Vinculado' );
  {$ENDIF}
  ECF.FechaRelatorio ;
end;

{ ------------------------------ Relatorio Gerencial -------------------------}
procedure TACBrECFClass.RelatorioGerencial(Relatorio: TStrings; Vias: Integer; Indice: Integer);
Var
  wMsgAguarde : String ;
  wRetentar : Boolean ;
  ECF: TACBrECF;
begin
  { Chama rotinas do Componente ECF para atualizar os Memos }
  ECF := GetECFComponente(Self);
  try
     ECF.FechaRelatorio ; { Fecha se ficou algum aberto }
  Except
  end ;

  wMsgAguarde := MsgAguarde ;
  MsgAguarde  := cACBrECFAbrindoRelatorioGerencial ;
  try
     ECF.AbreRelatorioGerencial(Indice) ;
  finally
     MsgAguarde := wMsgAguarde ;
  end ;

  wRetentar := Retentar ;
  try
     Retentar    := false ;
     fsVias      := Vias ;
     fsRelatorio := Relatorio ;
     fsIndiceRG  := Indice;

     {$IFNDEF NOGUI}
       if not ExibeMensagem  then
         DoRelatorioGerencial
       else
        begin
          { Isso fará a procedure LeResposta nao pintar o FormMsgAguarde }
          fsFormMsgControla := false ;
          FormMsgDoProcedure(DoRelatorioGerencial,0) ;
        end ;
     {$ELSE}
       DoRelatorioGerencial ;
     {$ENDIF}
  finally
     {$IFNDEF NOGUI}
       fsFormMsgControla := true ;
     {$ENDIF}
     Retentar := wRetentar ;
  end ;
end;

procedure TACBrECFClass.DoRelatorioGerencial;
begin
   ListaRelatorioGerencial( fsRelatorio, fsVias, fsIndiceRG )
end;

procedure TACBrECFClass.AbreRelatorioGerencial(Indice : Integer) ;
begin
  ErroAbstract('AbreRelatorioGerencial');
end;

procedure TACBrECFClass.LinhaRelatorioGerencial(Linha: AnsiString; IndiceBMP: Integer);
begin
  ErroAbstract('LinhaRelatorioGerencial');
end;

function TACBrECFClass.DecodificaTexto(Operacao: Char; Texto: String;
  var Resposta: String): Boolean;
begin
  Result := False ;
  ErroAbstract('DecodificaTexto');
end;

procedure TACBrECFClass.ListaRelatorioGerencial(Relatorio: TStrings;
  Vias: Integer; Indice: Integer);
Var
  Imp   : Integer ;
  Texto : String ;
  ECF: TACBrECF;
begin
  Imp := 0 ;
  ECF := GetECFComponente(Self);

  while Imp < Vias do
  begin
    try
      Texto := Format(MsgRelatorio,['Relatório Gerencial',Imp+1 ]) ;
    except
      Texto := MsgRelatorio ;
    end ;

    {$IFNDEF NOGUI}
      FormMsgPinta( Texto );
    {$ENDIF}

    ECF.LinhaRelatorioGerencial( Relatorio.Text ) ;

    Inc(Imp) ;
    if Imp < Vias then
    begin
      ECF.PulaLinhas ;
      ECF.CortaPapel ;
      if Imp < 1 then
         Sleep( 200 ) ;

      PausarRelatorio( Imp ) ;
    end ;
  end ;

  {$IFNDEF NOGUI}
    FormMsgPinta( cACBrECFFechandoRelatorioGerencial );
  {$ENDIF}

  ECF.FechaRelatorio ;
end;

{ ------------------------------ Pausar Relatorios -------------------------}

procedure TACBrECFClass.PausarRelatorio( Via : Integer );
Var Texto : String ;
    SecRest, SecAnt : Integer ;
    FimPausa : TDateTime ;
begin

  Try
     FimPausa := IncSecond( now, PausaRelatorio ) ;
     SecAnt := 0 ;
     {$IFNDEF NOGUI}
       fsFormMsgTeclaParaFechar := 13 ;
     {$ENDIF}

     while (now < FimPausa) do
     begin
       {$IFNDEF NOGUI}
         if not (fsFormMsgEstado = fmsProcessando) then
            Break ;

          SecRest := SecondsBetween(now,  FimPausa) ;
          if SecAnt <> SecRest then  { Verifica se mudou os segundos }
          begin
             SecAnt := SecRest ;

             try
                Texto := Format(MsgPausaRelatorio, [Via, SecRest]) ;
             except
                Texto := MsgPausaRelatorio ;
             end ;

             FormMsgPinta( Texto );
          end ;

          if Assigned( fpDevice ) then
             if fpDevice.ProcessMessages then
                Application.ProcessMessages;
       {$ELSE}
          sleep(100) ;
       {$ENDIF}
     end ;
  finally
     {$IFNDEF NOGUI}
       fsFormMsgTeclaParaFechar := 0 ;
       fsFormMsgEstado := fmsProcessando ;
     {$ENDIF}
  end ;
end;





{$IFNDEF NOGUI}
  {$IFDEF MSWINDOWS}
    procedure TACBrECFClass.BlockInput( const Block, ClearTypeAhead : Boolean ) ;
    var
       Msg: TMsg;
    begin
      LoadBlockInput;
      if not Assigned( xBlockInput ) then
         exit ;

      if ClearTypeAhead then
      begin
        try
           // Remove todas as Teclas do Buffer do Teclado //
           while PeekMessage(Msg, 0, WM_KEYFIRST, WM_KEYLAST, PM_REMOVE or PM_NOYIELD) do;
        except
        end ;
      end ;

      xBlockInput( Block ) ;
      fsUsandoBlockInput := Block ;
    end ;
  {$ENDIF}

  function TACBrECFClass.FormMsgDoProcedure(AProcedure: TACBrFormMsgProcedure;
    TeclaParaFechar: Word): Boolean;
  Var Timer : TTimer ;
    ECF: TACBrECF;
    {$IFDEF VisualCLX}
     OldOnEvent : TEventEvent;
     OldCursor  : TCursor ;
    {$ENDIF}
  begin
    Result := true ;
    {$IFDEF VisualCLX}
    OldOnEvent := Application.OnEvent ;
    OldCursor  := Screen.Cursor ;
    {$ENDIF}

    if Assigned(fsFormMsg) then
       Raise EACBrECFErro.Create( ACBrStr(cACBrECFFormMsgDoProcedureException)) ;

    {$IFDEF FMX}
    if Assigned(fsOnCriarFormMsg) then
      fsOnCriarFormMsg(fsFormMsg)
    else
      fsFormMsg := TForm.CreateNew( Application );
    {$ELSE}
    fsFormMsg := TForm.create( Application ) ;
    {$ENDIF}
    ECF := GetECFComponente(Self);

    try
       {$IFDEF VisualCLX}
       Application.OnEvent := FormMsgEvent ;
       {$ENDIF}
       fsFormMsgProcedureAExecutar := AProcedure ;
       fsFormMsgTeclaParaFechar    := TeclaParaFechar ;
       fsFormMsgEstado             := fmsProcessando ;

       {$IFDEF FMX}
       fsFormMsg.OnKeyDown          := FormMsgKeyPress ;
       fsFormMsg.Position           := TFormPosition.MainFormCenter ;
       fsFormMsg.FormStyle          := TFormStyle.StayOnTop ;
       if not Assigned(fsOnCriarFormMsg) then
       begin
         fsFormMsg.Fill.Kind          := TBrushKind.Solid;
         fsFormMsg.Fill.Color         := ECF.FormMsgColor ;
         fsFormMsg.TagString          := ECF.FormMsgFonte.Size.ToString + ';'
                                       + ECF.FormMsgFonte.Family + ';'
                                       + AlphaColorToString(ECF.FormMsgColorFont);
       end;
       {$ELSE}
       fsFormMsg.KeyPreview   := true ;
       fsFormMsg.OnKeyPress   := FormMsgKeyPress ;
       fsFormMsg.Color        := ECF.FormMsgColor ;
       fsFormMsg.Font         := ECF.FormMsgFonte ;
       fsFormMsg.Position     := poMainFormCenter ;
       fsFormMsg.FormStyle    := fsStayOnTop ;
       {$ENDIF}
       fsFormMsg.OnCloseQuery := FormMsgCloseQuery ;
       fsFormMsg.BorderIcons  := [] ;
       fsFormMsg.BorderStyle  := {$IFDEF VisualCLX} fbsNone {$ELSE} {$IFDEF FMX}TFmxFormBorderStyle.None{$ELSE} bsNone {$ENDIF}{$ENDIF};
       {$IFDEF FMX}
        if Assigned(fsOnCriarFormMsg) then
          fsFormMsg.Visible := False
        else
        begin
         fsFormMsg.Width        := 0 ;   { Cria o form escondido }
         fsFormMsg.Height       := 0 ;
        end;
       {$ELSE}
       fsFormMsg.Width        := 0 ;   { Cria o form escondido }
       fsFormMsg.Height       := 0 ;
       {$ENDIF}

       fsFormMsgException     := '' ;
       {$IFDEF LINUX}
        {$IFNDEF FPC}
         fsFormMsg.OnShow     := FormShow ;
        {$ENDIF}
       {$ENDIF}

       if BloqueiaMouseTeclado then
        begin
          { Quando o Timer for ativado, a procedure em fsFormMsgProcedureAExecutar
            será executada... Ao fim da Procurede o fsFormMsg é fechado }
          { O objeto Timer será destruido no proprio evento FormMsgTimer }

          Timer := TTimer.Create(fsFormMsg);
          Timer.Enabled  := false ;
          Timer.OnTimer  := FormMsgTimer ;
          Timer.Interval := 3 ;
          Timer.Enabled  := True ;

          fsFormMsg.ShowModal ;
        end
       else
        begin
          fsFormMsg.Show ;
          FormMsgTimer(Self);
        end ;

       if fsFormMsgException <> '' then
          raise EACBrECFErro.Create( fsFormMsgException ) ;
    finally
       {$IFDEF VisualCLX}
       Application.OnEvent := OldOnEvent;
       Screen.Cursor       := OldCursor ;
       {$ENDIF}
       FreeAndNil(fsFormMsg) ;
    end
  end;

  procedure TACBrECFClass.FormMsgTimer(Sender: TObject);
  begin
    if Sender is TTimer then
    begin
       TTimer(Sender).Enabled := false ;
       FreeAndNil( Sender ) ;
    end ;

    try
       try
          if Assigned( fsFormMsgProcedureAExecutar ) then
             fsFormMsgProcedureAExecutar  ;
       finally
          if Assigned( fsOnMsgAguarde ) then
             fsOnMsgAguarde( '' ) ;

          if fsFormMsgEstado = fmsProcessando then
             fsFormMsgEstado := fmsConcluido
          else
             fsFormMsgEstado := fmsAbortado ;

          fsFormMsg.Close ;
          //{$IFNDEF COMPLIB_CLX}
          {$IFDEF FMX}
          if Assigned(Application.MainForm) then
            Application.MainForm.BringToFront ;
          {$ELSE}
          Application.BringToFront ;
          {$ENDIF}
          //{$ENDIF}
       end ;
    except
      { Não dispra a exceção dentro do Envento do Timer para a ordem da Pilha
        de Exceções ficar correta }
      on E : Exception
      do begin
         fsFormMsgException := E.Message ;
      end ;
    end ;
  end;
  {$IFDEF FMX}
  procedure TACBrECFClass.FormMsgKeyPress(Sender: TObject; var Key: Word; var KeyChar: Char;
    Shift: TShiftState);
  begin
    if (fsFormMsgTeclaParaFechar <> 0)  and (fsFormMsgEstado <> fmsAbortado) then
    begin
      if (KeyChar <> #0) and (Integer(KeyChar.ToUpper) = fsFormMsgTeclaParaFechar) then
        fsFormMsgEstado := fmsAbortado
      else if (Key > 0) and (Key = fsFormMsgTeclaParaFechar) then
        fsFormMsgEstado := fmsAbortado
      else
      begin
        Key := 0;
        KeyChar := #0;
      end;
    end;
  end;
  {$ELSE}
  procedure TACBrECFClass.FormMsgKeyPress(Sender: TObject; var Key: Char);
  begin
    if (fsFormMsgTeclaParaFechar <> 0)  and
       (UpCase( Key ) = chr(fsFormMsgTeclaParaFechar)) and
       (fsFormMsgEstado <> fmsAbortado) then
       fsFormMsgEstado := fmsAbortado
    else
       Key := chr(0) ;
  end;
  {$ENDIF}

  procedure TACBrECFClass.FormMsgCloseQuery(Sender: TObject;
    var CanClose: Boolean);
  begin
    CanClose := (fsFormMsgEstado <> fmsProcessando) ;
  end;
  {$IFDEF VisualCLX}
    {$D-}
    procedure TACBrECFClass.FormMsgEvent(Sender: QObjectH; Event: QEventH;
      var Handled: Boolean);
    {$IFDEF QT3CLX}
    Var  EventType: QEventH;
    {$ELSE}
    Var  EventType: QEventType;
    {$ENDIF}
    begin
      {$IFDEF QT3CLX}
      EventType := Event;
      {$ELSE}
      EventType := QEvent_type(Event);
      {$ENDIF}

      {$IFDEF LINUX}
      { No Linux o Formulario pode ser Minimizado ou arrastado mesmo com um
        ShowModal sobrepondo a aplicação... Portanto vamos esconder o cursor e não
        permitir que ele se mova }
         if Assigned(fsFormMsg) then
         begin
            {$IFDEF QT3CLX}
            if EventType.type_ in [QEvent_Type_Close, QEvent_Type_Hide, QEvent_Type_Quit,
                             QEvent_Type_ShowMinimized, QEvent_Type_WindowDeActivate,
                             QEvent_Type_MouseMove, QEvent_Type_MouseButtonPress,
                             QEvent_Type_MouseButtonRelease,
                             QEvent_Type_MouseButtonDblClick,
                             QEvent_Type_DragMove,
                             QEvent_Type_Leave, QEvent_Type_Enter] then
            {$ELSE}
            if EventType in [QEventType_Close, QEventType_Hide, QEventType_Quit,
                             QEventType_ShowMinimized, QEventType_WindowDeActivate,
                             QEventType_MouseMove, QEventType_MouseButtonPress,
                             QEventType_MouseButtonRelease,
                             QEventType_MouseButtonDblClick,
                             QEventType_DragMove,
                             QEventType_Leave, QEventType_Enter] then
            {$ENDIF}
            begin
               Handled := true ;
               fsFormMsg.SetFocus ;
            end ;

            if fsFormMsg.Visible then
            begin
               Mouse.CursorPos := Point(fsFormMsg.Left,fsFormMsg.Top) ;
               Screen.Cursor   := crNone ;
            end ;
         end ;
      {$ENDIF}

      {$IFDEF QT3CLX}
      if EventType.type_ = QEvent_Type_Close then
         Handled := true

      else if ((EventType.type_ in [QEvent_Type_KeyPress]) and (fsFormMsgTeclaParaFechar = 0)) or
               ( EventType.type_ = QEvent_Type_MouseButtonPress) then
         Handled := true ;
      {$ELSE}
      if EventType = QEventType_Close then
         Handled := true

      else if ((EventType in [QEventType_KeyPress]) and (fsFormMsgTeclaParaFechar = 0)) or
               ( EventType = QEventType_MouseButtonPress) then
         Handled := true ;
      {$ENDIF}
    end;
    {$D+}
  {$ENDIF}
  procedure TACBrECFClass.FormMsgPinta( Texto : String );
  Var H, W, X, Y : Integer ;
  begin
    if Assigned( fsOnMsgAguarde ) then
       fsOnMsgAguarde( Texto ) ;

    if not Assigned( fsFormMsg ) then
       exit ;

    if fsFormMsg.Visible and ExibeMensagem then
    begin
       fsFormMsg.BringToFront ;
       {$IFDEF FMX}
       fsFormMsg.Activate;
       {$ELSE}
       fsFormMsg.SetFocus ;
       {$ENDIF}

       {$IFDEF FMX}
       if Assigned(fsOnDrawFormMsg) then
          fsOnDrawFormMsg(Texto)
       else
       begin
         with TStringList.Create do
         try
           StrictDelimiter := True;
           Delimiter := ';';
           DelimitedText := fsFormMsg.TagString;
           fsFormMsg.Canvas.Font.Size := Strings[0].ToSingle;
           fsFormMsg.Canvas.Font.Family := Strings[1];
           fsFormMsg.Canvas.Fill.Kind := TBrushKind.None;
           fsFormMsg.Canvas.Fill.Color := StringToAlphaColor(Strings[2]);
         finally
           Free;
         end;

         fsFormMsg.Width := Round(fsFormMsg.Canvas.TextWidth(Texto)) + 25;
         fsFormMsg.Height := Round(fsFormMsg.Canvas.TextHeight(Texto))+ 15;
         Application.ProcessMessages; //Se não adicionar essa linha, o firemonkey não faz a pintura o texto.
         fsFormMsg.Canvas.BeginScene; //Todo canvas no firemonkey é necessário este bloco de proteção.
         try
            fsFormMsg.Canvas.Stroke.Thickness := 1; //Espessura do pincel.
            fsFormMsg.Canvas.FillText(TRectF.Create(0, 0, fsFormMsg.Width, fsFormMsg.Height), Texto, True, 100, [], TTextAlign.Center, TTextAlign.Center);
         finally
           fsFormMsg.Canvas.EndScene;
         end;
       end;
      {$ELSE}

       with fsFormMsg.Canvas do      { Pintando <Texto> no Canvas do fpFormMsg }
       begin
          H := Trunc(TextHeight(Texto) + 10) ;    { Calcula o tamanho do Texto }
          W := Trunc(TextWidth (Texto) + 20) ;

          { Ajusta o Form para caber o Texto }
          if (abs(W - fsFormMsg.Width ) > 4) or
             (abs(H - fsFormMsg.Height) > 4) then
          begin
             fsFormMsg.Width  := W ;
             fsFormMsg.Height := H ;
             fsFormMsg.Position := poMainFormCenter ;
          end ;

          Brush.Color := fsFormMsg.Color ;
          Font.Color  := fsFormMsg.Font.Color ;
          Pen.Color   := fsFormMsg.Font.Color ;
          Rectangle(fsFormMsg.ClientRect);
         {$IFDEF VisualCLX}
          X := 0 ;
          Y := 0 ;
          TextRect(fsFormMsg.ClientRect,X,Y, Texto, 36 ) ; { 36 = No Centro }
         {$ELSE}
          { Na VCL nao tem como centralizar no Form nem quebra de Linhas }
          Texto := StringReplace( Texto, #10, ' ', [rfReplaceAll,rfIgnoreCase] ) ;
          X := 10 ;
          Y := 5 ;
          TextRect(fsFormMsg.ClientRect,X,Y, Texto ) ;
         {$ENDIF}
       end ;
      {$ENDIF}

       if Assigned( fpDevice ) then
          if fpDevice.ProcessMessages then
             Application.ProcessMessages;
    end ;
  end;

  function TACBrECFClass.FormMsgExibe : Boolean;
  begin
    Result := (ExibeMensagem or BloqueiaMouseTeclado) and
              (AguardaImpressao or ((TimeOut - TempoInicioMsg) > 1) ) and
              FormMsgControla and
              {$IFDEF FMX} Assigned(Application.MainForm) and Application.MainForm.Visible{$ELSE} Application.ShowMainForm{$ENDIF} ;
  end;

  {$IFDEF LINUX}
   {$IFNDEF FPC}
    procedure TACBrECFClass.FormShow(Sender: TObject);
    begin
     {$IFDEF QT3CLX}
      fsFormMsg.Handle.grabKeyboard;
      fsFormMsg.Handle.releaseKeyboard;
     {$ELSE}
      QWidget_grabKeyboard(fsFormMsg.Handle);
      QWidget_releaseKeyboard(fsFormMsg.Handle);
     {$ENDIF}
    end;
   {$ENDIF}
  {$ENDIF}
{$ENDIF}

procedure TACBrECFClass.ProgramarBitmapPromocional(const AIndice: Integer;
  const APathArquivo: AnsiString; const AAlinhamento: TACBrAlinhamento);
begin
  ErroAbstract('ProgramarBitmapPromocional');
end;

function TACBrECFClass.PossuiTagCodBarra(const ATexto: String): Boolean;
var
  I: Integer;
begin
  I := low(cTAGS_BARRAS);
  Result := False;
  while (not Result) and (I <= High(cTAGS_BARRAS)) do
  begin
    Result := (pos(cTAGS_BARRAS[I], ATexto) > 0);
    Inc( I );
  end;
end;

function TACBrECFClass.TraduzirTag(const ATag: AnsiString): AnsiString;
begin
  Result := '';
end;

function TACBrECFClass.TraduzirTagBloco(const ATag, Conteudo : AnsiString
   ) : AnsiString ;
var
   LowerTag : String ;
begin
  LowerTag := LowerCase( ATag );

  Result := TraduzirTag( LowerTag ) + Conteudo +
            TraduzirTag( '</'+copy(LowerTag,2,Length(LowerTag)) );

end ;

function TACBrECFClass.CodificarPaginaDeCodigoECF(ATexto: String): AnsiString;
begin
  if fpPaginaDeCodigo > 0 then
     Result := TranslateString( ACBrStrToAnsi( ATexto ), fpPaginaDeCodigo )
  else
     Result := TiraAcentos( ATexto );
end ;

function TACBrECFClass.DecodificarPaginaDeCodigoECF(ATexto : AnsiString
   ) : String ;
begin
  if fpPaginaDeCodigo > 0 then
     Result := ACBrStr( TranslateString( ATexto, 0, fpPaginaDeCodigo ) )
  else
     Result := ACBrStr( ATexto ) ;
end ;

procedure TACBrECFClass.PafMF_GerarCAT52(const DataInicial,
  DataFinal: TDateTime; const DirArquivos: String; NumeroSerie: String);
var
  NomeArquivo: String;
  DataArquivo: TDateTime;
begin
  if NumeroSerie = '' then
    NumeroSerie := GetNumSerie;

  DataArquivo := DataInicial;
  repeat
    NomeArquivo := IncludeTrailingPathDelimiter( DirArquivos ) +
                   NomeArqCAT52( RFDID, NumeroSerie, DataArquivo );
    Self.ArquivoMFD_DLL(DataArquivo, DataArquivo, NomeArquivo, [docTodos], finNFPTDM);

    DataArquivo := IncDay( DataArquivo, 1 );
  until DataArquivo > DataFinal;
end;


{ TACBrECFDadosRZ }

procedure TACBrECFDadosRZ.SetDataDoMovimento(AValue: TDateTime);
begin
   if fsDataDoMovimento=AValue then Exit;
   fsDataDoMovimento := DateOf( AValue );
end;

constructor TACBrECFDadosRZ.Create;
begin
   fsTotalizadoresNaoFiscais := TACBrECFComprovantesNaoFiscais.Create;
   fsRelatorioGerencial      := TACBrECFRelatoriosGerenciais.Create;
   fsMeiosDePagamento        := TACBrECFFormasPagamento.Create;
   fsTodasAliquotas          := TACBrECFAliquotas.Create;
   fsICMS                    := TACBrECFAliquotas.Create(False);
   fsISSQN                   := TACBrECFAliquotas.Create(False);

   Clear ;
end;

procedure TACBrECFDadosRZ.Clear;
begin
   fsTotalizadoresNaoFiscais.Clear ;
   fsRelatorioGerencial.Clear ;
   fsMeiosDePagamento.Clear ;
   fsICMS.Clear ;
   fsISSQN.Clear ;
   fsTodasAliquotas.Clear;

   fsCOO                         := -1 ;
   fsCFD                         := -1 ;
   fsCancelamentoISSQN           := -1 ;
   fsGNFC                        := -1 ;
   fsCRO                         := -1 ;
   fsValorVendaBruta             := -1 ;
   fsAcrescimoICMS               := -1 ;
   fsDescontoICMS                := -1 ;
   fsNaoTributadoICMS            := -1 ;
   fsCRZ                         := -1 ;
   fsGRG                         := -1 ;
   fsValorGrandeTotal            := -1 ;
   fsAcrescimoISSQN              := -1 ;
   fsNaoTributadoISSQN           := -1 ;
   fsIsentoICMS                  := -1 ;
   fsSubstituicaoTributariaICMS  := -1 ;
   fsTotalOperacaoNaoFiscal      := -1 ;
   fsDescontoISSQN               := -1 ;
   fsCancelamentoICMS            := -1 ;
   fsGNF                         := -1 ;
   fsIsentoISSQN                 := -1 ;
   fsSubstituicaoTributariaISSQN := -1 ;
   fsVendaLiquida                := -1 ;
   fsCFC                         := -1 ;
   fsCCF                         := -1 ;
   fsTotalISSQN                  := -1 ;
   fsTotalICMS                   := -1 ;
   fsCDC                         := -1 ;
   fsCCDC                        := -1 ;
   fsNCN                         := -1 ;
   fsCancelamentoOPNF            := -1 ;
   fsAcrescimoOPNF               := -1 ;
   fsDescontoOPNF                := -1 ;
   fsTotalTroco                  := -1;
   fsDataDoMovimento             := 0 ;
   fsDataDaImpressora            := 0 ;
   fsDataHoraEmissao             := 0 ;
   fsNumeroCOOInicial            := '' ;
   fsNumeroDoECF                 := '' ;
   fsNumeroDeSerie               := '' ;
   fsNumeroDeSerieMFD            := '' ;
   fsNumeroDaLoja                := '' ;
end ;

procedure TACBrECFDadosRZ.CalculaValoresVirtuais;
Var
  I : Integer ;
  V: Double;
begin
  // Computando Total de Operações não fiscais //
  if (fsTotalOperacaoNaoFiscal < 0) then
  begin
    fsTotalOperacaoNaoFiscal := IfThen(fsTotalizadoresNaoFiscais.Count > 0, 0, -1) ;

    For I := 0 to fsTotalizadoresNaoFiscais.Count-1 do
      fsTotalOperacaoNaoFiscal := fsTotalOperacaoNaoFiscal + fsTotalizadoresNaoFiscais[I].Total;
  end;

  // Computando Total de ICMS //
  if (fsTotalICMS < 0) then
  begin
    fsTotalICMS := IfThen(fsICMS.Count > 0, 0, -1) ;

    For I := 0 to fsICMS.Count-1 do
      fsTotalICMS := fsTotalICMS + fsICMS[I].Total;
  end;

  // Computando Total de ISSQN; //
  if (fsTotalISSQN < 0) then
  begin
    fsTotalISSQN := IfThen(fsISSQN.Count > 0, 0, -1) ;

    For I := 0 to fsISSQN.Count-1 do
      fsTotalISSQN := fsTotalISSQN + fsISSQN[I].Total;
  end;

  // Computando a Venda Bruta //
  if (fsValorVendaBruta < 0) then
  begin
    V := 0 ;

    // ICMS //
    if (IsentoICMS > -1) then
       V := V + IsentoICMS;

    if (NaoTributadoICMS > -1) then
       V := V + NaoTributadoICMS;

    if (SubstituicaoTributariaICMS > -1) then
       V := V + SubstituicaoTributariaICMS;

    if (DescontoICMS > -1) then
       V := V + DescontoICMS;

    if (CancelamentoICMS > -1) then
       V := V + CancelamentoICMS;

    // ISSQN //
    if (IsentoISSQN > -1) then
       V := V + IsentoISSQN;

    if (NaoTributadoISSQN > -1) then
       V := V + NaoTributadoISSQN;

    if (SubstituicaoTributariaISSQN > -1) then
       V := V + SubstituicaoTributariaISSQN;

    if (DescontoISSQN > -1) then
       V := V + DescontoISSQN;

    if (CancelamentoISSQN > -1) then
       V := V + CancelamentoISSQN;

{
    // OPNF //
    if (AcrescimoOPNF > -1) then
       V := V + AcrescimoOPNF;

    if (DescontoOPNF > -1) then
       V := V + DescontoOPNF;

    if (CancelamentoOPNF > -1) then             // Entra ?
       V := V + CancelamentoOPNF;
}
    // Aliquotas //
    For I := 0 to TodasAliquotas.Count-1 do
       V := V + TodasAliquotas[I].Total;

    if V > 0 then
       fsValorVendaBruta := V;
  end;

  // Computando a Venda Líquida //
  if (fsVendaLiquida < 0) and (ValorVendaBruta > -1) then
  begin
    fsVendaLiquida := ValorVendaBruta ;

    if (CancelamentoICMS > -1) then
      fsVendaLiquida := fsVendaLiquida - CancelamentoICMS ;

    if (DescontoICMS > -1) then
      fsVendaLiquida := fsVendaLiquida - DescontoICMS ;

    if (TotalISSQN > -1) then
      fsVendaLiquida := fsVendaLiquida - TotalISSQN;

    if (CancelamentoISSQN > -1) then
      fsVendaLiquida := fsVendaLiquida - CancelamentoISSQN;

    if (DescontoISSQN > -1) then
      fsVendaLiquida := fsVendaLiquida - DescontoISSQN;

    if (SubstituicaoTributariaISSQN > -1) then
      fsVendaLiquida := fsVendaLiquida - SubstituicaoTributariaISSQN ;

    if (NaoTributadoISSQN > -1) then
      fsVendaLiquida := fsVendaLiquida - NaoTributadoISSQN ;

    if (IsentoISSQN > -1) then
      fsVendaLiquida := fsVendaLiquida - IsentoISSQN ;
  end;
end;

function TACBrECFDadosRZ.MontaDadosReducaoZ: String;
Var
  I: Integer ;
  S: String;
begin
  Result := '[ECF]' + sLineBreak ;

  // Apenas grava no .INI os valores que foram realmente preenchidos pelo retorno do ECF //
  if DataDaImpressora > 0 then
     Result := Result + 'DataECF = ' + FormatDateTime('dd/mm/yy', DataDaImpressora) + sLineBreak ;
  if DataDoMovimento > 0 then
     Result := Result + 'DataMovimento = ' + FormatDateTime('dd/mm/yy', DataDoMovimento) + sLineBreak ;
  if DataHoraEmissao > 0 then
     Result := Result + 'DataHoraEmissao = ' + FormatDateTime('dd/mm/yy hh:nn:ss', DataHoraEmissao) + sLineBreak ;
  if NumeroDeSerie <> '' then
     Result := Result + 'NumSerie = '      + NumeroDeSerie               + sLineBreak ;
  if NumeroDeSerieMFD <> '' then
     Result := Result + 'NumSerieMFD = '   + NumeroDeSerieMFD            + sLineBreak ;
  if NumeroDoECF <> '' then
     Result := Result + 'NumECF = '        + NumeroDoECF                 + sLineBreak ;
  if NumeroDaLoja <> '' then
     Result := Result + 'NumLoja = '       + NumeroDaLoja                + sLineBreak ;
  if NumeroCOOInicial <> '' then
     Result := Result + 'NumCOOInicial = ' + NumeroCOOInicial            + sLineBreak ;
  if COO > -1 then
     Result := Result + 'NumCOO = '        + FormatFloat('000000', COO)  + sLineBreak ;
  if CRZ > -1 then
     Result := Result + 'NumCRZ = '        + FormatFloat('000000', CRZ)  + sLineBreak ;
  if CRO > -1 then
     Result := Result + 'NumCRO = '        + FormatFloat('000000', CRO)  + sLineBreak ;
  if GNF > -1 then
     Result := Result + 'NumGNF = '        + FormatFloat('000000', GNF)  + sLineBreak ;
  if CCF > -1 then
     Result := Result + 'NumCCF = '        + FormatFloat('000000', CCF)  + sLineBreak ;
  if CFD > -1 then
     Result := Result + 'NumCFD = '        + FormatFloat('000000', CFD)  + sLineBreak ;
  if CDC > -1 then
     Result := Result + 'NumCDC = '        + FormatFloat('000000', CDC)  + sLineBreak ;
  if GRG > -1 then
     Result := Result + 'NumGRG = '        + FormatFloat('000000', GRG)  + sLineBreak ;
  if GNFC > -1 then
  begin
     Result := Result + 'NumNFC = '        + FormatFloat('000000', GNFC) + sLineBreak ;
     Result := Result + 'NumGNFC = '       + FormatFloat('000000', GNFC) + sLineBreak ;
  end;
  if CFC > -1 then
     Result := Result + 'NumCFC = '        + FormatFloat('000000', CFC)  + sLineBreak ;
  if NCN > -1 then
     Result := Result + 'NumNCN = '        + FormatFloat('000000', NCN)  + sLineBreak ;
  if CCDC > -1 then
     Result := Result + 'NumCCDC = '       + FormatFloat('000000', CCDC) + sLineBreak ;

  Result := Result + sLineBreak + '[Totalizadores]' + sLineBreak ;

  if ValorVendaBruta > -1 then
     Result := Result + 'VendaBruta = '              + FormatFloat('0.00',ValorVendaBruta)        + sLineBreak ;
  if VendaLiquida > -1 then
     Result := Result + 'VendaLiquida = '            + FormatFloat('0.00',VendaLiquida)           + sLineBreak ;
  if ValorGrandeTotal > -1 then
     Result := Result + 'GrandeTotal = '             + FormatFloat('0.00',ValorGrandeTotal)       + sLineBreak ;
  if DescontoICMS > -1 then
     Result := Result + 'TotalDescontos = '          + FormatFloat('0.00',DescontoICMS)           + sLineBreak ;
  if CancelamentoICMS > -1 then
     Result := Result + 'TotalCancelamentos = '      + FormatFloat('0.00',CancelamentoICMS)       + sLineBreak ;
  if AcrescimoICMS > -1 then
     Result := Result + 'TotalAcrescimos = '         + FormatFloat('0.00',AcrescimoICMS)          + sLineBreak ;
  if DescontoISSQN > -1 then
     Result := Result + 'TotalDescontosISSQN = '     + FormatFloat('0.00',DescontoISSQN)          + sLineBreak ;
  if CancelamentoISSQN > -1 then
     Result := Result + 'TotalCancelamentosISSQN = ' + FormatFloat('0.00',CancelamentoISSQN)      + sLineBreak ;
  if AcrescimoISSQN > -1 then
     Result := Result + 'TotalAcrescimosISSQN = '    + FormatFloat('0.00',AcrescimoISSQN)         + sLineBreak ;
  if TotalOperacaoNaoFiscal > -1 then
     Result := Result + 'TotalNaoFiscal = '          + FormatFloat('0.00',TotalOperacaoNaoFiscal) + sLineBreak ;
  if DescontoOPNF > -1 then
     Result := Result + 'TotalDescontosOPNF = '      + FormatFloat('0.00',DescontoOPNF)           + sLineBreak ;
  if CancelamentoOPNF > -1 then
     Result := Result + 'TotalCancelamentosOPNF = '  + FormatFloat('0.00',CancelamentoOPNF)       + sLineBreak ;
  if AcrescimoOPNF > -1 then
     Result := Result + 'TotalAcrescimosOPNF = '     + FormatFloat('0.00',AcrescimoOPNF)          + sLineBreak ;
  if TotalTroco > -1 then
     Result := Result + 'TotalTroco = '              + FormatFloat('0.00',TotalTroco)             + sLineBreak ;

  if TodasAliquotas.Count > 0 then
     Result := Result + sLineBreak + '[Aliquotas]' + sLineBreak ;

  For I := 0 to TodasAliquotas.Count-1 do
  begin
     Result := Result +
               FormatFloat('00', I+1 ) +
               TodasAliquotas[I].Tipo +
               IntToStrZero(Round(TodasAliquotas[I].Aliquota*100),4) + ' = ' +
               FormatFloat('0.00',TodasAliquotas[I].Total) + sLineBreak ;
  end ;

  Result := Result + sLineBreak + '[OutrasICMS]' + sLineBreak ;

  if TotalICMS > -1 then
     Result := Result + 'TotalICMS = '                        + FormatFloat('0.00',TotalICMS)                   + sLineBreak ;
  if TotalISSQN > -1 then
     Result := Result + 'TotalISSQN = '                       + FormatFloat('0.00',TotalISSQN)                  + sLineBreak ;
  if SubstituicaoTributariaICMS > -1 then
     Result := Result + 'TotalSubstituicaoTributaria = '      + FormatFloat('0.00',SubstituicaoTributariaICMS)  + sLineBreak ;
  if NaoTributadoICMS > -1 then
     Result := Result + 'TotalNaoTributado = '                + FormatFloat('0.00',NaoTributadoICMS)            + sLineBreak ;
  if IsentoICMS > -1 then
     Result := Result + 'TotalIsencao = '                     + FormatFloat('0.00',IsentoICMS)                  + sLineBreak ;
  if SubstituicaoTributariaISSQN > -1 then
     Result := Result + 'TotalSubstituicaoTributariaISSQN = ' + FormatFloat('0.00',SubstituicaoTributariaISSQN) + sLineBreak ;
  if NaoTributadoISSQN > -1 then
     Result := Result + 'TotalNaoTributadoISSQN = '           + FormatFloat('0.00',NaoTributadoISSQN)           + sLineBreak ;
  if IsentoISSQN > -1 then
     Result := Result + 'TotalIsencaoISSQN = '                + FormatFloat('0.00',IsentoISSQN)                 + sLineBreak ;

  if TotalizadoresNaoFiscais.Count > 0 then
     Result := Result + sLineBreak + '[NaoFiscais]' + sLineBreak ;
  S := '';
  For I := 0 to TotalizadoresNaoFiscais.Count-1 do
  begin
     Result := Result + PadRight(TotalizadoresNaoFiscais[I].Indice,2) + '_' +
                        TotalizadoresNaoFiscais[I].Descricao + ' = ' +
                        FormatFloat('0.00',TotalizadoresNaoFiscais[I].Total) + sLineBreak ;
     S := S + 'CON_' + TotalizadoresNaoFiscais[I].Descricao +' = '+
          FormatFloat('0000', TotalizadoresNaoFiscais[I].Contador) + sLineBreak ;
  end;
  Result := Result + S + sLineBreak ;

  if RelatorioGerencial.Count > 0 then
     Result := Result + sLineBreak + '[RelatoriosGerenciais]' + sLineBreak ;
  For I := 0 to RelatorioGerencial.Count-1 do
  begin
     Result := Result + PadRight(RelatorioGerencial[I].Indice,2) + '_' +
                        RelatorioGerencial[I].Descricao +' = '+
                        FormatFloat('0000', RelatorioGerencial[I].Contador) + sLineBreak ;
  end ;

  if MeiosDePagamento.Count > 0 then
     Result := Result + sLineBreak + '[MeiosDePagamento]' + sLineBreak ;
  For I := 0 to MeiosDePagamento.Count-1 do
  begin
     Result := Result + PadRight(MeiosDePagamento[I].Indice,2) + '_' +
                        MeiosDePagamento[I].Descricao + ' = ' +
                        FormatFloat('0.00',MeiosDePagamento[I].Total) + sLineBreak ;
  end;
end;

procedure TACBrECFDadosRZ.AdicionaAliquota(AliqZ: TACBrECFAliquota);
begin
  fsTodasAliquotas.Add( AliqZ );

  if AliqZ.Tipo = 'S' then
    fsISSQN.Add( AliqZ )
  else
    fsICMS.Add( AliqZ );
end;

destructor TACBrECFDadosRZ.Destroy;
begin
   fsTotalizadoresNaoFiscais.Free;
   fsRelatorioGerencial.Free;
   fsMeiosDePagamento.Free;
   fsICMS.Free;
   fsISSQN.Free;
   fsTodasAliquotas.Free;

   inherited Destroy ;
end;

{ TACBrECFRodape }

procedure TACBrECFRodape.SetMD5(AValue : String) ;
Var
  P : Integer ;
  UpValue : String ;
begin
  if fsMD5 = AValue then Exit ;

  // Removendo o pre-fixo "MD5", "MD5:" e "MD-5:"
  UpValue := UpperCase(AValue);
  P       := 1 ;
  if LeftStr(UpValue,5) = 'MD-5:' then
     P := 6
  else if LeftStr(UpValue,4) = 'MD5:' then
     P := 5
  else if LeftStr(UpValue,3) = 'MD5' then
     P := 4 ;

  fsMD5 := copy(AValue,P,Length(AValue));
end ;

constructor TACBrECFRodape.Create;
begin
  inherited;
  fsMD5        := EmptyStr;
  fsCupomMania := False;
  fsMinasLegal := False;
  fsParaibaLegal := False;

  fsNotaLegalDF := TACBrECFRodapeNotaLegalDF.Create;
  fsNotaLegalDF.Imprimir := False;
  fsNotaLegalDF.fsProgramaDeCredito := False;

  fsRestaurante := TACBrECFRodapeRestaurante.Create;
  fsRestaurante.Imprimir := False;

  fsImposto := TACBrECFRodapeImposto.Create;

  fsPostoComustivel := TACBRRodapeAbastecimentos.Create;
  fsPostoComustivel.Imprimir := False;

  Self.Clear;
end;

destructor TACBrECFRodape.Destroy;
begin
  FreeAndNil(fsNotaLegalDF);
  FreeAndNil(fsRestaurante);
  FreeAndNil(fsImposto);
  FreeAndNil(fsPostoComustivel);
  inherited;
end;

procedure TACBrECFRodape.Clear;
begin
  fsDav        := EmptyStr;
  fsDavFarm    := EmptyStr;
  fsDavOs      := EmptyStr;
  fsPreVenda   := EmptyStr;
  fsNF         := EmptyStr;

  fsRestaurante.Imprimir:= False;
  fsRestaurante.CER    := 0;
  fsRestaurante.COO    := 0;
  fsRestaurante.ECF    := 0;

  // restante dos dados do nota legal DF não deve limpar
  fsNotaLegalDF.fsValorICMS := 0.00;
  fsNotaLegalDF.fsValorISS  := 0.00;

  fsImposto.ValorAproximado := 0.00;
  fsImposto.ValorAproximadoFederal := 0.00;
  fsImposto.ValorAproximadoEstadual := 0.00;
  fsImposto.ValorAproximadoMunicipal := 0.00;
  fsImposto.Fonte := '';
  fsImposto.Chave := '';
  fsImposto.ModoCompacto := False;
end;

{ TACBRodapeRAbastecimentos }

function TACBRRodapeAbastecimentos.Add(Obj: TACBRRodapeAbastecimento): Integer;
begin
  Result := inherited Add(Obj);
end;

function TACBRRodapeAbastecimentos.GetObject(Index: Integer): TACBRRodapeAbastecimento;
begin
  Result := TACBRRodapeAbastecimento(inherited Items[Index]);
end;

procedure TACBRRodapeAbastecimentos.Insert(Index: Integer; Obj: TACBRRodapeAbastecimento);
begin
  inherited Insert(Index, Obj);
end;

function TACBRRodapeAbastecimentos.New: TACBRRodapeAbastecimento;
begin
  Result := TACBRRodapeAbastecimento.Create;
  Add(Result);
end;

procedure TACBRRodapeAbastecimentos.SetObject(Index: Integer; Item: TACBRRodapeAbastecimento);
begin
  inherited Items[Index] := Item;
end;

end.
