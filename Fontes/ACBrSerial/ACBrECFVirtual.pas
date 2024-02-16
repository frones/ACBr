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

unit ACBrECFVirtual ;
{ Implementa a lógica e memória necessária para emular uma Impressora Fiscal,
  porém não possui propriedade ou metodos para Impressão.
  Deve ser herdado para a criação de um componente que implemente Impressão e
  outras funcionalidades }

interface
uses
  Classes, Math, SysUtils, IniFiles,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  {$IFDEF COMPILER6_UP}
    DateUtils, StrUtils
  {$ELSE}
    ACBrD5, Windows
  {$ENDIF},
  ACBrECFClass, ACBrDevice ,ACBrBase;

type

TACBrECFVirtualClass = class;
TACBrECFVirtualClassCupom = class;

{ TACBrECFVirtualClassItemCupom }

TACBrECFVirtualClassItemCupom = class
  private
    fsECFVirtualClassCupom: TACBrECFVirtualClassCupom;

    fsAliqPos: Integer;
    fsCodDepartamento: Integer;
    fsDescricao: String;
    fsSequencia: Integer;
    fsUnidade: String;
    fsValorUnit: Double;
    fsQtd: Double;
    fsCodigo: String;
    fsDescAcres :Double;
    function GetAsString: String;
    procedure SetAsString(const AValue: String);

  public
    constructor Create(AECFVirtualClassCupom: TACBrECFVirtualClassCupom);

    property Sequencia : Integer  read fsSequencia write fsSequencia ;
    property Codigo    : String  read fsCodigo    write fsCodigo    ;
    property Descricao : String  read fsDescricao write fsDescricao ;
    property Qtd       : Double  read fsQtd       write fsQtd       ;
      { Se Qtd = 0 Item foi cancelado }
    property ValorUnit : Double  read fsValorUnit write fsValorUnit ;
    property DescAcres : Double  read fsDescAcres write fsDescAcres ;
    property Unidade   : String  read fsUnidade   write fsUnidade ;
    property CodDepartamento : Integer read fsCodDepartamento write fsCodDepartamento ;
    property AliqPos   : Integer read fsAliqPos    write fsAliqPos;

    property AsString : String read GetAsString write SetAsString;

    function TotalLiquido: Double;
    function TotalBruto: Double;
end;

{ TACBrECFVirtualClassItensCupom }

TACBrECFVirtualClassItensCupom = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrECFVirtualClassItemCupom>{$EndIf})
  protected
    procedure SetObject (Index: Integer; Item: TACBrECFVirtualClassItemCupom);
    function GetObject (Index: Integer): TACBrECFVirtualClassItemCupom;
  public
    function New(AECFVirtualClassCupom: TACBrECFVirtualClassCupom): TACBrECFVirtualClassItemCupom;
    function Add (Obj: TACBrECFVirtualClassItemCupom): Integer;
    procedure Insert (Index: Integer; Obj: TACBrECFVirtualClassItemCupom);
    property Objects [Index: Integer]: TACBrECFVirtualClassItemCupom
      read GetObject write SetObject; default;
end;

{ TACBrECFVirtualClassPagamentoCupom }

TACBrECFVirtualClassPagamentoCupom = class
  private
    fsObservacao: String;
    fsValorPago: Double;
    fsPosFPG: Integer;
    function GetAsString: String;
    procedure SetAsString(const AValue: String);
  public
    property PosFPG    : Integer read fsPosFPG     write fsPosFPG;
    property ValorPago : Double  read fsValorPago  write fsValorPago;
    property Observacao: String  read fsObservacao write fsObservacao;

    property AsString : String read GetAsString write SetAsString;
end;

{ TACBrECFVirtualClassPagamentosCupom }

TACBrECFVirtualClassPagamentosCupom = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrECFVirtualClassPagamentoCupom>{$EndIf})
  protected
    procedure SetObject (Index: Integer; Item: TACBrECFVirtualClassPagamentoCupom);
    function GetObject (Index: Integer): TACBrECFVirtualClassPagamentoCupom;
  public
    function New: TACBrECFVirtualClassPagamentoCupom;
    function Add (Obj: TACBrECFVirtualClassPagamentoCupom): Integer;
    procedure Insert (Index: Integer; Obj: TACBrECFVirtualClassPagamentoCupom);
    property Objects [Index: Integer]: TACBrECFVirtualClassPagamentoCupom
      read GetObject write SetObject; default;
end;

{ TACBrECFVirtualClassCNFCupom }

TACBrECFVirtualClassCNFCupom = class
  private
    fsObservacao: String;
    fsSequencia: Integer;
    fsValor  : Double;
    fsPosCNF : Integer;
    function GetAsString: String;
    procedure SetAsString(const AValue: String);
  public
    property Sequencia : Integer  read fsSequencia  write fsSequencia ;
    property PosCNF    : Integer  read fsPosCNF     write fsPosCNF   ;
    property Valor     : Double   read fsValor      write fsValor;
    property Observacao: String   read fsObservacao write fsObservacao;

    property AsString  : String   read GetAsString write SetAsString;
end;

{ TACBrECFVirtualClassCNFsCupom }

TACBrECFVirtualClassCNFsCupom = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrECFVirtualClassCNFCupom>{$EndIf})
  protected
    procedure SetObject (Index: Integer; Item: TACBrECFVirtualClassCNFCupom);
    function GetObject (Index: Integer): TACBrECFVirtualClassCNFCupom;
  public
    function New: TACBrECFVirtualClassCNFCupom;
    function Add (Obj: TACBrECFVirtualClassCNFCupom): Integer;
    procedure Insert (Index: Integer; Obj: TACBrECFVirtualClassCNFCupom);
    property Objects [Index: Integer]: TACBrECFVirtualClassCNFCupom
      read GetObject write SetObject; default;
end;

{ TACBrECFVirtualClassAliquotaCupom }

TACBrECFVirtualClassAliquotaCupom = class
  private
    fsAliqPos: Integer;
    fsAliqValor: Double;
    fsRateio: Double;
    fsTipo: Char;
    fsTotal: Double;
    function GetAsString: String;
    procedure SetAsString(const AValue: String);
  public
    property AliqPos   : Integer read fsAliqPos   write fsAliqPos;
    property AliqValor : Double  read fsAliqValor write fsAliqValor;
    property Total     : Double  read fsTotal     write fsTotal;
    property Rateio    : Double  read fsRateio    write fsRateio;
    property Tipo      : Char    read fsTipo      write fsTipo;

    property AsString  : String   read GetAsString write SetAsString;

    function TotalLiquido: Double;
end;


{ TACBrECFVirtualClassAliquotasCupom }

TACBrECFVirtualClassAliquotasCupom = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrECFVirtualClassAliquotaCupom>{$EndIf})
  protected
    procedure SetObject (Index: Integer; Item: TACBrECFVirtualClassAliquotaCupom);
    function GetObject (Index: Integer): TACBrECFVirtualClassAliquotaCupom;
  public
    function New: TACBrECFVirtualClassAliquotaCupom;
    function Add (Obj: TACBrECFVirtualClassAliquotaCupom): Integer;
    procedure Insert (Index: Integer; Obj: TACBrECFVirtualClassAliquotaCupom);
    function Find( APos: Integer): TACBrECFVirtualClassAliquotaCupom;
    property Objects [Index: Integer]: TACBrECFVirtualClassAliquotaCupom
      read GetObject write SetObject; default;
end;

{ TACBrECFVirtualClassCupom }

TACBrECFVirtualClassCupom = class
  private
    fpECFVirtualClasse: TACBrECFVirtualClass;

    fpChaveDFe : String;

    fpAliquotasCupom  : TACBrECFVirtualClassAliquotasCupom;
    fpItensCupom      : TACBrECFVirtualClassItensCupom;
    fpPagamentosCupom : TACBrECFVirtualClassPagamentosCupom;
    fpCNFsCupom       : TACBrECFVirtualClassCNFsCupom;

    fpTotalPago        : Currency;
    fpSubtotalICMS     : Currency;
    fpSubtotalISSQN    : Currency;
    fpDescAcresSubtotalICMS : Currency;
    fpDescAcresSubtotalISSQN: Currency;

    function GetDescAcresSubtotal: Currency;
    function GetSubtotal: Currency;
    procedure SetDescAcresSubtotal(AValue: Currency);
    procedure VerificaFaixaItem(NumItem: Integer);

    function SomaAliquota(aAliq: TACBrECFAliquota; AValor: Currency):
       TACBrECFVirtualClassAliquotaCupom;
    procedure SubtraiAliquota(AAliqPos: Integer; AValor: Currency);
  public
    Constructor Create(AECFVirtualClasse: TACBrECFVirtualClass);
    Destructor Destroy; override;
    procedure Clear;

    function VendeItem(const ACodigo, ADescricao: String; AQtd, AValorUnitario: Double;
      ADescAcres: Double; AAliq: TACBrECFAliquota; const AUnidade: String;
      ACodDepartamento: Integer): TACBrECFVirtualClassItemCupom;
    procedure DescAcresItem(NumItem: Integer; ADescAcres: Double);
    procedure CancelaDescontoAcrescimoItem( NumItem : Integer;
      const TipoAcrescimoDesconto: String = 'D') ;
    procedure CancelaItem(NumItem: Integer);

    function EfetuaPagamento(AValor: Double; const AObservacao: String; APosFPG: Integer):
       TACBrECFVirtualClassPagamentoCupom;

    function RegistraCNF(AValor: Currency; const AObservacao: String; APosCNF: Integer):
       TACBrECFVirtualClassCNFCupom;
    procedure CancelaCNF(NumItem: Integer);

    procedure LoadFromINI( AIni: TCustomIniFile);
    procedure SaveToINI( AIni: TCustomIniFile);

    property Itens      : TACBrECFVirtualClassItensCupom      read fpItensCupom;
    property Pagamentos : TACBrECFVirtualClassPagamentosCupom read fpPagamentosCupom;
    property CNF        : TACBrECFVirtualClassCNFsCupom       read fpCNFsCupom;
    property Aliquotas  : TACBrECFVirtualClassAliquotasCupom  read fpAliquotasCupom;

    property SubtotalICMS     : Currency read fpSubtotalICMS;
    property SubtotalISSQN    : Currency read fpSubtotalISSQN;
    property SubTotal         : Currency read GetSubtotal;
    property TotalPago        : Currency read fpTotalPago;
    property DescAcresSubtotal: Currency read GetDescAcresSubtotal
      write SetDescAcresSubtotal;

    property DescAcresSubtotalICMS : Currency read fpDescAcresSubtotalICMS;
    property DescAcresSubtotalISSQN: Currency read fpDescAcresSubtotalISSQN;

    property ECFVirtualClasse: TACBrECFVirtualClass read fpECFVirtualClasse;

    property ChaveDFe : String read fpChaveDFe   write fpChaveDFe;
end;

TACBrECFVirtualLerGravarINI = procedure(ConteudoINI: TStrings; var Tratado: Boolean) of object;
TACBrECFVirtualQuandoCancelarCupom = procedure(const NumCOOCancelar: Integer;
  CupomVirtual: TACBrECFVirtualClassCupom; var PermiteCancelamento: Boolean) of object;
TACBrECFVirtualQuandoMudarEstado = procedure(const EstadoAtual: TACBrECFEstado;
  var NovoEstado: TACBrECFEstado) of object;
TACBrECFVirtualQuandoImprimirLinha = procedure(const ALinha: string) of object;

{ TACBrECFVirtual }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrECFVirtual = class( TACBrComponent )
  private
    fsECF: TACBrComponent;
    function GetCNPJ: String;
    function GetColunas: Integer;
    function GetIE: String;
    function GetIM: String;
    function GetNomeArqINI: String;
    function GetNumCRO: Integer;
    function GetNumECF: Integer;
    function GetNumSerie: String;
    function GetQuandoCancelarCupom: TACBrECFVirtualQuandoCancelarCupom;
    function GetQuandoGravarArqINI: TACBrECFVirtualLerGravarINI;
    function GetQuandoLerArqINI: TACBrECFVirtualLerGravarINI;
    function GetQuandoMudarEstado: TACBrECFVirtualQuandoMudarEstado;
    procedure SetCNPJ(const AValue: String);
    procedure SetColunas(AValue: Integer);
    procedure SetIE(const AValue: String);
    procedure SetIM(const AValue: String);
    procedure SetNomeArqINI(const AValue: String);
    procedure SetNumCRO(AValue: Integer);
    procedure SetNumECF(AValue: Integer);
    procedure SetNumSerie(const AValue: String);
    procedure SetQuandoCancelarCupom(AValue: TACBrECFVirtualQuandoCancelarCupom);
    procedure SetQuandoGravarArqINI(AValue: TACBrECFVirtualLerGravarINI);
    procedure SetQuandoLerArqINI(AValue: TACBrECFVirtualLerGravarINI);
    procedure SetQuandoMudarEstado(AValue: TACBrECFVirtualQuandoMudarEstado);
    function GetQuandoImprimirLinha: TACBrECFVirtualQuandoImprimirLinha;
    procedure SetQuandoImprimirLinha(const AValue: TACBrECFVirtualQuandoImprimirLinha);
  protected
    fpECFVirtualClass: TACBrECFVirtualClass;

    procedure SetECF(AValue: TACBrComponent); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure CreateVirtualClass ; virtual ;
  public
    constructor Create( AOwner : TComponent) ; override ;
    destructor Destroy ; override ;

    procedure LeArqINI ;
    procedure GravaArqINI ;

    property ECFVirtualClass : TACBrECFVirtualClass read fpECFVirtualClass ;

    property Colunas    : Integer read GetColunas    write SetColunas;
    property NomeArqINI : String  read GetNomeArqINI write SetNomeArqINI;
    property NumSerie   : String  read GetNumSerie   write SetNumSerie;
    property NumECF     : Integer read GetNumECF     write SetNumECF;
    property NumCRO     : Integer read GetNumCRO     write SetNumCRO;
    property CNPJ       : String  read GetCNPJ       write SetCNPJ;
    property IE         : String  read GetIE         write SetIE;
    property IM         : String  read GetIM         write SetIM;
  published
    property ECF : TACBrComponent read fsECF write SetECF ;

    property QuandoGravarArqINI : TACBrECFVirtualLerGravarINI read GetQuandoGravarArqINI
      write SetQuandoGravarArqINI ;
    property QuandoLerArqINI : TACBrECFVirtualLerGravarINI read GetQuandoLerArqINI
      write SetQuandoLerArqINI ;
    property QuandoCancelarCupom : TACBrECFVirtualQuandoCancelarCupom
      read GetQuandoCancelarCupom write SetQuandoCancelarCupom ;
    property QuandoMudarEstado : TACBrECFVirtualQuandoMudarEstado read GetQuandoMudarEstado
      write SetQuandoMudarEstado;
    property QuandoImprimirLinha : TACBrECFVirtualQuandoImprimirLinha read GetQuandoImprimirLinha
      write SetQuandoImprimirLinha;
end ;

{ Classe filha de TACBrECFClass com implementaçao para Virtual }

{ TACBrECFVirtualClass }

TACBrECFVirtualClass = class( TACBrECFClass )
  private
    fsECFVirtualOwner: TACBrECFVirtual;
    fsQuandoCancelarCupom: TACBrECFVirtualQuandoCancelarCupom;
    fsQuandoGravarArqINI: TACBrECFVirtualLerGravarINI;
    fsQuandoLerArqINI: TACBrECFVirtualLerGravarINI;
    fsQuandoMudarEstado: TACBrECFVirtualQuandoMudarEstado;
    fsQuandoImprimirLinha: TACBrECFVirtualQuandoImprimirLinha;

    function GetChaveCupom: String;
    procedure SetChaveCupom(const AValue: String);
    procedure SetColunas(AValue: Integer);
    procedure SetDevice(AValue: TACBrDevice);
    procedure SetNumCRO(AValue: Integer);
    procedure SetNumECF(AValue: Integer);
    procedure SetNumSerie(const AValue: String);
    procedure VerificaFaixaItem(NumItem: Integer);
    procedure Zera ;
    procedure ZeraCupom ;
    function CalculaNomeArqINI: String ;
  protected
    fpCupom      : TACBrECFVirtualClassCupom;
    fpNomeArqINI : String;
    fpNumSerie   : String ;
    fpNumECF     : Integer ;
    fpIE         : String ;
    fpCNPJ       : String ;
    fpPAF        : String ;
    fpIM         : String ;
    fpVerao      : Boolean ;
    fpDia        : TDateTime ;
    fpReducoesZ  : Integer ;
    fpLeiturasX  : Integer ;
    fpCuponsCancelados : Integer ;
    fpCuponsCanceladosTotalICMS: Double;
    fpCuponsCanceladosTotalISSQN: Double;
    fpCNFCancelados : Integer ;
    fpCNFCanceladosTotal : Double;
    fpCuponsCanceladosEmAberto: Integer;
    fpCuponsCanceladosEmAbertoTotalICMS: Double;
    fpCuponsCanceladosEmAbertoTotalISSQN: Double;
    fpCOOInicial : Integer;
    fpCOOFinal   : Integer;
    fpNumCRO     : Integer;
    fpNumCOO     : Integer;
    fpNumGNF     : Integer;
    fpNumGRG     : Integer;
    fpNumCDC     : Integer;
    fpNumCER     : Integer;
    fpGrandeTotal: Double;
    fpVendaBruta : Double;
    fpNumCCF     : Integer;

    fpTotalDescontosICMS  : Double;
    fpTotalAcrescimosICMS : Double;
    fpTotalDescontosISSQN : Double;
    fpTotalAcrescimosISSQN: Double;

    function GetDevice: TACBrDevice; virtual;
    function GetColunas: Integer; virtual;

    procedure AtivarVirtual ; virtual ;
    procedure INItoClass( ConteudoINI: TStrings ); virtual ;
    procedure ClasstoINI( ConteudoINI: TStrings ); virtual ;
    procedure CriarMemoriaInicial; virtual;
    procedure CriarAliquotasPadrao;
    procedure CriarFormasPagamentoPadrao;
    procedure AtualizarAliquotasMemoria;
    procedure AtualizarFormasPagamentoMemoria;
    procedure LeArqINIVirtual( ConteudoINI: TStrings ) ; virtual;
    procedure GravaArqINIVirtual( ConteudoINI: TStrings ) ; virtual ;

    Procedure AbreDia; virtual ;
    Procedure AbreDocumento ; virtual;
    Procedure AbreDocumentoVirtual ; virtual;
    Procedure VendeItemVirtual( ItemCupom: TACBrECFVirtualClassItemCupom ) ; virtual ;
    Procedure CancelaItemVendidoVirtual( NumItem : Integer ) ; virtual ;
    Procedure DescontoAcrescimoItemAnteriorVirtual(
      ItemCupom: TACBrECFVirtualClassItemCupom; PorcDesc: Double) ; virtual ;
    procedure CancelaDescontoAcrescimoItemVirtual(
      ItemCupom: TACBrECFVirtualClassItemCupom;
      TipoAcrescimoDesconto: String = 'D') ; Virtual ;
    Procedure SubtotalizaCupomVirtual( MensagemRodape : AnsiString  = '' ) ; virtual ;
    Procedure EfetuaPagamentoVirtual( Pagto  : TACBrECFVirtualClassPagamentoCupom ) ; virtual ;
    Procedure FechaCupomVirtual( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; virtual ;
    procedure VerificaPodeCancelarCupom( NumCOOCancelar: Integer = 0 ); virtual;
    Procedure CancelaCupomVirtual ; virtual ;

    Procedure LeituraXVirtual ; virtual ;
    Procedure ReducaoZVirtual(DataHora : TDateTime = 0 ) ; virtual ;
    procedure GetEstadoECFVirtual; virtual;
    procedure SetEstadoECFVirtual(const NovoEstado: TACBrECFEstado);

    Procedure AbreRelatorioGerencialVirtual(Indice: Integer = 0) ; virtual ;
    procedure AbreCupomVinculadoVirtual(COO: String; FPG: TACBrECFFormaPagamento;
      CodComprovanteNaoFiscal: String; SubtotalCupomAnterior, ValorFPG: Double ); virtual;
    Procedure FechaRelatorioVirtual ; virtual ;

    Procedure AbreNaoFiscalVirtual( CPF_CNPJ: String = ''; Nome: String = '';
       Endereco: String = '' ) ; virtual ;
    Procedure RegistraItemNaoFiscalVirtual( CNFCupom: TACBrECFVirtualClassCNFCupom ); virtual ;
    Procedure CancelaItemNaoFiscalVirtual( NumItem : Integer ); virtual;

    Procedure EnviaConsumidorVirtual ; virtual;

  protected
    function GetDataHora: TDateTime; override ;
    function GetNumCupom: String; override ;
    function GetNumGNF: String; override ;
    function GetNumGRG: String; override ;
    function GetNumCDC: String; override ;
    function GetNumCCF: String; override ;
    function GetNumCFC: String; override ;
    function GetNumGNFC: String; override ;
    function GetGrandeTotal: Double; override ;
    function GetVendaBruta: Double; override ;
    function GetTotalSubstituicaoTributaria: Double; override ;
    function GetTotalIsencao: Double; override ;
    function GetTotalNaoTributado: Double; override ;
    function GetNumReducoesZRestantes: String; override ;
    function GetTotalCancelamentosEmAberto: Double; override;
    function GetTotalCancelamentos: Double; override ;
    function GetTotalCancelamentosOPNF: Double; override;
    function GetTotalAcrescimos: Double; override ;
    function GetTotalDescontos: Double; override ;

    function GetTotalAcrescimosISSQN: Double; override;
    function GetTotalCancelamentosISSQN: Double; override;
    function GetTotalCancelamentosEmAbertoISSQN: Double; override;
    function GetTotalDescontosISSQN: Double; override;
    function GetTotalIsencaoISSQN: Double; override;
    function GetTotalNaoTributadoISSQN: Double; override;
    function GetTotalSubstituicaoTributariaISSQN: Double; override;

    function GetNumECF: String; override ;
    function GetCNPJ: String; override ;
    function GetIE: String; override ;
    function GetIM: String; override ;
    function GetPAF: String; override ;
    function GetUsuarioAtual: String; override ;
    function GetDataHoraSB: TDateTime; override ;
    function GetSubModeloECF: String ; override ;
    function GetNumCRO: String; override ;
    function GetNumCRZ: String; override ;
    function GetNumSerie: String; override ;
    function GetNumVersao: String; override ;
    function GetSubTotal: Double; override ;
    function GetTotalPago: Double; override ;

    function GetEstado: TACBrECFEstado; override ;
    function GetHorarioVerao: Boolean; override ;
    function GetArredonda : Boolean; override ;

    function GetDataMovimento: TDateTime; override;
    function GetNumCOOInicial: String; override;
    function GetNumUltimoItem: Integer; override;
 public
    Constructor Create( AECFVirtual : TACBrECFVirtual );
    Destructor Destroy  ; override ;

    procedure LeArqINI;
    procedure GravaArqINI;

    property QuandoGravarArqINI : TACBrECFVirtualLerGravarINI read fsQuandoGravarArqINI
      write fsQuandoGravarArqINI ;
    property QuandoLerArqINI    : TACBrECFVirtualLerGravarINI read fsQuandoLerArqINI
      write fsQuandoLerArqINI ;
    property QuandoCancelarCupom : TACBrECFVirtualQuandoCancelarCupom
      read fsQuandoCancelarCupom write fsQuandoCancelarCupom ;
    property QuandoMudarEstado : TACBrECFVirtualQuandoMudarEstado read fsQuandoMudarEstado
      write fsQuandoMudarEstado;
    property QuantoImprimirLinha: TACBrECFVirtualQuandoImprimirLinha read fsQuandoImprimirLinha
      write fsQuandoImprimirLinha;

    property ECFVirtual : TACBrECFVirtual read fsECFVirtualOwner ;
    property Device     : TACBrDevice     read GetDevice write SetDevice;

    property NomeArqINI : String read  fpNomeArqINI  write fpNomeArqINI ;
    Property Colunas    : Integer read GetColunas     write SetColunas;

    property NumSerie   : String read  fpNumSerie    write SetNumSerie;
    property NumECF     : Integer read fpNumECF      write SetNumECF;
    property NumCRO     : Integer read fpNumCRO      write SetNumCRO;
    property CNPJ       : String read  fpCNPJ        write fpCNPJ;
    property IE         : String read  fpIE          write fpIE;
    property IM         : String read  fpIM          write fpIM;

    property ChaveCupom : String read GetChaveCupom   write SetChaveCupom;

    procedure Ativar ; override ;
    procedure Desativar ; override ;

    Procedure AbreCupom ; override ;
    Procedure VendeItem( Codigo, Descricao : String; AliquotaECF : String;
       Qtd : Double ; ValorUnitario : Double; ValorDescontoAcrescimo : Double = 0;
       Unidade : String = ''; TipoDescontoAcrescimo : String = '%';
       DescontoAcrescimo : String = 'D'; CodDepartamento: Integer = -1 ) ; override ;
    Procedure DescontoAcrescimoItemAnterior( ValorDescontoAcrescimo : Double = 0;
       DescontoAcrescimo : String = 'D'; TipoDescontoAcrescimo : String = '%';
       NumItem : Integer = 0 ) ;  override ;
    procedure CancelaDescontoAcrescimoItem( NumItem : Integer;
      TipoAcrescimoDesconto: String = 'D') ; override ;
    Procedure CancelaItemVendido( NumItem : Integer ) ; override ;
    Procedure SubtotalizaCupom( DescontoAcrescimo : Double = 0;
       MensagemRodape : AnsiString  = '' ) ; override ;
    Procedure EfetuaPagamento( CodFormaPagto : String; Valor : Double;
       Observacao : AnsiString = ''; ImprimeVinculado : Boolean = false;
       CodMeioPagamento: Integer = 0) ; override ;
    Procedure FechaCupom( Observacao : AnsiString = ''; IndiceBMP : Integer = 0) ; override ;
    Procedure CancelaCupom( NumCOOCancelar: Integer = 0 ) ; override ;

    Procedure LeituraX ; override ;
    Procedure ReducaoZ(DataHora : TDateTime = 0 ) ; override ;

    Procedure AbreRelatorioGerencial(Indice: Integer = 0) ; override ;
    Procedure LinhaRelatorioGerencial( Linha : AnsiString; IndiceBMP: Integer = 0 ) ; override ;
    Procedure AbreCupomVinculado(COO, CodFormaPagto, CodComprovanteNaoFiscal :
       String; Valor : Double) ; override ;
    Procedure LinhaCupomVinculado( Linha : AnsiString ) ; override ;
    Procedure FechaRelatorio ; override ;
    Procedure CortaPapel( const CorteParcial : Boolean = false) ; override ;

    { Procedimentos de Cupom Não Fiscal }
    Procedure AbreNaoFiscal( CPF_CNPJ: String = ''; Nome: String = '';
       Endereco: String = '' ) ; override ;
    Procedure RegistraItemNaoFiscal( CodCNF : String; Valor : Double;
       Obs : AnsiString = '') ; override ;
    Procedure CancelaItemNaoFiscal(const AItem: Integer); override;
    Procedure FechaNaoFiscal( Observacao : AnsiString = ''; IndiceBMP : Integer = 0); override;

    procedure Sangria( const Valor: Double;  Obs : AnsiString;
       DescricaoCNF: String; DescricaoFPG: String; IndiceBMP: Integer ) ; override ;

    Procedure MudaHorarioVerao  ; overload ; override ;
    Procedure MudaHorarioVerao( EHorarioVerao : Boolean ) ; overload ; override ;

    procedure CarregaAliquotas ; override ;
    procedure LerTotaisAliquota ; override ;
    Procedure ProgramaAliquota( Aliquota : Double; Tipo : Char = 'T';
       Posicao : String = '') ; override ;
    function AchaICMSAliquota( var AliquotaICMS : String ) :
       TACBrECFAliquota ;  overload ; override ;

    procedure CarregaTotalizadoresNaoTributados ; override;

    procedure CarregaFormasPagamento ; override ;
    procedure LerTotaisFormaPagamento ; override ;
    Procedure ProgramaFormaPagamento( var Descricao: String;
       PermiteVinculado : Boolean = true; Posicao : String = '' ) ; override ;

    procedure CarregaRelatoriosGerenciais ; override ;
    procedure LerTotaisRelatoriosGerenciais ; override ;
    Procedure ProgramaRelatorioGerencial( var Descricao: String;
       Posicao : String = '') ; override ;

    procedure CarregaComprovantesNaoFiscais ; override ;
    procedure LerTotaisComprovanteNaoFiscal ; override ;
    Procedure ProgramaComprovanteNaoFiscal( var Descricao: String;
       Tipo : String = ''; Posicao : String = '') ; override ;

    Procedure IdentificaOperador(Nome : String); override;
    Procedure IdentificaPAF( NomeVersao, MD5 : String) ; override;

    function TraduzirTag(const ATag: AnsiString): AnsiString; override;
    function TraduzirTagBloco(const ATag, Conteudo: AnsiString): AnsiString; override;

    function RoundECF(AValue: Double): Double;
 end ;

implementation

Uses
  typinfo,
  ACBrECF,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrUtil.Math,
  ACBrUtil.FilesIO,
  ACBrUtil.Compatibilidade,
  ACBrConsts;

{ TACBrECFVirtualClassItemCupom }

function TACBrECFVirtualClassItemCupom.GetAsString: String;
begin
  Result := IntToStr( Sequencia )       + '|' +
            Codigo                      + '|' +
            Descricao                   + '|' +
            FloatToStr( Qtd )           + '|' +
            FloatToStr( ValorUnit )     + '|' +
            FloatToStr( DescAcres )     + '|' +
            Unidade                     + '|' +
            IntToStr( CodDepartamento ) + '|' +
            IntToStr( AliqPos )         + '|'
end;

procedure TACBrECFVirtualClassItemCupom.SetAsString(const AValue: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := StringReplace(AValue,'|',sLineBreak,[rfReplaceAll]);

    if SL.Count < 9 then exit ;

    Sequencia       := StrToInt( SL[0] );
    Codigo          := SL[1];
    Descricao       := SL[2];
    Qtd             := StrToFloat( SL[3] );
    ValorUnit       := StrToFloat( SL[4] );
    DescAcres       := StrToFloat( SL[5] );
    Unidade         := SL[6];
    CodDepartamento := StrToInt( SL[7] );
    AliqPos         := StrToInt( SL[8] );
  finally
    SL.Free;
  end;
end;

constructor TACBrECFVirtualClassItemCupom.Create(
  AECFVirtualClassCupom: TACBrECFVirtualClassCupom);
begin
  inherited Create;

  fsECFVirtualClassCupom := AECFVirtualClassCupom;
end;

function TACBrECFVirtualClassItemCupom.TotalLiquido: Double;
begin
  Result := TotalBruto;

  if DescAcres <> 0 then
    Result := fsECFVirtualClassCupom.ECFVirtualClasse.RoundECF( Result + DescAcres);
end;

function TACBrECFVirtualClassItemCupom.TotalBruto: Double;
begin
  Result := fsECFVirtualClassCupom.ECFVirtualClasse.RoundECF( Qtd * ValorUnit);
end;

{ TACBrECFVirtualClassItensCupom }

procedure TACBrECFVirtualClassItensCupom.SetObject(Index: Integer;
  Item: TACBrECFVirtualClassItemCupom);
begin
  inherited Items[Index] := Item;
end;

function TACBrECFVirtualClassItensCupom.GetObject(Index: Integer
  ): TACBrECFVirtualClassItemCupom;
begin
  Result := TACBrECFVirtualClassItemCupom(inherited Items[Index]);
end;

function TACBrECFVirtualClassItensCupom.New(
  AECFVirtualClassCupom: TACBrECFVirtualClassCupom
  ): TACBrECFVirtualClassItemCupom;
begin
  Result := TACBrECFVirtualClassItemCupom.Create(AECFVirtualClassCupom);
  Result.Sequencia := Count+1;
  Add(Result);
end;

function TACBrECFVirtualClassItensCupom.Add(Obj: TACBrECFVirtualClassItemCupom
  ): Integer;
begin
  Result := inherited Add(Obj) ;
end;

procedure TACBrECFVirtualClassItensCupom.Insert(Index: Integer;
  Obj: TACBrECFVirtualClassItemCupom);
begin
  inherited Insert(Index, Obj);
end;

{ TACBrECFVirtualClassPagamentoCupom }

function TACBrECFVirtualClassPagamentoCupom.GetAsString: String;
begin
   Result := IntToStr( PosFPG )      + '|' +
             FloatToStr( ValorPago ) + '|' +
             Observacao              + '|' ;
end;

procedure TACBrECFVirtualClassPagamentoCupom.SetAsString(const AValue: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := StringReplace(AValue,'|',sLineBreak,[rfReplaceAll]);

    if SL.Count < 3 then exit ;

    PosFPG     := StrToInt( SL[0] );
    ValorPago  := StrToFloat( SL[1] );
    Observacao := SL[2];
  finally
    SL.Free;
  end;
end;

{ TACBrECFVirtualClassPagamentosCupom }

procedure TACBrECFVirtualClassPagamentosCupom.SetObject(Index: Integer;
  Item: TACBrECFVirtualClassPagamentoCupom);
begin
  inherited Items[Index] := Item;
end;

function TACBrECFVirtualClassPagamentosCupom.GetObject(Index: Integer
  ): TACBrECFVirtualClassPagamentoCupom;
begin
  Result := TACBrECFVirtualClassPagamentoCupom(inherited Items[Index]);
end;

function TACBrECFVirtualClassPagamentosCupom.New: TACBrECFVirtualClassPagamentoCupom;
begin
  Result := TACBrECFVirtualClassPagamentoCupom.Create;
  Add(Result);
end;

function TACBrECFVirtualClassPagamentosCupom.Add(
  Obj: TACBrECFVirtualClassPagamentoCupom): Integer;
begin
  Result := inherited Add(Obj) ;
end;

procedure TACBrECFVirtualClassPagamentosCupom.Insert(Index: Integer;
  Obj: TACBrECFVirtualClassPagamentoCupom);
begin
  inherited Insert(Index, Obj);
end;

{ TACBrECFVirtualClassCNFCupom }

function TACBrECFVirtualClassCNFCupom.GetAsString: String;
begin
  Result := IntToStr( Sequencia ) + '|' +
            IntToStr( PosCNF )    + '|' +
            FloatToStr( Valor )   + '|' +
            Observacao            + '|';
end;

procedure TACBrECFVirtualClassCNFCupom.SetAsString(const AValue: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := StringReplace(AValue,'|',sLineBreak,[rfReplaceAll]);

    if SL.Count < 4 then exit ;

    Sequencia  := StrToInt( SL[0] );
    PosCNF     := StrToInt( SL[1] );
    Valor      := StrToFloat( SL[2] );
    Observacao := SL[3];
  finally
    SL.Free;
  end;
end;

{ TACBrECFVirtualClassCNFsCupom }

procedure TACBrECFVirtualClassCNFsCupom.SetObject(Index: Integer;
  Item: TACBrECFVirtualClassCNFCupom);
begin
  inherited Items[Index] := Item;
end;

function TACBrECFVirtualClassCNFsCupom.GetObject(Index: Integer
  ): TACBrECFVirtualClassCNFCupom;
begin
  Result := TACBrECFVirtualClassCNFCupom(inherited Items[Index]);
end;

function TACBrECFVirtualClassCNFsCupom.New: TACBrECFVirtualClassCNFCupom;
begin
  Result := TACBrECFVirtualClassCNFCupom.Create;
  Result.Sequencia := Count+1;
  Add(Result);
end;

function TACBrECFVirtualClassCNFsCupom.Add(Obj: TACBrECFVirtualClassCNFCupom
  ): Integer;
begin
  Result := inherited Add(Obj) ;
end;

procedure TACBrECFVirtualClassCNFsCupom.Insert(Index: Integer;
  Obj: TACBrECFVirtualClassCNFCupom);
begin
  inherited Insert(Index, Obj);
end;

{ TACBrECFVirtualClassAliquotaCupom }

function TACBrECFVirtualClassAliquotaCupom.GetAsString: String;
begin
  Result := IntToStr( AliqPos )     + '|' +
            FloatToStr( AliqValor ) + '|' +
            FloatToStr( Total )     + '|' +
            FloatToStr( Rateio )    + '|' + Tipo + '|';
end;

procedure TACBrECFVirtualClassAliquotaCupom.SetAsString(const AValue: String);
var
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Text := StringReplace(AValue,'|',sLineBreak,[rfReplaceAll]);

    if SL.Count < 3 then exit ;

    AliqPos   := StrToInt( SL[0] );
    AliqValor := StrToFloat( SL[1] );
    Total     := StrToFloat( SL[2] );
    Rateio    := StrToFloat( SL[3] );

    if (SL.Count > 4) and CharInSet(PadLeft(SL[4], 1)[1], ['T', 'S']) then
      Tipo := PadLeft(SL[4], 1)[1]
    else
      Tipo := 'T';
  finally
    SL.Free;
  end;
end;

function TACBrECFVirtualClassAliquotaCupom.TotalLiquido: Double;
begin
  Result := Total + Rateio;
end;


{ TACBrECFVirtualClassAliquotasCupom }

procedure TACBrECFVirtualClassAliquotasCupom.SetObject(Index: Integer;
  Item: TACBrECFVirtualClassAliquotaCupom);
begin
  inherited Items[Index] := Item;
end;

function TACBrECFVirtualClassAliquotasCupom.GetObject(Index: Integer
  ): TACBrECFVirtualClassAliquotaCupom;
begin
  Result := TACBrECFVirtualClassAliquotaCupom(inherited Items[Index]);
end;

function TACBrECFVirtualClassAliquotasCupom.New: TACBrECFVirtualClassAliquotaCupom;
begin
  Result := TACBrECFVirtualClassAliquotaCupom.Create;
  Add(Result);
end;

function TACBrECFVirtualClassAliquotasCupom.Add(
  Obj: TACBrECFVirtualClassAliquotaCupom): Integer;
begin
  Result := inherited Add(Obj) ;
end;

procedure TACBrECFVirtualClassAliquotasCupom.Insert(Index: Integer;
  Obj: TACBrECFVirtualClassAliquotaCupom);
begin
  inherited Insert(Index, Obj);
end;

function TACBrECFVirtualClassAliquotasCupom.Find(APos: Integer
  ): TACBrECFVirtualClassAliquotaCupom;
var
  I: Integer;
begin
  Result := Nil;

  for I := 0 to Count-1 do
  begin
    if (Objects[I].AliqPos = APos) then
    begin
      Result := Objects[I];
      Break;
    end;
  end;
end;


{ TACBrECFVirtualClassCupom }

constructor TACBrECFVirtualClassCupom.Create(
  AECFVirtualClasse: TACBrECFVirtualClass);
begin
  inherited Create;

  fpECFVirtualClasse := AECFVirtualClasse;

  fpItensCupom      := TACBrECFVirtualClassItensCupom.Create( true );
  fpPagamentosCupom := TACBrECFVirtualClassPagamentosCupom.Create( true );
  fpCNFsCupom       := TACBrECFVirtualClassCNFsCupom.Create( true );
  fpAliquotasCupom  := TACBrECFVirtualClassAliquotasCupom.create( True ) ;

  Clear;
end;

destructor TACBrECFVirtualClassCupom.Destroy;
begin
  fpItensCupom.Free ;
  fpPagamentosCupom.Free ;
  fpCNFsCupom.Free ;
  fpAliquotasCupom.Free;

  inherited Destroy;
end;

procedure TACBrECFVirtualClassCupom.Clear;
begin
  fpItensCupom.Clear;
  fpPagamentosCupom.Clear;
  fpCNFsCupom.Clear;
  fpAliquotasCupom.Clear;

  fpTotalPago              := 0;
  fpSubtotalICMS           := 0;
  fpSubtotalISSQN          := 0;
  fpDescAcresSubtotalICMS  := 0;
  fpDescAcresSubtotalISSQN := 0;
  fpChaveDFe               := '';
end;

function TACBrECFVirtualClassCupom.VendeItem(const ACodigo, ADescricao: String; AQtd,
  AValorUnitario: Double; ADescAcres: Double; AAliq: TACBrECFAliquota;
  const AUnidade: String; ACodDepartamento: Integer): TACBrECFVirtualClassItemCupom;
var
  TotalItem: Double;
begin
  if fpCNFsCupom.Count > 0 then
    raise EACBrECFERRO.Create(ACBrStr('Cupom Não Fiscal já iniciado')) ;

  if fpItensCupom.Count >= 999 then
    raise EACBrECFERRO.Create(ACBrStr('Máximo de Itens já atingido (999)')) ;

  Result := fpItensCupom.New(Self);

  with Result do
  begin
    Codigo          := ACodigo ;
    Descricao       := ADescricao ;
    Qtd             := AQtd ;
    ValorUnit       := AValorUnitario ;
    DescAcres       := ADescAcres;
    Unidade         := AUnidade ;
    CodDepartamento := ACodDepartamento;
    AliqPos         := AAliq.Sequencia-1;

    TotalItem := TotalLiquido;
  end;

  if (AAliq.Tipo = 'S') then
    fpSubtotalISSQN := fpSubtotalISSQN + TotalItem
  else
    fpSubtotalICMS  := fpSubtotalICMS + TotalItem;

  SomaAliquota(AAliq, TotalItem);
end;

procedure TACBrECFVirtualClassCupom.SetDescAcresSubtotal(AValue: Currency);
var
  I, P: Integer;
  NovoCampeao: Boolean;
  PercentualEfetivo, TotalDescAcresRateio, ValorResiduo, MaiorValorTotal,
    AliqMaiorValorTotal, wDescAcresSubtotal: Double;
begin
  wDescAcresSubtotal := DescAcresSubtotal;

  if (wDescAcresSubtotal = AValue) then
    Exit;

  if (wDescAcresSubtotal <> 0) then
    raise EACBrECFERRO.Create(ACBrStr('Desconto/Acrescimo de SubTotal já foi informado'));

  wDescAcresSubtotal := AValue;

  { Se não há Desconto ou Acrescimo no Subtotal, então tudo já foi feito... }
  if (wDescAcresSubtotal = 0) or (fpAliquotasCupom.Count = 0) then
    Exit;

  { Calculando o Rateio do Desconto ou Acrescimo, na base de calculo das aliquotas
    Exemplo: http://partners.bematech.com.br/bemacast/Paginas/post.aspx?idPost=5790 }
  PercentualEfetivo := TruncTo(wDescAcresSubtotal / SubTotal * 100, 2);
  TotalDescAcresRateio := 0;

  for I := 0 to fpAliquotasCupom.Count-1 do
  begin
    with fpAliquotasCupom[I] do
    begin
      Rateio := TruncTo(Total * (PercentualEfetivo/100), 2);
      TotalDescAcresRateio := TotalDescAcresRateio + Rateio;

      if (Tipo = 'S') then
      begin
        fpDescAcresSubtotalISSQN := fpDescAcresSubtotalISSQN + Rateio;
        fpSubtotalISSQN := fpSubtotalISSQN + Rateio;
      end
      else
      begin
        fpDescAcresSubtotalICMS  := fpDescAcresSubtotalICMS + Rateio;
        fpSubtotalICMS := fpSubtotalICMS + Rateio;
      end;
    end;
  end;

  { Se houver resíduo, deve achar a aliquota com maior Valor Total da Venda
    e aplica o resíduo nela...  Se houve empate, usa a Aliquota com o maior
    valor em Porcentagem }
  ValorResiduo := wDescAcresSubtotal - TotalDescAcresRateio;
  if (ValorResiduo <> 0) then
  begin
     // Achando grupo de maior valor ou maior aliquota //
     MaiorValorTotal := 0;
     AliqMaiorValorTotal := 0;
     P := -1;
     for I := 0 to fpAliquotasCupom.Count-1 do
     begin
       with fpAliquotasCupom[I] do
       begin
         NovoCampeao := (Total > MaiorValorTotal) or
                        ( (Total = MaiorValorTotal) and (AliqValor > AliqMaiorValorTotal) );

         if NovoCampeao then
         begin
           P := I;
           AliqMaiorValorTotal := AliqValor;
           MaiorValorTotal := Total;
         end
       end;
     end;

     if P < 0 then  // Commo assim ? Não achou um campeão ? Então use o primeiro Totalizador
       P := 0;

     with fpAliquotasCupom[P] do
     begin
       Rateio := Rateio + ValorResiduo;

       if (Tipo = 'S') then
       begin
         fpDescAcresSubtotalISSQN := fpDescAcresSubtotalISSQN + ValorResiduo;
         fpSubtotalISSQN := fpSubtotalISSQN + ValorResiduo;
       end
       else
       begin
         fpDescAcresSubtotalICMS  := fpDescAcresSubtotalICMS + ValorResiduo;
         fpSubtotalICMS := fpSubtotalICMS + ValorResiduo;
       end;
     end;
  end;
end;

function TACBrECFVirtualClassCupom.GetSubtotal: Currency;
begin
  // Soma dos totalizadores ICMS e Serviço
  Result := fpSubtotalICMS + fpSubtotalISSQN;
end;

function TACBrECFVirtualClassCupom.GetDescAcresSubtotal: Currency;
begin
  Result := fpDescAcresSubtotalICMS + fpDescAcresSubtotalISSQN;
end;

procedure TACBrECFVirtualClassCupom.VerificaFaixaItem(NumItem: Integer);
begin
  if (NumItem < 1) or (NumItem > fpItensCupom.Count) then
    raise EACBrECFERRO.create(ACBrStr('Item ('+IntToStrZero(NumItem,3)+') fora da Faixa.')) ;
end;

function TACBrECFVirtualClassCupom.SomaAliquota(aAliq: TACBrECFAliquota;
  AValor: Currency): TACBrECFVirtualClassAliquotaCupom;
var
  wAliqPos: Integer;
begin
  wAliqPos := (aAliq.Sequencia - 1);
  Result   := fpAliquotasCupom.Find(wAliqPos);

  if not Assigned(Result) then
  begin
    Result := fpAliquotasCupom.New;
    Result.AliqPos   := wAliqPos;
    Result.AliqValor := aAliq.Aliquota;
    Result.Tipo      := aAliq.Tipo;
  end;

  Result.Total := Result.Total + AValor;
end;

procedure TACBrECFVirtualClassCupom.SubtraiAliquota(AAliqPos: Integer;
  AValor: Currency);
var
  ALiq: TACBrECFVirtualClassAliquotaCupom;
  I: Integer;
begin
  ALiq := fpAliquotasCupom.Find(AAliqPos);

  if Assigned(ALiq) then
  begin
    ALiq.Total := ALiq.Total - AValor;

    if ALiq.Total <= 0 then
    begin
      I := fpAliquotasCupom.IndexOf(ALiq);
      fpAliquotasCupom.Delete(I);
    end;
  end;
end;

procedure TACBrECFVirtualClassCupom.DescAcresItem(NumItem: Integer;
  ADescAcres: Double);
var
  ItemCupom: TACBrECFVirtualClassItemCupom;
  ALiq: TACBrECFVirtualClassAliquotaCupom;
begin
  VerificaFaixaItem(NumItem);

  ItemCupom := fpItensCupom[NumItem-1];

  if (ItemCupom.DescAcres <> 0) then
    raise EACBrECFERRO.create(ACBrStr('Item ('+IntToStrZero(NumItem,3)+') já recebeu Desconto ou Acrescimo.')) ;

  ItemCupom.DescAcres := fpECFVirtualClasse.RoundECF( ADescAcres );

  ALiq := fpAliquotasCupom.Find(ItemCupom.AliqPos);
  if Assigned(ALiq) then
  begin
    if (ALiq.Tipo = 'S') then
      fpSubtotalISSQN := fpSubtotalISSQN + ItemCupom.DescAcres
    else
      fpSubtotalICMS := fpSubtotalICMS + ItemCupom.DescAcres;

    // Atualiza totais das Aliquotas
    ALiq.Total := ALiq.Total + ItemCupom.DescAcres;
  end;
end;

procedure TACBrECFVirtualClassCupom.CancelaDescontoAcrescimoItem(
  NumItem: Integer; const TipoAcrescimoDesconto: String);
var
  ItemCupom: TACBrECFVirtualClassItemCupom;
  ALiq: TACBrECFVirtualClassAliquotaCupom;
begin
  VerificaFaixaItem(NumItem);

  ItemCupom := fpItensCupom[NumItem-1];

  if (ItemCupom.DescAcres = 0) then
    raise EACBrECFERRO.create(ACBrStr('Item ('+IntToStrZero(NumItem,3)+') não possui Desconto ou Acrescimo.'));

  ALiq := fpAliquotasCupom.Find(ItemCupom.AliqPos);
  if Assigned(ALiq) then
  begin
    if (ALiq.Tipo = 'S') then
      fpSubtotalISSQN := fpSubtotalISSQN - ItemCupom.DescAcres
    else
      fpSubtotalICMS := fpSubtotalICMS - ItemCupom.DescAcres;

    // Atualiza totais das Aliquotas
    ALiq.Total := ALiq.Total - ItemCupom.DescAcres;
  end;
end;

procedure TACBrECFVirtualClassCupom.CancelaItem(NumItem: Integer);
var
  ItemCupom: TACBrECFVirtualClassItemCupom;
  TotalItem: Double;
begin
  VerificaFaixaItem(NumItem);

  ItemCupom := fpItensCupom[NumItem-1];
  with ItemCupom do
  begin
    TotalItem := TotalLiquido;
    Qtd := 0;
    DescAcres := 0;

    if (fpAliquotasCupom.Find(AliqPos).Tipo = 'S') then
      fpSubtotalISSQN := fpSubtotalISSQN - TotalItem
    else
      fpSubtotalICMS := fpSubtotalICMS - TotalItem;

    SubtraiAliquota(AliqPos, TotalItem);
  end;
end;

function TACBrECFVirtualClassCupom.EfetuaPagamento(AValor: Double;
  const AObservacao: String; APosFPG: Integer): TACBrECFVirtualClassPagamentoCupom;
begin
  if fpPagamentosCupom.Count >= 999 then
    raise EACBrECFERRO.Create(ACBrStr('Máximo de Pagamentos já atingido (999)')) ;

  AValor := fpECFVirtualClasse.RoundECF( AValor );

  Result := fpPagamentosCupom.New;
  with Result do
  begin
    PosFPG     := APosFPG ;
    ValorPago  := abs(AValor) ;
    Observacao := AObservacao ;

    fpTotalPago := fpTotalPago + max(AValor, 0);
  end;
end;

function TACBrECFVirtualClassCupom.RegistraCNF(AValor: Currency;
  const AObservacao: String; APosCNF: Integer): TACBrECFVirtualClassCNFCupom;
begin
  if fpItensCupom.Count > 0 then
    raise EACBrECFERRO.Create(ACBrStr('Cupom de venda já iniciado')) ;

  Result := fpCNFsCupom.New;

  with Result do
  begin
    Valor          := fpECFVirtualClasse.RoundECF( AValor );
    PosCNF         := APosCNF;
    Observacao     := AObservacao;
    fpSubtotalICMS := fpSubtotalICMS + Valor;      { Soma no Subtotal }
  end;
end;

procedure TACBrECFVirtualClassCupom.CancelaCNF(NumItem: Integer);
var
  ACNF: TACBrECFVirtualClassCNFCupom;
begin
  if (NumItem < 1) or (NumItem > fpCNFsCupom.Count) then
    raise EACBrECFERRO.create(ACBrStr('Item ('+IntToStrZero(NumItem,3)+') fora da Faixa.')) ;

  ACNF := fpCNFsCupom[NumItem-1];

  fpSubtotalICMS := fpSubtotalICMS - ACNF.fsValor;
  ACNF.Valor := 0;
end;

procedure TACBrECFVirtualClassCupom.LoadFromINI(AIni: TCustomIniFile);
var
  I: Integer;
  S, T: String;
  ItemCupom: TACBrECFVirtualClassItemCupom;
  AliqCupom: TACBrECFVirtualClassAliquotaCupom;
  PagtoCupom: TACBrECFVirtualClassPagamentoCupom;
  CNFCupom: TACBrECFVirtualClassCNFCupom;
begin
  Clear;

  fpChaveDFe := AIni.ReadString('Variaveis','ChaveCupom',fpChaveDFe);

  fpDescAcresSubtotalICMS  := AIni.ReadFloat('Cupom', 'DescontoAcrescimo', 0);
  fpDescAcresSubtotalISSQN := AIni.ReadFloat('Cupom', 'DescontoAcrescimoISSQN', 0);

  S := 'Cupom_Aliquotas';
  I := 0 ;
  while (I < 100) do
  begin
    T := AIni.ReadString(S, IntToStrZero(I, 2), '*FIM*');
    if T = '*FIM*' then break;

    AliqCupom := fpAliquotasCupom.New;
    AliqCupom.AsString := T;
    Inc( I );
  end;

  S := 'Cupom_Items';
  I := 0 ;
  while (I < 1000) do
  begin
    T := AIni.ReadString(S, IntToStrZero(I, 3), '*FIM*');
    if T = '*FIM*' then Break;

    ItemCupom := fpItensCupom.New(Self);
    ItemCupom.AsString := T;

    if (ItemCupom.Qtd > 0) then // Não foi cancelado ?
    begin
      AliqCupom := fpAliquotasCupom.Find(ItemCupom.AliqPos);
      if Assigned(AliqCupom) then
      begin
        if (AliqCupom.Tipo = 'S') then
          fpSubtotalISSQN := fpSubtotalISSQN + ItemCupom.TotalLiquido
        else
          fpSubtotalICMS  := fpSubtotalICMS + ItemCupom.TotalLiquido;
      end;
    end;

    Inc(I);
  end;

  S := 'Cupom_Pagamentos';
  I := 0 ;
  while (I < 1000) do
  begin
    T := AIni.ReadString( S, IntToStrZero(I, 3), '*FIM*') ;
    if T = '*FIM*' then break ;

    PagtoCupom := fpPagamentosCupom.New;
    PagtoCupom.AsString := T;
    if UpperCase(PagtoCupom.fsObservacao) <> 'TROCO' then
      fpTotalPago := fpTotalPago + max(PagtoCupom.ValorPago,0);
    Inc( I );
  end ;

  S := 'Cupom_Comprovantes_Nao_Fiscais';
  I := 0 ;
  while (I < 100) do
  begin
    T := AIni.ReadString( S, IntToStrZero(I, 2), '*FIM*') ;
    if T = '*FIM*' then break ;

    CNFCupom := fpCNFsCupom.New;
    CNFCupom.AsString := T;
    fpSubtotalICMS := fpSubtotalICMS + CNFCupom.Valor;      { Soma no Subtotal }
    Inc( I );
  end ;

  fpSubtotalICMS  := fpSubtotalICMS  + fpDescAcresSubtotalICMS;
  fpSubtotalISSQN := fpSubtotalISSQN + fpDescAcresSubtotalISSQN;
end;

procedure TACBrECFVirtualClassCupom.SaveToINI(AIni: TCustomIniFile);
var
  S: String;
  I: Integer;
begin
  AIni.WriteString('Variaveis', 'ChaveCupom', fpChaveDFe);

  AIni.WriteFloat('Cupom', 'DescontoAcrescimo', fpDescAcresSubtotalICMS);
  AIni.WriteFloat('Cupom', 'DescontoAcrescimoISSQN', fpDescAcresSubtotalISSQN);
  AIni.WriteFloat('Cupom', 'Subtotal', fpSubtotalICMS);
  AIni.WriteFloat('Cupom', 'SubTotalISSQN', fpSubtotalISSQN);
  AIni.WriteFloat('Cupom', 'TotalPago', fpTotalPago);

  S := 'Cupom_Items';
  for I := 0 to Itens.Count - 1 do
  begin
    with Itens[I] do
      AIni.WriteString(S, IntToStrZero(I, 3), AsString);
  end;

  S := 'Cupom_Pagamentos';
  for I := 0 to Pagamentos.Count - 1 do
  begin
    with Pagamentos[I] do
      AIni.WriteString(S ,IntToStrZero(I, 3), AsString);
  end;

  S := 'Cupom_Comprovantes_Nao_Fiscais';
  for I := 0 to CNF.Count - 1 do
  begin
    with CNF[I] do
      AIni.WriteString(S ,IntToStrZero(I, 2), AsString);
  end;

  S := 'Cupom_Aliquotas';
  for I := 0 to Aliquotas.Count - 1 do
  begin
    with Aliquotas[I] do
      AIni.WriteString(S ,IntToStrZero(I, 2), AsString);
  end;
end;

{ ---------------------------- TACBrECFVirtual ------------------------------- }

constructor TACBrECFVirtual.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fsECF := nil ;
  CreateVirtualClass;
end;

procedure TACBrECFVirtual.CreateVirtualClass;
begin
  fpECFVirtualClass := TACBrECFVirtualClass.create( self );
end;

destructor TACBrECFVirtual.Destroy;
begin
  if Assigned( fsECF ) then
  begin
    if TACBrECF(fsECF).ECF = fpECFVirtualClass then
    begin
      TACBrECF(fsECF).Desativar;
      TACBrECF(fsECF).Modelo := ecfNenhum;
    end;
  end;

  fpECFVirtualClass.Free;

  inherited Destroy;
end;

procedure TACBrECFVirtual.SetECF(AValue: TACBrComponent);
Var
  OldValue : TACBrECF ;
begin
  if AValue <> fsECF then
    if AValue <> nil then
      if not (AValue is TACBrECF) then
        raise Exception.Create(ACBrStr('ACBrVirtual.ECF deve ser do tipo TACBrECF')) ;

   if Assigned(fsECF) then
     fsECF.RemoveFreeNotification(Self);

   OldValue := TACBrECF(fsECF) ;   // Usa outra variavel para evitar Loop Infinito
   fsECF := AValue;                // na remoção da associação dos componentes

   if Assigned(OldValue) then
     if Assigned(OldValue.ECFVirtual) then
       OldValue.ECFVirtual := nil ;

   if AValue <> nil then
   begin
     AValue.FreeNotification(self);
     TACBrECF(AValue).ECFVirtual := Self ;
     // Passa referencia de ACBrECF.Device para self.ECFVirtualClass.Device
     ECFVirtualClass.Device := TACBrECF(AValue).Device;
   end ;
end ;

procedure TACBrECFVirtual.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if (Operation = opRemove) and (fsECF <> nil) and (AComponent is TACBrECF) then
    fsECF := nil ;
end;

function TACBrECFVirtual.GetCNPJ: String;
begin
  Result := fpECFVirtualClass.CNPJ;
end;

function TACBrECFVirtual.GetColunas: Integer;
begin
  Result := fpECFVirtualClass.Colunas;
end;

function TACBrECFVirtual.GetIE: String;
begin
  Result := fpECFVirtualClass.IE;
end;

function TACBrECFVirtual.GetIM: String;
begin
  Result := fpECFVirtualClass.IM;
end;

function TACBrECFVirtual.GetNomeArqINI: String;
begin
  Result := fpECFVirtualClass.NomeArqINI;

  if Result = '' then
     if not (csDesigning in Self.ComponentState) then
        Result := fpECFVirtualClass.CalculaNomeArqINI ;
end;

function TACBrECFVirtual.GetNumCRO: Integer;
begin
  Result := fpECFVirtualClass.NumCRO;
end;

function TACBrECFVirtual.GetNumECF: Integer;
begin
  Result := fpECFVirtualClass.NumECF;
end;

function TACBrECFVirtual.GetNumSerie: String;
begin
  Result := fpECFVirtualClass.NumSerie;
end;

function TACBrECFVirtual.GetQuandoCancelarCupom: TACBrECFVirtualQuandoCancelarCupom;
begin
  Result := fpECFVirtualClass.QuandoCancelarCupom;
end;

function TACBrECFVirtual.GetQuandoGravarArqINI: TACBrECFVirtualLerGravarINI;
begin
  Result := fpECFVirtualClass.QuandoGravarArqINI;
end;

function TACBrECFVirtual.GetQuandoImprimirLinha: TACBrECFVirtualQuandoImprimirLinha;
begin
  Result := fpECFVirtualClass.QuantoImprimirLinha;
end;

function TACBrECFVirtual.GetQuandoLerArqINI: TACBrECFVirtualLerGravarINI;
begin
  Result := fpECFVirtualClass.QuandoLerArqINI;
end;

function TACBrECFVirtual.GetQuandoMudarEstado: TACBrECFVirtualQuandoMudarEstado;
begin
  Result := fpECFVirtualClass.QuandoMudarEstado;
end;

procedure TACBrECFVirtual.SetCNPJ(const AValue: String);
begin
  fpECFVirtualClass.CNPJ := AValue;
end;

procedure TACBrECFVirtual.SetColunas(AValue: Integer);
begin
  fpECFVirtualClass.Colunas := AValue;
end;

procedure TACBrECFVirtual.SetIE(const AValue: String);
begin
  fpECFVirtualClass.IE := AValue;
end;

procedure TACBrECFVirtual.SetIM(const AValue: String);
begin
  fpECFVirtualClass.IM := AValue;
end;

procedure TACBrECFVirtual.SetNomeArqINI(const AValue: String);
begin
  fpECFVirtualClass.NomeArqINI := AValue;
end;

procedure TACBrECFVirtual.SetNumCRO(AValue: Integer);
begin
  fpECFVirtualClass.NumCRO := AValue;
end;

procedure TACBrECFVirtual.SetNumECF(AValue: Integer);
begin
  fpECFVirtualClass.NumECF := AValue;
end;

procedure TACBrECFVirtual.SetNumSerie(const AValue: String);
begin
  fpECFVirtualClass.NumSerie := AValue;
end;

procedure TACBrECFVirtual.SetQuandoCancelarCupom(AValue: TACBrECFVirtualQuandoCancelarCupom);
begin
  fpECFVirtualClass.QuandoCancelarCupom := AValue;
end;

procedure TACBrECFVirtual.SetQuandoGravarArqINI(AValue: TACBrECFVirtualLerGravarINI);
begin
  fpECFVirtualClass.QuandoGravarArqINI := AValue;
end;

procedure TACBrECFVirtual.SetQuandoImprimirLinha(const AValue: TACBrECFVirtualQuandoImprimirLinha);
begin
  fpECFVirtualClass.fsQuandoImprimirLinha := AValue;
end;

procedure TACBrECFVirtual.SetQuandoLerArqINI(AValue: TACBrECFVirtualLerGravarINI);
begin
  fpECFVirtualClass.QuandoLerArqINI := AValue;
end;

procedure TACBrECFVirtual.SetQuandoMudarEstado(AValue: TACBrECFVirtualQuandoMudarEstado);
begin
  fpECFVirtualClass.QuandoMudarEstado := AValue;
end;

procedure TACBrECFVirtual.LeArqINI;
begin
  fpECFVirtualClass.LeArqINI;
end;

procedure TACBrECFVirtual.GravaArqINI;
begin
  fpECFVirtualClass.GravaArqINI;
end;

{ --------------------------- TACBrECFVirtualClass --------------------------- }

constructor TACBrECFVirtualClass.Create(AECFVirtual: TACBrECFVirtual);
begin
  inherited create( AECFVirtual ) ;

  fpIdentificaConsumidorRodape := True ;
  fsECFVirtualOwner := AECFVirtual;
  fsQuandoLerArqINI := nil;
  fsQuandoGravarArqINI := nil;
  fsQuandoCancelarCupom := nil;
  fsQuandoMudarEstado := nil;
  fpCupom := TACBrECFVirtualClassCupom.Create(Self);
  fpNumMaxLinhasRodape := 0;
  fpArredondaItemMFD := True;

  Zera ;
end;

destructor TACBrECFVirtualClass.Destroy;
begin
  fpCupom.Free;

  inherited Destroy ;
end;

procedure TACBrECFVirtualClass.Zera ;
begin
  { Variaveis internas dessa classe }
  fpNomeArqINI := '' ;
  fpNumSerie   := '' ;
  fpNumECF     := 1 ;
  fpIE         := '012.345.678.90' ;
  fpCNPJ       := '01.234.567/0001-22' ;
  fpPAF        := '' ;
  Operador     := '' ;
  fpIM         := '1234-0' ;
  fpModeloStr  := 'ECFVirtual' ;
  fpVerao      := false ;
  fpDia        := now ;
  fpNumCRO     := 1 ;
  fpReducoesZ  := 0 ;
  fpLeiturasX  := 0 ;
  fpCOOInicial := 0 ;
  fpCOOFinal   := 0 ;
  fpNumCOO     := 0 ;
  fpNumGNF     := 0 ;
  fpNumGRG     := 0 ;
  fpNumCDC     := 0 ;
  fpNumCER     := 0 ;
  fpNumCCF     := 0 ;
  fpGrandeTotal:= 0 ;
  fpVendaBruta := 0 ;

  fpCuponsCancelados              := 0;
  fpCuponsCanceladosTotalICMS     := 0;
  fpCuponsCanceladosTotalISSQN    := 0;
  fpCNFCancelados                 := 0;
  fpCNFCanceladosTotal            := 0;
  fpCuponsCanceladosEmAberto      := 0;
  fpCuponsCanceladosEmAbertoTotalICMS  := 0;
  fpCuponsCanceladosEmAbertoTotalISSQN := 0;

  ZeraCupom;
end;

procedure TACBrECFVirtualClass.ZeraCupom;
begin
  fpCupom.Clear;
end;

procedure TACBrECFVirtualClass.Ativar;
begin
  if not Assigned(ECFVirtual) then
  begin
    inherited Ativar;
  end
  else
  begin
    GravaLog( sLineBreak +
              StringOfChar('-',80)+ sLineBreak +
              'ATIVAR - '+FormatDateTime('dd/mm/yy hh:nn:ss:zzz',now)+
              ' - Modelo: '+ModeloStr+
              //' - Porta: '+fpDevice.Porta+
              //' - TimeOut: '+IntToStr(TimeOut)+ sLineBreak +
              //'         Device: '+fpDevice.DeviceToString(False) +
              sLineBreak +
              StringOfChar('-',80) + sLineBreak );
    //SetEstadoECFVirtual(estDesconhecido);
    fpAtivo  := true ;
  end;

  try
    LeArqINI ;

    AtivarVirtual;
  except
    Desativar ;
    raise ;
  end ;
end;

procedure TACBrECFVirtualClass.AtivarVirtual;
var
  AtualizarAliq, AtualizarFPG: Boolean;
begin
  fpMFD := True;

  // Verificando se precisa inserir as novas aliquotas de Serviços //
  AtualizarAliq := (fpAliquotas.Count < 6);
  if not AtualizarAliq then
  begin
    AtualizarAliq := (fpAliquotas[3].Indice <> 'FS1') or
                     (fpAliquotas[4].Indice <> 'IS1') or
                     (fpAliquotas[5].Indice <> 'NS1');
  end;

  if AtualizarAliq then
    AtualizarAliquotasMemoria;

  // Verificando se precisa inserir 00-TROCO //
  AtualizarFPG := (fpFormasPagamentos.Count < 2);
  if not AtualizarFPG then
    AtualizarFPG := (fpFormasPagamentos[0].Indice <> '00');

  if AtualizarFPG then
    AtualizarFormasPagamentoMemoria;
end;

procedure TACBrECFVirtualClass.Desativar;
begin
  inherited Desativar ;
end;

procedure TACBrECFVirtualClass.AbreDia ;
begin
  GravaLog('AbreDia');
  fpDia        := now ;
  fpCOOInicial := fpNumCOO ;
end ;

procedure TACBrECFVirtualClass.AbreDocumento ;
begin
  GravaLog('AbreDocumento');

  if fpDia > now then
    raise EACBrECFERRO.create(ACBrStr('Erro ! A Data: '+DateToStr(fpDia)+
                                      '‚ maior do que a Data atual: '+DateToStr(now))) ;

  try
    fpNumCOO   := fpNumCOO + 1 ;
    fpCOOFinal := fpNumCOO ;

    if (CompareDate(fpDia, now) > 0) then
      AbreDia;

    AbreDocumentoVirtual;

    GravaArqINI;   // Grava estado do ECF
  except
    LeArqINI;
    raise;
  end;
end;

procedure TACBrECFVirtualClass.AbreDocumentoVirtual;
begin
  {}
end;

function TACBrECFVirtualClass.GetNumCupom: String;
begin
  Result := IntToStrZero( fpNumCOO, 6 ) ;
  GravaLog('GetNumCupom: '+Result);
end;

function TACBrECFVirtualClass.GetNumGNF: String;
begin
  Result := IntToStrZero( fpNumGNF, 6 ) ;
  GravaLog('GetNumGNF: '+Result);
end;

function TACBrECFVirtualClass.GetNumGRG: String;
begin
  Result := IntToStrZero( fpNumGRG, 6 ) ;
  GravaLog('GetNumGRG: '+Result);
end;

function TACBrECFVirtualClass.GetNumCDC: String;
begin
  Result := IntToStrZero( fpNumCDC, 6 ) ;
  GravaLog('GetNumCDC: '+Result);
end;

function TACBrECFVirtualClass.GetNumCCF: String;
begin
  Result := IntToStrZero( fpNumCCF, 6 ) ;
  GravaLog('GetNumCCF: '+Result);
end;

function TACBrECFVirtualClass.GetNumCFC: String;
begin
  Result := IntToStrZero( fpCuponsCancelados + fpCuponsCanceladosEmAberto, 6 ) ;
  GravaLog('GetNumCFC: '+Result);
end;

function TACBrECFVirtualClass.GetNumCOOInicial: String;
begin
  Result := IntToStrZero( fpCOOInicial, 6 ) ;
  GravaLog('GetNumCOOInicial: '+Result);
end;

function TACBrECFVirtualClass.GetNumUltimoItem: Integer;
begin
  Result := fpCupom.Itens.Count;
  GravaLog('GetNumUltimoItem: '+IntToStr(Result));
end;

function TACBrECFVirtualClass.GetNumGNFC: String;
begin
  Result := IntToStrZero( fpCNFCancelados, 6 ) ;
  GravaLog('GetNumGNFC: '+Result);
end;

function TACBrECFVirtualClass.GetGrandeTotal: Double;
begin
  Result := RoundTo( fpGrandeTotal, -2) ;
  GravaLog('GetGrandeTotal: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetVendaBruta: Double;
begin
  Result := RoundTo( fpVendaBruta, -2) ;
  GravaLog('GetVendaBruta: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetTotalSubstituicaoTributaria: Double;
begin
  Result := RoundTo( fpAliquotas[0].Total, -2 ) ;   // F1
  GravaLog('GetTotalSubstituicaoTributaria: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetTotalIsencao: Double;
begin
  Result := RoundTo( fpAliquotas[1].Total, -2 ) ;   // I1
  GravaLog('GetTotalIsencao: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetTotalNaoTributado: Double;
begin
  Result := RoundTo( fpAliquotas[2].Total,-2 ) ;    // N1
  GravaLog('GetTotalNaoTributado: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetTotalAcrescimos: Double;
begin
   Result := RoundTo(fpTotalAcrescimosICMS,-2);
   GravaLog('GetTotalAcrescimos: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetTotalCancelamentos: Double;
begin
   Result := RoundTo(fpCuponsCanceladosTotalICMS,-2);
   GravaLog('GetTotalCancelamentos: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetTotalCancelamentosOPNF: Double;
begin
  Result := RoundTo(fpCNFCanceladosTotal,-2);
  GravaLog('GetTotalCancelamentosOPNF: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetTotalCancelamentosEmAberto: Double;
begin
   Result := RoundTo(fpCuponsCanceladosEmAbertoTotalICMS,-2);
   GravaLog('GetTotalCancelamentosEmAberto: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetTotalDescontos: Double;
begin
   Result := RoundTo(fpTotalDescontosICMS, -2);
   GravaLog('GetTotalDescontos: ' + FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetTotalAcrescimosISSQN: Double;
begin
  Result := RoundTo(fpTotalAcrescimosISSQN, -2);
  GravaLog('GetTotalAcrescimosISSQN: ' + FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetTotalCancelamentosISSQN: Double;
begin
   Result := RoundTo(fpCuponsCanceladosTotalISSQN,-2);
   GravaLog('GetTotalCancelamentosISSQN: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetTotalCancelamentosEmAbertoISSQN: Double;
begin
   Result := RoundTo(fpCuponsCanceladosEmAbertoTotalISSQN, -2);
   GravaLog('GetTotalCancelamentosEmAbertoISSQN: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetTotalDescontosISSQN: Double;
begin
   Result := RoundTo(fpTotalDescontosISSQN, -2);
   GravaLog('GetTotalDescontosISSQN: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetTotalIsencaoISSQN: Double;
begin
  Result := RoundTo( fpAliquotas[4].Total, -2 ) ;   // IS1
  GravaLog('GetTotalIsencaoISSQN: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetTotalNaoTributadoISSQN: Double;
begin
  Result := RoundTo( fpAliquotas[5].Total, -2 ) ;   // NS1
  GravaLog('GetTotalNaoTributadoISSQN: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetTotalSubstituicaoTributariaISSQN: Double;
begin
  Result := RoundTo( fpAliquotas[3].Total, -2 ) ;   // FS1
  GravaLog('GetTotalSubstituicaoTributariaISSQN: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetNumReducoesZRestantes: String;
begin
  Result:= '9999';
  GravaLog('GetNumReducoesZRestantes: '+Result);
end;

function TACBrECFVirtualClass.GetNumECF: String;
begin
  Result := IntToStrZero(fpNumECF,3) ;
  GravaLog('GetNumECF: '+Result);
end;

function TACBrECFVirtualClass.GetCNPJ: String;
begin
  Result := fpCNPJ ;
  GravaLog('GetCNPJ: '+Result);
end;

function TACBrECFVirtualClass.GetIE: String;
begin
  Result := fpIE ;
  GravaLog('GetIE: '+Result);
end;

function TACBrECFVirtualClass.GetIM: String;
begin
  Result := fpIM ;
  GravaLog('GetIM: '+Result);
end;

function TACBrECFVirtualClass.GetPAF: String;
begin
  Result := fpPAF ;
  GravaLog('GetPAF: '+Result);
end;

function TACBrECFVirtualClass.GetUsuarioAtual: String;
begin
  Result := '0001' ;
  GravaLog('GetUsuarioAtual: '+Result);
end;

function TACBrECFVirtualClass.GetDataHora: TDateTime;
begin
  Result := now;
  GravaLog('GetDataHora: '+DateTimeToStr(Result));
end;

function TACBrECFVirtualClass.GetDataHoraSB: TDateTime;
begin
  Result := EncodeDateTime(2013,12,30,11,12,00,00);
  GravaLog('GetDataHoraSB: '+DateTimeToStr(Result));
end;

function TACBrECFVirtualClass.GetDataMovimento: TDateTime;
begin
  Result := fpDia ;
  GravaLog('GetDataMovimento: '+DateTimeToStr(Result));
end;

function TACBrECFVirtualClass.GetSubModeloECF: String;
begin
  Result := 'Virtual' ;
  GravaLog('GetSubModeloECF: '+Result);
end;

function TACBrECFVirtualClass.GetNumSerie: String;
begin
  Result := fpNumSerie ;
  GravaLog('GetNumSerie: '+Result);
end;

function TACBrECFVirtualClass.GetNumCRO: String;
begin
  Result := IntToStrZero(fpNumCRO, 3) ;
  GravaLog('GetNumCRO: '+Result);
end;

function TACBrECFVirtualClass.GetNumCRZ: String;
begin
  Result := IntToStrZero(fpReducoesZ, 6);
  GravaLog('GetNumCRZ: '+Result);
end;

function TACBrECFVirtualClass.GetNumVersao: String ;
begin
  Result := ACBR_VERSAO ;
  GravaLog('GetNumVersao: '+Result);
end;

function TACBrECFVirtualClass.GetTotalPago: Double;
begin
  Result := fpCupom.TotalPago;
  Result := RoundTo( Result, -2);
  GravaLog('GetTotalPago: '+FloatToStr(Result));
end;

function TACBrECFVirtualClass.GetSubTotal: Double;
begin
  Result := fpCupom.SubTotal;
  Result := RoundTo( Result, -2);
  GravaLog('GetSubTotal: '+FloatToStr(Result));
end;

procedure TACBrECFVirtualClass.MudaHorarioVerao ;
begin
  GravaLog('MudaHorarioVerao');
  fpVerao := not fpVerao ;

  try
    GravaArqINI ;
  except
    fpVerao := not fpVerao ;
    raise ;
  end ;
end;

procedure TACBrECFVirtualClass.MudaHorarioVerao(EHorarioVerao: Boolean);
begin
  GravaLog( ComandoLOG );
  if EHorarioVerao <> HorarioVerao then
    MudaHorarioVerao ;
end;

procedure TACBrECFVirtualClass.EnviaConsumidorVirtual;
begin
  Consumidor.Enviado := True ;
end;

procedure TACBrECFVirtualClass.VerificaPodeCancelarCupom(NumCOOCancelar: Integer
  );
begin
  GravaLog('VerificaPodeCancelarCupom');
  if ((fpCupom.Itens.Count = 0) and (Estado <> estVenda) ) and
     ((fpCupom.CNF.Count   = 0) and (Estado <> estNaoFiscal) ) then
    raise EACBrECFERRO.Create(ACBrStr('Último Documento não é Cupom')) ;
end;

procedure TACBrECFVirtualClass.AbreCupom ;
begin
  GravaLog('AbreCupom');
  TestaPodeAbrirCupom ;

  try
    ZeraCupom;

    SetEstadoECFVirtual(estVenda);
    fpNumCCF := fpNumCCF + 1 ;

    EnviaConsumidorVirtual;
    AbreDocumento ;
    //Sleep(7000) ;   // Simulando um atraso //
  except
    LeArqINI ;
    raise ;
  end ;
end;

procedure TACBrECFVirtualClass.VendeItem(Codigo, Descricao : String ;
  AliquotaECF : String ; Qtd : Double ; ValorUnitario : Double ;
  ValorDescontoAcrescimo : Double ; Unidade : String ;
  TipoDescontoAcrescimo : String ; DescontoAcrescimo : String ;
  CodDepartamento : Integer) ;
Var
  Aliq: TACBrECFAliquota;
  ValItemBruto: Double;
  ItemCupom: TACBrECFVirtualClassItemCupom;
begin
  GravaLog( ComandoLOG );

  if Estado <> estVenda then
    raise EACBrECFERRO.create(ACBrStr('O Estado nao é "VENDA" Cupom não Aberto')) ;

  if (Qtd <= 0) or (ValorUnitario <= 0) or (Descricao = '') or (Codigo = '') then
    raise EACBrECFERRO.create(ACBrStr('Erro. Parâmetros inválidos.')) ;

  Aliq := AchaICMSIndice( AliquotaECF );
  if Aliq = nil then
    raise EACBrECFERRO.create(ACBrStr('Aliquota '+AliquotaECF+' Inválida')) ;

  //DEBUG
  //Sleep(1000) ;   // Simulando um atraso

  try
    { Adicionando o Item Vendido no ObjectList.
      Ignora o ValorDescontoAcrescimo, pois o mesmo será processado abaixo,
      na chamada de "DescontoAcrescimoItemAnterior" }
    ItemCupom := fpCupom.VendeItem( Codigo, Descricao, Qtd, ValorUnitario, 0,
                                    Aliq, Unidade, CodDepartamento);

    { Somando o Total do Item, no GT e VendaBruta }
    ValItemBruto  := ItemCupom.TotalBruto;
    fpGrandeTotal := fpGrandeTotal + ValItemBruto ;
    fpVendaBruta  := fpVendaBruta  + ValItemBruto ;

    { Somando no Total diário da  aliquota }
    Aliq.Total := Aliq.Total + ItemCupom.TotalLiquido;

    VendeItemVirtual( ItemCupom );

    { Se o desconto é maior que zero envia o comando de desconto/acrescimo de item anterior }
    if ValorDescontoAcrescimo > 0 then
       DescontoAcrescimoItemAnterior( ValorDescontoAcrescimo,
                                      DescontoAcrescimo,
                                      TipoDescontoAcrescimo)    // Já chama GravaArqINI
    else
      GravaArqINI;
  except
    LeArqINI ;
    raise;
  end ;
end ;

procedure TACBrECFVirtualClass.DescontoAcrescimoItemAnterior(ValorDescontoAcrescimo: Double;
  DescontoAcrescimo: String; TipoDescontoAcrescimo: String; NumItem: Integer);
var
  ValorItem, ValDescAcres, PorcDescAcres: Double;
  StrDescAcre : String ;
  PosAliqItem: Integer;
  ItemCupom: TACBrECFVirtualClassItemCupom;
begin
  GravaLog( ComandoLOG );

  if (Estado <> estVenda) then
    raise EACBrECFERRO.create(ACBrStr('O Estado nao é "VENDA"')) ;

  if NumItem = 0 then
    NumItem := fpCupom.Itens.Count;

  VerificaFaixaItem(NumItem);

  ItemCupom := fpCupom.Itens[NumItem-1];

  if (ItemCupom.DescAcres > 0) then
    raise EACBrECFERRO.create(ACBrStr('Item ('+IntToStrZero(NumItem,3)+') já recebeu Acréscimo.')) ;

  if (ItemCupom.DescAcres < 0) then
    raise EACBrECFERRO.create(ACBrStr('Item ('+IntToStrZero(NumItem,3)+') já recebeu Desconto.')) ;

  StrDescAcre := IfThen(DescontoAcrescimo = 'D', 'DESCONTO', 'ACRESCIMO');

  with ItemCupom do
  begin
    ValorItem   := TotalBruto;
    PosAliqItem := AliqPos;
  end;

  if TipoDescontoAcrescimo = '%' then
  begin
    PorcDescAcres := ValorDescontoAcrescimo ;
    ValDescAcres  := RoundECF( ValorItem * (ValorDescontoAcrescimo / 100) );
  end
  else
  begin
    PorcDescAcres := RoundTo( (ValorDescontoAcrescimo / ValorItem) * 100, -2) ;
    ValDescAcres  := RoundECF( ValorDescontoAcrescimo );
  end;

  if PorcDescAcres >= 100 then
    raise EACBrECFERRO.create(ACBrStr(StrDescAcre+' maior do que 99,99%'));

  if DescontoAcrescimo = 'D' then
    ValDescAcres := -ValDescAcres;

  if TipoDescontoAcrescimo <> '%' then
    PorcDescAcres := 0;             // Preenche apenas se o Desconto for em %

  try
    fpCupom.DescAcresItem(NumItem, ValDescAcres);

    if (ValDescAcres < 0) then
    begin
      // Atualiza Total de Desconto
      if (fpAliquotas[PosAliqItem].Tipo = 'S') then
        fpTotalDescontosISSQN := Max(fpTotalDescontosISSQN - ValDescAcres, 0)
      else
        fpTotalDescontosICMS  := Max(fpTotalDescontosICMS  - ValDescAcres, 0);
    end
    else
    begin
      if (fpAliquotas[PosAliqItem].Tipo = 'S') then
        fpTotalAcrescimosISSQN := fpTotalAcrescimosISSQN + ValDescAcres
      else
        fpTotalAcrescimosICMS  := fpTotalAcrescimosICMS + ValDescAcres;

      { Se for Acréscimo, deve somar em GT e Venda Bruta }
      fpGrandeTotal := fpGrandeTotal + ValDescAcres;
      fpVendaBruta  := fpVendaBruta  + ValDescAcres;
    end;

    { Aplicando Desconto/Acrescimo no Total diário da Aliquota }
    with fpAliquotas[PosAliqItem] do
      Total := max(Total + ValDescAcres, 0) ;

    DescontoAcrescimoItemAnteriorVirtual( ItemCupom, PorcDescAcres );

    GravaArqINI;
  except
    LeArqINI ;
    raise;
  end ;
end;

 procedure TACBrECFVirtualClass.CancelaDescontoAcrescimoItem( NumItem : Integer;
      TipoAcrescimoDesconto: String = 'D') ;
var
  ValDescAcres: Double;
  PosAliqItem: Integer;
  ItemCupom: TACBrECFVirtualClassItemCupom;
begin
  GravaLog( ComandoLOG );

  if (Estado <> estVenda) then
    raise EACBrECFERRO.create(ACBrStr('O Estado nao é "VENDA"')) ;

  if NumItem = 0 then
    NumItem := fpCupom.Itens.Count;

  VerificaFaixaItem(NumItem);

  ItemCupom := fpCupom.Itens[NumItem-1];

  if (ItemCupom.DescAcres = 0) then
    raise EACBrECFERRO.create(ACBrStr('Item ('+IntToStrZero(NumItem,3)+') não possui Desconto ou Acréscimo.')) ;

  with ItemCupom do
  begin
    PosAliqItem  := AliqPos;
    ValDescAcres := DescAcres;
  end;

  try
    fpCupom.CancelaDescontoAcrescimoItem(NumItem, TipoAcrescimoDesconto);
    ItemCupom.DescAcres := 0;

    if (ValDescAcres < 0) then
    begin
      // Atualiza Total de Desconto
      if (fpAliquotas[PosAliqItem].Tipo = 'S') then
        fpTotalDescontosISSQN := Max(fpTotalDescontosISSQN + abs(ValDescAcres ), 0)
      else
        fpTotalDescontosICMS  := Max(fpTotalDescontosICMS  + abs(ValDescAcres), 0);
    end
    else
    begin
      if (fpAliquotas[PosAliqItem].Tipo = 'S') then
        fpTotalAcrescimosISSQN := fpTotalAcrescimosISSQN - ValDescAcres
      else
        fpTotalAcrescimosICMS  := fpTotalAcrescimosICMS - ValDescAcres;

      { Se for Acréscimo, deve somar em GT e Venda Bruta }
      fpGrandeTotal := fpGrandeTotal - ValDescAcres;
      fpVendaBruta  := fpVendaBruta  - ValDescAcres;
    end;

    { Aplicando Desconto/Acrescimo no Total diário da Aliquota }
    with fpAliquotas[PosAliqItem] do
      Total := max(Total - ValDescAcres, 0) ;

    CancelaDescontoAcrescimoItemVirtual( ItemCupom, TipoAcrescimoDesconto );

    GravaArqINI;
  except
    LeArqINI ;
    raise;
  end ;
 end;

procedure TACBrECFVirtualClass.VendeItemVirtual(
  ItemCupom: TACBrECFVirtualClassItemCupom);
begin
  {}
end;

procedure TACBrECFVirtualClass.DescontoAcrescimoItemAnteriorVirtual(
  ItemCupom: TACBrECFVirtualClassItemCupom; PorcDesc: Double);
begin
  {}
end;

procedure TACBrECFVirtualClass.CancelaItemVendido(NumItem: Integer);
var 
  ValorItem, DescAcresItem, TotalCanc: Double;
  PosAliqItem: Integer;
  wTipoAliq: Char;
begin
  GravaLog( ComandoLOG );

  if Estado <> estVenda then
    raise EACBrECFERRO.create(ACBrStr('O Estado nao é "VENDA"')) ;

  VerificaFaixaItem(NumItem);

  if fpCupom.Itens[NumItem-1].Qtd = 0 then
    raise EACBrECFERRO.create(ACBrStr('Item ('+IntToStrZero(NumItem,3)+') já foi cancelado.')) ;

  try
    CancelaItemVendidoVirtual( NumItem );

    with fpCupom.Itens[NumItem-1] do
    begin
      ValorItem     := TotalLiquido;
      PosAliqItem   := AliqPos;
      DescAcresItem := DescAcres;
      TotalCanc     := TotalBruto;
      wTipoAliq     := fpAliquotas[AliqPos].Tipo;
    end;

    fpCupom.CancelaItem( NumItem );

    if (DescAcresItem > 0) then
      TotalCanc := ValorItem;

    if (wTipoAliq = 'S') then
      fpCuponsCanceladosEmAbertoTotalISSQN := fpCuponsCanceladosEmAbertoTotalISSQN + TotalCanc
    else
      fpCuponsCanceladosEmAbertoTotalICMS  := fpCuponsCanceladosEmAbertoTotalICMS + TotalCanc;

    { Estornando do total de Acréscimos/Descontos. VendaBruta e GT nunca são estornadas }
    if (DescAcresItem < 0) then
    begin
      // Atualiza Total de Desconto
      if (wTipoAliq = 'S') then
        fpTotalDescontosISSQN := (fpTotalDescontosISSQN + DescAcresItem)
      else
        fpTotalDescontosICMS  := (fpTotalDescontosICMS  + DescAcresItem);
    end
    else
    begin
      // Atualiza Total de Acréscimo
      if (wTipoAliq = 'S') then
        fpTotalAcrescimosISSQN := fpTotalAcrescimosISSQN - DescAcresItem
      else
        fpTotalAcrescimosICMS  := fpTotalAcrescimosICMS - DescAcresItem;
    end;

    { Estornando do Total por aliquota }
    with fpAliquotas[ PosAliqItem ] do
      Total := max(Total - ValorItem, 0) ;

    GravaArqINI;
  except
    LeArqINI ;
    raise;
  end ;
end;

procedure TACBrECFVirtualClass.CancelaItemVendidoVirtual(NumItem: Integer);
begin
  {}
end;

procedure TACBrECFVirtualClass.SubtotalizaCupom(DescontoAcrescimo: Double;
       MensagemRodape : AnsiString );
var
  ValorTotal: Double;
  PosAliqItem, I: Integer;
begin
  GravaLog( ComandoLOG );

  if not (Estado in [estVenda, estNaoFiscal]) then
    raise EACBrECFERRO.create(ACBrStr('O Estado nao é "VENDA" Cupom não Aberto')) ;

  if SubTotal <= 0 then
    raise EACBrECFERRO.create(ACBrStr('Nenhum Item foi vendido ainda')) ;

  try
    { Essa atribuição irá recomputar o total por aliquota, considerando o Rateio
      desse Desconto / Acrescimo, nos totais por aliquota do Cupom.
      Veja "TACBrECFVirtualClassCupom.SetDescAcresSubtotal" }
    fpCupom.DescAcresSubtotal := DescontoAcrescimo;

    SetEstadoECFVirtual(estPagamento);

    if (DescontoAcrescimo < 0) then
    begin
      fpTotalDescontosICMS  := fpTotalDescontosICMS  - fpCupom.DescAcresSubtotalICMS;
      fpTotalDescontosISSQN := fpTotalDescontosISSQN - fpCupom.DescAcresSubtotalISSQN;
    end
    else
    begin
      fpTotalAcrescimosICMS  := fpTotalAcrescimosICMS  + fpCupom.DescAcresSubtotalICMS;
      fpTotalAcrescimosISSQN := fpTotalAcrescimosISSQN + fpCupom.DescAcresSubtotalISSQN;

      { Se for Acréscimo, deve somar em GT e Venda Bruta }
      fpVendaBruta  := fpVendaBruta  + DescontoAcrescimo;
      fpGrandeTotal := fpGrandeTotal + DescontoAcrescimo;
    end;

    { Recomputando Total Diário das Aliquotas. Lista fpCupom.Aliquotas,
      contem o total por Aliquota do Cupom, já considerando se o rateio de
      Desconto e Acrescimo no SubTotal }
    if fpCupom.Aliquotas.Count > 0 then
    begin
      { Primeiro, vamos remover o ValorTotal por Item, que já havia sido
        adicionado em "VendeItem"; }
      For I := 0 to fpCupom.Itens.Count-1 do
      begin
        with fpCupom.Itens[I] do
        begin
          ValorTotal := TotalLiquido;
          PosAliqItem:= AliqPos;
        end;

        with fpAliquotas[ PosAliqItem ] do
          Total := max(Total - ValorTotal, 0) ;
      end;

      { Agora, vamos adicionar o total computado por aliquota usada no cupom.
        Essa lista já contem o rateio do Desconto/Acrescimo dessa operação }
      for I := 0 to fpCupom.Aliquotas.Count-1 do
      begin
        with fpCupom.Aliquotas[I] do
        begin
          ValorTotal := TotalLiquido;
          PosAliqItem:= AliqPos;
        end;

        with fpAliquotas[ PosAliqItem ] do
          Total := max(Total + ValorTotal, 0) ;
      end;
    end;

    SubtotalizaCupomVirtual( MensagemRodape );

    GravaArqINI ;
  except
    LeArqINI ;
    raise;
  end ;
end;

procedure TACBrECFVirtualClass.SubtotalizaCupomVirtual(MensagemRodape: AnsiString);
begin
  {}
end;

procedure TACBrECFVirtualClass.EfetuaPagamento(CodFormaPagto: String; Valor: Double;
  Observacao: AnsiString; ImprimeVinculado: Boolean; CodMeioPagamento: Integer);
Var
  FPG : TACBrECFFormaPagamento ;
  Troco : Double ;
  Pagto : TACBrECFVirtualClassPagamentoCupom ;
  IndiceFPG: Integer;
begin
  GravaLog( ComandoLOG );

  if Estado <> estPagamento then
    raise EACBrECFERRO.create(ACBrStr('O Estado nao é "PAGAMENTO"')) ;

  if TotalPago >= SubTotal then
    raise EACBrECFERRO.create(ACBrStr('Total pago já foi atingido Cupom deve ser '+
                                      'encerrado')) ;

  FPG := AchaFPGIndice( CodFormaPagto ) ;
  if FPG = nil then
    raise EACBrECFERRO.create(ACBrStr('Forma de Pagamento '+CodFormaPagto+' Inválida')) ;

  IndiceFPG := StrToInt( FPG.Indice );
  if IndiceFPG < 1 then
    raise EACBrECFERRO.create(ACBrStr('Forma de Pagamento '+FPG.Indice+'-'+FPG.Descricao+
                                      ' não pode ser usada para Pagamentos')) ;

  try
    Pagto := fpCupom.EfetuaPagamento( Valor, Observacao, IndiceFPG);
    FPG.Total := RoundTo(FPG.Total + Pagto.ValorPago,-2) ;

    { Se tiver Troco, remove de 01 - DINHEIRO (indice = 0) }
    Troco := 0 ;
    if fpCupom.TotalPago >= fpCupom.SubTotal then  { Tem TROCO ? }
    begin
      Troco := fpCupom.TotalPago - fpCupom.SubTotal;
      Troco := RoundTo(Troco, -2) ;
    end;

    if Troco > 0 then
    begin
      FPG := fpFormasPagamentos[ 0 ] ;  // 0 = 00-TROCO
      FPG.Total := RoundTo(FPG.Total + Troco,-2) ;

      { Lançando o Troco como um pagamento no Cupom, porém negativo, com isso
        o Cancelamento de Cupom conseguir desfaze-lo }
      fpCupom.EfetuaPagamento( -Troco, 'TROCO', 0 );
    end ;

    EfetuaPagamentoVirtual( Pagto );

    GravaArqINI ;
  except
    LeArqINI ;
    raise;
  end ;
end;

procedure TACBrECFVirtualClass.EfetuaPagamentoVirtual(
  Pagto: TACBrECFVirtualClassPagamentoCupom);
begin
  {}
end;

procedure TACBrECFVirtualClass.FechaCupom(Observacao: AnsiString; IndiceBMP : Integer);
begin
  GravaLog( ComandoLOG );

  if Estado <> estPagamento then
    raise EACBrECFERRO.create(ACBrStr('O Estado nao é "PAGAMENTO", não houve SubTotal')) ;

  if CompareValue(TotalPago, SubTotal, 0.001) < 0 then
    raise EACBrECFERRO.create(ACBrStr('Total Pago é inferior ao Total do Cupom')) ;

  Observacao := StringReplace( Observacao, #10, CRLF, [rfReplaceAll] ) ;

  try
    EnviaConsumidorVirtual;
    FechaCupomVirtual(Observacao, IndiceBMP);

    SetEstadoECFVirtual(estLivre);

    GravaArqINI ;
  except
    LeArqINI;
    raise;
  end ;
end;

procedure TACBrECFVirtualClass.FechaCupomVirtual(Observacao: AnsiString;
  IndiceBMP: Integer);
begin
  {}
end;

procedure TACBrECFVirtualClass.CancelaCupom(NumCOOCancelar: Integer);
Var
  I, PosAliqItem, OldCOO: Integer;
  TotalAliq, SubTotalBrutoICMS, SubTotalBrutoISSQN: Double;
  PermiteCancelamento, CupomEstaAberto, EhNaoFiscal, AliquotasRemovidas: Boolean;
begin
  GravaLog( ComandoLOG );

  if NumCOOCancelar = 0 then
    NumCOOCancelar := fpNumCOO;

  if Assigned(fsQuandoCancelarCupom) then
  begin
    {
      A aplicação pode programar o evento "ACBrECFVirtual.QuandoCancelarCupom", para:
      1 - Verificar se o cancelamento do Documento com o "NumCOOCancelar", é
          permitido ou não. Se não o for, a aplicação pode:
          - Disparar um exception internamente, com uma mensagem específica do erro
            ou
          - Atribuir "False" ao parâmetro: PermiteCancelamento,
      2 - Atribuir dados do Cupom a ser cancelado ao parâmetro "CupomVirtual".
          Isso fará com que os valores desse cupom sejam estornados da memória
          do ECFVirtual, e portanto esse Cancelamento seja refletido na Redução Z
    }
    PermiteCancelamento := True;
    fsQuandoCancelarCupom( NumCOOCancelar, fpCupom, PermiteCancelamento) ;

    if not PermiteCancelamento then
      raise EACBrECFERRO.Create(ACBrStr('Cancelamento não permitido pela aplicação')) ;
  end
  else
    VerificaPodeCancelarCupom( NumCOOCancelar );

  try
    EhNaoFiscal := (fpCupom.CNF.Count > 0);
    CupomEstaAberto := (Estado in [estVenda, estPagamento, estNaoFiscal] );
    if not CupomEstaAberto then  // Cria um novo documento de Cancelamento
      Inc( fpNumCOO );

    OldCOO := fpNumCOO;
    CancelaCupomVirtual;

    { CancelaCupomVirtual pode modificar o valor de fpNumCOO, se considerar que
      o Documento já foi transmitido. Reavaliando valor de "CupomEstaAberto" }
    CupomEstaAberto := CupomEstaAberto and (fpNumCOO = OldCOO);

    { Aqui já temos todos os Descontos e Acrescimos. Os descontos devem ser
      revertidos, para calcularmos o SubTotalBruto }
    SubTotalBrutoICMS  := fpCupom.SubtotalICMS;
    SubTotalBrutoISSQN := fpCupom.SubtotalISSQN;

    { Removendo Desconto/Acrescimo do Subtotal, dos Totalizadores }
    if (fpCupom.DescAcresSubtotal < 0) then
    begin
      fpTotalDescontosICMS  := fpTotalDescontosICMS  + fpCupom.DescAcresSubtotalICMS;
      fpTotalDescontosISSQN := fpTotalDescontosISSQN + fpCupom.DescAcresSubtotalISSQN;

      SubTotalBrutoICMS  := SubTotalBrutoICMS  - fpCupom.DescAcresSubtotalICMS;
      SubTotalBrutoISSQN := SubTotalBrutoISSQN - fpCupom.DescAcresSubtotalISSQN;
    end
    else
    begin
      fpTotalAcrescimosICMS  := fpTotalAcrescimosICMS  - fpCupom.DescAcresSubtotalICMS;
      fpTotalAcrescimosISSQN := fpTotalAcrescimosISSQN - fpCupom.DescAcresSubtotalISSQN;
    end;

    { Removendo do TotalDiario por Aliquotas }
    AliquotasRemovidas := False;
    if fpCupom.Aliquotas.Count > 0 then
    begin
      AliquotasRemovidas := True;
      For I := 0 to fpCupom.Aliquotas.Count - 1 do
      begin
        with fpCupom.Aliquotas[I] do
        begin
          PosAliqItem := AliqPos;
          TotalAliq   := TotalLiquido;
        end;

        with fpAliquotas[ PosAliqItem ] do
          Total := Max( RoundTo(Total - TotalAliq,-2), 0) ;
      end;
    end;

    { Removendo Desconto/Acrescimo dos Itens dos Totalizadores }
    for I := 0 to fpCupom.Itens.Count - 1 do
    begin
      with fpCupom.Itens[I] do
      begin
        if Qtd > 0 then // Verifica se Item já não foi cancelado
        begin
          if not AliquotasRemovidas then     // Já removeu Aliquotas no passo anterior ?
          begin
            with fpAliquotas[ AliqPos ] do
              Total := Max( RoundTo(Total - TotalLiquido,-2), 0) ;
          end;

          if (DescAcres < 0) then
          begin
            if (fpAliquotas[AliqPos].Tipo = 'S') then
            begin
              fpTotalDescontosISSQN := fpTotalDescontosISSQN + DescAcres;
              SubTotalBrutoISSQN    := SubTotalBrutoISSQN - DescAcres;
            end
            else
            begin
              fpTotalDescontosICMS := fpTotalDescontosICMS + DescAcres;
              SubTotalBrutoICMS    := SubTotalBrutoICMS - DescAcres;
            end;
          end
          else
          begin
            if (fpAliquotas[AliqPos].Tipo = 'S') then
              fpTotalAcrescimosISSQN := fpTotalAcrescimosISSQN - DescAcres
            else
              fpTotalAcrescimosICMS  := fpTotalAcrescimosICMS - DescAcres;
          end;
        end;
      end;
    end;

    { Adicionando o TotalBruto do Cupom, em Cancelamentos, conforme o Estado do Cupom }
    if EhNaoFiscal then
    begin
      fpCNFCancelados      := fpCNFCancelados + 1 ;
      fpCNFCanceladosTotal := fpCNFCanceladosTotal + (SubTotalBrutoICMS + SubTotalBrutoISSQN);
    end
    else if CupomEstaAberto then
    begin
      fpCuponsCanceladosEmAberto := fpCuponsCanceladosEmAberto + 1;

      fpCuponsCanceladosEmAbertoTotalICMS  := fpCuponsCanceladosEmAbertoTotalICMS  + SubTotalBrutoICMS;
      fpCuponsCanceladosEmAbertoTotalISSQN := fpCuponsCanceladosEmAbertoTotalISSQN + SubTotalBrutoISSQN;
    end
    else
    begin
      fpCuponsCancelados      := fpCuponsCancelados + 1;

      fpCuponsCanceladosTotalICMS  := fpCuponsCanceladosTotalICMS  + SubTotalBrutoICMS;
      fpCuponsCanceladosTotalISSQN := fpCuponsCanceladosTotalISSQN + SubTotalBrutoISSQN;
    end;

    { Removendo do TotalDiario por Pagamento }
    For I := 0 to fpCupom.Pagamentos.Count - 1 do
      with fpCupom.Pagamentos[I] do
        with fpFormasPagamentos[ PosFPG ] do
          Total := Max( RoundTo(Total - ValorPago,-2), 0) ;

    { Removendo do TotalDiario por CNF }
    For I := 0 to fpCupom.CNF.Count - 1 do
      with fpCupom.CNF[I] do
        with fpComprovantesNaoFiscais[ PosCNF ] do
          Total := Max( RoundTo(Total - Valor,-2), 0) ;

    ZeraCupom;
    SetEstadoECFVirtual(estLivre);

    GravaArqINI ;
  except
    LeArqINI ;
    raise ;
  end ;
end;

procedure TACBrECFVirtualClass.CancelaCupomVirtual;
begin
  {}
end;

procedure TACBrECFVirtualClass.CancelaDescontoAcrescimoItemVirtual(
  ItemCupom: TACBrECFVirtualClassItemCupom; TipoAcrescimoDesconto: String);
begin
  {}
end;

procedure TACBrECFVirtualClass.LeituraX ;
var
  AbrirDia : Boolean ;
begin
  GravaLog( ComandoLOG );

  if Estado <> estRequerX then
    TestaPodeAbrirCupom ;

  AbrirDia := ( Estado in [estRequerX, estRequerZ] ) ;

  try
    ZeraCupom ;
    fpLeiturasX := fpLeiturasX + 1 ;
    SetEstadoECFVirtual(estLivre);

    if AbrirDia then
      AbreDia;

    LeituraXVirtual;
    AbreDocumento ;
  except
    LeArqINI ;
    raise ;
  end ;
end;

procedure TACBrECFVirtualClass.LeituraXVirtual;
begin
  {}
end;

procedure TACBrECFVirtualClass.ReducaoZ(DataHora : TDateTime) ;
var
  A: Integer ;
begin
  GravaLog( ComandoLOG );

  if Estado = estBloqueada then
    raise EACBrECFERRO.Create(ACBrStr('Dia já foi fechado. Redução Z já emitida')) ;

  if not (Estado in [estLivre,estRequerZ]) then
    raise EACBrECFERRO.create(ACBrStr('O Estado não é "LIVRE" Cancele o último Documento')) ;

  try
    ZeraCupom;
    fpReducoesZ := fpReducoesZ + 1 ;

    ReducaoZVirtual( DataHora );

    if (fpEstado = estRequerZ) then
    begin
      SetEstadoECFVirtual(estLivre);
      fpDia := now ;
    end
    else
      SetEstadoECFVirtual(estBloqueada);

    fpNumCER     := 0;
    fpVendaBruta := 0;
    fpTotalDescontosICMS   := 0;
    fpTotalDescontosISSQN  := 0;
    fpTotalAcrescimosICMS  := 0;
    fpTotalAcrescimosISSQN := 0;
    fpCNFCancelados        := 0;
    fpCNFCanceladosTotal   := 0;
    fpCuponsCancelados     := 0;
    fpCuponsCanceladosTotalICMS  := 0;
    fpCuponsCanceladosTotalISSQN := 0;
    fpCuponsCanceladosEmAberto   := 0;
    fpCuponsCanceladosEmAbertoTotalICMS  := 0;
    fpCuponsCanceladosEmAbertoTotalISSQN := 0;

    For A := 0 to fpAliquotas.Count - 1 do
      fpAliquotas[A].Total := 0 ;

    For A := 0 to fpFormasPagamentos.Count - 1 do
      fpFormasPagamentos[A].Total := 0 ;

    For A := 0 to fpComprovantesNaoFiscais.Count - 1 do
    begin
      fpComprovantesNaoFiscais[A].Total := 0 ;
      fpComprovantesNaoFiscais[A].Contador := 0 ;
    end;

    For A := 0 to fpRelatoriosGerenciais.Count - 1 do
        fpRelatoriosGerenciais[A].Contador := 0 ;

    AbreDia;
    AbreDocumento ;
  except
    LeArqINI ;
    raise ;
  end ;
end;

procedure TACBrECFVirtualClass.ReducaoZVirtual(DataHora: TDateTime);
begin
  {}
end;

procedure TACBrECFVirtualClass.GetEstadoECFVirtual;
begin
  {}
end;

procedure TACBrECFVirtualClass.SetEstadoECFVirtual(
  const NovoEstado: TACBrECFEstado);
var
  ANovoEstado: TACBrECFEstado;
begin
  GravaLog( 'SetEstadoECFVirtual: '+GetEnumName(TypeInfo(TACBrECFEstado), integer( NovoEstado ) ) );

  ANovoEstado := NovoEstado;
  if Assigned(fsQuandoMudarEstado) then
  begin
    fsQuandoMudarEstado(fpEstado, ANovoEstado);
    if ANovoEstado <> NovoEstado then
      GravaLog( '         Modificado: '+GetEnumName(TypeInfo(TACBrECFEstado), integer( ANovoEstado ) ) );
  end;

  fpEstado := ANovoEstado;
end;

procedure TACBrECFVirtualClass.AbreRelatorioGerencial(Indice : Integer) ;
var
  IndiceStr: String;
  RG: TACBrECFRelatorioGerencial;
begin
  GravaLog( ComandoLOG );

  if not (Estado in [estLivre,estRequerZ,estRequerX])  then
    raise EACBrECFERRO.Create(ACBrStr('O Estado não é "LIVRE"'));

  Indice := max(Indice,1);
  IndiceStr := IntToStrZero( Indice, 2 );
  RG := AchaRGIndice(IndiceStr);
  if RG = Nil then
    raise EACBrECFERRO.create(ACBrStr('Relatório Gerencial '+IndiceStr+' Inválido')) ;

  try
    fpNumGNF := fpNumGNF + 1 ;
    fpNumGRG := fpNumGRG + 1 ;
    fpNumCER := fpNumCER + 1 ;
    RG.Contador := RG.Contador + 1;

    ZeraCupom;
    SetEstadoECFVirtual(estRelatorio);

    AbreRelatorioGerencialVirtual( Indice );
    AbreDocumento ;

    GravaArqINI;
  except
    LeArqINI ;
    raise ;
  end ;
end;

procedure TACBrECFVirtualClass.AbreRelatorioGerencialVirtual(Indice: Integer);
begin
  {}
end;

procedure TACBrECFVirtualClass.LinhaRelatorioGerencial(Linha: AnsiString; IndiceBMP: Integer);
begin
  GravaLog( ComandoLOG );
end;

procedure TACBrECFVirtualClass.AbreCupomVinculado(COO, CodFormaPagto,
  CodComprovanteNaoFiscal: String; Valor: Double);
Var
  FPG : TACBrECFFormaPagamento ;
  I, PosFPG : Integer ;
  UsouPagamento : Boolean ;
  SubTotalCupomAnterior: Double;
begin
  GravaLog( ComandoLOG );

  if COO = '' then
    raise EACBrECFERRO.create(ACBrStr('COO inválido'));

  if Estado <> estLivre  then
    raise EACBrECFERRO.Create(ACBrStr('O Estado não é "LIVRE"')) ;

  if fpCupom.Pagamentos.Count < 1 then
    raise EACBrECFERRO.Create(ACBrStr('Ultimo Documento não é Cupom')) ;

  COO := Poem_Zeros(COO,6) ;

  FPG := AchaFPGIndice( CodFormaPagto ) ;
  if FPG = Nil then
    raise EACBrECFERRO.Create(ACBrStr('Posição de Pagamento: '+CodFormaPagto+' inválida'));

  if not FPG.PermiteVinculado then
    raise EACBrECFERRO.Create(ACBrStr('Forma de Pagamento: '+FPG.Indice+'-'+FPG.Descricao+
                                      ' não permite Vinculado'));

  UsouPagamento := False ;
  I := 0 ;
  while (not UsouPagamento) and (I < fpCupom.Pagamentos.Count) do
  begin
    PosFPG := fpCupom.Pagamentos[I].PosFPG ;
    UsouPagamento := (fpFormasPagamentos[ PosFPG ].Indice = FPG.Indice ) ;
    Inc( I ) ;
  end ;

  if not UsouPagamento then
    raise EACBrECFERRO.create(ACBrStr('Forma de Pagamento: '+FPG.Descricao+
                                      ' não foi utilizada no Cupom anterior')) ;

  try
    fpNumGNF := fpNumGNF + 1 ;
    fpNumCDC := fpNumCDC + 1 ;
    SubTotalCupomAnterior := Subtotal;

    //ZeraCupom;  // Não Zera Dados, para permitir chamar "CancelaCupom" após Vinculado
    SetEstadoECFVirtual(estRelatorio);

    AbreCupomVinculadoVirtual(COO, FPG, CodComprovanteNaoFiscal, SubTotalCupomAnterior, Valor);
    AbreDocumento ;
  except
     LeArqINI ;
     raise ;
  end ;
end;

procedure TACBrECFVirtualClass.AbreCupomVinculadoVirtual(COO: String;
  FPG: TACBrECFFormaPagamento; CodComprovanteNaoFiscal: String;
  SubtotalCupomAnterior, ValorFPG: Double);
begin
  {}
end;

procedure TACBrECFVirtualClass.LinhaCupomVinculado(Linha: AnsiString);
begin
  LinhaRelatorioGerencial( Linha );
end;

procedure TACBrECFVirtualClass.FechaRelatorio;
begin
  GravaLog( 'FechaRelatorio' );

  if Estado <> estRelatorio then exit ;

  try
    SetEstadoECFVirtual(estLivre);
    FechaRelatorioVirtual;

    GravaArqINI ;
  except
    LeArqINI;
    raise;
  end ;
end;

procedure TACBrECFVirtualClass.FechaRelatorioVirtual;
begin
  {}
end;

procedure TACBrECFVirtualClass.CortaPapel(const CorteParcial : Boolean) ;
begin
  GravaLog( ComandoLOG );
end ;

procedure TACBrECFVirtualClass.AbreNaoFiscal(CPF_CNPJ : String ; Nome : String ;
  Endereco : String) ;
begin
  GravaLog( ComandoLOG );
  TestaPodeAbrirCupom ;

  try
    ZeraCupom;
    SetEstadoECFVirtual(estNaoFiscal);

    AbreNaoFiscalVirtual(CPF_CNPJ, Nome);
    AbreDocumento ;
  except
    LeArqINI ;
    raise ;
  end ;
end;

procedure TACBrECFVirtualClass.AbreNaoFiscalVirtual(CPF_CNPJ: String;
  Nome: String; Endereco: String);
begin
  {}
end;

procedure TACBrECFVirtualClass.RegistraItemNaoFiscal(CodCNF : String ;
  Valor : Double ; Obs : AnsiString) ;
Var
  CNFCupom : TACBrECFVirtualClassCNFCupom ;
  CNF      : TACBrECFComprovanteNaoFiscal ;
  PosCNF   : Integer ;
begin
  GravaLog( ComandoLOG );
  if Estado <> estNaoFiscal then
    raise EACBrECFERRO.create(ACBrStr('Comprovante Não Fiscal não foi aberto')) ;

  if (Valor <= 0) then
    raise EACBrECFERRO.create(ACBrStr('Valor deve ser maior que Zero')) ;

  CNF := AchaCNFIndice( CodCNF );
  if CNF = Nil then
    raise EACBrECFERRO.create(ACBrStr('Comprovante Não Fiscal '+CodCNF+' Inválido')) ;

  PosCNF := fpComprovantesNaoFiscais.IndexOf( CNF );

  try
    CNFCupom := fpCupom.RegistraCNF(Valor, Obs, PosCNF);
    CNF.Total := RoundTo(CNF.Total + Valor,-2) ;

    RegistraItemNaoFiscalVirtual( CNFCupom );

    GravaArqINI ;
  except
    LeArqINI ;
    raise;
  end ;
end;

procedure TACBrECFVirtualClass.RegistraItemNaoFiscalVirtual(
  CNFCupom: TACBrECFVirtualClassCNFCupom);
begin
  {}
end;

procedure TACBrECFVirtualClass.CancelaItemNaoFiscal(const AItem: Integer);
var
  ValorItem: Double;
  PosCNFItem: Integer;
begin
  if (AItem < 1) or (AItem > fpCupom.CNF.Count) then
    raise EACBrECFERRO.create(ACBrStr('Item ('+IntToStrZero(AItem,3)+') fora da Faixa.')) ;

  try
    CancelaItemVendidoVirtual( AItem );

    with fpCupom.CNF[AItem-1] do
    begin
      ValorItem  := Valor;
      PosCNFItem := PosCNF;
    end;

    fpCupom.CancelaCNF( AItem );

    fpCNFCanceladosTotal := fpCNFCanceladosTotal + ValorItem;

    with fpComprovantesNaoFiscais[PosCNFItem] do
    begin
      Total := RoundTo(Total - ValorItem, -2) ;
    end;

    GravaArqINI;
  except
    LeArqINI ;
    raise;
  end;
end;

procedure TACBrECFVirtualClass.FechaNaoFiscal(Observacao: AnsiString;
  IndiceBMP: Integer);
begin
  GravaLog( ComandoLOG );

  Observacao := StringReplace( Observacao, #10, CRLF, [rfReplaceAll] ) ;

  try
    EnviaConsumidorVirtual;
    FechaCupomVirtual(Observacao, IndiceBMP);
    SetEstadoECFVirtual(estLivre);

    GravaArqINI ;
  except
    LeArqINI;
    raise;
  end ;
end;

procedure TACBrECFVirtualClass.Sangria(const Valor: Double; Obs: AnsiString;
  DescricaoCNF: String; DescricaoFPG: String; IndiceBMP: Integer);
Var
  CNF : TACBrECFComprovanteNaoFiscal ;
begin
  CNF := AchaCNFDescricao(DescricaoCNF, True) ;
  if CNF = nil then
     raise EACBrECFErro.Create( ACBrStr(Format(cACBrECFAchaCNFException,
                                   [ DescricaoCNF ] ))) ;

  AbreNaoFiscal();
  try
     RegistraItemNaoFiscal(CNF.Indice, Valor);
     FechaNaoFiscal( Obs, IndiceBMP);
  except
     try
        CancelaNaoFiscal
     except
     end;

     raise ;
  end ;
end;

procedure TACBrECFVirtualClass.CancelaItemNaoFiscalVirtual(NumItem: Integer);
begin
  {}
end;

procedure TACBrECFVirtualClass.LeArqINI;
Var
  SL      : TStringList ;
  Tratado : Boolean;
begin
  GravaLog('LeArqINI');

  if fpNomeArqINI = '' then
    fpNomeArqINI := CalculaNomeArqINI;

  Tratado := false;
  SL      := TStringList.Create() ;
  try
    if Assigned( fsQuandoLerArqINI ) then
      fsQuandoLerArqINI( SL, Tratado );

    if not Tratado then
      LeArqINIVirtual( SL );

    INItoClass( SL );
  finally
    SL.Free;
  end;
end;

procedure TACBrECFVirtualClass.LeArqINIVirtual(ConteudoINI: TStrings);
begin
  if not FileExists( fpNomeArqINI ) then
    CriarMemoriaInicial;

  ConteudoINI.LoadFromFile( fpNomeArqINI );
end;

procedure TACBrECFVirtualClass.INItoClass(ConteudoINI: TStrings);
Var
  Ini : TMemIniFile;
  A   : Integer ;
  S,T : String ;
  AliqICMS           : TACBrECFAliquota;
  FormaPagamento     : TACBrECFFormaPagamento;
  ComprovanteVirtual : TACBrECFComprovanteNaoFiscal;
  RelatGerencial     : TACBrECFRelatorioGerencial;
begin
  GravaLog('INItoClass');
  Ini := TMemIniFile.Create('');
  try
    Ini.Clear;
    Ini.SetStrings(ConteudoINI);

    fpEstado      := TACBrECFEstado(Ini.ReadInteger('Variaveis','Estado',Integer(fpEstado)));
    fpNumCOO      := Ini.ReadInteger('Variaveis','NumCupom',fpNumCOO);
    fpNumGNF      := Ini.ReadInteger('Variaveis','NumGNF',fpNumGNF);
    fpNumGRG      := Ini.ReadInteger('Variaveis','NumGRG',fpNumGRG);
    fpNumCDC      := Ini.ReadInteger('Variaveis','NumCDC',fpNumCDC);
    fpNumCER      := Ini.ReadInteger('Variaveis','NumCER',fpNumCER);
    fpGrandeTotal := Ini.ReadFloat('Variaveis','GrandeTotal',fpGrandeTotal);
    fpVendaBruta  := Ini.ReadFloat('Variaveis','VendaBruta',fpVendaBruta);
    fpNumCCF      := Ini.ReadInteger('Variaveis','NumCCF',fpNumCCF);
    fpDia         := Ini.ReadDate('Variaveis','DiaMovimento',fpDia);
    fpVerao       := Ini.ReadBool('Variaveis','HorarioVerao',fpVerao);
    fpReducoesZ   := Ini.ReadInteger('Variaveis','ReducoesZ',fpReducoesZ);
    fpLeiturasX   := Ini.ReadInteger('Variaveis','LeiturasX',fpLeiturasX);
    fpCOOInicial  := Ini.ReadInteger('Variaveis','COOInicial',fpCOOInicial);
    fpCOOFinal    := Ini.ReadInteger('Variaveis','COOFinal',fpCOOFinal);
    Operador      := Ini.ReadString('Variaveis','Operador',Operador);
    fpPAF         := Ini.ReadString('Variaveis','PAF',fpPAF);
    fpCuponsCancelados   := Ini.ReadInteger('Variaveis','CuponsCancelados', fpCuponsCancelados);
    fpCNFCancelados      := Ini.ReadInteger('Variaveis','CNFCancelados', fpCNFCancelados);
    fpCNFCanceladosTotal := Ini.ReadFloat('Variaveis', 'CNFCanceladosTotal', fpCNFCanceladosTotal);

    fpCuponsCanceladosTotalICMS  := Ini.ReadFloat('Variaveis',
       'CuponsCanceladosTotal', fpCuponsCanceladosTotalICMS);
    fpCuponsCanceladosTotalISSQN := Ini.ReadFloat('Variaveis',
       'CuponsCanceladosTotalISSQN', fpCuponsCanceladosTotalISSQN);
    fpCuponsCanceladosEmAberto := Ini.ReadInteger('Variaveis',
       'CuponsCanceladosEmAberto', fpCuponsCanceladosEmAberto);
    fpCuponsCanceladosEmAbertoTotalICMS := Ini.ReadFloat('Variaveis',
       'CuponsCanceladosEmAbertoTotal', fpCuponsCanceladosEmAbertoTotalICMS);
    fpCuponsCanceladosEmAbertoTotalISSQN := Ini.ReadFloat('Variaveis',
       'CuponsCanceladosEmAbertoTotalISSQN', fpCuponsCanceladosEmAbertoTotalISSQN);

    fpTotalDescontosICMS   := Ini.ReadFloat('Variaveis', 'TotalDescontos', fpTotalDescontosICMS);
    fpTotalAcrescimosICMS  := Ini.ReadFloat('Variaveis', 'TotalAcrescimos', fpTotalAcrescimosICMS);
    fpTotalDescontosISSQN  := Ini.ReadFloat('Variaveis', 'TotalDescontosISSQN', fpTotalDescontosISSQN);
    fpTotalAcrescimosISSQN := Ini.ReadFloat('Variaveis', 'TotalAcrescimosISSQN', fpTotalAcrescimosISSQN);

    inherited CarregaAliquotas ;   { Cria fpAliquotas }
    S := 'Aliquotas';
    A := 0 ;
    while true do
    begin
      T := Ini.ReadString( S, IntToStrZero(A,2), '*FIM*') ;
      if T = '*FIM*' then break ;

      AliqICMS := TACBrECFAliquota.Create ;
      AliqICMS.AsString := T ;

      fpAliquotas.Add( AliqICMS ) ;
      A := A + 1 ;
    end ;

    inherited CarregaFormasPagamento ;   { Cria fpFormasPagamentos }
    A := 0 ;
    S := 'Formas_Pagamento';
    while true do
    begin
      T := Ini.ReadString( S, IntToStrZero(A,2), '*FIM*') ;
      if T = '*FIM*' then break ;

      FormaPagamento := TACBrECFFormaPagamento.Create ;
      FormaPagamento.AsString := T ;

      fpFormasPagamentos.Add( FormaPagamento ) ;
      A := A + 1 ;
    end ;

    inherited CarregaRelatoriosGerenciais ;   { Cria fpRelatoriosGerenciais }
    A := 0 ;
    S := 'Relatorios_Gerenciais';
    while true do
    begin
      T := Ini.ReadString( S, IntToStrZero(A,2), '*FIM*') ;
      if T = '*FIM*' then break ;

      RelatGerencial := TACBrECFRelatorioGerencial.Create ;
      RelatGerencial.AsString := T ;

      fpRelatoriosGerenciais.Add( RelatGerencial ) ;
      A := A + 1 ;
    end ;

    inherited CarregaComprovantesNaoFiscais ;   { Cria fpComprovantesNaoFiscais }
    A := 0 ;
    S := 'Comprovantes_nao_Fiscais';
    while true do
    begin
      T := Ini.ReadString( S, IntToStrZero(A,2), '*FIM*') ;
      if T = '*FIM*' then break ;

      ComprovanteVirtual := TACBrECFComprovanteNaoFiscal.Create ;
      ComprovanteVirtual.AsString := T ;

      fpComprovantesNaoFiscais.Add( ComprovanteVirtual ) ;
      A := A + 1 ;
    end ;

    fpCupom.LoadFromINI(Ini);
  finally
    Ini.Free ;
  end ;
end;

procedure TACBrECFVirtualClass.CriarMemoriaInicial;
Var
  ComprovanteNaoFiscal: TACBrECFComprovanteNaoFiscal;
  RG: TACBrECFRelatorioGerencial;
begin
  GravaLog('CriarMemoriaInicial');
  try
     if fpNumSerie = '' then
       fpNumSerie := 'ACBR01NF'+ FormatDateTime( 'ddmmyyhhnnss', now ) +  ' ' ;
  except
  end ;

  CriarAliquotasPadrao;

  CriarFormasPagamentoPadrao;

  FreeAndNil( fpRelatoriosGerenciais ) ;
  inherited CarregaRelatoriosGerenciais;

  FreeAndNil( fpComprovantesNaoFiscais ) ;
  inherited CarregaComprovantesNaoFiscais;

  ComprovanteNaoFiscal := TACBrECFComprovanteNaoFiscal.create ;
  ComprovanteNaoFiscal.Indice    := '01' ;
  ComprovanteNaoFiscal.Descricao := 'SANGRIA' ;
  fpComprovantesNaoFiscais.Add( ComprovanteNaoFiscal ) ;

  ComprovanteNaoFiscal := TACBrECFComprovanteNaoFiscal.create ;
  ComprovanteNaoFiscal.Indice    := '02' ;
  ComprovanteNaoFiscal.Descricao := 'SUPRIMENTO' ;
  fpComprovantesNaoFiscais.Add( ComprovanteNaoFiscal ) ;

  RG := TACBrECFRelatorioGerencial.create;
  RG.Indice := '01';
  RG.Descricao := 'DIVERSOS';
  fpRelatoriosGerenciais.Add( RG );

  GravaArqINI ;
end ;

procedure TACBrECFVirtualClass.CriarAliquotasPadrao;
var
  Aliquota : TACBrECFAliquota ;
begin
  FreeAndNil( fpAliquotas ) ;
  inherited CarregaAliquotas;

  Aliquota := TACBrECFAliquota.create ;
  Aliquota.Indice   := 'F1' ;
  Aliquota.Tipo     := 'T' ;
  fpAliquotas.Add( Aliquota ) ;

  Aliquota := TACBrECFAliquota.create ;
  Aliquota.Indice   := 'I1' ;
  Aliquota.Tipo     := 'T' ;
  fpAliquotas.Add( Aliquota ) ;

  Aliquota := TACBrECFAliquota.create ;
  Aliquota.Indice   := 'N1' ;
  Aliquota.Tipo     := 'T' ;
  fpAliquotas.Add( Aliquota ) ;

  Aliquota := TACBrECFAliquota.create ;
  Aliquota.Indice   := 'FS1' ;
  Aliquota.Tipo     := 'S' ;
  fpAliquotas.Add( Aliquota ) ;

  Aliquota := TACBrECFAliquota.create ;
  Aliquota.Indice   := 'IS1' ;
  Aliquota.Tipo     := 'S' ;
  fpAliquotas.Add( Aliquota ) ;

  Aliquota := TACBrECFAliquota.create ;
  Aliquota.Indice   := 'NS1' ;
  Aliquota.Tipo     := 'S' ;
  fpAliquotas.Add( Aliquota ) ;
end;

procedure TACBrECFVirtualClass.CriarFormasPagamentoPadrao;
var
  FormaPagamento: TACBrECFFormaPagamento ;
begin
  FreeAndNil( fpFormasPagamentos ) ;
  inherited CarregaFormasPagamento;

  FormaPagamento := TACBrECFFormaPagamento.create ;
  FormaPagamento.Indice    := '00' ;
  FormaPagamento.Descricao := 'TROCO' ;
  FormaPagamento.PermiteVinculado := False;
  fpFormasPagamentos.Add( FormaPagamento ) ;

  FormaPagamento := TACBrECFFormaPagamento.create ;
  FormaPagamento.Indice    := '01' ;
  FormaPagamento.Descricao := 'DINHEIRO' ;
  FormaPagamento.PermiteVinculado := False;
  fpFormasPagamentos.Add( FormaPagamento ) ;
end;

procedure TACBrECFVirtualClass.AtualizarAliquotasMemoria;
var
  CopiaAliquotas: TACBrECFAliquotas;
  Aliq: TACBrECFAliquota;
  I: Integer;
begin
  CopiaAliquotas := TACBrECFAliquotas.Create(True);
  try
    For I := 0 to fpAliquotas.Count-1 do
    begin
      Aliq := TACBrECFAliquota.create;
      Aliq.Assign(fpAliquotas[I]);
      CopiaAliquotas.Add(Aliq);
    end;

    CriarAliquotasPadrao;
    if CopiaAliquotas.Count > 0 then
      fpAliquotas[0].Total := CopiaAliquotas[0].Total;

    if CopiaAliquotas.Count > 1 then
      fpAliquotas[1].Total := CopiaAliquotas[1].Total;

    if CopiaAliquotas.Count > 2 then
      fpAliquotas[2].Total := CopiaAliquotas[2].Total;

    if CopiaAliquotas.Count > 3 then
    begin
      For I := 3 to CopiaAliquotas.Count-1 do
      begin
        Aliq := TACBrECFAliquota.create;
        Aliq.Assign(CopiaAliquotas[I]);
        Aliq.Indice := IntToStrZero(fpAliquotas.Count+1, 2);  // Reposiciona o Indice
        fpAliquotas.Add(Aliq);
      end;
    end;

    GravaArqINI;
  finally
    CopiaAliquotas.Free;
  end;

end;

procedure TACBrECFVirtualClass.AtualizarFormasPagamentoMemoria;
var
  CopiaFPG: TACBrECFFormasPagamento;
  FPG: TACBrECFFormaPagamento;
  I: Integer;
begin
  CopiaFPG := TACBrECFFormasPagamento.Create(True);
  try
    For I := 0 to fpFormasPagamentos.Count-1 do
    begin
      FPG := TACBrECFFormaPagamento.create;
      FPG.Assign(fpFormasPagamentos[I]);
      CopiaFPG.Add(FPG);
    end;

    CriarFormasPagamentoPadrao;

    if CopiaFPG.Count > 0 then  // Movendo total de "Dinheiro"
      fpFormasPagamentos[1].Total := CopiaFPG[0].Total;

    if CopiaFPG.Count > 1 then  // Tem outras FPGs cadastradas ?
    begin
      For I := 1 to CopiaFPG.Count-1 do
      begin
        FPG := TACBrECFFormaPagamento.create;
        FPG.Assign(CopiaFPG[I]);
        fpFormasPagamentos.Add(FPG);
      end;
    end;

    GravaArqINI;
  finally
    CopiaFPG.Free;
  end;
end;

procedure TACBrECFVirtualClass.GravaArqINI;
var
  Tratado: Boolean;
  SL: TStringList;
begin
  GravaLog('GravaArqINI');

  if fpNomeArqINI = '' then
    fpNomeArqINI := CalculaNomeArqINI;

  Tratado := false;
  SL      := TStringList.Create() ;
  try
    ClasstoINI( SL );

    if Assigned( fsQuandoGravarArqINI ) then
      fsQuandoGravarArqINI( SL, Tratado );

    if not Tratado then
      GravaArqINIVirtual( SL );
  finally
    SL.Free;
  end;
end;

procedure TACBrECFVirtualClass.GravaArqINIVirtual(ConteudoINI: TStrings);
begin
   ConteudoINI.SaveToFile( fpNomeArqINI );
end;

procedure TACBrECFVirtualClass.ClasstoINI(ConteudoINI: TStrings);
Var
  Ini : TMemIniFile ;
  A   : Integer ;
  S   : String ;
begin
  GravaLog('ClasstoINI');
  Ini := TMemIniFile.Create( '' ) ;
  try
    Ini.Clear;
    Ini.SetStrings( ConteudoINI );

    Ini.WriteInteger('Variaveis','Estado',Integer( fpEstado) ) ;
    Ini.WriteInteger('Variaveis','NumCupom',fpNumCOO) ;
    Ini.WriteInteger('Variaveis','NumGNF',fpNumGNF) ;
    Ini.WriteInteger('Variaveis','NumGRG',fpNumGRG) ;
    Ini.WriteInteger('Variaveis','NumCDC',fpNumCDC) ;
    Ini.WriteInteger('Variaveis','NumCER',fpNumCER) ;
    Ini.WriteFloat('Variaveis','GrandeTotal',fpGrandeTotal) ;
    Ini.WriteFloat('Variaveis','VendaBruta',fpVendaBruta) ;
    Ini.WriteInteger('Variaveis','NumCCF',fpNumCCF) ;
    Ini.WriteDate('Variaveis','DiaMovimento',fpDia) ;
    Ini.WriteBool('Variaveis','HorarioVerao',fpVerao) ;
    Ini.WriteInteger('Variaveis','ReducoesZ',fpReducoesZ) ;
    Ini.WriteInteger('Variaveis','LeiturasX',fpLeiturasX) ;
    Ini.WriteInteger('Variaveis','COOInicial',fpCOOInicial) ;
    Ini.WriteInteger('Variaveis','COOFinal',fpCOOFinal) ;
    Ini.WriteInteger('Variaveis','CuponsCancelados',fpCuponsCancelados) ;
    Ini.WriteFloat('Variaveis', 'CuponsCanceladosTotal', fpCuponsCanceladosTotalICMS);
    Ini.WriteFloat('Variaveis', 'CuponsCanceladosTotalISSQN', fpCuponsCanceladosTotalISSQN);
    Ini.WriteInteger('Variaveis','CNFCancelados',fpCNFCancelados) ;
    Ini.WriteFloat('Variaveis', 'CNFCanceladosTotal', fpCNFCanceladosTotal);
    Ini.WriteInteger('Variaveis','CuponsCanceladosEmAberto',fpCuponsCanceladosEmAberto) ;
    Ini.WriteFloat('Variaveis', 'CuponsCanceladosEmAbertoTotal', fpCuponsCanceladosEmAbertoTotalICMS);
    Ini.WriteFloat('Variaveis', 'CuponsCanceladosEmAbertoTotalISSQN', fpCuponsCanceladosEmAbertoTotalISSQN);
    Ini.WriteFloat('Variaveis', 'TotalDescontos',fpTotalDescontosICMS);
    Ini.WriteFloat('Variaveis', 'TotalAcrescimos',fpTotalAcrescimosICMS);
    Ini.WriteFloat('Variaveis', 'TotalDescontosISSQN',fpTotalDescontosISSQN);
    Ini.WriteFloat('Variaveis', 'TotalAcrescimosISSQN',fpTotalAcrescimosISSQN);
    Ini.WriteString('Variaveis','Operador',Operador) ;
    Ini.WriteString('Variaveis','PAF',fpPAF) ;

    fpCupom.SaveToINI( Ini );

    if Assigned(fpFormasPagamentos) then
    begin
      S := 'Formas_Pagamento';
      for A := 0 to fpFormasPagamentos.Count - 1 do
      begin
        with fpFormasPagamentos[A] do
          Ini.WriteString( S ,IntToStrZero( A, 2), AsString ) ;
      end ;
    end;

    if Assigned(fpRelatoriosGerenciais) then
    begin
      S := 'Relatorios_Gerenciais';
      for A := 0 to fpRelatoriosGerenciais.Count - 1 do
      begin
        with fpRelatoriosGerenciais[A] do
          Ini.WriteString( S ,IntToStrZero( A, 2), AsString ) ;
      end ;
    end;

    if Assigned(fpComprovantesNaoFiscais) then
    begin
      S := 'Comprovantes_nao_Fiscais';
      for A := 0 to fpComprovantesNaoFiscais.Count - 1 do
      begin
        with fpComprovantesNaoFiscais[A] do
          Ini.WriteString( S ,IntToStrZero( A, 2), AsString ) ;
      end ;
    end;

    if Assigned(fpAliquotas) then
    begin
      S := 'Aliquotas';
      for A := 0 to fpAliquotas.Count - 1 do
      begin
        with fpAliquotas[A]  do
          Ini.WriteString( S ,IntToStrZero( A, 2), AsString ) ;
      end ;
    end;

    ConteudoINI.Clear;
    Ini.GetStrings( ConteudoINI );
  finally
    Ini.Free ;
  end ;
end;

function TACBrECFVirtualClass.CalculaNomeArqINI : String ;
begin
  Result := ApplicationPath+'acbrecf'+GetNumECF+'.ini';
end;

procedure TACBrECFVirtualClass.SetNumECF(AValue: Integer);
begin
  if fpNumECF = AValue then Exit;
  fpNumECF := min( max( AValue, 1), 999);
end;

procedure TACBrECFVirtualClass.SetNumSerie(const AValue: String);
begin
  if fpNumSerie = AValue then Exit;
  fpNumSerie := AValue;
end;

procedure TACBrECFVirtualClass.VerificaFaixaItem(NumItem: Integer);
begin
  if fpCupom.Itens.Count = 0 then
    raise EACBrECFERRO.create(ACBrStr('Nenhum Item foi vendido ainda')) ;

  if (NumItem < 1) or (NumItem > fpCupom.Itens.Count) then
    raise EACBrECFERRO.create('Item (' + IntToStrZero(NumItem, 3) + ') '+'fora da Faixa.') ;
end;

procedure TACBrECFVirtualClass.SetNumCRO(AValue: Integer);
begin
  if fpNumCRO = AValue then Exit;
  fpNumCRO := min( max( AValue, 1), 999);
end;

procedure TACBrECFVirtualClass.SetColunas(AValue: Integer);
begin
  if fpColunas = AValue then Exit;
  fpColunas := AValue;
end;

function TACBrECFVirtualClass.GetChaveCupom: String;
begin
  Result := fpCupom.ChaveDFe;
end;

procedure TACBrECFVirtualClass.SetChaveCupom(const AValue: String);
begin
  fpCupom.ChaveDFe := AValue;
end;

function TACBrECFVirtualClass.GetColunas: Integer;
begin
  Result := fpColunas;
end;

function TACBrECFVirtualClass.GetDevice: TACBrDevice;
begin
  Result := fpDevice;
end;

procedure TACBrECFVirtualClass.SetDevice(AValue: TACBrDevice);
begin
  fpDevice := AValue;
end;

function TACBrECFVirtualClass.GetEstado: TACBrECFEstado;
Var estAnterior : TACBrECFEstado ;
begin
  if (not fpAtivo) then
  begin
    Result := estNaoInicializada ;
    Exit ;
  end;

  estAnterior := fpEstado ;

  if not (fpEstado in [estNaoInicializada,estDesconhecido]) then
  begin
    if (CompareDate(now, fpDia) > 0) then
    begin
       case fpEstado of
         estLivre:
           SetEstadoECFVirtual(estRequerZ);
         estBloqueada:
           SetEstadoECFVirtual(estRequerX);
       end;
    end;
  end ;

  if fpEstado in [estDesconhecido, estNaoInicializada] then
    SetEstadoECFVirtual(estLivre);

  GetEstadoECFVirtual;

  if fpEstado <> estAnterior then
    GravaArqINI ;

  Result := fpEstado ;
  GravaLog('GetEstado '+GetEnumName(TypeInfo(TACBrECFEstado), integer( fpEstado ) ));
end ;

function TACBrECFVirtualClass.GetArredonda: Boolean;
begin
  Result := fpArredondaItemMFD ;
  GravaLog('GetArredonda: '+BoolToStr(Result));
end;

function TACBrECFVirtualClass.GetHorarioVerao: Boolean;
begin
  Result := fpVerao ;
  GravaLog('GetHorarioVerao: '+BoolToStr(Result));
end;

procedure TACBrECFVirtualClass.CarregaFormasPagamento;
begin
  GravaLog('CarregaFormasPagamento');
  LeArqINI;
end;

procedure TACBrECFVirtualClass.LerTotaisFormaPagamento;
begin
  GravaLog('LerTotaisFormaPagamento');
  CarregaFormasPagamento ;
end;

procedure TACBrECFVirtualClass.ProgramaFormaPagamento( var Descricao: String;
   PermiteVinculado : Boolean; Posicao : String ) ;
Var
  FPagto : TACBrECFFormaPagamento ;
  A : Integer ;
begin
  GravaLog( ComandoLOG );
  Descricao := LeftStr(Trim(Descricao),20) ;         { Ajustando tamanho final }

  if not Assigned(fpFormasPagamentos) then
    CarregaFormasPagamento;

  { Verificando se a Descriçao já foi programada antes (ja existe ?) }
  For A := 0 to fpFormasPagamentos.Count -1 do
    if trim(UpperCase( fpFormasPagamentos[A].Descricao )) = UpperCase(Descricao) then
      exit ;

  try
    FPagto := TACBrECFFormaPagamento.create ;
    FPagto.Indice           := IntToStrZero( fpFormasPagamentos.Count, 2 );
    FPagto.Descricao        := Descricao ;
    FPagto.PermiteVinculado := PermiteVinculado ;
    fpFormasPagamentos.Add( FPagto ) ;

    GravaArqINI ;
  except
    LeArqINI ;
    raise;
  end ;
end;

procedure TACBrECFVirtualClass.CarregaRelatoriosGerenciais;
begin
  GravaLog('CarregaRelatoriosGerenciais');
  LeArqINI;
end;

procedure TACBrECFVirtualClass.LerTotaisRelatoriosGerenciais;
begin
  GravaLog('LerTotaisRelatoriosGerenciais');
  CarregaRelatoriosGerenciais;
end;

procedure TACBrECFVirtualClass.ProgramaRelatorioGerencial(
  var Descricao: String; Posicao: String);
Var
  RelGer : TACBrECFRelatorioGerencial ;
  A : Integer ;
begin
  GravaLog( ComandoLOG );

  Descricao := LeftStr(Trim(Descricao),20) ;         { Ajustando tamanho final }

  if not Assigned(fpRelatoriosGerenciais) then
    CarregaRelatoriosGerenciais;

  { Verificando se a Descriçao já foi programada antes (ja existe ?) }
  For A := 0 to fpRelatoriosGerenciais.Count -1 do
    if trim(UpperCase( fpRelatoriosGerenciais[A].Descricao )) = UpperCase(Descricao) then
      exit ;

  try
    RelGer := TACBrECFRelatorioGerencial.create ;
    RelGer.Indice           := IntToStrZero( fpRelatoriosGerenciais.Count+1, 2 );
    RelGer.Descricao        := Descricao ;
    fpRelatoriosGerenciais.Add( RelGer ) ;

    GravaArqINI ;
  except
    LeArqINI ;
    raise;
  end ;
end;


procedure TACBrECFVirtualClass.CarregaAliquotas;
begin
  GravaLog('CarregaAliquotas');
  LeArqINI;
end;

procedure TACBrECFVirtualClass.LerTotaisAliquota;
begin
  GravaLog('LerTotaisAliquota');
  CarregaAliquotas ;
end;

function TACBrECFVirtualClass.AchaICMSAliquota(var AliquotaICMS: String):
   TACBrECFAliquota;
begin
  GravaLog( ComandoLOG );

  Result := inherited AchaICMSAliquota( AliquotaICMS );
end;

procedure TACBrECFVirtualClass.ProgramaAliquota( Aliquota : Double; Tipo : Char;
   Posicao : String) ;
Var
  Aliq : TACBrECFAliquota ;
  A    : Integer ;
begin
  GravaLog( ComandoLOG );

  Tipo := UpCase(Tipo) ;

  if not Assigned( fpAliquotas ) then
    CarregaAliquotas;

  { Verificando se a Aliquota já foi programada antes (ja existe ?) }
  For A := 0 to fpAliquotas.Count -1 do
    if (fpAliquotas[A].Aliquota = Aliquota) and
       (fpAliquotas[A].Tipo     = Tipo) then
      exit ;

  try
    Aliq := TACBrECFAliquota.create ;
    Aliq.Indice   := IntToStrZero( fpAliquotas.Count+1,2) ;
    Aliq.Aliquota := Aliquota ;
    Aliq.Tipo     := Tipo ;
    fpAliquotas.Add( Aliq ) ;

    GravaArqINI ;
  except
    LeArqINI ;
    raise;
  end ;
end;

procedure TACBrECFVirtualClass.CarregaTotalizadoresNaoTributados;
begin
  inherited;
end;

procedure TACBrECFVirtualClass.CarregaComprovantesNaoFiscais;
begin
  GravaLog( 'CarregaComprovantesNaoFiscais' );
  LeArqINI;
end;

procedure TACBrECFVirtualClass.LerTotaisComprovanteNaoFiscal ;
begin
  GravaLog( 'LerTotaisComprovanteNaoFiscal' );
  CarregaComprovantesNaoFiscais ;
end;

procedure TACBrECFVirtualClass.ProgramaComprovanteNaoFiscal(var Descricao : String ;
  Tipo : String ; Posicao : String) ;
Var
  CNF : TACBrECFComprovanteNaoFiscal ;
  A : Integer ;
begin
  GravaLog( ComandoLOG );

  if not Assigned( fpComprovantesNaoFiscais ) then
    CarregaComprovantesNaoFiscais;

  Descricao := LeftStr(Trim(Descricao),20) ;         { Ajustando tamanho final }
  Tipo      := UpperCase( Tipo ) ;
  if Tipo = '' then
    Tipo := 'V' ;

  { Verificando se a Descriçao já foi programada antes (ja existe ?) }
  For A := 0 to fpComprovantesNaoFiscais.Count -1 do
    if trim( UpperCase( fpComprovantesNaoFiscais[A].Descricao ) ) = UpperCase( Descricao ) then
      exit ;

  try
    CNF := TACBrECFComprovanteNaoFiscal.create ;
    CNF.Indice    := IntToStrZero( fpComprovantesNaoFiscais.Count+1, 2 )  ;
    CNF.Descricao := Descricao ;
    CNF.PermiteVinculado := (Tipo =  'V') ;
    fpComprovantesNaoFiscais.Add( CNF ) ;

    GravaArqINI ;
  except
    LeArqINI ;
    raise;
  end ;
end;

procedure TACBrECFVirtualClass.IdentificaOperador(Nome: String);
begin
  GravaLog( ComandoLOG );
  Operador := Nome;
end;

procedure TACBrECFVirtualClass.IdentificaPAF(NomeVersao, MD5: String);
begin
  GravaLog( ComandoLOG );

  fpPAF := '';
  if NomeVersao <> '' then
    fpPAF := NomeVersao ;

  if (MD5 <> '') then
  begin
    if (fpPAF <> '') then
      fpPAF := fpPAF + '|' ;

    fpPAF := fpPAF + MD5 ;
  end;
end;

function TACBrECFVirtualClass.TraduzirTag(const ATag: AnsiString): AnsiString;
begin
  // Não Traduz... pois tradução será feita por TACBrPosPrinter
  Result := ATag;
end;

function TACBrECFVirtualClass.TraduzirTagBloco(const ATag, Conteudo: AnsiString
  ): AnsiString;
begin
  // Não Traduz... pois tradução será feita por TACBrPosPrinter
  Result := ATag + Conteudo +
            TraduzirTag( '</'+copy(ATag,2,Length(ATag)) );
end;

function TACBrECFVirtualClass.RoundECF(AValue: Double): Double;
begin
  if fpArredondaItemMFD then
    Result := RoundABNT(AValue, -2)
  else
    Result := TruncTo( AValue, -2);
end;

end.

