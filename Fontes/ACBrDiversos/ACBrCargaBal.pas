{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Anderson Rogerio Bejatto                        }
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

unit ACBrCargaBal;

{$I ACBr.inc}

interface

uses
  SysUtils, Classes,
  {$IF DEFINED(HAS_SYSTEM_GENERICS)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$Else}
   Contnrs,
  {$IfEnd}
  ACBrBase;

type
  EACBrCargaBal = class(Exception);
  TACBrCargaBalTipoVenda = (tpvPeso, tpvUnidade, tpvEAN13, tpvEAN13Und);
  TACBrCargaBalModelo = (modFilizola, modToledo, modUrano, modUranoS, modToledoMGV5, modToledoMGV6, modToledoMGV7, modUranoURF32, modRamuza, modToledoMGV5Ver1);
  TACBrCargaBalProgresso = procedure(Mensagem: String; ProgressoAtual, ProgressoTotal: Integer) of object;
  TACBrCargaBalTipoValidade = (tpvDias, tpvMeses);

  // nutricional
  TACBrCargaBalNutriUndPorcao = (tpGramas, tpMililitros, tpUnidades);
  TACBRCargaBalNutriUndPorcao429 = (tpGramas429, tpMililitros429);
  TACBrCargaBalNutriPartdecimal = (tpPara0, tpPara14, tpPara13, tpPara12, tpPara23, tpPara34);
  TACBrCargaBalNutriPartdecimal429 = (tpPara0_429, tpPara14_429, tpPara13_429, tpPara12_429, tpPara23_429, tpPara34_429);
  TACBrCargaBalNutriMedCaseira = (tpColherSopa, tpColherCafe, tpColherCha, tpXicara, tpDeXicara, tpUnidade, tpPacote, tpFatia,
                                  tpFatiaFina, tpPedaco, tpFolha, tpPao, tpBiscoito, tpBisnaguinha, tpDisco, tpCopo, tpPorcao,
                                  tpTablete, tpSache, tpAlmodega, tpBife, tpFile, tpConcha, tpBala, tpPratoFundo, tpPitada, tpLata);
  TACBrCargaBalNutriMedCaseira429 = (tpColherSopa429, tpColherCafe429, tpColherCha429, tpXicara429, tpDeXicara429, tpUnidade429, tpPacote429, tpFatia429,
                                     tpFatiaFina429, tpPedaco429, tpFolha429, tpPao429, tpBiscoito429, tpBisnaguinha429, tpDisco429, tpCopo429, tpPorcao429,
                                     tpTablete429, tpSache429, tpAlmodega429, tpBife429, tpFile429, tpConcha429, tpBala429, tpPratoFundo429, tpPitada429, tpLata429,
                                     tpXicaraCha429, tpPratoRaso429);
  // Criado por causa da balanca toledo
  TAcbrCargaBalTeclado = class
  private
    fTecla: integer;
    fPaginaTeclado: integer;
    fCodTeclado: integer;
  public
    constructor create;
    property Codigo_Teclado : integer read fCodTeclado write fCodTeclado;
    property Pagina_Teclado : integer read fPaginaTeclado write fPaginaTeclado;
    property Tecla : integer read fTecla write fTecla;
  end;


  TACBrCargaBalSetor = class
  private
    FCodigo: Integer;
    FDescricao: String;
  public
    constructor Create;
    property Codigo: Integer read FCodigo write FCodigo;
    property Descricao: String read FDescricao write FDescricao;
  end;

  TACBrCargaBalNutricional = class
  private
    fCodigo: Integer;
    FDescricao : String;
    FQtd: Integer;
    FUndPorcao: TACBrCargaBalNutriUndPorcao;
    FPartInteira: Integer;
    FPartDecimal: TACBrCargaBalNutriPartdecimal;
    FMedCaseira: TACBrCargaBalNutriMedCaseira;
    FValorEnergetico: Integer;
    FCarboidrato: Currency;
    FProteina: Currency;
    FGorduraTotal: Currency;
    FGorduraSaturada: Currency;
    FGorduraTrans: Currency;
    FFibra: Currency;
    FSodio: Currency;
    FQtdeAutomaticaPorcao: Boolean;
    FQtdePorcEmb: Integer;
    FPartIntMedidaCaseira: Integer;
    FQtdePorcao: Integer;
    FMedCaseira429: TACBrCargaBalNutriMedCaseira429;
    FFibraAlimentar429: Double;
    FAcucaresAdicionados429: Double;
    FLactose429: Double;
    FAltoSodio429: Integer;
    FValorEnergetico429: Integer;
    FAcucaresTotais429: Double;
    FAltoGordura429: Integer;
    FGordurasTrans429: Double;
    FCarboidrato429: Currency;
    FProteinas429: Double;
    FAltoAcucar429: Integer;
    FGordurasTotais429: Double;
    FSodio429: Double;
    FGalactose429: Double;
    FGordurasSaturadas429: Double;
    FUndPorcao429: TACBRCargaBalNutriUndPorcao429;
    FPartDecMedidaCaseira429: TACBrCargaBalNutriPartdecimal429;
    FImprimeLactoseGalactose: Integer;
    FProteinasEstendido429: Double;
    FGordurasTotaisEstendido429: Double;
    FAcucaresAdicionadosEstendido429: Double;
    FAcucaresTotaisEstendido429: Double;
    procedure SetQtdeAutomaticaPorcao(const Value: Boolean);
    procedure SetQtdePorcEmb(const Value: Integer);
    procedure SetPartIntMedidaCaseira(const Value: Integer);
    procedure SetQtdePorcao(const Value: Integer);
    procedure SetMedCaseira429(const Value: TACBrCargaBalNutriMedCaseira429);
    procedure SetAcucaresAdicionados429(const Value: Double);
    procedure SetAcucaresTotais429(const Value: Double);
    procedure SetAltoAcucar429(const Value: Integer);
    procedure SetAltoGordura429(const Value: Integer);
    procedure SetAltoSodio429(const Value: Integer);
    procedure SetCarboidrato429(const Value: Currency);
    procedure SetFibraAlimentar429(const Value: Double);
    procedure SetGalactose429(const Value: Double);
    procedure SetGordurasSaturadas429(const Value: Double);
    procedure SetGordurasTotais429(const Value: Double);
    procedure SetGordurasTrans429(const Value: Double);
    procedure SetLactose429(const Value: Double);
    procedure SetProteinas429(const Value: Double);
    procedure SetSodio429(const Value: Double);
    procedure SetValorEnergetico429(const Value: Integer);
    procedure SetUndPorcao429(const Value: TACBRCargaBalNutriUndPorcao429);
    procedure SetPartDecMedidaCaseira429(const Value: TACBrCargaBalNutriPartdecimal429);
    procedure SetImprimeLactoseGalactose(const Value: Integer);
    procedure SetAcucaresAdicionadosEstendido429(const Value: Double);
    procedure SetAcucaresTotaisEstendido429(const Value: Double);
    procedure SetGordurasTotaisEstendido429(const Value: Double);
    procedure SetProteinasEstendido429(const Value: Double);
  public
    constructor Create;
    procedure Limpar;
    property Codigo: Integer read FCodigo write FCodigo;
    property Descricao: String read FDescricao write FDescricao;
    property Qtd: Integer read FQtd write FQtd;
    property UndPorcao: TACBrCargaBalNutriUndPorcao read FUndPorcao write FUndPorcao;
    property PartInteira: Integer read FPartInteira write FPartInteira;
    property PartDecimal: TACBrCargaBalNutriPartdecimal read FPartDecimal write FPartDecimal;
    property MedCaseira: TACBrCargaBalNutriMedCaseira read FMedCaseira write FMedCaseira;
    property ValorEnergetico: Integer read FValorEnergetico write FValorEnergetico;
    property Carboidrato: Currency read FCarboidrato write FCarboidrato;
    property Proteina: Currency read FProteina write FProteina;
    property GorduraTotal: Currency read FGorduraTotal write FGorduraTotal;
    property GorduraSaturada: Currency read FGorduraSaturada write FGorduraSaturada;
    property GorduraTrans: Currency read FGorduraTrans write FGorduraTrans;
    property Fibra: Currency read FFibra write FFibra;
    property Sodio: Currency read FSodio write FSodio;
    property QtdeAutomaticaPorcao429:Boolean read FQtdeAutomaticaPorcao write SetQtdeAutomaticaPorcao;
    property QtdePorcEmb429:Integer read FQtdePorcEmb write SetQtdePorcEmb;
    property QtdePorcao429:Integer read FQtdePorcao write SetQtdePorcao;
    property UndPorcao429:TACBRCargaBalNutriUndPorcao429 read FUndPorcao429 write SetUndPorcao429;
    property PartIntMedidaCaseira429:Integer read FPartIntMedidaCaseira write SetPartIntMedidaCaseira;
    property PartDecMedidaCaseira429:TACBrCargaBalNutriPartdecimal429 read FPartDecMedidaCaseira429 write SetPartDecMedidaCaseira429;
    property MedCaseira429:TACBrCargaBalNutriMedCaseira429 read FMedCaseira429 write SetMedCaseira429;
    property ValorEnergetico429:Integer read FValorEnergetico429 write SetValorEnergetico429;
    property Carboidrato429:Currency read FCarboidrato429 write SetCarboidrato429;
    property AcucaresTotais429:Double read FAcucaresTotais429 write SetAcucaresTotais429;
    property AcucaresAdicionados429:Double read FAcucaresAdicionados429 write SetAcucaresAdicionados429;
    property Proteinas429:Double read FProteinas429 write SetProteinas429;
    property GordurasTotais429:Double read FGordurasTotais429 write SetGordurasTotais429;
    property GordurasSaturadas429:Double read FGordurasSaturadas429 write SetGordurasSaturadas429;
    property GordurasTrans429:Double read FGordurasTrans429 write SetGordurasTrans429;
    property FibraAlimentar429:Double read FFibraAlimentar429 write SetFibraAlimentar429;
    property Sodio429:Double read FSodio429 write SetSodio429;
    property AltoAcucar429:Integer read FAltoAcucar429 write SetAltoAcucar429;
    property AltoGordura429:Integer read FAltoGordura429 write SetAltoGordura429;
    property AltoSodio429:Integer read FAltoSodio429 write SetAltoSodio429;
    property Lactose429:Double read FLactose429 write SetLactose429;
    property Galactose429:Double read FGalactose429 write SetGalactose429;
    property ImprimeLactoseGalactose:Integer read FImprimeLactoseGalactose write SetImprimeLactoseGalactose;
    property AcucaresAdicionadosEstendido429:Double read FAcucaresAdicionadosEstendido429 write SetAcucaresAdicionadosEstendido429;
    property AcucaresTotaisEstendido429:Double read FAcucaresTotaisEstendido429 write SetAcucaresTotaisEstendido429;
    property GordurasTotaisEstendido429:Double read FGordurasTotaisEstendido429 write SetGordurasTotaisEstendido429;
    property ProteinasEstendido429:Double read FProteinasEstendido429 write SetProteinasEstendido429;
  end;

  TACBrCargaBalInformacaoExtra = class
  private
    fCodigo: Integer;
    FObservacao: String;
    FReceita: String;
  public
    constructor Create;
    property Codigo: Integer read FCodigo write FCodigo;
    property Observacao: String read FObservacao write FObservacao;
    property Receita: String read FReceita write FReceita;
  end;


  TACBrCargaBalTaras = class
  private
    fCodigo: Integer;
    FDescricao : String;
    FValor : Currency;

  public
    constructor Create;
    property Codigo: Integer read FCodigo write FCodigo;
    property Descricao: String read FDescricao write FDescricao;
    property Valor: Currency read FValor write FValor;
  end;

  TACBrCargaBalFornecedor = class
  private
    FCodigo: Smallint;
    FObservacao : String; // 100 Bytes
    FDescricao1 : String; // 56 Bytes
    FDescricao2 : String; // 56 Bytes
    FDescricao3 : String; // 56 Bytes
    FDescricao4 : String; // 56 Bytes
    FDescricao5 : String; // 56 Bytes
  public
    constructor Create;
    property Codigo: Smallint read FCodigo write FCodigo;
    property Observacao: String read FObservacao write FObservacao;
    property Descricao1: String read FDescricao1 write FDescricao1;
    property Descricao2: String read FDescricao2 write FDescricao2;
    property Descricao3: String read FDescricao3 write FDescricao3;
    property Descricao4: String read FDescricao4 write FDescricao4;
    property Descricao5: String read FDescricao5 write FDescricao5;
  end;

  TACBrCargaBalFracionador = class
  private
    FCodigo: Smallint;
    FObservacao : String; // 100 Bytes
    FDescricao1 : String; // 56 Bytes
    FDescricao2 : String; // 56 Bytes
    FDescricao3 : String; // 56 Bytes
  public
    constructor Create;
    property Codigo: Smallint read FCodigo write FCodigo;
    property Observacao: String read FObservacao write FObservacao;
    property Descricao1: String read FDescricao1 write FDescricao1;
    property Descricao2: String read FDescricao2 write FDescricao2;
    property Descricao3: String read FDescricao3 write FDescricao3;
  end;

  TACBrCargaBalConservacao = class
  private
    FCodigo: Smallint;
    FObservacao : String; // 100 Bytes
    FDescricao1 : String; // 56 Bytes
    FDescricao2 : String; // 56 Bytes
    FDescricao3 : String; // 56 Bytes
  public
    constructor Create;
    property Codigo: Smallint read FCodigo write FCodigo;
    property Observacao: String read FObservacao write FObservacao;
    property Descricao1: String read FDescricao1 write FDescricao1;
    property Descricao2: String read FDescricao2 write FDescricao2;
    property Descricao3: String read FDescricao3 write FDescricao3;
  end;

  TACBrCargaBalExtra1 = class
  private
    FCodigo: Smallint;
    FObservacao : String;
    FLinha2: String;
    FLinha3: String;
    FLinha1: String;
    FLinha4: String;
    FLinha5: String;
    procedure SetLinha1(const Value: String);
    procedure SetLinha2(const Value: String);
    procedure SetLinha3(const Value: String);
    procedure SetLinha4(const Value: String);
    procedure SetLinha5(const Value: String); // 100 Bytes
  public
    constructor Create;
    property Codigo:Smallint read FCodigo write FCodigo;
    property Observacao: String read FObservacao write FObservacao;
    property Linha1:String read FLinha1 write SetLinha1;
    property Linha2:String read FLinha2 write SetLinha2;
    property Linha3:String read FLinha3 write SetLinha3;
    property Linha4:String read FLinha4 write SetLinha4;
    property Linha5:String read FLinha5 write SetLinha5;
  end;

  TACBrCargaBalExtra2 = class
  private
    FObservacao: String;
    FCodigo: Smallint;
    FLinha2: String;
    FLinha3: String;
    FLinha1: String;
    FLinha4: String;
    FLinha5: String;
    procedure SetCodigo(const Value: Smallint);
    procedure SetLinha1(const Value: String);
    procedure SetLinha2(const Value: String);
    procedure SetLinha3(const Value: String);
    procedure SetLinha4(const Value: String);
    procedure SetLinha5(const Value: String);
    procedure SetObservacao(const Value: String);
  public
    constructor Create;
    property Codigo:Smallint read FCodigo write SetCodigo;
    property Observacao:String read FObservacao write SetObservacao;
    property Linha1:String read FLinha1 write SetLinha1;
    property Linha2:String read FLinha2 write SetLinha2;
    property Linha3:String read FLinha3 write SetLinha3;
    property Linha4:String read FLinha4 write SetLinha4;
    property Linha5:String read FLinha5 write SetLinha5;
  end;

  TACBrCargaBalItem = class
  private
    FValorVenda: Currency;
    FModeloEtiqueta: Smallint;
    FDescricao: String;
    FCodigo: Integer;
    FTipo: TACBrCargaBalTipoVenda;
    FValidade: Smallint;
    FLote: String;
    FTipoValidade: TACBrCargaBalTipoValidade;
    FSetor: TACBrCargaBalSetor;
    FNutricional: TACBrCargaBalNutricional;
    FTara: TACBrCargaBalTaras;
    FFornecedor: TACBrCargaBalFornecedor;
    FFracionador: TACBrCargaBalFracionador;
    FConservacao: TACBrCargaBalConservacao;
    FExtra1:TACBrCargaBalExtra1;
    FExtra2:TACBrCargaBalExtra2;
    FCodigoTexto1: Integer;
    FCodigoTexto2: Integer;
    FCodigoTexto3: Integer;
    FCodigoInfoNutr: Integer;
    FCodigoTara: Integer;
    FCodigoFornecedor: Smallint;
    FCodigoFracionador: Smallint;
    FCodigoConservacao: Smallint;
    FImpValidade: Smallint;
    FImpEmbalagem: Smallint;
    FEAN13Fornecedor: string;
    FInformacaoExtra: TACBrCargaBalInformacaoExtra;
    FTeclado: TAcbrCargaBalTeclado;
    FCodigoExtra1: SmallInt;
    FCodigoExtra2: SmallInt;
    function ObterCodigoInfoExtra(AModelo : TACBrCargaBalModelo) : Integer;
    procedure SetCodigoExtra1(const Value: SmallInt);
    procedure SetExtra2(const Value: TACBrCargaBalExtra2);
    procedure SetCodigoExtra2(const Value: SmallInt);
  Public
    constructor Create;
    destructor Destroy; override;
    property Setor: TACBrCargaBalSetor read FSetor write FSetor;
    property ModeloEtiqueta: Smallint read FModeloEtiqueta write FModeloEtiqueta;
    property Tipo: TACBrCargaBalTipoVenda read FTipo write FTipo;
    property TipoValidade: TACBrCargaBalTipoValidade read FTipoValidade write FTipoValidade;
    property Codigo: Integer read FCodigo write FCodigo;
    property ValorVenda: Currency read FValorVenda write FValorVenda;
    property Validade: Smallint read FValidade write FValidade;
    property Lote: String read FLote write FLote;
    property Descricao: String read FDescricao write FDescricao;
    property InformacaoExtra: TACBrCargaBalInformacaoExtra read FInformacaoExtra write FInformacaoExtra;
    property Teclado: TAcbrCargaBalTeclado read FTeclado write FTeclado;
    property Nutricional: TACBrCargaBalNutricional Read FNutricional Write FNutricional;
    property Tara: TACBrCargaBalTaras Read FTara Write FTara;
    property Fornecedor: TACBrCargaBalFornecedor Read FFornecedor Write FFornecedor;
    property Fracionador: TACBrCargaBalFracionador Read FFracionador Write FFracionador;
    property Extra1:TACBrCargaBalExtra1 Read FExtra1 write FExtra1;
    property Extra2:TACBrCargaBalExtra2 read FExtra2 write SetExtra2;
    property Conservacao: TACBrCargaBalConservacao Read FConservacao Write FConservacao;
    property CodigoTexto1: Integer read FCodigoTexto1 write FCodigoTexto1;
    property CodigoTexto2: Integer read FCodigoTexto2 write FCodigoTexto2;
    property CodigoTexto3: Integer read FCodigoTexto3 write FCodigoTexto3;
    property CodigoInfoNutr: Integer read FCodigoInfoNutr write FCodigoInfoNutr;
    property CodigoTara: Integer Read FCodigoTara Write FCodigoTara Default 0;
    property CodigoFornecedor: Smallint Read FCodigoFornecedor Write FCodigoFornecedor Default 0;
    property ImpValidade: Smallint Read FImpvalidade Write FImpvalidade Default 1;
    property ImpEmbalagem: Smallint Read FImpEmbalagem Write FImpEmbalagem Default 1;
    property CodigoFracionador: Smallint Read FCodigoFracionador Write FCodigoFracionador Default 0;
    property CodigoConservacao: Smallint Read FCodigoConservacao Write FCodigoConservacao Default 0;
    property CodigoExtra1:SmallInt read FCodigoExtra1 write SetCodigoExtra1;
    property CodigoExtra2:SmallInt read FCodigoExtra2 write SetCodigoExtra2;
    property EAN13Fornecedor: string read FEAN13Fornecedor write FEAN13Fornecedor;
  end;

  TACBrCargaBalItens = class(TObjectList{$IfDef HAS_SYSTEM_GENERICS}<TACBrCargaBalItem>{$EndIf})
  private
    function GetItem(Index: Integer): TACBrCargaBalItem;
    procedure SetItem(Index: Integer; const Value: TACBrCargaBalItem);
  public
    constructor Create;
    destructor Destroy; Override;
    function New: TACBrCargaBalItem;
    property Items[Index: Integer]: TACBrCargaBalItem read GetItem write SetItem; Default;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrCargaBal = class( TACBrComponent )
  private
    FArquivosGerados: TStringList;
    FOnProgresso: TACBrCargaBalProgresso;
    FProdutos: TACBrCargaBalItens;
    FModelo: TACBrCargaBalModelo;
    procedure Progresso(const AMensagem: String; const AContAtual, AContTotal: Integer);

  protected
    function RFill(const Str: string; Tamanho: Integer = 0; Caracter: Char = ' '): string; overload;
    function LFIll(const Str: string; Tamanho: Integer = 0; Caracter: Char = '0'): string; overload;
    function LFIll(Valor: Currency; Tamanho: Integer; Decimais: Integer = 2; Caracter: Char = '0'): string; overload;
    function LFIll(Valor: Integer; Tamanho: Integer; Caracter: Char = '0'): string; overload;
    function LFIll(Valor: Boolean; Caracter:Char='0'): string; overload;

    function GetNomeArquivoNutricional: String;
    function GetNomeArquivoProduto: String;
    function GetNomeArquivoReceita: String;
    function GetNomeArquivoSetor: String;
    function GetNomeArquivoTaras: String;
    function GetNomeArquivoFornecedor: String;
    function GetNomeArquivoFracionador: String;
    function GetNomeArquivoConservacao: String;
    function GetNomeArquivoTeclado: String;
    function GetNomeArquivoExtra1:String;
    function GetNomeArquivoExtra2:String;

    function GetNomeArquivoRelacaoProdutoNutricional: String;
    function GetNomeArquivoRelacaoProdutoReceita: String;

    function GetTipoProdutoFilizola(Tipo: TACBrCargaBalTipoVenda): String;
    function GetTipoProdutoToledo(Tipo: TACBrCargaBalTipoVenda): String;
    function GetTipoProdutoUrano(Tipo: TACBrCargaBalTipoVenda): string;
    function GetTipoProdutoUranoURF32(Tipo: TACBrCargaBalTipoVenda): string;
    function GetTipoProdutoRamuza(Tipo: TACBrCargaBalTipoVenda): string;
    function CalcularSoma(const xStr: string): integer;
    function GetModeloStr: string;

    function GetTipoValidadeProdutoUranoURF32(Tipo: TACBrCargaBalTipoValidade): string;

    procedure PreencherFilizola(stlArquivo, stlSetor, stlNutricional, stlReceita: TStringList);
    procedure PreencherToledo(stlArquivo, stlNutricional, stlReceita, stlTara, stlFornecedor, stlFracionador, stlConservacao, stlSetor, stlTeclado, stlExtra1, stlExtra2: TStringList; Versao: Integer);
    procedure PreencherToledoMGV7(stlArquivo, stlNutricional, stlReceita, stlTara, stlFornecedor, stlFracionador, stlConservacao, stlSetor, stlTeclado, stlExtra1, stlExtra2: TStringList; Versao: Integer);
    procedure PreencherUrano(Arquivo: TStringList);
    procedure PreencherUranoS(Arquivo: TStringList);
    procedure PreencherUranoURF32(stlArquivo, stlNutricional, stlReceita, stlRelacaoProdutoNutricional, stlRelacaoProdutoReceita: TStringList);
    procedure PreencherRamuza(Arquivo: TStringList);

    function GetNutriUndPorcaoToledo(Tipo: TACBrCargaBalNutriUndPorcao): String;
    function GetNutriUnidPorcaoToledo429(Tipo:TACBRCargaBalNutriUndPorcao429):String;
    function GetNutriPartDecimalToledo(Tipo: TACBrCargaBalNutriPartdecimal): String;
    function GetNutriPartDecimalToledo429(Tipo:TACBrCargaBalNutriPartdecimal429):String;
    function GetNutriMedCaseiraToledo(Tipo: TACBrCargaBalNutriMedCaseira): String;
    function GetNutriMedCaseiraToledo429(Tipo: TACBrCargaBalNutriMedCaseira429): String;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GerarArquivos(const ADiretorio: String);
    property ArquivosGerados: TStringList read FArquivosGerados;
  published
    property Modelo: TACBrCargaBalModelo read FModelo write FModelo;
    property ModeloStr: String read GetModeloStr;
    property Produtos: TACBrCargaBalItens read FProdutos write FProdutos;
    property OnProgresso: TACBrCargaBalProgresso read FOnProgresso write FOnProgresso;
  end;

implementation

uses
  ACBrConsts, ACBrUtil.Strings;

{ TACBrCargaBalSetor }
constructor TACBrCargaBalSetor.Create;
begin
  FCodigo    := 0;
  FDescricao := EmptyStr;
end;

{ TACBrCargaBalNutricional }
constructor TACBrCargaBalNutricional.Create;
begin
  inherited;
  Limpar;
end;

procedure TACBrCargaBalNutricional.Limpar;
begin
  fCodigo          := 0;
  FQtd             := 0;
  FUndPorcao       := tpGramas;
  FPartInteira     := 0;
  FPartDecimal     := tpPara0;
  FMedCaseira      := tpColherSopa;
  FValorEnergetico := 0;
  FCarboidrato     := 0;
  FProteina        := 0;
  FGorduraTotal    := 0;
  FGorduraSaturada := 0;
  FGorduraTrans    := 0;
  FFibra           := 0;
  FSodio           := 0;
end;


procedure TACBrCargaBalNutricional.SetAcucaresAdicionados429(
  const Value: Double);
begin
  FAcucaresAdicionados429 := Value;
end;

procedure TACBrCargaBalNutricional.SetAcucaresAdicionadosEstendido429(
  const Value: Double);
begin
  FAcucaresAdicionadosEstendido429 := Value;
end;

procedure TACBrCargaBalNutricional.SetAcucaresTotais429(
  const Value: Double);
begin
  FAcucaresTotais429 := Value;
end;

procedure TACBrCargaBalNutricional.SetAcucaresTotaisEstendido429(
  const Value: Double);
begin
  FAcucaresTotaisEstendido429 := Value;
end;

procedure TACBrCargaBalNutricional.SetAltoAcucar429(const Value: Integer);
begin
  FAltoAcucar429 := Value;
end;

procedure TACBrCargaBalNutricional.SetAltoGordura429(const Value: Integer);
begin
  FAltoGordura429 := Value;
end;

procedure TACBrCargaBalNutricional.SetAltoSodio429(const Value: Integer);
begin
  FAltoSodio429 := Value;
end;

procedure TACBrCargaBalNutricional.SetCarboidrato429(
  const Value: Currency);
begin
  FCarboidrato429 := Value;
end;

procedure TACBrCargaBalNutricional.SetFibraAlimentar429(
  const Value: Double);
begin
  FFibraAlimentar429 := Value;
end;

procedure TACBrCargaBalNutricional.SetGalactose429(const Value: Double);
begin
  FGalactose429 := Value;
end;

procedure TACBrCargaBalNutricional.SetGordurasSaturadas429(
  const Value: Double);
begin
  FGordurasSaturadas429 := Value;
end;

procedure TACBrCargaBalNutricional.SetGordurasTotais429(
  const Value: Double);
begin
  FGordurasTotais429 := Value;
end;

procedure TACBrCargaBalNutricional.SetGordurasTotaisEstendido429(
  const Value: Double);
begin
  FGordurasTotaisEstendido429 := Value;
end;

procedure TACBrCargaBalNutricional.SetGordurasTrans429(
  const Value: Double);
begin
  FGordurasTrans429 := Value;
end;

procedure TACBrCargaBalNutricional.SetImprimeLactoseGalactose(
  const Value: Integer);
begin
  FImprimeLactoseGalactose := Value;
end;

procedure TACBrCargaBalNutricional.SetLactose429(const Value: Double);
begin
  FLactose429 := Value;
end;

procedure TACBrCargaBalNutricional.SetMedCaseira429(
  const Value: TACBrCargaBalNutriMedCaseira429);
begin
  FMedCaseira429 := Value;
end;

procedure TACBrCargaBalNutricional.SetPartDecMedidaCaseira429(
  const Value: TACBrCargaBalNutriPartdecimal429);
begin
  FPartDecMedidaCaseira429 := Value;
end;

procedure TACBrCargaBalNutricional.SetPartIntMedidaCaseira(
  const Value: Integer);
begin
  FPartIntMedidaCaseira := Value;
end;

procedure TACBrCargaBalNutricional.SetProteinas429(const Value: Double);
begin
  FProteinas429 := Value;
end;

procedure TACBrCargaBalNutricional.SetProteinasEstendido429(
  const Value: Double);
begin
  FProteinasEstendido429 := Value;
end;

procedure TACBrCargaBalNutricional.SetQtdeAutomaticaPorcao(
  const Value: Boolean);
begin
  FQtdeAutomaticaPorcao := Value;
end;

procedure TACBrCargaBalNutricional.SetQtdePorcao(const Value: Integer);
begin
  FQtdePorcao := Value;
end;

procedure TACBrCargaBalNutricional.SetQtdePorcEmb(const Value: Integer);
begin
  FQtdePorcEmb := Value;
end;

procedure TACBrCargaBalNutricional.SetSodio429(const Value: Double);
begin
  FSodio429 := Value;
end;

procedure TACBrCargaBalNutricional.SetUndPorcao429(
  const Value: TACBRCargaBalNutriUndPorcao429);
begin
  FUndPorcao429 := Value;
end;

procedure TACBrCargaBalNutricional.SetValorEnergetico429(
  const Value: Integer);
begin
  FValorEnergetico429 := Value;
end;

{TACBrCargaBalTaras}
constructor TACBrCargaBalTaras.Create;
begin
  fCodigo:= 0;
  FValor := 0;
end;


{ TACBrCargaBalItem }
constructor TACBrCargaBalItem.Create;
begin
  inherited Create;
  // Criacao da propriedade Setor
  FSetor := TACBrCargaBalSetor.Create;

  // Iniciar os campos de valores
  FCodigo          := 0;
  FDescricao       := EmptyStr;
  FTipo            := tpvPeso;
  FValorVenda      := 0.00;
  FModeloEtiqueta  := 0;
  FValidade        := 0;
  FLote            := '0';

  FNutricional     := TACBrCargaBalNutricional.Create;
  FTara            := TACBrCargaBalTaras.Create;
  FFornecedor      := TACBrCargaBalFornecedor.Create;
  FFracionador     := TACBrCargaBalFracionador.Create;
  FConservacao     := TACBrCargaBalConservacao.Create;
  FInformacaoExtra := TACBrCargaBalInformacaoExtra.Create;
  FTeclado         := TAcbrCargaBalTeclado.create;
  FExtra1          := TACBrCargaBalExtra1.Create;
  FExtra2          := TACBrCargaBalExtra2.Create;

  FCodigoTexto1    := 0;
  FCodigoTexto2    := 0;
  FCodigoTexto3    := 0;
  FCodigoInfoNutr  := 0;
  FImpValidade     := 1;
  FImpEmbalagem    := 1;
end;

destructor TACBrCargaBalItem.Destroy;
begin
  FTeclado.Free;
  FInformacaoExtra.Free;
  FFornecedor.Free;
  FFracionador.Free;
  FConservacao.Free;
  FSetor.Free;
  FNutricional.Free;
  FTara.Free;
  FExtra1.Free;
  FExtra2.Free;
  inherited;
end;

function TACBrCargaBalItem.ObterCodigoInfoExtra(AModelo : TACBrCargaBalModelo): Integer;
begin
  case AModelo of
    modToledo,
    modToledoMGV5,
    modToledoMGV6,
    modToledoMGV7,
    modFilizola :
    begin
      case Length(FInformacaoExtra.Receita) of
        0..2 : FInformacaoExtra.Codigo := 0;
      else
        if (FInformacaoExtra.Codigo = 0) then
        FInformacaoExtra.Codigo := Codigo;
      end;
    end;
  end;
 Result := FInformacaoExtra.Codigo;
end;

procedure TACBrCargaBalItem.SetCodigoExtra1(const Value: SmallInt);
begin
  FCodigoExtra1 := Value;
end;

procedure TACBrCargaBalItem.SetCodigoExtra2(const Value: SmallInt);
begin
  FCodigoExtra2 := Value;
end;

procedure TACBrCargaBalItem.SetExtra2(const Value: TACBrCargaBalExtra2);
begin
  FExtra2 := Value;
end;

{ TACBrCargaBalItens }

constructor TACBrCargaBalItens.create;
begin
  inherited Create(True);
end;

destructor TACBrCargaBalItens.destroy;
begin
  Clear;
  inherited Destroy;
end;

function TACBrCargaBalItens.GetItem(Index: Integer): TACBrCargaBalItem;
begin
  Result := TACBrCargaBalItem(inherited Items[Index]);
end;

function TACBrCargaBalItens.New: TACBrCargaBalItem;
begin
  Result := TACBrCargaBalItem.Create;
  Add(Result);
end;

procedure TACBrCargaBalItens.SetItem(Index: Integer;
  const Value: TACBrCargaBalItem);
begin
  inherited Items[Index] := Value;
end;

{ TACBrCargaBal }
constructor TACBrCargaBal.Create(AOwner: TComponent);
begin
  inherited;
  FProdutos := TACBrCargaBalItens.Create;
  FArquivosGerados := TStringList.Create ;
end;

destructor TACBrCargaBal.Destroy;
begin
  FProdutos.Free;
  FArquivosGerados.Free ;
  inherited;
end;

function TACBrCargaBal.RFill(const Str: string; Tamanho: Integer = 0; Caracter: Char = ' '): string;
begin
  if (Tamanho > 0) and (Length(Str) > Tamanho) then
    Result := Copy(Str, 1, Tamanho)
  else
    Result := Str + StringOfChar(Caracter, Tamanho - Length(Str));
end;

function TACBrCargaBal.LFill(const Str: string; Tamanho: Integer = 0; Caracter: Char = '0'): string;
begin
  if (Tamanho > 0) and (Length(Str) > Tamanho) then
    Result := Copy(Str, 1, Tamanho)
  else
    Result := StringOfChar(Caracter, Tamanho - length(Str)) + Str;
end;

function TACBrCargaBal.LFill(Valor: Currency; Tamanho: Integer;
  Decimais: Integer = 2; Caracter: Char = '0'): string;
var
  i, p: Integer;
begin
  p := 1;

  for i := 1 to Decimais do
    p := p * 10;

  Result := LFill(Trunc(Valor * p), Tamanho, Caracter);
end;

function TACBrCargaBal.LFill(Valor: Integer; Tamanho: Integer;
  Caracter: Char = '0'): string;
begin
  Result := LFill(IntToStr(Valor), Tamanho, Caracter);
end;

function TACBrCargaBal.GetNomeArquivoTaras: String;
begin
  case FModelo of
    modToledo,
    modToledoMGV5, modToledoMGV6, modToledoMGV7 : Result := 'TARA.TXT';
  end;
end;

function TACBrCargaBal.GetNomeArquivoProduto: String;
begin
  case FModelo of
    modFilizola   : Result := 'CADTXT.TXT';
    modToledo     : Result := 'TXITENS.TXT';
    modUrano      : Result := 'PRODUTOS.TXT';
    modUranoS     : Result := 'PRODUTOS.TXT';
    modToledoMGV5,
    modToledoMGV6,
    modToledoMGV7 : Result := 'ITENSMGV.TXT';
    modUranoURF32 : Result := 'PRODUTOS.TXT';
    modRamuza     : Result := 'RAMUZA_ORIGINAL.TXT';
  end;
end;

function TACBrCargaBal.GetNutriUndPorcaoToledo(Tipo: TACBrCargaBalNutriUndPorcao): String;
begin
   case Tipo of
    tpGramas     : Result := '0';
    tpMililitros : Result := '1';
    tpUnidades   : Result := '2';
   end;
end;

function TACBrCargaBal.GetNutriPartDecimalToledo(Tipo: TACBrCargaBalNutriPartdecimal): String;
begin
   case Tipo of
    tpPara0  : Result := '0';
    tpPara14 : Result := '1';
    tpPara13 : Result := '2';
    tpPara12 : Result := '3';
    tpPara23 : Result := '4';
    tpPara34 : Result := '5';
   end;
end;


function TACBrCargaBal.GetNutriPartDecimalToledo429(
  Tipo: TACBrCargaBalNutriPartdecimal429): String;
begin
   case Tipo of
    tpPara0_429  : Result := '0';
    tpPara14_429 : Result := '1';
    tpPara13_429 : Result := '2';
    tpPara12_429 : Result := '3';
    tpPara23_429 : Result := '4';
    tpPara34_429 : Result := '5';
   end;
end;

function TACBrCargaBal.GetNutriUnidPorcaoToledo429(Tipo: TACBRCargaBalNutriUndPorcao429): String;
begin
   case Tipo of
    tpGramas429     : Result := '0';
    tpMililitros429 : Result := '1';
   end;
end;

function TACBrCargaBal.GetTipoProdutoToledo(Tipo: TACBrCargaBalTipoVenda): String;
begin
  case Tipo of
    tpvPeso     : Result := '0';
    tpvUnidade  : Result := '1';
    tpvEAN13    : Result := '2';
    tpvEAN13Und : Result := '5';
  end;
end;

function TACBrCargaBal.GetNutriMedCaseiraToledo(Tipo: TACBrCargaBalNutriMedCaseira): String;
begin
  case tipo of
    tpColherSopa  : Result := '00';
    tpColherCafe  : Result := '01';
    tpColherCha   : Result := '02';
    tpXicara      : Result := '03';
    tpDeXicara    : Result := '04';
    tpUnidade     : Result := '05';
    tpPacote      : Result := '06';
    tpFatia       : Result := '07';
    tpFatiaFina   : Result := '08';
    tpPedaco      : Result := '09';
    tpFolha       : Result := '10';
    tpPao         : Result := '11';
    tpBiscoito    : Result := '12';
    tpBisnaguinha : Result := '13';
    tpDisco       : Result := '14';
    tpCopo        : Result := '15';
    tpPorcao      : Result := '16';
    tpTablete     : Result := '17';
    tpSache       : Result := '18';
    tpAlmodega    : Result := '19';
    tpBife        : Result := '20';
    tpFile        : Result := '21';
    tpConcha      : Result := '22';
    tpBala        : Result := '23';
    tpPratoFundo  : Result := '24';
    tpPitada      : Result := '25';
    tpLata        : Result := '26';
  end;
end;

function TACBrCargaBal.GetNutriMedCaseiraToledo429(Tipo: TACBrCargaBalNutriMedCaseira429): String;
begin
  case tipo of
    tpColherSopa429 : Result := '00';
    tpColherCafe429 : Result := '01';
    tpColherCha429  : Result := '02';
    tpXicara429     : Result := '03';
    tpDeXicara429   : Result := '04';
    tpUnidade429    : Result := '05';
    tpPacote429     : Result := '06';
    tpFatia429      : Result := '07';
    tpFatiaFina429  : Result := '08';
    tpPedaco429     : Result := '09';
    tpFolha429      : Result := '10';
    tpPao429        : Result := '11';
    tpBiscoito429   : Result := '12';
    tpBisnaguinha429: Result := '13';
    tpDisco429      : Result := '14';
    tpCopo429       : Result := '15';
    tpPorcao429     : Result := '16';
    tpTablete429    : Result := '17';
    tpSache429      : Result := '18';
    tpAlmodega429   : Result := '19';
    tpBife429       : Result := '20';
    tpFile429       : Result := '21';
    tpConcha429     : Result := '22';
    tpBala429       : Result := '23';
    tpPratoFundo429 : Result := '24';
    tpPitada429     : Result := '25';
    tpLata429       : Result := '26';
    tpXicaraCha429  : Result := '27';
    tpPratoRaso429  : Result := '28';
  end;
end;

function TACBrCargaBal.GetTipoProdutoFilizola(Tipo: TACBrCargaBalTipoVenda): String;
begin
  case Tipo of
    tpvPeso    : Result := 'P';
    tpvUnidade : Result := 'U';
  end;
end;

function TACBrCargaBal.GetTipoProdutoRamuza(
  Tipo: TACBrCargaBalTipoVenda): string;
begin
  case Tipo of
    tpvPeso    : Result:='0';
    tpvUnidade : Result:='1';
  end;
end;

function TACBrCargaBal.GetTipoProdutoUrano(Tipo: TACBrCargaBalTipoVenda): string;
begin
  case Tipo of
    tpvPeso    : Result:='P';
    tpvUnidade : Result:='U';
  end;
end;

function TACBrCargaBal.GetTipoProdutoUranoURF32(Tipo: TACBrCargaBalTipoVenda): string;
begin
  case Tipo of
    tpvPeso    : Result:='0';
    tpvUnidade : Result:='6';
  end;
end;

function TACBrCargaBal.GetNomeArquivoSetor: String;
begin
  // Toledo e Urano nao possuem arquivo de setor a parte
  case FModelo of
    modFilizola : Result := 'SETORTXT.TXT';
    modToledoMGV5,
    modToledoMGV6,
    modToledoMGV7 : Result := 'DEPTO.TXT';
  end;
end;

function TACBrCargaBal.GetNomeArquivoReceita: String;
begin
  // Urano nao possue arquivo de Receita a parte. EXCETO URANO URF32
  case FModelo of
    modFilizola : Result := 'REC_ASS.TXT';
    modToledoMGV5,
    modToledoMGV6,
    modToledoMGV7: Result := 'TXINFO.TXT';
    modUranoURF32: Result := 'RECEITAS.TXT';
  end;
end;

function TACBrCargaBal.GetNomeArquivoFornecedor: String;
begin
  case FModelo of
    modToledoMGV5,
    modToledoMGV6,
    modToledoMGV7 : Result := 'TXFORN.TXT';
  end;
end;

function TACBrCargaBal.GetNomeArquivoFracionador: String;
begin
  case FModelo of
    modToledoMGV5,
    modToledoMGV6,
    modToledoMGV7 : Result := 'FRACIONA.TXT';
  end;
end;

function TACBrCargaBal.GetNomeArquivoConservacao: String;
begin
  case FModelo of
    modToledoMGV5,
    modToledoMGV6,
    modToledoMGV7 : Result := 'CONSERVA.TXT';
  end;
end;

function TACBrCargaBal.GetNomeArquivoExtra1: String;
begin
  case FModelo of
    modToledoMGV5,
    modToledoMGV6,
    modToledoMGV7: Result := 'CAMPEXT1.TXT';
  end;
end;

function TACBrCargaBal.GetNomeArquivoExtra2: String;
begin
  case FModelo of
    modToledoMGV5,
    modToledoMGV6,
    modToledoMGV7: Result := 'CAMPEXT2.TXT';
  end;
end;

function TACBrCargaBal.GetNomeArquivoTeclado: String;
begin
  case FModelo of
    modToledoMGV5,
    modToledoMGV6,
    modToledoMGV7: Result := 'TXTECLAS.TXT';
  end;
end;

function TACBrCargaBal.GetNomeArquivoNutricional: String;
begin
  // A urano nao possuem arquivo nutricional a parte das informações
  // são incluídas no mesmo arquivo de itens.
  case FModelo of
    modToledoMGV5,
    modToledoMGV6,
    modToledoMGV7 : Result := 'INFNUTRI.TXT';
    modFilizola   : Result := 'NUTRI.TXT';
    modUranoURF32 : Result := 'INFORMACOESNUTRICIONAIS.TXT';
  end;
end;

function TACBrCargaBal.CalcularSoma(const xStr: string): Integer;
var
  I, Vl: Integer;
begin
  result:=0;
  Vl:=0;
  if Length(xStr)<1 then
    exit;
  for I:=1 to Length(xStr) do
  begin
    Vl:=Vl+Ord(xStr[I]);
  end;
  result:=Vl;
end;


procedure TACBrCargaBal.PreencherFilizola(stlArquivo, stlSetor, stlNutricional, stlReceita: TStringList);
var
  i, Total: Integer;
  areceita:string;
begin
  Total := Produtos.Count;

  for i := 0 to Total - 1 do
  begin
    stlArquivo.Add(
      LFIll(Produtos[i].Codigo, 6) +
      GetTipoProdutoFilizola(Produtos[i].Tipo) +
      RFIll(Produtos[i].Descricao, 22) +
      LFIll(Produtos[i].ValorVenda, 7, 2) +
      LFIll(Produtos[i].Validade, 3)+
      RFill(' ',126)+
      LFIll(Produtos[i].Tara.Valor, 4, 3)
    );

    if (Produtos[i].Setor.Descricao <> '') or (Produtos[i].Teclado.Tecla > 0) then
    stlSetor.Add(
      RFill(Produtos[i].Setor.Descricao, 12) +
      LFIll(Produtos[i].Codigo, 6) +
      LFIll(i + 1, 4) +
      LFill(Produtos[i].Teclado.Tecla, 3)
    );

    if (Produtos[i].Nutricional.Descricao <> '') then
    begin
      stlNutricional.Add(LFIll(Produtos[i].Codigo,6) +
       RFill(Produtos[i].Nutricional.Descricao,35) +
       LFIll(Produtos[i].Nutricional.ValorEnergetico,5) +
       LFIll(0,4) +
       LFIll(Produtos[i].Nutricional.Carboidrato,5,1) +
       LFIll(0,4) +
       LFIll(Produtos[i].Nutricional.Proteina,5,1) +
       LFIll(0,4) +
       LFIll(Produtos[i].Nutricional.GorduraTotal,5,1) +
       LFIll(0,4) +
       LFIll(Produtos[i].Nutricional.GorduraSaturada,5,1)+
       LFIll(0,4)+
       LFIll(Produtos[i].Nutricional.GorduraTrans,5,1) +
       LFIll(0,4)+
       LFIll(Produtos[i].Nutricional.Fibra,5,1)+
       LFIll(0,4)+
       RFill('*****',5)+
       RFill('****',4) +
       RFill('*****',5) +
       RFill('****',4) +
       LFIll(Produtos[i].Nutricional.Sodio,5,1)+
       LFIll(0,4)

      );
    end;

    // receita
    areceita := RFill(' ',12)+LFIll(Produtos[i].Codigo,6)+LFIll(Produtos[i].ObterCodigoInfoExtra(modFilizola),6)+RFill(Produtos[i].InformacaoExtra.Receita,840)+'@';
    if (Length(Produtos[i].InformacaoExtra.Receita)>2) and (stlReceita.IndexOf(areceita)<0) then
       stlReceita.Add(areceita);

    Progresso(Format('Gerando produto %6.6d %s', [Produtos[i].Codigo, Produtos[i].Descricao]), i, Total);
  end;
end;

procedure TACBrCargaBal.PreencherRamuza(Arquivo: TStringList);
var
  i, Total: Integer;
//  Anutri,areceita:string;
begin
  Total := Produtos.Count;

  for i := 0 to Total - 1 do
  begin

    Arquivo.Add(
      LFIll(IntToStr(i+1),4)+ // Numero da PLU 4bits
      'A'+ // separador fixo
      '0'+ // zero fixo
      LFIll(Produtos[i].Codigo, 6) +
      LFIll(Produtos[i].ValorVenda, 6, 2) +
      GetTipoProdutoRamuza(Produtos[i].Tipo) +
      LFIll(Produtos[i].Validade, 3) +
      LFIll('0',13)+ // codigo de 13 bits
      LFIll(Produtos[i].Tara.Valor * 100,5)+ // Tara pré determinada ou peso pré determinado – 5 bits.
      LFIll(Produtos[i].ModeloEtiqueta,2)+ // Número da etiqueta – 2 bits.

      {
      LFIll(Produtos[i].Nutricional.Qtd,6) +
      LFIll(Produtos[i].Nutricional.ValorEnergetico,5) +
      LFIll(Produtos[i].Nutricional.Carboidrato,6,1) +
      LFIll(Produtos[i].Nutricional.Proteina,6,1) +
      LFIll(Produtos[i].Nutricional.GorduraTotal,6,1) +
      LFIll(Produtos[i].Nutricional.GorduraSaturada,6,1)+
      LFIll(Produtos[i].Nutricional.GorduraTrans,6,1) +
      LFIll(Produtos[i].Nutricional.Fibra,6,1)+
      LFIll(Produtos[i].Nutricional.Sodio,5)
      }

      LFIll('1',2)+ // Informação nutricional 1 – 2 bits.
      LFIll(Produtos[i].Nutricional.Qtd,5)+ //Quantidade da informação nutricional 1 – 5 bits.
      LFIll(Produtos[i].Nutricional.ValorEnergetico,3)+ //Porcentagem da informação nutricional 1 – 3 bits. .

      LFIll('2',2)+ //Informação nutricional 2 – 2 bits.
      LFIll(Produtos[i].Nutricional.Qtd,5)+ //Quantidade da informação nutricional 2 – 5 bits.
      LFIll(Produtos[i].Nutricional.Carboidrato,3)+ // Porcentagem da informação nutricional 2 – 3 bits.

      LFIll('3',2)+ //Informação nutricional 3 – 2 bits.
      LFIll(Produtos[i].Nutricional.Qtd,5)+ //Quantidade da informação nutricional 3 – 5 bits.
      LFIll(Produtos[i].Nutricional.Proteina,3)+ //Porcentagem da informação nutricional 3 – 3 bits.

      LFIll('4',2)+ //Informação nutricional 4 – 2 bits.
      LFIll(Produtos[i].Nutricional.Qtd,5)+ //Quantidade da informação nutricional 4 – 5 bits.
      LFIll(Produtos[i].Nutricional.GorduraTotal,3)+ //Porcentagem da informação nutricional 4 – 3 bits.

      LFIll('5',2)+ //Informação nutricional 5 – 2 bits.
      LFIll(Produtos[i].Nutricional.Qtd,5)+ //Quantidade da informação nutricional 5 – 5 bits.
      LFIll(Produtos[i].Nutricional.GorduraSaturada,3)+ //Porcentagem da informação nutricional 5 – 3 bits.

      LFIll('6',2)+ //Informação nutricional 6 – 2 bits.
      LFIll(Produtos[i].Nutricional.Qtd,5)+ //Quantidade da informação nutricional 6 – 5 bits.
      LFIll(Produtos[i].Nutricional.GorduraTrans,3)+ //Porcentagem da informação nutricional 6 – 3 bits.

      LFIll('7',2)+ //Informação nutricional 7 – 2 bits.
      LFIll(Produtos[i].Nutricional.Qtd,5)+ //Quantidade da informação nutricional 7 – 5 bits.
      LFIll(Produtos[i].Nutricional.Fibra,3)+ //Porcentagem da informação nutricional 7 – 3 bits.

      LFIll('8',2)+ // Informação nutricional 8 – 2 bits.
      LFIll(Produtos[i].Nutricional.Qtd,5)+ // Quantidade da informação nutricional 8 – 5 bits.
      LFIll(Produtos[i].Nutricional.Sodio,3)+ // Porcentagem da informação nutricional 8 – 3 bits.

      LFIll('0',2)+ // Informação nutricional 9 – 2 bits.
      LFIll('0',5)+ // Quantidade da informação nutricional 9 – 5 bits.
      LFIll('0',3)+ // Porcentagem da informação nutricional 9 – 3 bits.

      LFIll('0',2)+ // Informação nutricional 10 – 2 bits.
      LFIll('0',5)+ // Quantidade da informação nutricional 10 – 5 bits.
      LFIll('0',3)+ // Porcentagem da informação nutricional 10 – 3 bits.

      LFIll('0',2)+ // Informação nutricional 11 – 2 bits.
      LFIll('0',5)+ // Quantidade da informação nutricional 11 – 5 bits.
      LFIll('0',3)+ // Porcentagem da informação nutricional 11 – 3 bits.

      LFIll('0',2)+ // Informação nutricional 12 – 2 bits.
      LFIll('0',5)+ // Quantidade da informação nutricional 12 – 5 bits.
      LFIll('0',3)+ // Porcentagem da informação nutricional 12 – 3 bits.

      '##'+ // Separador(sem espaços em branco antes nem depois).
      RFIll(Produtos[i].Descricao, 24) +  //Nome do produto.
      '##'+ // Separador. (sem espaços em branco antes nem depois)
      ''+ //Texto extra 1.
      '##' // Separador. (sem espaços em branco antes nem depois)
    );

    Progresso(Format('Gerando produto %6.6d %s', [Produtos[i].Codigo, Produtos[i].Descricao]), i, Total);
  end;
end;

procedure TACBrCargaBal.PreencherToledo(stlArquivo, stlNutricional, stlReceita, stlTara, stlFornecedor, stlFracionador, stlConservacao, stlSetor, stlTeclado, stlExtra1, stlExtra2: TStringList; Versao: Integer);
var
  i, Total : Integer;
  ANutri, AReceita, ATara, AFornecedor, AFracionador, AConservacao, ASetor, AExtra1, AExtra2: string;
  LTXTeclas:TStringList;
begin
  Total := Produtos.Count;
  LTXTeclas := TStringList.Create;
  try


    for i := 0 to Total - 1 do
    begin
      if Versao = 0 then
      begin
        stlArquivo.Add(
          LFIll(Produtos[i].Setor.Codigo, 2) +
          LFIll(Produtos[i].ModeloEtiqueta, 2) +
          GetTipoProdutoToledo(Produtos[i].Tipo) +
          LFIll(Produtos[i].Codigo, 6) +
          LFIll(Produtos[i].ValorVenda, 6, 2) +
          LFIll(Produtos[i].Validade, 3) +
          RFIll(Produtos[i].Descricao, 50) +
          RFIll(Produtos[i].InformacaoExtra.Receita, 250)
        );
      end
      else if Versao = 1 then
      begin
        // ITENSMGV.TXT - VERSÃO 1
        stlArquivo.Add(
          LFIll(Produtos[i].Setor.Codigo, 2) +
          GetTipoProdutoToledo(Produtos[i].Tipo) +
          LFIll(Produtos[i].Codigo, 6) +
          LFIll(Produtos[i].ValorVenda, 6, 2) +
          LFIll(Produtos[i].Validade, 3) +
          RFIll(Produtos[i].Descricao, 50) +
          LFIll(Produtos[i].Codigo, 6)+ // codigo inf extra
          LFIll('0', 3)+ // codigo imagem
          LFIll(Produtos[i].Nutricional.Codigo, 4)+ // codigo inf nutricional
//          RFill('1', 1)+ // imprime data de validade
          RFill(IntToStr(Produtos[i].ImpValidade), 1)+ // imprime data de validade
          RFill(IntToStr(Produtos[i].ImpEmbalagem), 1)+ // imprime data embalagem
          LFIll(Produtos[i].CodigoFornecedor, 4)+ // codigo fornecedor
          //LFIll('0', 4)+ // codigo fornecedor
          lFill(Produtos[i].Lote, 12)+ // lote
          lFill('0', 11)+ // codigo especial
          LFIll('0', 1)+ // versao do preco
          LFIll('0', 2)
        );

        // receita
        AReceita := LFIll(Produtos[i].Codigo, 6) +
                    RFill(Produtos[i].InformacaoExtra.Observacao, 100) +
                    RFill(Produtos[i].InformacaoExtra.Receita, 840);

        if (Length(Produtos[i].InformacaoExtra.Receita) > 2) and (stlReceita.IndexOf(AReceita) < 0) then
           stlReceita.Add(AReceita);

        ANutri := 'N'+ LFIll(Produtos[i].Nutricional.Codigo, 6) +
                  '0' +
                  LFIll(Produtos[i].Nutricional.Qtd, 3) +
                  GetNutriUndPorcaoToledo(Produtos[i].Nutricional.UndPorcao) +
                  LFIll(Produtos[i].Nutricional.PartInteira, 2) +
                  GetNutriPartDecimalToledo(Produtos[i].Nutricional.PartDecimal) +
                  GetNutriMedCaseiraToledo(Produtos[i].Nutricional.MedCaseira) +
                  LFIll(Produtos[i].Nutricional.ValorEnergetico, 4) +
                  LFIll(Produtos[i].Nutricional.Carboidrato, 4, 1) +
                  LFIll(Produtos[i].Nutricional.Proteina, 3, 1) +
                  LFIll(Produtos[i].Nutricional.GorduraTotal, 3, 1) +
                  LFIll(Produtos[i].Nutricional.GorduraSaturada, 3, 1) +
                  LFIll(Produtos[i].Nutricional.GorduraTrans, 3, 1) +
                  LFIll(Produtos[i].Nutricional.Fibra, 3, 1) +
                  LFIll(Produtos[i].Nutricional.Sodio, 5, 1);

        if (Produtos[i].Nutricional.Codigo > 0) and (stlNutricional.IndexOf(ANutri) < 0) then
           stlNutricional.Add(ANutri);


        ATara:= LFIll(Produtos[i].Tara.Codigo, 4) + LFIll(Produtos[i].Tara.Valor, 6, 3)+
                RFIll(Produtos[i].Tara.Descricao, 20);

        if (Produtos[i].Tara.Codigo > 0) and (stlTara.IndexOf(ATara) < 0) THEN
           stlTara.Add(ATara);

        AFornecedor := LFIll(Produtos[i].Fornecedor.Codigo, 4) + RFIll(Produtos[i].Fornecedor.Observacao, 100) +
                       RFill(Produtos[i].Fornecedor.Descricao1, 56) +
                       RFill(Produtos[i].Fornecedor.Descricao2, 56) +
                       RFill(Produtos[i].Fornecedor.Descricao3, 56) +
                       RFill(Produtos[i].Fornecedor.Descricao4, 56) +
                       RFill(Produtos[i].Fornecedor.Descricao5, 56);

        if (Produtos[i].Fornecedor.Codigo > 0) and (stlFornecedor.IndexOf(AFornecedor) < 0) then
          stlFornecedor.Add(AFornecedor);

        AFracionador := LFIll(Produtos[i].Fracionador.Codigo, 4) + RFIll(Produtos[i].Fracionador.Observacao, 100) +
                       RFill(Produtos[i].Fracionador.Descricao1, 56) +
                       RFill(Produtos[i].Fracionador.Descricao2, 56) +
                       RFill(Produtos[i].Fracionador.Descricao3, 56);

        if (Produtos[i].Fracionador.Codigo > 0) and (stlFracionador.IndexOf(AFracionador) < 0) then
          stlFracionador.Add(AFracionador);

        AConservacao := LFIll(Produtos[i].Conservacao.Codigo, 4) + RFIll(Produtos[i].Conservacao.Observacao, 100) +
                       RFill(Produtos[i].Conservacao.Descricao1, 56) +
                       RFill(Produtos[i].Conservacao.Descricao2, 56) +
                       RFill(Produtos[i].Conservacao.Descricao3, 56);

        if (Produtos[i].Conservacao.Codigo > 0) and (stlConservacao.IndexOf(AConservacao) < 0) then
          stlConservacao.Add(AConservacao);

        ASetor := LFIll(Produtos[i].Setor.Codigo, 2) + RFIll(Produtos[i].Setor.Descricao, 40);

        if ((Produtos[i].Setor.Codigo > 0) and (stlSetor.IndexOf(ASetor) < 0)) then
          stlSetor.Add(ASetor);
      end
      else if Versao = 2 then
      begin
        // ITENSMGV.TXT - VERSÃO 2
        stlArquivo.Add(
          LFIll(Produtos[i].Setor.Codigo, 2) +
          GetTipoProdutoToledo(Produtos[i].Tipo) +
          LFIll(Produtos[i].Codigo, 6) +
          LFIll(Produtos[i].ValorVenda, 6, 2) +
          LFIll(Produtos[i].Validade, 3) +
          RFIll(Produtos[i].Descricao, 50) +
          LFIll(Produtos[i].ObterCodigoInfoExtra(modToledoMGV5), 6)+ // codigo inf extra
          LFIll('0', 4)+ // codigo imagem
          LFIll(Produtos[i].Nutricional.Codigo,6)+ // codigo inf nutricional
//          RFill('1', 1)+ // imprime data de validade
          RFill(IntToStr(Produtos[i].ImpValidade), 1)+ // imprime data de validade
          RFill(IntToStr(Produtos[i].ImpEmbalagem), 1)+ // imprime data embalagem
          LFIll(Produtos[i].CodigoFornecedor, 4)+ // codigo fornecedor
          //LFIll('0', 4)+ // codigo fornecedor
          lFill(Produtos[i].Lote, 12)+ // lote
          lFill('0', 11)+ // codigo especial
          LFIll('0', 1)+ // versao do preco
          LFIll('0', 4)+ // codigo do som
          LFIll(IntToStr(Produtos[i].CodigoTara),4)+ // codigo da tara
          //LFIll('0', 4)+ // codigo da tara
          LFIll(Produtos[i].CodigoFracionador, 4)+ // codigo da fracionador
          LFIll('0', 4)+ // Código do Campo Extra 1
          LFIll('0', 4)+ // Código do Campo Extra 2
          LFIll(Produtos[i].CodigoConservacao, 4)+ // Código da Conservação
          LFIll(Produtos[i].EAN13Fornecedor, 12) // EAN-13, quando utilizado Tipo de Produto EAN-13
        );

        if Produtos[i].Teclado.Tecla > 0 then
        begin
          stlTeclado.Add(
            LFill(Produtos[i].Teclado.Codigo_Teclado, 2) +
            LFill(Produtos[i].Teclado.Pagina_Teclado, 1) +
            LFIll(Produtos[i].Teclado.Tecla, 2) +
            LFIll(Produtos[i].Codigo, 6) +
            '0' +
            RFIll(Produtos[i].Descricao, 24) +
            RFIll('', 255)
          );
        end;

        if (Length(Produtos[i].InformacaoExtra.Receita) > 2) then
        begin
        // receita
          AReceita := LFIll(Produtos[i].ObterCodigoInfoExtra(modToledoMGV5), 6) +
                      RFill(Produtos[i].InformacaoExtra.Observacao, 100) +
                      RFill(Produtos[i].InformacaoExtra.Receita, 840);

          if (stlReceita.IndexOf(AReceita) < 0) then
            stlReceita.Add(AReceita);
        end;

        if (Produtos[i].Nutricional.Codigo > 0) then
        begin
         ANutri := 'N'+ LFIll(Produtos[i].Nutricional.Codigo, 6) +
                   '0' +
                   LFIll(Produtos[i].Nutricional.Qtd, 3) +
                   GetNutriUndPorcaoToledo(Produtos[i].Nutricional.UndPorcao) +
                   LFIll(Produtos[i].Nutricional.PartInteira, 2) +
                   GetNutriPartDecimalToledo(Produtos[i].Nutricional.PartDecimal) +
                   GetNutriMedCaseiraToledo(Produtos[i].Nutricional.MedCaseira) +
                   LFIll(Produtos[i].Nutricional.ValorEnergetico, 4) +
                   LFIll(Produtos[i].Nutricional.Carboidrato, 4, 1) +
                   LFIll(Produtos[i].Nutricional.Proteina, 3, 1) +
                   LFIll(Produtos[i].Nutricional.GorduraTotal, 3, 1) +
                   LFIll(Produtos[i].Nutricional.GorduraSaturada, 3, 1) +
                   LFIll(Produtos[i].Nutricional.GorduraTrans, 3, 1) +
                   LFIll(Produtos[i].Nutricional.Fibra, 3, 1) +
                   LFIll(Produtos[i].Nutricional.Sodio, 5, 1);

         if (stlNutricional.IndexOf(ANutri) < 0) then
            stlNutricional.Add(ANutri);
        end;

        if (Produtos[i].Tara.Codigo > 0) then
        begin
         ATara := LFIll(Produtos[i].Tara.Codigo, 4) + LFIll(Produtos[i].Tara.Valor, 6, 3)+
                  RFIll(Produtos[i].Tara.Descricao, 20);

         if (stlTara.IndexOf(ATara) < 0) then
            stlTara.Add(ATara);
        end;

        if (Produtos[i].Fornecedor.Codigo > 0) then
        begin
         AFornecedor := LFIll(Produtos[i].Fornecedor.Codigo, 4) + RFIll(Produtos[i].Fornecedor.Observacao, 100) +
                        RFill(Produtos[i].Fornecedor.Descricao1, 56) +
                        RFill(Produtos[i].Fornecedor.Descricao2, 56) +
                        RFill(Produtos[i].Fornecedor.Descricao3, 56) +
                        RFill(Produtos[i].Fornecedor.Descricao4, 56) +
                        RFill(Produtos[i].Fornecedor.Descricao5, 56);

         if (stlFornecedor.IndexOf(AFornecedor) < 0) then
           stlFornecedor.Add(AFornecedor);
        end;

        if (Produtos[i].Fracionador.Codigo > 0) then
        begin
         AFracionador := LFIll(Produtos[i].Fracionador.Codigo, 4) + RFIll(Produtos[i].Fracionador.Observacao, 100) +
                        RFill(Produtos[i].Fracionador.Descricao1, 56) +
                        RFill(Produtos[i].Fracionador.Descricao2, 56) +
                        RFill(Produtos[i].Fracionador.Descricao3, 56);

         if (stlFracionador.IndexOf(AFracionador) < 0) then
           stlFracionador.Add(AFracionador);
        end;


        if (Produtos[i].Conservacao.Codigo > 0) then
        begin
         AConservacao := LFIll(Produtos[i].Conservacao.Codigo, 4) + RFIll(Produtos[i].Conservacao.Observacao, 100) +
                        RFill(Produtos[i].Conservacao.Descricao1, 56) +
                        RFill(Produtos[i].Conservacao.Descricao2, 56) +
                        RFill(Produtos[i].Conservacao.Descricao3, 56);

         if (stlConservacao.IndexOf(AConservacao) < 0) then
           stlConservacao.Add(AConservacao);
        end;


        if (Produtos[i].Setor.Codigo > 0) then
        begin
         ASetor := LFIll(Produtos[i].Setor.Codigo, 2) + RFIll(Produtos[i].Setor.Descricao, 40);

         if (stlSetor.IndexOf(ASetor) < 0) then
           stlSetor.Add(ASetor);
        end;

      end
      else if Versao = 3 then
      begin
        // ITENSMGV.TXT - VERSÃO 3
        stlArquivo.Add(
          LFIll(Produtos[i].Setor.Codigo, 2) +
          GetTipoProdutoToledo(Produtos[i].Tipo) +
          LFIll(Produtos[i].Codigo, 6) +
          LFIll(Produtos[i].ValorVenda, 6, 2) +
          LFIll(Produtos[i].Validade, 3) +
          RFIll(Produtos[i].Descricao, 50) +
          LFIll(Produtos[i].ObterCodigoInfoExtra(modToledoMGV6), 6)+ // codigo inf extra
          LFIll('0', 4)+ // codigo imagem
          LFIll(Produtos[i].Nutricional.Codigo,6)+ // codigo inf nutricional
          RFill(IntToStr(Produtos[i].ImpValidade), 1)+ // imprime data de validade
          RFill(IntToStr(Produtos[i].ImpEmbalagem), 1)+ // imprime data embalagem
          LFIll(Produtos[i].CodigoFornecedor, 4)+ // codigo fornecedor
          //LFIll('0', 4)+ // codigo fornecedor
          lFill(Produtos[i].Lote, 12)+ // lote
          lFill('0', 11)+ // codigo especial
          LFIll('0', 1)+ // versao do preco
          LFIll('0', 4)+ // codigo do som
          LFIll(IntToStr(Produtos[i].CodigoTara),4)+ // codigo da tara
          //LFIll('0', 4)+ // codigo da tara
          LFIll(Produtos[i].CodigoFracionador, 4)+ // codigo da fracionador
          LFIll(Produtos[i].CodigoExtra1, 4)+ // Código do Campo Extra 1
          LFIll(Produtos[i].CodigoExtra2, 4)+ // Código do Campo Extra 2
          LFIll(Produtos[i].CodigoConservacao, 4)+ // Código da Conservação
          LFIll(Produtos[i].EAN13Fornecedor, 12) // EAN-13, quando utilizado Tipo de Produto EAN-13

          (* Novos campos para o arquivo do MGV6 - De acordo com a observação
            eles podem estar inexistentes no arquivo, pois serão considerados como não associados.*)
          {LFIll('0', 6)+ // Percentual de Glaciamento
          LFIll('0', 2)+ // Sequencia de departamentos Associados. Ex: Para associar departamentos 2 e 5: |0205|
          LFIll('', 35)+ // Descritivo do Item – Terceira Linha
          LFIll('', 35)+ // Descritivo do Item – Quarta Linha
          LFIll('0', 4)+ // Código do Campo Extra 3
          LFIll('0', 4)+ // Código do Campo Extra 4
          LFIll('0', 6) // Código da mídia (Prix 6 Touch)}
        );
        //TXTECLAS.TXT

        if Produtos[i].Teclado.Tecla > 0 then
        begin
          if LTXTeclas.IndexOf('OBS'+LFill(Produtos[i].Teclado.Codigo_Teclado, 2)+RFIll('TECLADO '+IntToStr(Produtos[i].Teclado.Codigo_Teclado), 100)) < 0 then
            LTXTeclas.Add('OBS'+LFill(Produtos[i].Teclado.Codigo_Teclado, 2)+RFIll('TECLADO '+IntToStr(Produtos[i].Teclado.Codigo_Teclado), 100));
          stlTeclado.Add(
            LFill(Produtos[i].Teclado.Codigo_Teclado, 2) +
            LFill(Produtos[i].Teclado.Pagina_Teclado, 1) +
            LFIll(Produtos[i].Teclado.Tecla, 2) +
            LFIll(Produtos[i].Codigo, 6) +
            '0' +
            RFIll(Produtos[i].Descricao, 24) +
            RFIll('', 255)
          );
        end;

        if (Length(Produtos[i].InformacaoExtra.Receita) > 2) then
        begin
        // receita
          AReceita := LFIll(Produtos[i].ObterCodigoInfoExtra(modToledoMGV6), 6) +
                      RFill(Produtos[i].InformacaoExtra.Observacao, 100) +
                      RFill(Produtos[i].InformacaoExtra.Receita, 840);

          if (stlReceita.IndexOf(AReceita) < 0) then
             stlReceita.Add(AReceita);
        end;

        if (Produtos[i].Nutricional.Codigo > 0) then
        begin
         ANutri := 'N'+ LFIll(Produtos[i].Nutricional.Codigo, 6) +
                  '0' +
                  LFIll(Produtos[i].Nutricional.Qtd, 3) +
                  GetNutriUndPorcaoToledo(Produtos[i].Nutricional.UndPorcao) +
                  LFIll(Produtos[i].Nutricional.PartInteira, 2) +
                  GetNutriPartDecimalToledo(Produtos[i].Nutricional.PartDecimal) +
                  GetNutriMedCaseiraToledo(Produtos[i].Nutricional.MedCaseira) +
                  LFIll(Produtos[i].Nutricional.ValorEnergetico, 4) +
                  LFIll(Produtos[i].Nutricional.Carboidrato, 4, 1) +
                  LFIll(Produtos[i].Nutricional.Proteina, 3, 1) +
                  LFIll(Produtos[i].Nutricional.GorduraTotal, 3, 1) +
                  LFIll(Produtos[i].Nutricional.GorduraSaturada, 3, 1) +
                  LFIll(Produtos[i].Nutricional.GorduraTrans, 3, 1) +
                  LFIll(Produtos[i].Nutricional.Fibra, 3, 1) +
                  LFIll(Produtos[i].Nutricional.Sodio, 5, 1);

         if (stlNutricional.IndexOf(ANutri) < 0) then
           stlNutricional.Add(ANutri);
       end;

       if (Produtos[i].Tara.Codigo > 0) then
       begin
        ATara := LFIll(Produtos[i].Tara.Codigo, 4) + LFIll(Produtos[i].Tara.Valor, 6, 3)+
                 RFIll(Produtos[i].Tara.Descricao, 20);

        if (stlTara.IndexOf(ATara) < 0) THEN
           stlTara.Add(ATara);
       end;

       if (Produtos[i].Fornecedor.Codigo > 0) then
       begin
        AFornecedor := LFIll(Produtos[i].Fornecedor.Codigo, 4) + RFIll(Produtos[i].Fornecedor.Observacao, 100) +
                       RFill(Produtos[i].Fornecedor.Descricao1, 56) +
                       RFill(Produtos[i].Fornecedor.Descricao2, 56) +
                       RFill(Produtos[i].Fornecedor.Descricao3, 56) +
                       RFill(Produtos[i].Fornecedor.Descricao4, 56) +
                       RFill(Produtos[i].Fornecedor.Descricao5, 56);

        if (stlFornecedor.IndexOf(AFornecedor) < 0) then
          stlFornecedor.Add(AFornecedor);
       end;

       if (Produtos[i].Fracionador.Codigo > 0) then
       begin
        AFracionador := LFIll(Produtos[i].Fracionador.Codigo, 4) + RFIll(Produtos[i].Fracionador.Observacao, 100) +
                       RFill(Produtos[i].Fracionador.Descricao1, 56) +
                       RFill(Produtos[i].Fracionador.Descricao2, 56) +
                       RFill(Produtos[i].Fracionador.Descricao3, 56);

        if (stlFracionador.IndexOf(AFracionador) < 0) then
          stlFracionador.Add(AFracionador);
       end;

       if (Produtos[i].Conservacao.Codigo > 0) then
       begin
        AConservacao := LFIll(Produtos[i].Conservacao.Codigo, 4) + RFIll(Produtos[i].Conservacao.Observacao, 100) +
                       RFill(Produtos[i].Conservacao.Descricao1, 56) +
                       RFill(Produtos[i].Conservacao.Descricao2, 56) +
                       RFill(Produtos[i].Conservacao.Descricao3, 56);

        if (stlConservacao.IndexOf(AConservacao) < 0) then
          stlConservacao.Add(AConservacao);
       end;

       if (Produtos[i].Extra1.Codigo>0) then begin
         AExtra1:=LFIll(Produtos[i].Extra1.Codigo,4)+RFill(Produtos[i].Extra1.Observacao,100)+
                  RFill(Produtos[i].Extra1.Linha1,56)+
                  RFill(Produtos[i].Extra1.Linha2,56)+
                  RFill(Produtos[i].Extra1.Linha3,56)+
                  RFill(Produtos[i].Extra1.Linha4,56)+
                  RFill(Produtos[i].Extra1.Linha5,56);
         if (stlExtra1.IndexOf(AExtra1)<0) then
           stlExtra1.Add(AExtra1);
       end;

       if (Produtos[i].Extra2.Codigo>0) then begin
         AExtra2:=LFIll(Produtos[i].Extra2.Codigo,4)+RFill(Produtos[i].Extra2.Observacao,100)+
                  RFill(Produtos[i].Extra2.Linha1,56)+
                  RFill(Produtos[i].Extra2.Linha2,56)+
                  RFill(Produtos[i].Extra2.Linha3,56)+
                  RFill(Produtos[i].Extra2.Linha4,56)+
                  RFill(Produtos[i].Extra2.Linha5,56);
         if (stlExtra2.IndexOf(AExtra2)<0) then
           stlExtra2.Add(AExtra2);
       end;

       if (Produtos[i].Setor.Codigo > 0) then
       begin
        ASetor := LFIll(Produtos[i].Setor.Codigo, 2) + RFIll(Produtos[i].Setor.Descricao, 40);

        if (stlSetor.IndexOf(ASetor) < 0) then
          stlSetor.Add(ASetor);
       end;

      end;

      Progresso(Format('Gerando produto %6.6d %s', [Produtos[i].Codigo, Produtos[i].Descricao]), i, Total);
    end;
    if LTXTeclas.Count > 0 then
      stlTeclado.Text := LTXTeclas.Text + stlTeclado.Text;
  finally
    LTXTeclas.Free;
  end;
end;

procedure TACBrCargaBal.PreencherToledoMGV7(stlArquivo, stlNutricional, stlReceita, stlTara, stlFornecedor, stlFracionador, stlConservacao, stlSetor, stlTeclado, stlExtra1, stlExtra2: TStringList; Versao: Integer);
var i, Total : Integer;
    ANutri, AReceita, ATara, AFornecedor, AFracionador, AConservacao, ASetor, AExtra1, AExtra2: string;
    LTXTeclas:TStringList;
begin
  Total:=Produtos.Count;
  LTXTeclas:=TStringList.Create;
  try
    for i:=0 to Total-1 do begin
      if Versao=4 then begin
        // ITENSMGV.TXT - VERSÃO 4
        stlArquivo.Add(LFIll(Produtos[i].Setor.Codigo, 2) +
                       GetTipoProdutoToledo(Produtos[i].Tipo) +
                       LFIll(Produtos[i].Codigo, 6) +
                       LFIll(Produtos[i].ValorVenda, 6, 2) +
                       LFIll(Produtos[i].Validade, 3) +
                       RFIll(Produtos[i].Descricao, 50) +
                       LFIll(Produtos[i].ObterCodigoInfoExtra(modToledoMGV6), 6)+ // codigo inf extra
                       LFIll('0', 4)+ // codigo imagem
                       LFIll(Produtos[i].Nutricional.Codigo,6)+ // codigo inf nutricional
                       RFill(IntToStr(Produtos[i].ImpValidade), 1)+ // imprime data de validade
                       RFill(IntToStr(Produtos[i].ImpEmbalagem), 1)+ // imprime data embalagem
                       LFIll(Produtos[i].CodigoFornecedor, 4)+ // codigo fornecedor
                       //LFIll('0', 4)+ // codigo fornecedor
                       lFill(Produtos[i].Lote, 12)+ // lote
                       lFill('0', 11)+ // codigo especial
                       LFIll('0', 1)+ // versao do preco
                       LFIll('0', 4)+ // codigo do som
                       LFIll(IntToStr(Produtos[i].CodigoTara),4)+ // codigo da tara
                       //LFIll('0', 4)+ // codigo da tara
                       LFIll(Produtos[i].CodigoFracionador, 4)+ // codigo da fracionador
                       LFIll(Produtos[i].CodigoExtra1, 4)+ // Código do Campo Extra 1
                       LFIll(Produtos[i].CodigoExtra2, 4)+ // Código do Campo Extra 2
                       LFIll(Produtos[i].CodigoConservacao, 4)+ // Código da Conservação
                       LFIll(Produtos[i].EAN13Fornecedor, 12) // EAN-13, quando utilizado Tipo de Produto EAN-13
                       );
        //TXTECLAS.TXT
        if Produtos[i].Teclado.Tecla > 0 then begin
          if LTXTeclas.IndexOf('OBS'+LFill(Produtos[i].Teclado.Codigo_Teclado, 2)+RFIll('TECLADO '+IntToStr(Produtos[i].Teclado.Codigo_Teclado), 100)) < 0 then
            LTXTeclas.Add('OBS'+LFill(Produtos[i].Teclado.Codigo_Teclado, 2)+RFIll('TECLADO '+IntToStr(Produtos[i].Teclado.Codigo_Teclado), 100));
          stlTeclado.Add(LFill(Produtos[i].Teclado.Codigo_Teclado, 2) +
                         LFill(Produtos[i].Teclado.Pagina_Teclado, 1) +
                         LFIll(Produtos[i].Teclado.Tecla, 2) +
                         LFIll(Produtos[i].Codigo, 6) +
                         '0' +
                         RFIll(Produtos[i].Descricao, 24) +
                         RFIll('', 255));
        end;
        if (Length(Produtos[i].InformacaoExtra.Receita) > 2) then begin
        // receita
          AReceita := LFIll(Produtos[i].ObterCodigoInfoExtra(modToledoMGV6), 6) +
                      RFill(Produtos[i].InformacaoExtra.Observacao, 100) +
                      RFill(Produtos[i].InformacaoExtra.Receita, 840);
          if (stlReceita.IndexOf(AReceita) < 0) then
            stlReceita.Add(AReceita);
        end;
        if (Produtos[i].Nutricional.Codigo > 0) then begin
          ANutri:='N'+
                  LFIll(Produtos[i].Nutricional.Codigo, 6) +
                  '0' +
                  LFIll(Produtos[i].Nutricional.Qtd, 3) +
                  GetNutriUndPorcaoToledo(Produtos[i].Nutricional.UndPorcao) +
                  LFIll(Produtos[i].Nutricional.PartInteira, 2) +
                  GetNutriPartDecimalToledo(Produtos[i].Nutricional.PartDecimal) +
                  GetNutriMedCaseiraToledo(Produtos[i].Nutricional.MedCaseira) +
                  LFIll(Produtos[i].Nutricional.ValorEnergetico, 4) +
                  LFIll(Produtos[i].Nutricional.Carboidrato, 4, 1) +
                  LFIll(Produtos[i].Nutricional.Proteina, 3, 1) +
                  LFIll(Produtos[i].Nutricional.GorduraTotal, 3, 1) +
                  LFIll(Produtos[i].Nutricional.GorduraSaturada, 3, 1) +
                  LFIll(Produtos[i].Nutricional.GorduraTrans, 3, 1) +
                  LFIll(Produtos[i].Nutricional.Fibra, 3, 1) +
                  LFIll(Produtos[i].Nutricional.Sodio, 5, 1)+
                  LFIll('0',4)+
                  '|'+
                  LFILL(Produtos[i].Nutricional.QtdeAutomaticaPorcao429)+
                  LFIll(Produtos[i].Nutricional.QtdePorcEmb429,3)+
                  LFIll(Produtos[i].Nutricional.QtdePorcao429,3)+
                  GetNutriUnidPorcaoToledo429(Produtos[i].Nutricional.UndPorcao429)+
                  LFIll(Produtos[i].Nutricional.PartIntMedidaCaseira429,2)+
                  GetNutriPartDecimalToledo429(Produtos[i].Nutricional.PartDecMedidaCaseira429)+
                  GetNutriMedCaseiraToledo429(Produtos[i].Nutricional.MedCaseira429)+
                  LFIll(Produtos[i].Nutricional.ValorEnergetico429,4)+
                  LFIll(Produtos[i].Nutricional.Carboidrato429,4,1)+
                  LFIll(Produtos[i].Nutricional.AcucaresTotais429,3,1)+
                  LFIll(Produtos[i].Nutricional.AcucaresAdicionados429,3,1)+
                  LFIll(Produtos[i].Nutricional.Proteinas429,3,1)+
                  LFIll(Produtos[i].Nutricional.GordurasTotais429,3,1)+
                  LFIll(Produtos[i].Nutricional.GordurasSaturadas429,3,1)+
                  LFIll(Produtos[i].Nutricional.GordurasTrans429,3,1)+
                  LFIll(Produtos[i].Nutricional.FibraAlimentar429,3,1)+
                  LFIll(Produtos[i].Nutricional.Sodio429,5,1)+
                  LFIll(Produtos[i].Nutricional.AltoAcucar429,1)+
                  LFIll(Produtos[i].Nutricional.AltoGordura429,1)+
                  LFIll(Produtos[i].Nutricional.AltoSodio429,1)+
                  LFIll(Produtos[i].Nutricional.Lactose429,5,1)+
                  LFIll(Produtos[i].Nutricional.Galactose429,5,1)+
                  LFIll(Produtos[i].Nutricional.ImprimeLactoseGalactose,1)+
                  LFIll(Produtos[i].Nutricional.AcucaresAdicionadosEstendido429,5)+
                  LFIll(Produtos[i].Nutricional.AcucaresTotaisEstendido429,5)+
                  LFIll(Produtos[i].Nutricional.GordurasTotaisEstendido429,5)+
                  LFIll(Produtos[i].Nutricional.ProteinasEstendido429,5);
         if (stlNutricional.IndexOf(ANutri) < 0) then
          stlNutricional.Add(ANutri);
        end;
        if (Produtos[i].Tara.Codigo > 0) then begin
          ATara := LFIll(Produtos[i].Tara.Codigo, 4) +
                   LFIll(Produtos[i].Tara.Valor, 6, 3)+
                   RFIll(Produtos[i].Tara.Descricao, 20);
          if (stlTara.IndexOf(ATara) < 0) then
           stlTara.Add(ATara);
        end;
        if (Produtos[i].Fornecedor.Codigo > 0) then begin
          AFornecedor := LFIll(Produtos[i].Fornecedor.Codigo, 4) +
                         RFIll(Produtos[i].Fornecedor.Observacao, 100) +
                         RFill(Produtos[i].Fornecedor.Descricao1, 56) +
                         RFill(Produtos[i].Fornecedor.Descricao2, 56) +
                         RFill(Produtos[i].Fornecedor.Descricao3, 56) +
                         RFill(Produtos[i].Fornecedor.Descricao4, 56) +
                         RFill(Produtos[i].Fornecedor.Descricao5, 56);
          if (stlFornecedor.IndexOf(AFornecedor) < 0) then
            stlFornecedor.Add(AFornecedor);
        end;
        if (Produtos[i].Fracionador.Codigo > 0) then begin
          AFracionador := LFIll(Produtos[i].Fracionador.Codigo, 4) +
                          RFIll(Produtos[i].Fracionador.Observacao, 100) +
                          RFill(Produtos[i].Fracionador.Descricao1, 56) +
                          RFill(Produtos[i].Fracionador.Descricao2, 56) +
                          RFill(Produtos[i].Fracionador.Descricao3, 56);
           if (stlFracionador.IndexOf(AFracionador) < 0) then
            stlFracionador.Add(AFracionador);
        end;
        if (Produtos[i].Conservacao.Codigo > 0) then begin
          AConservacao := LFIll(Produtos[i].Conservacao.Codigo, 4) +
                          RFIll(Produtos[i].Conservacao.Observacao, 100) +
                          RFill(Produtos[i].Conservacao.Descricao1, 56) +
                          RFill(Produtos[i].Conservacao.Descricao2, 56) +
                          RFill(Produtos[i].Conservacao.Descricao3, 56);
          if (stlConservacao.IndexOf(AConservacao) < 0) then
           stlConservacao.Add(AConservacao);
        end;
        if (Produtos[i].Extra1.Codigo>0) then begin
          AExtra1:=LFIll(Produtos[i].Extra1.Codigo,4)+
                   RFill(Produtos[i].Extra1.Observacao,100)+
                   RFill(Produtos[i].Extra1.Linha1,56)+
                   RFill(Produtos[i].Extra1.Linha2,56)+
                   RFill(Produtos[i].Extra1.Linha3,56)+
                   RFill(Produtos[i].Extra1.Linha4,56)+
                   RFill(Produtos[i].Extra1.Linha5,56);
          if (stlExtra1.IndexOf(AExtra1)<0) then
           stlExtra1.Add(AExtra1);
        end;
        if (Produtos[i].Extra2.Codigo>0) then begin
          AExtra2:=LFIll(Produtos[i].Extra2.Codigo,4)+
                   RFill(Produtos[i].Extra2.Observacao,100)+
                   RFill(Produtos[i].Extra2.Linha1,56)+
                   RFill(Produtos[i].Extra2.Linha2,56)+
                   RFill(Produtos[i].Extra2.Linha3,56)+
                   RFill(Produtos[i].Extra2.Linha4,56)+
                   RFill(Produtos[i].Extra2.Linha5,56);
          if (stlExtra2.IndexOf(AExtra2)<0) then
           stlExtra2.Add(AExtra2);
        end;
        if (Produtos[i].Setor.Codigo > 0) then begin
          ASetor:=LFIll(Produtos[i].Setor.Codigo, 2) + 
                  RFIll(Produtos[i].Setor.Descricao, 40);
         if (stlSetor.IndexOf(ASetor) < 0) then
          stlSetor.Add(ASetor);
        end;
      end;
      Progresso(Format('Gerando produto %6.6d %s', [Produtos[i].Codigo, Produtos[i].Descricao]), i, Total);
    end;
    if LTXTeclas.Count > 0 then
      stlTeclado.Text := LTXTeclas.Text + stlTeclado.Text;
  finally
    LTXTeclas.Free;
  end;
end;

procedure TACBrCargaBal.PreencherUrano(Arquivo: TStringList);
var
  i, Total, xtam: Integer;
  xnutric: string;
begin
  //modelo do arquivo: serve somente para as novas balanças urano (linha top e topmax)
  //0x10+0x02+codigo[5]+pesagem[35]+chksum[4]+0x03+0x13+0x10
                //      pesagem[35]=tipoproduto[1]+descricao[20]+preco[9]+validade[4]+tipovalidade[1]

  Total := Produtos.Count;

  for i := 0 to Total - 1 do
  begin
    //linha do produto
    Arquivo.Add(#10#02 +
      LFIll(Produtos[i].Codigo, 5) +
      GetTipoProdutoUrano(Produtos[i].Tipo) +
      RFIll(Produtos[i].Descricao, 20) +
      FormatCurr('000000.00', Produtos[i].ValorVenda) +
      LFIll(Produtos[i].Validade, 4) +
      'D'
    );
    xtam := CalcularSoma(Arquivo[Arquivo.Count-1]);
    Arquivo[Arquivo.Count-1] := Arquivo[Arquivo.Count-1] + IntToHex(xtam, 4) + #03;

    if Produtos[i].Nutricional.FQtd>0 then
    begin
      //linha da informação nutricional
      //0x11+0x02+codigo[5]+pesagem[35]+informacoes nutricionais[258]+chksum[4]+0x03+0x13+0x10
                                      //informacoes nutricionais[258]=1 linha de 41 caracteres para porção
                                      // e 8 linhas de 21 caracteres para [calorias,carboidratos,proteínas,gorduras totais,
                                      //                                   gorduras saturadas,gordura trans,fibra alimentar,
                                      //                                   sódio].

      Arquivo.Add(#11#02 +
        LFIll(Produtos[i].Codigo, 5) +
        GetTipoProdutoUrano(Produtos[i].Tipo) +
        RFIll(Produtos[i].Descricao, 20) +
        FormatCurr('000000.00', Produtos[i].ValorVenda) +
        LFIll(Produtos[i].Validade, 4) +
        'D' +
        RFIll('' {Produtos[i].Nutricional.FUndPorcao}, 209)
        );

      xtam := CalcularSoma(Arquivo[Arquivo.Count-1]);
      Arquivo[Arquivo.Count-1] := Arquivo[Arquivo.Count-1] + IntToHex(xtam, 4) + #03;
    end;

    if Produtos[i].InformacaoExtra.Receita <> '' then
    begin
    //linha da receita
    //0x12+0x02+codigo[5]+pesagem[35]+informacoes adicionais[615]+chksum[4]+0x03+0x13+0x10
                                     //informacoes adicionais[615]=15 linhas de 41 caracteres.
      xnutric:='';

      if Produtos[i].Nutricional.FQtd<=0 then
        xnutric := RFill('', 209);

      Arquivo.Add(#12#02 +
        LFIll(Produtos[i].Codigo, 5) +
        GetTipoProdutoUrano(Produtos[i].Tipo) +
        RFIll(Produtos[i].Descricao, 20) +
        FormatCurr('000000.00', Produtos[i].ValorVenda) +
        LFIll(Produtos[i].Validade, 4) +
        'D' +
        xnutric +
        RFIll(Produtos[i].InformacaoExtra.Receita, 615)
        );

      xtam := CalcularSoma(Arquivo[Arquivo.Count-1]);
      Arquivo[Arquivo.Count-1] := Arquivo[Arquivo.Count-1] + IntToHex(xtam, 4) + #03;
    end;

    Progresso(Format('Gerando produto %6.6d %s', [Produtos[i].Codigo, Produtos[i].Descricao]), i, Total);
  end;
end;

procedure TACBrCargaBal.PreencherUranoS(Arquivo: TStringList);
var
  i, Total, xtam: Integer;
  xnutric: string;
  inform : string;
begin
  //modelo do arquivo: serve somente para as novas balanças urano (linha top e topmax)
  //0x10+0x02+codigo[5]+pesagem[35]+chksum[4]+0x03+0x13+0x10
                //      pesagem[35]=tipoproduto[1]+descricao[20]+preco[9]+validade[4]+tipovalidade[1]

  DecimalSeparator := '.';
  Total := Produtos.Count;

  for i := 0 to Total - 1 do
  begin
    //linha do produto
    inform := LFIll(Produtos[i].Codigo, 6) +
    GetTipoProdutoUrano(Produtos[i].Tipo) +
    RFIll(Produtos[i].Descricao, 30) +
    FormatCurr('000000.00', Produtos[i].ValorVenda) +
    LFIll(Produtos[i].Validade, 4) + 'D0000                            ';
    xtam := CalcularSoma(inform);
    Arquivo.Add( ^P + ^B + inform + LowerCase(IntToHex(xtam, 4)) + ^C);

    if Produtos[i].Nutricional.FQtd>0 then
    begin
      //linha da informação nutricional
      //0x11+0x02+codigo[5]+pesagem[35]+informacoes nutricionais[258]+chksum[4]+0x03+0x13+0x10
                                      //informacoes nutricionais[258]=1 linha de 41 caracteres para porção
                                      // e 8 linhas de 21 caracteres para [calorias,carboidratos,proteínas,gorduras totais,
                                      //                                   gorduras saturadas,gordura trans,fibra alimentar,
                                      //                                   sódio].

      Arquivo.Add(#11#02 +
        LFIll(Produtos[i].Codigo, 5) +
        GetTipoProdutoUrano(Produtos[i].Tipo) +
        RFIll(Produtos[i].Descricao, 20) +
        FormatCurr('000000.00', Produtos[i].ValorVenda) +
        LFIll(Produtos[i].Validade, 4) +
        'D' +
        RFIll('' {Produtos[i].Nutricional.FUndPorcao}, 209)
        );

      xtam := CalcularSoma(Arquivo[Arquivo.Count-1]);
      Arquivo[Arquivo.Count-1] := Arquivo[Arquivo.Count-1] + IntToHex(xtam, 4) + #03;
    end;

    if Produtos[i].InformacaoExtra.Receita <> '' then
    begin
    //linha da receita
    //0x12+0x02+codigo[5]+pesagem[35]+informacoes adicionais[615]+chksum[4]+0x03+0x13+0x10
                                    //informacoes adicionais[615]=15 linhas de 41 caracteres.
      xnutric:='';


      if Produtos[i].Nutricional.FQtd<=0 then
        xnutric := RFill('', 209);

      Arquivo.Add(#12#02 +
        LFIll(Produtos[i].Codigo, 5) +
        GetTipoProdutoUrano(Produtos[i].Tipo) +
        RFIll(Produtos[i].Descricao, 20) +
        FormatCurr('000000.00', Produtos[i].ValorVenda) +
        LFIll(Produtos[i].Validade, 4) +
        'D' +
        xnutric +
        RFIll(Produtos[i].InformacaoExtra.Receita, 615)
        );

      xtam := CalcularSoma(Arquivo[Arquivo.Count-1]);
      Arquivo[Arquivo.Count-1] := Arquivo[Arquivo.Count-1] + IntToHex(xtam, 4) + #03;
    end;

    Progresso(Format('Gerando produto %6.6d %s', [Produtos[i].Codigo, Produtos[i].Descricao]), i, Total);
  end;
end;

procedure TACBrCargaBal.PreencherUranoURF32(stlArquivo, stlNutricional, stlReceita, stlRelacaoProdutoNutricional, stlRelacaoProdutoReceita: TStringList);
var
  i, iTotal: Integer;
  sReceita:string;
  function TrocaPontoPorVirgula(sString: String): String;
  var
    iPos :Integer;
    sRetorno :String;
  begin
    sRetorno := '';
    for iPos := 1 to Length(sString) do
      begin
        if sString[iPos] <> '.' then
          sRetorno := sRetorno + sString[iPos]
        else
          sRetorno := sRetorno + ',';
      end;
     Result := sRetorno;
  end;

  function ValorComVirgula(nValor:Currency):string;
  var
    sTemp:string;
  begin
    sTemp := LFIll(FormatFloat('###,###,##0.00', nValor), 9, ' ');
    Result := TrocaPontoPorVirgula(sTemp);
  end;

begin
  iTotal := Produtos.Count;

  for i := 0 to iTotal - 1 do
    begin
      stlArquivo.Add(LFIll(Produtos[i].Codigo, 6, ' ') +          // Código
                  '*' +                                        // Flag * para transmitir
                  GetTipoProdutoUranoURF32(Produtos[i].Tipo) + // Tipo
                  RFIll(Produtos[i].Descricao, 20, ' ') +      // Nome
                  ValorComVirgula(Produtos[i].ValorVenda) +    // Preço
                  LFIll(Produtos[i].Validade, 5, ' ') +        // Validade
                  LFIll(GetTipoValidadeProdutoUranoURF32(Produtos[i].TipoValidade), 1)   // Validade 2 - Indica se é M=Meses ou D=Dias
                  );

      // Informações Nutricionais
      stlNutricional.Add(LFIll(Produtos[i].Codigo,5, ' ') +
                      LFIll(Produtos[i].Nutricional.Qtd,6) +
                      LFIll(Produtos[i].Nutricional.ValorEnergetico,5) +
                      LFIll(Produtos[i].Nutricional.Carboidrato,6,1) +
                      LFIll(Produtos[i].Nutricional.Proteina,6,1) +
                      LFIll(Produtos[i].Nutricional.GorduraTotal,6,1) +
                      LFIll(Produtos[i].Nutricional.GorduraSaturada,6,1)+
                      LFIll(Produtos[i].Nutricional.GorduraTrans,6,1) +
                      LFIll(Produtos[i].Nutricional.Fibra,6,1)+
                      LFIll(Produtos[i].Nutricional.Sodio,5)
                      );

      if Produtos[i].Nutricional.Qtd > 0 then
        begin
          stlRelacaoProdutoNutricional.Add(LFIll(Produtos[i].Codigo,6, ' ') +
                                        LFIll(Produtos[i].Codigo,5, ' ')
                                        );
        end;

      // Receita
      sReceita := LFIll(Produtos[i].Codigo,4) +
                  ' ' + // Flag * para transmitir
                  RFill(Produtos[i].InformacaoExtra.Receita,254);

      if (Length(Produtos[i].InformacaoExtra.Receita) > 2) and (stlReceita.IndexOf(sReceita) < 0) then
        begin
          stlReceita.Add(sReceita);
          stlRelacaoProdutoReceita.Add(LFIll(Produtos[i].Codigo,6, ' ') +
                                    LFIll(Produtos[i].Codigo,5, ' ')
                                        );

        end;

      Progresso(Format('Gerando produto %6.6d %s', [Produtos[i].Codigo, Produtos[i].Descricao]), i, iTotal);
    end;
end;

procedure TACBrCargaBal.Progresso(const AMensagem: String; const AContAtual,
  AContTotal: Integer);
begin
  if Assigned(FOnProgresso) then
    FOnProgresso(AMensagem, AContAtual, AContTotal);
end;

procedure TACBrCargaBal.GerarArquivos(const ADiretorio: String);
var
  Produto, Setor, Receita, Nutricional, Tara, Fornecedor, Fracionador, Conservacao, Extra1, Extra2: TStringList;
  RelacaoProdutoNutricional, RelacaoProdutoReceita, Teclado: TStringList;
  NomeArquivo: TFileName;
  Total: integer;
begin
  if Trim(ADiretorio) = EmptyStr then
    raise EACBrCargaBal.Create(ACBrStr('Informe o diretório onde serão gerados os arquivos de carga!'));

  if not DirectoryExists(ADiretorio) then
    raise EACBrCargaBal.Create(ACBrStr('Diretorio informado não existe!'));

  if Self.Produtos.Count = 0 then
    raise EACBrCargaBal.Create(ACBrStr('Não foram informados os produtos para a geração!'));

  FArquivosGerados.Clear;

  Produto                   := TStringList.Create;
  Setor                     := TStringList.Create;
  Receita                   := TStringList.Create;
  Tara                      := TStringList.Create;
  Nutricional               := TStringList.Create;
  RelacaoProdutoNutricional := TStringList.Create;
  RelacaoProdutoReceita     := TStringList.Create;
  Fornecedor                := TStringList.Create;
  Fracionador               := TStringList.Create;
  Conservacao               := TStringList.Create;
  Teclado                   := TStringList.Create;
  Extra1                    := TStringList.Create;
  Extra2                    := TStringList.Create;
  try
    Total := Self.Produtos.Count;
    Progresso(ACBrStr('Iniciando a geração dos arquivos'), 0, Total);

    // Varre os registros gerando o arquivo em lista
    case FModelo of
      modFilizola   : PreencherFilizola(Produto, Setor, Nutricional, Receita);
      modToledo     : PreencherToledo(Produto, Nutricional, Receita, Tara, nil, nil, nil, nil, nil, nil, nil, 0);
      modUrano      : PreencherUrano(Produto);
      modUranoS     : PreencherUranoS(Produto);
      modToledoMGV5 : PreencherToledo(Produto, Nutricional, Receita, Tara, Fornecedor, Fracionador, Conservacao, Setor, Teclado, Extra1, Extra2, 2);
      modToledoMGV6 : PreencherToledo(Produto, Nutricional, Receita, Tara, Fornecedor, Fracionador, Conservacao, Setor, Teclado, Extra1, Extra2, 3);
      modToledoMGV7 : PreencherToledoMGV7(Produto, Nutricional, Receita, Tara, Fornecedor, Fracionador, Conservacao, Setor, Teclado, Extra1, Extra2, 4);
      modUranoURF32 : PreencherUranoURF32(Produto, Nutricional, Receita, RelacaoProdutoNutricional, RelacaoProdutoReceita);
      modRamuza     : PreencherRamuza(Produto);
      modToledoMGV5Ver1 : PreencherToledo(Produto, Nutricional, Receita, Tara, Fornecedor, Fracionador, Conservacao, Setor, nil, nil, nil, 1);
    end;

    // Monta o nome do arquivo de produtos seguindo o padrao da balanca
    if Produto.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoProduto;
      Produto.SaveToFile(NomeArquivo);
      FArquivosGerados.Add(NomeArquivo) ;
    end;

    // Gerar arquivo de setores se houverem dados e o arquivo for separado
    if Setor.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoSetor;
      Setor.SaveToFile(NomeArquivo);
      FArquivosGerados.Add(NomeArquivo) ;
    end;

    // Gerar arquivo de receitas se houverem dados e o arquivo for separado
    if Receita.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoReceita;
      Receita.SaveToFile(NomeArquivo);
      FArquivosGerados.Add(NomeArquivo) ;
    end;

    // Gerar arquivo de Nutricionais se houverem dados e o arquivo for separado
    if Nutricional.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoNutricional;
      Nutricional.SaveToFile(NomeArquivo);
      FArquivosGerados.Add(NomeArquivo) ;
    end;

    // Gerar arquivo de relação entre Produto e Nutricionais se houverem dados e o arquivo for separado
    if RelacaoProdutoNutricional.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoRelacaoProdutoNutricional;
      RelacaoProdutoNutricional.SaveToFile(NomeArquivo);
      FArquivosGerados.Add(NomeArquivo) ;
    end;

    // Gerar arquivo de relção entre Produto e Receita se houverem dados e o arquivo for separado
    if RelacaoProdutoReceita.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoRelacaoProdutoReceita;
      RelacaoProdutoReceita.SaveToFile(NomeArquivo);
      FArquivosGerados.Add(NomeArquivo) ;
    end;

    //Gerar arquivo de taras(peso de embalagens)
    if Tara.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoTaras;
      Tara.SaveToFile(NomeArquivo);
      FArquivosGerados.Add(NomeArquivo) ;
    end;

    //Gerar arquivo de fornecedor
    if Fornecedor.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoFornecedor;
      Fornecedor.SaveToFile(NomeArquivo);
      FArquivosGerados.Add(NomeArquivo) ;
    end;

    //Gerar arquivo de fracionador
    if Fracionador.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoFracionador;
      Fracionador.SaveToFile(NomeArquivo);
      FArquivosGerados.Add(NomeArquivo) ;
    end;

    //Gerar arquivo Campext1.txt
    if Extra1.Count>0 then begin
      NomeArquivo:=IncludeTrailingPathDelimiter(Adiretorio)+GetNomeArquivoExtra1;
      Extra1.SaveToFile(NomeArquivo);
      FArquivosGerados.Add(NomeArquivo);
    end;

    //Gerar arquivo Campext2.txt
    if Extra2.Count>0 then begin
      NomeArquivo:=IncludeTrailingPathDelimiter(ADiretorio)+GetNomeArquivoExtra2;
      Extra2.SaveToFile(NomeArquivo);
      FArquivosGerados.Add(NomeArquivo);
    end;

    //Gerar arquivo de conservacao
    if Conservacao.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoConservacao;
      Conservacao.SaveToFile(NomeArquivo);
      FArquivosGerados.Add(NomeArquivo) ;
    end;

    if Teclado.Count > 0 then
    begin
      NomeArquivo := IncludeTrailingPathDelimiter(ADiretorio) + GetNomeArquivoTeclado;
      Teclado.SaveToFile(NomeArquivo);
      FArquivosGerados.Add(NomeArquivo) ;
    end;

    Progresso('Terminado', Total, Total);
  finally
    Produto.Free;
    Setor.Free;
    Receita.Free;
    Tara.Free;
    Nutricional.Free;
    RelacaoProdutoNutricional.Free;
    RelacaoProdutoReceita.Free;
    Fornecedor.Free;
    Fracionador.Free;
    Conservacao.Free;
    Teclado.Free;
    Extra1.Free;
    Extra2.Free;
  end;
end;

function TACBrCargaBal.GetModeloStr: string;
begin
 case fModelo of
   modFilizola   : result := 'Filizola';
   modToledo     : result := 'Toledo';
   modUrano      : result := 'Urano';
   modUranoS     : result := 'Urano (S)';
   modToledoMGV5 : result := 'Toledo MGV5';
   modToledoMGV6 : result := 'Toledo MGV6';
   modToledoMGV7 : result := 'Toledo MGV7';
   modUranoURF32 : result := 'Urano URF32';
   modRamuza     : result := 'Ramuza';
 end;
end;

function TACBrCargaBal.GetTipoValidadeProdutoUranoURF32(
  Tipo: TACBrCargaBalTipoValidade): string;
begin
  case Tipo of
    tpvDias    : Result := 'D';
    tpvMeses   : Result := 'M';
  end;
end;

function TACBrCargaBal.GetNomeArquivoRelacaoProdutoNutricional: String;
begin
  case FModelo of
    modUranoURF32: Result := 'PRODUTOSINFORMACOESNUTRICIONAIS.TXT';
  end;
end;

function TACBrCargaBal.GetNomeArquivoRelacaoProdutoReceita: String;
begin
  case FModelo of
    modUranoURF32: Result := 'PRODUTOSRECEITAS.TXT';
  end;
end;

function TACBrCargaBal.LFIll(Valor: Boolean; Caracter: Char='0'): string;
begin
  if Valor then
    Result:=LFill(IntToStr(1), 1, Caracter)
  else
    Result:=LFill(IntToStr(0), 1, Caracter);
end;

{ TACBrCargaBalFornecedor }

constructor TACBrCargaBalFornecedor.Create;
begin
  FCodigo := 0;
  FObservacao := EmptyStr;
  FDescricao1 := EmptyStr;
  FDescricao2 := EmptyStr;
  FDescricao3 := EmptyStr;
  FDescricao4 := EmptyStr;
  FDescricao5 := EmptyStr;
end;

{ TACBrCargaBalFracionador }

constructor TACBrCargaBalFracionador.Create;
begin
  FCodigo := 0;
  FObservacao := EmptyStr;
  FDescricao1 := EmptyStr;
  FDescricao2 := EmptyStr;
  FDescricao3 := EmptyStr;
end;

{ TACBrCargaBalConservacao }

constructor TACBrCargaBalConservacao.Create;
begin
  FCodigo := 0;
  FObservacao := EmptyStr;
  FDescricao1 := EmptyStr;
  FDescricao2 := EmptyStr;
  FDescricao3 := EmptyStr;
end;

{ TACBrCargaBalInformacaoExtras }

constructor TACBrCargaBalInformacaoExtra.Create;
begin
  inherited;
  fCodigo     := 0;
  FReceita    := EmptyStr;
  FObservacao := EmptyStr;
end;

{ TAcbrCargaBalTecla }

constructor TAcbrCargaBalTeclado.create;
begin
  fCodTeclado := 1; //  Padrao por causa da toledo e pensando para que os outros programadores nao precisem mandar caso nao tenham
  fPaginaTeclado := 1; // Padrao por causa da toledo
  fTecla := 0;
end;

{ TACBrCargaBalExtra1 }

constructor TACBrCargaBalExtra1.Create;
begin
  FCodigo := 0;
  FObservacao := EmptyStr;
  FLinha1:=EmptyStr;
  FLinha2:=EmptyStr;
  FLinha3:=EmptyStr;
  FLinha4:=EmptyStr;
  FLinha5:=EmptyStr;
end;

procedure TACBrCargaBalExtra1.SetLinha1(const Value: String);
begin
  FLinha1 := Value;
end;

procedure TACBrCargaBalExtra1.SetLinha2(const Value: String);
begin
  FLinha2 := Value;
end;

procedure TACBrCargaBalExtra1.SetLinha3(const Value: String);
begin
  FLinha3 := Value;
end;

procedure TACBrCargaBalExtra1.SetLinha4(const Value: String);
begin
  FLinha4 := Value;
end;

procedure TACBrCargaBalExtra1.SetLinha5(const Value: String);
begin
  FLinha5 := Value;
end;

{ TACBrCargaBalExtra2 }

constructor TACBrCargaBalExtra2.Create;
begin
  FCodigo:=0;
  FObservacao:=EmptyStr;
  FLinha1:=EmptyStr;
  FLinha2:=EmptyStr;
  FLinha3:=EmptyStr;
  FLinha4:=EmptyStr;
  FLinha5:=EmptyStr;
end;

procedure TACBrCargaBalExtra2.SetCodigo(const Value: Smallint);
begin
  FCodigo := Value;
end;

procedure TACBrCargaBalExtra2.SetLinha1(const Value: String);
begin
  FLinha1 := Value;
end;

procedure TACBrCargaBalExtra2.SetLinha2(const Value: String);
begin
  FLinha2 := Value;
end;

procedure TACBrCargaBalExtra2.SetLinha3(const Value: String);
begin
  FLinha3 := Value;
end;

procedure TACBrCargaBalExtra2.SetLinha4(const Value: String);
begin
  FLinha4 := Value;
end;

procedure TACBrCargaBalExtra2.SetLinha5(const Value: String);
begin
  FLinha5 := Value;
end;

procedure TACBrCargaBalExtra2.SetObservacao(const Value: String);
begin
  FObservacao := Value;
end;

end.
