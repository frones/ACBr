{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrNFSeXClass;

interface

uses
  SysUtils, Classes,
  {$IFNDEF VER130}
    Variants,
  {$ENDIF}
  {$IF DEFINED(NEXTGEN)}
   System.Generics.Collections, System.Generics.Defaults,
  {$ELSEIF DEFINED(DELPHICOMPILER16_UP)}
   System.Contnrs,
  {$IFEND}
  ACBrBase,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrNFSeXConversao;

type

  TInfID = class(TObject)
  private
    FID: string;
  public
    property ID: string read FID write FID;
  end;

  TIdentificacaoRps = class(TObject)
  private
    FNumero: string;
    FSerie: string;
    FTipo: TTipoRps;
  public
    property Numero: string read FNumero write FNumero;
    property Serie: string read FSerie write FSerie;
    property Tipo: TTipoRps read FTipo write FTipo;
  end;

  TIdentificacaoNfse = class(TObject)
  private
    FNumero: string;
    FCnpj: string;
    FInscricaoMunicipal: string;
    FCodigoMunicipio: string;
  public
    property Numero: string read FNumero write FNumero;
    property Cnpj: string read FCnpj write FCnpj;
    property InscricaoMunicipal: string read FInscricaoMunicipal write FInscricaoMunicipal;
    property CodigoMunicipio: string read FCodigoMunicipio write FCodigoMunicipio;
  end;

  TValoresNfse = class(TObject)
  private
    FBaseCalculo: Double;
    FAliquota: Double;
    FValorIss: Double;
    FValorLiquidoNfse: Double;
    FvCalcDR: Double;
    FtpBM: string;
    FvCalcBM: Double;
    FvTotalRet: Double;
  public
    property BaseCalculo: Double read FBaseCalculo write FBaseCalculo;
    property Aliquota: Double read FAliquota write FAliquota;
    property ValorIss: Double read FValorIss write FValorIss;
    property ValorLiquidoNfse: Double read FValorLiquidoNfse write FValorLiquidoNfse;
    // Provedor PadraoNacional
    property vCalcDR: Double read FvCalcDR write FvCalcDR;
    property tpBM: string read FtpBM write FtpBM;
    property vCalcBM: Double read FvCalcBM write FvCalcBM;
    property vTotalRet: Double read FvTotalRet write FvTotalRet;
  end;

  TNFSeMun = class(TObject)
  private
    FcMunNFSeMun: string;
    FnNFSeMun: string;
    FcVerifNFSeMun: string;
  public
    property cMunNFSeMun: string read FcMunNFSeMun write FcMunNFSeMun;
    property nNFSeMun: string read FnNFSeMun write FnNFSeMun;
    property cVerifNFSeMun: string read FcVerifNFSeMun write FcVerifNFSeMun;
  end;

  TNFNFS = class(TObject)
  private
    FnNFS: string;
    FmodNFS: string;
    FserieNFS: string;
  public
    property nNFS: string read FnNFS write FnNFS;
    property modNFS: string read FmodNFS write FmodNFS;
    property serieNFS: string read FserieNFS write FserieNFS;
  end;

  TIdentificacao = class(TObject)
  private
    FCpfCnpj: string;
    FInscricaoMunicipal: string;
    FInscricaoEstadual: string;
    FDocEstrangeiro: string;
    FTipo: TTipoPessoa;
    FNif: string;
    FCAEPF: string;

    function GetCnpj: string;
    procedure SetCnpj(const Value: string);
  public
    property Cnpj: string read GetCnpj write SetCnpj;
    property CpfCnpj: string read FCpfCnpj write FCpfCnpj;
    property InscricaoMunicipal: string read FInscricaoMunicipal write FInscricaoMunicipal;
    property InscricaoEstadual: string read FInscricaoEstadual write FInscricaoEstadual;
    property DocEstrangeiro: string read FDocEstrangeiro write FDocEstrangeiro;
    property Tipo: TTipoPessoa read FTipo write FTipo;
    property Nif: string read FNif write FNif;
    property CAEPF: string read FCAEPF write FCAEPF;
  end;

  TContato = class(TObject)
  private
    FTelefone: string;
    FEmail: string;
    FDDD: string;
    FTipoTelefone: string;
    FxSite: string;
  public
    property Telefone: string read FTelefone write FTelefone;
    property Email: string read FEmail write FEmail;
    property DDD: string read FDDD write FDDD;
    property TipoTelefone: string read FTipoTelefone write FTipoTelefone;
    property xSite: string read FxSite write FxSite;
  end;

  TEndereco = class(TObject)
  private
    FEnderecoInformado: string;
    FTipoLogradouro: string;
    FEndereco: string;
    FNumero: string;
    FComplemento: string;
    FTipoBairro: string;
    FBairro: string;
    FCodigoMunicipio: string;
    FUF: string;
    FCEP: string;
    FxMunicipio: string;
    FCodigoPais: Integer;
    FxPais: string;
  public
    property EnderecoInformado: string read FEnderecoInformado write FEnderecoInformado;
    property TipoLogradouro: string read FTipoLogradouro write FTipoLogradouro;
    property Endereco: string read FEndereco write FEndereco;
    property Numero: string read FNumero write FNumero;
    property Complemento: string read FComplemento write FComplemento;
    property TipoBairro: string read FTipoBairro write FTipoBairro;
    property Bairro: string read FBairro write FBairro;
    property CodigoMunicipio: string read FCodigoMunicipio write FCodigoMunicipio;
    property UF: string read FUF write FUF;
    property CEP: string read FCEP write FCEP;
    property xMunicipio: string read FxMunicipio write FxMunicipio;
    property CodigoPais: Integer read FCodigoPais write FCodigoPais;
    property xPais: string read FxPais write FxPais;
  end;

  TInfoPessoa = class(TObject)
  private
    FIdentificacao: TIdentificacao;
    FRazaoSocial: string;
    FEndereco: TEndereco;
    FContato: TContato;
  public
    constructor Create;
    destructor Destroy; override;

    property Identificacao: TIdentificacao read FIdentificacao write FIdentificacao;
    property RazaoSocial: string read FRazaoSocial write FRazaoSocial;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property Contato: TContato read FContato write FContato;
  end;

  TDocDeducaoCollectionItem = class(TObject)
  private
    FchNFSe: string;
    FchNFe: string;
    FNFSeMun: TNFSeMun;
    FNFNFS: TNFNFS;
    FnDocFisc: string;
    FnDoc: string;
    FtpDedRed: TtpDedRed;
    FxDescOutDed: string;
    FdtEmiDoc: TDateTime;
    FvDedutivelRedutivel: Double;
    FvDeducaoReducao: Double;
    Ffornec: TInfoPessoa;
  public
    constructor Create;
    destructor Destroy; override;

    property chNFSe: string read FchNFSe write FchNFSe;
    property chNFe: string read FchNFe write FchNFe;
    property NFSeMun: TNFSeMun read FNFSeMun write FNFSeMun;
    property NFNFS: TNFNFS read FNFNFS write FNFNFS;
    property nDocFisc: string read FnDocFisc write FnDocFisc;
    property nDoc: string read FnDoc write FnDoc;
    property tpDedRed: TtpDedRed read FtpDedRed write FtpDedRed;
    property xDescOutDed: string read FxDescOutDed write FxDescOutDed;
    property dtEmiDoc: TDateTime read FdtEmiDoc write FdtEmiDoc;
    property vDedutivelRedutivel: Double read FvDedutivelRedutivel write FvDedutivelRedutivel;
    property vDeducaoReducao: Double read FvDeducaoReducao write FvDeducaoReducao;
    property fornec: TInfoPessoa read Ffornec write Ffornec;
  end;

  TDocDeducaoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDocDeducaoCollectionItem;
    procedure SetItem(Index: Integer; Value: TDocDeducaoCollectionItem);
  public
    function Add: TDocDeducaoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDocDeducaoCollectionItem;
    property Items[Index: Integer]: TDocDeducaoCollectionItem read GetItem write SetItem; default;
  end;

  TtribMun = class(TObject)
  private
    FtribISSQN: TtribISSQN;
    FcPaisResult: Integer;
    FtpBM: TtpBM;
    FnBM: string;
    FvRedBCBM: Double;
    FpRedBCBM: Double;
    FtpSusp: TtpSusp;
    FnProcesso: string;
    FtpImunidade: TtpImunidade;
    FpAliq: Double;
    FtpRetISSQN: TtpRetISSQN;
  public
    property tribISSQN: TtribISSQN read FtribISSQN write FtribISSQN;
    property cPaisResult: Integer read FcPaisResult write FcPaisResult;
    property tpBM: TtpBM read FtpBM write FtpBM;
    property nBM: string read FnBM write FnBM;
    property vRedBCBM: Double read FvRedBCBM write FvRedBCBM;
    property pRedBCBM: Double read FpRedBCBM write FpRedBCBM;
    property tpSusp: TtpSusp read FtpSusp write FtpSusp;
    property nProcesso: string read FnProcesso write FnProcesso;
    property tpImunidade: TtpImunidade read FtpImunidade write FtpImunidade;
    property pAliq: Double read FpAliq write FpAliq;
    property tpRetISSQN: TtpRetISSQN read FtpRetISSQN write FtpRetISSQN;
  end;

  TtribFed = class(TObject)
  private
    FCST: TCST;
    FvBCPisCofins: Double;
    FpAliqPis: Double;
    FpAliqCofins: Double;
    FvPis: Double;
    FvCofins: Double;
    FtpRetPisCofins: TtpRetPisCofins;
    FvRetCP: Double;
    FvRetIRRF: Double;
    FvRetCSLL: Double;
  public
    property CST: TCST read FCST write FCST;
    property vBCPisCofins: Double read FvBCPisCofins write FvBCPisCofins;
    property pAliqPis: Double read FpAliqPis write FpAliqPis;
    property pAliqCofins: Double read FpAliqCofins write FpAliqCofins;
    property vPis: Double read FvPis write FvPis;
    property vCofins: Double read FvCofins write FvCofins;
    property tpRetPisCofins: TtpRetPisCofins read FtpRetPisCofins write FtpRetPisCofins;
    property vRetCP: Double read FvRetCP write FvRetCP;
    property vRetIRRF: Double read FvRetIRRF write FvRetIRRF;
    property vRetCSLL: Double read FvRetCSLL write FvRetCSLL;
  end;

  TtotTrib = class(TObject)
  private
    FvTotTribFed: Double;
    FvTotTribEst: Double;
    FvTotTribMun: Double;
    FpTotTribFed: Double;
    FpTotTribEst: Double;
    FpTotTribMun: Double;
    FindTotTrib: TindTotTrib;
    FpTotTribSN: Double;
  public
    property vTotTribFed: Double read FvTotTribFed write FvTotTribFed;
    property vTotTribEst: Double read FvTotTribEst write FvTotTribEst;
    property vTotTribMun: Double read FvTotTribMun write FvTotTribMun;
    property pTotTribFed: Double read FpTotTribFed write FpTotTribFed;
    property pTotTribEst: Double read FpTotTribEst write FpTotTribEst;
    property pTotTribMun: Double read FpTotTribMun write FpTotTribMun;
    property indTotTrib: TindTotTrib read FindTotTrib write FindTotTrib;
    property pTotTribSN: Double read FpTotTribSN write FpTotTribSN;
  end;

  TValores = class(TObject)
  private
    FValorServicos: Double;
    FValorDeducoes: Double;
    FValorPis: Double;
    FValorCofins: Double;
    FValorInss: Double;
    FValorIr: Double;
    FValorCsll: Double;
    FValorCpp: Double; //Elotech
    FIssRetido: TnfseSituacaoTributaria;
    FValorIss: Double;
    FOutrasRetencoes: Double;
    FBaseCalculo: Double;
    FAliquota: Double;
    FAliquotaSN: Double; // Aliquota usada pelo Provedor Conam
    FAliquotaPis: Double;
    FAliquotaCofins: Double;
    FAliquotaInss: Double;
    FAliquotaIr: Double;
    FAliquotaCsll: Double;
    FAliquotaCpp: Double; // Aliquota usada pelo Provedor Elotech
    FOutrosDescontos: Double;
    FValorLiquidoNfse: Double;
    FValorIssRetido: Double;
    FDescontoCondicionado: Double;
    FDescontoIncondicionado: Double;
    FJustificativaDeducao: string;
    FvalorOutrasRetencoes: Double;
    FDescricaoOutrasRetencoes: string;
    FvalorRepasse: Double; // Governa
    FValorDespesasNaoTributaveis: Double; // Governa
    FValorTotalRecebido: Double;
    FValorTotalTributos: Double;
    FIrrfIndenizacao: Double;
    FRetidoPis: TnfseSimNao;
    FRetidoCofins: TnfseSimNao;
    FRetidoInss: TnfseSimNao;
    FRetidoIr: TnfseSimNao;
    FRetidoCsll: TnfseSimNao;
    FRetidoCpp: TnfseSimNao; // Elotech
    FQtdeDiaria: Double;
    FValorTaxaTurismo: Double;
    FValorRecebido: Double;
    FAliquotaDeducoes: Double;
    FDocDeducao: TDocDeducaoCollection;
    FtribMun: TtribMun;
    FtribFed: TtribFed;
    FtotTrib: TtotTrib;
    FTipoDeducao: TTipoDeducao;
    FRetencoesFederais: Double;
    FValorTotalNotaFiscal: Double;

    procedure SetDocDeducao(const Value: TDocDeducaoCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property ValorServicos: Double read FValorServicos write FValorServicos;
    property ValorDeducoes: Double read FValorDeducoes write FValorDeducoes;
    property ValorPis: Double read FValorPis write FValorPis;
    property ValorCofins: Double read FValorCofins write FValorCofins;
    property ValorInss: Double read FValorInss write FValorInss;
    property ValorIr: Double read FValorIr write FValorIr;
    property ValorCsll: Double read FValorCsll write FValorCsll;
    property ValorCpp: Double read FValorCpp write FValorCpp;
    property IssRetido: TnfseSituacaoTributaria read FIssRetido write FIssRetido;
    property ValorIss: Double read FValorIss write FValorIss;
    property OutrasRetencoes: Double read FOutrasRetencoes write FOutrasRetencoes;
    property BaseCalculo: Double read FBaseCalculo write FBaseCalculo;
    property Aliquota: Double read FAliquota write FAliquota;
    // Aliquota usada pelo Provedor Conam
    property AliquotaSN: Double read FAliquotaSN write FAliquotaSN;
    // Aliquotas usadas pelo Provedor IssDsf
    property AliquotaPis: Double read FAliquotaPis write FAliquotaPis;
    property AliquotaCofins: Double read FAliquotaCofins write FAliquotaCofins;
    property AliquotaInss: Double read FAliquotaInss write FAliquotaInss;
    property AliquotaIr: Double read FAliquotaIr write FAliquotaIr;
    property AliquotaCsll: Double read FAliquotaCsll write FAliquotaCsll;
    property AliquotaCpp: Double read FAliquotaCpp write FAliquotaCpp;
    // Usado pelo Provedor EL
    property OutrosDescontos: Double read FOutrosDescontos write FOutrosDescontos;

    property ValorLiquidoNfse: Double read FValorLiquidoNfse write FValorLiquidoNfse;
    property ValorIssRetido: Double read FValorIssRetido write FValorIssRetido;
    property DescontoCondicionado: Double read FDescontoCondicionado write FDescontoCondicionado;
    property DescontoIncondicionado: Double read FDescontoIncondicionado write FDescontoIncondicionado;
    //Just. usada pelo provedor Equiplano
    property JustificativaDeducao: string read FJustificativaDeducao write FJustificativaDeducao;
    //propriedade do Provedor Governa
    property valorOutrasRetencoes: Double read FvalorOutrasRetencoes write FvalorOutrasRetencoes;
    property DescricaoOutrasRetencoes: string read FDescricaoOutrasRetencoes write FDescricaoOutrasRetencoes;
    property ValorRepasse: Double read FValorRepasse write FValorRepasse;
    //Provedor Infisc V 11
    property ValorDespesasNaoTributaveis: Double read FValorDespesasNaoTributaveis write FValorDespesasNaoTributaveis;
    //Recife
    property ValorTotalRecebido: Double read FValorTotalRecebido write FValorTotalRecebido;
    //Provedor proSimplISSv2
    property ValorTotalTributos: Double read FValorTotalTributos write FValorTotalTributos;
    //Provedor Tecnos
    property IrrfIndenizacao: Double read FIrrfIndenizacao write FIrrfIndenizacao;
    //Provedor Elotech
    property RetidoPis: TnfseSimNao read FRetidoPis write FRetidoPis;
    property RetidoCofins: TnfseSimNao read FRetidoCofins write FRetidoCofins;
    property RetidoInss: TnfseSimNao read FRetidoInss write FRetidoInss;
    property RetidoIr: TnfseSimNao read FRetidoIr write FRetidoIr;
    property RetidoCsll: TnfseSimNao read FRetidoCsll write FRetidoCsll;
    property RetidoCpp: TnfseSimNao read FRetidoCpp write FRetidoCpp;
    //Provedor SystemPro
    property QtdeDiaria: Double read FQtdeDiaria write FQtdeDiaria;
    property ValorTaxaTurismo: Double read FValorTaxaTurismo write FValorTaxaTurismo;
    //Provedor PadraoNacional
    property ValorRecebido: Double read FValorRecebido write FValorRecebido;
    property AliquotaDeducoes: Double read FAliquotaDeducoes write FAliquotaDeducoes;
    property DocDeducao: TDocDeducaoCollection read FDocDeducao write SetDocDeducao;
    property tribMun: TtribMun read FtribMun write FtribMun;
    property tribFed: TtribFed read FtribFed write FtribFed;
    property totTrib: TtotTrib read FtotTrib write FtotTrib;
    //provedor CTAConsult
    property TipoDeducao: TTipoDeducao read FTipoDeducao write FTipoDeducao;

    property RetencoesFederais: Double read FRetencoesFederais write FRetencoesFederais;
    property ValorTotalNotaFiscal: Double read FValorTotalNotaFiscal write FValorTotalNotaFiscal;
  end;

  TDadosDeducao = class(TObject)
  private
    FvTotTribFed: TTipoDeducao;
    FCpfCnpj: string;
    FNumeroNotaFiscalReferencia: string;
    FValorTotalNotaFiscal: Double;
    FPercentualADeduzir: Double;
    FValorADeduzir: Double;
  public
    property TipoDeducao: TTipoDeducao read FvTotTribFed write FvTotTribFed;
    property CpfCnpj: string read FCpfCnpj write FCpfCnpj;
    property NumeroNotaFiscalReferencia: string read FNumeroNotaFiscalReferencia write FNumeroNotaFiscalReferencia;
    property ValorTotalNotaFiscal: Double read FValorTotalNotaFiscal write FValorTotalNotaFiscal;
    property PercentualADeduzir: Double read FPercentualADeduzir write FPercentualADeduzir;
    property ValorADeduzir: Double read FValorADeduzir write FValorADeduzir;
  end;

  TItemServicoCollectionItem = class(TObject)
  private
    FCodServ: string;
    FCodLCServ: string;
    FItemListaServico: string;
    FxItemListaServico: string;
    FDescricao: string;
    FUnidade: string;
    FTipoUnidade: TUnidade;

    FQuantidade: Double;
    FValorUnitario: Double;
    FValorTotal: Double;
    FBaseCalculo: Double;

    FValorDeducoes: Double;
    FxJustDeducao: string;

    FDescontoCondicionado: Double;
    FDescontoIncondicionado: Double;

    FQtdeDiaria: Double;
    FValorTaxaTurismo: Double;

    FAliqReducao: Double;
    FValorReducao: Double;

    FAliquota: Double;
    FValorISS: Double;
    FValorISSRetido: Double;
    FValorPisRetido: Double;
    FValorCofinsRetido: Double;	
    FValorInssRetido: Double;
    FValorIrRetido: Double;
    FValorCsllRetido: Double;
    FValorCppRetido: Double;

    FAliqISSST: Double;
    FValorISSST: Double;

    FValorBCCSLL: Double;
    FAliqRetCSLL: Double;
    FValorCSLL: Double;

    FValorBCPIS: Double;
    FAliqRetPIS: Double;
    FValorPIS: Double;

    FValorBCCOFINS: Double;
    FAliqRetCOFINS: Double;
    FValorCOFINS: Double;

    FValorBCINSS: Double;
    FAliqRetINSS: Double;
    FValorINSS: Double;

    FValorBCRetIRRF: Double;
    FAliqRetIRRF: Double;
    FValorIRRF: Double;

    FTributavel: TnfseSimNao;
    FCodigoCnae: string;
    FDadosDeducao: TDadosDeducao;

    FTribMunPrestador: TnfseSimNao;
    FCodMunPrestacao: string;
    FSituacaoTributaria: Integer;
    FCodCNO: string;
  public
    constructor Create;
    destructor Destroy; override;

    property CodServ: string read FCodServ write FCodServ;
    property CodLCServ: string read FCodLCServ write FCodLCServ;
    property ItemListaServico: string read FItemListaServico write FItemListaServico;
    property xItemListaServico: string read FxItemListaServico write FxItemListaServico;
    property Descricao: string read FDescricao write FDescricao;
    property Unidade: string read FUnidade write FUnidade;
    property TipoUnidade: TUnidade read FTipoUnidade write FTipoUnidade;

    property Quantidade: Double read FQuantidade write FQuantidade;
    property ValorUnitario: Double read FValorUnitario write FValorUnitario;
    property ValorTotal: Double read FValorTotal write FValorTotal;
    property BaseCalculo: Double read FBaseCalculo write FBaseCalculo;

    property ValorDeducoes: Double read FValorDeducoes write FValorDeducoes;
    property xJustDeducao: string read FxJustDeducao write FxJustDeducao;

    property DescontoCondicionado: Double read FDescontoCondicionado write FDescontoCondicionado;
    property DescontoIncondicionado: Double read FDescontoIncondicionado write FDescontoIncondicionado;

    property QtdeDiaria: Double read FQtdeDiaria write FQtdeDiaria;
    property ValorTaxaTurismo: Double read FValorTaxaTurismo write FValorTaxaTurismo;

    property AliqReducao: Double read FAliqReducao write FAliqReducao;
    property ValorReducao: Double read FValorReducao write FValorReducao;

    property Aliquota: Double read FAliquota write FAliquota;
    property ValorISS: Double read FValorISS write FValorISS;
    property ValorISSRetido: Double read FValorISSRetido write FValorISSRetido;
	property ValorPisRetido: Double read FValorPisRetido write FValorPisRetido;
    property ValorCofinsRetido: Double read FValorCofinsRetido write FValorCofinsRetido;
    property ValorInssRetido: Double read FValorInssRetido write FValorInssRetido;
    property ValorIrRetido: Double read FValorIrRetido write FValorIrRetido;
    property ValorCsllRetido: Double read FValorCsllRetido write FValorCsllRetido;
    property ValorCppRetido: Double read FValorCppRetido write FValorCppRetido;

    property AliqISSST: Double read FAliqISSST write FAliqISSST;
    property ValorISSST: Double read FValorISSST write FValorISSST;

    property ValorBCCSLL: Double read FValorBCCSLL write FValorBCCSLL;
    property AliqRetCSLL: Double read FAliqRetCSLL write FAliqRetCSLL;
    property ValorCSLL: Double read FValorCSLL write FValorCSLL;

    property ValorBCPIS: Double read FValorBCPIS write FValorBCPIS;
    property AliqRetPIS: Double read FAliqRetPIS write FAliqRetPIS;
    property ValorPIS: Double read FValorPIS write FValorPIS;

    property ValorBCCOFINS: Double read FValorBCCOFINS write FValorBCCOFINS;
    property AliqRetCOFINS: Double read FAliqRetCOFINS write FAliqRetCOFINS;
    property ValorCOFINS: Double read FValorCOFINS write FValorCOFINS;

    property ValorBCINSS: Double read FValorBCINSS write FValorBCINSS;
    property AliqRetINSS: Double read FAliqRetINSS write FAliqRetINSS;
    property ValorINSS: Double read FValorINSS write FValorINSS;

    property ValorBCRetIRRF: Double read FValorBCRetIRRF write FValorBCRetIRRF;
    property AliqRetIRRF: Double read FAliqRetIRRF write FAliqRetIRRF;
    property ValorIRRF: Double read FValorIRRF write FValorIRRF;

    // Provedor EloTech
    property Tributavel: TnfseSimNao read FTributavel write FTributavel;
    property CodigoCnae: string read FCodigoCnae write FCodigoCnae;
    property DadosDeducao: TDadosDeducao read FDadosDeducao write FDadosDeducao;

    // Provedor IPM
    property TribMunPrestador: TnfseSimNao read FTribMunPrestador write FTribMunPrestador;
    property CodMunPrestacao: string read FCodMunPrestacao write FCodMunPrestacao;
    property SituacaoTributaria: Integer read FSituacaoTributaria write FSituacaoTributaria;
    property CodCNO: string read FCodCNO write FCodCNO;
  end;

  TItemServicoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TItemServicoCollectionItem;
    procedure SetItem(Index: Integer; Value: TItemServicoCollectionItem);
  public
    function Add: TItemServicoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TItemServicoCollectionItem;
    property Items[Index: Integer]: TItemServicoCollectionItem read GetItem write SetItem; default;
  end;

  // classe usada no provedor IssDSF
  TDeducaoCollectionItem = class(TObject)
  private
    FDeducaoPor: TDeducaoPor;
    FTipoDeducao: TTipoDeducao;
    FCpfCnpjReferencia: string;
    FNumeroNFReferencia: string;
    FValorTotalReferencia: Double;
    FPercentualDeduzir: Double;
    FValorDeduzir: Double;
  public
    property DeducaoPor: TDeducaoPor read FDeducaoPor write FDeducaoPor;
    property TipoDeducao: TTipoDeducao read FTipoDeducao write FTipoDeducao;
    property CpfCnpjReferencia: string read FCpfCnpjReferencia write FCpfCnpjReferencia;
    property NumeroNFReferencia: string read FNumeroNFReferencia write FNumeroNFReferencia;
    property ValorTotalReferencia: Double read FValorTotalReferencia write FValorTotalReferencia;
    property PercentualDeduzir: Double read FPercentualDeduzir write FPercentualDeduzir;
    property ValorDeduzir: Double read FValorDeduzir write FValorDeduzir;
  end;

  TDeducaoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDeducaoCollectionItem;
    procedure SetItem(Index: Integer; Value: TDeducaoCollectionItem);
  public
    function Add: TDeducaoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDeducaoCollectionItem;
    property Items[Index: Integer]: TDeducaoCollectionItem read GetItem write SetItem; default;
  end;

  TComExterior = class(TObject)
  private
    FmdPrestacao: TmdPrestacao;
    FvincPrest: TvincPrest;
    FtpMoeda: Integer;
    FvServMoeda: Double;
    FmecAFComexP: TmecAFComexP;
    FmecAFComexT: TmecAFComexT;
    FmovTempBens: TMovTempBens;
    FnDI: string;
    FnRE: string;
    Fmdic: Integer;
  public
    property mdPrestacao: TmdPrestacao read FmdPrestacao write FmdPrestacao;
    property vincPrest: TvincPrest read FvincPrest write FvincPrest;
    property tpMoeda: Integer read FtpMoeda write FtpMoeda;
    property vServMoeda: Double read FvServMoeda write FvServMoeda;
    property mecAFComexP: TmecAFComexP read FmecAFComexP write FmecAFComexP;
    property mecAFComexT: TmecAFComexT read FmecAFComexT write FmecAFComexT;
    property movTempBens: TMovTempBens read FmovTempBens write FmovTempBens;
    property nDI: string read FnDI write FnDI;
    property nRE: string read FnRE write FnRE;
    property mdic: Integer read Fmdic write Fmdic;
  end;

  TLocacao = class(TObject)
  private
    Fcateg: Tcateg;
    Fobjeto: Tobjeto;
    Fextensao: string;
    FnPostes: Integer;
  public
    property categ: Tcateg read Fcateg write Fcateg;
    property objeto: Tobjeto read Fobjeto write Fobjeto;
    property extensao: string read Fextensao write Fextensao;
    property nPostes: Integer read FnPostes write FnPostes;
  end;

  TEvento = class(TObject)
  private
    Fdesc: string;
    FdtIni: TDateTime;
    FdtFim: TDateTime;
    Fid: string;
    FEndereco: TEndereco;
  public
    constructor Create;
    destructor Destroy; override;

    property desc: string read Fdesc write Fdesc;
    property dtIni: TDateTime read FdtIni write FdtIni;
    property dtFim: TDateTime read FdtFim write FdtFim;
    property id: string read Fid write Fid;
    property Endereco: TEndereco read FEndereco write FEndereco;
  end;

  TExplRod = class(TObject)
  private
    FcategVeic: TcategVeic;
    FnEixos: Integer;
    Frodagem: Trodagem;
    Fsentido: string;
    Fplaca: string;
    FcodAcessoPed: string;
    FcodContrato: string;
  public
    property categVeic: TcategVeic read FcategVeic write FcategVeic;
    property nEixos: Integer read FnEixos write FnEixos;
    property rodagem: Trodagem read Frodagem write Frodagem;
    property sentido: string read Fsentido write Fsentido;
    property placa: string read Fplaca write Fplaca;
    property codAcessoPed: string read FcodAcessoPed write FcodAcessoPed;
    property codContrato: string read FcodContrato write FcodContrato;
  end;

  TinfoCompl = class(TObject)
  private
    FidDocTec: string;
    FdocRef: string;
    FxInfComp: string;
  public
    property idDocTec: string read FidDocTec write FidDocTec;
    property docRef: string read FdocRef write FdocRef;
    property xInfComp: string read FxInfComp write FxInfComp;
  end;

  // classe usada no provedor CTAConsult
  TImpostoCollectionItem = class(TObject)
  private
    FCodigo: Integer;
    FDescricao: string;
    FAliquota: Double;
    FValor: Double;
  public
    property Codigo: Integer read FCodigo write FCodigo;
    property Descricao: string read FDescricao write FDescricao;
    property Aliquota: Double read FAliquota write FAliquota;
    property Valor: Double read FValor write FValor;
  end;

  TImpostoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TImpostoCollectionItem;
    procedure SetItem(Index: Integer; Value: TImpostoCollectionItem);
  public
    function Add: TImpostoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TImpostoCollectionItem;
    property Items[Index: Integer]: TImpostoCollectionItem read GetItem write SetItem; default;
  end;

  TDadosServico = class(TObject)
  private
    FValores: TValores;
    FItemListaServico: string;
    FCodigoCnae: string;
    FCodigoTributacaoMunicipio: string;
    FxCodigoTributacaoMunicipio: string;
    FDiscriminacao: string;
    FCodigoMunicipio: string;
    FMunicipioPrestacaoServico: string;
    FCodigoPais: Integer;
    FExigibilidadeISS: TnfseExigibilidadeISS;
    FMunicipioIncidencia: Integer;
    FNumeroProcesso: string;
    FxItemListaServico: string;
    FItemServico: TItemServicoCollection;
    FResponsavelRetencao: TnfseResponsavelRetencao;
    FDescricao: string;
    // Provedor IssDsf
    FDeducao: TDeducaoCollection;
    FOperacao: TOperacao;
    FTributacao: TTributacao;
    // Provedor Governa
    FUFPrestacao: string;
    // Provedor SP
    FValorCargaTributaria: Double;
    FPercentualCargaTributaria: Double;
    FFonteCargaTributaria: string;
    FValorTotalRecebido: Double;
    // Provedor ISSBarueri
    FPrestadoEmViasPublicas: Boolean;
    // Provedor GeisWeb
    FTipoLancamento: TTipoLancamento;
    FCodigoNBS: string;
    FCodigoInterContr: string;
    FcomExt: TComExterior;
    FLocacao: TLocacao;
    FEvento: TEvento;
    FExplRod: TExplRod;
    FinfoCompl: TinfoCompl;
    FImposto: TImpostoCollection;
    FIdentifNaoExigibilidade: string;
    FxMunicipioIncidencia: string;

    procedure SetItemServico(Value: TItemServicoCollection);
    procedure SetDeducao(const Value: TDeducaoCollection);
    procedure SetImposto(const Value: TImpostoCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property Valores: TValores read FValores write FValores;
    property ItemListaServico: string read FItemListaServico write FItemListaServico;
    property CodigoCnae: string read FCodigoCnae write FCodigoCnae;
    property CodigoTributacaoMunicipio: string read FCodigoTributacaoMunicipio write FCodigoTributacaoMunicipio;
    property xCodigoTributacaoMunicipio: string read FxCodigoTributacaoMunicipio write FxCodigoTributacaoMunicipio;
    property Discriminacao: string read FDiscriminacao write FDiscriminacao;
    property CodigoMunicipio: string read FCodigoMunicipio write FCodigoMunicipio;
    property MunicipioPrestacaoServico: string read FMunicipioPrestacaoServico write FMunicipioPrestacaoServico;
    property CodigoPais: Integer read FCodigoPais write FCodigoPais;
    property ExigibilidadeISS: TnfseExigibilidadeISS read FExigibilidadeISS write FExigibilidadeISS;
    property IdentifNaoExigibilidade: string read FIdentifNaoExigibilidade write FIdentifNaoExigibilidade;
    property MunicipioIncidencia: Integer read FMunicipioIncidencia write FMunicipioIncidencia;
    property NumeroProcesso: string read FNumeroProcesso write FNumeroProcesso;
    property xItemListaServico: string read FxItemListaServico write FxItemListaServico;
    property ItemServico: TItemServicoCollection read FItemServico write SetItemServico;
    property ResponsavelRetencao: TnfseResponsavelRetencao read FResponsavelRetencao write FResponsavelRetencao;
    property Descricao: string read FDescricao write FDescricao;
    property xMunicipioIncidencia: string read FxMunicipioIncidencia write FxMunicipioIncidencia;
    // Provedor IssDsf
    property Deducao: TDeducaoCollection read FDeducao write SetDeducao;
    property Operacao: TOperacao read FOperacao write FOperacao;
    property Tributacao: TTributacao read FTributacao write FTributacao;
    // Provedor Governa
    property UFPrestacao: string read FUFPrestacao write FUFPrestacao;
    // Provedor SP
    property ValorCargaTributaria: Double read FValorCargaTributaria write FValorCargaTributaria;
    property PercentualCargaTributaria: Double read FPercentualCargaTributaria write FPercentualCargaTributaria;
    property FonteCargaTributaria: string read FFonteCargaTributaria write FFonteCargaTributaria;
    property ValorTotalRecebido: Double read FValorTotalRecebido write FValorTotalRecebido;

    // Provedor ISSBarueri
    property PrestadoEmViasPublicas: Boolean read FPrestadoEmViasPublicas write FPrestadoEmViasPublicas;
    // Provedor GeisWeb
    property TipoLancamento: TTipoLancamento read FTipoLancamento write FTipoLancamento;
    // Provedor PadraoNacional
    property CodigoNBS: string read FCodigoNBS write FCodigoNBS;
    property CodigoInterContr: string read FCodigoInterContr write FCodigoInterContr;
    property comExt: TComExterior read FcomExt write FcomExt;
    property Locacao: TLocacao read FLocacao write FLocacao;
    property Evento: TEvento read FEvento write FEvento;
    property ExplRod: TExplRod read FExplRod write FExplRod;
    property infoCompl: TinfoCompl read FinfoCompl write FinfoCompl;
    // Provedor CTAConsult
    property Imposto: TImpostoCollection read FImposto write SetImposto;
  end;

  TDadosPessoa = class(TObject)
  private
    FIdentificacao: TIdentificacao;

    FRazaoSocial: string;
    FNomeFantasia: string;

    FEndereco: TEndereco;
    FContato: TContato;
  public
    constructor Create;
    destructor Destroy; override;

    property Identificacao: TIdentificacao read FIdentificacao write FIdentificacao;
    property RazaoSocial: string read FRazaoSocial write FRazaoSocial;
    property NomeFantasia: string read FNomeFantasia write FNomeFantasia;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property Contato: TContato read FContato write FContato;
  end;

  TDadosPrestador = class(TObject)
  private
    FIdentificacaoPrestador: TIdentificacao;

    FRazaoSocial: string;
    FNomeFantasia: string;

    FEndereco: TEndereco;
    FContato: TContato;

    FcUF: Integer;
    Fcrc: string;
    Fcrc_estado: string;
    FValorReceitaBruta: Double;
    FAnexo: string;
    FDataInicioAtividade: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;

    property IdentificacaoPrestador: TIdentificacao read FIdentificacaoPrestador write FIdentificacaoPrestador;
    property RazaoSocial: string read FRazaoSocial write FRazaoSocial;
    property NomeFantasia: string read FNomeFantasia write FNomeFantasia;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property Contato: TContato read FContato write FContato;
    // Provedor ISSDigital
    property cUF: Integer read FcUF write FcUF;
    // Provedor SigISS
    property crc: string read Fcrc write Fcrc;
    property crc_estado: string read Fcrc_estado write Fcrc_estado;
    // Provedor WebFisco
    property ValorReceitaBruta: Double read FValorReceitaBruta write FValorReceitaBruta;
    property Anexo: string read FAnexo write FAnexo;
    property DataInicioAtividade: TDateTime read FDataInicioAtividade write FDataInicioAtividade;
  end;

  TDadosTomador = class(TObject)
  private
    FIdentificacaoTomador: TIdentificacao;

    FRazaoSocial: string;
    FNomeFantasia: string;

    FEndereco: TEndereco;
    FContato: TContato;

    FAtualizaTomador: TnfseSimNao;
    FTomadorExterior: TnfseSimNao;
  public
    constructor Create;
    destructor Destroy; override;

    property IdentificacaoTomador: TIdentificacao read FIdentificacaoTomador write FIdentificacaoTomador;
    property RazaoSocial: string read FRazaoSocial write FRazaoSocial;
    property NomeFantasia: string read FNomeFantasia write FNomeFantasia;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property Contato: TContato read FContato write FContato;
    property AtualizaTomador: TnfseSimNao read FAtualizaTomador write FAtualizaTomador;
    property TomadorExterior: TnfseSimNao read FTomadorExterior write FTomadorExterior;
  end;

  TDadosIntermediario = class(TObject)
  private
    FIdentificacao: TIdentificacao;

    FRazaoSocial: string;
    FIssRetido: TnfseSituacaoTributaria;

    FEndereco: TEndereco;
    FContato: TContato;
    FCodigoMunicipio: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Identificacao: TIdentificacao read FIdentificacao write FIdentificacao;
    property RazaoSocial: string read FRazaoSocial write FRazaoSocial;
    property IssRetido: TnfseSituacaoTributaria read FIssRetido write FIssRetido;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property Contato: TContato read FContato write FContato;
    property CodigoMunicipio: string read FCodigoMunicipio write FCodigoMunicipio;
  end;

  TIdentificacaoOrgaoGerador = class(TObject)
  private
    FCodigoMunicipio: string;
    FUf: string;
  public
    property CodigoMunicipio: string read FCodigoMunicipio write FCodigoMunicipio;
    property Uf: string read FUf write FUf;
  end;

  TDadosConstrucaoCivil = class(TObject)
  private
    FCodigoObra: string;
    FArt: string;
    FEndereco: TEndereco;

    FnCei: string;
    FnProj: string;
    FnMatri: string;
    FnNumeroEncapsulamento: string;
    FinscImobFisc: String;
  public
    constructor Create;
    destructor Destroy; override;

    property CodigoObra: string read FCodigoObra write FCodigoObra;
    property Art: string read FArt write FArt;
    property Endereco: TEndereco read FEndereco write FEndereco;

    property nCei: string read FnCei write FnCei;
    property nProj: string read FnProj write FnProj;
    property nMatri: string read FnMatri write FnMatri;
    property nNumeroEncapsulamento: string read FnNumeroEncapsulamento write FnNumeroEncapsulamento;
    // Provedor PadraoNacional
    property inscImobFisc: String read FinscImobFisc write FinscImobFisc;
  end;

  TParcelasCollectionItem = class(TObject)
  private
    FCondicao: TnfseCondicaoPagamento;
    FParcela: string;
    FDataVencimento: TDateTime;
    FValor: Double;
  public
    property Condicao: TnfseCondicaoPagamento read FCondicao write FCondicao;
    property Parcela: string read FParcela write FParcela;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
    property Valor: Double read FValor write FValor;
  end;

  TParcelasCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TParcelasCollectionItem;
    procedure SetItem(Index: Integer; Const Value: TParcelasCollectionItem);
  public
    function Add: TParcelasCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TParcelasCollectionItem;
    property Items[Index: Integer]: TParcelasCollectionItem read GetItem write SetItem; default;
  end;

  TCondicaoPagamento = class(TObject)
  private
    FCondicao: TnfseCondicaoPagamento;
    FQtdParcela: Integer;
    FParcelas: TParcelasCollection;
    procedure SetParcelas(const Value: TParcelasCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property Condicao: TnfseCondicaoPagamento read FCondicao write FCondicao;
    property QtdParcela: Integer read FQtdParcela write FQtdParcela;
    property Parcelas: TParcelasCollection read FParcelas write SetParcelas;
 end;

  TemailCollectionItem = class(TObject)
  private
    FemailCC: string;
  public
    property emailCC: string read FemailCC write FemailCC;
  end;

  TemailCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TemailCollectionItem;
    procedure SetItem(Index: Integer; Value: TemailCollectionItem);
  public
    function Add: TemailCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TemailCollectionItem;
    property Items[Index: Integer]: TemailCollectionItem read GetItem write SetItem; default;
  end;

  TDadosTransportadora = class(TObject)
  private
    FxNomeTrans: string;
    FxCpfCnpjTrans: string;
    FxInscEstTrans: string;
    FxPlacaTrans: string;
    FxEndTrans: string;
    FcMunTrans: Integer;
    FxMunTrans: string;
    FxUFTrans: string;
    FcPaisTrans: Integer;
    FxPaisTrans: string;
    FvTipoFreteTrans: TnfseFrete;
  public
    property xNomeTrans: string read FxNomeTrans write FxNomeTrans;
    property xCpfCnpjTrans: string read FxCpfCnpjTrans write FxCpfCnpjTrans;
    property xInscEstTrans: string read FxInscEstTrans write FxInscEstTrans;
    property xPlacaTrans: string read FxPlacaTrans write FxPlacaTrans;
    property xEndTrans: string read FxEndTrans write FxEndTrans;
    property cMunTrans: Integer read FcMunTrans write FcMunTrans;
    property xMunTrans: string read FxMunTrans write FxMunTrans;
    property xUFTrans: string read FxUFTrans write FxUFTrans;
    property cPaisTrans: Integer read FcPaisTrans write FcPaisTrans;
    property xPaisTrans: string read FxPaisTrans write FxPaisTrans;
    property vTipoFreteTrans: TnfseFrete read FvTipoFreteTrans write FvTipoFreteTrans;
  end;

  TDespesaCollectionItem = class(TObject)
  private
    FnItemDesp: string;
    FxDesp: string;
    FdDesp: TDateTime;
    FvDesp: Double;
  public
    property nItemDesp: string read FnItemDesp write FnItemDesp;
    property xDesp: string read FxDesp write FxDesp;
    property dDesp: TDateTime read FdDesp write FdDesp;
    property vDesp: Double read FvDesp write FvDesp;
  end;

  TDespesaCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TDespesaCollectionItem;
    procedure SetItem(Index: Integer; Value: TDespesaCollectionItem);
  public
    function Add: TDespesaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDespesaCollectionItem;
    property Items[Index: Integer]: TDespesaCollectionItem read GetItem write SetItem;
  end;

  TPedidoCancelamento = class(TObject)
  private
    FInfID: TInfID;
    FIdentificacaoNfse: TIdentificacaoNfse;
    FCodigoCancelamento: string;
  public
    constructor Create;
    destructor Destroy; override;

    property InfID: TInfID read FInfID write FInfID;
    property IdentificacaoNfse: TIdentificacaoNfse read FIdentificacaoNfse write FIdentificacaoNfse;
    property CodigoCancelamento: string read FCodigoCancelamento write FCodigoCancelamento;
  end;

  TConfirmacaoCancelamento = class(TObject)
  private
    FInfID: TInfID;
    FPedido: TPedidoCancelamento;
    FSucesso: Boolean;
    FDataHora: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;

    property InfID: TInfID read FInfID write FInfID;
    property Pedido: TPedidoCancelamento read FPedido write FPedido;
    property Sucesso: Boolean read FSucesso write FSucesso;
    property DataHora: TDateTime read FDataHora write FDataHora;
  end;

  TQuartoCollectionItem = class(TObject)
  private
    FCodigoInternoQuarto: Integer;
    FQtdHospedes: Integer;
    FCheckIn: TDateTime;
    FQtdDiarias: Integer;
    FValorDiaria: Double;
  public
    constructor Create;

    property CodigoInternoQuarto: Integer read FCodigoInternoQuarto write FCodigoInternoQuarto;
    property QtdHospedes: Integer read FQtdHospedes write FQtdHospedes;
    property CheckIn: TDateTime read FCheckIn write FCheckIn;
    property QtdDiarias: Integer read FQtdDiarias write FQtdDiarias;
    property ValorDiaria: Double read FValorDiaria write FValorDiaria;
  end;

  TQuartoCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TQuartoCollectionItem;
    procedure SetItem(Index: Integer; Value: TQuartoCollectionItem);
  public
    function New: TQuartoCollectionItem;
    property Items[Index: Integer]: TQuartoCollectionItem read GetItem write SetItem; default;
  end;

  TGenericosCollectionItem = class(TObject)
  private
    FTitulo: string;
    FDescricao: string;
  public
    property Titulo: string read FTitulo write FTitulo;
    property Descricao: string read FDescricao write FDescricao;
  end;

  TGenericosCollection = class(TACBrObjectList)
  private
    function GetItem(Index: Integer): TGenericosCollectionItem;
    procedure SetItem(Index: Integer; Value: TGenericosCollectionItem);
  public
    function Add: TGenericosCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TGenericosCollectionItem;
    property Items[Index: Integer]: TGenericosCollectionItem read GetItem write SetItem; default;
  end;

  { TSubstituicao }

  TSubstituicao = class(TObject)
  private
    FchSubstda: string;
    FcMotivo: TcMotivo;
    FxMotivo: string;
  public
    property chSubstda: string read FchSubstda write FchSubstda;
    property cMotivo: TcMotivo read FcMotivo write FcMotivo;
    property xMotivo: string read FxMotivo write FxMotivo;
  end;

  { TinfNFSe }

  TinfNFSe = class(TObject)
  private
    FID: string;
    FxLocEmi: string;
    FxLocPrestacao: string;
    FnNFSe: string;
    FcLocIncid: Integer;
    FxLocIncid: string;
    FxTribNac: string;
    FxTribMun: string;
    FxNBS: string;
    FverAplic: string;
    FambGer: TambGer;
    FtpEmis: TtpEmis;
    FprocEmi: TprocEmi;
    FcStat: Integer;
    FdhProc: TDateTime;
    FnDFSe: string;
    Femit: TDadosPessoa;
    Fvalores: TValoresNfse;
  public
    constructor Create;
    destructor Destroy; override;

    property ID: string read FID write FID;
    property xLocEmi: string read FxLocEmi write FxLocEmi;
    property xLocPrestacao: string read FxLocPrestacao write FxLocPrestacao;
    property nNFSe: string read FnNFSe write FnNFSe;
    property cLocIncid: Integer read FcLocIncid write FcLocIncid;
    property xLocIncid: string read FxLocIncid write FxLocIncid;
    property xTribNac: string read FxTribNac write FxTribNac;
    property xTribMun: string read FxTribMun write FxTribMun;
    property xNBS: string read FxNBS write FxNBS;
    property verAplic: string read FverAplic write FverAplic;
    property ambGer: TambGer read FambGer write FambGer;
    property tpEmis: TtpEmis read FtpEmis write FtpEmis;
    property procEmi: TprocEmi read FprocEmi write FprocEmi;
    property cStat: Integer read FcStat write FcStat;
    property dhProc: TDateTime read FdhProc write FdhProc;
    property nDFSe: string read FnDFSe write FnDFSe;
    property emit: TDadosPessoa read Femit write Femit;
    property valores: TValoresNfse read Fvalores write Fvalores;
  end;

  { TLinkNFSeParam }

  TLinkNFSeParam = class(TObject)
  private
    FAmbiente: Integer;
    FProLinkURL: String;
    FHomLinkURL: String;
    FNumNFSe: String;
    FCodVerificacao: String;
    FChaveAcesso: String;
    FValorServico: String;
    FCNPJ: String;
    FInscMun: String;
    FxMunicipio: String;
  public
    property Ambiente: Integer read FAmbiente write FAmbiente;
    property ProLinkURL: String read FProLinkURL write FProLinkURL;
    property HomLinkURL: String read FHomLinkURL write FHomLinkURL;
    property NumNFSe: String read FNumNFSe write FNumNFSe;
    property CodVerificacao: String read FCodVerificacao write FCodVerificacao;
    property ChaveAcesso: String read FChaveAcesso write FChaveAcesso;
    property ValorServico: String read FValorServico write FValorServico;
    property CNPJ: String read FCNPJ write FCNPJ;
    property InscMun: String read FInscMun write FInscMun;
    property xMunicipio: String read FxMunicipio write FxMunicipio;
  end;

  TNFSe = class(TPersistent)
  private
    // RPS e NFSe
    FSituacao: Integer;

    FSituacaoNfse: TStatusNFSe;
    FNomeArq: string;
    FInfID: TInfID;
    FIdentificacaoRps: TIdentificacaoRps;
    FDataEmissao: TDateTime;
    FNaturezaOperacao: TnfseNaturezaOperacao;
    FRegimeEspecialTributacao: TnfseRegimeEspecialTributacao;
    FOptanteSimplesNacional: TnfseSimNao;
    FOptanteMEISimei: TnfseSimNao;
    // Provedor Conam
    FDataEmissaoRps: TDateTime;
    FDataOptanteSimplesNacional: TDateTime;
    FLogradouroLocalPrestacaoServico: TLogradouroLocalPrestacaoServico;
    FIncentivadorCultural: TnfseSimNao;
    FProducao: TnfseSimNao;
    FStatusRps: TStatusRps;
    FRpsSubstituido: TIdentificacaoRps;
    FSeriePrestacao: string;
    FServico: TDadosServico;
    FQuartos: TQuartoCollection;
    FPrestador: TDadosPrestador;
    FTomador: TDadosTomador;
    FIntermediario: TDadosIntermediario;
    FConstrucaoCivil: TDadosConstrucaoCivil;
    FDeducaoMateriais: TnfseSimNao;
    FCondicaoPagamento: TCondicaoPagamento;
    FDataPagamento: TDateTime;
    // NFSe
    FNumero: string;
    FCodigoVerificacao: string;
    FCompetencia: TDateTime;
    FNfseSubstituida: string;
    FOutrasInformacoes: string;
    FValorCredito: Double;
    FOrgaoGerador: TIdentificacaoOrgaoGerador;
    FValoresNfse: TValoresNfse;
    FChaveAcesso: string;
    FLink: string;

    // RPS e NFSe
    FDespesa: TDespesaCollection;

    FNumeroLote: string;
//    FProtocolo: string;
    FdhRecebimento: TDateTime;

    FNfseCancelamento: TConfirmacaoCancelamento;
    FNfseSubstituidora: string;
    // Provedor ISSDSF
    FMotivoCancelamento: string;
    // Provedor ISSBarueri
    FCodigoCancelamento: string;
    // Provedor Infisc
    FcNFSe: Integer;

    // Provedor Infisc Versão XML 1.1
    FTipoEmissao: TTipoEmissao;
    FEmpreitadaGlobal: TEmpreitadaGlobal;
    FModeloNFSe: string;
    FTransportadora: TDadosTransportadora;
    FCanhoto: TnfseCanhoto;

    Femail: TemailCollection;
    FTipoRecolhimento: string;
    FRegRec: TRegRec;
    FFrmRec: TFrmRec;
    FTipoTributacaoRPS: TTipoTributacaoRPS;
    FAssinatura: string;
    FInformacoesComplementares: string;

    FPercentualCargaTributaria: Double;
    FValorCargaTributaria: Double;
    FPercentualCargaTributariaMunicipal: Double;
    FValorCargaTributariaMunicipal: Double;
    FPercentualCargaTributariaEstadual: Double;
    FValorCargaTributariaEstadual: Double;
    FTipoNota: Integer;
    FSiglaUF: string;
    FEspecieDocumento: Integer;
    FSerieTalonario: Integer;
    FFormaPagamento: Integer;
    FNumeroParcelas: Integer;
    Fid_sis_legado: Integer;
    FSituacaoTrib: TSituacaoTrib;
    FrefNF: string;
    FGenericos: TGenericosCollection;
    FverAplic: string;
    FtpEmit: TtpEmit;
    FOptanteSN: TOptanteSN;
    FRegimeApuracaoSN: TRegimeApuracaoSN;
    Fsubst: TSubstituicao;

    FinfNFSe: TinfNFSe;
    FDescricaoCodigoTributacaoMunicipio: string;
    FEqptoRecibo: string;
    FVencimento: TDateTime;

    procedure Setemail(const Value: TemailCollection);
    procedure SetInformacoesComplementares(const Value: string);
    procedure SetDespesa(const Value: TDespesaCollection);
    procedure SetGenericos(const Value: TGenericosCollection);
    procedure SetQuartos(const Value: TQuartoCollection);
  public
    constructor Create;
    destructor Destroy; override;

    function LinkNFSe(LinkNFSeParam: TLinkNFSeParam): String;

    procedure Clear;
  published
    // RPS e NFSe
    property Situacao: Integer read FSituacao write FSituacao;
    property SituacaoNfse: TStatusNFSe read FSituacaoNfse write FSituacaoNfse;
    property NomeArq: string read FNomeArq write FNomeArq;
    property InfID: TInfID read FInfID write FInfID;
    property IdentificacaoRps: TIdentificacaoRps read FIdentificacaoRps write FIdentificacaoRps;
    property DataEmissao: TDateTime read FDataEmissao write FDataEmissao;
    property NaturezaOperacao: TnfseNaturezaOperacao read FNaturezaOperacao write FNaturezaOperacao;
    property RegimeEspecialTributacao: TnfseRegimeEspecialTributacao read FRegimeEspecialTributacao write FRegimeEspecialTributacao;
    property OptanteSimplesNacional: TnfseSimNao read FOptanteSimplesNacional write FOptanteSimplesNacional;
    property OptanteMEISimei: TnfseSimNao read FOptanteMEISimei write FOptanteMEISimei;
    //Provedor Conam
    property DataOptanteSimplesNacional: TDateTime read FDataOptanteSimplesNacional write FDataOptanteSimplesNacional;
    property LogradouLocalPrestacaoServico: TLogradouroLocalPrestacaoServico read FLogradouroLocalPrestacaoServico write FLogradouroLocalPrestacaoServico;
    property IncentivadorCultural: TnfseSimNao read FIncentivadorCultural write FIncentivadorCultural;
    property Producao: TnfseSimNao read FProducao write FProducao;
    property StatusRps: TStatusRps read FStatusRps write FStatusRps;
    property RpsSubstituido: TIdentificacaoRps read FRpsSubstituido write FRpsSubstituido;
    property DataEmissaoRps: TDateTime read FDataEmissaoRps write FDataEmissaoRps;
    // Provedor IssDsf
    property SeriePrestacao: string read FSeriePrestacao write FSeriePrestacao;
    property Servico: TDadosServico read FServico write FServico;
    property Quartos: TQuartoCollection read FQuartos write SetQuartos;
    property Prestador: TDadosPrestador read FPrestador write FPrestador;
    property Tomador: TDadosTomador read FTomador write FTomador;
    property Intermediario: TDadosIntermediario read FIntermediario write FIntermediario;
    property ConstrucaoCivil: TDadosConstrucaoCivil read FConstrucaoCivil write FConstrucaoCivil;
    property DeducaoMateriais: TnfseSimNao read FDeducaoMateriais write FDeducaoMateriais;
    property CondicaoPagamento: TCondicaoPagamento read FCondicaoPagamento write FCondicaoPagamento;
    // Provedor FintelISS
    property DataPagamento: TDateTime read FDataPagamento write FDataPagamento;
    // NFSe
    property Numero: string read FNumero write FNumero;
    property CodigoVerificacao: string read FCodigoVerificacao write FCodigoVerificacao;
    property Competencia: TDateTime read FCompetencia write FCompetencia;
    property NfseSubstituida: string read FNfseSubstituida write FNfseSubstituida;
    property OutrasInformacoes: string read FOutrasInformacoes write FOutrasInformacoes;
    property InformacoesComplementares: string read FInformacoesComplementares write SetInformacoesComplementares;
    property ValorCredito: Double read FValorCredito write FValorCredito;
    property OrgaoGerador: TIdentificacaoOrgaoGerador read FOrgaoGerador write FOrgaoGerador;
    property ValoresNfse: TValoresNfse read FValoresNfse write FValoresNfse;
    property ChaveAcesso: string read FChaveAcesso write FChaveAcesso;
    property Link: string read FLink write FLink;
    property NumeroLote: string read FNumeroLote write FNumeroLote;
    property DescricaoCodigoTributacaoMunicipio: string read FDescricaoCodigoTributacaoMunicipio write FDescricaoCodigoTributacaoMunicipio;
//    property Protocolo: string read FProtocolo write FProtocolo;
    property dhRecebimento: TDateTime read FdhRecebimento write FdhRecebimento;
    property NfseCancelamento: TConfirmacaoCancelamento read FNfseCancelamento write FNfseCancelamento;
    property NfseSubstituidora: string read FNfseSubstituidora write FNfseSubstituidora;
    // Provedor ISSDSF
    property MotivoCancelamento: string read FMotivoCancelamento write FMotivoCancelamento;
    // Provedor ISSBarueri
    property CodigoCancelamento: string read FCodigoCancelamento write FCodigoCancelamento;
    // Provedor Infisc
    property cNFSe: Integer read FcNFSe write FcNFSe;
    property refNF: string read FrefNF write FrefNF;
    // Provedor Infisc Versão XML 1.1
    property TipoEmissao: TTipoEmissao read FTipoEmissao write FTipoEmissao;
    property EmpreitadaGlobal: TEmpreitadaGlobal read FEmpreitadaGlobal write FEmpreitadaGlobal;
    property ModeloNFSe: string read FModeloNFSe write FModeloNFSe;
    property Canhoto: TnfseCanhoto read FCanhoto Write FCanhoto;
    property Transportadora: TDadosTransportadora read FTransportadora write FTransportadora;
    property Despesa: TDespesaCollection read FDespesa write SetDespesa;
    // Provedor Governa
    property TipoRecolhimento: string read FTipoRecolhimento write FTipoRecolhimento;

    property email: TemailCollection read Femail write Setemail;

    property TipoTributacaoRPS: TTipoTributacaoRPS read FTipoTributacaoRPS write FTipoTributacaoRPS;

    // Provedor SP
    property Assinatura: string read FAssinatura write FAssinatura;
    // Provedor Governa
    property RegRec: TRegRec read FRegRec write FRegRec;
    property FrmRec: TFrmRec read FFrmRec write FFrmRec;
    // Provedor Techos
    property PercentualCargaTributaria: Double read FPercentualCargaTributaria write FPercentualCargaTributaria;
    property ValorCargaTributaria: Double read FValorCargaTributaria write FValorCargaTributaria;
    property PercentualCargaTributariaMunicipal: Double read FPercentualCargaTributariaMunicipal write FPercentualCargaTributariaMunicipal;
    property ValorCargaTributariaMunicipal: Double read FValorCargaTributariaMunicipal write FValorCargaTributariaMunicipal;
    property PercentualCargaTributariaEstadual: Double read FPercentualCargaTributariaEstadual write FPercentualCargaTributariaEstadual;
    property ValorCargaTributariaEstadual: Double read FValorCargaTributariaEstadual write FValorCargaTributariaEstadual;
    property TipoNota: Integer read FTipoNota write FTipoNota;
    property SiglaUF: string read FSiglaUF write FSiglaUF;
    property EspecieDocumento: Integer read FEspecieDocumento write FEspecieDocumento;
    property SerieTalonario: Integer read FSerieTalonario write FSerieTalonario;
    property FormaPagamento: Integer read FFormaPagamento write FFormaPagamento;
    property NumeroParcelas: Integer read FNumeroParcelas write FNumeroParcelas;
    // Provedor SigISS
    // Código da nota no sistema legado do contribuinte.
    property id_sis_legado: Integer read Fid_sis_legado write Fid_sis_legado;
    property SituacaoTrib: TSituacaoTrib read FSituacaoTrib write FSituacaoTrib;
    // Provedor IPM
    property Genericos: TGenericosCollection read FGenericos write SetGenericos;
    // Provedor PadraoNacional
    property verAplic: string read FverAplic write FverAplic;
    property tpEmit: TtpEmit read FtpEmit write FtpEmit;
    property OptanteSN: TOptanteSN read FOptanteSN write FOptanteSN;
    property RegimeApuracaoSN: TRegimeApuracaoSN read FRegimeApuracaoSN write FRegimeApuracaoSN;
    property subst: TSubstituicao read Fsubst write Fsubst;

    property infNFSe: TinfNFSe read FinfNFSe write FinfNFSe;
    // Provedor eGoverneISS
    property EqptoRecibo: string read FEqptoRecibo write FEqptoRecibo;
    // Provedor RLZ
    property Vencimento: TDateTime read FVencimento write FVencimento;

  end;

  TSubstituicaoNfse = class(TObject)
  private
    FInfID: TInfID;
    FNfseSubstituidora: string;
  public
    constructor Create;
    destructor Destroy; override;

    property InfID: TInfID read FInfID write FInfID;
    property NfseSubstituidora: string read FNfseSubstituidora write FNfseSubstituidora;
  end;

const
  CMUN_EXTERIOR = 9999999;
  XMUN_EXTERIOR = 'EXTERIOR';
  UF_EXTERIOR = 'EX';

implementation

uses
  ACBrValidador;

{ TDadosServicoRPS }

constructor TDadosServico.Create;
begin
  inherited Create;

  FValores := TValores.Create;

  with FValores do
  begin
    FValorServicos := 0;
    FValorDeducoes := 0;
    FValorPis := 0;
    FValorCofins := 0;
    FValorInss := 0;
    FValorIr := 0;
    FValorCsll := 0;
    FIssRetido := stNormal;
    FValorIss := 0;
    FValorIssRetido := 0;
    FOutrasRetencoes := 0;
    FBaseCalculo := 0;
    FAliquota := 0;
    FValorLiquidoNfse := 0;
    FDescontoIncondicionado := 0;
    FDescontoCondicionado := 0;
    FValorDespesasNaoTributaveis := 0;
    FRetencoesFederais := 0;
    FValorTotalNotaFiscal := 0;
  end;

  FItemServico := TItemServicoCollection.Create;
  FDeducao := TDeducaoCollection.Create;
  FcomExt := TComExterior.Create;
  FLocacao := TLocacao.Create;
  FEvento := TEvento.Create;
  FExplRod := TExplRod.Create;
  FinfoCompl := TinfoCompl.Create;
  FImposto := TImpostoCollection.Create;

  FDescricao := '';
  FPrestadoEmViasPublicas := False;
end;

destructor TDadosServico.Destroy;
begin
  FValores.Free;
  FItemServico.Free;
  FDeducao.Free;
  FcomExt.Free;
  FLocacao.Free;
  FEvento.Free;
  FExplRod.Free;
  FinfoCompl.Free;
  FImposto.Free;

  inherited Destroy;
end;

procedure TDadosServico.SetDeducao(const Value: TDeducaoCollection);
begin
  FDeducao := Value;
end;

procedure TDadosServico.SetImposto(const Value: TImpostoCollection);
begin
  FImposto := Value;
end;

procedure TDadosServico.SetItemServico(Value: TItemServicoCollection);
begin
  FItemServico.Assign(Value);
end;

{ TDadosPrestador }

constructor TDadosPrestador.Create;
begin
  inherited Create;

  FIdentificacaoPrestador := TIdentificacao.Create;
  FEndereco := TEndereco.Create;
  FContato := TContato.Create;

  with FIdentificacaoPrestador do
  begin
    CpfCnpj := '';
    InscricaoMunicipal := '';
    InscricaoEstadual := '';
  end;
end;

destructor TDadosPrestador.Destroy;
begin
  FIdentificacaoPrestador.Free;
  FEndereco.Free;
  FContato.Free;

  inherited Destroy;
end;

{ TDadosTomador }

constructor TDadosTomador.Create;
begin
  inherited Create;

  FIdentificacaoTomador := TIdentificacao.Create;
  FEndereco := TEndereco.Create;
  FContato := TContato.Create;
end;

destructor TDadosTomador.Destroy;
begin
  FIdentificacaoTomador.Free;
  FEndereco.Free;
  FContato.Free;

  inherited Destroy;
end;

{ TNFSe }

procedure TNFSe.Clear;
begin
  // RPS e NFSe
  FNomeArq := '';
  FIdentificacaoRps.FTipo := trRPS;
  FDataEmissao := 0;
  FNaturezaOperacao := no1;
  FRegimeEspecialTributacao := retNenhum;
  FOptanteSimplesNacional := snNao;
  FOptanteMEISimei := snNao;
  FIncentivadorCultural := snNao;
  FStatusRps := srNormal;

  FRpsSubstituido.FNumero := '';
  FRpsSubstituido.FSerie := '';
  FRpsSubstituido.FTipo := trRPS;

  FSituacaoNfse := snNormal;
  FNfseCancelamento.DataHora := 0;
  FNfseSubstituidora := '';
  // NFSe
  FNumero := '';
  FCodigoVerificacao := '';
  FCompetencia := 0;
  FNfseSubstituida := '';
  FOutrasInformacoes := '';
  FInformacoesComplementares := '';
  FValorCredito := 0;

  // Provedor Infisc Versão XML 1.1
  FTipoEmissao := teNormalNFSe;
  FEmpreitadaGlobal := EgOutros;
  FModeloNFSe := '';
  FCanhoto := tcNenhum;

  FLogradouroLocalPrestacaoServico := llpTomador;

  FServico.FItemServico.Clear;
  FServico.FDeducao.Clear;
  FCondicaoPagamento.FParcelas.Clear;

  FQuartos.Clear;
  Femail.Clear;
  FDespesa.Clear;
  FGenericos.Clear;

  FverAplic := '';
  FOptanteSN := osnNaoOptante;
  FRegimeApuracaoSN := raFederaisMunicipalpeloSN;
end;

constructor TNFSe.Create;
begin
  inherited Create;

  FInfID := TInfID.Create;
  FIdentificacaoRps := TIdentificacaoRps.Create;
  FRpsSubstituido := TIdentificacaoRps.Create;
  FServico := TDadosServico.Create;
  FPrestador := TDadosPrestador.Create;
  FTomador := TDadosTomador.Create;
  FIntermediario := TDadosIntermediario.Create;
  FConstrucaoCivil := TDadosConstrucaoCivil.Create;
  FCondicaoPagamento := TCondicaoPagamento.Create;
  FQuartos := TQuartoCollection.Create;
  FOrgaoGerador := TIdentificacaoOrgaoGerador.Create;
  FValoresNfse := TValoresNfse.Create;
  FNfseCancelamento := TConfirmacaoCancelamento.Create;
  FTransportadora := TDadosTransportadora.Create;
  Femail := TemailCollection.Create;
  FDespesa := TDespesaCollection.Create;
  FGenericos := TGenericosCollection.Create;
  Fsubst := TSubstituicao.Create;
  FinfNFSe := TinfNFSe.Create;

  Clear;
end;

destructor TNFSe.Destroy;
begin
  // RPS e NFSe
  FInfID.Free;
  FIdentificacaoRps.Free;
  FRpsSubstituido.Free;
  FServico.Free;
  FPrestador.Free;
  FTomador.Free;
  FIntermediario.Free;
  FConstrucaoCivil.Free;
  FCondicaoPagamento.Free;
  FQuartos.Free;
  // NFSe
  FOrgaoGerador.Free;
  FValoresNfse.Free;
  // RPS e NFSe
  FNfseCancelamento.Free;
  Femail.Free;
  FDespesa.Free;
  FTransportadora.Free;
  FGenericos.Free;
  Fsubst.Free;
  FinfNFSe.Free;

  inherited Destroy;
end;

procedure TNFSe.SetInformacoesComplementares(const Value: string);
begin
  FInformacoesComplementares := Value;
end;

procedure TNFSe.SetQuartos(const Value: TQuartoCollection);
begin
  FQuartos := Value;
end;

procedure TNFSe.Setemail(const Value: TemailCollection);
begin
  Femail := Value;
end;

procedure TNFSe.SetGenericos(const Value: TGenericosCollection);
begin
  FGenericos := Value;
end;

procedure TNFSe.SetDespesa(const Value: TDespesaCollection);
begin
  FDespesa := Value;
end;

function TNFSe.LinkNFSe(LinkNFSeParam: TLinkNFSeParam): String;
var
  Texto: String;

  procedure PreencherData(NomeTag: string; Valor: TDateTime);
  var
    Pos1: Integer;
    TagReplace: string;
    Format: string;
  begin
    Pos1 := Pos('%' + NomeTag + ':',Texto);

    if Pos1 > 0 then
    begin
      TagReplace := Copy(Texto,Pos1+1,Length(Texto));
      Pos1 := Pos('%',TagReplace);

      if Pos1 > 0 then
      begin
        TagReplace := '%' + Copy(TagReplace,1,Pos1);

        Pos1 := Pos(':',TagReplace);
        Format := Copy(TagReplace,Pos1+1,Length(TagReplace)-Pos1-1);

        Texto := StringReplace(Texto, TagReplace, FormatDateBr(Valor, Format), [rfReplaceAll]);
      end;
    end;
  end;
begin
  if not Assigned(LinkNFSeParam) then
  begin
    Result := '';
    exit;
  end;

  if LinkNFSeParam.CodVerificacao = '' then
    LinkNFSeParam.CodVerificacao := FCodigoVerificacao;

  if LinkNFSeParam.NumNFSe = '' then
    LinkNFSeParam.NumNFSe := FNumero;

  if LinkNFSeParam.ChaveAcesso = '' then
    LinkNFSeParam.ChaveAcesso := FChaveAcesso;

  if LinkNFSeParam.ValorServico = '' then
    LinkNFSeParam.ValorServico := FloatToStr(Servico.Valores.ValorLiquidoNfse);

  if LinkNFSeParam.CNPJ = '' then
    LinkNFSeParam.CNPJ := Prestador.IdentificacaoPrestador.CpfCnpj;

  if LinkNFSeParam.InscMun = '' then
    LinkNFSeParam.InscMun := Prestador.IdentificacaoPrestador.InscricaoMunicipal;

  if LinkNFSeParam.FAmbiente = 0 then
    Texto := LinkNFSeParam.FProLinkURL
  else
    Texto := LinkNFSeParam.FHomLinkURL;

  // %CodVerif%          : Representa o Código de Verificação da NFS-e
  // %CodVerifSoAlfanum% : Representa o Código de Verificação da NFS-e sem formatação
  // %NumeroNFSe%        : Representa o Numero da NFS-e
  // %ChaveAcesso%       : Representa a Chave de Acesso
  // %ValorServico%      : Representa o Valor do Serviço
  // %Cnpj%              : Representa o CNPJ do Emitente - Configuração
  // %CnpjComMascara%    : Representa o CNPJ do Emitente - Configuração com mascara
  // %InscMunic%         : Representa a Inscrição Municipal do Emitente - Configuração
  // %xMunicipio%        : Representa o Nome do Município - Configuração
  // %DataEmissao:X..X%: : Representa a Data de Emissão da NFSe com o formato preenchido após os ":" - Dados da NFSe

  Texto := StringReplace(Texto, '%CodVerif%', LinkNFSeParam.CodVerificacao, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%CodVerifSoAlfanum%',
             OnlyAlphaNum(LinkNFSeParam.CodVerificacao), [rfReplaceAll]);
  Texto := StringReplace(Texto, '%NumeroNFSe%', LinkNFSeParam.NumNFSe, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%ChaveAcesso%', LinkNFSeParam.ChaveAcesso, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%ValorServico%', LinkNFSeParam.ValorServico, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%Cnpj%', LinkNFSeParam.CNPJ, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%CnpjComMascara%', FormatarCNPJouCPF(LinkNFSeParam.CNPJ), [rfReplaceAll]);
  Texto := StringReplace(Texto, '%InscMunic%', LinkNFSeParam.InscMun, [rfReplaceAll]);
  Texto := StringReplace(Texto, '%xMunicipio%', LowerCase(OnlyAlphaNum(LinkNFSeParam.xMunicipio)), [rfReplaceAll]);
  PreencherData('DataEmissao',DataEmissao);

  Result := Texto;
end;

{ TPedidoCancelamento }

constructor TPedidoCancelamento.Create;
begin
  inherited Create;

  FInfID := TInfID.Create;
  FIdentificacaoNfse := TIdentificacaoNfse.Create;
  FCodigoCancelamento := '';
end;

destructor TPedidoCancelamento.Destroy;
begin
  FInfID.Free;
  FIdentificacaoNfse.Free;

  inherited Destroy;
end;

{ TConfirmacaoCancelamento }

constructor TConfirmacaoCancelamento.Create;
begin
  inherited Create;

  FInfID := TInfID.Create;
  FPedido := TPedidoCancelamento.Create;
end;

destructor TConfirmacaoCancelamento.Destroy;
begin
  FInfID.Free;
  FPedido.Free;

  inherited Destroy;
end;

{ TSubstituicaoNfse }

constructor TSubstituicaoNfse.Create;
begin
  inherited Create;

  FInfID := TInfID.Create;
  FNfseSubstituidora := '';
end;

destructor TSubstituicaoNfse.Destroy;
begin
  FInfID.Free;

  inherited Destroy;
end;

{ TItemServicoCollection }

function TItemServicoCollection.Add: TItemServicoCollectionItem;
begin
  Result := Self.New;
end;

function TItemServicoCollection.GetItem(Index: Integer): TItemServicoCollectionItem;
begin
  Result := TItemServicoCollectionItem(inherited Items[Index]);
end;

procedure TItemServicoCollection.SetItem(Index: Integer;
  Value: TItemServicoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TItemServicoCollection.New: TItemServicoCollectionItem;
begin
  Result := TItemServicoCollectionItem.Create;
  Self.Add(Result);
end;

{ TDeducaoCollection }
function TDeducaoCollection.Add: TDeducaoCollectionItem;
begin
  Result := Self.New;
end;

function TDeducaoCollection.GetItem(Index: Integer): TDeducaoCollectionItem;
begin
  Result := TDeducaoCollectionItem(inherited Items[Index]);
end;

procedure TDeducaoCollection.SetItem(Index: Integer;
  Value: TDeducaoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDeducaoCollection.New: TDeducaoCollectionItem;
begin
  Result := TDeducaoCollectionItem.Create;
  Self.Add(Result);
end;

{ TItemServicoCollectionItem }

constructor TItemServicoCollectionItem.Create;
begin
  inherited Create;

  // Provedor Infisc Versão XML 1.1
  FCodServ := '';
  FUnidade := 'UN';

  FDadosDeducao := TDadosDeducao.Create;;
end;

destructor TItemServicoCollectionItem.Destroy;
begin
  FDadosDeducao.Free;

  inherited Destroy;
end;

{ TParcelasCollection }
function TParcelasCollection.Add: TParcelasCollectionItem;
begin
  Result := Self.New;
end;

function TParcelasCollection.GetItem(Index: Integer): TParcelasCollectionItem;
begin
  Result := TParcelasCollectionItem(inherited Items[Index]);
end;

procedure TParcelasCollection.SetItem(Index: Integer;
  const Value: TParcelasCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TParcelasCollection.New: TParcelasCollectionItem;
begin
  Result := TParcelasCollectionItem.Create;
  Self.Add(Result);
end;

{ TCondicaoPagamento }
constructor TCondicaoPagamento.Create;
begin
  inherited Create;

  FParcelas := TParcelasCollection.Create;
end;

destructor TCondicaoPagamento.Destroy;
begin
  FParcelas.Free;

  inherited Destroy;
end;

procedure TCondicaoPagamento.SetParcelas(const Value: TParcelasCollection);
begin
  FParcelas.Assign(Value);
end;

{ TemailCollection }

function TemailCollection.Add: TemailCollectionItem;
begin
  Result := Self.New;
end;

function TemailCollection.GetItem(Index: Integer): TemailCollectionItem;
begin
  Result := TemailCollectionItem(inherited Items[Index]);
end;

procedure TemailCollection.SetItem(Index: Integer;
  Value: TemailCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TemailCollection.New: TemailCollectionItem;
begin
  Result := TemailCollectionItem.Create;
  Self.Add(Result);
end;

{ TDespesaCollection }

function TDespesaCollection.Add: TDespesaCollectionItem;
begin
  Result := Self.New;
end;

function TDespesaCollection.GetItem(Index: Integer): TDespesaCollectionItem;
begin
  Result := TDespesaCollectionItem(inherited Items[Index]);
end;

procedure TDespesaCollection.SetItem(Index: Integer; Value: TDespesaCollectionItem);
begin
  inherited Items[Index] := Value;
end;

function TDespesaCollection.New: TDespesaCollectionItem;
begin
  Result := TDespesaCollectionItem.Create;
  Self.Add(Result);
end;

{ TQuartoCollection }

function TQuartoCollection.GetItem(Index: Integer): TQuartoCollectionItem;
begin
  Result := TQuartoCollectionItem(inherited Items[Index]);
end;

function TQuartoCollection.New: TQuartoCollectionItem;
begin
  Result := TQuartoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TQuartoCollection.SetItem(Index: Integer; Value: TQuartoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TQuartoCollectionItem }

constructor TQuartoCollectionItem.Create;
begin
  inherited Create;
end;

{ TIdentificacao }

function TIdentificacao.GetCnpj: string;
begin
  Result := FCpfCnpj;
end;

procedure TIdentificacao.SetCnpj(const Value: string);
begin
  FCpfCnpj := Value;
end;

{ TGenericosCollection }

function TGenericosCollection.Add: TGenericosCollectionItem;
begin
  Result := Self.New;
end;

function TGenericosCollection.GetItem(Index: Integer): TGenericosCollectionItem;
begin
  Result := TGenericosCollectionItem(inherited Items[Index]);
end;

function TGenericosCollection.New: TGenericosCollectionItem;
begin
  Result := TGenericosCollectionItem.Create;
  Self.Add(Result);
end;

procedure TGenericosCollection.SetItem(Index: Integer;
  Value: TGenericosCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TDadosIntermediario }

constructor TDadosIntermediario.Create;
begin
  inherited Create;

  FIdentificacao := TIdentificacao.Create;
  FEndereco := TEndereco.Create;
  FContato := TContato.Create;
end;

destructor TDadosIntermediario.Destroy;
begin
  FIdentificacao.Free;
  FEndereco.Free;
  FContato.Free;

  inherited Destroy;
end;

{ TDadosConstrucaoCivil }

constructor TDadosConstrucaoCivil.Create;
begin
  inherited Create;

  FEndereco := TEndereco.Create;
end;

destructor TDadosConstrucaoCivil.Destroy;
begin
  FEndereco.Free;

  inherited Destroy;
end;

{ TEvento }

constructor TEvento.Create;
begin
  inherited Create;

  FEndereco := TEndereco.Create;
end;

destructor TEvento.Destroy;
begin
  FEndereco.Free;

  inherited Destroy;
end;

{ TValores }

constructor TValores.Create;
begin
  inherited Create;

  FDocDeducao := TDocDeducaoCollection.Create;
  FtribMun := TtribMun.Create;
  FtribFed := TtribFed.Create;
  FtotTrib := TtotTrib.Create;
end;

destructor TValores.Destroy;
begin
  FDocDeducao.Free;
  FtribMun.Free;
  FtribFed.Free;
  FtotTrib.Free;

  inherited Destroy;
end;

procedure TValores.SetDocDeducao(const Value: TDocDeducaoCollection);
begin
  FDocDeducao := Value;
end;

{ TDocDeducaoCollection }

function TDocDeducaoCollection.Add: TDocDeducaoCollectionItem;
begin
  Result := Self.New;
end;

function TDocDeducaoCollection.GetItem(
  Index: Integer): TDocDeducaoCollectionItem;
begin
  Result := TDocDeducaoCollectionItem(inherited Items[Index]);
end;

function TDocDeducaoCollection.New: TDocDeducaoCollectionItem;
begin
  Result := TDocDeducaoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TDocDeducaoCollection.SetItem(Index: Integer;
  Value: TDocDeducaoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

{ TDocDeducaoCollectionItem }

constructor TDocDeducaoCollectionItem.Create;
begin
  inherited Create;

  FNFSeMun := TNFSeMun.Create;
  FNFNFS := TNFNFS.Create;
  Ffornec := TInfoPessoa.Create;
end;

destructor TDocDeducaoCollectionItem.Destroy;
begin
  FNFSeMun.Free;
  FNFNFS.Free;
  Ffornec.Free;

  inherited Destroy;
end;

{ TInfoPessoa }

constructor TInfoPessoa.Create;
begin
  inherited Create;

  FIdentificacao := TIdentificacao.Create;
  FEndereco := TEndereco.Create;
  FContato := TContato.Create;
end;

destructor TInfoPessoa.Destroy;
begin
  FIdentificacao.Free;
  FEndereco.Free;
  FContato.Free;

  inherited Destroy;
end;

{ TinfNFSe }

constructor TinfNFSe.Create;
begin
  inherited Create;

  Femit := TDadosPessoa.Create;
  Fvalores := TValoresNfse.Create;
end;

destructor TinfNFSe.Destroy;
begin
  Femit.Free;
  Fvalores.Free;

  inherited Destroy;
end;

{ TDadosPessoa }

constructor TDadosPessoa.Create;
begin
  inherited Create;

  FIdentificacao := TIdentificacao.Create;
  FEndereco := TEndereco.Create;
  FContato := TContato.Create;
end;

destructor TDadosPessoa.Destroy;
begin
  FIdentificacao.Free;
  FEndereco.Free;
  FContato.Free;

  inherited Destroy;
end;

{ TImpostoCollection }

function TImpostoCollection.Add: TImpostoCollectionItem;
begin
  Result := Self.New;
end;

function TImpostoCollection.GetItem(Index: Integer): TImpostoCollectionItem;
begin
  Result := TImpostoCollectionItem(inherited Items[Index]);
end;

function TImpostoCollection.New: TImpostoCollectionItem;
begin
  Result := TImpostoCollectionItem.Create;
  Self.Add(Result);
end;

procedure TImpostoCollection.SetItem(Index: Integer;
  Value: TImpostoCollectionItem);
begin
  inherited Items[Index] := Value;
end;

end.
