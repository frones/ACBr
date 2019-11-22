{******************************************************************************}
{ Projeto: Componente ACBrNFSe                                                 }
{  Biblioteca multiplataforma de componentes Delphi                            }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do Projeto ACBr     }
{ Componentes localizado em http://www.sourceforge.net/projects/acbr           }
{                                                                              }
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
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{              Praça Anita Costa, 34 - Tatuí - SP - 18270-410                  }
{                                                                              }
{******************************************************************************}

{$I ACBr.inc}

unit pnfsNFSe;

interface

uses
  SysUtils, Classes, Contnrs,
  {$IFNDEF VER130}
    Variants,
  {$ENDIF}
  pnfsConversao;

type

 TItemServicoCollectionItem         = class;
 TDeducaoCollectionItem             = class;
 TDadosServico                      = class;
 TCondicaoPagamento                 = class;
 TemailCollectionItem               = class;
 TDespesaCollectionItem             = class;

 TNFSe                              = class;

 TConfirmacaoCancelamento           = class;
 
//******************************************************************************

  TMsgRetornoIdentificacaoRps = class(TObject)
  private
    FNumero: String;
    FSerie: String;
    FTipo: TnfseTipoRps;
  public
    property Numero: String     read FNumero write FNumero;
    property Serie: String      read FSerie  write FSerie;
    property Tipo: TnfseTipoRps read FTipo   write FTipo;
  end;

  TInformacoesLote = class(TObject)
  private
    FNumeroLote: String;
    FInscricaoPrestador: String;
    FCPFCNPJRemetente: String;
    FDataEnvioLote: TDateTime;
    FQtdNotasProcessadas: Integer;
    FTempoProcessamento: Integer;
    FValorTotalServico: Currency;
  public
    property NumeroLote: String           read FNumeroLote          write FNumeroLote;
    property InscricaoPrestador: String   read FInscricaoPrestador  write FInscricaoPrestador;
    property CPFCNPJRemetente: String     read FCPFCNPJRemetente    write FCPFCNPJRemetente;
    property DataEnvioLote: TDateTime     read FDataEnvioLote       write FDataEnvioLote;
    property QtdNotasProcessadas: Integer read FQtdNotasProcessadas write FQtdNotasProcessadas;
    property TempoProcessamento: Integer  read FTempoProcessamento  write FTempoProcessamento;
    property ValorTotalServico: Currency  read FValorTotalServico   write FValorTotalServico;
  end;

  TChaveNFeRPS = class(TObject)
  private
    FInscricaoPrestador: String;
    FNumero: String;
    FCodigoVerificacao: String;
    FLink: String;
    FNumeroRPS: String;
    FSerieRPS: String;
  public
    property InscricaoPrestador: String read FInscricaoPrestador write FInscricaoPrestador;
    // NFS-e
    property Numero: String             read FNumero             write FNumero;
    property CodigoVerificacao: String  read FCodigoVerificacao  write FCodigoVerificacao;
    property Link: String               read FLink               write FLink;
    // RPS
    property SerieRPS: String           read FSerieRPS           write FSerieRPS;
    property NumeroRPS: String          read FNumeroRPS          write FNumeroRPS;
  end;

//******************************************************************************

 TInfID = class(TObject)
  private
    FID: string;
  public
    property ID: String read FID write FID;
  end;

 TIdentificacaoRps = class(TObject)
  private
    FNumero: String;
    FSerie: String;
    FTipo: TnfseTipoRps;
  public
    property Numero: String read FNumero write FNumero;
    property Serie: String read FSerie write FSerie;
    property Tipo: TnfseTipoRps read FTipo write FTipo;
  end;

 TIdentificacaoNfse = class(TObject)
  private
    FNumero: String;
    FCnpj: String;
    FInscricaoMunicipal: String;
    FCodigoMunicipio: String;
  public
    property Numero: String read FNumero write FNumero;
    property Cnpj: String read FCnpj write FCnpj;
    property InscricaoMunicipal: String read FInscricaoMunicipal write FInscricaoMunicipal;
    property CodigoMunicipio: String read FCodigoMunicipio write FCodigoMunicipio;
  end;

 TValoresNfse = class(TObject)
 private
   FBaseCalculo: Currency;
   FAliquota: Double;
   FValorIss: Currency;
   FValorLiquidoNfse: Currency;
 public
   property BaseCalculo: Currency read FBaseCalculo write FBaseCalculo;
   property Aliquota: Double read FAliquota write FAliquota;
   property ValorIss: Currency read FValorIss write FValorIss;
   property ValorLiquidoNfse: Currency read FValorLiquidoNfse write FValorLiquidoNfse;
 end;

 TValores = class(TObject)
  private
    FValorServicos: Currency;
    FValorDeducoes: Currency;
    FValorPis: Currency;
    FValorCofins: Currency;
    FValorInss: Currency;
    FValorIr: Currency;
    FValorCsll: Currency;
    FIssRetido: TnfseSituacaoTributaria;
    FValorIss: Currency;
    FOutrasRetencoes: Currency;
    FBaseCalculo: Currency;
    FAliquota: Double;
    FAliquotaSN: Currency; // Aliquota usada pelo Provedor conam
    FAliquotaPis: Currency;
    FAliquotaCofins: Currency;
    FAliquotaInss: Currency;
    FAliquotaIr: Currency;
    FAliquotaCsll: Currency;
    FOutrosDescontos: Currency;
    FValorLiquidoNfse: Currency;
    FValorIssRetido: Currency;
    FDescontoCondicionado: Currency;
    FDescontoIncondicionado: Currency;
    FJustificativaDeducao: String;
    FvalorOutrasRetencoes: Currency;
    FDescricaoOutrasRetencoes: String;
    FvalorRepasse: Currency; //Governa
    FValorDespesasNaoTributaveis: Currency; //Governa
    FValorTotalRecebido: Currency;
  public
    property ValorServicos: Currency read FValorServicos write FValorServicos;
    property ValorDeducoes: Currency read FValorDeducoes write FValorDeducoes;
    property ValorPis: Currency read FValorPis write FValorPis;
    property ValorCofins: Currency read FValorCofins write FValorCofins;
    property ValorInss: Currency read FValorInss write FValorInss;
    property ValorIr: Currency read FValorIr write FValorIr;
    property ValorCsll: Currency read FValorCsll write FValorCsll;
    property IssRetido: TnfseSituacaoTributaria read FIssRetido write FIssRetido;
    property ValorIss: Currency read FValorIss write FValorIss;
    property OutrasRetencoes: Currency read FOutrasRetencoes write FOutrasRetencoes;
    property BaseCalculo: Currency read FBaseCalculo write FBaseCalculo;
    property Aliquota: Double read FAliquota write FAliquota;
    // Aliquota usada pelo Provedor conam
    property AliquotaSN: Currency read FAliquotaSN write FAliquotaSN;

    // Aliquotas usadas pelo Provedor IssDsf
    property AliquotaPis: Currency read FAliquotaPis write FAliquotaPis;
    property AliquotaCofins: Currency read FAliquotaCofins write FAliquotaCofins;
    property AliquotaInss: Currency read FAliquotaInss write FAliquotaInss;
    property AliquotaIr: Currency read FAliquotaIr write FAliquotaIr;
    property AliquotaCsll: Currency read FAliquotaCsll write FAliquotaCsll;

    // Gumercino 16/01/2018 - Provedor EL
    property OutrosDescontos: Currency read FOutrosDescontos write FOutrosDescontos;

    property ValorLiquidoNfse: Currency read FValorLiquidoNfse write FValorLiquidoNfse;
    property ValorIssRetido: Currency read FValorIssRetido write FValorIssRetido;
    property DescontoCondicionado: Currency read FDescontoCondicionado write FDescontoCondicionado;
    property DescontoIncondicionado: Currency read FDescontoIncondicionado write FDescontoIncondicionado;
    //Just. usada pelo provedor Equiplano
    property JustificativaDeducao: String read FJustificativaDeducao write FJustificativaDeducao;
    //propriedade do Provedor Governa
    property valorOutrasRetencoes: Currency read FvalorOutrasRetencoes write FvalorOutrasRetencoes;
    property DescricaoOutrasRetencoes: String read FDescricaoOutrasRetencoes write FDescricaoOutrasRetencoes;
    property ValorRepasse: Currency read FValorRepasse write FValorRepasse;
    //Provedor Infisc V 11
    property ValorDespesasNaoTributaveis: Currency read FValorDespesasNaoTributaveis write FValorDespesasNaoTributaveis;
    //Recife
    property ValorTotalRecebido: Currency read FValorTotalRecebido write FValorTotalRecebido;
  end;

  TItemServicoCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TItemServicoCollectionItem;
    procedure SetItem(Index: Integer; Value: TItemServicoCollectionItem);
  public
    function Add: TItemServicoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TItemServicoCollectionItem;
    property Items[Index: Integer]: TItemServicoCollectionItem read GetItem write SetItem; default;
  end;

  TItemServicoCollectionItem = class(TObject)
  private
    FDescricao : String;
//    FQuantidade : Integer;
    FQuantidade : Currency; 
    FValorUnitario : Currency;
    FValorTotal : Currency;
    FAliquota: Double;
    FValorIss: Currency;
    FBaseCalculo: Currency;
    FValorDeducoes: Currency;
    FValorServicos: Currency;
    FDescontoCondicionado: Currency;
    FDescontoIncondicionado: Currency;
    FDiscriminacao: String;
    FTributavel : TnfseSimNao;
    //Provedor: SystemPro
    FValorCsll: Currency;
    FValorPis: Currency;
    FValorCofins: Currency;
    FValorInss: Currency;
    FValorIr: Currency;
    FQuantidadeDiaria: Currency;
    FValorTaxaTurismo: Currency;
    //Provedor: Infisc
    FCodigo: String;
    // Provedor Infisc Versão XML 1.1
    FCodServ: String;
    FCodLCServ: String;
    FUnidade: String;
    FAlicotaISSST: currency;
    FValorISSST: currency;
    FpRetIR: currency;
    FvBCCSLL: currency;
    FpRetINSS: currency;
    FvBCINSS: currency;
    FvBCPISPASEP: currency;
    FvBCCOFINS: currency;
    FvBCRetIR: currency;
    FpRetCSLL: currency;
    FvDed: currency;
    FpRetPISPASEP: currency;
    FpRetCOFINS: currency;
    FvRed: currency;
    FTipoUnidade: TUnidade;
    FItemListaServico: String;

  public
    constructor Create;

    property Codigo: String read FCodigo write FCodigo;
    property Descricao: String read FDescricao write FDescricao;
//    property Quantidade: Integer read FQuantidade write FQuantidade;
    property Quantidade: Currency read FQuantidade write FQuantidade;
    property ValorUnitario: Currency read FValorUnitario write FValorUnitario;
    property ValorTotal : Currency read FValorTotal write FValorTotal;
    property ValorServicos: Currency read FValorServicos write FValorServicos;
    property ValorDeducoes: Currency read FValorDeducoes write FValorDeducoes;
    property ValorIss: Currency read FValorIss write FValorIss;
    property Aliquota: Double read FAliquota write FAliquota;
    property BaseCalculo: Currency read FBaseCalculo write FBaseCalculo;
    property DescontoCondicionado: Currency read FDescontoCondicionado write FDescontoCondicionado;
    property DescontoIncondicionado: Currency read FDescontoIncondicionado write FDescontoIncondicionado;
    property Discriminacao: String read FDiscriminacao write FDiscriminacao;
    property Tributavel : TnfseSimNao read FTributavel write FTributavel;
    //Provedor: SystemPro
    property ValorPis: Currency read FValorPis write FValorPis;
    property ValorCofins: Currency read FValorCofins write FValorCofins;
    property ValorInss: Currency read FValorInss write FValorInss;
    property ValorIr: Currency read FValorIr write FValorIr;
    property ValorCsll: Currency read FValorCsll write FValorCsll;
    property QuantidadeDiaria: Currency read FQuantidadeDiaria write FQuantidadeDiaria;
    property ValorTaxaTurismo: Currency read FValorTaxaTurismo write FValorTaxaTurismo;
    // Provedor Infisc Versão XML 1.1
    property CodServ     : String read FCodServ write FCodServ;
    property CodLCServ   : String read FCodLCServ write FCodLCServ;
    property Unidade     : String read FUnidade write FUnidade;
    property AlicotaISSST: currency read FAlicotaISSST write FAlicotaISSST;
    property ValorISSST  : currency read FValorISSST write FValorISSST;
    property vDed: currency read FvDed write FvDed;
    property vBCINSS: currency read FvBCINSS write FvBCINSS;
    property pRetINSS: currency read FpRetINSS write FpRetINSS;
    property vRed: currency read FvRed write FvRed;
    property vBCRetIR: currency read FvBCRetIR write FvBCRetIR;
    property pRetIR: currency read FpRetIR write FpRetIR;
    property vBCCOFINS: currency read FvBCCOFINS write FvBCCOFINS;
    property pRetCOFINS: currency read FpRetCOFINS write FpRetCOFINS;
    property vBCCSLL: currency read FvBCCSLL write FvBCCSLL;
    property pRetCSLL: currency read FpRetCSLL write FpRetCSLL;
    property vBCPISPASEP: currency read FvBCPISPASEP write FvBCPISPASEP;
    property pRetPISPASEP: currency read FpRetPISPASEP write FpRetPISPASEP;
    property TipoUnidade: TUnidade read FTipoUnidade write FTipoUnidade;
    property ItemListaServico: String read FItemListaServico write FItemListaServico;
  end;

 TDeducaoCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TDeducaoCollectionItem;
    procedure SetItem(Index: Integer; Value: TDeducaoCollectionItem);
  public
    function Add: TDeducaoCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDeducaoCollectionItem;
    property Items[Index: Integer]: TDeducaoCollectionItem read GetItem write SetItem; default;
  end;

 //classe usada no provedor IssDSF
 TDeducaoCollectionItem = class(TObject)
  private
    FDeducaoPor : TnfseDeducaoPor;
    FTipoDeducao : TnfseTipoDeducao;
    FCpfCnpjReferencia : String;
    FNumeroNFReferencia : String;
    FValorTotalReferencia : Currency;
    FPercentualDeduzir: Currency;
    FValorDeduzir: Currency;
  public
    property DeducaoPor : TnfseDeducaoPor read FDeducaoPor write FDeducaoPor;
    property TipoDeducao : TnfseTipoDeducao read FTipoDeducao write FTipoDeducao;
    property CpfCnpjReferencia : String read FCpfCnpjReferencia write FCpfCnpjReferencia;
    property NumeroNFReferencia : String read FNumeroNFReferencia write FNumeroNFReferencia;
    property ValorTotalReferencia : Currency read FValorTotalReferencia write FValorTotalReferencia;
    property PercentualDeduzir: Currency read FPercentualDeduzir write FPercentualDeduzir;
    property ValorDeduzir: Currency read FValorDeduzir write FValorDeduzir;
  end;

 TDadosServico = class(TObject)
  private
    FValores: TValores;
    FItemListaServico: String;
    FCodigoCnae: String;
    FCodigoTributacaoMunicipio: String;
    FDiscriminacao: String;
    FCodigoMunicipio: String;
    FCodigoPais: Integer;
    FExigibilidadeISS: TnfseExigibilidadeISS;
    FMunicipioIncidencia: Integer;
    FNumeroProcesso: String;
    FxItemListaServico: ansiString;
    FItemServico: TItemServicoCollection;
    FResponsavelRetencao: TnfseResponsavelRetencao;
    FDescricao: String;
    // Provedor IssDsf
    FDeducao: TDeducaoCollection;
    FOperacao: TOperacao;
    FTributacao: TTributacao;
    // Provedor Governa
    FUFPrestacao: String;
    // Provedor SP
    FValorCargaTributaria: currency;
    FPercentualCargaTributaria: currency;
    FFonteCargaTributaria: String;

    procedure SetItemServico(Value: TItemServicoCollection);
  public
    constructor Create;
    destructor Destroy; override;

    property Valores: TValores read FValores write FValores;
    property ItemListaServico: String read FItemListaServico write FItemListaServico;
    property CodigoCnae: String read FCodigoCnae write FCodigoCnae;
    property CodigoTributacaoMunicipio: String read FCodigoTributacaoMunicipio write FCodigoTributacaoMunicipio;
    property Discriminacao: String read FDiscriminacao write FDiscriminacao;
    property CodigoMunicipio: String read FCodigoMunicipio write FCodigoMunicipio;
    property CodigoPais: Integer read FCodigoPais write FCodigoPais;
    property ExigibilidadeISS: TnfseExigibilidadeISS read FExigibilidadeISS write FExigibilidadeISS;
    property MunicipioIncidencia: Integer read FMunicipioIncidencia write FMunicipioIncidencia;
    property NumeroProcesso: String read FNumeroProcesso write FNumeroProcesso;
    property xItemListaServico: ansiString read FxItemListaServico write FxItemListaServico;
    property ItemServico: TItemServicoCollection read FItemServico write SetItemServico;
    property ResponsavelRetencao: TnfseResponsavelRetencao read FResponsavelRetencao write FResponsavelRetencao;
    property Descricao: String read FDescricao write FDescricao;
    // Provedor IssDsf
    property Deducao: TDeducaoCollection read FDeducao write FDeducao;
    property Operacao: TOperacao read FOperacao write FOperacao;
    property Tributacao: TTributacao read FTributacao write FTributacao;
    // Provedor Governa
    property UFPrestacao: String read FUFPrestacao write FUFPrestacao;
    // Provedor SP
    property ValorCargaTributaria: currency read FValorCargaTributaria write FValorCargaTributaria;
    property PercentualCargaTributaria: currency read FPercentualCargaTributaria write FPercentualCargaTributaria;
    property FonteCargaTributaria: String read FFonteCargaTributaria write FFonteCargaTributaria;
  end;

 TIdentificacaoPrestador = class(TObject)
  private
    FCnpj: String;
    FInscricaoMunicipal: String;
    FSenha: String;
    FFraseSecreta: String;
    FcUF: Integer;
    FInscricaoEstadual: String;
    FChaveAcesso: String;
    FUsuario: Integer;
    FCNPJ_Prefeitura: String;
    FValorReceitaBruta: Currency;
    FDataInicioAtividade: TDateTime;
  public
    property Cnpj: String read FCnpj write FCnpj;
    property InscricaoMunicipal: String read FInscricaoMunicipal write FInscricaoMunicipal;
    // As propriedades abaixo são Utilizadas pelo provedor ISSDigital
    property Senha: String read FSenha write FSenha;
    property FraseSecreta: String read FFraseSecreta write FFraseSecreta;
    property cUF: Integer read FcUF write FcUF;
    property InscricaoEstadual: String read FInscricaoEstadual write FInscricaoEstadual;
    //Chave de Acesso usada no Provedor Governa
    property ChaveAcesso: String read FChaveAcesso write FChaveAcesso;
    // usado pelo provedor WebFisco
    property Usuario: Integer read FUsuario write FUsuario;
    property CNPJ_Prefeitura: String read FCNPJ_Prefeitura write FCNPJ_Prefeitura;
    property ValorReceitaBruta: Currency read FValorReceitaBruta write FValorReceitaBruta;
    property DataInicioAtividade: TDateTime read FDataInicioAtividade write FDataInicioAtividade;
  end;

 TEndereco = class(TObject)
  private
    FEnderecoInformado: Boolean;
    FTipoLogradouro: String;
    FEndereco: String;
    FNumero: String;
    FComplemento: String;
    FTipoBairro : String;
    FBairro: String;
    FCodigoMunicipio: String;
    FUF: String;
    FCEP: String;
    FxMunicipio: String;
    FCodigoPais: Integer;
    FxPais: String;
  public
    property EnderecoInformado: Boolean read FEnderecoInformado write FEnderecoInformado;
    property TipoLogradouro: String read FTipoLogradouro write FTipoLogradouro;
    property Endereco: String read FEndereco write FEndereco;
    property Numero: String read FNumero write FNumero;
    property Complemento: String read FComplemento write FComplemento;
    property TipoBairro: String read FTipoBairro write FTipoBairro;
    property Bairro: String read FBairro write FBairro;
    property CodigoMunicipio: String read FCodigoMunicipio write FCodigoMunicipio;
    property UF: String read FUF write FUF;
    property CEP: String read FCEP write FCEP;
    property xMunicipio: String read FxMunicipio write FxMunicipio;
    property CodigoPais: Integer read FCodigoPais write FCodigoPais; 
    property xPais: String read FxPais write FxPais;
  end;

 TContato = class(TObject)
  private
    FTelefone: String;
    FEmail: String;
    FDDD: String;
    FTipoTelefone: string;
  public
    property Telefone: String read FTelefone write FTelefone;
    property Email: String read FEmail write FEmail;
    property DDD: String read FDDD write FDDD;
    property TipoTelefone: string read FTipoTelefone write FTipoTelefone;
  end;

 TDadosPrestador = class(TObject)
  private
    FIdentificacaoPrestador: TIdentificacaoPrestador;
    FRazaoSocial: String;
    FNomeFantasia: String;
    FEndereco: TEndereco;
    FContato: TContato;
  public
    constructor Create;
    destructor Destroy; override;

    property IdentificacaoPrestador: TIdentificacaoPrestador read FIdentificacaoPrestador write FIdentificacaoPrestador;
    property RazaoSocial: String read FRazaoSocial write FRazaoSocial;
    property NomeFantasia: String read FNomeFantasia write FNomeFantasia;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property Contato: TContato read FContato write FContato;
  end;

  TIdentificacaoTomador = class(TObject)
  private
    FCpfCnpj: String;
    FInscricaoMunicipal: String;
    FInscricaoEstadual: String;
    FDocTomadorEstrangeiro: String;
  public
    property CpfCnpj: String read FCpfCnpj write FCpfCnpj;
    property InscricaoMunicipal: String read FInscricaoMunicipal write FInscricaoMunicipal;
    property InscricaoEstadual: String read FInscricaoEstadual write FInscricaoEstadual;
    property DocTomadorEstrangeiro: String read FDocTomadorEstrangeiro write FDocTomadorEstrangeiro;
  end;

 TDadosTomador = class(TObject)
  private
    FIdentificacaoTomador: TIDentificacaoTomador;
    FRazaoSocial: String;
    FEndereco: TEndereco;
    FContato: TContato;
    FAtualizaTomador: TnfseSimNao;
    FTomadorExterior: TnfseSimNao;
    FNifTomador: String;
  public
    constructor Create;
    destructor Destroy; override;

    property IdentificacaoTomador: TIdentificacaoTomador read FIdentificacaoTomador write FIdentificacaoTomador;
    property RazaoSocial: String read FRazaoSocial write FRazaoSocial;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property Contato: TContato read FContato write FContato;
    property AtualizaTomador: TnfseSimNao read FAtualizaTomador write FAtualizaTomador;
    property TomadorExterior: TnfseSimNao read FTomadorExterior write FTomadorExterior;
    property NifTomador: String read FNifTomador write FNifTomador;
  end;

 TIdentificacaoIntermediarioServico = class(TObject)
  private
    FRazaoSocial: String;
    FCpfCnpj: String;
    FInscricaoMunicipal: String;
    FIssRetido: TnfseSituacaoTributaria;
    FEMail: String;
  public
    property RazaoSocial: String read FRazaoSocial write FRazaoSocial;
    property CpfCnpj: String read FCpfCnpj write FCpfCnpj;
    property InscricaoMunicipal: String read FInscricaoMunicipal write FInscricaoMunicipal;
    property IssRetido: TnfseSituacaoTributaria read FIssRetido write FIssRetido;
    property EMail: String read FEMail write FEMail;
  end;

 TIdentificacaoOrgaoGerador = class(TObject)
  private
    FCodigoMunicipio: String;
    FUf: String;
  public
    property CodigoMunicipio: String read FCodigoMunicipio write FCodigoMunicipio;
    property Uf: String read FUf write FUf;
  end;

 TDadosConstrucaoCivil = class(TObject)
  private
    FCodigoObra: String;
    FArt: String;
    FLogradouroObra: string;
    FComplementoObra: string;
    FNumeroObra: string;
    FBairroObra: string;
    FCEPObra: string;
    FCodigoMunicipioObra: string;
    FUFObra: string;
    FCodigoPaisObra: integer;
    FxPaisObra: string;
    FnCei: string;
    FnProj: string;
    FnMatri: string;
    FnNumeroEncapsulamento : string;
  public
    property CodigoObra: String read FCodigoObra write FCodigoObra;
    property Art: String read FArt write FArt;
    property LogradouroObra: string read FLogradouroObra write FLogradouroObra;
    property ComplementoObra: string read FComplementoObra write FComplementoObra;
    property NumeroObra: string read FNumeroObra write FNumeroObra;
    property BairroObra: string read FBairroObra write FBairroObra;
    property CEPObra: string read FCEPObra write FCEPObra;
    property CodigoMunicipioObra: string read FCodigoMunicipioObra write FCodigoMunicipioObra;
    property UFObra: string read FUFObra write FUFObra;
    property CodigoPaisObra: integer read FCodigoPaisObra write FCodigoPaisObra;
    property xPaisObra: string read FxPaisObra write FxPaisObra;
    property nCei: String read FnCei write FnCei;
    property nProj: String read FnProj write FnProj;
    property nMatri: String read FnMatri write FnMatri;
    property nNumeroEncapsulamento : string read FnNumeroEncapsulamento write FnNumeroEncapsulamento;
  end;

  TParcelasCollectionItem = class(TObject)
  private
    FCondicao: TnfseCondicaoPagamento;
    FParcela: Integer;
    FDataVencimento: TDateTime;
    FValor: Currency;
  public
    property Condicao: TnfseCondicaoPagamento read FCondicao write FCondicao;
    property Parcela: Integer read FParcela write FParcela;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
    property Valor: Currency read FValor write FValor;
  end;

  TParcelasCollection = class(TObjectList)
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

 TemailCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TemailCollectionItem;
    procedure SetItem(Index: Integer; Value: TemailCollectionItem);
  public
    function Add: TemailCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TemailCollectionItem;
    property Items[Index: Integer]: TemailCollectionItem read GetItem write SetItem; default;
  end;

 TemailCollectionItem = class(TObject)
  private
    FemailCC: String;
  public
    property emailCC: String read FemailCC write FemailCC;
  end;

 TDadosTransportadora = class(TObject)
  private
    FxNomeTrans: String;
    FxCpfCnpjTrans: String;
    FxInscEstTrans: String;
    FxPlacaTrans: String;
    FxEndTrans: String;
    FcMunTrans: Integer;
    FxMunTrans: String;
    FxUFTrans: String;
    FcPaisTrans: Integer;
    FxPaisTrans: String;
    FvTipoFreteTrans: TnfseFrete;
  public
    property xNomeTrans: String read FxNomeTrans write FxNomeTrans;
    property xCpfCnpjTrans: String read FxCpfCnpjTrans write FxCpfCnpjTrans;
    property xInscEstTrans: String read FxInscEstTrans write FxInscEstTrans;
    property xPlacaTrans: String read FxPlacaTrans write FxPlacaTrans;
    property xEndTrans: String read FxEndTrans write FxEndTrans;
    property cMunTrans: Integer read FcMunTrans write FcMunTrans;
    property xMunTrans: String read FxMunTrans write FxMunTrans;
    property xUFTrans: String read FxUFTrans write FxUFTrans;
    property cPaisTrans: Integer read FcPaisTrans write FcPaisTrans;
    property xPaisTrans: String read FxPaisTrans write FxPaisTrans;
    property vTipoFreteTrans: TnfseFrete read FvTipoFreteTrans write FvTipoFreteTrans;
  end;

  TDespesaCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TDespesaCollectionItem;
    procedure SetItem(Index: Integer; Value: TDespesaCollectionItem);
  public
    function Add: TDespesaCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TDespesaCollectionItem;
    property Items[Index: Integer]: TDespesaCollectionItem read GetItem write SetItem;
  end;

  TDespesaCollectionItem = class(TObject)
  private
    FnItemDesp: String;
    FxDesp: String;
    FdDesp: TDateTime;
    FvDesp: Currency;
  public
    property nItemDesp: String read FnItemDesp write FnItemDesp;
    property xDesp: String read FxDesp write FxDesp;
    property dDesp: TDateTime read FdDesp write FdDesp;
    property vDesp: Currency read FvDesp write FvDesp;
  end;

  TAssinaComChaveParamsCollectionItem = class(TObject)
  private
    FParam: String;
    FConteudo: String;
  public
    property Param: String read FParam write FParam;
    property Conteudo: String read FConteudo write FConteudo;
  end;

  TAssinaComChaveParamsCollection = class(TObjectList)
  private
    function GetItem(Index: Integer): TAssinaComChaveParamsCollectionItem;
    procedure SetItem(Index: Integer; Const Value: TAssinaComChaveParamsCollectionItem);
  public
    function Add: TAssinaComChaveParamsCollectionItem; overload; deprecated {$IfDef SUPPORTS_DEPRECATED_DETAILS} 'Obsoleta: Use a função New'{$EndIf};
    function New: TAssinaComChaveParamsCollectionItem;
    property Items[Index: Integer]: TAssinaComChaveParamsCollectionItem read GetItem write SetItem; default;
  end;

 TNFSe = class(TPersistent)
  private
    // RPS e NFSe
    FNomeArq: String;
    FInfID: TInfID;
    FIdentificacaoRps: TIdentificacaoRps;
    FDataEmissao: TDateTime;
    FDataEmissaoRps: TDateTime;
    FNaturezaOperacao: TnfseNaturezaOperacao;
    FRegimeEspecialTributacao: TnfseRegimeEspecialTributacao;
    FOptanteSimplesNacional: TnfseSimNao;
    //Provedor Conam
    FDataOptanteSimplesNacional: TDateTime;
    FLogradouroLocalPrestacaoServico: TnfseLogradouroLocalPrestacaoServico;
    FIncentivadorCultural: TnfseSimNao;
    FProducao: TnfseSimNao;
    FStatus: TnfseStatusRps;
    FRpsSubstituido: TIdentificacaoRps;
    FSeriePrestacao: String;
    FServico: TDadosServico;
    FPrestador: TIdentificacaoPrestador;
    FTomador: TDadosTomador;
    FIntermediarioServico: TIdentificacaoIntermediarioServico;
    FConstrucaoCivil: TDadosConstrucaoCivil;
    FDeducaoMateriais: TnfseSimNao;
    FCondicaoPagamento: TCondicaoPagamento;
    // NFSe
    FNumero: String;
    FCodigoVerificacao: String;
    FCompetencia: String;
    FNfseSubstituida: String;
    FOutrasInformacoes: String;
    FValorCredito: Currency;
    FPrestadorServico: TDadosPrestador;
    FOrgaoGerador: TIdentificacaoOrgaoGerador;
    FValoresNfse: TValoresNfse;
    FAutenticador: String; // para provedor EGoverneISS
    FLink: String; // para provedor EGoverneISS
    // RPS e NFSe
    FDespesa: TDespesaCollection;

    FNumeroLote: String;
    FProtocolo: String;
    FdhRecebimento: TDateTime;
    FSituacao: String;

    FXML: AnsiString;

    FNfseCancelamento: TConfirmacaoCancelamento;
    FNfseSubstituidora: String;
    FMotivoCancelamento: String; // para provedor ISSDSF
    FChaveNFSe: String; // para provedor Infisc

    // Provedor Infisc Versão XML 1.1
    FTipoEmissao: TnfseTEmissao;
    FEmpreitadaGlobal: TnfseTEmpreitadaGlobal;
    FModeloNFSe: String;
    FCancelada: TnfseSimNao;
    FTransportadora: TDadosTransportadora;
    FCanhoto: TnfseCanhoto;

    Femail: TemailCollection;
    FTipoRecolhimento: String;
    FRegRec: TnfseRegRec; //String;
    FFrmRec: TnfseFrmRec; //String;
    FTipoTributacaoRPS: TnfseTTributacaoRPS;
    FAssinatura: String;
    FInformacoesComplementares: String;

    FAssinaComChaveParams: TAssinaComChaveParamsCollection;

    procedure Setemail(const Value: TemailCollection);
    procedure SetInformacoesComplementares(const Value: String);

  public
    constructor Create;
    destructor Destroy; override;
  published
    // RPS e NFSe
    property NomeArq: String read FNomeArq write FNomeArq;
    property InfID: TInfID read FInfID write FInfID;
    property IdentificacaoRps: TIdentificacaoRps read FIdentificacaoRps write FIdentificacaoRps;
    property DataEmissao: TDateTime read FDataEmissao write FDataEmissao;
    property DataEmissaoRps: TDateTime read FDataEmissaoRps write FDataEmissaoRps;
    property NaturezaOperacao: TnfseNaturezaOperacao read FNaturezaOperacao write FNaturezaOperacao;
    property RegimeEspecialTributacao: TnfseRegimeEspecialTributacao read FRegimeEspecialTributacao write FRegimeEspecialTributacao;
    property OptanteSimplesNacional: TnfseSimNao read FOptanteSimplesNacional write FOptanteSimplesNacional;
    //Provedor Conam
    property DataOptanteSimplesNacional: TDateTime read FDataOptanteSimplesNacional write FDataOptanteSimplesNacional;
    property LogradouLocalPrestacaoServico: TnfseLogradouroLocalPrestacaoServico read FLogradouroLocalPrestacaoServico write FLogradouroLocalPrestacaoServico;
    property IncentivadorCultural: TnfseSimNao read FIncentivadorCultural write FIncentivadorCultural;
    property Producao: TnfseSimNao read FProducao write FProducao;
    property Status: TnfseStatusRps read FStatus write FStatus;
    property RpsSubstituido: TIdentificacaoRps read FRpsSubstituido write FRpsSubstituido;
    //SeriePrestacao usada no provedor IssDsf
    property SeriePrestacao: String read FSeriePrestacao write FSeriePrestacao;
    property Servico: TDadosServico read FServico write FServico;
    property Prestador: TIdentificacaoPrestador read FPrestador write FPrestador;
    property Tomador: TDadosTomador read FTomador write FTomador;
    property IntermediarioServico: TIdentificacaoIntermediarioServico read FIntermediarioServico write FIntermediarioServico;
    property ConstrucaoCivil: TDadosConstrucaoCivil read FConstrucaoCivil write FConstrucaoCivil;
    property DeducaoMateriais: TnfseSimNao read FDeducaoMateriais write FDeducaoMateriais;
    property CondicaoPagamento: TCondicaoPagamento read FCondicaoPagamento write FCondicaoPagamento;
    // NFSe
    property Numero: String read FNumero write FNumero;
    property CodigoVerificacao: String read FCodigoVerificacao write FCodigoVerificacao;
    property Competencia: String read FCompetencia write FCompetencia;
    property NfseSubstituida: String read FNfseSubstituida write FNfseSubstituida;
    property OutrasInformacoes: String read FOutrasInformacoes write FOutrasInformacoes;
    property InformacoesComplementares: String read FInformacoesComplementares write SetInformacoesComplementares;
    property ValorCredito: Currency read FValorCredito write FValorCredito;
    property PrestadorServico: TDadosPrestador read FPrestadorServico write FPrestadorServico;
    property OrgaoGerador: TIdentificacaoOrgaoGerador read FOrgaoGerador write FOrgaoGerador;
    property ValoresNfse: TValoresNfse read FValoresNfse write FValoresNfse;
    // propriedades para provedor EGoverneISS
    property Autenticador: String read FAutenticador write FAutenticador;
    property Link: String read FLink write FLink;
    property NumeroLote: String read FNumeroLote write FNumeroLote;
    property Protocolo: String read FProtocolo write FProtocolo;
    property dhRecebimento: TDateTime read FdhRecebimento write FdhRecebimento;
    property Situacao: String read FSituacao write FSituacao;
    property XML: AnsiString read FXML write FXML;
    property NfseCancelamento: TConfirmacaoCancelamento read FNfseCancelamento write FNfseCancelamento;
    property NfseSubstituidora: String read FNfseSubstituidora write FNfseSubstituidora;
    property MotivoCancelamento: String read FMotivoCancelamento write FMotivoCancelamento; // para provedor ISSDSF
    property ChaveNFSe: String read FChaveNFSe write FChaveNFSe; // para provedor Infisc
    // Provedor Infisc Versão XML 1.1
    property TipoEmissao: TnfseTEmissao read FTipoEmissao write FTipoEmissao;
    property EmpreitadaGlobal: TnfseTEmpreitadaGlobal read FEmpreitadaGlobal write FEmpreitadaGlobal;
    property ModeloNFSe: String read FModeloNFSe write FModeloNFSe;
    property Cancelada: TnfseSimNao read FCancelada write FCancelada;
    property Canhoto: TnfseCanhoto read FCanhoto Write FCanhoto;
    property Transportadora: TDadosTransportadora read FTransportadora write FTransportadora;
    property Despesa: TDespesaCollection read FDespesa write FDespesa;
    // propriedade para provedor Governa
    property TipoRecolhimento: String read FTipoRecolhimento write FTipoRecolhimento;

    property email: TemailCollection read Femail write Setemail;

    property TipoTributacaoRPS: TnfseTTributacaoRPS read FTipoTributacaoRPS write FTipoTributacaoRPS;

    property AssinaComChaveParams: TAssinaComChaveParamsCollection read FAssinaComChaveParams write FAssinaComChaveParams;

    // Provedor SP
    property Assinatura: String read FAssinatura write FAssinatura;
    property RegRec: TnfseRegRec read FRegRec write FRegRec; //Governa
    property FrmRec: TnfseFrmRec read FFrmRec write FFrmRec; //Governa
  end;

 TLoteRps = class(TObject)
  private
    FInfID: TInfID;
    FNumeroLote: String;
    FCnpj: String;
    FInscricaoMunicipal: String;
    FQuantidadeRps: String;
  public
    constructor Create;
    destructor Destroy; override;

    property InfID: TInfID read FInfID write FInfID;
    property NumeroLote: String read FNumeroLote write FNumeroLote;
    property Cnpj: String read FCnpj write FCnpj;
    property InscricaoMunicipal: String read FInscricaoMunicipal write FInscricaoMunicipal;
    property QuantidadeRps: String read FQuantidadeRps write FQuantidadeRps;
  end;

 TPedidoCancelamento = class(TObject)
  private
    FInfID: TInfID;
    FIdentificacaoNfse: TIdentificacaoNfse;
    FCodigoCancelamento: String;
  public
    constructor Create;
    destructor Destroy; override;

    property InfID: TInfID read FInfID write FInfID;
    property IdentificacaoNfse: TIdentificacaoNfse read FIdentificacaoNfse write FIdentificacaoNfse;
    property CodigoCancelamento: String read FCodigoCancelamento write FCodigoCancelamento;
  end;

 TConfirmacaoCancelamento = class(TObject)
  private
    FInfID: TInfID;
    FPedido: TPedidoCancelamento;
    FDataHora: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;

    property InfID: TInfID read FInfID write FInfID;
    property Pedido: TPedidoCancelamento read FPedido write FPedido;
    property DataHora: TDateTime read FDataHora write FDataHora;
  end;

 TSubstituicaoNfse = class(TObject)
  private
    FInfID: TInfID;
    FNfseSubstituidora: String;
  public
    constructor Create;
    destructor Destroy; override;

    property InfID: TInfID read FInfID write FInfID;
    property NfseSubstituidora: String read FNfseSubstituidora write FNfseSubstituidora;
  end;

const
  CMUN_EXTERIOR: Integer = 9999999;
  XMUN_EXTERIOR: String = 'EXTERIOR';
  UF_EXTERIOR: String = 'EX';

implementation

{ TDadosServicoRPS }

constructor TDadosServico.Create;
begin
 inherited Create;

 FValores := TValores.Create;

 with FValores do
  begin
   FValorServicos          := 0;
   FValorDeducoes          := 0;
   FValorPis               := 0;
   FValorCofins            := 0;
   FValorInss              := 0;
   FValorIr                := 0;
   FValorCsll              := 0;
   FIssRetido              := stNormal;
   FValorIss               := 0;
   FValorIssRetido         := 0;
   FOutrasRetencoes        := 0;
   FBaseCalculo            := 0;
   FAliquota               := 0;
   FValorLiquidoNfse       := 0;
   FDescontoIncondicionado := 0;
   FDescontoCondicionado   := 0;
   FValorDespesasNaoTributaveis := 0;
  end;

 FItemServico := TItemServicoCollection.Create;
 FDeducao     := TDeducaoCollection.Create;
 FDescricao   := '';
 
end;

destructor TDadosServico.Destroy;
begin
 FValores.Free;
 FItemServico.Free;
 FDeducao.Free;
 
 inherited destroy;
end;

procedure TDadosServico.SetItemServico(Value: TItemServicoCollection);
begin
 FItemServico.Assign(Value);
end;

{ TDadosPrestador }

constructor TDadosPrestador.Create;
begin
 inherited Create;

 FIdentificacaoPrestador := TIdentificacaoPrestador.Create;
 FEndereco               := TEndereco.Create;
 FContato                := TContato.Create;

 with FIdentificacaoPrestador do
  begin
   Cnpj               := '';
   InscricaoMunicipal := '';
   InscricaoEstadual  := '';
  end;
end;

destructor TDadosPrestador.Destroy;
begin
 FIdentificacaoPrestador.Free;
 FEndereco.Free;
 FContato.Free;

 inherited destroy;
end;

{ TDadosTomador }

constructor TDadosTomador.Create;
begin
 inherited Create;

 FIdentificacaoTomador := TIdentificacaoTomador.Create;
 FEndereco             := TEndereco.Create;
 FContato              := TContato.Create;
end;

destructor TDadosTomador.Destroy;
begin
 FIdentificacaoTomador.Free;
 FEndereco.Free;
 FContato.Free;

 inherited destroy;
end;

{ TNFSe }

constructor TNFSe.Create;
begin
 inherited create;
 // RPS e NFSe
 FNomeArq                      := '';
 FInfID                        := TInfID.Create;
 FIdentificacaoRps             := TIdentificacaoRps.Create;
 FIdentificacaoRps.FTipo       := trRPS;
 FDataEmissao                  := 0;
 FNaturezaOperacao             := no1;
 FRegimeEspecialTributacao     := retNenhum;
 FOptanteSimplesNacional       := snNao;
 FIncentivadorCultural         := snNao;
 FStatus                       := srNormal;
 FRpsSubstituido               := TIdentificacaoRps.Create;
 FRpsSubstituido.FTipo         := trRPS;
 FServico                      := TDadosServico.Create;
 FPrestador                    := TIdentificacaoPrestador.Create;
 FPrestador.Cnpj               := '';
 FPrestador.InscricaoMunicipal := '';
 FTomador                      := TDadosTomador.Create;
 FIntermediarioServico         := TIdentificacaoIntermediarioServico.Create;
 FConstrucaoCivil              := TDadosConstrucaoCivil.Create;
 FCondicaoPagamento            := TCondicaoPagamento.Create;
 // NFSe
 FNumero                       := '';
 FCodigoVerificacao            := '';
 FCompetencia                  := '';
 FNfseSubstituida              := '';
 FOutrasInformacoes            := '';
 FInformacoesComplementares    := '';
 FValorCredito                 := 0;
 FPrestadorServico             := TDadosPrestador.Create;
 FOrgaoGerador                 := TIdentificacaoOrgaoGerador.Create;
 FValoresNfse                  := TValoresNfse.Create;
 // RPS e NFSe
 FNfseCancelamento             := TConfirmacaoCancelamento.Create;
 FNfseCancelamento.DataHora    := 0;
 FNfseSubstituidora            := '';

// Provedor Infisc Versão XML 1.1
 FTipoEmissao                  := teNormalNFSe;
 FEmpreitadaGlobal             := EgOutros;
 FModeloNFSe                   := '55';
 FCancelada                    := snNao;
 FCanhoto                      := tcNenhum;
 FTransportadora               := TDadosTransportadora.Create;

 FLogradouroLocalPrestacaoServico := llpTomador;

 Femail                        := TemailCollection.Create;
 FDespesa                      := TDespesaCollection.Create;

 FAssinaComChaveParams         := TAssinaComChaveParamsCollection.Create;
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
 FIntermediarioServico.Free;
 FConstrucaoCivil.Free;
 FCondicaoPagamento.Free;
 // NFSe
 FPrestadorServico.Free;
 FOrgaoGerador.Free;
 FValoresNfse.Free;
 // RPS e NFSe
 FNfseCancelamento.Free;
 Femail.Free;
 FDespesa.Free;

 FAssinaComChaveParams.Free;

 FTransportadora.Free;

 inherited Destroy;
end;

procedure TNFSe.SetInformacoesComplementares(const Value: String);
begin
  FInformacoesComplementares := Value;
end;

procedure TNFSe.Setemail(const Value: TemailCollection);
begin
  Femail := Value;
end;

{ TLoteRps }

constructor TLoteRps.Create;
begin
 inherited create;
 FInfID              := TInfID.Create;
 FNumeroLote         := '';
 FCnpj               := '';
 FInscricaoMunicipal := '';
 FQuantidadeRps      := '';
end;

destructor TLoteRps.Destroy;
begin
 FInfID.Free;

 inherited Destroy;
end;

{ TPedidoCancelamento }

constructor TPedidoCancelamento.Create;
begin
  inherited Create;
  FInfID              := TInfID.Create;
  FIdentificacaoNfse  := TIdentificacaoNfse.Create;
  FCodigoCancelamento := '';
end;

destructor TPedidoCancelamento.Destroy;
begin
  FInfID.Free;
  FIdentificacaoNfse.Free;
  inherited;
end;

{ TConfirmacaoCancelamento }

constructor TConfirmacaoCancelamento.Create;
begin
  inherited Create;
  FInfID     := TInfID.Create;
  FPedido    := TPedidoCancelamento.Create;
end;

destructor TConfirmacaoCancelamento.Destroy;
begin
  FInfID.Free;
  FPedido.Free;
  inherited;
end;

{ TSubstituicaoNfse }

constructor TSubstituicaoNfse.Create;
begin
  inherited Create;
  FInfID             := TInfID.Create;
  FNfseSubstituidora := '';
end;

destructor TSubstituicaoNfse.Destroy;
begin
  FInfID.Free;
  inherited;
end;

{ TItemServicoCollection }

function TItemServicoCollection.Add: TItemServicoCollectionItem;
begin
  Result := Self.New;
end;

function TItemServicoCollection.GetItem(Index: Integer): TItemServicoCollectionItem;
begin
  Result := TItemServicoCollectionItem(inherited GetItem(Index));
end;

procedure TItemServicoCollection.SetItem(Index: Integer;
  Value: TItemServicoCollectionItem);
begin
  inherited SetItem(Index, Value);
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
  Result := TDeducaoCollectionItem(inherited GetItem(Index));
end;

procedure TDeducaoCollection.SetItem(Index: Integer;
  Value: TDeducaoCollectionItem);
begin
  inherited SetItem(Index, Value);
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
  FCodigo  := '';
  FCodServ := '';
  FUnidade := 'UN';
end;

{ TParcelasCollection }
function TParcelasCollection.Add: TParcelasCollectionItem;
begin
  Result := Self.New;
end;

function TParcelasCollection.GetItem(Index: Integer): TParcelasCollectionItem;
begin
  Result := TParcelasCollectionItem(inherited GetItem(Index));
end;

procedure TParcelasCollection.SetItem(Index: Integer;
  const Value: TParcelasCollectionItem);
begin
  inherited SetItem(Index, Value);
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
  inherited;
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
  Result := TemailCollectionItem(inherited GetItem(Index));
end;

procedure TemailCollection.SetItem(Index: Integer;
  Value: TemailCollectionItem);
begin
  inherited SetItem(Index, Value);
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
  Result := Inherited GetItem(Index) as TDespesaCollectionItem;
end;

procedure TDespesaCollection.SetItem(Index: Integer; Value: TDespesaCollectionItem);
begin
  Inherited SetItem(Index, Value);
end;

function TDespesaCollection.New: TDespesaCollectionItem;
begin
  Result := TDespesaCollectionItem.Create;
  Self.Add(Result);
end;

{ TAssinaComChaveParamsCollection }

function TAssinaComChaveParamsCollection.Add: TAssinaComChaveParamsCollectionItem;
begin
  Result := Self.New;
end;

function TAssinaComChaveParamsCollection.GetItem(
  Index: Integer): TAssinaComChaveParamsCollectionItem;
begin
  Result := TAssinaComChaveParamsCollectionItem(inherited GetItem(Index));
end;

procedure TAssinaComChaveParamsCollection.SetItem(Index: Integer;
  const Value: TAssinaComChaveParamsCollectionItem);
begin
  inherited SetItem(Index, Value);
end;

function TAssinaComChaveParamsCollection.New: TAssinaComChaveParamsCollectionItem;
begin
  Result := TAssinaComChaveParamsCollectionItem.Create;
  Self.Add(Result);
end;

end.
