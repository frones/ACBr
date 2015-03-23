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
  SysUtils, Classes,
  {$IFNDEF VER130}
    Variants,
  {$ENDIF}
  pnfsConversao, pnfsSignature;

type

 TInfID                             = class;
 TIdentificacaoRps                  = class;
 TIdentificacaoNfse                 = class;
 TValoresNfse                       = class;
 TValores                           = class;
 TItemServicoCollection             = class;
 TItemServicoCollectionItem         = class;
 TDeducaoCollection                 = class;
 TDeducaoCollectionItem             = class;
 TDadosServico                      = class;
 TIdentificacaoPrestador            = class;
 TEndereco                          = class;
 TContato                           = class;
 TDadosPrestador                    = class;
 TIdentificacaoTomador              = class;
 TDadosTomador                      = class;
 TIdentificacaoIntermediarioServico = class;
 TIdentificacaoOrgaoGerador         = class;
 TDadosConstrucaoCivil              = class;
 TParcelasCollectionItem            = class;
 TParcelasCollection                = class;
 TCondicaoPagamento                 = class;

 TNFSe                              = class;

 TLoteRps                           = class;

 TPedidoCancelamento                = class;
 TConfirmacaoCancelamento           = class;
 TSubstituicaoNfse                  = class;

 TInfID = class(TPersistent)
  private
    FID: string;
  published
    property ID: string read FID write FID;
  end;

 TIdentificacaoRps = class(TPersistent)
  private
    FNumero: string;
    FSerie: string;
    FTipo: TnfseTipoRps;
  published
    property Numero: string read FNumero write FNumero;
    property Serie: string read FSerie write FSerie;
    property Tipo: TnfseTipoRps read FTipo write FTipo;
  end;

 TIdentificacaoNfse = class(TPersistent)
  private
    FNumero: string;
    FCnpj: string;
    FInscricaoMunicipal: string;
    FCodigoMunicipio: string;
  published
    property Numero: string read FNumero write FNumero;
    property Cnpj: string read FCnpj write FCnpj;
    property InscricaoMunicipal: string read FInscricaoMunicipal write FInscricaoMunicipal;
    property CodigoMunicipio: string read FCodigoMunicipio write FCodigoMunicipio;
  end;

 TValoresNfse = class(TPersistent)
 private
   FBaseCalculo: currency;
   FAliquota: currency;
   FValorIss: currency;
   FValorLiquidoNfse: currency;
 published
   property BaseCalculo: currency read FBaseCalculo write FBaseCalculo;
   property Aliquota: currency read FAliquota write FAliquota;
   property ValorIss: currency read FValorIss write FValorIss;
   property ValorLiquidoNfse: currency read FValorLiquidoNfse write FValorLiquidoNfse;
 end;

 TValores = class(TPersistent)
  private
    FValorServicos: currency;
    FValorDeducoes: currency;
    FValorPis: currency;
    FValorCofins: currency;
    FValorInss: currency;
    FValorIr: currency;
    FValorCsll: currency;
    FIssRetido: TnfseSituacaoTributaria;
    FValorIss: currency;
    FOutrasRetencoes: currency;
    FBaseCalculo: currency;
    FAliquota: currency;
    FAliquotaPis: currency;
    FAliquotaCofins: currency;
    FAliquotaInss: currency;
    FAliquotaIr: currency;
    FAliquotaCsll: currency;
    FValorLiquidoNfse: currency;
    FValorIssRetido: currency;
    FDescontoCondicionado: currency;
    FDescontoIncondicionado: currency;
    FJustificativaDeducao: String;
  published
    property ValorServicos: currency read FValorServicos write FValorServicos;
    property ValorDeducoes: currency read FValorDeducoes write FValorDeducoes;
    property ValorPis: currency read FValorPis write FValorPis;
    property ValorCofins: currency read FValorCofins write FValorCofins;
    property ValorInss: currency read FValorInss write FValorInss;
    property ValorIr: currency read FValorIr write FValorIr;
    property ValorCsll: currency read FValorCsll write FValorCsll;
    property IssRetido: TnfseSituacaoTributaria read FIssRetido write FIssRetido;
    property ValorIss: currency read FValorIss write FValorIss;
    property OutrasRetencoes: currency read FOutrasRetencoes write FOutrasRetencoes;
    property BaseCalculo: currency read FBaseCalculo write FBaseCalculo;
    property Aliquota: currency read FAliquota write FAliquota;
    // Aliquotas usadas pelo Provedor IssDsf
    property AliquotaPis: currency read FAliquotaPis write FAliquotaPis;
    property AliquotaCofins: currency read FAliquotaCofins write FAliquotaCofins;
    property AliquotaInss: currency read FAliquotaInss write FAliquotaInss;
    property AliquotaIr: currency read FAliquotaIr write FAliquotaIr;
    property AliquotaCsll: currency read FAliquotaCsll write FAliquotaCsll;

    property ValorLiquidoNfse: currency read FValorLiquidoNfse write FValorLiquidoNfse;
    property ValorIssRetido: currency read FValorIssRetido write FValorIssRetido;
    property DescontoCondicionado: currency read FDescontoCondicionado write FDescontoCondicionado;
    property DescontoIncondicionado: currency read FDescontoIncondicionado write FDescontoIncondicionado;
    //Just. usada pelo provedor Equiplano
    property JustificativaDeducao: String read FJustificativaDeducao write FJustificativaDeducao;
  end;

  TItemServicoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TItemServicoCollectionItem;
    procedure SetItem(Index: Integer; Value: TItemServicoCollectionItem);
  public
    constructor Create(AOwner: TDadosServico);
    function Add: TItemServicoCollectionItem;
    property Items[Index: Integer]: TItemServicoCollectionItem read GetItem write SetItem; default;
  end;

  TItemServicoCollectionItem = class(TCollectionItem)
  private
    FDescricao : String;
//    FQuantidade : Integer;
    FQuantidade : currency; // Alterado pois o provedor SimplISS aceita quantidade de servico não inteira.
    FValorUnitario : currency;
    FValorTotal : currency;
    FAliquota: currency;
    FValorIss: currency;
    FBaseCalculo: currency;
    FValorDeducoes: currency;
    FValorServicos: currency;
    FDescontoCondicionado: currency;
    FDescontoIncondicionado: currency;
    FDiscriminacao: string;
    FTributavel : TnfseSimNao;
    //Provedor: SystemPro
    FValorCsll: currency;
    FValorPis: currency;
    FValorCofins: currency;
    FValorInss: currency;
    FValorIr: currency;
    //Provedor: Infisc
    FCodigo: string;

    // Alterado Por Moro em 18/02/2015 - Provedor Infisc Versão XML 1.1
    FCodServ: string;
    FCodLCServ: string;
    FUnidade: string;

  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property Codigo: string read FCodigo write FCodigo;
    property Descricao: string read FDescricao write FDescricao;
//    property Quantidade: Integer read FQuantidade write FQuantidade;
    property Quantidade: currency read FQuantidade write FQuantidade;
    property ValorUnitario: currency read FValorUnitario write FValorUnitario;
    property ValorTotal : currency read FValorTotal write FValorTotal;
    property ValorServicos: currency read FValorServicos write FValorServicos;
    property ValorDeducoes: currency read FValorDeducoes write FValorDeducoes;
    property ValorIss: currency read FValorIss write FValorIss;
    property Aliquota: currency read FAliquota write FAliquota;
    property BaseCalculo: currency read FBaseCalculo write FBaseCalculo;
    property DescontoCondicionado: currency read FDescontoCondicionado write FDescontoCondicionado;
    property DescontoIncondicionado: currency read FDescontoIncondicionado write FDescontoIncondicionado;
    property Discriminacao: string read FDiscriminacao write FDiscriminacao;
    property Tributavel : TnfseSimNao read FTributavel write FTributavel;
    //Provedor: SystemPro
    property ValorPis: currency read FValorPis write FValorPis;
    property ValorCofins: currency read FValorCofins write FValorCofins;
    property ValorInss: currency read FValorInss write FValorInss;
    property ValorIr: currency read FValorIr write FValorIr;
    property ValorCsll: currency read FValorCsll write FValorCsll;

    // Alterado Por Moro em 18/02/2015 - Provedor Infisc Versão XML 1.1
    property CodServ     : string read FCodServ write FCodServ;
    property CodLCServ   : string read FCodLCServ write FCodLCServ;
    property Unidade     : string read FUnidade write FUnidade;
  end;

 TDeducaoCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDeducaoCollectionItem;
    procedure SetItem(Index: Integer; Value: TDeducaoCollectionItem);
  public
    constructor Create(AOwner: TDadosServico);
    function Add: TDeducaoCollectionItem;
    property Items[Index: Integer]: TDeducaoCollectionItem read GetItem write SetItem; default;
  end;

 //classe usada no provedor IssDSF
 TDeducaoCollectionItem = class(TCollectionItem)
  private
    FDeducaoPor : TnfseDeducaoPor;
    FTipoDeducao : TnfseTipoDeducao;
    FCpfCnpjReferencia : string;
    FNumeroNFReferencia : String;
    FValorTotalReferencia : currency;
    FPercentualDeduzir: currency;
    FValorDeduzir: currency;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property DeducaoPor : TnfseDeducaoPor read FDeducaoPor write FDeducaoPor;
    property TipoDeducao : TnfseTipoDeducao read FTipoDeducao write FTipoDeducao;
    property CpfCnpjReferencia : string read FCpfCnpjReferencia write FCpfCnpjReferencia;
    property NumeroNFReferencia : String read FNumeroNFReferencia write FNumeroNFReferencia;
    property ValorTotalReferencia : currency read FValorTotalReferencia write FValorTotalReferencia;
    property PercentualDeduzir: currency read FPercentualDeduzir write FPercentualDeduzir;
    property ValorDeduzir: currency read FValorDeduzir write FValorDeduzir;
  end;

 TDadosServico = class(TPersistent)
  private
    FValores: TValores;
    FItemListaServico: string;
    FCodigoCnae: string;
    FCodigoTributacaoMunicipio: string;
    FDiscriminacao: string;
    FCodigoMunicipio: string;
    FCodigoPais: Integer;
    FExigibilidadeISS: TnfseExigibilidadeISS;
    FMunicipioIncidencia: Integer;
    FNumeroProcesso: string;
    FxItemListaServico: ansistring;
    FItemServico: TItemServicoCollection;
    FResponsavelRetencao: TnfseResponsavelRetencao;
    FDescricao: string;
    FDeducao : TDeducaoCollection;

    procedure SetItemServico(Value: TItemServicoCollection);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Valores: TValores read FValores write FValores;
    property ItemListaServico: string read FItemListaServico write FItemListaServico;
    property CodigoCnae: string read FCodigoCnae write FCodigoCnae;
    property CodigoTributacaoMunicipio: string read FCodigoTributacaoMunicipio write FCodigoTributacaoMunicipio;
    property Discriminacao: string read FDiscriminacao write FDiscriminacao;
    property CodigoMunicipio: string read FCodigoMunicipio write FCodigoMunicipio;
    property CodigoPais: Integer read FCodigoPais write FCodigoPais;
    property ExigibilidadeISS: TnfseExigibilidadeISS read FExigibilidadeISS write FExigibilidadeISS;
    property MunicipioIncidencia: Integer read FMunicipioIncidencia write FMunicipioIncidencia;
    property NumeroProcesso: string read FNumeroProcesso write FNumeroProcesso;
    property xItemListaServico: ansistring read FxItemListaServico write FxItemListaServico;
    property ItemServico: TItemServicoCollection read FItemServico write SetItemServico;
    property ResponsavelRetencao: TnfseResponsavelRetencao read FResponsavelRetencao write FResponsavelRetencao;
    property Descricao: string read FDescricao write FDescricao;
    // Deducao usada pelo Provedor IssDsf
    property Deducao : TDeducaoCollection read FDeducao write FDeducao;
  end;

 TIdentificacaoPrestador = class(TPersistent)
  private
    FCnpj: string;
    FInscricaoMunicipal: string;
    FSenha: string;
    FFraseSecreta: string;
    FcUF: integer;
    //Alterado por Moro em 27/02/2015
    FInscricaoEstadual: string;
  published
    property Cnpj: string read FCnpj write FCnpj;
    property InscricaoMunicipal: string read FInscricaoMunicipal write FInscricaoMunicipal;
    // As propriedades abaixo são Utilizadas pelo provedor ISSDigital
    property Senha: string read FSenha write FSenha;
    property FraseSecreta: string read FFraseSecreta write FFraseSecreta;
    property cUF: integer read FcUF write FcUF;
    //Alterado por Moro em 27/02/2015
    property InscricaoEstadual: string read FInscricaoEstadual write FInscricaoEstadual;
  end;

 TEndereco = class(TPersistent)
  private
    FTipoLogradouro: string;
    FEndereco: string;
    FNumero: string;
    FComplemento: string;
    FTipoBairro : string;
    FBairro: string;
    FCodigoMunicipio: string;
    FUF: string;
    FCEP: string;
    FxMunicipio: string;
    FCodigoPais: integer;
    FxPais: String;
  published
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
    property CodigoPais: integer read FCodigoPais write FCodigoPais; 
    property xPais: String read FxPais write FxPais;
  end;

 TContato = class(TPersistent)
  private
    FTelefone: string;
    FEmail: string;
  published
    property Telefone: string read FTelefone write FTelefone;
    property Email: string read FEmail write FEmail;
  end;

 TDadosPrestador = class(TPersistent)
  private
    FIdentificacaoPrestador: TIdentificacaoPrestador;
    FRazaoSocial: string;
    FNomeFantasia: string;
    FEndereco: TEndereco;
    FContato: TContato;
  public
    constructor Create(AOwner: TNFSe);
    destructor Destroy; override;
  published
    property IdentificacaoPrestador: TIdentificacaoPrestador read FIdentificacaoPrestador write FIdentificacaoPrestador;
    property RazaoSocial: string read FRazaoSocial write FRazaoSocial;
    property NomeFantasia: string read FNomeFantasia write FNomeFantasia;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property Contato: TContato read FContato write FContato;
  end;

  TIdentificacaoTomador = class(TPersistent)
  private
    FCpfCnpj: string;
    FInscricaoMunicipal: string;
    FInscricaoEstadual: string;
    FDocTomadorEstrangeiro: string;
  published
    property CpfCnpj: string read FCpfCnpj write FCpfCnpj;
    property InscricaoMunicipal: string read FInscricaoMunicipal write FInscricaoMunicipal;
    property InscricaoEstadual: string read FInscricaoEstadual write FInscricaoEstadual;
    property DocTomadorEstrangeiro: string read FDocTomadorEstrangeiro write FDocTomadorEstrangeiro;
  end;

 TDadosTomador = class(TPersistent)
  private
    FIdentificacaoTomador: TIDentificacaoTomador;
    FRazaoSocial: string;
    FEndereco: TEndereco;
    FContato: TContato;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property IdentificacaoTomador: TIdentificacaoTomador read FIdentificacaoTomador write FIdentificacaoTomador;
    property RazaoSocial: string read FRazaoSocial write FRazaoSocial;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property Contato: TContato read FContato write FContato;
  end;

 TIdentificacaoIntermediarioServico = class(TPersistent)
  private
    FRazaoSocial: string;
    FCpfCnpj: string;
    FInscricaoMunicipal: string;
  published
    property RazaoSocial: string read FRazaoSocial write FRazaoSocial;
    property CpfCnpj: string read FCpfCnpj write FCpfCnpj;
    property InscricaoMunicipal: string read FInscricaoMunicipal write FInscricaoMunicipal;
  end;

 TIdentificacaoOrgaoGerador = class(TPersistent)
  private
    FCodigoMunicipio: string;
    FUf: string;
  published
    property CodigoMunicipio: string read FCodigoMunicipio write FCodigoMunicipio;
    property Uf: string read FUf write FUf;
  end;

 TDadosConstrucaoCivil = class(TPersistent)
  private
    FCodigoObra: string;
    FArt: string;
  published
    property CodigoObra: string read FCodigoObra write FCodigoObra;
    property Art: string read FArt write FArt;
  end;

  TParcelasCollectionItem = class(TCollectionItem)
  private
    FParcela: Integer;
    FDataVencimento: TDateTime;
    FValor: Currency;
  published
    property Parcela: Integer read FParcela write FParcela;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
    property Valor: Currency read FValor write FValor;
  end;

  TParcelasCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TParcelasCollectionItem;
    procedure SetItem(Index: Integer; Const Value: TParcelasCollectionItem);
  public
    constructor Create(AOwner: TCondicaoPagamento);

    function Add: TParcelasCollectionItem;
    property Items[Index: Integer]: TParcelasCollectionItem read GetItem write SetItem; default;
  end;

  TCondicaoPagamento = class(TPersistent)
  private
    FCondicao: TnfseCondicaoPagamento;
    FQtdParcela: Integer;
    FParcelas: TParcelasCollection;
    procedure SetParcelas(const Value: TParcelasCollection);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Condicao: TnfseCondicaoPagamento read FCondicao write FCondicao;
    property QtdParcela: Integer read FQtdParcela write FQtdParcela;
    property Parcelas: TParcelasCollection read FParcelas write SetParcelas;
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
    FIncentivadorCultural: TnfseSimNao;
    FProducao: TnfseSimNao;
    FStatus: TnfseStatusRps;
    FRpsSubstituido: TIdentificacaoRps;
    FSeriePrestacao: string;
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
    // RPS e NFSe
    FSignature: TSignature;

    FNumeroLote: String;
    FProtocolo: String;
    FdhRecebimento: TDateTime;
    FSituacao: String;

    FXML: AnsiString;

    FNfseCancelamento: TConfirmacaoCancelamento;
    FNfseSubstituidora: String;
    FMotivoCancelamento: string; // para provedor ISSDSF
    FChaveNFSe: string; // para provedor Infisc

    // Alterado Por Moro em 18/02/2015 - Provedor Infisc Versão XML 1.1
    FTipoEmissao: TnfseTEmissao;
    FEmpreitadaGlobal: TnfseTEmpreitadaGlobal;
    FModeloNFSe: string;
    FCancelada: TnfseSimNao;

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
    property IncentivadorCultural: TnfseSimNao read FIncentivadorCultural write FIncentivadorCultural;
    property Producao: TnfseSimNao read FProducao write FProducao;
    property Status: TnfseStatusRps read FStatus write FStatus;
    property RpsSubstituido: TIdentificacaoRps read FRpsSubstituido write FRpsSubstituido;
    //SeriePrestacao usada no provedor IssDsf
    property SeriePrestacao: string read FSeriePrestacao write FSeriePrestacao;
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
    property ValorCredito: Currency read FValorCredito write FValorCredito;
    property PrestadorServico: TDadosPrestador read FPrestadorServico write FPrestadorServico;
    property OrgaoGerador: TIdentificacaoOrgaoGerador read FOrgaoGerador write FOrgaoGerador;
    property ValoresNfse: TValoresNfse read FValoresNfse write FValoresNfse;
    // RPS e NFSe
    property signature: Tsignature read Fsignature write Fsignature;

    property NumeroLote: String read FNumeroLote write FNumeroLote;
    property Protocolo: String read FProtocolo write FProtocolo;
    property dhRecebimento: TDateTime read FdhRecebimento write FdhRecebimento;
    property Situacao: String read FSituacao write FSituacao;
    property XML: AnsiString read FXML write FXML;
    property NfseCancelamento: TConfirmacaoCancelamento read FNfseCancelamento write FNfseCancelamento;
    property NfseSubstituidora: String read FNfseSubstituidora write FNfseSubstituidora;
    property MotivoCancelamento: string read FMotivoCancelamento write FMotivoCancelamento; // para provedor ISSDSF
    property ChaveNFSe: String read FChaveNFSe write FChaveNFSe; // para provedor Infisc

    // Alterado Por Moro em 18/02/2015 - Provedor Infisc Versão XML 1.1
    property TipoEmissao: TnfseTEmissao read FTipoEmissao write FTipoEmissao;
    property EmpreitadaGlobal: TnfseTEmpreitadaGlobal read FEmpreitadaGlobal write FEmpreitadaGlobal;
    property ModeloNFSe: string read FModeloNFSe write FModeloNFSe;
    property Cancelada: TnfseSimNao read FCancelada write FCancelada;

  end;

 TLoteRps = class(TPersistent)
  private
    FInfID: TInfID;
    FNumeroLote: String;
    FCnpj: string;
    FInscricaoMunicipal: string;
    FQuantidadeRps: String;
    FSignature: TSignature;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property InfID: TInfID read FInfID write FInfID;
    property NumeroLote: String read FNumeroLote write FNumeroLote;
    property Cnpj: String read FCnpj write FCnpj;
    property InscricaoMunicipal: string read FInscricaoMunicipal write FInscricaoMunicipal;
    property QuantidadeRps: String read FQuantidadeRps write FQuantidadeRps;
    property signature: Tsignature read Fsignature write Fsignature;
  end;

 TPedidoCancelamento = class(TPersistent)
  private
    FInfID: TInfID;
    FIdentificacaoNfse: TIdentificacaoNfse;
    FCodigoCancelamento: String;
    FSignature: TSignature;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property InfID: TInfID read FInfID write FInfID;
    property IdentificacaoNfse: TIdentificacaoNfse read FIdentificacaoNfse write FIdentificacaoNfse;
    property CodigoCancelamento: String read FCodigoCancelamento write FCodigoCancelamento;
    property signature: Tsignature read Fsignature write Fsignature;
  end;

 TConfirmacaoCancelamento = class(TPersistent)
  private
    FInfID: TInfID;
    FPedido: TPedidoCancelamento;
    FDataHora: TDateTime;
    FSignature: TSignature;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property InfID: TInfID read FInfID write FInfID;
    property Pedido: TPedidoCancelamento read FPedido write FPedido;
    property DataHora: TDateTime read FDataHora write FDataHora;
    property signature: Tsignature read Fsignature write Fsignature;
  end;

 TSubstituicaoNfse = class(TPersistent)
  private
    FInfID: TInfID;
    FNfseSubstituidora: String;
    FSignature: TSignature;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property InfID: TInfID read FInfID write FInfID;
    property NfseSubstituidora: String read FNfseSubstituidora write FNfseSubstituidora;
    property signature: Tsignature read Fsignature write Fsignature;
  end;

const
  CMUN_EXTERIOR: integer = 9999999;
  XMUN_EXTERIOR: string = 'EXTERIOR';
  UF_EXTERIOR: string = 'EX';

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
  end;

 FItemServico := TItemServicoCollection.Create(Self);
 FDeducao     := TDeducaoCollection.Create(Self);
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

constructor TDadosPrestador.Create(AOwner: TNFSe);
begin
 inherited Create;

 FIdentificacaoPrestador := TIdentificacaoPrestador.Create;
 FEndereco               := TEndereco.Create;
 FContato                := TContato.Create;

 with FIdentificacaoPrestador do
  begin
   Cnpj               := '';
   InscricaoMunicipal := '';
   //Alterado por Moro em 27/02/2015
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
 FNaturezaOperacao             := noTributacaoNoMunicipio;
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
 FValorCredito                 := 0;
 FPrestadorServico             := TDadosPrestador.Create(self);
 FOrgaoGerador                 := TIdentificacaoOrgaoGerador.Create;
 FValoresNfse                  := TValoresNfse.Create; 
 // RPS e NFSe
 Fsignature                    := Tsignature.create;

 FNfseCancelamento             := TConfirmacaoCancelamento.Create;
 FNfseCancelamento.DataHora    := 0;
 FNfseSubstituidora            := '';

// Alterado Por Moro em 18/02/2015 - Provedor Infisc Versão XML 1.1
 FTipoEmissao                  := TeNormal;
 FEmpreitadaGlobal             := EgOutros;
 FModeloNFSe                   := '55';
 FCancelada                    := snNao;

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
 Fsignature.Free;
 FNfseCancelamento.Free;

 inherited Destroy;
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
 Fsignature          := Tsignature.create;
end;

destructor TLoteRps.Destroy;
begin
 FInfID.Free;
 Fsignature.Free;

 inherited Destroy;
end;

{ TPedidoCancelamento }

constructor TPedidoCancelamento.Create;
begin
 FInfID              := TInfID.Create;
 FIdentificacaoNfse  := TIdentificacaoNfse.Create;
 FCodigoCancelamento := '';
 Fsignature          := Tsignature.create;
end;

destructor TPedidoCancelamento.Destroy;
begin
 FInfID.Free;
 FIdentificacaoNfse.Free;
 Fsignature.Free;

  inherited;
end;

{ TConfirmacaoCancelamento }

constructor TConfirmacaoCancelamento.Create;
begin
 FInfID     := TInfID.Create;
 FPedido    := TPedidoCancelamento.Create;
 Fsignature := Tsignature.create;
end;

destructor TConfirmacaoCancelamento.Destroy;
begin
 FInfID.Free;
 FPedido.Free;
 Fsignature.Free;

  inherited;
end;

{ TSubstituicaoNfse }

constructor TSubstituicaoNfse.Create;
begin
 FInfID             := TInfID.Create;
 FNfseSubstituidora := '';
 Fsignature         := Tsignature.create;
end;

destructor TSubstituicaoNfse.Destroy;
begin
 FInfID.Free;
 Fsignature.Free;

  inherited;
end;

{ TItemServicoCollection }

function TItemServicoCollection.Add: TItemServicoCollectionItem;
begin
  Result := TItemServicoCollectionItem(inherited Add);
  Result.create;
end;

constructor TItemServicoCollection.Create(AOwner: TDadosServico);
begin
  inherited Create(TItemServicoCollectionItem);
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

{ TDeducaoCollection }
function TDeducaoCollection.Add: TDeducaoCollectionItem;
begin
  Result := TDeducaoCollectionItem(inherited Add);
  Result.create;
end;

constructor TDeducaoCollection.Create(AOwner: TDadosServico);
begin
  inherited Create(TDeducaoCollectionItem);
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

{ TItemServicoCollectionItem }

constructor TItemServicoCollectionItem.Create;
begin
  // Alterado Por Moro em 18/02/2015 - Provedor Infisc Versão XML 1.1
  FCodigo  := '';
  FCodServ := '';
  FUnidade := 'UN'; 
end;

destructor TItemServicoCollectionItem.Destroy;
begin

  inherited;
end;

{ TDeducaoCollectionItem }

constructor TDeducaoCollectionItem.Create;
begin

end;

destructor TDeducaoCollectionItem.Destroy;
begin

  inherited;
end;

{ TParcelasCollection }
function TParcelasCollection.Add: TParcelasCollectionItem;
begin
  Result := TParcelasCollectionItem(inherited Add);
end;

constructor TParcelasCollection.Create(AOwner : TCondicaoPagamento);
begin
  inherited Create(TParcelasCollectionItem);
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

{ TCondicaoPagamento }
constructor TCondicaoPagamento.Create;
begin
  inherited Create;
  FParcelas := TParcelasCollection.Create(Self);
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

end.
