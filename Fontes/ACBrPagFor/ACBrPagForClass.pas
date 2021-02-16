{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Jurisato Junior                           }
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

unit ACBrPagForClass;

interface

uses
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  Controls, SysUtils, Classes, ACBrPagForConversao;

type
  TLote = class;
  TPagFor = class;

  TArquivoTXT = class(TCollectionItem)
  private
    FArquivoTXT: TStringList;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property ArquivoTXT: TStringList read FArquivoTXT write FArquivoTXT;
  end;

  TArquivoTXTList = class(TCollection)
  private
    function GetItem(Index: Integer): TArquivoTXT;
    procedure SetItem(Index: Integer; Value: TArquivoTXT);
    function GetLastItem: TArquivoTXT;
    procedure SetLastItem(const Value: TArquivoTXT);
  public
    constructor Create;
    function Add: TArquivoTXT;
    property Items[Index: Integer]: TArquivoTXT read GetItem write SetItem; default;
    property Last: TArquivoTXT read GetLastItem write SetLastItem;
  end;

  TGeral = class(TPersistent)
  private
    FBanco: TBanco;
    FSubstitutaBanco: TBanco;
    FidTributo: TIndTributo;
  public
    property Banco: TBanco read FBanco write FBanco;
    property SubstitutaBanco: TBanco read FSubstitutaBanco write FSubstitutaBanco;
    property idTributo: TIndTributo read FidTributo write FidTributo;
  end;

  TInscricao = class(TPersistent)
  private
    FTipo: TTipoInscricao; // Tamanho 1
    FNumero: String; // Tamanho 14 ou 15
  public
    property Tipo: TTipoInscricao read FTipo write FTipo;
    property Numero: String read FNumero write FNumero;
  end;

  TAgencia = class(TPersistent)
  private
    FCodigo: Integer;
    FDV: String; // Tamanho 1
  public
    property Codigo: Integer read FCodigo write FCodigo;
    property DV: String read FDV write FDV;
  end;

  TConta = class(TPersistent)
  private
    FTipoConta: Integer;
    FNumero: Integer;
    FDV: String; // Tamanho 1
  public
    property TipoConta: Integer read FTipoConta write FTipoConta;
    property Numero: Integer read FNumero write FNumero;
    property DV: String read FDV write FDV;
  end;

  TContaCorrente = class(TPersistent)
  private
    FAgencia: TAgencia;
    FConta: TConta;
    FDV: String; // Tamanho 1
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Agencia: TAgencia read FAgencia write FAgencia;
    property Conta: TConta read FConta write FConta;
    property DV: String read FDV write FDV;
  end;

  TEmpresa = class(TPersistent)
  private
    FInscricao: TInscricao; // Tamanho 14 ou 15
    FConvenio: String; // Tamanho 20
    FContaCorrente : TContaCorrente;
    FNome: String; // Tamanho 30
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Convenio: String read FConvenio write FConvenio;
    property ContaCorrente: TContaCorrente read FContaCorrente write FContaCorrente;
    property Nome: String read FNome write FNome;
  end;

  TArquivo = class(TPersistent)
  private
    FCodigo: TTipoArquivo; // Tamanho 1
    FDataGeracao: TDateTime; // DDMMAAAA
    FHoraGeracao: TDateTime; // HHMMSS
    FSequencia: Integer;
    FDensidade: Integer;
  public
    property Codigo: TTipoArquivo read FCodigo write FCodigo;
    property DataGeracao: TDateTime read FDataGeracao write FDataGeracao;
    property HoraGeracao: TDateTime read FHoraGeracao write FHoraGeracao;
    property Sequencia: Integer read FSequencia write FSequencia;
    property Densidade: Integer read FDensidade write FDensidade;
  end;

  TTotais = class(TPersistent)
  private
    FQtdeLotes: Integer;
    FQtdeRegistros: Integer;
    FQtdeContasConciliadas: Integer;
  public
    property QtdeLotes: Integer read FQtdeLotes write FQtdeLotes;
    property QtdeRegistros: Integer read FQtdeRegistros write FQtdeRegistros;
    property QtdeContasConciliadas: Integer read FQtdeContasConciliadas write FQtdeContasConciliadas;
  end;

  TServico = class(TPersistent)
  private
    FOperacao: TTipoOperacao; // Tamanho 1
    FFormaLancamento: TFormaLancamento; // Tamanho 2
    FTipoServico: TTipoServico; // Tamanho 2
  public
    property Operacao: TTipoOperacao read FOperacao write FOperacao;
    property FormaLancamento: TFormaLancamento read FFormaLancamento write FFormaLancamento;
    property TipoServico: TTipoServico read FTipoServico write FTipoServico;
  end;

  TEndereco = class(TPersistent)
  private
    FLogradouro: String; // Tamanho 30
    FNumero: Integer;
    FComplemento: String; // Tamanho 15
    FBairro: String; // Tamanho 15
    FCidade: String; // Tamanho 20
    FCEP: Integer; // Tamanho 8
    FEstado: String; // Tamanho 2
  public
    property Logradouro: String read FLogradouro write FLogradouro;
    property Numero: Integer read FNumero write FNumero;
    property Complemento: String read FComplemento write FComplemento;
    property Bairro: String read FBairro write FBairro;
    property Cidade: String read FCidade write FCidade;
    property CEP: Integer read FCEP write FCEP;
    property Estado: String read FEstado write FEstado;
  end;

  TControleCobranca = class(TPersistent)
  private
    FNumRemRet: Integer;
    FDataGravacao: TDateTime;  // DDMMAAAA
  public
    property NumRemRet: Integer read FNumRemRet write FNumRemRet;
    property DataGravacao: TDateTime read FDataGravacao write FDataGravacao;
  end;

  TTitulos = class(TPersistent)
  private
    FQtdeTitulosCobranca: Integer;
    FValorTitulosCarteira: Double;
  public
    property QtdeTitulosCobranca: Integer read FQtdeTitulosCobranca write FQtdeTitulosCobranca;
    property ValorTitulosCarteira: Double read FValorTitulosCarteira write FValorTitulosCarteira;
  end;

  TCobranca = class(TPersistent)
  private
    FCarteira: Integer; // Tamanho 1
    FCadastramento: Integer; // Tamanho 1
    FTipoDocumento: String; // Tamanho 1
    FEmissaoBoleto: Integer; // Tamanho 1
    FDistribuicaoBoleto: String; // Tamanho 1
  public
    property Carteira: Integer read FCarteira write FCarteira;
    property Cadastramento: Integer read FCadastramento write FCadastramento;
    property TipoDocumento: String read FTipoDocumento write FTipoDocumento;
    property EmissaoBoleto: Integer read FEmissaoBoleto write FEmissaoBoleto;
    property DistribuicaoBoleto: String read FDistribuicaoBoleto write FDistribuicaoBoleto;
  end;

  TAcrescimosDescontos = class(TPersistent)
  private
    FCodigo: Integer; // Tamanho 1
    FData: TDateTime; // DDMMAAAA
    FValor: Double;
  public
    property Codigo: Integer read FCodigo write FCodigo;
    property Data: TDateTime read FData write FData;
    property Valor: Double read FValor write FValor;
  end;

  TSacado = class(TPersistent)
  private
    FInscricao: TInscricao; // Tamanho 15
    FNome: String; // Tamanho 40
    FEndereco: String; // Tamanho 40
    FBairro: String; // Tamanho 15
    FCEP: Integer; // Tamanho 8
    FCidade: String; // Tamanho 15
    FUF: String; // Tamanho 2
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Nome: String read FNome write FNome;
    property Endereco: String read FEndereco write FEndereco;
    property Bairro: String read FBairro write FBairro;
    property CEP: Integer read FCEP write FCEP;
    property Cidade: String read FCidade write FCidade;
    property UF: String read FUF write FUF;
  end;

  TAvalista = class(TPersistent)
  private
    FInscricao: TInscricao; // Tamanho 15
    FNome: String; // Tamanho 40
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Nome: String read FNome write FNome;
  end;

  TCedente = class(TPersistent)
  private
    FInscricao: TInscricao; // Tamanho 15
    FNome: string;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Nome: String read FNome write FNome;
  end;

  TDebitoAutomatico = class(TPersistent)
  private
    FBanco: TBanco;
    FContaCorrente : TContaCorrente;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Banco: TBanco read FBanco write FBanco;
    property ContaCorrente: TContaCorrente read FContaCorrente write FContaCorrente;
  end;

  TFavorecido = class(TPersistent)
  private
    FCamara: Integer;
    FBanco: TBanco;
    FContaCorrente: TContaCorrente;
    FNome: String; // Tamanho 30
    FInscricao: TInscricao;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Camara: Integer read FCamara write FCamara;
    property Banco: TBanco read FBanco write FBanco;
    property ContaCorrente: TContaCorrente read FContaCorrente write FContaCorrente;
    property Nome: String read FNome write FNome;
    property Inscricao: TInscricao read FInscricao write FInscricao;
  end;

  TMoeda = class(TPersistent)
  private
    FTipo : TTipoMoeda; // Tamanho 3
    FQtde : Double;
  public
    property Tipo: TTipoMoeda read FTipo write FTipo;
    property Qtde: Double read FQtde write FQtde;
  end;

  TCredito = class(TPersistent)
  private
    FSeuNumero: String; // Tamanho 20
    FDataPagamento: TDateTime; // DDMMAAAA
    FMoeda: TMoeda;
    FValorPagamento: Double;
    FNossoNumero: String; // Tamanho 20
    FDataReal: TDateTime; // DDMMAAAA
    FValorReal: Double;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property SeuNumero: String read FSeuNumero write FSeuNumero;
    property DataPagamento: TDateTime read FDataPagamento write FDataPagamento;
    property Moeda: TMoeda read FMoeda write FMoeda;
    property ValorPagamento: Double read FValorPagamento write FValorPagamento;
    property NossoNumero: String read FNossoNumero write FNossoNumero;
    property DataReal: TDateTime read FDataReal write FDataReal;
    property ValorReal: Double read FValorReal write FValorReal;
  end;

  // Estrutura do Registro 0 utilizado em todos os Arquivos
  // Primeiro e unico no Arquivo

  TAviso = class(TCollectionItem)
  private
    FCodigoRetorno: String;
    FMensagemRetorno: String;
    FSegmento: String;
    FSegmentoFilho: String;
    FSeuNumero: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CodigoRetorno: String read FCodigoRetorno write FCodigoRetorno;
    property MensagemRetorno: String read FMensagemRetorno write FMensagemRetorno;
    property Segmento: String read FSegmento write FSegmento;
    property SegmentoFilho: String read FSegmentoFilho write FSegmentoFilho;
    property SeuNumero: String read FSeuNumero write FSeuNumero;
  end;

  TAvisoList = class(TCollection)
  private
    function GetItem(Index: Integer): TAviso;
    procedure SetItem(Index: Integer; Value: TAviso);
    function GetLastItem: TAviso;
    procedure SetLastItem(const Value: TAviso);
  public
    constructor Create;
    function Add: TAviso;
    property Items[Index: Integer]: TAviso read GetItem write SetItem; default;
    property Last: TAviso read GetLastItem write SetLastItem;
  end;

  TRegistro0 = class(TPersistent)
  private
    FEmpresa: TEmpresa; // Tamanho 14
    FNomeBanco: String; // Tamanho 30
    FArquivo: TArquivo;
    FReservadoBanco: String; // Tamanho 20
    FReservadoEmpresa: String; // Tamanho 20
    FAviso: TAvisoList;
  public
    constructor Create(AOwner: TPagFor);
    destructor Destroy; override;
  published
    property Empresa: TEmpresa read FEmpresa write FEmpresa;
    property NomeBanco: String read FNomeBanco write FNomeBanco;
    property Arquivo: TArquivo read FArquivo write FArquivo;
    property ReservadoBanco: String read FReservadoBanco write FReservadoBanco;
    property ReservadoEmpresa: String read FReservadoEmpresa write FReservadoEmpresa;
    property Aviso: TAvisoList read FAviso write FAviso;
  end;

  // Estrutura do Registro 9 utilizado em todos os Arquivos
  // Ultimo e unico no Arquivo

  TRegistro9 = class(TPersistent)
  private
    FTotais: TTotais;
  public
    constructor Create(AOwner: TPagFor);
    destructor Destroy; override;
  published
    property Totais: TTotais read FTotais write FTotais;
  end;

  // Estrutura do Registro 1 utilizado para os Serviços
  // uma para cada lote de serviços - Identifica o Inicio do Lote

  TRegistro1 = class(TPersistent)
  private
    FServico: TServico;
    FEmpresa: TEmpresa; // Tamanho 15
    FInformacao1: String; // Tamanho 40
    FEndereco: TEndereco;
    FInformacao2: String; // Tamanho 40
    FControleCobranca: TControleCobranca;
    FDataCredito: TDateTime; // DDMMAAAA
  public
    constructor Create(AOwner: TLote);
    destructor Destroy; override;
  published
    property Servico: TServico read FServico write FServico;
    property Empresa: TEmpresa read FEmpresa write FEmpresa;
    property Informacao1: String read FInformacao1 write FInformacao1;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property Informacao2: String read FInformacao2 write FInformacao2;
    property ControleCobranca: TControleCobranca read FControleCobranca write FControleCobranca;
    property DataCredito: TDateTime read FDataCredito write FDataCredito;
  end;

  // Estrutura do Registro 5 utilizado para os Serviços
  // uma para cada lote de serviços - Identifica o Fim do Lote

  TRegistro5 = class(TPersistent)
  private
    FTotalCobrancaSimples: TTitulos;
    FTotalCobrancaVinculada: TTitulos;
    FTotalCobrancaCaucionada: TTitulos;
    FTotalCobrancaDescontada: TTitulos;
    FValor: Double;
    FQtdeMoeda: Double;
    FNumAvisoLancamento: Integer; // Tamanho 8 (6 numericos e 2 brancos)
    FNumAvisoDebito: Integer;
    FTotalOutrasEntidades: Double;
    FTotalValorAcrescimo: Double;
    FTotalValorArrecadado: Double;
  public
    constructor Create(AOwner: TLote);
    destructor Destroy; override;
  published
    property TotalCobrancaSimples: TTitulos read FTotalCobrancaSimples write FTotalCobrancaSimples;
    property TotalCobrancaVinculada: TTitulos read FTotalCobrancaVinculada write FTotalCobrancaVinculada;
    property TotalCobrancaCaucionada: TTitulos read FTotalCobrancaCaucionada write FTotalCobrancaCaucionada;
    property TotalCobrancaDescontada: TTitulos read FTotalCobrancaDescontada write FTotalCobrancaDescontada;
    property Valor: Double read FValor write FValor;
    property QtdeMoeda: Double read FQtdeMoeda write FQtdeMoeda;
    property NumAvisoLancamento: Integer read FNumAvisoLancamento write FNumAvisoLancamento;
    property NumAvisoDebito: Integer read FNumAvisoDebito write FNumAvisoDebito;

    property TotalOutrasEntidades: Double read FTotalOutrasEntidades write FTotalOutrasEntidades;
    property TotalValorAcrescimo: Double read FTotalValorAcrescimo write FTotalValorAcrescimo;
    property TotalValorArrecadado: Double read FTotalValorArrecadado write FTotalValorArrecadado;
  end;

   // Estrutura do Segmento B (Opcional)

  TSegmentoB = class(TCollectionItem)
  private
    FInscricao: TInscricao; // Tamanho 14
    FEndereco: TEndereco;
    FDataVencimento: TDateTime; // DDMMAAAA
    FValor: Double;
    FAbatimento: Double;
    FDesconto: Double;
    FMora: Double;
    FMulta: Double;
    FCodigoDoc: String; // Tamanho 15
    FAviso: Integer; // Tamanho 1
    FCodigoUG: Integer; // Tamanho 6
    FEmail: String;
    FTelefone: String;
    FHonorario: Double;
    FAcrescimo: Double;
    FCodOcorrencia: string;
    FDescOcorrencia: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
    property Valor: Double read FValor write FValor;
    property Abatimento: Double read FAbatimento write FAbatimento;
    property Desconto: Double read FDesconto write FDesconto;
    property Mora: Double read FMora write FMora;
    property Multa: Double read FMulta write FMulta;
    property CodigoDoc: String read FCodigoDoc write FCodigoDoc;
    property Aviso: Integer read FAviso write FAviso;
    property CodigoUG: Integer read FCodigoUG write FCodigoUG;
    property Email: String read FEmail write FEmail;
    property Telefone: String read FTelefone write FTelefone;
    property Honorario: Double read FHonorario write FHonorario;
    property Acrescimo: Double read FAcrescimo write FAcrescimo;
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
    property DescOcorrencia: String read FDescOcorrencia write FDescOcorrencia;
  end;

  TSegmentoBList = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoB;
    procedure SetItem(Index: Integer; Value: TSegmentoB);
    function GetLastItem: TSegmentoB;
    procedure SetLastItem(const Value: TSegmentoB);
  public
    constructor Create{(AOwner: TLote)};
    function Add: TSegmentoB;
    property Items[Index: Integer]: TSegmentoB read GetItem write SetItem; default;
    property Last: TSegmentoB read GetLastItem write SetLastItem;
  end;

  // Estrutura do Segmento C (Opcional)

  TSegmentoC = class(TCollectionItem)
  private
    FValorCSLL: Double;
    FValorIR: Double;
    FValorISS: Double;
    FValorIOF: Double;
    FDeducoes: Double;
    FAcrescimos: Double;
    FContaCorrente: TContaCorrente;
    FValorINSS: Double;
    FVencimento: TDate;
    FValorDocumento: Double;
    FValorPIS: Double;
    FValorCOFINS: Double;
    FDescontos: Double;
    FMora: Double;
    FMulta: Double;
    FNumeroFaturaDocumento: String;
    FAbatimentos: Double;
    FCodOcorrencia: string;
    FDescOcorrencia: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property ValorCSLL: Double read FValorCSLL write FValorCSLL;
    property ValorIR: Double read FValorIR write FValorIR;
    property ValorISS: Double read FValorISS write FValorISS;
    property ValorIOF: Double read FValorIOF write FValorIOF;
    property Deducoes: Double read FDeducoes write FDeducoes;
    property Acrescimos: Double read FAcrescimos write FAcrescimos;
    property ContaCorrente: TContaCorrente read FContaCorrente write FContaCorrente;
    property ValorINSS: Double read FValorINSS write FValorINSS;
    property Vencimento: TDate read FVencimento write FVencimento;
    property ValorDocumento: Double read FValorDocumento write FValorDocumento;
    property ValorPIS: Double read FValorPIS write FValorPIS;
    property ValorCOFINS: Double read FValorCOFINS write FValorCOFINS;
    property Descontos: Double read FDescontos write FDescontos;
    property Mora: Double read FMora write FMora;
    property Multa: Double read FMulta write FMulta;
    property NumeroFaturaDocumento: String read FNumeroFaturaDocumento write FNumeroFaturaDocumento;
    property Abatimentos: Double read FAbatimentos write FAbatimentos;
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
    property DescOcorrencia: String read FDescOcorrencia write FDescOcorrencia;
  end;

  TSegmentoCList = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoC;
    procedure SetItem(Index: Integer; Value: TSegmentoC);
    function GetLastItem: TSegmentoC;
    procedure SetLastItem(const Value: TSegmentoC);
  public
    constructor Create{(AOwner: TLote)};
    function Add: TSegmentoC;
    property Items[Index: Integer]: TSegmentoC read GetItem write SetItem; default;
    property Last: TSegmentoC read GetLastItem write SetLastItem;
  end;

  // Estrutura do Segmento D (Opcional)

  TSegmentoD = class(TCollectionItem)
  private
    FPeriodoCompetencia: Integer;
    FCentroCusto: String;
    FCodigoFuncionario: String;
    FCargo: String;
    FFeriasInicio: TDate;
    FFeriasFim: TDate;
    FDependentesIR: Integer;
    FDependentesSalarioFamilia: Integer;
    FHorasSemanais: Integer;
    FSalarioContribuicao: Double;
    FFGTS: Double;
    FValorCredito: Double;
    FValorDebito: Double;
    FValorLiquido: Double;
    FValorBase: Double;
    FBaseCalculoIRRF: Double;
    FBaseCalculoFGTS: Double;
    FDisponibilizacao: String;
    FCodOcorrencia: string;
    FDescOcorrencia: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property PeriodoCompetencia: Integer read FPeriodoCompetencia write FPeriodoCompetencia;
    property CentroCusto: String read FCentroCusto write FCentroCusto;
    property CodigoFuncionario: String read FCodigoFuncionario write FCodigoFuncionario;
    property Cargo: String read FCargo write FCargo;
    property FeriasInicio: TDate read FFeriasInicio write FFeriasInicio;
    property FeriasFim: TDate read FFeriasFim write FFeriasFim;
    property DependentesIR: Integer read FDependentesIR write FDependentesIR;
    property DependentesSalarioFamilia: Integer read FDependentesSalarioFamilia write FDependentesSalarioFamilia;
    property HorasSemanais: Integer read FHorasSemanais write FHorasSemanais;
    property SalarioContribuicao: Double read FSalarioContribuicao write FSalarioContribuicao;
    property FGTS: Double read FFGTS write FFGTS;
    property ValorCredito: Double read FValorCredito write FValorCredito;
    property ValorDebito: Double read FValorDebito write FValorDebito;
    property ValorLiquido: Double read FValorLiquido write FValorLiquido;
    property ValorBase: Double read FValorBase write FValorBase;
    property BaseCalculoIRRF: Double read FBaseCalculoIRRF write FBaseCalculoIRRF;
    property BaseCalculoFGTS: Double read FBaseCalculoFGTS write FBaseCalculoFGTS;
    property Disponibilizacao: String read FDisponibilizacao write FDisponibilizacao;
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
    property DescOcorrencia: String read FDescOcorrencia write FDescOcorrencia;
  end;

  TSegmentoDList = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoD;
    procedure SetItem(Index: Integer; Value: TSegmentoD);
    function GetLastItem: TSegmentoD;
    procedure SetLastItem(const Value: TSegmentoD);
  public
    constructor Create{(AOwner: TLote)};
    function Add: TSegmentoD;
    property Items[Index: Integer]: TSegmentoD read GetItem write SetItem; default;
    property Last: TSegmentoD read GetLastItem write SetLastItem;
  end;

  // Estrutura do Segmento E (Opcional)

  TSegmentoE = class(TCollectionItem)
  private
    FInformacaoComplementar: String;
    FMovimento: TTipoMovimentoPagto;
    FCodOcorrencia: string;
    FDescOcorrencia: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property Movimento: TTipoMovimentoPagto read FMovimento write FMovimento;
    property InformacaoComplementar: String read FInformacaoComplementar write FInformacaoComplementar;
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
    property DescOcorrencia: String read FDescOcorrencia write FDescOcorrencia;
  end;

  TSegmentoEList = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoE;
    procedure SetItem(Index: Integer; Value: TSegmentoE);
    function GetLastItem: TSegmentoE;
    procedure SetLastItem(const Value: TSegmentoE);
  public
    constructor Create;
    function Add: TSegmentoE;
    property Items[Index: Integer]: TSegmentoE read GetItem write SetItem; default;
    property Last: TSegmentoE read GetLastItem write SetLastItem;
  end;

  // Estrutura do Segmento F (Opcional)

  TSegmentoF = class(TCollectionItem)
  private
    FInformacaoComplementar: String;
    FCodOcorrencia: string;
    FDescOcorrencia: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property InformacaoComplementar: String read FInformacaoComplementar write FInformacaoComplementar;
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
    property DescOcorrencia: String read FDescOcorrencia write FDescOcorrencia;
  end;

  TSegmentoFList = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoF;
    procedure SetItem(Index: Integer; Value: TSegmentoF);
    function GetLastItem: TSegmentoF;
    procedure SetLastItem(const Value: TSegmentoF);
  public
    constructor Create{(AOwner: TLote)};
    function Add: TSegmentoF;
    property Items[Index: Integer]: TSegmentoF read GetItem write SetItem; default;
    property Last: TSegmentoF read GetLastItem write SetLastItem;
  end;

  // Estrutura do Segmento Z (Opcional)

  TSegmentoZ = class(TCollectionItem)
  private
    FAutenticacao : String; // Tamanho 64
    FSeuNumero: String;
    FNossoNumero: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property Autenticacao: String read FAutenticacao write FAutenticacao;
    property SeuNumero: String read FSeuNumero write FSeuNumero;
    property NossoNumero: String read FNossoNumero write FNossoNumero;
  end;

  TSegmentoZList = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoZ;
    procedure SetItem(Index: Integer; Value: TSegmentoZ);
    function GetLastItem: TSegmentoZ;
    procedure SetLastItem(const Value: TSegmentoZ);
  public
    constructor Create{(AOwner: TLote)};
    function Add: TSegmentoZ;
    property Items[Index: Integer]: TSegmentoZ read GetItem write SetItem; default;
    property Last: TSegmentoZ read GetLastItem write SetLastItem;
  end;


  // Lote: Pagamento Através de Crédito em Conta, Cheque, OP, DOC, TED ou
  //       Pagamento com Autenticação
  // Serviço: 20 = Pagamento

  // Lote: Débito em Conta Corrente
  // Serviço: xx = Débito em Conta Corrente

  // Segmentos Obrigatórios de Remessa: A
  // Segmentos Opcionais de Remessa: B, C

  // Estrutura do Segmento A (Obrigatorio)

  TSegmentoA = class(TCollectionItem)
  private
    FTipoMovimento: TTipoMovimento; // Tamanho 1
    FCodMovimento: TInstrucaoMovimento; // Tamanho 2
    FFavorecido: TFavorecido;
    FCredito: TCredito;
    FInformacao2: String; // Tamanho 40
    FCodigoDOC: String; // Tamanho 2
    FCodigoTED: String; // Tamanho 5
    FCodigoComp: String; // Tamanho 2
    FAviso: Integer; // Tamanho 1
    FCodOcorrencia : String; // Tamanho 2

    FSegmentoB: TSegmentoBList;
    FSegmentoC: TSegmentoCList;
    FSegmentoD: TSegmentoDList;
    FSegmentoE: TSegmentoEList;
    FSegmentoF: TSegmentoFList;
    FSegmentoZ: TSegmentoZList;
    FDescOcorrencia: String;

    procedure SetSegmentoB(const Value: TSegmentoBList);
    procedure SetSegmentoC(const Value: TSegmentoCList);
    procedure SetSegmentoD(const Value: TSegmentoDList);
    procedure SetSegmentoE(const Value: TSegmentoEList);
    procedure SetSegmentoF(const Value: TSegmentoFList);
    procedure SetSegmentoZ(const Value: TSegmentoZList);
    function GetPagamentoLiberado: Boolean;

  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property TipoMovimento: TTipoMovimento read FTipoMovimento write FTipoMovimento;
    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property Favorecido: TFavorecido read FFavorecido write FFavorecido;
    property Credito: TCredito read FCredito write FCredito;
    property Informacao2: String read FInformacao2 write FInformacao2;
    property CodigoDOC: String read FCodigoDOC write FCodigoDOC;
    property CodigoTED: String read FCodigoTED write FCodigoTED;
    property CodigoComp: String read FCodigoComp write FCodigoComp;
    property Aviso: Integer read FAviso write FAviso;
    property CodOcorrencia: String read FCodOcorrencia write FCodOcorrencia;
    property DescOcorrencia: String read FDescOcorrencia write FDescOcorrencia;
    property PagamentoLiberado: Boolean read GetPagamentoLiberado;
    property SegmentoB: TSegmentoBList read FSegmentoB write SetSegmentoB;
    property SegmentoC: TSegmentoCList read FSegmentoC write SetSegmentoC;
    property SegmentoD: TSegmentoDList read FSegmentoD write SetSegmentoD;
    property SegmentoE: TSegmentoEList read FSegmentoE write SetSegmentoE;
    property SegmentoF: TSegmentoFList read FSegmentoF write SetSegmentoF;
    property SegmentoZ: TSegmentoZList read FSegmentoZ write SetSegmentoZ;
  end;

  TSegmentoAList = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoA;
    procedure SetItem(Index: Integer; Value: TSegmentoA);
    function GetLastItem: TSegmentoA;
    procedure SetLastItem(const Value: TSegmentoA);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoA;
    property Items[Index: Integer]: TSegmentoA read GetItem write SetItem; default;
    property Last: TSegmentoA read GetLastItem write SetLastItem;
  end;

  TSegmentoH = class(TCollectionItem)
  private
    FAvalista: TAvalista;
    FDesconto2: TAcrescimosDescontos;
    FDesconto3: TAcrescimosDescontos;
    FMulta: TAcrescimosDescontos;
    FAbatimento: Double;
    FInformacao1: string;
    FInformacao2: string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property Avalista: TAvalista read FAvalista write FAvalista;
    property Desconto2: TAcrescimosDescontos read FDesconto2 write FDesconto2;
    property Desconto3: TAcrescimosDescontos read FDesconto3 write FDesconto3;
    property Multa: TAcrescimosDescontos read FMulta write FMulta;
    property Abatimento: Double read FAbatimento write FAbatimento;
    property Informacao1: string read FInformacao1 write FInformacao1;
    property Informacao2: string read FInformacao2 write FInformacao2;
  end;

  TSegmentoHList = class(TCollection)
    private
    function GetItem(Index: Integer): TSegmentoH;
    procedure SetItem(Index: Integer; Value: TSegmentoH);
    function GetLastItem: TSegmentoH;
    procedure SetLastItem(const Value: TSegmentoH);
  public
    constructor Create;
    function Add: TSegmentoH;
    property Items[Index: Integer]: TSegmentoH read GetItem write SetItem; default;
    property Last: TSegmentoH read GetLastItem write SetLastItem;
  end;

  TSegmentoG = class(TCollectionItem)
  private
    FCodigoBarras: string;
    FCedente: TCedente;
    FVencimento: TDate;
    FValorTitulo: Double;
    FQtdeMoeda: Double;
    FCodigoMoeda: Integer;
    FNumeroDocumento: string;
    FAgenciaCobradora: Integer;
    FDVCobradora: string;
    FPraca: string;
    FCarteira: string;
    FEspecieTitulo: Integer;
    FDataEmissao: TDateTime;
    FJurosMora: Double;
    FDesconto1: TAcrescimosDescontos;
    FCodigoProtesto: Integer;
    FPrazoProtesto: Integer;
    FDataLimite: TDate;

    FSegmentoH: TSegmentoHList;

    procedure SetSegmentoH(const Value: TSegmentoHList);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CodigoBarras: string read FCodigoBarras write FCodigoBarras;
    property Cedente: TCedente read FCedente write FCedente;
    property Vencimento: TDate read FVencimento write FVencimento;
    property ValorTitulo: Double read FValorTitulo write FValorTitulo;
    property QtdeMoeda: Double read FQtdeMoeda write FQtdeMoeda;
    property CodigoMoeda: Integer read FCodigoMoeda write FCodigoMoeda;
    property NumeroDocumento: string read FNumeroDocumento write FNumeroDocumento;
    property AgenciaCobradora: Integer read FAgenciaCobradora write FAgenciaCobradora;
    property DVCobradora: string read FDVCobradora write FDVCobradora;
    property Praca: string read FPraca write FPraca;
    property Carteira: string read FCarteira write FCarteira;
    property EspecieTitulo: Integer read FEspecieTitulo write FEspecieTitulo;
    property DataEmissao: TDateTime read FDataEmissao write FDataEmissao;
    property JurosMora: Double read FJurosMora write FJurosMora;
    property Desconto1: TAcrescimosDescontos read FDesconto1 write FDesconto1;
    property CodigoProtesto: Integer read FCodigoProtesto write FCodigoProtesto;
    property PrazoProtesto: Integer read FPrazoProtesto write FPrazoProtesto;
    property DataLimite: TDate read FDataLimite write FDataLimite;
    property SegmentoH: TSegmentoHList read FSegmentoH write SetSegmentoH;
  end;

  TSegmentoGList = class(TCollection)
    private
    function GetItem(Index: Integer): TSegmentoG;
    procedure SetItem(Index: Integer; Value: TSegmentoG);
    function GetLastItem: TSegmentoG;
    procedure SetLastItem(const Value: TSegmentoG);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoG;
    property Items[Index: Integer]: TSegmentoG read GetItem write SetItem; default;
    property Last: TSegmentoG read GetLastItem write SetLastItem;
  end;

  TPagador = class
  private
    FInscricao: TInscricao;
    FNome: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Nome: String read FNome write FNome;
  end;

  TBeneficiario = class
  private
    FInscricao: TInscricao;
    FNome: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Nome: String read FNome write FNome;
  end;

  TSacadorAvalista = class
  private
    FInscricao: TInscricao;
    FNome: String;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;

    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Nome: String read FNome write FNome;
  end;

  TSegmentoJ52 = class(TCollectionItem)
  private
    FCodMovimento: TInstrucaoMovimento;
    FTipoMovimento: TTipoMovimento;
    FPagador: TPagador;
    FBeneficiario: TBeneficiario;
    FSacadorAvalista: TSacadorAvalista;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property TipoMovimento: TTipoMovimento read FTipoMovimento write FTipoMovimento;
    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property Pagador: TPagador read FPagador write FPagador;
    property Beneficiario: TBeneficiario read FBeneficiario write FBeneficiario;
    property SacadorAvalista: TSacadorAvalista read FSacadorAvalista write FSacadorAvalista;
  end;

  TSegmentoJ52List = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoJ52;
    procedure SetItem(Index: Integer; Value: TSegmentoJ52);
    function GetLastItem: TSegmentoJ52;
    procedure SetLastItem(const Value: TSegmentoJ52);
  public
    constructor Create;
    function Add: TSegmentoJ52;
    property Items[Index: Integer]: TSegmentoJ52 read GetItem write SetItem; default;
    property Last: TSegmentoJ52 read GetLastItem write SetLastItem;
  end;

  // Lote: Pagamento de Titulos de Cobrança
  // Serviço: 20 = Pagamentos

  // Segmentos Obrigatórios de Remessa: J
  // Segmentos Opcionais de Remessa:

  // Estrutura do Segmento J (Obrigatorio)

  TSegmentoJ = class(TCollectionItem)
  private
    FCodMovimento: TInstrucaoMovimento;
    FCodigoBarras    : String;
    FNomeCedente     : String;
    FDataVencimento  : TDateTime;
    FValorTitulo     : Double;
    FDesconto        : Double;
    FAcrescimo       : Double;
    FDataPagamento   : TDateTime;
    FValorPagamento  : Double;
    FQtdeMoeda       : Double;
    FReferenciaSacado: String;
    FCodigoMoeda     : Integer;
    FCodOcorrencia : String;
    FSegmentoJ52: TSegmentoJ52List;
    FSegmentoB: TSegmentoBList;
    FSegmentoC: TSegmentoCList;
    FSegmentoZ: TSegmentoZList;
    FDescOcorrencia: String;
    FNossoNumero: String;
    procedure SetSegmentoJ52(const Value: TSegmentoJ52List);
    procedure SetSegmentoB(const Value: TSegmentoBList);
    procedure SetSegmentoC(const Value: TSegmentoCList);
    procedure SetSegmentoZ(const Value: TSegmentoZList);
    function GetPagamentoLiberado: Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property CodigoBarras: String read FCodigoBarras write FCodigoBarras;
    property NomeCedente: String read FNomeCedente write FNomeCedente;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
    property ValorTitulo: Double read FValorTitulo write FValorTitulo;
    property Desconto: Double read FDesconto write FDesconto;
    property Acrescimo: Double read FAcrescimo write FAcrescimo;
    property DataPagamento: TDateTime read FDataPagamento write FDataPagamento;
    property ValorPagamento: Double read FValorPagamento write FValorPagamento;
    property QtdeMoeda: Double read FQtdeMoeda write FQtdeMoeda;
    property ReferenciaSacado: String read FReferenciaSacado write FReferenciaSacado;
    property CodigoMoeda: Integer read FCodigoMoeda write FCodigoMoeda;
    property CodOcorrencia: String read FCodOcorrencia write FCodOcorrencia;
    property DescOcorrencia: String read FDescOcorrencia write FDescOcorrencia;
    property NossoNumero: String read FNossoNumero write FNossoNumero;
    property PagamentoLiberado: Boolean read GetPagamentoLiberado;
    property SegmentoJ52: TSegmentoJ52List read FSegmentoJ52 write SetSegmentoJ52;
    property SegmentoB: TSegmentoBList read FSegmentoB write SetSegmentoB;
    property SegmentoC: TSegmentoCList read FSegmentoC write SetSegmentoC;
    property SegmentoZ: TSegmentoZList read FSegmentoZ write SetSegmentoZ;
  end;

  TSegmentoJList = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoJ;
    procedure SetItem(Index: Integer; Value: TSegmentoJ);
    function GetLastItem: TSegmentoJ;
    procedure SetLastItem(const Value: TSegmentoJ);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoJ;
    property Items[Index: Integer]: TSegmentoJ read GetItem write SetItem; default;
    property Last: TSegmentoJ read GetLastItem write SetLastItem;
  end;

  // Estrutura do Segmento W* (Opcional)
  // * = obrigatório para FGTS convenio 0181 e 0182

  TSegmentoW = class(TCollectionItem)
  private
    FComplementoRegistro: Integer;
    FInformacoes1ou2    : String; // Tamanho 1
    FInformacoes1       : String; // Tamanho 80
    FInformacoes2       : String; // Tamanho 80
    FInformacoes3       : String;
    FInformacoes4       : String;
    FPagFGTS            : Boolean;
    FCodReceita         : String; // Tamanho 6
    FTipoIdContribuinte : String; // Tamanho 2
    FIdContribuinte     : String; // Tamanho 14
    FIdentificador      : String; // Tamanho 16
    FLacreConecSocial   : String; // Tamanho 9
    FLacreDV            : String; // Tamanho 2
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property ComplementoRegistro: Integer read FComplementoRegistro write FComplementoRegistro;
    property Informacoes1ou2: String read FInformacoes1ou2 write FInformacoes1ou2;
    property Informacoes1: String read FInformacoes1 write FInformacoes1;
    property Informacoes2: String read FInformacoes2 write FInformacoes2;
    property Informacoes3: String read FInformacoes3 write FInformacoes3;
    property Informacoes4: String read FInformacoes4 write FInformacoes4;
    property PagFGTS: Boolean read FPagFGTS write FPagFGTS;
    property CodReceita: String read FCodReceita write FCodReceita;
    property TipoIdContribuinte: String read FTipoIdContribuinte write FTipoIdContribuinte;
    property IdContribuinte: String read FIdContribuinte write FIdContribuinte;
    property Identificador: String read FIdentificador write FIdentificador;
    property LacreConecSocial: String read FLacreConecSocial write FLacreConecSocial;
    property LacreDV: String read FLacreDV write FLacreDV;
  end;

  TSegmentoWList = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoW;
    procedure SetItem(Index: Integer; Value: TSegmentoW);
    function GetLastItem: TSegmentoW;
    procedure SetLastItem(const Value: TSegmentoW);
  public
    constructor Create;
    function Add: TSegmentoW;
    property Items[Index: Integer]: TSegmentoW read GetItem write SetItem; default;
    property Last: TSegmentoW read GetLastItem write SetLastItem;
  end;

  // Lote: Pagamento de Tributos sem código de barras
  // Serviço: 22 = Pagamento de Contas, Tributos e Impostos

  // Segmentos Obrigatórios de Remessa: N
  // Segmentos Opcionais de Remessa: B, W e Z

  // Estrutura do Segmento N (Obrigatorio)

  TSegmentoN = class(TPersistent)
  private
    FCodMovimento    : TInstrucaoMovimento; // Tamanho 2
    FSeuNumero       : String; // Tamanho 20
    FNossoNumero     : String; // Tamanho 20
    FNomeContribuinte: String; // Tamanho 30
    FDataPagamento   : TDateTime; // DDMMAAAA
    FValorPagamento  : Double;
    FSegmentoB: TSegmentoBList;
    FSegmentoW: TSegmentoWList;
    FSegmentoZ: TSegmentoZList;
    FCodOcorrencia: string;
    FDescOcorrencia: String;
    procedure SetSegmentoB(const Value: TSegmentoBList);
    procedure SetSegmentoW(const Value: TSegmentoWList);
    procedure SetSegmentoZ(const Value: TSegmentoZList);
    function GetPagamentoLiberado: Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  public
    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property SeuNumero: String read FSeuNumero write FSeuNumero;
    property NossoNumero: String read FNossoNumero write FNossoNumero;
    property NomeContribuinte: String read FNomeContribuinte write FNomeContribuinte;
    property DataPagamento: TDateTime read FDataPagamento write FDataPagamento;
    property ValorPagamento: Double read FValorPagamento write FValorPagamento;
    property SegmentoB: TSegmentoBList read FSegmentoB write SetSegmentoB;
    property SegmentoW: TSegmentoWList read FSegmentoW write SetSegmentoW;
    property SegmentoZ: TSegmentoZList read FSegmentoZ write SetSegmentoZ;
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
    property DescOcorrencia: String read FDescOcorrencia write FDescOcorrencia;
    property PagamentoLiberado: Boolean read GetPagamentoLiberado;
  end;

  // Segmento N1 - GPS

  TSegmentoN1 = class(TCollectionItem)
  private
    FSegmentoN           : TSegmentoN;
    FReceita             : Integer;
    FTipoContribuinte    : Integer;
    FidContribuinte      : String; // Tamanho 14
    FCompetencia         : Integer;
    FValorTributo        : Double;
    FValorOutrasEntidades: Double;
    FAtualizacaoMonetaria: Double;
    FCodigoPagamento     : TCodigoPagamentoGps;
    FMesAnoCompetencia   : Integer;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property SegmentoN: TSegmentoN read FSegmentoN write FSegmentoN;
    property Receita: Integer read FReceita write FReceita;
    property TipoContribuinte: Integer read FTipoContribuinte write FTipoContribuinte;
    property idContribuinte: String read FidContribuinte write FidContribuinte;
    property Competencia: Integer read FCompetencia write FCompetencia;
    property ValorTributo: Double read FValorTributo write FValorTributo;
    property ValorOutrasEntidades: Double read FValorOutrasEntidades write FValorOutrasEntidades;
    property AtualizacaoMonetaria: Double read FAtualizacaoMonetaria write FAtualizacaoMonetaria;
    property CodigoPagamento: TCodigoPagamentoGps read FCodigoPagamento write FCodigoPagamento;
    property MesAnoCompetencia: Integer read FMesAnoCompetencia write FMesAnoCompetencia;
  end;

  TSegmentoN1List = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoN1;
    procedure SetItem(Index: Integer; Value: TSegmentoN1);
    function GetLastItem: TSegmentoN1;
    procedure SetLastItem(const Value: TSegmentoN1);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoN1;
    property Items[Index: Integer]: TSegmentoN1 read GetItem write SetItem; default;
    property Last: TSegmentoN1 read GetLastItem write SetLastItem;
  end;

  // Segmento N2 - DARF Normal

  TSegmentoN2 = class(TCollectionItem)
  private
    FSegmentoN       : TSegmentoN;
    FReceita         : Integer;
    FTipoContribuinte: TTipoInscricao;
    FidContribuinte  : String; // Tamanho 14
    FPeriodo         : TDateTime;   // DDMMAAAA
    FReferencia      : String; // Tamanho 17
    FValorPrincipal  : Double;
    FMulta           : Double;
    FJuros           : Double;
    FDataVencimento  : TDateTime; // DDMMAAAA
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property SegmentoN: TSegmentoN read FSegmentoN write FSegmentoN;
    property Receita: Integer read FReceita write FReceita;
    property TipoContribuinte: TTipoInscricao read FTipoContribuinte write FTipoContribuinte;
    property idContribuinte: String read FidContribuinte write FidContribuinte;
    property Periodo: TDateTime read FPeriodo write FPeriodo;
    property Referencia: String read FReferencia write FReferencia;
    property ValorPrincipal: Double read FValorPrincipal write FValorPrincipal;
    property Multa: Double read FMulta write FMulta;
    property Juros: Double read FJuros write FJuros;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
  end;

  TSegmentoN2List = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoN2;
    procedure SetItem(Index: Integer; Value: TSegmentoN2);
    function GetLastItem: TSegmentoN2;
    procedure SetLastItem(const Value: TSegmentoN2);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoN2;
    property Items[Index: Integer]: TSegmentoN2 read GetItem write SetItem; default;
    property Last: TSegmentoN2 read GetLastItem write SetLastItem;
  end;

  // Segmento N3 - DARF Simples

  TSegmentoN3 = class(TCollectionItem)
  private
    FSegmentoN       : TSegmentoN;
    FReceita         : Integer;
    FTipoContribuinte: TTipoInscricao;
    FidContribuinte  : String; // Tamanho 14
    FPeriodo         : TDateTime;  // DDMMAAAA
    FReceitaBruta    : Double;
    FPercentual      : Double;
    FValorPrincipal  : Double;
    FMulta           : Double;
    FJuros           : Double;
    FDataVencimento  : TDate;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property SegmentoN: TSegmentoN read FSegmentoN write FSegmentoN;
    property Receita: Integer read FReceita write FReceita;
    property TipoContribuinte: TTipoInscricao read FTipoContribuinte write FTipoContribuinte;
    property idContribuinte: String read FidContribuinte write FidContribuinte;
    property Periodo: TDateTime read FPeriodo write FPeriodo;
    property ReceitaBruta: Double read FReceitaBruta write FReceitaBruta;
    property Percentual: Double read FPercentual write FPercentual;
    property ValorPrincipal: Double read FValorPrincipal write FValorPrincipal;
    property Multa: Double read FMulta write FMulta;
    property Juros: Double read FJuros write FJuros;
    property DataVencimento: TDate read FDataVencimento write FDataVencimento;
  end;

  TSegmentoN3List = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoN3;
    procedure SetItem(Index: Integer; Value: TSegmentoN3);
    function GetLastItem: TSegmentoN3;
    procedure SetLastItem(const Value: TSegmentoN3);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoN3;
    property Items[Index: Integer]: TSegmentoN3 read GetItem write SetItem; default;
    property Last: TSegmentoN3 read GetLastItem write SetLastItem;
  end;

  // Segmento N4 - GARE-SP
  // idTributo: 22 = Tributo GARE-SP ICMS
  //            23 = Tributo GARE-SP DR
  //            24 =Tributo GARE-SP ITCMD

  TSegmentoN4 = class(TCollectionItem)
  private
    FSegmentoN       : TSegmentoN;
    FReceita         : Integer;
    FTipoContribuinte: TTipoInscricao;
    FidContribuinte  : String; // Tamanho 14
    FDataVencimento  : TDateTime; // DDMMAAAA
    FInscEst         : String; // Tamanho 12
    FNumEtiqueta     : String; // Tamanho 13
    FReferencia      : Integer;
    FNumParcela      : String; // Tamanho 13
    FValorReceita    : Double;
    FJuros           : Double;
    FMulta           : Double;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property SegmentoN: TSegmentoN read FSegmentoN write FSegmentoN;
    property Receita: Integer read FReceita write FReceita;
    property TipoContribuinte: TTipoInscricao read FTipoContribuinte write FTipoContribuinte;
    property idContribuinte: String read FidContribuinte write FidContribuinte;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
    property InscEst: String read FInscEst write FInscEst;
    property NumEtiqueta: String read FNumEtiqueta write FNumEtiqueta;
    property Referencia: Integer read FReferencia write FReferencia;
    property NumParcela: String read FNumParcela write FNumParcela;
    property ValorReceita: Double read FValorReceita write FValorReceita;
    property Juros: Double read FJuros write FJuros;
    property Multa: Double read FMulta write FMulta;
  end;

  TSegmentoN4List = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoN4;
    procedure SetItem(Index: Integer; Value: TSegmentoN4);
    function GetLastItem: TSegmentoN4;
    procedure SetLastItem(const Value: TSegmentoN4);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoN4;
    property Items[Index: Integer]: TSegmentoN4 read GetItem write SetItem; default;
    property Last: TSegmentoN4 read GetLastItem write SetLastItem;
  end;

  // Segmento N5 - IPVA
  // Segmento N6 - DPVAT
  // Segmento N7 - Licenciamento

  TSegmentoN567 = class(TCollectionItem)
  private
    FSegmentoN       : TSegmentoN;
    FTributo         : TIndTributo;
    FReceita         : Integer;
    FTipoContribuinte: TTipoInscricao;
    FidContribuinte  : String; // Tamanho 14
    FExercicio       : Integer;
    FRenavam         : String; // Tamanho 9
    FEstado          : String; // Tamanho 2
    FMunicipio       : Integer;
    FPlaca           : String; // Tamanho 7
    FOpcaoPagamento  : String; // Tamanho 1
    FOpcaoRetirada   : String; // Tamanho 1
    FValorTributo    : Double;
    FDesconto        : Double;
    FDataVencimento  : TDateTime;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property SegmentoN: TSegmentoN read FSegmentoN write FSegmentoN;
    property Tributo: TIndTributo read FTributo write FTributo;
    property Receita: Integer read FReceita write FReceita;
    property TipoContribuinte: TTipoInscricao read FTipoContribuinte write FTipoContribuinte;
    property idContribuinte: String read FidContribuinte write FidContribuinte;
    property Exercicio: Integer read FExercicio write FExercicio;
    property Renavam: String read FRenavam write FRenavam;
    property Estado: String read FEstado write FEstado;
    property Municipio: Integer read FMunicipio write FMunicipio;
    property Placa: String read FPlaca write FPlaca;
    property OpcaoPagamento: String read FOpcaoPagamento write FOpcaoPagamento;
    property OpcaoRetirada: String read FOpcaoRetirada write FOpcaoRetirada;
    property ValorTributo: Double read FValorTributo write FValorTributo;
    property Desconto: Double read FDesconto write FDesconto;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
  end;

  TSegmentoN567List = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoN567;
    procedure SetItem(Index: Integer; Value: TSegmentoN567);
    function GetLastItem: TSegmentoN567;
    procedure SetLastItem(const Value: TSegmentoN567);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoN567;
    property Items[Index: Integer]: TSegmentoN567 read GetItem write SetItem; default;
    property Last: TSegmentoN567 read GetLastItem write SetLastItem;
  end;

  // Segmento N8 - DARJ

  TSegmentoN8 = class(TCollectionItem)
  private
    FSegmentoN           : TSegmentoN;
    FReceita             : Integer;
    FTipoContribuinte    : TTipoInscricao;
    FidContribuinte      : String; // Tamanho 14
    FInscEst             : String; // Tamanho 8
    FOrigem              : String; // Tamanho 16
    FValorPrincipal      : Double;
    FAtualizacaoMonetaria: Double;
    FMora                : Double;
    FMulta               : Double;
    FDataVencimento      : TDateTime; // DDMMAAAA
    FPeriodoParcela      :Integer;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property SegmentoN: TSegmentoN read FSegmentoN write FSegmentoN;
    property Receita: Integer read FReceita write FReceita;
    property TipoContribuinte: TTipoInscricao read FTipoContribuinte write FTipoContribuinte;
    property idContribuinte: String read FidContribuinte write FidContribuinte;
    property InscEst: String read FInscEst write FInscEst;
    property Origem: String read FOrigem write FOrigem;
    property ValorPrincipal: Double read FValorPrincipal write FValorPrincipal;
    property AtualizacaoMonetaria: Double read FAtualizacaoMonetaria write FAtualizacaoMonetaria;
    property Mora: Double read FMora write FMora;
    property Multa: Double read FMulta write FMulta;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
    property PeriodoParcela: Integer read FPeriodoParcela write FPeriodoParcela;
  end;

  TSegmentoN8List = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoN8;
    procedure SetItem(Index: Integer; Value: TSegmentoN8);
    function GetLastItem: TSegmentoN8;
    procedure SetLastItem(const Value: TSegmentoN8);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoN8;
    property Items[Index: Integer]: TSegmentoN8 read GetItem write SetItem; default;
    property Last: TSegmentoN8 read GetLastItem write SetLastItem;
  end;

// Segmento N9 - FGTS- GRF/GRRF/GRDE

  TSegmentoN9 = class(TCollectionItem)
  private
    FSegmentoN        : TSegmentoN;
    FReceita          : Integer;
    FTipoContribuinte : TTipoInscricao;
    FidContribuinte   : String;
    FCodigoBarras     : String;
    FIdentificador    : Integer;
    FLacre            : Integer;
    FLacreDigito: Integer;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property SegmentoN: TSegmentoN read FSegmentoN write FSegmentoN;
    property Receita: Integer read FReceita write FReceita;
    property TipoContribuinte: TTipoInscricao read FTipoContribuinte write FTipoContribuinte;
    property idContribuinte: String read FidContribuinte write FidContribuinte;
    property CodigoBarras: String read FCodigoBarras write FCodigoBarras;
    property Identificador: Integer read FIdentificador write FIdentificador;
    property Lacre: Integer read FLacre write FLacre;
    property LacreDigito: Integer read FLacreDigito write FLacreDigito;
  end;

  TSegmentoN9List = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoN9;
    procedure SetItem(Index: Integer; Value: TSegmentoN9);
    function GetLastItem: TSegmentoN9;
    procedure SetLastItem(const Value: TSegmentoN9);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoN9;
    property Items[Index: Integer]: TSegmentoN9 read GetItem write SetItem; default;
    property Last: TSegmentoN9 read GetLastItem write SetLastItem;
  end;

  // Lote: Pagamento de Tributos com código de barras
  // Serviço: 22 = Pagamento de Contas, Tributos e Impostos

  // Segmentos Obrigatórios de Remessa: O
  // Segmentos Opcionais de Remessa: W*, Z e B
  // * = obrigatório para FGTS convenio 0181 e 0182

  // Estrutura do Segmento O (Obrigatorio)

  TSegmentoO = class(TCollectionItem)
  private
    FCodMovimento: TInstrucaoMovimento; // Tamanho 2
    FCodigoBarras      : String; // Tamanho 44
    FNomeConcessionaria: String; // Tamanho 30
    FDataVencimento    : TDateTime; // DDMMAAAA
    FDataPagamento     : TDateTime; // DDMMAAAA
    FValorPagamento    : Double;
    FSeuNumero         : String; // Tamanho 20
    FNossoNumero       : String; // Tamanho 20
    FCodOcorrencia : String;
    FSegmentoZ: TSegmentoZList;
    FQuantidadeMoeda: Double;
    FValorPago: Double;
    FNotaFiscal: Integer;
    FDescOcorrencia: String;
    procedure SetSegmentoZ(const Value: TSegmentoZList);
    function GetPagamentoLiberado: Boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property CodigoBarras: String read FCodigoBarras write FCodigoBarras;
    property NomeConcessionaria: String read FNomeConcessionaria write FNomeConcessionaria;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
    property DataPagamento: TDateTime read FDataPagamento write FDataPagamento;
    property ValorPagamento: Double read FValorPagamento write FValorPagamento;
    property SeuNumero: String read FSeuNumero write FSeuNumero;
    property NossoNumero: String read FNossoNumero write FNossoNumero;
    property CodOcorrencia: String read FCodOcorrencia write FCodOcorrencia;
    property DescOcorrencia: String read FDescOcorrencia write FDescOcorrencia;
    property SegmentoZ: TSegmentoZList read FSegmentoZ write SetSegmentoZ;
    property QuantidadeMoeda: Double read FQuantidadeMoeda write FQuantidadeMoeda;
    property ValorPago: Double read FValorPago write FValorPago;
    property NotaFiscal: Integer read FNotaFiscal write FNotaFiscal;
    property PagamentoLiberado: Boolean read GetPagamentoLiberado;
  end;

  TSegmentoOList = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoO;
    procedure SetItem(Index: Integer; Value: TSegmentoO);
    function GetLastItem: TSegmentoO;
    procedure SetLastItem(const Value: TSegmentoO);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoO;
    property Items[Index: Integer]: TSegmentoO read GetItem write SetItem; default;
    property Last: TSegmentoO read GetLastItem write SetLastItem;
  end;

  // Lote: Titulos em Cobrança
  // Serviço: 01 = Cobrança

  // Segmentos Obrigatórios de Remessa: P e Q
  // Segmentos Opcionais de Remessa: R, S e Y

  // Estrutura do Segmento P (Obrigatorio)

  TSegmentoP = class(TCollectionItem)
  private
    FCodMovimento: TInstrucaoMovimento; // Tamanho 2
    FContaCorrente       : TContaCorrente;
    FNossoNumero         : String; // Tamanho 20
    FCobranca            : TCobranca;
    FNumeroDocumento     : String; // Tamanho 15
    FVencimento          : TDateTime; // DDMMAAAA
    FValorTitulo         : Double;
    FAgenciaCobradora    : Integer;
    FDV                  : String; // Tamanho 1
    FEspecieTitulo       : Integer;
    FAceito              : String; // Tamanho 1
    FDataEmissao         : TDateTime; // DDMMAAAA
    FJuros               : TAcrescimosDescontos;
    FDesconto            : TAcrescimosDescontos;
    FValorIOF            : Double;
    FValorAbatimento     : Double;
    FUsoEmpresaCedente   : String; // Tamanho 25
    FCodigoProtesto      : Integer; // Tamanho 1
    FPrazoProtesto       : Integer;
    FCodigoBaixaDevolucao: Integer; // Tamanho 1
    FPrazoBaixaDevolucao : Integer;
    FCodigoMoeda         : Integer;
    FNumeroContrato      : Integer;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property ContaCorrente: TContaCorrente read FContaCorrente write FContaCorrente;
    property NossoNumero: String read FNossoNumero write FNossoNumero;
    property Cobranca: TCobranca read FCobranca write FCobranca;
    property NumeroDocumento: String read FNumeroDocumento write FNumeroDocumento;
    property Vencimento: TDateTime read FVencimento write FVencimento;
    property ValorTitulo: Double read FValorTitulo write FValorTitulo;
    property AgenciaCobradora: Integer read FAgenciaCobradora write FAgenciaCobradora;
    property DV: String read FDV write FDV;
    property EspecieTitulo: Integer read FEspecieTitulo write FEspecieTitulo;
    property Aceito: String read FAceito write FAceito;
    property DataEmissao: TDateTime read FDataEmissao write FDataEmissao;
    property Juros: TAcrescimosDescontos read FJuros write FJuros;
    property Desconto: TAcrescimosDescontos read FDesconto write FDesconto;
    property ValorIOF: Double read FValorIOF write FValorIOF;
    property ValorAbatimento: Double read FValorAbatimento write FValorAbatimento;
    property UsoEmpresaCedente: String read FUsoEmpresaCedente write FUsoEmpresaCedente;
    property CodigoProtesto: Integer read FCodigoProtesto write FCodigoProtesto;
    property PrazoProtesto: Integer read FPrazoProtesto write FPrazoProtesto;
    property CodigoBaixaDevolucao: Integer read FCodigoBaixaDevolucao write FCodigoBaixaDevolucao;
    property PrazoBaixaDevolucao: Integer read FPrazoBaixaDevolucao write FPrazoBaixaDevolucao;
    property CodigoMoeda: Integer read FCodigoMoeda write FCodigoMoeda;
    property NumeroContrato: Integer read FNumeroContrato write FNumeroContrato;
  end;

  TSegmentoPList = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoP;
    procedure SetItem(Index: Integer; Value: TSegmentoP);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoP;
    property Items[Index: Integer]: TSegmentoP read GetItem write SetItem; default;
  end;

  // Estrutura do Segmento Q (Obrigatorio)

  TSegmentoQ = class(TCollectionItem)
  private
    FCodMovimento             : TInstrucaoMovimento; // Tamanho 2
    FSacado                   : TSacado;
    FAvalista                 : TAvalista;
    FBancoCorrespondente      : TBanco; // Tamanho 3
    FNossoNumeroCorrespondente: String; // Tamanho 20
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property Sacado: TSacado read FSacado write FSacado;
    property Avalista: TAvalista read FAvalista write FAvalista;
    property BancoCorrespondente: TBanco read FBancoCorrespondente write FBancoCorrespondente;
    property NossoNumeroCorrespondente: String read FNossoNumeroCorrespondente write FNossoNumeroCorrespondente;
  end;

  TSegmentoQList = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoQ;
    procedure SetItem(Index: Integer; Value: TSegmentoQ);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoQ;
    property Items[Index: Integer]: TSegmentoQ read GetItem write SetItem; default;
  end;

  // Estrutura do Segmento R (Opcional)

  TSegmentoR = class(TCollectionItem)
  private
    FCodMovimento         : TInstrucaoMovimento; // Tamanho 2
    FDesconto2            : TAcrescimosDescontos;
    FDesconto3            : TAcrescimosDescontos;
    FMulta                : TAcrescimosDescontos;
    FInformacaoaoSacado   : String; // Tamanho 10
    FInformacao3          : String; // Tamanho 40
    FInformacao4          : String; // Tamanho 40
    FCodOcorrSacado       : String; // Tamanho 8
    FDebitoAutomatico     : TDebitoAutomatico;
    FAvisoDebitoAutomatico: Integer;
    FDescOcorrencia: String; // Tamanho 1
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property Desconto2: TAcrescimosDescontos read FDesconto2 write FDesconto2;
    property Desconto3: TAcrescimosDescontos read FDesconto3 write FDesconto3;
    property Multa: TAcrescimosDescontos read FMulta write FMulta;
    property InformacaoaoSacado: String read FInformacaoaoSacado write FInformacaoaoSacado;
    property Informacao3: String read FInformacao3 write FInformacao3;
    property Informacao4: String read FInformacao4 write FInformacao4;
    property CodOcorrSacado: String read FCodOcorrSacado write FCodOcorrSacado;
    property DescOcorrencia: String read FDescOcorrencia write FDescOcorrencia;
    property DebitoAutomatico: TDebitoAutomatico read FDebitoAutomatico write FDebitoAutomatico;
    property AvisoDebitoAutomatico: Integer read FAvisoDebitoAutomatico write FAvisoDebitoAutomatico;
  end;

  TSegmentoRList = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoR;
    procedure SetItem(Index: Integer; Value: TSegmentoR);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoR;
    property Items[Index: Integer]: TSegmentoR read GetItem write SetItem; default;
  end;

  // Estrutura do Segmento S (Opcional)

  TSegmentoS = class(TCollectionItem)
  private
    FCodMovimento : TInstrucaoMovimento; // Tamanho 2
    FTipoImpressao: Integer;
    FNumerodaLinha: Integer;
    FMensagem     : String; // Tamanho 140
    FTipodeFonte  : Integer;
    FInformacao5  : String; // Tamanho 40
    FInformacao6  : String; // Tamanho 40
    FInformacao7  : String; // Tamanho 40
    FInformacao8  : String; // Tamanho 40
    FInformacao9  : String; // Tamanho 40
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property TipoImpressao: Integer read FTipoImpressao write FTipoImpressao;
    property NumerodaLinha: Integer read FNumerodaLinha write FNumerodaLinha;
    property Mensagem: String read FMensagem write FMensagem;
    property TipodeFonte: Integer read FTipodeFonte write FTipodeFonte;
    property Informacao5: String read FInformacao5 write FInformacao5;
    property Informacao6: String read FInformacao6 write FInformacao6;
    property Informacao7: String read FInformacao7 write FInformacao7;
    property Informacao8: String read FInformacao8 write FInformacao8;
    property Informacao9: String read FInformacao9 write FInformacao9;
  end;

  TSegmentoSList = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoS;
    procedure SetItem(Index: Integer; Value: TSegmentoS);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoS;
    property Items[Index: Integer]: TSegmentoS read GetItem write SetItem; default;
  end;


  // Estrutura do Segmento Y (Opcional)

  TSegmentoY = class(TCollectionItem)
  private
    FCodMovimento: TInstrucaoMovimento; // Tamanho 2
    FCodRegistro : Integer; // Tamanho 2

    {Incompleto}
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property CodRegistro: Integer read FCodRegistro write FCodRegistro;
  end;

  TSegmentoYList = class(TCollection)
  private
    function GetItem(Index: Integer): TSegmentoY;
    procedure SetItem(Index: Integer; Value: TSegmentoY);
  public
    constructor Create(AOwner: TLote);
    function Add: TSegmentoY;
    property Items[Index: Integer]: TSegmentoY read GetItem write SetItem; default;
  end;

  // Lote

  TLote = class(TCollectionItem)
  private
    FRegistro1: TRegistro1;
    FRegistro5: TRegistro5;
    FSegmentoA: TSegmentoAList;
    FSegmentoG: TSegmentoGList;
    FSegmentoJ: TSegmentoJList;
    FSegmentoN1: TSegmentoN1List;
    FSegmentoN2: TSegmentoN2List;
    FSegmentoN3: TSegmentoN3List;
    FSegmentoN4: TSegmentoN4List;
    FSegmentoN567: TSegmentoN567List;
    FSegmentoN8: TSegmentoN8List;
    FSegmentoN9: TSegmentoN9List;
    FSegmentoO: TSegmentoOList;
    FSegmentoP: TSegmentoPList;
    FSegmentoQ: TSegmentoQList;
    FSegmentoR: TSegmentoRList;
    FSegmentoS: TSegmentoSList;
    FSegmentoW: TSegmentoWList;
    FSegmentoY: TSegmentoYList;

    procedure SetSegmentoA(const Value: TSegmentoAList);
    procedure SetSegmentoG(const Value: TSegmentoGList);
    procedure SetSegmentoJ(const Value: TSegmentoJList);
    procedure SetSegmentoN1(const Value: TSegmentoN1List);
    procedure SetSegmentoN2(const Value: TSegmentoN2List);
    procedure SetSegmentoN3(const Value: TSegmentoN3List);
    procedure SetSegmentoN4(const Value: TSegmentoN4List);
    procedure SetSegmentoN567(const Value: TSegmentoN567List);
    procedure SetSegmentoN8(const Value: TSegmentoN8List);
    procedure SetSegmentoN9(const Value: TSegmentoN9List);
    procedure SetSegmentoO(const Value: TSegmentoOList);
    procedure SetSegmentoP(const Value: TSegmentoPList);
    procedure SetSegmentoQ(const Value: TSegmentoQList);
    procedure SetSegmentoR(const Value: TSegmentoRList);
    procedure SetSegmentoS(const Value: TSegmentoSList);
    procedure SetSegmentoW(const Value: TSegmentoWList);
    procedure SetSegmentoY(const Value: TSegmentoYList);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
  published
    property Registro1: TRegistro1 read FRegistro1 write FRegistro1;

    property SegmentoA: TSegmentoAList read FSegmentoA write SetSegmentoA;
    property SegmentoG: TSegmentoGList read FSegmentoG write SetSegmentoG;
    property SegmentoJ: TSegmentoJList read FSegmentoJ write SetSegmentoJ;
    property SegmentoN1: TSegmentoN1List read FSegmentoN1 write SetSegmentoN1;
    property SegmentoN2: TSegmentoN2List read FSegmentoN2 write SetSegmentoN2;
    property SegmentoN3: TSegmentoN3List read FSegmentoN3 write SetSegmentoN3;
    property SegmentoN4: TSegmentoN4List read FSegmentoN4 write SetSegmentoN4;
    property SegmentoN567: TSegmentoN567List read FSegmentoN567 write SetSegmentoN567;
    property SegmentoN8: TSegmentoN8List read FSegmentoN8 write SetSegmentoN8;
    property SegmentoN9: TSegmentoN9List read FSegmentoN9 write SetSegmentoN9;
    property SegmentoO: TSegmentoOList read FSegmentoO write SetSegmentoO;
    property SegmentoP: TSegmentoPList read FSegmentoP write SetSegmentoP;
    property SegmentoQ: TSegmentoQList read FSegmentoQ write SetSegmentoQ;
    property SegmentoR: TSegmentoRList read FSegmentoR write SetSegmentoR;
    property SegmentoS: TSegmentoSList read FSegmentoS write SetSegmentoS;
    property SegmentoW: TSegmentoWList read FSegmentoW write SetSegmentoW;
    property SegmentoY: TSegmentoYList read FSegmentoY write SetSegmentoY;

    property Registro5: TRegistro5 read FRegistro5 write FRegistro5;
  end;

  TLoteList = class(TCollection)
  private
    function GetItem(Index: Integer): TLote;
    procedure SetItem(Index: Integer; Value: TLote);
    function GetLastItem: TLote;
    procedure SetLastItem(const Value: TLote);
  public
    constructor Create(AOwner: TPagFor);
    function Add: TLote;
    property Items[Index: Integer]: TLote read GetItem write SetItem; default;
    property Last: TLote read GetLastItem write SetLastItem;
  end;

  TPagFor = class(TPersistent)
  private
    FAtivo: Boolean;
    FGeral: TGeral;
    FRegistro0: TRegistro0;
    FLote: TLoteList;
    FRegistro9: TRegistro9;

    procedure SetLote(const Value: TLoteList);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Ativo: Boolean read FAtivo write FAtivo;
    property Geral: TGeral read FGeral write FGeral;

    property Registro0: TRegistro0 read FRegistro0 write FRegistro0;
    property Lote: TLoteList read FLote write SetLote;
    property Registro9: TRegistro9 read FRegistro9 write FRegistro9;
  end;

const
  PAGAMENTO_LIBERADO_BANCO = '00 BD BDCI BDCD BDCN'; // Os códigos de aprovação de pagamento (pagamento efetuado)
  PAGAMENTO_LIBERADO_AVISO = '00 BD';                // Os códigos que nào vai gerar aviso
// OBS: estes dados foram pegos do manual do ITAU.
// BD   - Pagamento Agendado.
// BDCI - Pagamento acatado, porém o CPF/CNPJ é inválido.
// BDCD - Pagamento acatado, porém o CPF/CNPJ informado não é o mesmo que está cadastrado para a agência conta creditada.
// BDCN - Pagamento acatado, porém a agência/conta informada (ainda) não existe.

implementation

{ TContaCorrente }

constructor TContaCorrente.Create;
begin
  inherited Create;

  FAgencia := TAgencia.Create;
  FConta   := TConta.Create;
end;

destructor TContaCorrente.Destroy;
begin
  FAgencia.Free;
  FConta.Free;

  inherited;
end;

{ TEmpresa }

constructor TEmpresa.Create;
begin
  inherited Create;

  FInscricao     := TInscricao.Create;
  FContaCorrente := TContaCorrente.Create;
end;

destructor TEmpresa.Destroy;
begin
  FInscricao.Free;
  FContaCorrente.Free;

  inherited;
end;

{ TSacado }

constructor TSacado.Create;
begin
  inherited Create;

  FInscricao := TInscricao.Create;
end;

destructor TSacado.Destroy;
begin
  FInscricao.Free;

  inherited;
end;

{ TAvalista }

constructor TAvalista.Create;
begin
  inherited Create;

  FInscricao := TInscricao.Create;
end;

destructor TAvalista.Destroy;
begin
  FInscricao.Free;

  inherited;
end;

{ TDebitoAutomatico }

constructor TDebitoAutomatico.Create;
begin
  inherited Create;

  FContaCorrente := TContaCorrente.Create;
end;

destructor TDebitoAutomatico.Destroy;
begin
  FContaCorrente.Free;

  inherited;
end;

{ TFavorecido }

constructor TFavorecido.Create;
begin
  inherited Create;

  FContaCorrente := TContaCorrente.Create;
  FInscricao := TInscricao.Create;
end;

destructor TFavorecido.Destroy;
begin
  FContaCorrente.Free;
  FInscricao.Free;

  inherited;
end;

{ TCredito }

constructor TCredito.Create;
begin
  inherited Create;

  FMoeda := TMoeda.Create;
end;

destructor TCredito.Destroy;
begin
  FMoeda.Free;

  inherited;
end;

{ TRegistro0 }

constructor TRegistro0.Create(AOwner: TPagFor);
begin
  inherited Create;

  FEmpresa := TEmpresa.Create;
  FArquivo := TArquivo.Create;
  FAviso   := TAvisoList.Create;
end;

destructor TRegistro0.Destroy;
begin
  FEmpresa.Free;
  FArquivo.Free;
  FAviso.Free;

  inherited;
end;

{ TRegistro9 }

constructor TRegistro9.Create(AOwner: TPagFor);
begin
  inherited Create;

  FTotais := TTotais.Create;
end;

destructor TRegistro9.Destroy;
begin
  FTotais.Free;

  inherited;
end;

{ TRegistro1 }

constructor TRegistro1.Create(AOwner: TLote);
begin
  inherited Create;

  FServico          := TServico.Create;
  FEmpresa          := TEmpresa .Create;
  FEndereco         := TEndereco.Create;
  FControleCobranca := TControleCobranca.Create;
end;

destructor TRegistro1.Destroy;
begin
  FServico.Free;
  FEmpresa.Free;
  FEndereco.Free;
  FControleCobranca.Free;

  inherited;
end;

{ TRegistro5 }

constructor TRegistro5.Create(AOwner: TLote);
begin
  inherited Create;

  FTotalCobrancaSimples    := TTitulos.Create;
  FTotalCobrancaVinculada  := TTitulos.Create;
  FTotalCobrancaCaucionada := TTitulos.Create;
  FTotalCobrancaDescontada := TTitulos.Create;
end;

destructor TRegistro5.Destroy;
begin
  FTotalCobrancaSimples.Free;
  FTotalCobrancaVinculada.Free;
  FTotalCobrancaCaucionada.Free;
  FTotalCobrancaDescontada.Free;

  inherited;
end;

{ TSegmentoA }

constructor TSegmentoA.Create;
begin
  FFavorecido := TFavorecido.Create;
  FCredito    := TCredito.Create;

  FSegmentoB := TSegmentoBList.Create{(Self)};
  FSegmentoC := TSegmentoCList.Create{(Self)};
  FSegmentoD := TSegmentoDList.Create{(Self)};
  FSegmentoE := TSegmentoEList.Create{(Self)};
  FSegmentoF := TSegmentoFList.Create{(Self)};
  FSegmentoZ := TSegmentoZList.Create{(Self)};
end;

destructor TSegmentoA.Destroy;
begin
  FFavorecido.Free;
  FCredito.Free;
  FSegmentoB.Free;
  FSegmentoC.Free;
  FSegmentoD.Free;
  FSegmentoE.Free;
  FSegmentoF.Free;
  FSegmentoZ.Free;

  inherited;
end;


function TSegmentoA.GetPagamentoLiberado: Boolean;
var
  I: Integer;
begin
  Result := POS(CodOcorrencia, PAGAMENTO_LIBERADO_BANCO) > 0;

  if Result then
  begin
    for I := 0 to SegmentoB.Count - 1 do
      if Result then
        Result := POS(SegmentoB.Items[I].CodOcorrencia, PAGAMENTO_LIBERADO_BANCO) > 0;

    if not Result then
      Exit;

    for I := 0 to SegmentoC.Count - 1 do
      if Result then
        Result := POS(SegmentoC.Items[I].CodOcorrencia, PAGAMENTO_LIBERADO_BANCO) > 0;

    if not Result then
      Exit;

    for I := 0 to SegmentoD.Count - 1 do
      if Result then
        Result := POS(SegmentoD.Items[I].CodOcorrencia, PAGAMENTO_LIBERADO_BANCO) > 0;

    if not Result then
      Exit;

    for I := 0 to SegmentoE.Count - 1 do
      if Result then
        Result := POS(SegmentoE.Items[I].CodOcorrencia, PAGAMENTO_LIBERADO_BANCO) > 0;

    if not Result then
      Exit;

    for I := 0 to SegmentoF.Count - 1 do
      if Result then
        Result := POS(SegmentoF.Items[I].CodOcorrencia, PAGAMENTO_LIBERADO_BANCO) > 0;
  end;
end;

procedure TSegmentoA.SetSegmentoB(const Value: TSegmentoBList);
begin
  FSegmentoB := Value;
end;

procedure TSegmentoA.SetSegmentoC(const Value: TSegmentoCList);
begin
  FSegmentoC := Value;
end;

procedure TSegmentoA.SetSegmentoD(const Value: TSegmentoDList);
begin
  FSegmentoD := Value;
end;

procedure TSegmentoA.SetSegmentoE(const Value: TSegmentoEList);
begin
  FSegmentoE := Value;
end;

procedure TSegmentoA.SetSegmentoF(const Value: TSegmentoFList);
begin
  FSegmentoF := Value;
end;

procedure TSegmentoA.SetSegmentoZ(const Value: TSegmentoZList);
begin
  FSegmentoZ := Value;
end;

{ TSegmentoAList }

function TSegmentoAList.Add: TSegmentoA;
begin
  Result := TSegmentoA(inherited Add);
  Result.create;
end;

constructor TSegmentoAList.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoA);
end;

function TSegmentoAList.GetItem(Index: Integer): TSegmentoA;
begin
  Result := TSegmentoA(inherited GetItem(Index));
end;

function TSegmentoAList.GetLastItem: TSegmentoA;
begin
  Result := TSegmentoA(inherited GetItem(Count-1));
end;

procedure TSegmentoAList.SetItem(Index: Integer; Value: TSegmentoA);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoAList.SetLastItem(const Value: TSegmentoA);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoB }

constructor TSegmentoB.Create;
begin
  FInscricao := TInscricao.Create;
  FEndereco  := TEndereco.Create;
end;

destructor TSegmentoB.Destroy;
begin
  FInscricao.Free;
  FEndereco.Free;

  inherited;
end;

{ TSegmentoBList }

function TSegmentoBList.Add: TSegmentoB;
begin
  Result := TSegmentoB(inherited Add);
  Result.create;
end;

constructor TSegmentoBList.Create{(AOwner: TLote)};
begin
  inherited Create(TSegmentoB);
end;

function TSegmentoBList.GetItem(Index: Integer): TSegmentoB;
begin
  Result := TSegmentoB(inherited GetItem(Index));
end;

function TSegmentoBList.GetLastItem: TSegmentoB;
begin
  Result := TSegmentoB(inherited GetItem(Count-1));
end;

procedure TSegmentoBList.SetItem(Index: Integer; Value: TSegmentoB);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoBList.SetLastItem(const Value: TSegmentoB);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoC }

constructor TSegmentoC.Create;
begin
  FContaCorrente := TContaCorrente.Create;
end;

destructor TSegmentoC.Destroy;
begin
  FContaCorrente.Free;

  inherited;
end;

{ TSegmentoCList }

function TSegmentoCList.Add: TSegmentoC;
begin
  Result := TSegmentoC(inherited Add);
  Result.create;
end;

constructor TSegmentoCList.Create{(AOwner: TLote)};
begin
  inherited Create(TSegmentoC);
end;

function TSegmentoCList.GetItem(Index: Integer): TSegmentoC;
begin
  Result := TSegmentoC(inherited GetItem(Index));
end;

function TSegmentoCList.GetLastItem: TSegmentoC;
begin
  Result := TSegmentoC(inherited GetItem(Count-1));
end;

procedure TSegmentoCList.SetItem(Index: Integer; Value: TSegmentoC);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoCList.SetLastItem(const Value: TSegmentoC);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoJ }

constructor TSegmentoJ.Create;
begin
  FSegmentoJ52 := TSegmentoJ52List.Create;
  FSegmentoB := TSegmentoBList.Create;
  FSegmentoC := TSegmentoCList.Create;
  FSegmentoZ := TSegmentoZList.Create;
end;

destructor TSegmentoJ.Destroy;
begin
  FSegmentoJ52.Free;
  FSegmentoB.Free;
  FSegmentoC.Free;
  FSegmentoZ.Free;

  inherited;
end;

function TSegmentoJ.GetPagamentoLiberado: Boolean;
var
  I: Integer;
begin
  Result := POS(CodOcorrencia, PAGAMENTO_LIBERADO_BANCO) > 0;

  if Result then
  begin
    for I := 0 to SegmentoB.Count - 1 do
      if Result then
        Result := POS(SegmentoB.Items[I].CodOcorrencia, PAGAMENTO_LIBERADO_BANCO) > 0;

    if not Result then
      Exit;

    for I := 0 to SegmentoC.Count - 1 do
      if Result then
        Result := POS(SegmentoC.Items[I].CodOcorrencia, PAGAMENTO_LIBERADO_BANCO) > 0;
  end;
end;

procedure TSegmentoJ.SetSegmentoB(const Value: TSegmentoBList);
begin
  FSegmentoB := Value;
end;

procedure TSegmentoJ.SetSegmentoC(const Value: TSegmentoCList);
begin
  FSegmentoC := Value;
end;

procedure TSegmentoJ.SetSegmentoJ52(const Value: TSegmentoJ52List);
begin
  FSegmentoJ52 := Value;
end;

procedure TSegmentoJ.SetSegmentoZ(const Value: TSegmentoZList);
begin
  FSegmentoZ := Value;
end;

{ TSegmentoJList }

function TSegmentoJList.Add: TSegmentoJ;
begin
  Result := TSegmentoJ(inherited Add);
  Result.create;
end;

constructor TSegmentoJList.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoJ);
end;

function TSegmentoJList.GetItem(Index: Integer): TSegmentoJ;
begin
  Result := TSegmentoJ(inherited GetItem(Index));
end;

function TSegmentoJList.GetLastItem: TSegmentoJ;
begin
  Result := TSegmentoJ(inherited GetItem(Count-1));
end;

procedure TSegmentoJList.SetItem(Index: Integer; Value: TSegmentoJ);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoJList.SetLastItem(const Value: TSegmentoJ);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoN1 }

constructor TSegmentoN1.Create;
begin
  FSegmentoN := TSegmentoN.Create;
end;

destructor TSegmentoN1.Destroy;
begin
  FSegmentoN.Free;

  inherited;
end;

{ TSegmentoN1List }

function TSegmentoN1List.Add: TSegmentoN1;
begin
  Result := TSegmentoN1(inherited Add);
  Result.create;
end;

constructor TSegmentoN1List.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoN1);
end;

function TSegmentoN1List.GetItem(Index: Integer): TSegmentoN1;
begin
  Result := TSegmentoN1(inherited GetItem(Index));
end;

function TSegmentoN1List.GetLastItem: TSegmentoN1;
begin
  Result := TSegmentoN1(inherited GetItem(Count-1));
end;

procedure TSegmentoN1List.SetItem(Index: Integer; Value: TSegmentoN1);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoN1List.SetLastItem(const Value: TSegmentoN1);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoN2 }

constructor TSegmentoN2.Create;
begin
  FSegmentoN := TSegmentoN.Create;
end;

destructor TSegmentoN2.Destroy;
begin
  FSegmentoN.Free;
  inherited;
end;

{ TSegmentoN2List }

function TSegmentoN2List.Add: TSegmentoN2;
begin
  Result := TSegmentoN2(inherited Add);
  Result.create;
end;

constructor TSegmentoN2List.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoN2);
end;

function TSegmentoN2List.GetItem(Index: Integer): TSegmentoN2;
begin
  Result := TSegmentoN2(inherited GetItem(Index));
end;

function TSegmentoN2List.GetLastItem: TSegmentoN2;
begin
  Result := TSegmentoN2(inherited GetItem(Count-1));
end;

procedure TSegmentoN2List.SetItem(Index: Integer; Value: TSegmentoN2);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoN2List.SetLastItem(const Value: TSegmentoN2);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoN3 }

constructor TSegmentoN3.Create;
begin
  FSegmentoN := TSegmentoN.Create;
end;

destructor TSegmentoN3.Destroy;
begin
  FSegmentoN.Free;

  inherited;
end;

{ TSegmentoN3List }

function TSegmentoN3List.Add: TSegmentoN3;
begin
  Result := TSegmentoN3(inherited Add);
  Result.create;
end;

constructor TSegmentoN3List.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoN3);
end;

function TSegmentoN3List.GetItem(Index: Integer): TSegmentoN3;
begin
  Result := TSegmentoN3(inherited GetItem(Index));
end;

function TSegmentoN3List.GetLastItem: TSegmentoN3;
begin
  Result := TSegmentoN3(inherited GetItem(Count-1));
end;

procedure TSegmentoN3List.SetItem(Index: Integer; Value: TSegmentoN3);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoN3List.SetLastItem(const Value: TSegmentoN3);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoN4 }

constructor TSegmentoN4.Create;
begin
  FSegmentoN := TSegmentoN.Create;
end;

destructor TSegmentoN4.Destroy;
begin
  FSegmentoN.Free;
  inherited;
end;

{ TSegmentoN4List }

function TSegmentoN4List.Add: TSegmentoN4;
begin
  Result := TSegmentoN4(inherited Add);
  Result.create;
end;

constructor TSegmentoN4List.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoN4);
end;

function TSegmentoN4List.GetItem(Index: Integer): TSegmentoN4;
begin
  Result := TSegmentoN4(inherited GetItem(Index));
end;

function TSegmentoN4List.GetLastItem: TSegmentoN4;
begin
  Result := TSegmentoN4(inherited GetItem(Count-1));
end;

procedure TSegmentoN4List.SetItem(Index: Integer; Value: TSegmentoN4);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoN4List.SetLastItem(const Value: TSegmentoN4);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoN567 }

constructor TSegmentoN567.Create;
begin
  FSegmentoN := TSegmentoN.Create;
end;

destructor TSegmentoN567.Destroy;
begin
  FSegmentoN.Free;

  inherited;
end;

{ TSegmentoN567List }

function TSegmentoN567List.Add: TSegmentoN567;
begin
  Result := TSegmentoN567(inherited Add);
  Result.create;
end;

constructor TSegmentoN567List.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoN567);
end;

function TSegmentoN567List.GetItem(Index: Integer): TSegmentoN567;
begin
  Result := TSegmentoN567(inherited GetItem(Index));
end;

function TSegmentoN567List.GetLastItem: TSegmentoN567;
begin
  Result := TSegmentoN567(inherited GetItem(Count-1));
end;

procedure TSegmentoN567List.SetItem(Index: Integer; Value: TSegmentoN567);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoN567List.SetLastItem(const Value: TSegmentoN567);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoN8 }

constructor TSegmentoN8.Create;
begin
  FSegmentoN := TSegmentoN.Create;
end;

destructor TSegmentoN8.Destroy;
begin
  FSegmentoN.Free;
  inherited;
end;

{ TSegmentoN8List }

function TSegmentoN8List.Add: TSegmentoN8;
begin
  Result := TSegmentoN8(inherited Add);
  Result.create;
end;

constructor TSegmentoN8List.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoN8);
end;

function TSegmentoN8List.GetItem(Index: Integer): TSegmentoN8;
begin
  Result := TSegmentoN8(inherited GetItem(Index));
end;

function TSegmentoN8List.GetLastItem: TSegmentoN8;
begin
  Result := TSegmentoN8(inherited GetItem(Count-1));
end;

procedure TSegmentoN8List.SetItem(Index: Integer; Value: TSegmentoN8);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoN8List.SetLastItem(const Value: TSegmentoN8);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoO }

constructor TSegmentoO.Create;
begin
  FSegmentoZ := TSegmentoZList.Create;
end;

destructor TSegmentoO.Destroy;
begin
  FSegmentoZ.Free;
  inherited;
end;

function TSegmentoO.GetPagamentoLiberado: Boolean;
begin
  Result := POS(CodOcorrencia, PAGAMENTO_LIBERADO_BANCO) > 0;
end;

procedure TSegmentoO.SetSegmentoZ(const Value: TSegmentoZList);
begin
  FSegmentoZ := Value;
end;

{ TSegmentoOList }

function TSegmentoOList.Add: TSegmentoO;
begin
  Result := TSegmentoO(inherited Add);
  Result.create;
end;

constructor TSegmentoOList.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoO);
end;

function TSegmentoOList.GetItem(Index: Integer): TSegmentoO;
begin
  Result := TSegmentoO(inherited GetItem(Index));
end;

function TSegmentoOList.GetLastItem: TSegmentoO;
begin
  Result := TSegmentoO(inherited GetItem(Count-1));
end;

procedure TSegmentoOList.SetItem(Index: Integer; Value: TSegmentoO);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoOList.SetLastItem(const Value: TSegmentoO);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoP }

constructor TSegmentoP.Create;
begin
  FContaCorrente := TContaCorrente.Create;
  FCobranca      := TCobranca.Create;
  FJuros         := TAcrescimosDescontos.Create;
  FDesconto      := TAcrescimosDescontos.Create;
end;

destructor TSegmentoP.Destroy;
begin
  FContaCorrente.Free;
  FCobranca.Free;
  FJuros.Free;
  FDesconto.Free;

  inherited;
end;

{ TSegmentoPList }

function TSegmentoPList.Add: TSegmentoP;
begin
  Result := TSegmentoP(inherited Add);
  Result.create;
end;

constructor TSegmentoPList.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoP);
end;

function TSegmentoPList.GetItem(Index: Integer): TSegmentoP;
begin
  Result := TSegmentoP(inherited GetItem(Index));
end;

procedure TSegmentoPList.SetItem(Index: Integer; Value: TSegmentoP);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoQ }

constructor TSegmentoQ.Create;
begin
  FSacado   := TSacado.Create;
  FAvalista := TAvalista.Create;
end;

destructor TSegmentoQ.Destroy;
begin
  FSacado.Free;
  FAvalista.Free;

  inherited;
end;

{ TSegmentoQList }

function TSegmentoQList.Add: TSegmentoQ;
begin
  Result := TSegmentoQ(inherited Add);
  Result.create;
end;

constructor TSegmentoQList.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoQ);
end;

function TSegmentoQList.GetItem(Index: Integer): TSegmentoQ;
begin
  Result := TSegmentoQ(inherited GetItem(Index));
end;

procedure TSegmentoQList.SetItem(Index: Integer; Value: TSegmentoQ);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoR }

constructor TSegmentoR.Create;
begin
  FDesconto2        := TAcrescimosDescontos.Create;
  FDesconto3        := TAcrescimosDescontos.Create;
  FMulta            := TAcrescimosDescontos.Create;
  FDebitoAutomatico := TDebitoAutomatico.Create;
end;

destructor TSegmentoR.Destroy;
begin
  FDesconto2.Free;
  FDesconto3.Free;
  FMulta.Free;
  FDebitoAutomatico.Free;

  inherited;
end;

{ TSegmentoRList }

function TSegmentoRList.Add: TSegmentoR;
begin
  Result := TSegmentoR(inherited Add);
  Result.create;
end;

constructor TSegmentoRList.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoR);
end;

function TSegmentoRList.GetItem(Index: Integer): TSegmentoR;
begin
  Result := TSegmentoR(inherited GetItem(Index));
end;

procedure TSegmentoRList.SetItem(Index: Integer; Value: TSegmentoR);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoS }

constructor TSegmentoS.Create;
begin

end;

destructor TSegmentoS.Destroy;
begin

  inherited;
end;

{ TSegmentoSList }

function TSegmentoSList.Add: TSegmentoS;
begin
  Result := TSegmentoS(inherited Add);
  Result.create;
end;

constructor TSegmentoSList.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoS);
end;

function TSegmentoSList.GetItem(Index: Integer): TSegmentoS;
begin
  Result := TSegmentoS(inherited GetItem(Index));
end;

procedure TSegmentoSList.SetItem(Index: Integer; Value: TSegmentoS);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoW }

constructor TSegmentoW.Create;
begin

end;

destructor TSegmentoW.Destroy;
begin

  inherited;
end;

{ TSegmentoWList }

function TSegmentoWList.Add: TSegmentoW;
begin
  Result := TSegmentoW(inherited Add);
  Result.create;
end;

constructor TSegmentoWList.Create;
begin
  inherited Create(TSegmentoW);
end;

function TSegmentoWList.GetItem(Index: Integer): TSegmentoW;
begin
  Result := TSegmentoW(inherited GetItem(Index));
end;

function TSegmentoWList.GetLastItem: TSegmentoW;
begin
  Result := TSegmentoW(inherited GetItem(Count-1));
end;

procedure TSegmentoWList.SetItem(Index: Integer; Value: TSegmentoW);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoWList.SetLastItem(const Value: TSegmentoW);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoY }

constructor TSegmentoY.Create;
begin

end;

destructor TSegmentoY.Destroy;
begin

  inherited;
end;

{ TSegmentoYList }

function TSegmentoYList.Add: TSegmentoY;
begin
  Result := TSegmentoY(inherited Add);
  Result.create;
end;

constructor TSegmentoYList.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoY);
end;

function TSegmentoYList.GetItem(Index: Integer): TSegmentoY;
begin
  Result := TSegmentoY(inherited GetItem(Index));
end;

procedure TSegmentoYList.SetItem(Index: Integer; Value: TSegmentoY);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoZ }

constructor TSegmentoZ.Create;
begin

end;

destructor TSegmentoZ.Destroy;
begin

  inherited;
end;

{ TSegmentoZList }

function TSegmentoZList.Add: TSegmentoZ;
begin
  Result := TSegmentoZ(inherited Add);
  Result.create;
end;

constructor TSegmentoZList.Create{(AOwner: TLote)};
begin
  inherited Create(TSegmentoZ);
end;

function TSegmentoZList.GetItem(Index: Integer): TSegmentoZ;
begin
  Result := TSegmentoZ(inherited GetItem(Index));
end;

function TSegmentoZList.GetLastItem: TSegmentoZ;
begin
  Result := TSegmentoZ(inherited GetItem(Count-1));
end;

procedure TSegmentoZList.SetItem(Index: Integer; Value: TSegmentoZ);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoZList.SetLastItem(const Value: TSegmentoZ);
begin
  inherited SetItem(Count-1, Value);
end;

{ TPagFor }

constructor TPagFor.Create;
begin
  FGeral := TGeral.Create;

  FRegistro0 := TRegistro0.Create(Self);
  FLote      := TLoteList.Create(Self);
  FRegistro9 := TRegistro9.Create(Self);
end;

destructor TPagFor.Destroy;
begin
  FGeral.Free;

  FRegistro0.Free;
  FLote.Free;
  FRegistro9.Free;

  inherited;
end;

procedure TPagFor.SetLote(const Value: TLoteList);
begin
  FLote := Value;
end;

{ TLote }

constructor TLote.Create;
begin
  FRegistro1 := TRegistro1.Create(Self);

  FSegmentoA := TSegmentoAList.Create(Self);
  FSegmentoG := TSegmentoGList.Create(Self);
  FSegmentoJ := TSegmentoJList.Create(Self);
  FSegmentoN1 := TSegmentoN1List.Create(Self);
  FSegmentoN2 := TSegmentoN2List.Create(Self);
  FSegmentoN3 := TSegmentoN3List.Create(Self);
  FSegmentoN4 := TSegmentoN4List.Create(Self);
  FSegmentoN567 := TSegmentoN567List.Create(Self);
  FSegmentoN8 := TSegmentoN8List.Create(Self);
  FSegmentoN9 := TSegmentoN9List.Create(Self);
  FSegmentoO := TSegmentoOList.Create(Self);
  FSegmentoP := TSegmentoPList.Create(Self);
  FSegmentoQ := TSegmentoQList.Create(Self);
  FSegmentoR := TSegmentoRList.Create(Self);
  FSegmentoS := TSegmentoSList.Create(Self);
  FSegmentoY := TSegmentoYList.Create(Self);

  FRegistro5 := TRegistro5.Create(Self);
end;

destructor TLote.Destroy;
begin
  FRegistro1.Free;

  FSegmentoA.Free;
  FSegmentoG.Free;
  FSegmentoJ.Free;
  FSegmentoN1.Free;
  FSegmentoN2.Free;
  FSegmentoN3.Free;
  FSegmentoN4.Free;
  FSegmentoN567.Free;
  FSegmentoN8.Free;
  FSegmentoN9.Free;
  FSegmentoO.Free;
  FSegmentoP.Free;
  FSegmentoQ.Free;
  FSegmentoR.Free;
  FSegmentoS.Free;
  FSegmentoW.Free;
  FSegmentoY.Free;

  FRegistro5.Free;

  inherited;
end;

procedure TLote.SetSegmentoA(const Value: TSegmentoAList);
begin
  FSegmentoA := Value;
end;

procedure TLote.SetSegmentoG(const Value: TSegmentoGList);
begin
  FSegmentoG := Value;
end;

procedure TLote.SetSegmentoJ(const Value: TSegmentoJList);
begin
  FSegmentoJ := Value;
end;

procedure TLote.SetSegmentoN1(const Value: TSegmentoN1List);
begin
  FSegmentoN1 := Value;
end;

procedure TLote.SetSegmentoN2(const Value: TSegmentoN2List);
begin
  FSegmentoN2 := Value;
end;

procedure TLote.SetSegmentoN3(const Value: TSegmentoN3List);
begin
  FSegmentoN3 := Value;
end;

procedure TLote.SetSegmentoN4(const Value: TSegmentoN4List);
begin
  FSegmentoN4 := Value;
end;

procedure TLote.SetSegmentoN567(const Value: TSegmentoN567List);
begin
  FSegmentoN567 := Value;
end;

procedure TLote.SetSegmentoN8(const Value: TSegmentoN8List);
begin
  FSegmentoN8 := Value;
end;

procedure TLote.SetSegmentoN9(const Value: TSegmentoN9List);
begin
  FSegmentoN9 := Value;
end;

procedure TLote.SetSegmentoO(const Value: TSegmentoOList);
begin
  FSegmentoO := Value;
end;

procedure TLote.SetSegmentoP(const Value: TSegmentoPList);
begin
  FSegmentoP := Value;
end;

procedure TLote.SetSegmentoQ(const Value: TSegmentoQList);
begin
  FSegmentoQ := Value;
end;

procedure TLote.SetSegmentoR(const Value: TSegmentoRList);
begin
  FSegmentoR := Value;
end;

procedure TLote.SetSegmentoS(const Value: TSegmentoSList);
begin
  FSegmentoS := Value;
end;

procedure TLote.SetSegmentoW(const Value: TSegmentoWList);
begin
  FSegmentoW := Value;
end;

procedure TLote.SetSegmentoY(const Value: TSegmentoYList);
begin
  FSegmentoY := Value;
end;

{ TLoteList }

function TLoteList.Add: TLote;
begin
  Result := TLote(inherited Add);
  Result.create;
end;

constructor TLoteList.Create(AOwner: TPagFor);
begin
  inherited Create(TLote);
end;

function TLoteList.GetItem(Index: Integer): TLote;
begin
  Result := TLote(inherited GetItem(Index));
end;

function TLoteList.GetLastItem: TLote;
begin
  Result := TLote(inherited GetItem(Count-1));
end;

procedure TLoteList.SetItem(Index: Integer; Value: TLote);
begin
  inherited SetItem(Index, Value);
end;

procedure TLoteList.SetLastItem(const Value: TLote);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoDList }

function TSegmentoDList.Add: TSegmentoD;
begin
  Result := TSegmentoD(inherited Add);
  Result.create;
end;

constructor TSegmentoDList.Create{(AOwner: TLote)};
begin
  inherited Create(TSegmentoD);
end;

function TSegmentoDList.GetItem(Index: Integer): TSegmentoD;
begin
  Result := TSegmentoD(inherited GetItem(Index));
end;

function TSegmentoDList.GetLastItem: TSegmentoD;
begin
  Result := TSegmentoD(inherited GetItem(Count-1));
end;

procedure TSegmentoDList.SetItem(Index: Integer; Value: TSegmentoD);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoDList.SetLastItem(const Value: TSegmentoD);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoD }

constructor TSegmentoD.Create;
begin

end;

destructor TSegmentoD.Destroy;
begin

  inherited;
end;

{ TSegmentoEList }

function TSegmentoEList.Add: TSegmentoE;
begin
  Result := TSegmentoE(inherited Add);
  Result.create;
end;

constructor TSegmentoEList.Create{(AOwner: TLote)};
begin
  inherited Create(TSegmentoE);
end;

function TSegmentoEList.GetItem(Index: Integer): TSegmentoE;
begin
  Result := TSegmentoE(inherited GetItem(Index));
end;

function TSegmentoEList.GetLastItem: TSegmentoE;
begin
  Result := TSegmentoE(inherited GetItem(Count-1));
end;

procedure TSegmentoEList.SetItem(Index: Integer; Value: TSegmentoE);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoEList.SetLastItem(const Value: TSegmentoE);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoE }

constructor TSegmentoE.Create;
begin

end;

destructor TSegmentoE.Destroy;
begin

  inherited;
end;

{ TSegmentoFList }

function TSegmentoFList.Add: TSegmentoF;
begin
  Result := TSegmentoF(inherited Add);
  Result.create;
end;

constructor TSegmentoFList.Create{(AOwner: TLote)};
begin
  inherited Create(TSegmentoF);
end;

function TSegmentoFList.GetItem(Index: Integer): TSegmentoF;
begin
  Result := TSegmentoF(inherited GetItem(Index));
end;

function TSegmentoFList.GetLastItem: TSegmentoF;
begin
  Result := TSegmentoF(inherited GetItem(Count-1));
end;

procedure TSegmentoFList.SetItem(Index: Integer; Value: TSegmentoF);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoFList.SetLastItem(const Value: TSegmentoF);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoF }

constructor TSegmentoF.Create;
begin

end;

destructor TSegmentoF.Destroy;
begin

  inherited;
end;

{ TSegmentoJ52List }

function TSegmentoJ52List.Add: TSegmentoJ52;
begin
  Result := TSegmentoJ52(inherited Add);
  Result.Create;
end;

constructor TSegmentoJ52List.Create;
begin
  inherited Create(TSegmentoJ52);
end;

function TSegmentoJ52List.GetItem(Index: Integer): TSegmentoJ52;
begin
  Result := TSegmentoJ52(inherited GetItem(Index));
end;

function TSegmentoJ52List.GetLastItem: TSegmentoJ52;
begin
  Result := TSegmentoJ52(inherited GetItem(Count-1));
end;

procedure TSegmentoJ52List.SetItem(Index: Integer; Value: TSegmentoJ52);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoJ52List.SetLastItem(const Value: TSegmentoJ52);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoJ52 }

constructor TSegmentoJ52.Create;
begin
  FPagador := TPagador.Create;
  FBeneficiario := TBeneficiario.Create;
  FSacadorAvalista := TSacadorAvalista.Create;
end;

destructor TSegmentoJ52.Destroy;
begin
  FPagador.Free;
  FBeneficiario.Free;
  FSacadorAvalista.Free;

  inherited;
end;

{ TPagador }

constructor TPagador.Create;
begin
  FInscricao := TInscricao.Create;
end;

destructor TPagador.Destroy;
begin
  FInscricao.Free;

  inherited;
end;

{ TBeneficiario }

constructor TBeneficiario.Create;
begin
  FInscricao := TInscricao.Create;
end;

destructor TBeneficiario.Destroy;
begin
  FInscricao.Free;

  inherited;
end;

{ TSacadorAvalista }

constructor TSacadorAvalista.Create;
begin
  FInscricao := TInscricao.Create;
end;

destructor TSacadorAvalista.Destroy;
begin
  FInscricao.Free;

  inherited;
end;

{ TSegmentoN9 }

constructor TSegmentoN9.Create;
begin
  FSegmentoN := TSegmentoN.Create;
end;

destructor TSegmentoN9.Destroy;
begin
  FSegmentoN.Free;

  inherited;
end;

{ TSegmentoN9List }

function TSegmentoN9List.Add: TSegmentoN9;
begin
  Result := TSegmentoN9(inherited Add);
  Result.Create;
end;

constructor TSegmentoN9List.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoN9);
end;

function TSegmentoN9List.GetItem(Index: Integer): TSegmentoN9;
begin
  Result := TSegmentoN9(inherited GetItem(Index));
end;

function TSegmentoN9List.GetLastItem: TSegmentoN9;
begin
  Result := TSegmentoN9(inherited GetItem(Count-1));
end;

procedure TSegmentoN9List.SetItem(Index: Integer; Value: TSegmentoN9);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoN9List.SetLastItem(const Value: TSegmentoN9);
begin
  inherited SetItem(Count-1, Value);
end;

{ TArquivoTXT }

constructor TArquivoTXT.Create;
begin
  FArquivoTXT := TStringList.Create;
end;

destructor TArquivoTXT.Destroy;
begin
  FArquivoTXT.Free;

  inherited;
end;

{ TArquivoTXTList }

function TArquivoTXTList.Add: TArquivoTXT;
begin
  Result := TArquivoTXT(inherited Add);
  Result.create;
end;

constructor TArquivoTXTList.Create;
begin
  inherited Create(TArquivoTXT);
end;

function TArquivoTXTList.GetItem(Index: Integer): TArquivoTXT;
begin
  Result := TArquivoTXT(inherited GetItem(Index));
end;

function TArquivoTXTList.GetLastItem: TArquivoTXT;
begin
  Result := TArquivoTXT(inherited GetItem(Count-1));
end;

procedure TArquivoTXTList.SetItem(Index: Integer; Value: TArquivoTXT);
begin
  inherited SetItem(Index, Value);
end;

procedure TArquivoTXTList.SetLastItem(const Value: TArquivoTXT);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoN }

constructor TSegmentoN.Create;
begin
  FSegmentoB := TSegmentoBList.Create;
  FSegmentoW := TSegmentoWList.Create;
  FSegmentoZ := TSegmentoZList.Create;
end;

destructor TSegmentoN.Destroy;
begin
  FSegmentoB.Free;
  FSegmentoW.Free;
  FSegmentoZ.Free;

  inherited;
end;

function TSegmentoN.GetPagamentoLiberado: Boolean;
var
  I: Integer;
begin
  Result := POS(CodOcorrencia, PAGAMENTO_LIBERADO_BANCO) > 0;

  if Result then
  begin
    for I := 0 to SegmentoB.Count - 1 do
      if Result then
        Result := POS(SegmentoB.Items[I].CodOcorrencia, PAGAMENTO_LIBERADO_BANCO) > 0;
  end;
end;

procedure TSegmentoN.SetSegmentoB(const Value: TSegmentoBList);
begin
  FSegmentoB := Value;
end;

procedure TSegmentoN.SetSegmentoW(const Value: TSegmentoWList);
begin
  FSegmentoW := Value;
end;

procedure TSegmentoN.SetSegmentoZ(const Value: TSegmentoZList);
begin
  FSegmentoZ := Value;
end;

{ TAvisoList }

function TAvisoList.Add: TAviso;
begin
  Result := TAviso(inherited Add);
  Result.create;
end;

constructor TAvisoList.Create;
begin
  inherited Create(TAviso);
end;

function TAvisoList.GetItem(Index: Integer): TAviso;
begin
  Result := TAviso(inherited GetItem(Index));
end;

function TAvisoList.GetLastItem: TAviso;
begin
  Result := TAviso(inherited GetItem(Count-1));
end;

procedure TAvisoList.SetItem(Index: Integer; Value: TAviso);
begin
  inherited SetItem(Index, Value);
end;

procedure TAvisoList.SetLastItem(const Value: TAviso);
begin
  inherited SetItem(Count-1, Value);
end;

{ TAviso }

constructor TAviso.Create;
begin

end;

destructor TAviso.Destroy;
begin

  inherited;
end;

{ TSegmentoGList }

function TSegmentoGList.Add: TSegmentoG;
begin
  Result := TSegmentoG(inherited Add);
  Result.create;
end;

constructor TSegmentoGList.Create(AOwner: TLote);
begin
  inherited Create(TSegmentoG);
end;

function TSegmentoGList.GetItem(Index: Integer): TSegmentoG;
begin
  Result := TSegmentoG(inherited GetItem(Index));
end;

function TSegmentoGList.GetLastItem: TSegmentoG;
begin
  Result := TSegmentoG(inherited GetItem(Count-1));
end;

procedure TSegmentoGList.SetItem(Index: Integer; Value: TSegmentoG);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoGList.SetLastItem(const Value: TSegmentoG);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoG }

constructor TSegmentoG.Create;
begin
  FCedente := TCedente.Create;
  FDesconto1 := TAcrescimosDescontos.Create;
  FSegmentoH := TSegmentoHList.Create();
end;

destructor TSegmentoG.Destroy;
begin
  FCedente.Free;
  FDesconto1.Free;
  SegmentoH.Free;

  inherited;
end;

procedure TSegmentoG.SetSegmentoH(const Value: TSegmentoHList);
begin
  FSegmentoH := Value;
end;

{ TCedente }

constructor TCedente.Create;
begin
  inherited Create;

  FInscricao := TInscricao.Create;
end;

destructor TCedente.Destroy;
begin
  FInscricao.Free;

  inherited;
end;

{ TSegmentoHList }

function TSegmentoHList.Add: TSegmentoH;
begin
  Result := TSegmentoH(inherited Add);
  Result.create;
end;

constructor TSegmentoHList.Create;
begin
  inherited Create(TSegmentoH);
end;

function TSegmentoHList.GetItem(Index: Integer): TSegmentoH;
begin
  Result := TSegmentoH(inherited GetItem(Index));
end;

function TSegmentoHList.GetLastItem: TSegmentoH;
begin
  Result := TSegmentoH(inherited GetItem(Count-1));
end;

procedure TSegmentoHList.SetItem(Index: Integer; Value: TSegmentoH);
begin
  inherited SetItem(Index, Value);
end;

procedure TSegmentoHList.SetLastItem(const Value: TSegmentoH);
begin
  inherited SetItem(Count-1, Value);
end;

{ TSegmentoH }

constructor TSegmentoH.Create;
begin
  FAvalista := TAvalista.Create;
  FDesconto2 := TAcrescimosDescontos.Create;
  FDesconto3 := TAcrescimosDescontos.Create;
  FMulta := TAcrescimosDescontos.Create;
end;

destructor TSegmentoH.Destroy;
begin
  FAvalista.Free;
  FDesconto2.Free;
  FDesconto3.Free;
  FMulta.Free;

  inherited;
end;

end.
