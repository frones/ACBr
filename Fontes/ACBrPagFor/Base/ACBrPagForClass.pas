{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
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

unit ACBrPagForClass;

interface

uses
{$IFNDEF VER130}
  Variants,
{$ENDIF}
  Controls, SysUtils, Classes, Contnrs,
  ACBrPagForConversao;

type
  TArquivoTXT = class(TObject)
  private
    FArquivoTXT: TStringList;
  public
    constructor Create;
    destructor Destroy; override;

    property ArquivoTXT: TStringList read FArquivoTXT write FArquivoTXT;
  end;

  TArquivoTXTList = class(TObjectList)
  private
    function GetItem(Index: Integer): TArquivoTXT;
    procedure SetItem(Index: Integer; Value: TArquivoTXT);
  public
    function New: TArquivoTXT;
    function Last: TArquivoTXT;
    property Items[Index: Integer]: TArquivoTXT read GetItem write SetItem; default;
  end;

  TGeral = class(TObject)
  private
    FBanco: TBanco;
    FSubstitutaBanco: TBanco;
    FidTributo: TIndTributo;
    FAmbienteCliente: string;
  public
    property Banco: TBanco read FBanco write FBanco;
    property SubstitutaBanco: TBanco read FSubstitutaBanco write FSubstitutaBanco;
    property idTributo: TIndTributo read FidTributo write FidTributo;
    // Usado pela Caixa
    property AmbienteCliente: string read FAmbienteCliente write FAmbienteCliente;
  end;

  TInscricao = class(TObject)
  private
    FTipo: TTipoInscricao; // Tamanho 1
    FNumero: string; // Tamanho 14 ou 15
  public
    property Tipo: TTipoInscricao read FTipo write FTipo;
    property Numero: string read FNumero write FNumero;
  end;

  TAgencia = class(TObject)
  private
    FCodigo: Integer;
    FDV: string; // Tamanho 1
  public
    property Codigo: Integer read FCodigo write FCodigo;
    property DV: string read FDV write FDV;
  end;

  TConta = class(TObject)
  private
    FTipoConta: Integer;
    FNumero: Int64;
    FDV: string; // Tamanho 1
  public
    property TipoConta: Integer read FTipoConta write FTipoConta;
    property Numero: Int64 read FNumero write FNumero;
    property DV: string read FDV write FDV;
  end;

  TContaCorrente = class(TObject)
  private
    FAgencia: TAgencia;
    FConta: TConta;
    FDV: string; // Tamanho 1
  public
    constructor Create;
    destructor Destroy; override;

    property Agencia: TAgencia read FAgencia write FAgencia;
    property Conta: TConta read FConta write FConta;
    property DV: string read FDV write FDV;
  end;

  TEmpresa = class(TObject)
  private
    FInscricao: TInscricao; // Tamanho 14 ou 15
    FConvenio: string; // Tamanho 20
    FContaCorrente : TContaCorrente;
    FNome: string; // Tamanho 30
  public
    constructor Create;
    destructor Destroy; override;

    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Convenio: string read FConvenio write FConvenio;
    property ContaCorrente: TContaCorrente read FContaCorrente write FContaCorrente;
    property Nome: string read FNome write FNome;
  end;

  TArquivo = class(TObject)
  private
    FCodigo: TTipoArquivo; // Tamanho 1
    FDataGeracao: TDateTime; // DDMMAAAA
    FHoraGeracao: TDateTime; // HHMMSS
    FSequencia: Integer;
    FDensidade: Integer;
    FParamTransm: string;
  public
    property Codigo: TTipoArquivo read FCodigo write FCodigo;
    property DataGeracao: TDateTime read FDataGeracao write FDataGeracao;
    property HoraGeracao: TDateTime read FHoraGeracao write FHoraGeracao;
    property Sequencia: Integer read FSequencia write FSequencia;
    property Densidade: Integer read FDensidade write FDensidade;
    // Usado pela Caixa
    property ParamTransm: string read FParamTransm write FParamTransm;
  end;

  TTotais = class(TObject)
  private
    FQtdeLotes: Integer;
    FQtdeRegistros: Integer;
    FQtdeContasConciliadas: Integer;
  public
    property QtdeLotes: Integer read FQtdeLotes write FQtdeLotes;
    property QtdeRegistros: Integer read FQtdeRegistros write FQtdeRegistros;
    property QtdeContasConciliadas: Integer read FQtdeContasConciliadas write FQtdeContasConciliadas;
  end;

  TServico = class(TObject)
  private
    FOperacao: TTipoOperacao; // Tamanho 1
    FFormaLancamento: TFormaLancamento; // Tamanho 2
    FTipoServico: TTipoServico; // Tamanho 2
  public
    property Operacao: TTipoOperacao read FOperacao write FOperacao;
    property FormaLancamento: TFormaLancamento read FFormaLancamento write FFormaLancamento;
    property TipoServico: TTipoServico read FTipoServico write FTipoServico;
  end;

  TEndereco = class(TObject)
  private
    FLogradouro: string; // Tamanho 30
    FNumero: string;
    FComplemento: string; // Tamanho 15
    FBairro: string; // Tamanho 15
    FCidade: string; // Tamanho 20
    FCEP: Integer; // Tamanho 8
    FEstado: string; // Tamanho 2
  public
    property Logradouro: string read FLogradouro write FLogradouro;
    property Numero: string read FNumero write FNumero;
    property Complemento: string read FComplemento write FComplemento;
    property Bairro: string read FBairro write FBairro;
    property Cidade: string read FCidade write FCidade;
    property CEP: Integer read FCEP write FCEP;
    property Estado: string read FEstado write FEstado;
  end;

  TControleCobranca = class(TObject)
  private
    FNumRemRet: Integer;
    FDataGravacao: TDateTime;  // DDMMAAAA
  public
    property NumRemRet: Integer read FNumRemRet write FNumRemRet;
    property DataGravacao: TDateTime read FDataGravacao write FDataGravacao;
  end;

  TTitulos = class(TObject)
  private
    FQtdeTitulosCobranca: Integer;
    FValorTitulosCarteira: Double;
  public
    property QtdeTitulosCobranca: Integer read FQtdeTitulosCobranca write FQtdeTitulosCobranca;
    property ValorTitulosCarteira: Double read FValorTitulosCarteira write FValorTitulosCarteira;
  end;

  {
  TCobranca = class(TObject)
  private
    FCarteira: Integer; // Tamanho 1
    FCadastramento: Integer; // Tamanho 1
    FTipoDocumento: string; // Tamanho 1
    FEmissaoBoleto: Integer; // Tamanho 1
    FDistribuicaoBoleto: string; // Tamanho 1
  public
    property Carteira: Integer read FCarteira write FCarteira;
    property Cadastramento: Integer read FCadastramento write FCadastramento;
    property TipoDocumento: string read FTipoDocumento write FTipoDocumento;
    property EmissaoBoleto: Integer read FEmissaoBoleto write FEmissaoBoleto;
    property DistribuicaoBoleto: string read FDistribuicaoBoleto write FDistribuicaoBoleto;
  end;
  }

  TAcrescimosDescontos = class(TObject)
  private
    FCodigo: Integer; // Tamanho 1
    FData: TDateTime; // DDMMAAAA
    FValor: Double;
  public
    property Codigo: Integer read FCodigo write FCodigo;
    property Data: TDateTime read FData write FData;
    property Valor: Double read FValor write FValor;
  end;

  TSacado = class(TObject)
  private
    FInscricao: TInscricao; // Tamanho 15
    FNome: string; // Tamanho 40
    FEndereco: string; // Tamanho 40
    FBairro: string; // Tamanho 15
    FCEP: Integer; // Tamanho 8
    FCidade: string; // Tamanho 15
    FUF: string; // Tamanho 2
  public
    constructor Create;
    destructor Destroy; override;

    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Nome: string read FNome write FNome;
    property Endereco: string read FEndereco write FEndereco;
    property Bairro: string read FBairro write FBairro;
    property CEP: Integer read FCEP write FCEP;
    property Cidade: string read FCidade write FCidade;
    property UF: string read FUF write FUF;
  end;

  TAvalista = class(TObject)
  private
    FInscricao: TInscricao; // Tamanho 15
    FNome: string; // Tamanho 40
  public
    constructor Create;
    destructor Destroy; override;

    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Nome: string read FNome write FNome;
  end;

  TCedente = class(TObject)
  private
    FInscricao: TInscricao; // Tamanho 15
    FNome: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Nome: string read FNome write FNome;
  end;

  TDebitoAutomatico = class(TObject)
  private
    FBanco: TBanco;
    FContaCorrente : TContaCorrente;
  public
    constructor Create;
    destructor Destroy; override;

    property Banco: TBanco read FBanco write FBanco;
    property ContaCorrente: TContaCorrente read FContaCorrente write FContaCorrente;
  end;

  TFavorecido = class(TObject)
  private
    FCamara: Integer;
    FBanco: TBanco;
    FContaCorrente: TContaCorrente;
    FNome: string; // Tamanho 30
    FInscricao: TInscricao;
    FIDTipoTransferencia: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Camara: Integer read FCamara write FCamara;
    property Banco: TBanco read FBanco write FBanco;
    property ContaCorrente: TContaCorrente read FContaCorrente write FContaCorrente;
    property Nome: string read FNome write FNome;
    property Inscricao: TInscricao read FInscricao write FInscricao;
    property IDTipoTransfencia: string read FIDTipoTransferencia write FIDTipoTransferencia;
  end;

  TMoeda = class(TObject)
  private
    FTipo : TTipoMoeda; // Tamanho 3
    FQtde : Double;
  public
    property Tipo: TTipoMoeda read FTipo write FTipo;
    property Qtde: Double read FQtde write FQtde;
  end;

  TCredito = class(TObject)
  private
    FSeuNumero: string; // Tamanho 20
    FDataPagamento: TDateTime; // DDMMAAAA
    FMoeda: TMoeda;
    FValorPagamento: Double;
    FNossoNumero: string; // Tamanho 20
    FDataReal: TDateTime; // DDMMAAAA
    FValorReal: Double;
    FQtdeParcelas: Integer;
    FIndBloqueio: string;
    FFormaParcelamento: Integer;
    FDiaVencimento: Integer;
    FNumParcela: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property SeuNumero: string read FSeuNumero write FSeuNumero;
    property DataPagamento: TDateTime read FDataPagamento write FDataPagamento;
    property Moeda: TMoeda read FMoeda write FMoeda;
    property ValorPagamento: Double read FValorPagamento write FValorPagamento;
    property NossoNumero: string read FNossoNumero write FNossoNumero;
    property DataReal: TDateTime read FDataReal write FDataReal;
    property ValorReal: Double read FValorReal write FValorReal;
    // Usado pela Caixa
    property QtdeParcelas: Integer read FQtdeParcelas write FQtdeParcelas;
    property IndBloqueio: string read FIndBloqueio write FIndBloqueio;
    property FormaParcelamento: Integer read FFormaParcelamento write FFormaParcelamento;
    property DiaVencimento: Integer read FDiaVencimento write FDiaVencimento;
    property NumParcela: Integer read FNumParcela write FNumParcela;
  end;

  TAviso = class(TObject)
  private
    FCodigoRetorno: string;
    FMensagemRetorno: string;
    FSegmento: string;
    FSegmentoFilho: string;
    FSeuNumero: string;
  public
    property CodigoRetorno: string read FCodigoRetorno write FCodigoRetorno;
    property MensagemRetorno: string read FMensagemRetorno write FMensagemRetorno;
    property Segmento: string read FSegmento write FSegmento;
    property SegmentoFilho: string read FSegmentoFilho write FSegmentoFilho;
    property SeuNumero: string read FSeuNumero write FSeuNumero;
  end;

  TAvisoList = class(TObjectList)
  private
    function GetItem(Index: Integer): TAviso;
    procedure SetItem(Index: Integer; Value: TAviso);
  public
    function New: TAviso;
    function Last: TAviso;
    property Items[Index: Integer]: TAviso read GetItem write SetItem; default;
  end;

  // Estrutura do Registro 0 utilizado em todos os Arquivos
  // Primeiro e unico no Arquivo

  TRegistro0 = class(TObject)
  private
    FEmpresa: TEmpresa; // Tamanho 14
    FNomeBanco: string; // Tamanho 30
    FArquivo: TArquivo;
    FReservadoBanco: string; // Tamanho 20
    FReservadoEmpresa: string; // Tamanho 20
    FAviso: TAvisoList;
    FRemessaTeste: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property Empresa: TEmpresa read FEmpresa write FEmpresa;
    property NomeBanco: string read FNomeBanco write FNomeBanco;
    property Arquivo: TArquivo read FArquivo write FArquivo;
    property ReservadoBanco: string read FReservadoBanco write FReservadoBanco;
    property ReservadoEmpresa: string read FReservadoEmpresa write FReservadoEmpresa;
    property Aviso: TAvisoList read FAviso write FAviso;
    property RemessaTeste: Boolean read FRemessaTeste write FRemessaTeste;
  end;

  // Estrutura do Registro 9 utilizado em todos os Arquivos
  // Ultimo e unico no Arquivo

  TRegistro9 = class(TObject)
  private
    FTotais: TTotais;
  public
    constructor Create;
    destructor Destroy; override;

    property Totais: TTotais read FTotais write FTotais;
  end;

  // Estrutura do Registro 1 utilizado para os Serviços
  // uma para cada lote de serviços - Identifica o Inicio do Lote

  TRegistro1 = class(TObject)
  private
    FServico: TServico;
    FEmpresa: TEmpresa; // Tamanho 15
    FInformacao1: string; // Tamanho 40
    FEndereco: TEndereco;
    FIndFormaPag: TIndFormaPag;
    FInformacao2: string; // Tamanho 40
    FControleCobranca: TControleCobranca;
    FDataCredito: TDateTime; // DDMMAAAA
    // Conciliacao Bancaria
    FData: TDateTime;
    FValor: Double;
    FSituacao: string;
    FStatus: string;
    FTipoMoeda: string;
    FSequencia: Integer;
    FTipoCompromisso: Integer;
    FCodigoCompromisso: Integer;
    FCodOcorrencia: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Servico: TServico read FServico write FServico;
    property Empresa: TEmpresa read FEmpresa write FEmpresa;
    property Informacao1: string read FInformacao1 write FInformacao1;
    property Endereco: TEndereco read FEndereco write FEndereco;
    property IndFormaPag: TIndFormaPag read FIndFormaPag write FIndFormaPag;
    property Informacao2: string read FInformacao2 write FInformacao2;
    property ControleCobranca: TControleCobranca read FControleCobranca write FControleCobranca;
    property DataCredito: TDateTime read FDataCredito write FDataCredito;
    // Conciliacao Bancaria
    property Data: TDateTime read FData write FData;
    property Valor: Double read FValor write FValor;
    property Situacao: string read FSituacao write FSituacao;
    property Status: string read FStatus write FStatus;
    property TipoMoeda: string read FTipoMoeda write FTipoMoeda;
    property Sequencia: Integer read FSequencia write FSequencia;
    // Usado pela Caixa
    property TipoCompromisso: Integer read FTipoCompromisso write FTipoCompromisso;
    property CodigoCompromisso: Integer read FCodigoCompromisso write FCodigoCompromisso;
    // Usado pelo Bradesco
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
  end;

  // Estrutura do Registro 5 utilizado para os Serviços
  // uma para cada lote de serviços - Identifica o Fim do Lote

  TRegistro5 = class(TObject)
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
    // Conciliacao Bancaria
    FBloqueadoAcima24h: Double;
    FLimite: Double;
    FBloqueadoAte24h: Double;
    FData: TDateTime;
    FSituacao: string;
    FStatus: string;
    FQtdeRegistros: Integer;
    FValorDebitos: Double;
    FValorCreditos: Double;
    FCodOcorrencia: string;
  public
    constructor Create;
    destructor Destroy; override;

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
    // Conciliacao Bancaria
    property BloqueadoAcima24h: Double read FBloqueadoAcima24h write FBloqueadoAcima24h;
    property Limite: Double read FLimite write FLimite;
    property BloqueadoAte24h: Double read FBloqueadoAte24h write FBloqueadoAte24h;
    property Data: TDateTime read FData write FData;
    property Situacao: string read FSituacao write FSituacao;
    property Status: string read FStatus write FStatus;
    property QtdeRegistros: Integer read FQtdeRegistros write FQtdeRegistros;
    property ValorDebitos: Double read FValorDebitos write FValorDebitos;
    property ValorCreditos: Double read FValorCreditos write FValorCreditos;
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
  end;

   // Estrutura do Segmento B (Opcional)

  TSegmentoB = class(TObject)
  private
    FInscricao: TInscricao; // Tamanho 14
    FEndereco: TEndereco;
    FInformacao10: string;
    FInformacao11: string;
    FInformacao12: string;

    FDataVencimento: TDateTime; // DDMMAAAA
    FValor: Double;
    FAbatimento: Double;
    FDesconto: Double;
    FMora: Double;
    FMulta: Double;
    FCodigoDoc: string; // Tamanho 15
    FAviso: Integer; // Tamanho 1
    FCodigoUG: Integer; // Tamanho 6
    FEmail: string;
    FTelefone: string;
    FHonorario: Double;
    FAcrescimo: Double;
    FCodOcorrencia: string;
    FCodigoISPB: Integer;
    FPixTipoChave: TTipoChavePIX;
    FPixMensagem: string;
    FPixTXID: string;
    FPixChave: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Informacao10: string read FInformacao10 write FInformacao10;
    property Informacao11: string read FInformacao11 write FInformacao11;
    property Informacao12: string read FInformacao12 write FInformacao12;

    property Endereco: TEndereco read FEndereco write FEndereco;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
    property Valor: Double read FValor write FValor;
    property Abatimento: Double read FAbatimento write FAbatimento;
    property Desconto: Double read FDesconto write FDesconto;
    property Mora: Double read FMora write FMora;
    property Multa: Double read FMulta write FMulta;
    property CodigoDoc: string read FCodigoDoc write FCodigoDoc;
    property Aviso: Integer read FAviso write FAviso;
    property CodigoUG: Integer read FCodigoUG write FCodigoUG;
    property Email: string read FEmail write FEmail;
    property Telefone: string read FTelefone write FTelefone;
    property Honorario: Double read FHonorario write FHonorario;
    property Acrescimo: Double read FAcrescimo write FAcrescimo;
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
    property CodigoISPB: Integer read FCodigoISPB write FCodigoISPB;
    property PixTipoChave: TTipoChavePix read FPixTipoChave write FPixTipoChave;
    property PixMensagem: string read FPixMensagem write FPixMensagem;
    property PixTXID: string read FPixTXID write FPixTXID;
    property PixChave: string read FPixChave write FPixChave;
  end;

  TSegmentoBList = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoB;
    procedure SetItem(Index: Integer; Value: TSegmentoB);
  public
    function New: TSegmentoB;
    function First: TSegmentoB;
    function Last: TSegmentoB;
    property Items[Index: Integer]: TSegmentoB read GetItem write SetItem; default;
  end;

  // Estrutura do Segmento C (Opcional)

  TSubstituta = class(TObject)
  private
    FContaCorrente: TContaCorrente;
  public
    constructor Create;
    destructor Destroy; override;

    property ContaCorrente: TContaCorrente read FContaCorrente write FContaCorrente;
  end;

  TSegmentoC = class(TObject)
  private
    FValorCSLL: Double;
    FValorIR: Double;
    FValorISS: Double;
    FValorIOF: Double;
    FDeducoes: Double;
    FAcrescimos: Double;
    FSubstituta: TSubstituta;
    FValorINSS: Double;
    FNumContaPagCreditada: string;

    FVencimento: TDateTime;
    FValorDocumento: Double;
    FValorPIS: Double;
    FValorCOFINS: Double;
    FDescontos: Double;
    FMora: Double;
    FMulta: Double;
    FNumeroFaturaDocumento: string;
    FAbatimentos: Double;
    FCodOcorrencia: string;
  public
    constructor Create;
    destructor Destroy; override;

    property ValorCSLL: Double read FValorCSLL write FValorCSLL;
    property ValorIR: Double read FValorIR write FValorIR;
    property ValorISS: Double read FValorISS write FValorISS;
    property ValorIOF: Double read FValorIOF write FValorIOF;
    property Deducoes: Double read FDeducoes write FDeducoes;
    property Acrescimos: Double read FAcrescimos write FAcrescimos;
    property Substituta: TSubstituta read FSubstituta write FSubstituta;
    property ValorINSS: Double read FValorINSS write FValorINSS;
    property NumContaPagCreditada: string read FNumContaPagCreditada write FNumContaPagCreditada;

    property Vencimento: TDateTime read FVencimento write FVencimento;
    property ValorDocumento: Double read FValorDocumento write FValorDocumento;
    property ValorPIS: Double read FValorPIS write FValorPIS;
    property ValorCOFINS: Double read FValorCOFINS write FValorCOFINS;
    property Descontos: Double read FDescontos write FDescontos;
    property Mora: Double read FMora write FMora;
    property Multa: Double read FMulta write FMulta;
    property NumeroFaturaDocumento: string read FNumeroFaturaDocumento write FNumeroFaturaDocumento;
    property Abatimentos: Double read FAbatimentos write FAbatimentos;
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
  end;

  TSegmentoCList = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoC;
    procedure SetItem(Index: Integer; Value: TSegmentoC);
  public
    function New: TSegmentoC;
    function First: TSegmentoC;
    function Last: TSegmentoC;
    property Items[Index: Integer]: TSegmentoC read GetItem write SetItem; default;
  end;

  // Estrutura do Segmento E (Obrigatório - Retorno)
  // Extrato de Conta Corrente para Conciliação Bancária

  TSegmentoE = class(TObject)
  private
    FConvenio: string;
    FContaCorrente: TContaCorrente;
    FNome: string;
    FNaturezaLanc: TNaturezaLanc;
    FTipoComplemento: Integer;
    FComplemento: string;
    FCPMF: string;
    FDataContabil: TDateTime;
    FDataLancamento: TDateTime;
    FValor: Double;
    FTipoLancamento: string;
    FCategoria: Integer;
    FCodigoHistorico: string;
    FHistorico: string;
    FNumeroDocumento: string;
  public
    property Convenio: string read FConvenio write FConvenio;
    property ContaCorrente: TContaCorrente read FContaCorrente write FContaCorrente;
    property Nome: string read FNome write FNome;
    property NaturezaLanc: TNaturezaLanc read FNaturezaLanc write FNaturezaLanc;
    property TipoComplemento: Integer read FTipoComplemento write FTipoComplemento;
    property Complemento: string read FComplemento write FComplemento;
    property CPMF: string read FCPMF write FCPMF;
    property DataContabil: TDateTime read FDataContabil write FDataContabil;
    property DataLancamento: TDateTime read FDataLancamento write FDataLancamento;
    property Valor: Double read FValor write FValor;
    property TipoLancamento: string read FTipoLancamento write FTipoLancamento;
    property Categoria: Integer read FCategoria write FCategoria;
    property CodigoHistorico: string read FCodigoHistorico write FCodigoHistorico;
    property Historico: string read FHistorico write FHistorico;
    property NumeroDocumento: string read FNumeroDocumento write FNumeroDocumento;
  end;

  TSegmentoEList = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoE;
    procedure SetItem(Index: Integer; Value: TSegmentoE);
  public
    function New: TSegmentoE;
    function Last: TSegmentoE;
    property Items[Index: Integer]: TSegmentoE read GetItem write SetItem; default;
  end;

  // Estrutura do Segmento F (Obrigatório - Retorno)
  // Extrato para Gestão de Caixa

  TSegmentoF = class(TObject)
  private
    FInformacaoComplementar: string;
//    FCodOcorrencia: string;
  public
    property InformacaoComplementar: string read FInformacaoComplementar write FInformacaoComplementar;
//    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
  end;

  TSegmentoFList = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoF;
    procedure SetItem(Index: Integer; Value: TSegmentoF);
  public
    function New: TSegmentoF;
    function Last: TSegmentoF;
    property Items[Index: Integer]: TSegmentoF read GetItem write SetItem; default;
  end;

  // Estrutura do Segmento Z (Opcional)

  TSegmentoZ = class(TObject)
  private
    FAutenticacao : string; // Tamanho 64
    FSeuNumero: string;
    FNossoNumero: string;
    FCodOcorrencia: string;
  public
    property Autenticacao: string read FAutenticacao write FAutenticacao;
    property SeuNumero: string read FSeuNumero write FSeuNumero;
    property NossoNumero: string read FNossoNumero write FNossoNumero;
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
  end;

  TSegmentoZList = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoZ;
    procedure SetItem(Index: Integer; Value: TSegmentoZ);
  public
    function New: TSegmentoZ;
    function Last: TSegmentoZ;
    property Items[Index: Integer]: TSegmentoZ read GetItem write SetItem; default;
  end;


  // Lote: Pagamento Através de Crédito em Conta, Cheque, OP, DOC, TED ou
  //       Pagamento com Autenticação
  // Serviço: 20 = Pagamento

  // Lote: Débito em Conta Corrente
  // Serviço: xx = Débito em Conta Corrente

  // Segmentos Obrigatórios de Remessa: A
  // Segmentos Opcionais de Remessa: B, C

  // Estrutura do Segmento A (Obrigatorio)

  TSegmentoA = class(TObject)
  private
    FTipoMovimento: TTipoMovimento; // Tamanho 1
    FCodMovimento: TInstrucaoMovimento; // Tamanho 2
    FFavorecido: TFavorecido;
    FCredito: TCredito;
    FInformacao2: string; // Tamanho 40
    FCodigoDOC: string; // Tamanho 2
    FCodigoTED: string; // Tamanho 5
    FCodigoComp: string; // Tamanho 2
    FAviso: Integer; // Tamanho 1
    FCodOcorrencia : string; // Tamanho 2
    FNumeroDocumento: Integer;
    FCodigoISPB: Integer;

    FSegmentoB: TSegmentoBList;
    FSegmentoC: TSegmentoCList;
//    FSegmentoE: TSegmentoEList;
//    FSegmentoF: TSegmentoFList;
    FSegmentoZ: TSegmentoZList;

    procedure SetSegmentoB(const Value: TSegmentoBList);
    procedure SetSegmentoC(const Value: TSegmentoCList);
//    procedure SetSegmentoE(const Value: TSegmentoEList);
//    procedure SetSegmentoF(const Value: TSegmentoFList);
    procedure SetSegmentoZ(const Value: TSegmentoZList);
    function GetPagamentoLiberado: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    property TipoMovimento: TTipoMovimento read FTipoMovimento write FTipoMovimento;
    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property Favorecido: TFavorecido read FFavorecido write FFavorecido;
    property Credito: TCredito read FCredito write FCredito;
    property Informacao2: string read FInformacao2 write FInformacao2;
    property CodigoDOC: string read FCodigoDOC write FCodigoDOC;
    property CodigoTED: string read FCodigoTED write FCodigoTED;
    property CodigoComp: string read FCodigoComp write FCodigoComp;
    property Aviso: Integer read FAviso write FAviso;
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
    property PagamentoLiberado: Boolean read GetPagamentoLiberado;
    property NumeroDocumento: Integer read FNumeroDocumento write FNumeroDocumento;
    property CodigoISPB: Integer read FCodigoISPB write FCodigoISPB;
    property SegmentoB: TSegmentoBList read FSegmentoB write SetSegmentoB;
    property SegmentoC: TSegmentoCList read FSegmentoC write SetSegmentoC;
//    property SegmentoE: TSegmentoEList read FSegmentoE write SetSegmentoE;
//    property SegmentoF: TSegmentoFList read FSegmentoF write SetSegmentoF;
    property SegmentoZ: TSegmentoZList read FSegmentoZ write SetSegmentoZ;
  end;

  TSegmentoAList = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoA;
    procedure SetItem(Index: Integer; Value: TSegmentoA);
  public
    function New: TSegmentoA;
    function First: TSegmentoA;
    function Last: TSegmentoA;
    property Items[Index: Integer]: TSegmentoA read GetItem write SetItem; default;
  end;

  TSegmentoH = class(TObject)
  private
    FAvalista: TAvalista;
    FDesconto2: TAcrescimosDescontos;
    FDesconto3: TAcrescimosDescontos;
    FMulta: TAcrescimosDescontos;
    FAbatimento: Double;
    FInformacao1: string;
    FInformacao2: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Avalista: TAvalista read FAvalista write FAvalista;
    property Desconto2: TAcrescimosDescontos read FDesconto2 write FDesconto2;
    property Desconto3: TAcrescimosDescontos read FDesconto3 write FDesconto3;
    property Multa: TAcrescimosDescontos read FMulta write FMulta;
    property Abatimento: Double read FAbatimento write FAbatimento;
    property Informacao1: string read FInformacao1 write FInformacao1;
    property Informacao2: string read FInformacao2 write FInformacao2;
  end;

  TSegmentoHList = class(TObjectList)
    private
    function GetItem(Index: Integer): TSegmentoH;
    procedure SetItem(Index: Integer; Value: TSegmentoH);
  public
    function New: TSegmentoH;
    function Last: TSegmentoH;
    property Items[Index: Integer]: TSegmentoH read GetItem write SetItem; default;
  end;

  TSegmentoG = class(TObject)
  private
    FCodigoBarras: string;
    FCedente: TCedente;
    FVencimento: TDateTime;
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
    FDataLimite: TDateTime;

    FSegmentoH: TSegmentoHList;

    procedure SetSegmentoH(const Value: TSegmentoHList);
  public
    constructor Create;
    destructor Destroy; override;

    property CodigoBarras: string read FCodigoBarras write FCodigoBarras;
    property Cedente: TCedente read FCedente write FCedente;
    property Vencimento: TDateTime read FVencimento write FVencimento;
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
    property DataLimite: TDateTime read FDataLimite write FDataLimite;
    property SegmentoH: TSegmentoHList read FSegmentoH write SetSegmentoH;
  end;

  TSegmentoGList = class(TObjectList)
    private
    function GetItem(Index: Integer): TSegmentoG;
    procedure SetItem(Index: Integer; Value: TSegmentoG);
  public
    function New: TSegmentoG;
    function Last: TSegmentoG;
    property Items[Index: Integer]: TSegmentoG read GetItem write SetItem; default;
  end;

  TPagador = class(TObject)
  private
    FInscricao: TInscricao;
    FNome: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Nome: string read FNome write FNome;
  end;

  TBeneficiario = class(TObject)
  private
    FInscricao: TInscricao;
    FNome: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Nome: string read FNome write FNome;
  end;

  TSacadorAvalista = class(TObject)
  private
    FInscricao: TInscricao;
    FNome: string;
  public
    constructor Create;
    destructor Destroy; override;

    property Inscricao: TInscricao read FInscricao write FInscricao;
    property Nome: string read FNome write FNome;
  end;

  TSegmentoJ52 = class(TObject)
  private
    FTipoMovimento: TTipoMovimento;
    FCodMovimento: TInstrucaoMovimento;
    FPagador: TPagador;
    FBeneficiario: TBeneficiario;
    FSacadorAvalista: TSacadorAvalista;
    FChave: string;
    FTXID: string;
  public
    constructor Create;
    destructor Destroy; override;

    property TipoMovimento: TTipoMovimento read FTipoMovimento write FTipoMovimento;
    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property Pagador: TPagador read FPagador write FPagador;
    property Beneficiario: TBeneficiario read FBeneficiario write FBeneficiario;
    property SacadorAvalista: TSacadorAvalista read FSacadorAvalista write FSacadorAvalista;
    property Chave: string read FChave write FChave;
    property TXID: string read FTXID write FTXID;
  end;

  TSegmentoJ52List = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoJ52;
    procedure SetItem(Index: Integer; Value: TSegmentoJ52);
  public
    function New: TSegmentoJ52;
    function Last: TSegmentoJ52;
    property Items[Index: Integer]: TSegmentoJ52 read GetItem write SetItem; default;
  end;

  TSegmentoJ99 = class(TObject)
  private
    FCodMovimento: TInstrucaoMovimento;
    FCodAutenticacao: Integer;
    FNunDocumento: string;
    FDataHoraPagamento: TDateTime;
    FProtocoloPagamento: string;
  public
    constructor Create;
    destructor Destroy; override;

    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property CodAutenticacao: Integer read FCodAutenticacao write FCodAutenticacao;
    property NunDocumento: string read FNunDocumento write FNunDocumento;
    property DataHoraPagamento: TDateTime read FDataHoraPagamento write FDataHoraPagamento;
    property ProtocoloPagamento: string read FProtocoloPagamento write FProtocoloPagamento;
  end;

  TSegmentoJ99List = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoJ99;
    procedure SetItem(Index: Integer; Value: TSegmentoJ99);
  public
    function New: TSegmentoJ99;
    function Last: TSegmentoJ99;
    property Items[Index: Integer]: TSegmentoJ99 read GetItem write SetItem; default;
  end;

  // Lote: Pagamento de Titulos de Cobrança
  // Serviço: 20 = Pagamentos

  // Segmentos Obrigatórios de Remessa: J
  // Segmentos Opcionais de Remessa:

  // Estrutura do Segmento J (Obrigatorio)

  TSegmentoJ = class(TObject)
  private
    FTipoMovimento: TTipoMovimento;
    FCodMovimento: TInstrucaoMovimento;
    FCodigoBarras: string;
    FNomeCedente: string;
    FDataVencimento: TDateTime;
    FValorTitulo: Double;
    FDesconto: Double;
    FAcrescimo: Double;
    FDataPagamento: TDateTime;
    FValorPagamento: Double;
    FQtdeMoeda: Double;
    FReferenciaSacado: string;
    FCodigoMoeda: Integer;
    FCodOcorrencia: string;
    FSegmentoJ52: TSegmentoJ52List;
    FSegmentoJ99: TSegmentoJ99List;
//    FSegmentoB: TSegmentoBList;
//    FSegmentoC: TSegmentoCList;
    FSegmentoZ: TSegmentoZList;
    FNossoNumero: string;
    FSeuNumero: string;

    procedure SetSegmentoJ52(const Value: TSegmentoJ52List);
    procedure SetSegmentoJ99(const Value: TSegmentoJ99List);
//    procedure SetSegmentoB(const Value: TSegmentoBList);
//    procedure SetSegmentoC(const Value: TSegmentoCList);
    procedure SetSegmentoZ(const Value: TSegmentoZList);
    function GetPagamentoLiberado: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property TipoMovimento: TTipoMovimento read FTipoMovimento write FTipoMovimento;
    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property CodigoBarras: string read FCodigoBarras write FCodigoBarras;
    property NomeCedente: string read FNomeCedente write FNomeCedente;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
    property ValorTitulo: Double read FValorTitulo write FValorTitulo;
    property Desconto: Double read FDesconto write FDesconto;
    property Acrescimo: Double read FAcrescimo write FAcrescimo;
    property DataPagamento: TDateTime read FDataPagamento write FDataPagamento;
    property ValorPagamento: Double read FValorPagamento write FValorPagamento;
    property QtdeMoeda: Double read FQtdeMoeda write FQtdeMoeda;
    property ReferenciaSacado: string read FReferenciaSacado write FReferenciaSacado;
    property CodigoMoeda: Integer read FCodigoMoeda write FCodigoMoeda;
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
    property NossoNumero: string read FNossoNumero write FNossoNumero;
    property PagamentoLiberado: Boolean read GetPagamentoLiberado;
    property SegmentoJ52: TSegmentoJ52List read FSegmentoJ52 write SetSegmentoJ52;
    property SegmentoJ99: TSegmentoJ99List read FSegmentoJ99 write SetSegmentoJ99;
//    property SegmentoB: TSegmentoBList read FSegmentoB write SetSegmentoB;
//    property SegmentoC: TSegmentoCList read FSegmentoC write SetSegmentoC;
    property SegmentoZ: TSegmentoZList read FSegmentoZ write SetSegmentoZ;
    // Usado pela Caixa
    property SeuNumero: string read FSeuNumero write FSeuNumero;
  end;

  TSegmentoJList = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoJ;
    procedure SetItem(Index: Integer; Value: TSegmentoJ);
  public
    function New: TSegmentoJ;
    function First: TSegmentoJ;
    function Last: TSegmentoJ;
    property Items[Index: Integer]: TSegmentoJ read GetItem write SetItem; default;
  end;

  // Estrutura do Segmento W* (Opcional)
  // * = obrigatório para FGTS convenio 0181 e 0182

  TSegmentoW = class(TObject)
  private
    FComplementoRegistro: Integer;
    FInformacoes1ou2    : string; // Tamanho 1
    FInformacoes1       : string; // Tamanho 80
    FInformacoes2       : string; // Tamanho 80
    FInformacoes3       : string;
    FInformacoes4       : string;
    FPagFGTS            : Boolean;
    FCodReceita         : string; // Tamanho 6
    FTipoIdContribuinte : string; // Tamanho 2
    FIdContribuinte     : string; // Tamanho 14
    FIdentificador      : string; // Tamanho 16
    FLacreConecSocial   : string; // Tamanho 9
    FLacreDV            : string; // Tamanho 2
    FCodOcorrencia: string;
  public
    property ComplementoRegistro: Integer read FComplementoRegistro write FComplementoRegistro;
    property Informacoes1ou2: string read FInformacoes1ou2 write FInformacoes1ou2;
    property Informacoes1: string read FInformacoes1 write FInformacoes1;
    property Informacoes2: string read FInformacoes2 write FInformacoes2;
    property Informacoes3: string read FInformacoes3 write FInformacoes3;
    property Informacoes4: string read FInformacoes4 write FInformacoes4;
    property PagFGTS: Boolean read FPagFGTS write FPagFGTS;
    property CodReceita: string read FCodReceita write FCodReceita;
    property TipoIdContribuinte: string read FTipoIdContribuinte write FTipoIdContribuinte;
    property IdContribuinte: string read FIdContribuinte write FIdContribuinte;
    property Identificador: string read FIdentificador write FIdentificador;
    property LacreConecSocial: string read FLacreConecSocial write FLacreConecSocial;
    property LacreDV: string read FLacreDV write FLacreDV;
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
  end;

  TSegmentoWList = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoW;
    procedure SetItem(Index: Integer; Value: TSegmentoW);
  public
    function New: TSegmentoW;
    function First: TSegmentoW;
    function Last: TSegmentoW;
    property Items[Index: Integer]: TSegmentoW read GetItem write SetItem; default;
  end;

  // Lote: Pagamento de Tributos sem código de barras
  // Serviço: 22 = Pagamento de Contas, Tributos e Impostos

  // Segmentos Obrigatórios de Remessa: N
  // Segmentos Opcionais de Remessa: B, W e Z

  // Estrutura do Segmento N (Obrigatorio)

  TSegmentoN = class(TObject)
  private
    FTipoMovimento: TTipoMovimento;
    FCodMovimento: TInstrucaoMovimento; // Tamanho 2
    FSeuNumero: string; // Tamanho 20
    FNossoNumero: string; // Tamanho 20
    FNomeContribuinte: string; // Tamanho 30
    FDataPagamento: TDateTime; // DDMMAAAA
    FValorPagamento: Double;
    FSegmentoB: TSegmentoBList;
    FSegmentoW: TSegmentoWList;
    FSegmentoZ: TSegmentoZList;
    FCodOcorrencia: string;

    procedure SetSegmentoB(const Value: TSegmentoBList);
    procedure SetSegmentoW(const Value: TSegmentoWList);
    procedure SetSegmentoZ(const Value: TSegmentoZList);
    function GetPagamentoLiberado: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
  public
    property TipoMovimento: TTipoMovimento read FTipoMovimento write FTipoMovimento;
    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property SeuNumero: string read FSeuNumero write FSeuNumero;
    property NossoNumero: string read FNossoNumero write FNossoNumero;
    property NomeContribuinte: string read FNomeContribuinte write FNomeContribuinte;
    property DataPagamento: TDateTime read FDataPagamento write FDataPagamento;
    property ValorPagamento: Double read FValorPagamento write FValorPagamento;
    property SegmentoB: TSegmentoBList read FSegmentoB write SetSegmentoB;
    property SegmentoW: TSegmentoWList read FSegmentoW write SetSegmentoW;
    property SegmentoZ: TSegmentoZList read FSegmentoZ write SetSegmentoZ;
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
    property PagamentoLiberado: Boolean read GetPagamentoLiberado;
  end;

  // Segmento N1 - GPS

  TSegmentoN1 = class(TObject)
  private
    FSegmentoN           : TSegmentoN;
    FReceita             : Integer;
    FTipoContribuinte    : TTipoInscricao;
    FidContribuinte      : string; // Tamanho 14
    FCompetencia         : Integer;
    FValorTributo        : Double;
    FValorOutrasEntidades: Double;
    FAtualizacaoMonetaria: Double;
    FCodigoPagamento     : TCodigoPagamentoGps;
    FMesAnoCompetencia   : Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property SegmentoN: TSegmentoN read FSegmentoN write FSegmentoN;
    property Receita: Integer read FReceita write FReceita;
    property TipoContribuinte: TTipoInscricao read FTipoContribuinte write FTipoContribuinte;
    property idContribuinte: string read FidContribuinte write FidContribuinte;
    property Competencia: Integer read FCompetencia write FCompetencia;
    property ValorTributo: Double read FValorTributo write FValorTributo;
    property ValorOutrasEntidades: Double read FValorOutrasEntidades write FValorOutrasEntidades;
    property AtualizacaoMonetaria: Double read FAtualizacaoMonetaria write FAtualizacaoMonetaria;
    property CodigoPagamento: TCodigoPagamentoGps read FCodigoPagamento write FCodigoPagamento;
    property MesAnoCompetencia: Integer read FMesAnoCompetencia write FMesAnoCompetencia;
  end;

  TSegmentoN1List = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoN1;
    procedure SetItem(Index: Integer; Value: TSegmentoN1);
  public
    function New: TSegmentoN1;
    function First: TSegmentoN1;
    function Last: TSegmentoN1;
    property Items[Index: Integer]: TSegmentoN1 read GetItem write SetItem; default;
  end;

  // Segmento N2 - DARF Normal

  TSegmentoN2 = class(TObject)
  private
    FSegmentoN       : TSegmentoN;
    FReceita         : Integer;
    FTipoContribuinte: TTipoInscricao;
    FidContribuinte  : string; // Tamanho 14
    FPeriodo         : TDateTime;   // DDMMAAAA
    FReferencia      : string; // Tamanho 17
    FValorPrincipal  : Double;
    FMulta           : Double;
    FJuros           : Double;
    FDataVencimento  : TDateTime; // DDMMAAAA
  public
    constructor Create;
    destructor Destroy; override;

    property SegmentoN: TSegmentoN read FSegmentoN write FSegmentoN;
    property Receita: Integer read FReceita write FReceita;
    property TipoContribuinte: TTipoInscricao read FTipoContribuinte write FTipoContribuinte;
    property idContribuinte: string read FidContribuinte write FidContribuinte;
    property Periodo: TDateTime read FPeriodo write FPeriodo;
    property Referencia: string read FReferencia write FReferencia;
    property ValorPrincipal: Double read FValorPrincipal write FValorPrincipal;
    property Multa: Double read FMulta write FMulta;
    property Juros: Double read FJuros write FJuros;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
  end;

  TSegmentoN2List = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoN2;
    procedure SetItem(Index: Integer; Value: TSegmentoN2);
  public
    function New: TSegmentoN2;
    function Last: TSegmentoN2;
    property Items[Index: Integer]: TSegmentoN2 read GetItem write SetItem; default;
  end;

  // Segmento N3 - DARF Simples

  TSegmentoN3 = class(TObject)
  private
    FSegmentoN       : TSegmentoN;
    FReceita         : Integer;
    FTipoContribuinte: TTipoInscricao;
    FidContribuinte  : string; // Tamanho 14
    FPeriodo         : TDateTime;  // DDMMAAAA
    FReceitaBruta    : Double;
    FPercentual      : Double;
    FValorPrincipal  : Double;
    FMulta           : Double;
    FJuros           : Double;
    FDataVencimento  : TDateTime;
  public
    constructor Create;
    destructor Destroy; override;

    property SegmentoN: TSegmentoN read FSegmentoN write FSegmentoN;
    property Receita: Integer read FReceita write FReceita;
    property TipoContribuinte: TTipoInscricao read FTipoContribuinte write FTipoContribuinte;
    property idContribuinte: string read FidContribuinte write FidContribuinte;
    property Periodo: TDateTime read FPeriodo write FPeriodo;
    property ReceitaBruta: Double read FReceitaBruta write FReceitaBruta;
    property Percentual: Double read FPercentual write FPercentual;
    property ValorPrincipal: Double read FValorPrincipal write FValorPrincipal;
    property Multa: Double read FMulta write FMulta;
    property Juros: Double read FJuros write FJuros;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
  end;

  TSegmentoN3List = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoN3;
    procedure SetItem(Index: Integer; Value: TSegmentoN3);
  public
    function New: TSegmentoN3;
    function First: TSegmentoN3;
    function Last: TSegmentoN3;
    property Items[Index: Integer]: TSegmentoN3 read GetItem write SetItem; default;
  end;

  // Segmento N4 - GARE-SP
  // idTributo: 22 = Tributo GARE-SP ICMS
  //            23 = Tributo GARE-SP DR
  //            24 = Tributo GARE-SP ITCMD

  TSegmentoN4 = class(TObject)
  private
    FSegmentoN       : TSegmentoN;
    FReceita         : Integer;
    FTipoContribuinte: TTipoInscricao;
    FidContribuinte  : string; // Tamanho 14
    FDataVencimento  : TDateTime; // DDMMAAAA
    FInscEst         : string; // Tamanho 12
    FNumEtiqueta     : string; // Tamanho 13
    FReferencia      : Integer;
    FNumParcela      : string; // Tamanho 13
    FValorReceita    : Double;
    FJuros           : Double;
    FMulta           : Double;
  public
    constructor Create;
    destructor Destroy; override;

    property SegmentoN: TSegmentoN read FSegmentoN write FSegmentoN;
    property Receita: Integer read FReceita write FReceita;
    property TipoContribuinte: TTipoInscricao read FTipoContribuinte write FTipoContribuinte;
    property idContribuinte: string read FidContribuinte write FidContribuinte;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
    property InscEst: string read FInscEst write FInscEst;
    property NumEtiqueta: string read FNumEtiqueta write FNumEtiqueta;
    property Referencia: Integer read FReferencia write FReferencia;
    property NumParcela: string read FNumParcela write FNumParcela;
    property ValorReceita: Double read FValorReceita write FValorReceita;
    property Juros: Double read FJuros write FJuros;
    property Multa: Double read FMulta write FMulta;
  end;

  TSegmentoN4List = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoN4;
    procedure SetItem(Index: Integer; Value: TSegmentoN4);
  public
    function New: TSegmentoN4;
    function First: TSegmentoN4;
    function Last: TSegmentoN4;
    property Items[Index: Integer]: TSegmentoN4 read GetItem write SetItem; default;
  end;

  // Segmento N5 - IPVA
  // Segmento N6 - DPVAT
  // Segmento N7 - Licenciamento

  TSegmentoN567 = class(TObject)
  private
    FSegmentoN       : TSegmentoN;
    FTributo         : TIndTributo;
    FReceita         : Integer;
    FTipoContribuinte: TTipoInscricao;
    FidContribuinte  : string; // Tamanho 14
    FExercicio       : Integer;
    FRenavam         : string; // Tamanho 9
    FEstado          : string; // Tamanho 2
    FMunicipio       : Integer;
    FPlaca           : string; // Tamanho 7
    FOpcaoPagamento  : string; // Tamanho 1
    FOpcaoRetirada   : string; // Tamanho 1
    FValorTributo    : Double;
    FDesconto        : Double;
    FDataVencimento  : TDateTime;
    FNovoRenavam: string;
  public
    constructor Create;
    destructor Destroy; override;

    property SegmentoN: TSegmentoN read FSegmentoN write FSegmentoN;
    property Tributo: TIndTributo read FTributo write FTributo;
    property Receita: Integer read FReceita write FReceita;
    property TipoContribuinte: TTipoInscricao read FTipoContribuinte write FTipoContribuinte;
    property idContribuinte: string read FidContribuinte write FidContribuinte;
    property Exercicio: Integer read FExercicio write FExercicio;
    property Renavam: string read FRenavam write FRenavam;
    property Estado: string read FEstado write FEstado;
    property Municipio: Integer read FMunicipio write FMunicipio;
    property Placa: string read FPlaca write FPlaca;
    property OpcaoPagamento: string read FOpcaoPagamento write FOpcaoPagamento;
    property OpcaoRetirada: string read FOpcaoRetirada write FOpcaoRetirada;
    property ValorTributo: Double read FValorTributo write FValorTributo;
    property Desconto: Double read FDesconto write FDesconto;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
    property NovoRenavam: string read FNovoRenavam write FNovoRenavam;
  end;

  TSegmentoN567List = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoN567;
    procedure SetItem(Index: Integer; Value: TSegmentoN567);
  public
    function New: TSegmentoN567;
    function First: TSegmentoN567;
    function Last: TSegmentoN567;
    property Items[Index: Integer]: TSegmentoN567 read GetItem write SetItem; default;
  end;

  // Segmento N8 - DARJ

  TSegmentoN8 = class(TObject)
  private
    FSegmentoN           : TSegmentoN;
    FReceita             : Integer;
    FTipoContribuinte    : TTipoInscricao;
    FidContribuinte      : string; // Tamanho 14
    FInscEst             : string; // Tamanho 8
    FOrigem              : string; // Tamanho 16
    FValorPrincipal      : Double;
    FAtualizacaoMonetaria: Double;
    FMora                : Double;
    FMulta               : Double;
    FDataVencimento      : TDateTime; // DDMMAAAA
    FPeriodoParcela      :Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property SegmentoN: TSegmentoN read FSegmentoN write FSegmentoN;
    property Receita: Integer read FReceita write FReceita;
    property TipoContribuinte: TTipoInscricao read FTipoContribuinte write FTipoContribuinte;
    property idContribuinte: string read FidContribuinte write FidContribuinte;
    property InscEst: string read FInscEst write FInscEst;
    property Origem: string read FOrigem write FOrigem;
    property ValorPrincipal: Double read FValorPrincipal write FValorPrincipal;
    property AtualizacaoMonetaria: Double read FAtualizacaoMonetaria write FAtualizacaoMonetaria;
    property Mora: Double read FMora write FMora;
    property Multa: Double read FMulta write FMulta;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
    property PeriodoParcela: Integer read FPeriodoParcela write FPeriodoParcela;
  end;

  TSegmentoN8List = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoN8;
    procedure SetItem(Index: Integer; Value: TSegmentoN8);
  public
    function New: TSegmentoN8;
    function First: TSegmentoN8;
    function Last: TSegmentoN8;
    property Items[Index: Integer]: TSegmentoN8 read GetItem write SetItem; default;
  end;

// Segmento N9 - FGTS- GRF/GRRF/GRDE

  TSegmentoN9 = class(TObject)
  private
    FSegmentoN        : TSegmentoN;
    FReceita          : Integer;
    FTipoContribuinte : TTipoInscricao;
    FidContribuinte   : string;
    FCodigoBarras     : string;
    FIdentificador    : Integer;
    FLacre            : Integer;
    FLacreDigito: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    property SegmentoN: TSegmentoN read FSegmentoN write FSegmentoN;
    property Receita: Integer read FReceita write FReceita;
    property TipoContribuinte: TTipoInscricao read FTipoContribuinte write FTipoContribuinte;
    property idContribuinte: string read FidContribuinte write FidContribuinte;
    property CodigoBarras: string read FCodigoBarras write FCodigoBarras;
    property Identificador: Integer read FIdentificador write FIdentificador;
    property Lacre: Integer read FLacre write FLacre;
    property LacreDigito: Integer read FLacreDigito write FLacreDigito;
  end;

  TSegmentoN9List = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoN9;
    procedure SetItem(Index: Integer; Value: TSegmentoN9);
  public
    function New: TSegmentoN9;
    function Last: TSegmentoN9;
    property Items[Index: Integer]: TSegmentoN9 read GetItem write SetItem; default;
  end;

  // Lote: Pagamento de Tributos com código de barras
  // Serviço: 22 = Pagamento de Contas, Tributos e Impostos

  // Segmentos Obrigatórios de Remessa: O
  // Segmentos Opcionais de Remessa: W*, Z e B
  // * = obrigatório para FGTS convenio 0181 e 0182

  // Estrutura do Segmento O (Obrigatorio)

  TSegmentoO = class(TObject)
  private
    FTipoMovimento: TTipoMovimento; // Tamanho 1
    FCodMovimento: TInstrucaoMovimento; // Tamanho 2
    FCodigoBarras      : string; // Tamanho 44
    FNomeConcessionaria: string; // Tamanho 30
    FDataVencimento    : TDateTime; // DDMMAAAA
    FDataPagamento     : TDateTime; // DDMMAAAA
    FValorPagamento    : Double;
    FSeuNumero         : string; // Tamanho 20
    FNossoNumero       : string; // Tamanho 20
    FCodOcorrencia : string;
    FSegmentoW: TSegmentoWList;
    FSegmentoZ: TSegmentoZList;
    FSegmentoB: TSegmentoBList;
    FQuantidadeMoeda: Double;
    FValorPago: Double;
    FNotaFiscal: Integer;

    procedure SetSegmentoW(const Value: TSegmentoWList);
    procedure SetSegmentoZ(const Value: TSegmentoZList);
    procedure SetSegmentoB(const Value: TSegmentoBList);
    function GetPagamentoLiberado: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    property TipoMovimento: TTipoMovimento read FTipoMovimento write FTipoMovimento;
    property CodMovimento: TInstrucaoMovimento read FCodMovimento write FCodMovimento;
    property CodigoBarras: string read FCodigoBarras write FCodigoBarras;
    property NomeConcessionaria: string read FNomeConcessionaria write FNomeConcessionaria;
    property DataVencimento: TDateTime read FDataVencimento write FDataVencimento;
    property DataPagamento: TDateTime read FDataPagamento write FDataPagamento;
    property ValorPagamento: Double read FValorPagamento write FValorPagamento;
    property SeuNumero: string read FSeuNumero write FSeuNumero;
    property NossoNumero: string read FNossoNumero write FNossoNumero;
    property CodOcorrencia: string read FCodOcorrencia write FCodOcorrencia;
    property SegmentoW: TSegmentoWList read FSegmentoW write SetSegmentoW;
    property SegmentoZ: TSegmentoZList read FSegmentoZ write SetSegmentoZ;
    property SegmentoB: TSegmentoBList read FSegmentoB write SetSegmentoB;
    property QuantidadeMoeda: Double read FQuantidadeMoeda write FQuantidadeMoeda;
    property ValorPago: Double read FValorPago write FValorPago;
    property NotaFiscal: Integer read FNotaFiscal write FNotaFiscal;
    property PagamentoLiberado: Boolean read GetPagamentoLiberado;
  end;

  TSegmentoOList = class(TObjectList)
  private
    function GetItem(Index: Integer): TSegmentoO;
    procedure SetItem(Index: Integer; Value: TSegmentoO);
  public
    function New: TSegmentoO;
    function First: TSegmentoO;
    function Last: TSegmentoO;
    property Items[Index: Integer]: TSegmentoO read GetItem write SetItem; default;
  end;

  // Lote

  TLote = class(TObject)
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
    FSegmentoW: TSegmentoWList;

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
    procedure SetSegmentoW(const Value: TSegmentoWList);
  public
    constructor Create;
    destructor Destroy; override;

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
    property SegmentoW: TSegmentoWList read FSegmentoW write SetSegmentoW;

    property Registro5: TRegistro5 read FRegistro5 write FRegistro5;
  end;

  TLoteList = class(TObjectList)
  private
    function GetItem(Index: Integer): TLote;
    procedure SetItem(Index: Integer; Value: TLote);
  public
    function New: TLote;
    function Last: TLote;
    property Items[Index: Integer]: TLote read GetItem write SetItem; default;
  end;

  TPagFor = class(TPersistent)
  private
    FAtivo: Boolean;
    FGeral: TGeral;
    FRegistro0: TRegistro0;
    FLote: TLoteList;
    FRegistro9: TRegistro9;
    FNomeArq: string;

    procedure SetLote(const Value: TLoteList);
  public
    constructor Create;
    destructor Destroy; override;

    property Ativo: Boolean read FAtivo write FAtivo;
    property Geral: TGeral read FGeral write FGeral;

    property Registro0: TRegistro0 read FRegistro0 write FRegistro0;
    property Lote: TLoteList read FLote write SetLote;
    property Registro9: TRegistro9 read FRegistro9 write FRegistro9;
    property NomeArq: string read FNomeArq write FNomeArq;
  end;

const
  // Os códigos de aprovação de pagamento (pagamento efetuado)
  PAGAMENTO_LIBERADO_BANCO = '00 BD BDCI BDCD BDCN';
  // Os códigos que não vai gerar aviso
  PAGAMENTO_LIBERADO_AVISO = '00';
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

  inherited Destroy;
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

  inherited Destroy;
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

  inherited Destroy;
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

  inherited Destroy;
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

  inherited Destroy;
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

  inherited Destroy;
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

  inherited Destroy;
end;

{ TRegistro0 }

constructor TRegistro0.Create;
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

  inherited Destroy;
end;

{ TRegistro9 }

constructor TRegistro9.Create;
begin
  inherited Create;

  FTotais := TTotais.Create;
end;

destructor TRegistro9.Destroy;
begin
  FTotais.Free;

  inherited Destroy;
end;

{ TRegistro1 }

constructor TRegistro1.Create;
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

  inherited Destroy;
end;

{ TRegistro5 }

constructor TRegistro5.Create;
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

  inherited Destroy;
end;

{ TSegmentoA }

constructor TSegmentoA.Create;
begin
  inherited Create;
  FFavorecido := TFavorecido.Create;
  FCredito    := TCredito.Create;

  FSegmentoB := TSegmentoBList.Create;
  FSegmentoC := TSegmentoCList.Create;
//  FSegmentoE := TSegmentoEList.Create{(Self)};
//  FSegmentoF := TSegmentoFList.Create{(Self)};
  FSegmentoZ := TSegmentoZList.Create;
end;

destructor TSegmentoA.Destroy;
begin
  FFavorecido.Free;
  FCredito.Free;
  FSegmentoB.Free;
  FSegmentoC.Free;
//  FSegmentoE.Free;
//  FSegmentoF.Free;
  FSegmentoZ.Free;

  inherited Destroy;
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
{
procedure TSegmentoA.SetSegmentoE(const Value: TSegmentoEList);
begin
  FSegmentoE := Value;
end;

procedure TSegmentoA.SetSegmentoF(const Value: TSegmentoFList);
begin
  FSegmentoF := Value;
end;
}
procedure TSegmentoA.SetSegmentoZ(const Value: TSegmentoZList);
begin
  FSegmentoZ := Value;
end;

{ TSegmentoAList }

function TSegmentoAList.New: TSegmentoA;
begin
  Result := TSegmentoA.Create;
  Add(Result);
end;

function TSegmentoAList.GetItem(Index: Integer): TSegmentoA;
begin
  Result := TSegmentoA(inherited GetItem(Index));
end;

function TSegmentoAList.First: TSegmentoA;
begin
  Result := TSegmentoA(inherited First);
end;

function TSegmentoAList.Last: TSegmentoA;
begin
  Result := TSegmentoA(inherited Last);
end;

procedure TSegmentoAList.SetItem(Index: Integer; Value: TSegmentoA);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoB }

constructor TSegmentoB.Create;
begin
  inherited Create;

  FInscricao := TInscricao.Create;
  FEndereco  := TEndereco.Create;

  FPixTipoChave := tcpNenhum;
end;

destructor TSegmentoB.Destroy;
begin
  FInscricao.Free;
  FEndereco.Free;

  inherited Destroy;
end;

{ TSegmentoBList }

function TSegmentoBList.New: TSegmentoB;
begin
  Result := TSegmentoB.Create;
  Add(Result);
end;

function TSegmentoBList.First: TSegmentoB;
begin
  Result := TSegmentoB(inherited First);
end;

function TSegmentoBList.GetItem(Index: Integer): TSegmentoB;
begin
  Result := TSegmentoB(inherited GetItem(Index));
end;

function TSegmentoBList.Last: TSegmentoB;
begin
  Result := TSegmentoB(inherited Last);
end;

procedure TSegmentoBList.SetItem(Index: Integer; Value: TSegmentoB);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoC }

constructor TSegmentoC.Create;
begin
  inherited Create;

  FSubstituta := TSubstituta.Create;
end;

destructor TSegmentoC.Destroy;
begin
  FSubstituta.Free;

  inherited Destroy;
end;

{ TSegmentoCList }

function TSegmentoCList.New: TSegmentoC;
begin
  Result := TSegmentoC.Create;
  Add(Result);
end;

function TSegmentoCList.GetItem(Index: Integer): TSegmentoC;
begin
  Result := TSegmentoC(inherited GetItem(Index));
end;

function TSegmentoCList.First: TSegmentoC;
begin
  Result := TSegmentoC(inherited First);
end;

function TSegmentoCList.Last: TSegmentoC;
begin
  Result := TSegmentoC(inherited Last);
end;

procedure TSegmentoCList.SetItem(Index: Integer; Value: TSegmentoC);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoJ }

constructor TSegmentoJ.Create;
begin
  inherited Create;

  FSegmentoJ52 := TSegmentoJ52List.Create;
  FSegmentoJ99 := TSegmentoJ99List.Create;
//  FSegmentoB := TSegmentoBList.Create;
//  FSegmentoC := TSegmentoCList.Create;
  FSegmentoZ := TSegmentoZList.Create;
end;

destructor TSegmentoJ.Destroy;
begin
  FSegmentoJ52.Free;
  FSegmentoJ99.Free;
//  FSegmentoB.Free;
//  FSegmentoC.Free;
  FSegmentoZ.Free;

  inherited Destroy;
end;

function TSegmentoJ.GetPagamentoLiberado: Boolean;
//var
//  I: Integer;
begin
  Result := POS(CodOcorrencia, PAGAMENTO_LIBERADO_BANCO) > 0;
  {
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
  }
end;
{
procedure TSegmentoJ.SetSegmentoB(const Value: TSegmentoBList);
begin
  FSegmentoB := Value;
end;

procedure TSegmentoJ.SetSegmentoC(const Value: TSegmentoCList);
begin
  FSegmentoC := Value;
end;
}
procedure TSegmentoJ.SetSegmentoJ52(const Value: TSegmentoJ52List);
begin
  FSegmentoJ52 := Value;
end;

procedure TSegmentoJ.SetSegmentoJ99(const Value: TSegmentoJ99List);
begin
  FSegmentoJ99 := Value;
end;

procedure TSegmentoJ.SetSegmentoZ(const Value: TSegmentoZList);
begin
  FSegmentoZ := Value;
end;

{ TSegmentoJList }

function TSegmentoJList.New: TSegmentoJ;
begin
  Result := TSegmentoJ.Create;
  Add(Result);
end;

function TSegmentoJList.GetItem(Index: Integer): TSegmentoJ;
begin
  Result := TSegmentoJ(inherited GetItem(Index));
end;

function TSegmentoJList.First: TSegmentoJ;
begin
  Result := TSegmentoJ(inherited First);
end;

function TSegmentoJList.Last: TSegmentoJ;
begin
  Result := TSegmentoJ(inherited Last);
end;

procedure TSegmentoJList.SetItem(Index: Integer; Value: TSegmentoJ);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoN1 }

constructor TSegmentoN1.Create;
begin
  inherited Create;

  FSegmentoN := TSegmentoN.Create;
end;

destructor TSegmentoN1.Destroy;
begin
  FSegmentoN.Free;

  inherited Destroy;
end;

{ TSegmentoN1List }

function TSegmentoN1List.New: TSegmentoN1;
begin
  Result := TSegmentoN1.Create;
  Add(Result);
end;

function TSegmentoN1List.GetItem(Index: Integer): TSegmentoN1;
begin
  Result := TSegmentoN1(inherited GetItem(Index));
end;

function TSegmentoN1List.First: TSegmentoN1;
begin
  Result := TSegmentoN1(inherited First);
end;

function TSegmentoN1List.Last: TSegmentoN1;
begin
  Result := TSegmentoN1(inherited Last);
end;

procedure TSegmentoN1List.SetItem(Index: Integer; Value: TSegmentoN1);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoN2 }

constructor TSegmentoN2.Create;
begin
  inherited Create;

  FSegmentoN := TSegmentoN.Create;
end;

destructor TSegmentoN2.Destroy;
begin
  FSegmentoN.Free;

  inherited Destroy;
end;

{ TSegmentoN2List }

function TSegmentoN2List.New: TSegmentoN2;
begin
  Result := TSegmentoN2.Create;
  Add(Result);
end;

function TSegmentoN2List.GetItem(Index: Integer): TSegmentoN2;
begin
  Result := TSegmentoN2(inherited GetItem(Index));
end;

function TSegmentoN2List.Last: TSegmentoN2;
begin
  Result := TSegmentoN2(inherited GetItem(Count-1));
end;

procedure TSegmentoN2List.SetItem(Index: Integer; Value: TSegmentoN2);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoN3 }

constructor TSegmentoN3.Create;
begin
  inherited Create;

  FSegmentoN := TSegmentoN.Create;
end;

destructor TSegmentoN3.Destroy;
begin
  FSegmentoN.Free;

  inherited Destroy;
end;

{ TSegmentoN3List }

function TSegmentoN3List.New: TSegmentoN3;
begin
  Result := TSegmentoN3.Create;
  Add(Result);
end;

function TSegmentoN3List.GetItem(Index: Integer): TSegmentoN3;
begin
  Result := TSegmentoN3(inherited GetItem(Index));
end;

function TSegmentoN3List.First: TSegmentoN3;
begin
  Result := TSegmentoN3(inherited First);
end;

function TSegmentoN3List.Last: TSegmentoN3;
begin
  Result := TSegmentoN3(inherited Last);
end;

procedure TSegmentoN3List.SetItem(Index: Integer; Value: TSegmentoN3);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoN4 }

constructor TSegmentoN4.Create;
begin
  inherited Create;

  FSegmentoN := TSegmentoN.Create;
end;

destructor TSegmentoN4.Destroy;
begin
  FSegmentoN.Free;

  inherited Destroy;
end;

{ TSegmentoN4List }

function TSegmentoN4List.New: TSegmentoN4;
begin
  Result := TSegmentoN4.Create;
  Add(Result);
end;

function TSegmentoN4List.GetItem(Index: Integer): TSegmentoN4;
begin
  Result := TSegmentoN4(inherited GetItem(Index));
end;

function TSegmentoN4List.First: TSegmentoN4;
begin
  Result := TSegmentoN4(inherited First);
end;

function TSegmentoN4List.Last: TSegmentoN4;
begin
  Result := TSegmentoN4(inherited Last);
end;

procedure TSegmentoN4List.SetItem(Index: Integer; Value: TSegmentoN4);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoN567 }

constructor TSegmentoN567.Create;
begin
  inherited Create;

  FSegmentoN := TSegmentoN.Create;
end;

destructor TSegmentoN567.Destroy;
begin
  FSegmentoN.Free;

  inherited Destroy;
end;

{ TSegmentoN567List }

function TSegmentoN567List.New: TSegmentoN567;
begin
  Result := TSegmentoN567.Create;
  Add(Result);
end;

function TSegmentoN567List.GetItem(Index: Integer): TSegmentoN567;
begin
  Result := TSegmentoN567(inherited GetItem(Index));
end;

function TSegmentoN567List.First: TSegmentoN567;
begin
  Result := TSegmentoN567(inherited First);
end;

function TSegmentoN567List.Last: TSegmentoN567;
begin
  Result := TSegmentoN567(inherited Last);
end;

procedure TSegmentoN567List.SetItem(Index: Integer; Value: TSegmentoN567);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoN8 }

constructor TSegmentoN8.Create;
begin
  inherited Create;

  FSegmentoN := TSegmentoN.Create;
end;

destructor TSegmentoN8.Destroy;
begin
  FSegmentoN.Free;

  inherited Destroy;
end;

{ TSegmentoN8List }

function TSegmentoN8List.New: TSegmentoN8;
begin
  Result := TSegmentoN8.Create;
  Add(Result);
end;

function TSegmentoN8List.GetItem(Index: Integer): TSegmentoN8;
begin
  Result := TSegmentoN8(inherited GetItem(Index));
end;

function TSegmentoN8List.First: TSegmentoN8;
begin
  Result := TSegmentoN8(inherited First);
end;

function TSegmentoN8List.Last: TSegmentoN8;
begin
  Result := TSegmentoN8(inherited Last);
end;

procedure TSegmentoN8List.SetItem(Index: Integer; Value: TSegmentoN8);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoO }

constructor TSegmentoO.Create;
begin
  inherited Create;

  FSegmentoW := TSegmentoWList.Create;
  FSegmentoZ := TSegmentoZList.Create;
  FSegmentoB := TSegmentoBList.Create;
end;

destructor TSegmentoO.Destroy;
begin
  FSegmentoW.Free;
  FSegmentoZ.Free;
  FSegmentoB.Free;

  inherited Destroy;
end;

function TSegmentoO.GetPagamentoLiberado: Boolean;
begin
  Result := POS(CodOcorrencia, PAGAMENTO_LIBERADO_BANCO) > 0;
end;

procedure TSegmentoO.SetSegmentoW(const Value: TSegmentoWList);
begin
  FSegmentoW := Value;
end;

procedure TSegmentoO.SetSegmentoZ(const Value: TSegmentoZList);
begin
  FSegmentoZ := Value;
end;

procedure TSegmentoO.SetSegmentoB(const Value: TSegmentoBList);
begin
  FSegmentoB := Value;
end;

{ TSegmentoOList }

function TSegmentoOList.New: TSegmentoO;
begin
  Result := TSegmentoO.Create;
  Add(Result);
end;

function TSegmentoOList.GetItem(Index: Integer): TSegmentoO;
begin
  Result := TSegmentoO(inherited GetItem(Index));
end;

function TSegmentoOList.First: TSegmentoO;
begin
  Result := TSegmentoO(inherited First);
end;

function TSegmentoOList.Last: TSegmentoO;
begin
  Result := TSegmentoO(inherited Last);
end;

procedure TSegmentoOList.SetItem(Index: Integer; Value: TSegmentoO);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoWList }

function TSegmentoWList.New: TSegmentoW;
begin
  Result := TSegmentoW.Create;
  Add(Result);
end;

function TSegmentoWList.GetItem(Index: Integer): TSegmentoW;
begin
  Result := TSegmentoW(inherited GetItem(Index));
end;

function TSegmentoWList.First: TSegmentoW;
begin
  Result := TSegmentoW(inherited First);
end;

function TSegmentoWList.Last: TSegmentoW;
begin
  Result := TSegmentoW(inherited Last);
end;

procedure TSegmentoWList.SetItem(Index: Integer; Value: TSegmentoW);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoZList }

function TSegmentoZList.New: TSegmentoZ;
begin
  Result := TSegmentoZ.Create;
  Add(Result);
end;

function TSegmentoZList.GetItem(Index: Integer): TSegmentoZ;
begin
  Result := TSegmentoZ(inherited GetItem(Index));
end;

function TSegmentoZList.Last: TSegmentoZ;
begin
  Result := TSegmentoZ(inherited Last);
end;

procedure TSegmentoZList.SetItem(Index: Integer; Value: TSegmentoZ);
begin
  inherited SetItem(Index, Value);
end;

{ TPagFor }

constructor TPagFor.Create;
begin
  inherited Create;

  FGeral := TGeral.Create;
  FRegistro0 := TRegistro0.Create;
  FLote      := TLoteList.Create;
  FRegistro9 := TRegistro9.Create;
end;

destructor TPagFor.Destroy;
begin
  FGeral.Free;
  FRegistro0.Free;
  FLote.Free;
  FRegistro9.Free;

  inherited Destroy;
end;

procedure TPagFor.SetLote(const Value: TLoteList);
begin
  FLote := Value;
end;

{ TLote }

constructor TLote.Create;
begin
  inherited Create;

  FRegistro1 := TRegistro1.Create;

  FSegmentoA    := TSegmentoAList.Create;
  FSegmentoG    := TSegmentoGList.Create;
  FSegmentoJ    := TSegmentoJList.Create;
  FSegmentoN1   := TSegmentoN1List.Create;
  FSegmentoN2   := TSegmentoN2List.Create;
  FSegmentoN3   := TSegmentoN3List.Create;
  FSegmentoN4   := TSegmentoN4List.Create;
  FSegmentoN567 := TSegmentoN567List.Create;
  FSegmentoN8   := TSegmentoN8List.Create;
  FSegmentoN9   := TSegmentoN9List.Create;
  FSegmentoO    := TSegmentoOList.Create;
  FSegmentoW    := TSegmentoWList.Create;

  FRegistro5    := TRegistro5.Create;
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
  FSegmentoW.Free;

  FRegistro5.Free;

  inherited Destroy;
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

procedure TLote.SetSegmentoW(const Value: TSegmentoWList);
begin
  FSegmentoW := Value;
end;

{ TLoteList }

function TLoteList.New: TLote;
begin
  Result := TLote.Create;
  Add(Result);
end;

function TLoteList.GetItem(Index: Integer): TLote;
begin
  Result := TLote(inherited GetItem(Index));
end;

function TLoteList.Last: TLote;
begin
  Result := TLote(inherited Last);
end;

procedure TLoteList.SetItem(Index: Integer; Value: TLote);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoEList }

function TSegmentoEList.New: TSegmentoE;
begin
  Result := TSegmentoE.Create;
  Add(Result);
end;

function TSegmentoEList.GetItem(Index: Integer): TSegmentoE;
begin
  Result := TSegmentoE(inherited GetItem(Index));
end;

function TSegmentoEList.Last: TSegmentoE;
begin
  Result := TSegmentoE(inherited Last);
end;

procedure TSegmentoEList.SetItem(Index: Integer; Value: TSegmentoE);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoFList }

function TSegmentoFList.New: TSegmentoF;
begin
  Result := TSegmentoF.Create;
  Add(Result);
end;

function TSegmentoFList.GetItem(Index: Integer): TSegmentoF;
begin
  Result := TSegmentoF(inherited GetItem(Index));
end;

function TSegmentoFList.Last: TSegmentoF;
begin
  Result := TSegmentoF(inherited Last);
end;

procedure TSegmentoFList.SetItem(Index: Integer; Value: TSegmentoF);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoJ52List }

function TSegmentoJ52List.New: TSegmentoJ52;
begin
  Result := TSegmentoJ52.Create;
  Add(Result);
end;

function TSegmentoJ52List.GetItem(Index: Integer): TSegmentoJ52;
begin
  Result := TSegmentoJ52(inherited GetItem(Index));
end;

function TSegmentoJ52List.Last: TSegmentoJ52;
begin
  Result := TSegmentoJ52(inherited Last);
end;

procedure TSegmentoJ52List.SetItem(Index: Integer; Value: TSegmentoJ52);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoJ52 }

constructor TSegmentoJ52.Create;
begin
  inherited Create;

  FPagador := TPagador.Create;
  FBeneficiario := TBeneficiario.Create;
  FSacadorAvalista := TSacadorAvalista.Create;
end;

destructor TSegmentoJ52.Destroy;
begin
  FPagador.Free;
  FBeneficiario.Free;
  FSacadorAvalista.Free;

  inherited Destroy;
end;

{ TPagador }

constructor TPagador.Create;
begin
  inherited Create;

  FInscricao := TInscricao.Create;
end;

destructor TPagador.Destroy;
begin
  FInscricao.Free;

  inherited Destroy;
end;

{ TBeneficiario }

constructor TBeneficiario.Create;
begin
  inherited Create;

  FInscricao := TInscricao.Create;
end;

destructor TBeneficiario.Destroy;
begin
  FInscricao.Free;

  inherited Destroy;
end;

{ TSacadorAvalista }

constructor TSacadorAvalista.Create;
begin
  inherited Create;

  FInscricao := TInscricao.Create;
end;

destructor TSacadorAvalista.Destroy;
begin
  FInscricao.Free;

  inherited Destroy;
end;

{ TSegmentoN9 }

constructor TSegmentoN9.Create;
begin
  inherited Create;

  FSegmentoN := TSegmentoN.Create;
end;

destructor TSegmentoN9.Destroy;
begin
  FSegmentoN.Free;

  inherited Destroy;
end;

{ TSegmentoN9List }

function TSegmentoN9List.New: TSegmentoN9;
begin
  Result := TSegmentoN9.Create;
  Add(Result);
end;

function TSegmentoN9List.GetItem(Index: Integer): TSegmentoN9;
begin
  Result := TSegmentoN9(inherited GetItem(Index));
end;

function TSegmentoN9List.Last: TSegmentoN9;
begin
  Result := TSegmentoN9(inherited Last);
end;

procedure TSegmentoN9List.SetItem(Index: Integer; Value: TSegmentoN9);
begin
  inherited SetItem(Index, Value);
end;

{ TArquivoTXT }

constructor TArquivoTXT.Create;
begin
  inherited Create;

  FArquivoTXT := TStringList.Create;
end;

destructor TArquivoTXT.Destroy;
begin
  FArquivoTXT.Free;

  inherited Destroy;
end;

{ TArquivoTXTList }

function TArquivoTXTList.New: TArquivoTXT;
begin
  Result := TArquivoTXT.Create;
  Add(Result);
end;

function TArquivoTXTList.GetItem(Index: Integer): TArquivoTXT;
begin
  Result := TArquivoTXT(inherited GetItem(Index));
end;

function TArquivoTXTList.Last: TArquivoTXT;
begin
  Result := TArquivoTXT(inherited Last);
end;

procedure TArquivoTXTList.SetItem(Index: Integer; Value: TArquivoTXT);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoN }

constructor TSegmentoN.Create;
begin
  inherited Create;

  FSegmentoB := TSegmentoBList.Create;
  FSegmentoW := TSegmentoWList.Create;
  FSegmentoZ := TSegmentoZList.Create;
end;

destructor TSegmentoN.Destroy;
begin
  FSegmentoB.Free;
  FSegmentoW.Free;
  FSegmentoZ.Free;

  inherited Destroy;
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

function TAvisoList.New: TAviso;
begin
  Result := TAviso.Create;
  Add(Result);
end;

function TAvisoList.GetItem(Index: Integer): TAviso;
begin
  Result := TAviso(inherited GetItem(Index));
end;

function TAvisoList.Last: TAviso;
begin
  Result := TAviso(inherited Last);
end;

procedure TAvisoList.SetItem(Index: Integer; Value: TAviso);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoGList }

function TSegmentoGList.New: TSegmentoG;
begin
  Result := TSegmentoG.Create;
  Add(Result);
end;

function TSegmentoGList.GetItem(Index: Integer): TSegmentoG;
begin
  Result := TSegmentoG(inherited GetItem(Index));
end;

function TSegmentoGList.Last: TSegmentoG;
begin
  Result := TSegmentoG(inherited Last);
end;

procedure TSegmentoGList.SetItem(Index: Integer; Value: TSegmentoG);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoG }

constructor TSegmentoG.Create;
begin
  inherited Create;

  FCedente := TCedente.Create;
  FDesconto1 := TAcrescimosDescontos.Create;
  FSegmentoH := TSegmentoHList.Create();
end;

destructor TSegmentoG.Destroy;
begin
  FCedente.Free;
  FDesconto1.Free;
  SegmentoH.Free;

  inherited Destroy;
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

  inherited Destroy;
end;

{ TSegmentoHList }

function TSegmentoHList.New: TSegmentoH;
begin
  Result := TSegmentoH.Create;
  Add(Result);
end;

function TSegmentoHList.GetItem(Index: Integer): TSegmentoH;
begin
  Result := TSegmentoH(inherited GetItem(Index));
end;

function TSegmentoHList.Last: TSegmentoH;
begin
  Result := TSegmentoH(inherited Last);
end;

procedure TSegmentoHList.SetItem(Index: Integer; Value: TSegmentoH);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoH }

constructor TSegmentoH.Create;
begin
  inherited Create;

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

  inherited Destroy;
end;

{ TSegmentoJ99List }

function TSegmentoJ99List.GetItem(Index: Integer): TSegmentoJ99;
begin
  Result := TSegmentoJ99(inherited GetItem(Index));
end;

function TSegmentoJ99List.Last: TSegmentoJ99;
begin
  Result := TSegmentoJ99(inherited Last);
end;

function TSegmentoJ99List.New: TSegmentoJ99;
begin
  Result := TSegmentoJ99.Create;
  Add(Result);
end;

procedure TSegmentoJ99List.SetItem(Index: Integer; Value: TSegmentoJ99);
begin
  inherited SetItem(Index, Value);
end;

{ TSegmentoJ99 }

constructor TSegmentoJ99.Create;
begin
  inherited Create;

end;

destructor TSegmentoJ99.Destroy;
begin

  inherited Destroy;
end;

{ TSubstituta }

constructor TSubstituta.Create;
begin
  inherited Create;

  FContaCorrente := TContaCorrente.Create;
end;

destructor TSubstituta.Destroy;
begin
  FContaCorrente.Free;

  inherited Destroy;
end;

end.
