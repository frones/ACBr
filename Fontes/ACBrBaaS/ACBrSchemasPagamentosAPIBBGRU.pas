{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César                                                                }
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

// Documentação:
// https://apoio.developers.bb.com.br/referency/post/61cdac823948cb0012557c8f

{$I ACBr.inc}

unit ACBrSchemasPagamentosAPIBBGRU;

interface

uses
  Classes, SysUtils, ACBrAPIBase, ACBrJSON,
  ACBrSchemasPagamentosAPIBB;

type

  { TACBrPagamentosBBGRU }

  TACBrPagamentosBBGRU = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBGRU);

    property codigo: String read fcodigo write fcodigo;
    property nomeRecebedor: String read fnomeRecebedor write fnomeRecebedor;
    property numeroReferencia: String read fnumeroReferencia write fnumeroReferencia;
    property mesAnoCompetencia: Integer read fmesAnoCompetencia write fmesAnoCompetencia;
    property dataVencimento: TDateTime read fdataVencimento write fdataVencimento;
    property identificacaoContribuinte: Integer read fidentificacaoContribuinte write fidentificacaoContribuinte;
    property valorPrincipal: Double read fvalorPrincipal write fvalorPrincipal;
    property valorDesconto: Double read fvalorDesconto write fvalorDesconto;
    property valorOutroDeducao: Double read fvalorOutroDeducao write fvalorOutroDeducao;
    property valorMulta: Double read fvalorMulta write fvalorMulta;
    property valorJuroEncargo: Double read fvalorJuroEncargo write fvalorJuroEncargo;
    property valorOutro: Double read fvalorOutro write fvalorOutro;
    property texto: String read ftexto write ftexto;
  end;

  { TACBrPagamentosBBGRULista }

  TACBrPagamentosBBGRULista = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBGRU;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBGRU);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentosBBGRU): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentosBBGRU);
    function New: TACBrPagamentosBBGRU;
    property Items[aIndex: Integer]: TACBrPagamentosBBGRU read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBConsultaRespostaGRU }

  TACBrPagamentosBBConsultaRespostaGRU = class(TACBrPagamentosBBConsultaRespostaBase)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBConsultaRespostaGRU);

    property listaPagamentos: TACBrPagamentosBBGRULista read flistaPagamentos write flistaPagamentos;
    property listaOcorrencias: TACBrPagamentosBBOcorrenciaLista read flistaDevolucao write flistaDevolucao;
  end;

  { TACBrPagamentosBBPagamentoGRU }

  TACBrPagamentosBBPagamentoGRU = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBPagamentoGRU);

    property id: Integer read fid write fid;
    property data: TDateTime read fdata write fdata;
    property valor: Double read fvalor write fvalor;
    property valorPrincipal: Double read fvalorPrincipal write fvalorPrincipal;
    property valorDesconto: Double read fvalorDesconto write fvalorDesconto;
    property valorOutroDeducao: Double read fvalorOutroDeducao write fvalorOutroDeducao;
    property valorMulta: Double read fvalorMulta write fvalorMulta;
    property valorJuroEncargo: Double read fvalorJuroEncargo write fvalorJuroEncargo;
    property valorOutro: Double read fvalorOutro write fvalorOutro;
    property cpfCnpjContribuinte: Int64 read fcpfCnpjContribuinte write fcpfCnpjContribuinte;
    property numeroDocumentoDebito: Integer read fnumeroDocumentoDebito write fnumeroDocumentoDebito;
    property textoDescricao: String read ftextoDescricao write ftextoDescricao;
  end;

  { TACBrPagamentosBBPagamentosGRU }

  TACBrPagamentosBBPagamentosGRU = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBPagamentoGRU;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBPagamentoGRU);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentosBBPagamentoGRU): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentosBBPagamentoGRU);
    function New: TACBrPagamentosBBPagamentoGRU;
    property Items[aIndex: Integer]: TACBrPagamentosBBPagamentoGRU read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBLancamentoGRU }

  TACBrPagamentosBBLancamentoGRU = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLancamentoGRU);

    property nomeConvenente: String read fnomeConvenente write fnomeConvenente;
    property textoCodigoBarras: String read ftextoCodigoBarras write ftextoCodigoBarras;
    property numeroReferencia: String read fnumeroReferencia write fnumeroReferencia;
    property dataVencimento: TDateTime read fdataVencimento write fdataVencimento;
    property mesAnoCompetencia: Integer read fmesAnoCompetencia write fmesAnoCompetencia;
    property pagamento: TACBrPagamentosBBPagamentosGRU read fpagamento write fpagamento;
    property indicadorMovimentoAceito: String read findicadorMovimentoAceito write findicadorMovimentoAceito;
    property erros: TACBrPagamentosBBLancamentoErros read ferros write ferros;
  end;

  { TACBrPagamentosBBLancamentosGRU }

  TACBrPagamentosBBLancamentosGRU = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBLancamentoGRU;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBLancamentoGRU);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentosBBLancamentoGRU): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentosBBLancamentoGRU);
    function New: TACBrPagamentosBBLancamentoGRU;
    property Items[aIndex: Integer]: TACBrPagamentosBBLancamentoGRU read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBConsultaLoteRespostaGRU }

  TACBrPagamentosBBConsultaLoteRespostaGRU = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBConsultaLoteRespostaGRU);

    property numeroRequisicao: Integer read fnumeroRequisicao write fnumeroRequisicao;
    property codigoEstadoRequisicao: TACBrPagamentosBBEstadoRequisicao read fcodigoEstadoRequisicao write fcodigoEstadoRequisicao;
    property quantidadeTotalLancamento: Integer read fquantidadeTotalLancamento write fquantidadeTotalLancamento;
    property valorTotalLancamento: Double read fvalorTotalLancamento write fvalorTotalLancamento;
    property quantidadeTotalValido: Integer read fquantidadeTotalValido write fquantidadeTotalValido;
    property valorLancamentosValidos: Double read fvalorLancamentosValidos write fvalorLancamentosValidos;
    property lancamentos: TACBrPagamentosBBLancamentosGRU read flancamentos write flancamentos;
  end;

  { TACBrPagamentosBBRequisicaoGRU }

  TACBrPagamentosBBRequisicaoGRU = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBRequisicaoGRU);

    property codigoBarras: String read fcodigoBarras write fcodigoBarras;
    property dataVencimento: TDateTime read fdataVencimento write fdataVencimento;
    property dataPagamento: TDateTime read fdataPagamento write fdataPagamento;
    property valorPagamento: Double read fvalorPagamento write fvalorPagamento;
    property numeroDocumentoDebito: Integer read fnumeroDocumentoDebito write fnumeroDocumentoDebito;
    property textoPagamento: String read ftextoPagamento write ftextoPagamento;
    property numeroReferencia: String read fnumeroReferencia write fnumeroReferencia;
    property mesAnoCompetencia: Integer read fmesAnoCompetencia write fmesAnoCompetencia;
    property idContribuinte: Int64 read fidContribuinte write fidContribuinte;
    property valorPrincipal: Double read fvalorPrincipal write fvalorPrincipal;
    property valorDesconto: Double read fvalorDesconto write fvalorDesconto;
    property valorOutraDeducao: Double read fvalorOutraDeducao write fvalorOutraDeducao;
    property valorMulta: Double read fvalorMulta write fvalorMulta;
    property valorJuroEncargo: Double read fvalorJuroEncargo write fvalorJuroEncargo;
    property valorOutroAcrescimo: Double read fvalorOutroAcrescimo write fvalorOutroAcrescimo;
  end;

  { TACBrPagamentosBBRequisicaoGRULista }

  TACBrPagamentosBBRequisicaoGRULista = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBRequisicaoGRU;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBRequisicaoGRU);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentosBBRequisicaoGRU): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentosBBRequisicaoGRU);
    function New: TACBrPagamentosBBRequisicaoGRU;
    property Items[aIndex: Integer]: TACBrPagamentosBBRequisicaoGRU read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBLotePagamentosRequisicaoGRU }

  TACBrPagamentosBBLotePagamentosRequisicaoGRU = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLotePagamentosRequisicaoGRU);

    property numeroRequisicao: Integer read fnumeroRequisicao write fnumeroRequisicao;
    property codigoContrato: Integer read fcodigoContrato write fcodigoContrato;
    property agencia: Integer read fagencia write fagencia;
    property conta: Integer read fconta write fconta;
    property digitoConta: String read fdigitoConta write fdigitoConta;
    property listaRequisicao: TACBrPagamentosBBRequisicaoGRULista read flistaRequisicao write flistaRequisicao;
  end;

implementation

end.

