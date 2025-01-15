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

unit ACBrSchemasPagamentosAPIBBDARFPreto;

interface

uses
  Classes, SysUtils, ACBrAPIBase, ACBrJSON,
  ACBrSchemasPagamentosAPIBB;

type

  { TACBrPagamentosBBDARFPreto }

  TACBrPagamentosBBDARFPreto = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBDARFPreto);

    property codigo: Integer read fcodigo write fcodigo;
    property tipoContribuinte: TACBrPagamentosBBTipoContribuinte read ftipoContribuinte write ftipoContribuinte;
    property identificacaoContribuinte: Integer read fidentificacaoContribuinte write fidentificacaoContribuinte;
    property identificacaoTributo: String read fidentificacaoTributo write fidentificacaoTributo;
    property dataApuracao: TDateTime read fdataApuracao write fdataApuracao;
    property numeroReferencia: Integer read fnumeroReferencia write fnumeroReferencia;
    property valorPrincipal: Double read fvalorPrincipal write fvalorPrincipal;
    property valorMulta: Double read fvalorMulta write fvalorMulta;
    property valorJuroEncargo: Double read fvalorJuroEncargo write fvalorJuroEncargo;
    property dataVencimento: TDateTime read fdataVencimento write fdataVencimento;
    property textoLivre: String read ftextoLivre write ftextoLivre;
  end;

  { TACBrPagamentosBBDARFPretoLista }

  TACBrPagamentosBBDARFPretoLista = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBDARFPreto;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBDARFPreto);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentosBBDARFPreto): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentosBBDARFPreto);
    function New: TACBrPagamentosBBDARFPreto;
    property Items[aIndex: Integer]: TACBrPagamentosBBDARFPreto read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBConsultaRespostaDARFPreto }

  TACBrPagamentosBBConsultaRespostaDARFPreto = class(TACBrPagamentosBBConsultaRespostaBase)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBConsultaRespostaDARFPreto);

    property listaPagamentos: TACBrPagamentosBBDARFPretoLista read flistaPagamentos write flistaPagamentos;
    property listaDevolucao: TACBrPagamentosBBDevolucaoListaBase read flistaDevolucao write flistaDevolucao;
  end;

  { TACBrPagamentosBBPagamentoDARFPreto }

  TACBrPagamentosBBPagamentoDARFPreto = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBPagamentoDARFPreto);

    property id: Integer read fid write fid;
    property data: TDateTime read fdata write fdata;
    property valor: Double read fvalor write fvalor;
    property valorPrincipal: Double read fvalorPrincipal write fvalorPrincipal;
    property valorMulta: Double read fvalorMulta write fvalorMulta;
    property valorJuroEncargo: Double read fvalorJuroEncargo write fvalorJuroEncargo;
    property cpfCnpjContribuinte: Int64 read fcpfCnpjContribuinte write fcpfCnpjContribuinte;
    property numeroDocumentoDebito: Integer read fnumeroDocumentoDebito write fnumeroDocumentoDebito;
    property textoDescricao: String read ftextoDescricao write ftextoDescricao;
  end;

  { TACBrPagamentosBBPagamentosDARFPreto }

  TACBrPagamentosBBPagamentosDARFPreto = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBPagamentoDARFPreto;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBPagamentoDARFPreto);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentosBBPagamentoDARFPreto): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentosBBPagamentoDARFPreto);
    function New: TACBrPagamentosBBPagamentoDARFPreto;
    property Items[aIndex: Integer]: TACBrPagamentosBBPagamentoDARFPreto read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBLancamentoDARFPreto }

  TACBrPagamentosBBLancamentoDARFPreto = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLancamentoDARFPreto);

    property nomeConvenente: String read fnomeConvenente write fnomeConvenente;
    property numeroReferencia: String read fnumeroReferencia write fnumeroReferencia;
    property dataVencimento: TDateTime read fdataVencimento write fdataVencimento;
    property pagamento: TACBrPagamentosBBPagamentosDARFPreto read fpagamento write fpagamento;
    property indicadorMovimentoAceito: String read findicadorMovimentoAceito write findicadorMovimentoAceito;
    property erros: TACBrPagamentosBBLancamentoErros read ferros write ferros;
    property codigoClienteContrato: Integer read fcodigoClienteContrato write fcodigoClienteContrato;
    property codigoDocumento: String read fcodigoDocumento write fcodigoDocumento;
    property codigoReceitaTributo: Integer read fcodigoReceitaTributo write fcodigoReceitaTributo;
    property codigoIdentificadorTributo: String read fcodigoIdentificadorTributo write fcodigoIdentificadorTributo;
    property codigoTipoContribuinte: TACBrPagamentosBBTipoContribuinte read fcodigoTipoContribuinte write fcodigoTipoContribuinte;
    property dataApuracao: TDateTime read fdataApuracao write fdataApuracao;
  end;

  { TACBrPagamentosBBLancamentosDARFPreto }

  TACBrPagamentosBBLancamentosDARFPreto = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBLancamentoDARFPreto;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBLancamentoDARFPreto);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentosBBLancamentoDARFPreto): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentosBBLancamentoDARFPreto);
    function New: TACBrPagamentosBBLancamentoDARFPreto;
    property Items[aIndex: Integer]: TACBrPagamentosBBLancamentoDARFPreto read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBConsultaLoteRespostaDARFPreto }

  TACBrPagamentosBBConsultaLoteRespostaDARFPreto = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBConsultaLoteRespostaDARFPreto);

    property id: Integer read fid write fid;
    property codigoEstado: TACBrPagamentosBBEstadoRequisicao read fcodigoEstado write fcodigoEstado;
    property quantidadeLancamentos: Integer read fquantidadeLancamentos write fquantidadeLancamentos;
    property valorLancamentos: Double read fvalorLancamentos write fvalorLancamentos;
    property quantidadeLancamentosValidos: Integer read fquantidadeLancamentosValidos write fquantidadeLancamentosValidos;
    property valorLancamentosValidos: Double read fvalorLancamentosValidos write fvalorLancamentosValidos;
    property lancamentos: TACBrPagamentosBBLancamentosDARFPreto read flancamentos write flancamentos;
  end;

  { TACBrPagamentosBBRequisicaoDARFPreto }

  TACBrPagamentosBBRequisicaoDARFPreto = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBRequisicaoDARFPreto);

    property dataPagamento: TDateTime read fdataPagamento write fdataPagamento;
    property valorPagamento: Double read fvalorPagamento write fvalorPagamento;
    property numeroDocumentoDebito: Integer read fnumeroDocumentoDebito write fnumeroDocumentoDebito;
    property codigoSeuDocumento: String read fcodigoSeuDocumento write fcodigoSeuDocumento;
    property textoDescricaoPagamento: String read ftextoDescricaoPagamento write ftextoDescricaoPagamento;
    property codigoReceitaTributo: Integer read fcodigoReceitaTributo write fcodigoReceitaTributo
    property codigoTipoContribuinte: TACBrPagamentosBBTipoContribuinte read fcodigoTipoContribuinte write fcodigoTipoContribuinte;
    property numeroIdentificacaoContribuinte: Integer read fnumeroIdentificacaoContribuinte write fnumeroIdentificacaoContribuinte;
    property codigoIdentificadorTributo: String read fcodigoIdentificadorTributo write fcodigoIdentificadorTributo;
    property dataApuracao: TDateTime read fdataApuracao write fdataApuracao;
    property numeroReferencia: Int64 read fnumeroReferencia write fnumeroReferencia;
    property valorPrincipal: Double read fvalorPrincipal write fvalorPrincipal;
    property valorMulta: Double read fvalorMulta write fvalorMulta;
    property valorJuroEncargo: Double read fvalorJuroEncargo write fvalorJuroEncargo;
    property dataVencimento: TDateTime read fdataVencimento write fdataVencimento;
  end;

  { TACBrPagamentosBBRequisicaoDARFPretoLista }

  TACBrPagamentosBBRequisicaoDARFPretoLista = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBRequisicaoDARFPreto;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBRequisicaoDARFPreto);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentosBBRequisicaoDARFPreto): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentosBBRequisicaoDARFPreto);
    function New: TACBrPagamentosBBRequisicaoDARFPreto;
    property Items[aIndex: Integer]: TACBrPagamentosBBRequisicaoDARFPreto read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBLotePagamentosRequisicaoDARFPreto }

  TACBrPagamentosBBLotePagamentosRequisicaoDARFPreto = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLotePagamentosRequisicaoDARFPreto);

    property id: Int64 read fid write fid;
    property codigoContrato: Int64 read fcodigoContrato write fcodigoContrato;
    property numeroAgenciaDebito: Integer read fnumeroAgenciaDebito write fnumeroAgenciaDebito;
    property numeroContaCorrenteDebito: Int64 read fnumeroContaCorrenteDebito write fnumeroContaCorrenteDebito;
    property digitoVerificadorContaCorrenteDebito: String read fdigitoVerificadorContaCorrenteDebito write fdigitoVerificadorContaCorrenteDebito;
    property lancamentos: TACBrPagamentosBBRequisicaoDARFPretoLista read flancamentos write flancamentos;
  end;

implementation

end.

