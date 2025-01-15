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

unit ACBrSchemasPagamentosAPIBBCodigoBarra;

interface

uses
  Classes, SysUtils, ACBrAPIBase, ACBrJSON,
  ACBrSchemasPagamentosAPIBB;

type

  { TACBrPagamentosBBCodigoBarra }

  TACBrPagamentosBBCodigoBarra = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBCodigoBarra);

    property codigo: String read fcodigo write fcodigo;
    property nomeRecebedor: String read fnomeRecebedor write fnomeRecebedor;
    property seuNumero: String read fseuNumero write fseuNumero;
    property texto: String read ftexto write ftexto;
  end;

  { TACBrPagamentosBBCodigoBarraLista }

  TACBrPagamentosBBCodigoBarraLista = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBCodigoBarra;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBCodigoBarra);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentosBBCodigoBarra): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentosBBCodigoBarra);
    function New: TACBrPagamentosBBCodigoBarra;
    property Items[aIndex: Integer]: TACBrPagamentosBBCodigoBarra read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBConsultaRespostaCodigoBarra }

  TACBrPagamentosBBConsultaRespostaCodigoBarra = class(TACBrPagamentosBBConsultaRespostaBase)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBConsultaRespostaCodigoBarra);

    property listaPagamentos: TACBrPagamentosBBCodigoBarraLista read flistaPagamentos write flistaPagamentos;
    property listaDevolucao: TACBrPagamentosBBDevolucaoListaBase read flistaDevolucao write flistaDevolucao;
  end;

  { TACBrPagamentosBBPagamentoCodigoBarra }

  TACBrPagamentosBBPagamentoCodigoBarra = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBPagamentoCodigoBarra);

    property codigoPagamento: String read fcodigoPagamento write fcodigoPagamento;
    property nomeBeneficiario: String read fnomeBeneficiario write fnomeBeneficiario;
    property codigoBarras: String read fcodigoBarras write fcodigoBarras;
    property dataPagamento: TDateTime read fdataPagamento write fdataPagamento;
    property valorPagamento: Double read fvalorPagamento write fvalorPagamento;
    property documentoDebito: Double read fdocumentoDebito write fdocumentoDebito;
    property codigoSeuDocumento: String read fcodigoSeuDocumento write fcodigoSeuDocumento;
    property descricaoPagamento: String read fdescricaoPagamento write fdescricaoPagamento;
    property indicadorAceite: String read findicadorAceite write findicadorAceite;
    property erros: TACBrPagamenosBBTransferenciaErros read ferros write ferros;
  end;

  { TACBrPagamentosBBPagamentosCodigoBarra }

  TACBrPagamentosBBPagamentosCodigoBarra = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBPagamentoCodigoBarra;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBPagamentoCodigoBarra);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentosBBPagamentoCodigoBarra): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentosBBPagamentoCodigoBarra);
    function New: TACBrPagamentosBBPagamentoCodigoBarra;
    property Items[aIndex: Integer]: TACBrPagamentosBBPagamentoCodigoBarra read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBConsultaLoteRespostaCodigoBarra }

  TACBrPagamentosBBConsultaLoteRespostaCodigoBarra = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBConsultaLoteRespostaCodigoBarra);

    property numeroRequisicao: Integer read fnumeroRequisicao write fnumeroRequisicao;
    property estadoRequisicao: TACBrPagamentosBBEstadoRequisicao read festadoRequisicao write festadoRequisicao;
    property quantidadePagamentos: Integer read fquantidadePagamentos write fquantidadePagamentos;
    property valorPagamentos: Double read fvalorPagamentos write fvalorPagamentos;
    property quantidadePagamentosValidos: Integer read fquantidadePagamentosValidos write fquantidadePagamentosValidos;
    property valorPagamentosValidos: Double read fvalorPagamentosValidos write fvalorPagamentosValidos;
    property pagamentos: TACBrPagamentosBBPagamentosCodigoBarra read fpagamentos write fpagamentos;
  end;

  { TACBrPagamentosBBRequisicaoCodigoBarras }

  TACBrPagamentosBBRequisicaoCodigoBarras = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBRequisicaoCodigoBarras);

    property codigoBarras: String read fcodigoBarras write fcodigoBarras;
    property dataPagamento: TDateTime read fdataPagamento write fdataPagamento;
    property valorPagamento: Double read fvalorPagamento write fvalorPagamento;
    property numeroDocumentoDebito: Int64 read fnumeroDocumentoDebito write fnumeroDocumentoDebito;
    property codigoSeuDocumento: String read fcodigoSeuDocumento write fcodigoSeuDocumento;
    property descricaoPagamento: String read fdescricaoPagamento write fdescricaoPagamento;
  end;

  { TACBrPagamentosBBRequisicaoCodigoBarrasLista }

  TACBrPagamentosBBRequisicaoCodigoBarrasLista = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBRequisicaoCodigoBarras;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBRequisicaoCodigoBarras);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentosBBRequisicaoCodigoBarras): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentosBBRequisicaoCodigoBarras);
    function New: TACBrPagamentosBBRequisicaoCodigoBarras;
    property Items[aIndex: Integer]: TACBrPagamentosBBRequisicaoCodigoBarras read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBLotePagamentosRequisicaoCodigoBarras }

  TACBrPagamentosBBLotePagamentosRequisicaoCodigoBarras = class(TACBrPagamentoBBLotePagamentosRequisicao)
  private
  protected
    procedure AssignSchema(aSource: TACBrPagamentoBBLotePagamentosRequisicao); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLotePagamentosRequisicaoCodigoBarras);

    property lancamentos: TACBrPagamentosBBRequisicaoCodigoBarrasLista read flancamentos write flancamentos;
  end;

  { TACBrPagamentosBBVinculadoCodigoBarras }
  TACBrPagamentosBBVinculadoCodigoBarras = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBVinculadoCodigoBarras);

    property identificadorPagamento: Int64 read fidentificadorPagamento write fidentificadorPagamento;
    property autenticacaoPagamento: String read fautenticacaoPagamento write fautenticacaoPagamento;
    property agenciaDebito: Integer read fagenciaDebito write fagenciaDebito;
    property contaCorrenteDebito: Integer read fcontaCorrenteDebito write fcontaCorrenteDebito;
    property digitoVerificadorContaCorrente: String read fdigitoVerificadorContaCorrente write fdigitoVerificadorContaCorrente;
    property numeroRequisicao: Integer read fnumeroRequisicao write fnumeroRequisicao;
    property documentoDebito: Int64 read fdocumentoDebito write fdocumentoDebito;
    property dataAgendamento: TDateTime read fdataAgendamento write fdataAgendamento;
    property dataPagamento: TDateTime read fdataPagamento write fdataPagamento;
    property valorPagamento: Double read fvalorPagamento write fvalorPagamento;
    property nomeBeneficiario: String read fnomeBeneficiario write fnomeBeneficiario;
    property estadoPagamento: String read festadoPagamento write festadoPagamento;
    property descricaoPagamento: String read fdescricaoPagamento write fdescricaoPagamento;
    property erros: TACBrPagamentosBBVinculadosErros read ferros write ferros;
  end;

  { TACBrPagamentosBBVinculadoCodigoBarrasLista }

  TACBrPagamentosBBVinculadoCodigoBarrasLista = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBVinculadoCodigoBarras;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBVinculadoCodigoBarras);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentosBBVinculadoCodigoBarras): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentosBBVinculadoCodigoBarras);
    function New: TACBrPagamentosBBVinculadoCodigoBarras;
    property Items[aIndex: Integer]: TACBrPagamentosBBVinculadoCodigoBarras read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBVinculadosCodigoBarrasResposta }
  TACBrPagamentosBBVinculadosCodigoBarrasResposta = class(TACBrAPISchema)
  private
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBVinculadosCodigoBarrasResposta);

    property pagamentos: TACBrPagamentosBBVinculadoCodigoBarrasLista read f write f;
  end;

implementation

end.

