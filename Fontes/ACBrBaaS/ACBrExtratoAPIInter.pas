{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César Vieira                                                         }
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

(*

  Documentação
  https://apoio.developers.bb.com.br/referency/post/647f6ca4dcefbe0012888655

*)   

{$I ACBr.inc}

unit ACBrExtratoAPIInter;

interface

uses
  Classes, SysUtils, ACBrAPIBase, ACBrJSON, ACBrExtratoAPI;

const
  cExtratoInterURLSandbox = 'https://cdpj-sandbox.partners.uatinter.co';
  cExtratoInterURLProducao = 'https://cdpj.partners.bancointer.com.br';
  cExtratoInterEndPointAuth = '/oauth/v2/token';
  cExtratoInterEndPointExtrato = '/banking/v2/extrato/completo';

type

  TACBrExtratoInterTipoTransacao = (
    ittNenhum,
    ittDebitoEmConta,
    ittDepositoBoleto,
    ittAntecipacaoRecebiveis,
    ittAntecipacaoRecebiveisCartao,
    ittBoletoCobranca,
    ittCambio,
    ittCashBack,
    ittCheque,
    ittEstorno,
    ittDomicilioCartao,
    ittFinanciamento,
    ittImposto,
    ittInterpag,
    ittInvestimento,
    ittJuros,
    ittMaquininhaGranito,
    ittMulta,
    ittOutros,
    ittPagamento,
    ittPix,
    ittProventos,
    ittSaque,
    ittCompraDebito,
    ittDebitoAutomatico,
    ittTarifa,
    ittTransferencia
  );

  { TACBrExtratoInterTransacaoDetalhesPix }

  TACBrExtratoInterTransacaoDetalhesPix = class(TACBrAPISchema)
  private
    fagenciaRecebedor: String;
    fchavePixRecebedor: String;
    fcontaBancariaRecebedor: String;
    fcpfCnpjPagador: String;
    fcpfCnpjRecebedor: String;
    fdescricaoPix: String;
    fendToEndId: String;
    fnomeEmpresaPagador: String;
    fnomeEmpresaRecebedor: String;
    fnomePagador: String;
    fnomeRecebedor: String;
    forigemMovimentacao: String;
    ftipoDetalhe: String;
    ftxId: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrExtratoInterTransacaoDetalhesPix);

    property txId: String read ftxId write ftxId;
    property nomePagador: String read fnomePagador write fnomePagador;
    property descricaoPix: String read fdescricaoPix write fdescricaoPix;
    property cpfCnpjPagador: String read fcpfCnpjPagador write fcpfCnpjPagador;
    property contaBancariaRecebedor: String read fcontaBancariaRecebedor write fcontaBancariaRecebedor;
    property nomeEmpresaPagador: String read fnomeEmpresaPagador write fnomeEmpresaPagador;
    property tipoDetalhe: String read ftipoDetalhe write ftipoDetalhe;
    property endToEndId: String read fendToEndId write fendToEndId;
    property chavePixRecebedor: String read fchavePixRecebedor write fchavePixRecebedor;
    property nomeEmpresaRecebedor: String read fnomeEmpresaRecebedor write fnomeEmpresaRecebedor;
    property nomeRecebedor: String read fnomeRecebedor write fnomeRecebedor;
    property agenciaRecebedor: String read fagenciaRecebedor write fagenciaRecebedor;
    property cpfCnpjRecebedor: String read fcpfCnpjRecebedor write fcpfCnpjRecebedor;
    property origemMovimentacao: String read forigemMovimentacao write forigemMovimentacao;
  end;

  { TACBrExtratoInterTransacaoDetalhesBoletoCobranca }

  TACBrExtratoInterTransacaoDetalhesBoletoCobranca = class(TACBrAPISchema)
  private
    fabatimento: String;
    fcodBarras: String;
    fcpfCnpj: String;
    fdataEmissao: String;
    fdataLimite: String;
    fdataTransacao: String;
    fdataVencimento: String;
    fdesconto1: String;
    fdesconto2: String;
    fdesconto3: String;
    fjuros: String;
    fmulta: String;
    fnome: String;
    fnossoNumero: String;
    fseuNumero: String;
    ftipoDetalhe: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrExtratoInterTransacaoDetalhesBoletoCobranca);

    property dataVencimento: String read fdataVencimento write fdataVencimento;
    property dataTransacao: String read fdataTransacao write fdataTransacao;
    property nossoNumero: String read fnossoNumero write fnossoNumero;
    property seuNumero: String read fseuNumero write fseuNumero;
    property codBarras: String read fcodBarras write fcodBarras;
    property juros: String read fjuros write fjuros;
    property multa: String read fmulta write fmulta;
    property desconto1: String read fdesconto1 write fdesconto1;
    property desconto2: String read fdesconto2 write fdesconto2;
    property desconto3: String read fdesconto3 write fdesconto3;
    property nome: String read fnome write fnome;
    property dataLimite: String read fdataLimite write fdataLimite;
    property tipoDetalhe: String read ftipoDetalhe write ftipoDetalhe;
    property cpfCnpj: String read fcpfCnpj write fcpfCnpj;
    property dataEmissao: String read fdataEmissao write fdataEmissao;
    property abatimento: String read fabatimento write fabatimento;
  end;

  { TACBrExtratoInterTransacaoDetalhesCashback }

  TACBrExtratoInterTransacaoDetalhesCashback = class(TACBrAPISchema)
  private
    fproduto: String;
    ftipoDetalhe: String;
    fvalorCompra: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrExtratoInterTransacaoDetalhesCashback);

    property valorCompra: String read fvalorCompra write fvalorCompra;
    property produto: String read fproduto write fproduto;
    property tipoDetalhe: String read ftipoDetalhe write ftipoDetalhe;
  end;

  { TACBrExtratoInterTransacaoDetalhesCheque }

  TACBrExtratoInterTransacaoDetalhesCheque = class(TACBrAPISchema)
  private
    fagencia: String;
    fcodigoAfiliado: String;
    fcontaBancaria: String;
    fdataRetorno: String;
    fdescricaoChequeBancario: String;
    fmotivoRetorno: String;
    fnomeEmpresa: String;
    fnumeroChequeBancario: String;
    ftipoDetalhe: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrExtratoInterTransacaoDetalhesCheque);

    property agencia: String read fagencia write fagencia;
    property numeroChequeBancario: String read fnumeroChequeBancario write fnumeroChequeBancario;
    property contaBancaria: String read fcontaBancaria write fcontaBancaria;
    property dataRetorno: String read fdataRetorno write fdataRetorno;
    property motivoRetorno: String read fmotivoRetorno write fmotivoRetorno;
    property descricaoChequeBancario: String read fdescricaoChequeBancario write fdescricaoChequeBancario;
    property nomeEmpresa: String read fnomeEmpresa write fnomeEmpresa;
    property tipoDetalhe: String read ftipoDetalhe write ftipoDetalhe;
    property codigoAfiliado: String read fcodigoAfiliado write fcodigoAfiliado;
  end;

  { TACBrExtratoInterTransacaoDetalhesCompraDebito }

  TACBrExtratoInterTransacaoDetalhesCompraDebito = class(TACBrAPISchema)
  private
    festabelecimento: String;
    ftipoDetalhe: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrExtratoInterTransacaoDetalhesCompraDebito);

    property estabelecimento: String read festabelecimento write festabelecimento;
    property tipoDetalhe: String read ftipoDetalhe write ftipoDetalhe;
  end;

  { TACBrExtratoInterTransacaoDetalhesDepositoBoleto }

  TACBrExtratoInterTransacaoDetalhesDepositoBoleto = class(TACBrAPISchema)
  private
    fcodBarras: String;
    fdataEmissao: String;
    fdataVencimento: String;
    fnossoNumero: String;
    ftipoDetalhe: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrExtratoInterTransacaoDetalhesDepositoBoleto);

    property dataVencimento: String read fdataVencimento write fdataVencimento;
    property tipoDetalhe: String read ftipoDetalhe write ftipoDetalhe;
    property dataEmissao: String read fdataEmissao write fdataEmissao;
    property nossoNumero: String read fnossoNumero write fnossoNumero;
    property codBarras: String read fcodBarras write fcodBarras;
  end;

  { TACBrExtratoInterTransacaoDetalhesTransferencia }

  TACBrExtratoInterTransacaoDetalhesTransferencia = class(TACBrAPISchema)
  private
    fagenciaPagador: String;
    fagenciaRecebedor: String;
    fbancoRecebedor: String;
    fcontaBancariaPagador: String;
    fcontaBancariaRecebedor: String;
    fcpfCnpjPagador: String;
    fcpfCnpjRecebedor: String;
    fdataEfetivacao: String;
    fdescricaoTransferencia: String;
    fidTransferencia: String;
    fnomeEmpresaPagador: String;
    fnomePagador: String;
    fnomeRecebedor: String;
    ftipoDetalhe: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrExtratoInterTransacaoDetalhesTransferencia);

    property contaBancariaPagador: String read fcontaBancariaPagador write fcontaBancariaPagador;
    property descricaoTransferencia: String read fdescricaoTransferencia write fdescricaoTransferencia;
    property agenciaPagador: String read fagenciaPagador write fagenciaPagador;
    property bancoRecebedor: String read fbancoRecebedor write fbancoRecebedor;
    property contaBancariaRecebedor: String read fcontaBancariaRecebedor write fcontaBancariaRecebedor;
    property cpfCnpjRecebedor: String read fcpfCnpjRecebedor write fcpfCnpjRecebedor;
    property cpfCnpjPagador: String read fcpfCnpjPagador write fcpfCnpjPagador;
    property nomePagador: String read fnomePagador write fnomePagador;
    property nomeEmpresaPagador: String read fnomeEmpresaPagador write fnomeEmpresaPagador;
    property nomeRecebedor: String read fnomeRecebedor write fnomeRecebedor;
    property tipoDetalhe: String read ftipoDetalhe write ftipoDetalhe;
    property idTransferencia: String read fidTransferencia write fidTransferencia;
    property agenciaRecebedor: String read fagenciaRecebedor write fagenciaRecebedor;
    property dataEfetivacao: String read fdataEfetivacao write fdataEfetivacao;
  end;

  { TACBrExtratoInterTransacaoDetalhesPagamento }

  TACBrExtratoInterTransacaoDetalhesPagamento = class(TACBrAPISchema)
  private
    fadicionado: String;
    fagencia: String;
    fautenticacao: String;
    fcodBarras: String;
    fcodigoAfiliado: String;
    fcodigoReceita: String;
    fcontaBancaria: String;
    fcpfCnpj: String;
    fdataVencimento: String;
    fdesconto: String;
    fdetalheDescricao: String;
    fempresaEmissora: String;
    fempresaOrigem: String;
    fhora: String;
    fjuros: String;
    flinhaDigitavel: String;
    fmulta: String;
    fnomeDestinatario: String;
    fnomeOrigem: String;
    fperiodoApuracao: String;
    ftipoDetalhe: String;
    fvalorAumentado: String;
    fvalorOriginal: String;
    fvalorParcial: String;
    fvalorPrincipal: String;
    fvalorTotal: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrExtratoInterTransacaoDetalhesPagamento);

    property valorTotal: String read fvalorTotal write fvalorTotal;
    property detalheDescricao: String read fdetalheDescricao write fdetalheDescricao;
    property contaBancaria: String read fcontaBancaria write fcontaBancaria;
    property agencia: String read fagencia write fagencia;
    property adicionado: String read fadicionado write fadicionado;
    property dataVencimento: String read fdataVencimento write fdataVencimento;
    property codigoAfiliado: String read fcodigoAfiliado write fcodigoAfiliado;
    property empresaEmissora: String read fempresaEmissora write fempresaEmissora;
    property valorOriginal: String read fvalorOriginal write fvalorOriginal;
    property desconto: String read fdesconto write fdesconto;
    property cpfCnpj: String read fcpfCnpj write fcpfCnpj;
    property valorPrincipal: String read fvalorPrincipal write fvalorPrincipal;
    property periodoApuracao: String read fperiodoApuracao write fperiodoApuracao;
    property valorAumentado: String read fvalorAumentado write fvalorAumentado;
    property codBarras: String read fcodBarras write fcodBarras;
    property valorParcial: String read fvalorParcial write fvalorParcial;
    property hora: String read fhora write fhora;
    property juros: String read fjuros write fjuros;
    property multa: String read fmulta write fmulta;
    property empresaOrigem: String read fempresaOrigem write fempresaOrigem;
    property nomeDestinatario: String read fnomeDestinatario write fnomeDestinatario;
    property tipoDetalhe: String read ftipoDetalhe write ftipoDetalhe;
    property nomeOrigem: String read fnomeOrigem write fnomeOrigem;
    property codigoReceita: String read fcodigoReceita write fcodigoReceita;
    property linhaDigitavel: String read flinhaDigitavel write flinhaDigitavel;
    property autenticacao: String read fautenticacao write fautenticacao;
  end;

  { TACBrExtratoInterTransacaoDetalhesTarifa }

  TACBrExtratoInterTransacaoDetalhesTarifa = class(TACBrAPISchema)
  private
    fcodBarras: String;
    fdataEmissao: String;
    fdataTransacao: String;
    fdataVencimento: String;
    fendToEndId: String;
    fnossoNumero: String;
    fseuNumero: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrExtratoInterTransacaoDetalhesTarifa);

    property dataVencimento: String read fdataVencimento write fdataVencimento;
    property dataEmissao: String read fdataEmissao write fdataEmissao;
    property dataTransacao: String read fdataTransacao write fdataTransacao;
    property nossoNumero: String read fnossoNumero write fnossoNumero;
    property seuNumero: String read fseuNumero write fseuNumero;
    property codBarras: String read fcodBarras write fcodBarras;
    property endToEndId: String read fendToEndId write fendToEndId;
  end;

  { TACBrExtratoInterTransacao }

  TACBrExtratoInterTransacao = class(TACBrAPISchema)
  private
    fvalor: Double;
    ftitulo: String;
    fdescricao: String;
    fidTransacao: String;
    fdataInclusao: TDateTime;
    fdataTransacao: TDateTime;
    fpix: TACBrExtratoInterTransacaoDetalhesPix;
    ftipoOperacao: TACBrExtratoAPITipoOperacao;
    ftipoTransacao: TACBrExtratoInterTipoTransacao;
    fcheque: TACBrExtratoInterTransacaoDetalhesCheque;
    ftarifa: TACBrExtratoInterTransacaoDetalhesTarifa;
    fcashback: TACBrExtratoInterTransacaoDetalhesCashback;
    fpagamento: TACBrExtratoInterTransacaoDetalhesPagamento;
    fcompraDebito: TACBrExtratoInterTransacaoDetalhesCompraDebito;
    ftransferencia: TACBrExtratoInterTransacaoDetalhesTransferencia;
    fdepositoBoleto: TACBrExtratoInterTransacaoDetalhesDepositoBoleto;
    fboletoCobranca: TACBrExtratoInterTransacaoDetalhesBoletoCobranca;

    function GetboletoCobranca: TACBrExtratoInterTransacaoDetalhesBoletoCobranca;
    function Getcashback: TACBrExtratoInterTransacaoDetalhesCashback;
    function Getcheque: TACBrExtratoInterTransacaoDetalhesCheque;
    function GetcompraDebito: TACBrExtratoInterTransacaoDetalhesCompraDebito;
    function GetdepositoBoleto: TACBrExtratoInterTransacaoDetalhesDepositoBoleto;
    function Getpagamento: TACBrExtratoInterTransacaoDetalhesPagamento;
    function Getpix: TACBrExtratoInterTransacaoDetalhesPix;
    function Gettarifa: TACBrExtratoInterTransacaoDetalhesTarifa;
    function Gettransferencia: TACBrExtratoInterTransacaoDetalhesTransferencia;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrExtratoInterTransacao);

    property idTransacao: String read fidTransacao write fidTransacao;
    property dataInclusao: TDateTime read fdataInclusao write fdataInclusao;
    property dataTransacao: TDateTime read fdataTransacao write fdataTransacao;
    property tipoTransacao: TACBrExtratoInterTipoTransacao read ftipoTransacao write ftipoTransacao;
    property tipoOperacao: TACBrExtratoAPITipoOperacao read ftipoOperacao write ftipoOperacao;
    property valor: Double read fvalor write fvalor;
    property titulo: String read ftitulo write ftitulo;
    property descricao: String read fdescricao write fdescricao;

    property pix: TACBrExtratoInterTransacaoDetalhesPix read Getpix;
    property boletoCobranca: TACBrExtratoInterTransacaoDetalhesBoletoCobranca read GetboletoCobranca;
    property cashback: TACBrExtratoInterTransacaoDetalhesCashback read Getcashback;
    property cheque: TACBrExtratoInterTransacaoDetalhesCheque read Getcheque;
    property compraDebito: TACBrExtratoInterTransacaoDetalhesCompraDebito read GetcompraDebito;
    property depositoBoleto: TACBrExtratoInterTransacaoDetalhesDepositoBoleto read GetdepositoBoleto;
    property transferencia: TACBrExtratoInterTransacaoDetalhesTransferencia read Gettransferencia;
    property pagamento: TACBrExtratoInterTransacaoDetalhesPagamento read Getpagamento;
    property tarifa: TACBrExtratoInterTransacaoDetalhesTarifa read Gettarifa;
  end;

  { TACBrExtratoInterTransacoes }

  TACBrExtratoInterTransacoes = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrExtratoInterTransacao;
    procedure SetItem(aIndex: Integer; aValue: TACBrExtratoInterTransacao);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrExtratoInterTransacao): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrExtratoInterTransacao);
    function New: TACBrExtratoInterTransacao;
    property Items[aIndex: Integer]: TACBrExtratoInterTransacao read GetItem write SetItem; default;
  end;

  { TACBrExtratoInterResult }

  TACBrExtratoInterResult = class(TACBrExtratoConsultado)
  private
    fnumeroDeElementos: Integer;
    fprimeiraPagina: Boolean;
    ftamanhoPagina: Integer;
    ftotalElementos: Integer;
    ftotalPaginas: Integer;
    ftransacoes: TACBrExtratoInterTransacoes;
    fultimaPagina: Boolean;
    function Gettransacoes: TACBrExtratoInterTransacoes;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
    procedure ConverterParaExtratoConsultado;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrExtratoInterResult);
    
    property totalPaginas: Integer read ftotalPaginas write ftotalPaginas;
    property totalElementos: Integer read ftotalElementos write ftotalElementos;
    property ultimaPagina: Boolean read fultimaPagina write fultimaPagina;
    property primeiraPagina: Boolean read fprimeiraPagina write fprimeiraPagina;
    property tamanhoPagina: Integer read ftamanhoPagina write ftamanhoPagina;
    property numeroDeElementos: Integer read fnumeroDeElementos write fnumeroDeElementos;
    property transacoes: TACBrExtratoInterTransacoes read Gettransacoes;
  end;

  { TACBrExtratoInterErro }

  TACBrExtratoInterErro = class(TACBrExtratoErro)
  private
    ftype: String;
    fdetail: String;
    ftimestamp: String;
    ftitle: String;
    fmessage: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrExtratoInterErro);

    property type_: String read ftype write ftype;
    property title: String read ftitle write ftitle;
    property detail: String read fdetail write fdetail;
    property message: String read fmessage write fmessage;
    property timestamp: String read ftimestamp write ftimestamp;
  end;

  { TACBrExtratoAPIInter }

  TACBrExtratoAPIInter = class(TACBrExtratoAPIBancoClass)
  protected
    function GetRespostaErro: TACBrExtratoErro; override;
    function GetExtratoConsultado: TACBrExtratoConsultado; override;

    function CalcularURL: String; override;
    procedure Autenticar; override;
  public
    function ConsultarExtrato(
      const aAgencia, aConta: String;
      const aDataInicio: TDateTime = 0;
      const aDataFim: TDateTime = 0;
      const aPagina: Integer = 0;
      const aRegistrosPorPag: Integer = 0): Boolean; override;
  end;

  function TipoTransacaoToString(const aTipo: TACBrExtratoInterTipoTransacao): String;
  function StringToTipoTransacao(const aStr: String): TACBrExtratoInterTipoTransacao;

implementation

uses
  synautil, synacode, DateUtils,
  StrUtils,
  ACBrSocket,
  ACBrUtil.DateTime,
  ACBrUtil.Strings,
  ACBrUtil.Base;

function TipoTransacaoToString(const aTipo: TACBrExtratoInterTipoTransacao): String;
begin
  Result := EmptyStr;
  case aTipo of
    ittDebitoEmConta: Result := 'DEBITO_EM_CONTA';
    ittDepositoBoleto: Result := 'DEPOSITO_BOLETO';
    ittAntecipacaoRecebiveis: Result := 'ANTECIPACAO_RECEBIVEIS';
    ittAntecipacaoRecebiveisCartao: Result := 'ANTECIPACAO_RECEBIVEIS_CARTAO';
    ittBoletoCobranca: Result := 'BOLETO_COBRANCA';
    ittCambio: Result := 'CAMBIO';
    ittCashBack: Result := 'CASHBACK';
    ittCheque: Result := 'CHEQUE';
    ittEstorno: Result := 'ESTORNO';
    ittDomicilioCartao: Result := 'DOMICILIO_CARTAO';
    ittFinanciamento: Result := 'FINANCIAMENTO';
    ittImposto: Result := 'IMPOSTO';
    ittInterpag: Result := 'INTERPAG';
    ittInvestimento: Result := 'INVESTIMENTO';
    ittJuros: Result := 'JUROS';
    ittMaquininhaGranito: Result := 'MAQUININHA_GRANITO';
    ittMulta: Result := 'MULTA';
    ittOutros: Result := 'OUTROS';
    ittPagamento: Result := 'PAGAMENTO';
    ittPix: Result := 'PIX';
    ittProventos: Result := 'PROVENTOS';
    ittSaque: Result := 'SAQUE';
    ittCompraDebito: Result := 'COMPRA_DEBITO';
    ittDebitoAutomatico: Result := 'DEBITO_AUTOMATICO';
    ittTarifa: Result := 'TARIFA';
    ittTransferencia: Result := 'TRANSFERENCIA';
  end;
end;

function StringToTipoTransacao(const aStr: String): TACBrExtratoInterTipoTransacao;
var
  s: String;
begin
  s := UpperCase(aStr);
  Result := ittNenhum;
  if (s = 'DEBITO_EM_CONTA') then
    Result := ittDebitoEmConta
  else if (s = 'DEPOSITO_BOLETO') then
    Result := ittDepositoBoleto
  else if (s = 'ANTECIPACAO_RECEBIVEIS') then
    Result := ittAntecipacaoRecebiveis
  else if (s = 'ANTECIPACAO_RECEBIVEIS_CARTAO') then
    Result := ittAntecipacaoRecebiveisCartao
  else if (s = 'BOLETO_COBRANCA') then
    Result := ittBoletoCobranca
  else if (s = 'CAMBIO') then
    Result := ittCambio
  else if (s = 'CASHBACK') then
    Result := ittCashBack
  else if (s = 'CHEQUE') then
    Result := ittCheque
  else if (s = 'ESTORNO') then
    Result := ittEstorno
  else if (s = 'DOMICILIO_CARTAO') then
    Result := ittDomicilioCartao
  else if (s = 'FINANCIAMENTO') then
    Result := ittFinanciamento
  else if (s = 'IMPOSTO') then
    Result := ittImposto
  else if (s = 'INTERPAG') then
    Result := ittInterpag
  else if (s = 'INVESTIMENTO') then
    Result := ittInvestimento
  else if (s = 'JUROS') then
    Result := ittJuros
  else if (s = 'MAQUININHA_GRANITO') then
    Result := ittMaquininhaGranito
  else if (s = 'MULTA') then
    Result := ittMulta
  else if (s = 'OUTROS') then
    Result := ittOutros
  else if (s = 'PAGAMENTO') then
    Result := ittPagamento
  else if (s = 'PIX') then
    Result := ittPix
  else if (s = 'PROVENTOS') then
    Result := ittProventos
  else if (s = 'SAQUE') then
    Result := ittSaque
  else if (s = 'COMPRA_DEBITO') then
    Result := ittCompraDebito
  else if (s = 'DEBITO_AUTOMATICO') then
    Result := ittDebitoAutomatico
  else if (s = 'TARIFA') then
    Result := ittTarifa
  else if (s = 'TRANSFERENCIA') then
    Result := ittTransferencia;
end;

{ TACBrExtratoInterTransacaoDetalhesTarifa }

procedure TACBrExtratoInterTransacaoDetalhesTarifa.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrExtratoInterTransacaoDetalhesTarifa) then
    Assign(TACBrExtratoInterTransacaoDetalhesTarifa(aSource));
end;

procedure TACBrExtratoInterTransacaoDetalhesTarifa.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('codBarras', fcodBarras)
    .AddPair('dataEmissao', fdataEmissao)
    .AddPair('dataTransacao', fdataTransacao)
    .AddPair('dataVencimento', fdataVencimento)
    .AddPair('endToEndId', fendToEndId)
    .AddPair('nossoNumero', fnossoNumero)
    .AddPair('seuNumero', fseuNumero);
end;

procedure TACBrExtratoInterTransacaoDetalhesTarifa.DoReadFromJSon(aJSon: TACBrJSONObject);
begin 
  aJSon
    .Value('codBarras', fcodBarras)
    .Value('dataEmissao', fdataEmissao)
    .Value('dataTransacao', fdataTransacao)
    .Value('dataVencimento', fdataVencimento)
    .Value('endToEndId', fendToEndId)
    .Value('nossoNumero', fnossoNumero)
    .Value('seuNumero', fseuNumero);
end;

procedure TACBrExtratoInterTransacaoDetalhesTarifa.Clear;
begin
  fcodBarras := EmptyStr;
  fdataEmissao := EmptyStr;
  fdataTransacao := EmptyStr;
  fdataVencimento := EmptyStr;
  fendToEndId := EmptyStr;
  fnossoNumero := EmptyStr;
  fseuNumero := EmptyStr;
end;

function TACBrExtratoInterTransacaoDetalhesTarifa.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fcodBarras) and
    EstaVazio(fdataEmissao) and
    EstaVazio(fdataTransacao) and
    EstaVazio(fdataVencimento) and
    EstaVazio(fendToEndId) and
    EstaVazio(fnossoNumero) and
    EstaVazio(fseuNumero);
end;

procedure TACBrExtratoInterTransacaoDetalhesTarifa.Assign(
  aSource: TACBrExtratoInterTransacaoDetalhesTarifa);
begin
  fcodBarras := aSource.codBarras;
  fdataEmissao := aSource.dataEmissao;
  fdataTransacao := aSource.dataTransacao;
  fdataVencimento := aSource.dataVencimento;
  fendToEndId := aSource.endToEndId;
  fnossoNumero := aSource.nossoNumero;
  fseuNumero := aSource.seuNumero;
end;

{ TACBrExtratoInterTransacaoDetalhesPagamento }

procedure TACBrExtratoInterTransacaoDetalhesPagamento.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrExtratoInterTransacaoDetalhesPagamento) then
    Assign(TACBrExtratoInterTransacaoDetalhesPagamento(aSource));
end;

procedure TACBrExtratoInterTransacaoDetalhesPagamento.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('adicionado', fadicionado)
    .AddPair('agencia', fagencia)
    .AddPair('autenticacao', fautenticacao)
    .AddPair('codBarras', fcodBarras)
    .AddPair('codigoAfiliado', fcodigoAfiliado)
    .AddPair('codigoReceita', fcodigoReceita)
    .AddPair('contaBancaria', fcontaBancaria)
    .AddPair('cpfCnpj', fcpfCnpj)
    .AddPair('dataVencimento', fdataVencimento)
    .AddPair('desconto', fdesconto)
    .AddPair('detalheDescricao', fdetalheDescricao)
    .AddPair('empresaEmissora', fempresaEmissora)
    .AddPair('empresaOrigem', fempresaOrigem)
    .AddPair('hora', fhora)
    .AddPair('juros', fjuros)
    .AddPair('linhaDigitavel', flinhaDigitavel)
    .AddPair('multa', fmulta)
    .AddPair('nomeDestinatario', fnomeDestinatario)
    .AddPair('nomeOrigem', fnomeOrigem)
    .AddPair('periodoApuracao', fperiodoApuracao)
    .AddPair('tipoDetalhe', ftipoDetalhe)
    .AddPair('valorAumentado', fvalorAumentado)
    .AddPair('valorOriginal', fvalorOriginal)
    .AddPair('valorParcial', fvalorParcial)
    .AddPair('valorPrincipal', fvalorPrincipal)
    .AddPair('valorTotal', fvalorTotal);
end;

procedure TACBrExtratoInterTransacaoDetalhesPagamento.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('adicionado', fadicionado)
    .Value('agencia', fagencia)
    .Value('autenticacao', fautenticacao)
    .Value('codBarras', fcodBarras)
    .Value('codigoAfiliado', fcodigoAfiliado)
    .Value('codigoReceita', fcodigoReceita)
    .Value('contaBancaria', fcontaBancaria)
    .Value('cpfCnpj', fcpfCnpj)
    .Value('dataVencimento', fdataVencimento)
    .Value('desconto', fdesconto)
    .Value('detalheDescricao', fdetalheDescricao)
    .Value('empresaEmissora', fempresaEmissora)
    .Value('empresaOrigem', fempresaOrigem)
    .Value('hora', fhora)
    .Value('juros', fjuros)
    .Value('linhaDigitavel', flinhaDigitavel)
    .Value('multa', fmulta)
    .Value('nomeDestinatario', fnomeDestinatario)
    .Value('nomeOrigem', fnomeOrigem)
    .Value('periodoApuracao', fperiodoApuracao)
    .Value('tipoDetalhe', ftipoDetalhe)
    .Value('valorAumentado', fvalorAumentado)
    .Value('valorOriginal', fvalorOriginal)
    .Value('valorParcial', fvalorParcial)
    .Value('valorPrincipal', fvalorPrincipal)
    .Value('valorTotal', fvalorTotal);
end;

procedure TACBrExtratoInterTransacaoDetalhesPagamento.Clear;
begin
  fadicionado := EmptyStr;
  fagencia := EmptyStr;
  fautenticacao := EmptyStr;
  fcodBarras := EmptyStr;
  fcodigoAfiliado := EmptyStr;
  fcodigoReceita := EmptyStr;
  fcontaBancaria := EmptyStr;
  fcpfCnpj := EmptyStr;
  fdataVencimento := EmptyStr;
  fdesconto := EmptyStr;
  fdetalheDescricao := EmptyStr;
  fempresaEmissora := EmptyStr;
  fempresaOrigem := EmptyStr;
  fhora := EmptyStr;
  fjuros := EmptyStr;
  flinhaDigitavel := EmptyStr;
  fmulta := EmptyStr;
  fnomeDestinatario := EmptyStr;
  fnomeOrigem := EmptyStr;
  fperiodoApuracao := EmptyStr;
  ftipoDetalhe := EmptyStr;
  fvalorAumentado := EmptyStr;
  fvalorOriginal := EmptyStr;
  fvalorParcial := EmptyStr;
  fvalorPrincipal := EmptyStr;
  fvalorTotal := EmptyStr;
end;

function TACBrExtratoInterTransacaoDetalhesPagamento.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fadicionado) and
    EstaVazio(fagencia) and
    EstaVazio(fautenticacao) and
    EstaVazio(fcodBarras) and
    EstaVazio(fcodigoAfiliado) and
    EstaVazio(fcodigoReceita) and
    EstaVazio(fcontaBancaria) and
    EstaVazio(fcpfCnpj) and
    EstaVazio(fdataVencimento) and
    EstaVazio(fdesconto) and
    EstaVazio(fdetalheDescricao) and
    EstaVazio(fempresaEmissora) and
    EstaVazio(fempresaOrigem) and
    EstaVazio(fhora) and
    EstaVazio(fjuros) and
    EstaVazio(flinhaDigitavel) and
    EstaVazio(fmulta) and
    EstaVazio(fnomeDestinatario) and
    EstaVazio(fnomeOrigem) and
    EstaVazio(fperiodoApuracao) and
    EstaVazio(ftipoDetalhe) and
    EstaVazio(fvalorAumentado) and
    EstaVazio(fvalorOriginal) and
    EstaVazio(fvalorParcial) and
    EstaVazio(fvalorPrincipal) and
    EstaVazio(fvalorTotal);
end;

procedure TACBrExtratoInterTransacaoDetalhesPagamento.Assign(aSource: TACBrExtratoInterTransacaoDetalhesPagamento);
begin
  fadicionado := aSource.adicionado;
  fagencia := aSource.agencia;
  fautenticacao := aSource.autenticacao;
  fcodBarras := aSource.codBarras;
  fcodigoAfiliado := aSource.codigoAfiliado;
  fcodigoReceita := aSource.codigoReceita;
  fcontaBancaria := aSource.contaBancaria;
  fcpfCnpj := aSource.cpfCnpj;
  fdataVencimento := aSource.dataVencimento;
  fdesconto := aSource.desconto;
  fdetalheDescricao := aSource.detalheDescricao;
  fempresaEmissora := aSource.empresaEmissora;
  fempresaOrigem := aSource.empresaOrigem;
  fhora := aSource.hora;
  fjuros := aSource.juros;
  flinhaDigitavel := aSource.linhaDigitavel;
  fmulta := aSource.multa;
  fnomeDestinatario := aSource.nomeDestinatario;
  fnomeOrigem := aSource.nomeOrigem;
  fperiodoApuracao := aSource.periodoApuracao;
  ftipoDetalhe := aSource.tipoDetalhe;
  fvalorAumentado := aSource.valorAumentado;
  fvalorOriginal := aSource.valorOriginal;
  fvalorParcial := aSource.valorParcial;
  fvalorPrincipal := aSource.valorPrincipal;
  fvalorTotal := aSource.valorTotal;
end;

{ TACBrExtratoInterTransacaoDetalhesTransferencia }

procedure TACBrExtratoInterTransacaoDetalhesTransferencia.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrExtratoInterTransacaoDetalhesTransferencia) then
    Assign(TACBrExtratoInterTransacaoDetalhesTransferencia(aSource));
end;

procedure TACBrExtratoInterTransacaoDetalhesTransferencia.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('agenciaPagador', fagenciaPagador)
    .AddPair('agenciaRecebedor', fagenciaRecebedor)
    .AddPair('bancoRecebedor', fbancoRecebedor)
    .AddPair('contaBancariaPagador', fcontaBancariaPagador)
    .AddPair('contaBancariaRecebedor', fcontaBancariaRecebedor)
    .AddPair('cpfCnpjPagador', fcpfCnpjPagador)
    .AddPair('cpfCnpjRecebedor', fcpfCnpjRecebedor)
    .AddPair('dataEfetivacao', fdataEfetivacao)
    .AddPair('descricaoTransferencia', fdescricaoTransferencia)
    .AddPair('idTransferencia', fidTransferencia)
    .AddPair('nomeEmpresaPagador', fnomeEmpresaPagador)
    .AddPair('nomePagador', fnomePagador)
    .AddPair('nomeRecebedor', fnomeRecebedor)
    .AddPair('tipoDetalhe', ftipoDetalhe);
end;

procedure TACBrExtratoInterTransacaoDetalhesTransferencia.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('agenciaPagador', fagenciaPagador)
    .Value('agenciaRecebedor', fagenciaRecebedor)
    .Value('bancoRecebedor', fbancoRecebedor)
    .Value('contaBancariaPagador', fcontaBancariaPagador)
    .Value('contaBancariaRecebedor', fcontaBancariaRecebedor)
    .Value('cpfCnpjPagador', fcpfCnpjPagador)
    .Value('cpfCnpjRecebedor', fcpfCnpjRecebedor)
    .Value('dataEfetivacao', fdataEfetivacao)
    .Value('descricaoTransferencia', fdescricaoTransferencia)
    .Value('idTransferencia', fidTransferencia)
    .Value('nomeEmpresaPagador', fnomeEmpresaPagador)
    .Value('nomePagador', fnomePagador)
    .Value('nomeRecebedor', fnomeRecebedor)
    .Value('tipoDetalhe', ftipoDetalhe);
end;

procedure TACBrExtratoInterTransacaoDetalhesTransferencia.Clear;
begin
  fagenciaPagador := EmptyStr;
  fagenciaRecebedor := EmptyStr;
  fbancoRecebedor := EmptyStr;
  fcontaBancariaPagador := EmptyStr;
  fcontaBancariaRecebedor := EmptyStr;
  fcpfCnpjPagador := EmptyStr;
  fcpfCnpjRecebedor := EmptyStr;
  fdataEfetivacao := EmptyStr;
  fdescricaoTransferencia := EmptyStr;
  fidTransferencia := EmptyStr;
  fnomeEmpresaPagador := EmptyStr;
  fnomePagador := EmptyStr;
  fnomeRecebedor := EmptyStr;
  ftipoDetalhe := EmptyStr;
end;

function TACBrExtratoInterTransacaoDetalhesTransferencia.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fagenciaPagador) and
    EstaVazio(fagenciaRecebedor) and
    EstaVazio(fbancoRecebedor) and
    EstaVazio(fcontaBancariaPagador) and
    EstaVazio(fcontaBancariaRecebedor) and
    EstaVazio(fcpfCnpjPagador) and
    EstaVazio(fcpfCnpjRecebedor) and
    EstaVazio(fdataEfetivacao) and
    EstaVazio(fdescricaoTransferencia) and
    EstaVazio(fidTransferencia) and
    EstaVazio(fnomeEmpresaPagador) and
    EstaVazio(fnomePagador) and
    EstaVazio(fnomeRecebedor) and
    EstaVazio(ftipoDetalhe);
end;

procedure TACBrExtratoInterTransacaoDetalhesTransferencia.Assign(aSource: TACBrExtratoInterTransacaoDetalhesTransferencia);
begin
  fagenciaPagador := aSource.agenciaPagador;
  fagenciaRecebedor := aSource.agenciaRecebedor;
  fbancoRecebedor := aSource.bancoRecebedor;
  fcontaBancariaPagador := aSource.contaBancariaPagador;
  fcontaBancariaRecebedor := aSource.contaBancariaRecebedor;
  fcpfCnpjPagador := aSource.cpfCnpjPagador;
  fcpfCnpjRecebedor := aSource.cpfCnpjRecebedor;
  fdataEfetivacao := aSource.dataEfetivacao;
  fdescricaoTransferencia := aSource.descricaoTransferencia;
  fidTransferencia := aSource.idTransferencia;
  fnomeEmpresaPagador := aSource.nomeEmpresaPagador;
  fnomePagador := aSource.nomePagador;
  fnomeRecebedor := aSource.nomeRecebedor;
  ftipoDetalhe := aSource.tipoDetalhe;
end;

{ TACBrExtratoInterTransacaoDetalhesDepositoBoleto }

procedure TACBrExtratoInterTransacaoDetalhesDepositoBoleto.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrExtratoInterTransacaoDetalhesDepositoBoleto) then
    Assign(TACBrExtratoInterTransacaoDetalhesDepositoBoleto(aSource));
end;

procedure TACBrExtratoInterTransacaoDetalhesDepositoBoleto.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('codBarras', fcodBarras)
    .AddPair('dataEmissao', fdataEmissao)
    .AddPair('dataVencimento', fdataVencimento)
    .AddPair('nossoNumero', fnossoNumero)
    .AddPair('tipoDetalhe', ftipoDetalhe);
end;

procedure TACBrExtratoInterTransacaoDetalhesDepositoBoleto.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('codBarras', fcodBarras)
    .Value('dataEmissao', fdataEmissao)
    .Value('dataVencimento', fdataVencimento)
    .Value('nossoNumero', fnossoNumero)
    .Value('tipoDetalhe', ftipoDetalhe);
end;

procedure TACBrExtratoInterTransacaoDetalhesDepositoBoleto.Clear;
begin
  fcodBarras := EmptyStr;
  fdataEmissao := EmptyStr;
  fdataVencimento := EmptyStr;
  fnossoNumero := EmptyStr;
  ftipoDetalhe := EmptyStr;
end;

function TACBrExtratoInterTransacaoDetalhesDepositoBoleto.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fcodBarras) and
    EstaVazio(fdataEmissao) and
    EstaVazio(fdataVencimento) and
    EstaVazio(fnossoNumero) and
    EstaVazio(ftipoDetalhe);
end;

procedure TACBrExtratoInterTransacaoDetalhesDepositoBoleto.Assign(aSource: TACBrExtratoInterTransacaoDetalhesDepositoBoleto);
begin
  fcodBarras := aSource.codBarras;
  fdataEmissao := aSource.dataEmissao;
  fdataVencimento := aSource.dataVencimento;
  fnossoNumero := aSource.nossoNumero;
  ftipoDetalhe := aSource.tipoDetalhe;
end;

{ TACBrExtratoInterTransacaoDetalhesCompraDebito }

procedure TACBrExtratoInterTransacaoDetalhesCompraDebito.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrExtratoInterTransacaoDetalhesCompraDebito) then
    Assign(TACBrExtratoInterTransacaoDetalhesCompraDebito(aSource));
end;

procedure TACBrExtratoInterTransacaoDetalhesCompraDebito.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('estabelecimento', festabelecimento)
    .AddPair('tipoDetalhe', ftipoDetalhe);
end;

procedure TACBrExtratoInterTransacaoDetalhesCompraDebito.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('estabelecimento', festabelecimento)
    .Value('tipoDetalhe', ftipoDetalhe);
end;

procedure TACBrExtratoInterTransacaoDetalhesCompraDebito.Clear;
begin
  festabelecimento := EmptyStr;
  ftipoDetalhe := EmptyStr;
end;

function TACBrExtratoInterTransacaoDetalhesCompraDebito.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(festabelecimento) and
    EstaVazio(ftipoDetalhe);
end;

procedure TACBrExtratoInterTransacaoDetalhesCompraDebito.Assign(aSource: TACBrExtratoInterTransacaoDetalhesCompraDebito);
begin
  festabelecimento := aSource.estabelecimento;
  ftipoDetalhe := aSource.tipoDetalhe;
end;

{ TACBrExtratoInterTransacaoDetalhesCheque }

procedure TACBrExtratoInterTransacaoDetalhesCheque.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrExtratoInterTransacaoDetalhesCheque) then
    Assign(TACBrExtratoInterTransacaoDetalhesCheque(aSource));
end;

procedure TACBrExtratoInterTransacaoDetalhesCheque.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('agencia', fagencia)
    .AddPair('codigoAfiliado', fcodigoAfiliado)
    .AddPair('contaBancaria', fcontaBancaria)
    .AddPair('dataRetorno', fdataRetorno)
    .AddPair('descricaoChequeBancario', fdescricaoChequeBancario)
    .AddPair('motivoRetorno', fmotivoRetorno)
    .AddPair('nomeEmpresa', fnomeEmpresa)
    .AddPair('numeroChequeBancario', fnumeroChequeBancario)
    .AddPair('tipoDetalhe', ftipoDetalhe);
end;

procedure TACBrExtratoInterTransacaoDetalhesCheque.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('agencia', fagencia)
    .Value('codigoAfiliado', fcodigoAfiliado)
    .Value('contaBancaria', fcontaBancaria)
    .Value('dataRetorno', fdataRetorno)
    .Value('descricaoChequeBancario', fdescricaoChequeBancario)
    .Value('motivoRetorno', fmotivoRetorno)
    .Value('nomeEmpresa', fnomeEmpresa)
    .Value('numeroChequeBancario', fnumeroChequeBancario)
    .Value('tipoDetalhe', ftipoDetalhe);
end;

procedure TACBrExtratoInterTransacaoDetalhesCheque.Clear;
begin
  fagencia := EmptyStr;
  fcodigoAfiliado := EmptyStr;
  fcontaBancaria := EmptyStr;
  fdataRetorno := EmptyStr;
  fdescricaoChequeBancario := EmptyStr;
  fmotivoRetorno := EmptyStr;
  fnomeEmpresa := EmptyStr;
  fnumeroChequeBancario := EmptyStr;
  ftipoDetalhe := EmptyStr;
end;

function TACBrExtratoInterTransacaoDetalhesCheque.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fagencia) and
    EstaVazio(fcodigoAfiliado) and
    EstaVazio(fcontaBancaria) and
    EstaVazio(fdataRetorno) and
    EstaVazio(fdescricaoChequeBancario) and
    EstaVazio(fmotivoRetorno) and
    EstaVazio(fnomeEmpresa) and
    EstaVazio(fnumeroChequeBancario) and
    EstaVazio(ftipoDetalhe);
end;

procedure TACBrExtratoInterTransacaoDetalhesCheque.Assign(aSource: TACBrExtratoInterTransacaoDetalhesCheque);
begin
  fagencia := aSource.agencia;
  fcodigoAfiliado := aSource.codigoAfiliado;
  fcontaBancaria := aSource.contaBancaria;
  fdataRetorno := aSource.dataRetorno;
  fdescricaoChequeBancario := aSource.descricaoChequeBancario;
  fmotivoRetorno := aSource.motivoRetorno;
  fnomeEmpresa := aSource.nomeEmpresa;
  fnumeroChequeBancario := aSource.numeroChequeBancario;
  ftipoDetalhe := aSource.tipoDetalhe;
end;

{ TACBrExtratoInterTransacaoDetalhesCashback }

procedure TACBrExtratoInterTransacaoDetalhesCashback.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrExtratoInterTransacaoDetalhesCashback) then
    Assign(TACBrExtratoInterTransacaoDetalhesCashback(aSource));
end;

procedure TACBrExtratoInterTransacaoDetalhesCashback.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('produto', fproduto)
    .AddPair('tipoDetalhe', ftipoDetalhe)
    .AddPair('valorCompra', fvalorCompra);
end;

procedure TACBrExtratoInterTransacaoDetalhesCashback.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('produto', fproduto)
    .Value('tipoDetalhe', ftipoDetalhe)
    .Value('valorCompra', fvalorCompra);
end;

procedure TACBrExtratoInterTransacaoDetalhesCashback.Clear;
begin
  fproduto := EmptyStr;
  ftipoDetalhe := EmptyStr;
  fvalorCompra := EmptyStr;
end;

function TACBrExtratoInterTransacaoDetalhesCashback.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fproduto) and
    EstaVazio(ftipoDetalhe) and
    EstaVazio(fvalorCompra);
end;

procedure TACBrExtratoInterTransacaoDetalhesCashback.Assign(aSource: TACBrExtratoInterTransacaoDetalhesCashback);
begin
  fproduto := aSource.produto;
  ftipoDetalhe := aSource.tipoDetalhe;
  fvalorCompra := aSource.valorCompra;
end;

{ TACBrExtratoInterTransacaoDetalhesBoletoCobranca }

procedure TACBrExtratoInterTransacaoDetalhesBoletoCobranca.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrExtratoInterTransacaoDetalhesBoletoCobranca) then
    Assign(TACBrExtratoInterTransacaoDetalhesBoletoCobranca(aSource));
end;

procedure TACBrExtratoInterTransacaoDetalhesBoletoCobranca.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('abatimento', fabatimento)
    .AddPair('codBarras', fcodBarras)
    .AddPair('cpfCnpj', fcpfCnpj)
    .AddPair('dataEmissao', fdataEmissao)
    .AddPair('dataLimite', fdataLimite)
    .AddPair('dataTransacao', fdataTransacao)
    .AddPair('dataVencimento', fdataVencimento)
    .AddPair('desconto1', fdesconto1)
    .AddPair('desconto2', fdesconto2)
    .AddPair('desconto3', fdesconto3)
    .AddPair('juros', fjuros)
    .AddPair('multa', fmulta)
    .AddPair('nome', fnome)
    .AddPair('nossoNumero', fnossoNumero)
    .AddPair('seuNumero', fseuNumero)
    .AddPair('tipoDetalhe', ftipoDetalhe);
end;

procedure TACBrExtratoInterTransacaoDetalhesBoletoCobranca.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('abatimento', fabatimento)
    .Value('codBarras', fcodBarras)
    .Value('cpfCnpj', fcpfCnpj)
    .Value('dataEmissao', fdataEmissao)
    .Value('dataLimite', fdataLimite)
    .Value('dataTransacao', fdataTransacao)
    .Value('dataVencimento', fdataVencimento)
    .Value('desconto1', fdesconto1)
    .Value('desconto2', fdesconto2)
    .Value('desconto3', fdesconto3)
    .Value('juros', fjuros)
    .Value('multa', fmulta)
    .Value('nome', fnome)
    .Value('nossoNumero', fnossoNumero)
    .Value('seuNumero', fseuNumero)
    .Value('tipoDetalhe', ftipoDetalhe);
end;

procedure TACBrExtratoInterTransacaoDetalhesBoletoCobranca.Clear;
begin
  fabatimento := EmptyStr;
  fcodBarras := EmptyStr;
  fcpfCnpj := EmptyStr;
  fdataEmissao := EmptyStr;
  fdataLimite := EmptyStr;
  fdataTransacao := EmptyStr;
  fdataVencimento := EmptyStr;
  fdesconto1 := EmptyStr;
  fdesconto2 := EmptyStr;
  fdesconto3 := EmptyStr;
  fjuros := EmptyStr;
  fmulta := EmptyStr;
  fnome := EmptyStr;
  fnossoNumero := EmptyStr;
  fseuNumero := EmptyStr;
  ftipoDetalhe := EmptyStr;
end;

function TACBrExtratoInterTransacaoDetalhesBoletoCobranca.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fabatimento) and
    EstaVazio(fcodBarras) and
    EstaVazio(fcpfCnpj) and
    EstaVazio(fdataEmissao) and
    EstaVazio(fdataLimite) and
    EstaVazio(fdataTransacao) and
    EstaVazio(fdataVencimento) and
    EstaVazio(fdesconto1) and
    EstaVazio(fdesconto2) and
    EstaVazio(fdesconto3) and
    EstaVazio(fjuros) and
    EstaVazio(fmulta) and
    EstaVazio(fnome) and
    EstaVazio(fnossoNumero) and
    EstaVazio(fseuNumero) and
    EstaVazio(ftipoDetalhe);
end;

procedure TACBrExtratoInterTransacaoDetalhesBoletoCobranca.Assign(aSource: TACBrExtratoInterTransacaoDetalhesBoletoCobranca);
begin
  fabatimento := aSource.abatimento;
  fcodBarras := aSource.codBarras;
  fcpfCnpj := aSource.cpfCnpj;
  fdataEmissao := aSource.dataEmissao;
  fdataLimite := aSource.dataLimite;
  fdataTransacao := aSource.dataTransacao;
  fdataVencimento := aSource.dataVencimento;
  fdesconto1 := aSource.desconto1;
  fdesconto2 := aSource.desconto2;
  fdesconto3 := aSource.desconto3;
  fjuros := aSource.juros;
  fmulta := aSource.multa;
  fnome := aSource.nome;
  fnossoNumero := aSource.nossoNumero;
  fseuNumero := aSource.seuNumero;
  ftipoDetalhe := aSource.tipoDetalhe;
end;

{ TACBrExtratoInterTransacaoDetalhesPix }

procedure TACBrExtratoInterTransacaoDetalhesPix.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrExtratoInterTransacaoDetalhesPix) then
    Assign(TACBrExtratoInterTransacaoDetalhesPix(aSource));
end;

procedure TACBrExtratoInterTransacaoDetalhesPix.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('agenciaRecebedor', fagenciaRecebedor)
    .AddPair('chavePixRecebedor', fchavePixRecebedor)
    .AddPair('contaBancariaRecebedor', fcontaBancariaRecebedor)
    .AddPair('cpfCnpjPagador', fcpfCnpjPagador)
    .AddPair('cpfCnpjRecebedor', fcpfCnpjRecebedor)
    .AddPair('descricaoPix', fdescricaoPix)
    .AddPair('endToEndId', fendToEndId)
    .AddPair('nomeEmpresaPagador', fnomeEmpresaPagador)
    .AddPair('nomeEmpresaRecebedor', fnomeEmpresaRecebedor)
    .AddPair('nomePagador', fnomePagador)
    .AddPair('nomeRecebedor', fnomeRecebedor)
    .AddPair('origemMovimentacao', forigemMovimentacao)
    .AddPair('tipoDetalhe', ftipoDetalhe)
    .AddPair('txId', ftxId);
end;

procedure TACBrExtratoInterTransacaoDetalhesPix.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('agenciaRecebedor', fagenciaRecebedor)
    .Value('chavePixRecebedor', fchavePixRecebedor)
    .Value('contaBancariaRecebedor', fcontaBancariaRecebedor)
    .Value('cpfCnpjPagador', fcpfCnpjPagador)
    .Value('cpfCnpjRecebedor', fcpfCnpjRecebedor)
    .Value('descricaoPix', fdescricaoPix)
    .Value('endToEndId', fendToEndId)
    .Value('nomeEmpresaPagador', fnomeEmpresaPagador)
    .Value('nomeEmpresaRecebedor', fnomeEmpresaRecebedor)
    .Value('nomePagador', fnomePagador)
    .Value('nomeRecebedor', fnomeRecebedor)
    .Value('origemMovimentacao', forigemMovimentacao)
    .Value('tipoDetalhe', ftipoDetalhe)
    .Value('txId', ftxId);
end;

procedure TACBrExtratoInterTransacaoDetalhesPix.Clear;
begin
  fagenciaRecebedor := EmptyStr;
  fchavePixRecebedor := EmptyStr;
  fcontaBancariaRecebedor := EmptyStr;
  fcpfCnpjPagador := EmptyStr;
  fcpfCnpjRecebedor := EmptyStr;
  fdescricaoPix := EmptyStr;
  fendToEndId := EmptyStr;
  fnomeEmpresaPagador := EmptyStr;
  fnomeEmpresaRecebedor := EmptyStr;
  fnomePagador := EmptyStr;
  fnomeRecebedor := EmptyStr;
  forigemMovimentacao := EmptyStr;
  ftipoDetalhe := EmptyStr;
  ftxId := EmptyStr;
end;

function TACBrExtratoInterTransacaoDetalhesPix.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fagenciaRecebedor) and
    EstaVazio(fchavePixRecebedor) and
    EstaVazio(fcontaBancariaRecebedor) and
    EstaVazio(fcpfCnpjPagador) and
    EstaVazio(fcpfCnpjRecebedor) and
    EstaVazio(fdescricaoPix) and
    EstaVazio(fendToEndId) and
    EstaVazio(fnomeEmpresaPagador) and
    EstaVazio(fnomeEmpresaRecebedor) and
    EstaVazio(fnomePagador) and
    EstaVazio(fnomeRecebedor) and
    EstaVazio(forigemMovimentacao) and
    EstaVazio(ftipoDetalhe) and
    EstaVazio(ftxId);
end;

procedure TACBrExtratoInterTransacaoDetalhesPix.Assign(aSource: TACBrExtratoInterTransacaoDetalhesPix);
begin
  fagenciaRecebedor := aSource.agenciaRecebedor;
  fchavePixRecebedor := aSource.chavePixRecebedor;
  fcontaBancariaRecebedor := aSource.contaBancariaRecebedor;
  fcpfCnpjPagador := aSource.cpfCnpjPagador;
  fcpfCnpjRecebedor := aSource.cpfCnpjRecebedor;
  fdescricaoPix := aSource.descricaoPix;
  fendToEndId := aSource.endToEndId;
  fnomeEmpresaPagador := aSource.nomeEmpresaPagador;
  fnomeEmpresaRecebedor := aSource.nomeEmpresaRecebedor;
  fnomePagador := aSource.nomePagador;
  fnomeRecebedor := aSource.nomeRecebedor;
  forigemMovimentacao := aSource.origemMovimentacao;
  ftipoDetalhe := aSource.tipoDetalhe;
  ftxId := aSource.txId;
end;

{ TACBrExtratoInterErro }

procedure TACBrExtratoInterErro.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrExtratoInterErro) then
    Assign(TACBrExtratoInterErro(aSource));
end;

procedure TACBrExtratoInterErro.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('type_', ftype)
    .AddPair('detail', fdetail)
    .AddPair('timestamp', ftimestamp)
    .AddPair('message', fmessage)
    .AddPair('title', ftitle);
end;

procedure TACBrExtratoInterErro.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('type_', ftype)
    .Value('detail', fdetail)
    .Value('timestamp', ftimestamp)
    .Value('message', fmessage)
    .Value('title', ftitle);

  fptitulo := ftitle;
  fpmensagem := fdetail;
  if EstaVazio(fpmensagem) then
    fpmensagem := fmessage;
end;

procedure TACBrExtratoInterErro.Clear;
begin
  inherited Clear;
  ftype := EmptyStr;
  fdetail := EmptyStr;
  ftimestamp := EmptyStr;
  ftitle := EmptyStr;
  fmessage := EmptyStr;
end;

function TACBrExtratoInterErro.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(ftype) and
    EstaVazio(fdetail) and
    EstaVazio(ftimestamp) and
    EstaVazio(ftitle) and
    EstaVazio(fmessage);
end;

procedure TACBrExtratoInterErro.Assign(aSource: TACBrExtratoInterErro);
begin
  ftype := aSource.type_;
  fdetail := aSource.detail;
  ftimestamp := aSource.timestamp;
  ftitle := aSource.title;
  fmessage := aSource.message;
end;

{ TACBrExtratoAPIInter }

function TACBrExtratoAPIInter.GetRespostaErro: TACBrExtratoErro;
begin
  if (not Assigned(fpRespostaErro)) then
    fpRespostaErro := TACBrExtratoInterErro.Create;
  Result := fpRespostaErro;
end;

function TACBrExtratoAPIInter.GetExtratoConsultado: TACBrExtratoConsultado;
begin
  if (not Assigned(fpExtratoConsultado)) then
    fpExtratoConsultado := TACBrExtratoInterResult.Create;
  Result := fpExtratoConsultado;
end;

function TACBrExtratoAPIInter.CalcularURL: String;
begin
  Result := EmptyStr;
  if (not Assigned(fpOwner)) then
    Exit;

  if (fpOwner.Ambiente = eamProducao) then
    Result := cExtratoInterURLProducao
  else if (fpOwner.Ambiente = eamHomologacao) then
    Result := cExtratoInterURLSandbox;
end;

procedure TACBrExtratoAPIInter.Autenticar;
var
  wURL, Body: String;
  js: TACBrJSONObject;
  qp: TACBrHTTPQueryParams;
  sec: Integer;
begin
  LimparHTTP;
  wURL := CalcularURL + cExtratoInterEndPointAuth;
  qp := TACBrHTTPQueryParams.Create;
  try
    qp.Values['grant_type'] := 'client_credentials';
    qp.Values['scope'] := 'extrato.read';
    qp.Values['client_id'] := ClientID;
    qp.Values['client_secret'] := ClientSecret;
    Body := qp.AsURL;
    WriteStrToStream(HTTPSend.Document, Body);
    HttpSend.MimeType := cContentTypeApplicationWwwFormUrlEncoded;
  finally
    qp.Free;
  end;

  HTTPMethod(ChttpMethodPOST, wURL);
  if (HTTPResultCode = HTTP_OK) then
  begin
    js := TACBrJSONObject.Parse(HTTPResponse);
    try
      fpToken := js.AsString['access_token'];
      sec := js.AsInteger['expires_in'];
    finally
      js.Free;
    end;

    if EstaVazio(Trim(fpToken)) then
      DispararExcecao(EACBrAPIException.Create(ACBrStr('Erro de Autenticação')));

    fpValidadeToken := IncSecond(Now, sec);
    fpAutenticado := True;
  end
  else
    DispararExcecao(EACBrAPIException.CreateFmt('Erro HTTP: %d, Metodo: %s, URL: %s',
      [HTTPResultCode, cHTTPMethodPOST, wURL]));
end;

function TACBrExtratoAPIInter.ConsultarExtrato(const aAgencia, aConta: String;
  const aDataInicio: TDateTime; const aDataFim: TDateTime;
  const aPagina: Integer; const aRegistrosPorPag: Integer): Boolean;
var
  wURL: String;
begin
  Result := False;
  PrepararHTTP;
  RegistrarLog('  TACBrExtratoAPIInter.ConsultarExtrato(Conta: ' + aConta + ')');
  
  HttpSend.Protocol := '1.1';
  if NaoEstaZerado(aPagina) then
    URLQueryParams.Values['pagina'] := IntToStr(aPagina);
  if NaoEstaZerado(aRegistrosPorPag) then
    URLQueryParams.Values['tamanhoPagina'] := IntToStr(aRegistrosPorPag);
  if NaoEstaZerado(aDataInicio) then
    URLQueryParams.Values['dataInicio'] := FormatDateTime('yyyy-mm-dd', aDataInicio);
  if NaoEstaZerado(aDataFim) then
    URLQueryParams.Values['dataFim'] := FormatDateTime('yyyy-mm-dd', aDataFim);

  if NaoEstaVazio(aConta) then
    HTTPSend.Headers.Add('x-conta-corrente:' + aConta);

  if NaoEstaVazio(fpToken) then
    HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ fpToken);

  wURL := CalcularURL + cExtratoInterEndPointExtrato;
  HTTPMethod(cHTTPMethodGET, wURL);

  Result := (HTTPResultCode = HTTP_OK);
  if Result then
    ExtratoConsultado.AsJSON := HTTPResponse
  else
    RespostaErro.AsJSON := HTTPResponse;
end;

{ TACBrExtratoInterResult }

function TACBrExtratoInterResult.Gettransacoes: TACBrExtratoInterTransacoes;
begin
  if (not Assigned(ftransacoes)) then
    ftransacoes := TACBrExtratoInterTransacoes.Create('transacoes');
  Result := ftransacoes;
end;

procedure TACBrExtratoInterResult.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrExtratoInterResult) then
    Assign(TACBrExtratoInterResult(aSource));
end;

procedure TACBrExtratoInterResult.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('primeiraPagina', fprimeiraPagina)
    .AddPair('ultimaPagina', fultimaPagina)
    .AddPair('tamanhoPagina', ftamanhoPagina)
    .AddPair('numeroDeElementos', fnumeroDeElementos)
    .AddPair('totalPaginas', ftotalPaginas)
    .AddPair('totalElementos', ftotalElementos);

  if Assigned(ftransacoes) then
    ftransacoes.WriteToJSon(aJSon);
end;

procedure TACBrExtratoInterResult.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('primeiraPagina', fprimeiraPagina)
    .Value('ultimaPagina', fultimaPagina)
    .Value('tamanhoPagina', ftamanhoPagina)
    .Value('numeroDeElementos', fnumeroDeElementos)
    .Value('totalPaginas', ftotalPaginas)
    .Value('totalElementos', ftotalElementos);
  transacoes.ReadFromJSon(aJSon);

  ConverterParaExtratoConsultado;
end;

procedure TACBrExtratoInterResult.ConverterParaExtratoConsultado;
var
  i: Integer;
begin
  fpTotalPaginas := ftotalPaginas;
  fpTotalRegistros := ftotalElementos;
  fpRegistrosPaginaAtual := fnumeroDeElementos;

  if Assigned(ftransacoes) then
  for i := 0 to ftransacoes.Count - 1 do
    with Lancamentos.New do
    begin
      DataLancamento := ftransacoes[i].dataInclusao;
      DataMovimento := ftransacoes[i].dataTransacao;
      Valor := ftransacoes[i].valor;
      TipoOperacao := ftransacoes[i].tipoOperacao;
      Descricao := ftransacoes[i].descricao;
      InfoComplementar := ftransacoes[i].titulo;
    end;
end;

destructor TACBrExtratoInterResult.Destroy;
begin
  if Assigned(ftransacoes) then
    ftransacoes.Free;
  inherited Destroy;
end;

procedure TACBrExtratoInterResult.Clear;
begin
  fnumeroDeElementos := 0;
  ftamanhoPagina := 0;
  ftotalElementos := 0;
  ftotalPaginas := 0;
  fultimaPagina := False;
  fprimeiraPagina := False;

  if Assigned(ftransacoes) then
    ftransacoes.Clear;
end;

function TACBrExtratoInterResult.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroDeElementos) and
    EstaZerado(ftamanhoPagina) and
    EstaZerado(ftotalElementos) and
    EstaZerado(ftotalPaginas) and
    (not fprimeiraPagina) and
    (not fultimaPagina);

  if Assigned(ftransacoes) then
    Result := Result and ftransacoes.IsEmpty;
end;

procedure TACBrExtratoInterResult.Assign(aSource: TACBrExtratoInterResult);
begin
  fprimeiraPagina := aSource.primeiraPagina;
  fultimaPagina := aSource.ultimaPagina;
  ftamanhoPagina := aSource.tamanhoPagina;
  fnumeroDeElementos := aSource.numeroDeElementos;
  ftotalPaginas := aSource.totalPaginas;
  ftotalElementos := aSource.totalElementos;
  transacoes.Assign(aSource.transacoes);
end;

{ TACBrExtratoInterTransacoes }

function TACBrExtratoInterTransacoes.GetItem(aIndex: Integer): TACBrExtratoInterTransacao;
begin
  Result := TACBrExtratoInterTransacao(inherited Items[aIndex]);
end;

procedure TACBrExtratoInterTransacoes.SetItem(aIndex: Integer; aValue: TACBrExtratoInterTransacao);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrExtratoInterTransacoes.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrExtratoInterTransacoes.Add(aLancamento: TACBrExtratoInterTransacao): Integer;
begin
  Result := inherited Add(aLancamento);
end;

procedure TACBrExtratoInterTransacoes.Insert(aIndex: Integer; aLancamento: TACBrExtratoInterTransacao);
begin
  inherited Insert(aIndex, aLancamento);
end;

function TACBrExtratoInterTransacoes.New: TACBrExtratoInterTransacao;
begin
  Result := TACBrExtratoInterTransacao.Create;
  Self.Add(Result);
end;

{ TACBrExtratoInterTransacao }

function TACBrExtratoInterTransacao.GetboletoCobranca: TACBrExtratoInterTransacaoDetalhesBoletoCobranca;
begin
  if (not Assigned(fboletoCobranca)) then
    fboletoCobranca := TACBrExtratoInterTransacaoDetalhesBoletoCobranca.Create('detalhes');
  Result := fboletoCobranca;
end;

function TACBrExtratoInterTransacao.Getcashback: TACBrExtratoInterTransacaoDetalhesCashback;
begin
  if (not Assigned(fcashback)) then
    fcashback := TACBrExtratoInterTransacaoDetalhesCashback.Create('detalhes');
  Result := fcashback;
end;

function TACBrExtratoInterTransacao.Getcheque: TACBrExtratoInterTransacaoDetalhesCheque;
begin
  if (not Assigned(fcheque)) then
    fcheque := TACBrExtratoInterTransacaoDetalhesCheque.Create('detalhes');
  Result := fcheque;
end;

function TACBrExtratoInterTransacao.GetcompraDebito: TACBrExtratoInterTransacaoDetalhesCompraDebito;
begin
  if (not Assigned(fcompraDebito)) then
    fcompraDebito := TACBrExtratoInterTransacaoDetalhesCompraDebito.Create('detalhes');
  Result := fcompraDebito;
end;

function TACBrExtratoInterTransacao.GetdepositoBoleto: TACBrExtratoInterTransacaoDetalhesDepositoBoleto;
begin
  if (not Assigned(fdepositoBoleto)) then
    fdepositoBoleto := TACBrExtratoInterTransacaoDetalhesDepositoBoleto.Create('detalhes');
  Result := fdepositoBoleto;
end;

function TACBrExtratoInterTransacao.Getpagamento: TACBrExtratoInterTransacaoDetalhesPagamento;
begin
  if (not Assigned(fpagamento)) then
    fpagamento := TACBrExtratoInterTransacaoDetalhesPagamento.Create('detalhes');
  Result := fpagamento;
end;

function TACBrExtratoInterTransacao.Getpix: TACBrExtratoInterTransacaoDetalhesPix;
begin
  if (not Assigned(fpix)) then
    fpix := TACBrExtratoInterTransacaoDetalhesPix.Create('detalhes');
  Result := fpix;
end;

function TACBrExtratoInterTransacao.Gettarifa: TACBrExtratoInterTransacaoDetalhesTarifa;
begin
  if (not Assigned(ftarifa)) then
    ftarifa := TACBrExtratoInterTransacaoDetalhesTarifa.Create('detalhes');
  Result := ftarifa;
end;

function TACBrExtratoInterTransacao.Gettransferencia: TACBrExtratoInterTransacaoDetalhesTransferencia;
begin
  if (not Assigned(ftransferencia)) then
    ftransferencia := TACBrExtratoInterTransacaoDetalhesTransferencia.Create('detalhes');
  Result := ftransferencia;
end;

procedure TACBrExtratoInterTransacao.AssignSchema(aSource: TACBrAPISchema);
begin
  if (ASource is TACBrExtratoInterTransacao) then
    Assign(TACBrExtratoInterTransacao(ASource));
end;

procedure TACBrExtratoInterTransacao.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('valor', fvalor)
    .AddPair('titulo', ftitulo)
    .AddPair('descricao', fdescricao)
    .AddPair('idTransacao', fidTransacao)
    .AddPair('dataInclusao', fdataInclusao)
    .AddPair('dataTransacao', fdataTransacao)
    .AddPair('tipoOperacao', TipoLancamentoToString(ftipoOperacao))
    .AddPair('tipoTransacao', TipoTransacaoToString(ftipoTransacao));
  if Assigned(fpix) then
    fpix.WriteToJSon(aJSon);
  if Assigned(fcheque) then
    fcheque.WriteToJSon(aJSon);
  if Assigned(fcashback) then
    fcashback.WriteToJSon(aJSon);
  if Assigned(fcompraDebito) then
    fcompraDebito.WriteToJSon(aJSon);
  if Assigned(fdepositoBoleto) then
    fdepositoBoleto.WriteToJSon(aJSon);
  if Assigned(fboletoCobranca) then
    fboletoCobranca.WriteToJSon(aJSon);
  if Assigned(ftransferencia) then
    ftransferencia.WriteToJSon(aJSon);
  if Assigned(fpagamento) then
    fpagamento.WriteToJSon(aJSon);
  if Assigned(ftarifa) then
    ftarifa.WriteToJSon(aJSon);
end;

procedure TACBrExtratoInterTransacao.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s1, s2, s3, s4: String;
begin
  {$IfDef FPC}
  s1 := EmptyStr;
  s2 := EmptyStr; 
  s3 := EmptyStr;
  s4 := EmptyStr;
  {$EndIf}

  AJSon
    .Value('valor', fvalor)
    .Value('titulo', ftitulo)
    .Value('descricao', fdescricao)
    .Value('idTransacao', fidTransacao)
    .Value('tipoOperacao', s1)
    .Value('tipoTransacao', s2)
    .Value('dataInclusao', s3)
    .Value('dataTransacao', s4);

  if NaoEstaVazio(s1) then
    ftipoOperacao := StringToTipoLancamento(s1);
  if NaoEstaVazio(s2) then
    ftipoTransacao := StringToTipoTransacao(s2);

  if NaoEstaVazio(s3) then
  begin
    if (Length(s3) > 19) then
      s3 := LeftStr(s3, 19);
    fdataInclusao := StringToDateTime(s3, 'yyyy-mm-dd hh:nn:ss');
  end;

  if NaoEstaVazio(s4) then
  begin
    if (Length(s4) > 10) then
      s4 := LeftStr(s4, 10);
    fdataTransacao := StringToDateTime(s4, 'yyyy-mm-dd');
  end;

  case ftipoTransacao of
    ittDepositoBoleto: depositoBoleto.ReadFromJSon(aJSon);
    ittBoletoCobranca: boletoCobranca.ReadFromJSon(aJSon);
    ittCashBack: cashback.ReadFromJSon(aJSon);
    ittCheque: cheque.ReadFromJSon(aJSon);
    ittPagamento: pagamento.ReadFromJSon(aJSon);
    ittPix: pix.ReadFromJSon(aJSon);
    ittCompraDebito: compraDebito.ReadFromJSon(aJSon);
    ittTarifa: tarifa.ReadFromJSon(aJSon);
    ittTransferencia: transferencia.ReadFromJSon(aJSon);
  end;
end;

destructor TACBrExtratoInterTransacao.Destroy;
begin
  if Assigned(fpix) then
    fpix.Free;
                     
  if Assigned(fboletoCobranca) then
    fboletoCobranca.Free;

  if Assigned(fcashback) then
    fcashback.Free;

  if Assigned(fcheque) then
    fcheque.Free;

  if Assigned(fcompraDebito) then
    fcompraDebito.Free;

  if Assigned(fdepositoBoleto) then
    fdepositoBoleto.Free;

  if Assigned(ftransferencia) then
    ftransferencia.Free;

  if Assigned(fpagamento) then
    fpagamento.Free;

  if Assigned(ftarifa) then
    ftarifa.Free;

  inherited Destroy;
end;

procedure TACBrExtratoInterTransacao.Clear;
begin
  fvalor := 0;
  ftitulo := EmptyStr;
  fdescricao := EmptyStr;
  fidTransacao := EmptyStr;
  fdataInclusao := 0;
  fdataTransacao := 0;
  ftipoOperacao := etoNenhum;
  ftipoTransacao := ittNenhum;
  
  if Assigned(fpix) then
    fpix.Clear;

  if Assigned(fcheque) then
    fcheque.Clear;

  if Assigned(fcashback) then
    fcashback.Clear;

  if Assigned(fcompraDebito) then
    fcompraDebito.Clear;

  if Assigned(fdepositoBoleto) then
    fdepositoBoleto.Clear;

  if Assigned(fboletoCobranca) then
    fboletoCobranca.Clear;

  if Assigned(ftransferencia) then
    ftransferencia.Clear;

  if Assigned(fpagamento) then
    fpagamento.Clear;

  if Assigned(ftarifa) then
    ftarifa.Clear;
end;

function TACBrExtratoInterTransacao.IsEmpty: Boolean;
begin
  Result :=                                      
    EstaZerado(fvalor) and
    EstaVazio(ftitulo) and
    EstaVazio(fdescricao) and
    EstaVazio(fidTransacao) and
    EstaZerado(fdataInclusao) and
    EstaZerado(fdataTransacao) and
    (ftipoOperacao = etoNenhum) and
    (ftipoTransacao = ittNenhum);

  if Assigned(fpix) then
    Result := Result and fpix.IsEmpty;

  if Assigned(fcheque) then
    Result := Result and fcheque.IsEmpty;

  if Assigned(fcashback) then
    Result := Result and fcashback.IsEmpty;

  if Assigned(fcompraDebito) then
    Result := Result and fcompraDebito.IsEmpty;

  if Assigned(fdepositoBoleto) then
    Result := Result and fdepositoBoleto.IsEmpty;

  if Assigned(fboletoCobranca) then
    Result := Result and fboletoCobranca.IsEmpty;

  if Assigned(ftransferencia) then
    Result := Result and ftransferencia.IsEmpty;

  if Assigned(fpagamento) then
    Result := Result and fpagamento.IsEmpty;

  if Assigned(ftarifa) then
    Result := Result and ftarifa.IsEmpty;
end;

procedure TACBrExtratoInterTransacao.Assign(aSource: TACBrExtratoInterTransacao);
begin
  fvalor := aSource.valor;
  ftitulo := aSource.titulo;
  fdescricao := aSource.descricao;
  fidTransacao := aSource.idTransacao;
  fdataInclusao := aSource.dataInclusao;
  fdataTransacao := aSource.dataTransacao;
  ftipoOperacao := aSource.tipoOperacao;
  ftipoTransacao := aSource.tipoTransacao;

  pix.Assign(aSource.pix);
  cheque.Assign(aSource.cheque);
  cashback.Assign(aSource.cashback);
  compraDebito.Assign(aSource.compraDebito);
  depositoBoleto.Assign(aSource.depositoBoleto);
  boletoCobranca.Assign(boletoCobranca);
  transferencia.Assign(transferencia);
  pagamento.Assign(pagamento);
  tarifa.Assign(aSource.tarifa);
end;

end.