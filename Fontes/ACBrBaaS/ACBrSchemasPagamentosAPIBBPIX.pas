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

unit ACBrSchemasPagamentosAPIBBPIX;

interface

uses
  Classes, SysUtils, ACBrAPIBase, ACBrJSON,
  ACBrSchemasPagamentosAPIBB;

type

  { TACBrPagamentosBBTransferenciaPIXBase }

  TACBrPagamentosBBTransferenciaPIXBase = class(TACBrAPISchema)
  private
    fagencia: Integer;
    fcnpj: Integer;
    fconta: Integer;
    fcontaPagamento: String;
    fcpf: Integer;
    fdata: TDateTime;
    fdddTelefone: Integer;
    fdescricaoPagamento: String;
    fdescricaoPagamentoInstantaneo: String;
    fdigitoVerificadorConta: String;
    fdocumentoCredito: Integer;
    fdocumentoDebito: Integer;
    femail: String;
    fformaIdentificacao: TACBrPagamentosBBFormaIdentificacao;
    fidentificacaoAleatoria: String;
    fnumeroCOMPE: Integer;
    fnumeroISPB: Integer;
    ftelefone: Integer;
    ftipoConta: TACBrPagamentosBBTipoConta;
    fvalor: Double;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBTransferenciaPIXBase);

    property data: TDateTime read fdata write fdata;
    property valor: Double read fvalor write fvalor;
    property documentoDebito: Integer read fdocumentoDebito write fdocumentoDebito;
    property documentoCredito: Integer read fdocumentoCredito write fdocumentoCredito;
    property descricaoPagamento: String read fdescricaoPagamento write fdescricaoPagamento;
    property descricaoPagamentoInstantaneo: String read fdescricaoPagamentoInstantaneo write fdescricaoPagamentoInstantaneo;
    property formaIdentificacao: TACBrPagamentosBBFormaIdentificacao read fformaIdentificacao write fformaIdentificacao;
    property dddTelefone: Integer read fdddTelefone write fdddTelefone;
    property telefone: Integer read ftelefone write ftelefone;
    property email: String read femail write femail;
    property cpf: Integer read fcpf write fcpf;
    property cnpj: Integer read fcnpj write fcnpj;
    property identificacaoAleatoria: String read fidentificacaoAleatoria write fidentificacaoAleatoria;
    property numeroCOMPE: Integer read fnumeroCOMPE write fnumeroCOMPE;
    property numeroISPB: Integer read fnumeroISPB write fnumeroISPB;
    property tipoConta: TACBrPagamentosBBTipoConta read ftipoConta write ftipoConta;
    property agencia: Integer read fagencia write fagencia;
    property conta: Integer read fconta write fconta;
    property digitoVerificadorConta: String read fdigitoVerificadorConta write fdigitoVerificadorConta;
    property contaPagamento: String read fcontaPagamento write fcontaPagamento;
  end;

  { TACBrPagamentosBBTransferenciasPIXBase }

  TACBrPagamentosBBTransferenciasPIXBase = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBTransferenciaPIXBase;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBTransferenciaPIXBase);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBTransferenciaPIXBase): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBTransferenciaPIXBase);
    function New: TACBrPagamentosBBTransferenciaPIXBase;
    property Items[aIndex: Integer]: TACBrPagamentosBBTransferenciaPIXBase read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBTransferenciaPIX }

  TACBrPagamentosBBTransferenciaPIX = class(TACBrPagamentosBBTransferenciaPIXBase)
  private
    ferros: TACBrPagamenosBBTransferenciaErros;
    fidentificadorPagamento: Integer;
    findicadorMovimentoAceito: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBTransferenciaPIX);

    property identificadorPagamento: Integer read fidentificadorPagamento write fidentificadorPagamento;
    property indicadorMovimentoAceito: String read findicadorMovimentoAceito write findicadorMovimentoAceito;
    property erros: TACBrPagamenosBBTransferenciaErros read ferros write ferros;
  end;

  { TACBrPagamentosBBTransferenciasPIX }

  TACBrPagamentosBBTransferenciasPIX = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBTransferenciaPIX;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBTransferenciaPIX);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBTransferenciaPIX): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBTransferenciaPIX);
    function New: TACBrPagamentosBBTransferenciaPIX;
    property Items[aIndex: Integer]: TACBrPagamentosBBTransferenciaPIX read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBTransferenciaPIXRequisicao }

  TACBrPagamentosBBTransferenciaPIXRequisicao = class(TACBrAPISchema)
  private
    fagenciaDebito: Integer;
    fcontaCorrenteDebito: Integer;
    fdigitoVerificadorContaCorrente: String;
    flistaTransferencias: TACBrPagamentosBBTransferenciasPIXBase;
    fnumeroContrato: Integer;
    fnumeroRequisicao: Integer;
    ftipoPagamento: TACBrPagamentosBBTipoPagamento;
    function GetlistaTransferencias: TACBrPagamentosBBTransferenciasPIXBase;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBTransferenciaPIXRequisicao);

    property numeroRequisicao: Integer read fnumeroRequisicao write fnumeroRequisicao;
    property numeroContrato: Integer read fnumeroContrato write fnumeroContrato;
    property agenciaDebito: Integer read fagenciaDebito write fagenciaDebito;
    property contaCorrenteDebito: Integer read fcontaCorrenteDebito write fcontaCorrenteDebito;
    property digitoVerificadorContaCorrente: String read fdigitoVerificadorContaCorrente write fdigitoVerificadorContaCorrente;
    property tipoPagamento: TACBrPagamentosBBTipoPagamento read ftipoPagamento write ftipoPagamento;
    property listaTransferencias: TACBrPagamentosBBTransferenciasPIXBase read GetlistaTransferencias write flistaTransferencias;
  end;

  { TACBrPagamentosBBTransferenciaPIXResposta }

  TACBrPagamentosBBTransferenciaPIXResposta = class(TACBrAPISchema)
  private
    festadoRequisicao: TACBrPagamentosBBEstadoRequisicao;
    flistaTransferencias: TACBrPagamentosBBTransferenciasPIX;
    fnumeroRequisicao: Integer;
    fquantidadeTransferencias: Integer;
    fquantidadeTransferenciasValidas: Integer;
    fvalorTransferencias: Double;
    fvalorTransferenciasValidas: Double;
    function GetlistaTransferencias: TACBrPagamentosBBTransferenciasPIX;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBTransferenciaPIXResposta);

    property numeroRequisicao: Integer read fnumeroRequisicao write fnumeroRequisicao;
    property estadoRequisicao: TACBrPagamentosBBEstadoRequisicao read festadoRequisicao write festadoRequisicao;
    property quantidadeTransferencias: Integer read fquantidadeTransferencias write fquantidadeTransferencias;
    property valorTransferencias: Double read fvalorTransferencias write fvalorTransferencias;
    property quantidadeTransferenciasValidas: Integer read fquantidadeTransferenciasValidas write fquantidadeTransferenciasValidas;
    property valorTransferenciasValidas: Double read fvalorTransferenciasValidas write fvalorTransferenciasValidas;
    property listaTransferencias: TACBrPagamentosBBTransferenciasPIX read GetlistaTransferencias write flistaTransferencias;
  end;

  { TACBrPagamentosBBPixDetalhamento }

  TACBrPagamentosBBPixDetalhamento = class(TACBrAPISchema)
  private
    fagenciaCredito: Integer;
    fcontaCorrenteCredito: Integer;
    fcpfCnpjBeneficiario: Int64;
    fdddTelefone: Integer;
    fdescricaoPagamentoInstantaneo: Integer;
    fdigitoVerificadorContaCorrente: String;
    fdocumentoCredito: Integer;
    femail: String;
    fformaIdentificacao: String;
    fidentificacaoAleatoria: String;
    fnomeBeneficiario: String;
    fnumeroCOMPE: Integer;
    fnumeroContaPagamentoCredito: String;
    fnumeroISPB: Integer;
    ftelefone: Integer;
    ftextoPix: String;
    ftipoBeneficiario: TACBrPagamentosBBTipoBeneficiario;
    ftipoConta: TACBrPagamentosBBTipoConta;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBPixDetalhamento);

    property numeroCOMPE: Integer read fnumeroCOMPE write fnumeroCOMPE;
    property numeroISPB: Integer read fnumeroISPB write fnumeroISPB;
    property agenciaCredito: Integer read fagenciaCredito write fagenciaCredito;
    property contaCorrenteCredito: Integer read fcontaCorrenteCredito write fcontaCorrenteCredito;
    property digitoVerificadorContaCorrente: String read fdigitoVerificadorContaCorrente write fdigitoVerificadorContaCorrente;
    property numeroContaPagamentoCredito: String read fnumeroContaPagamentoCredito write fnumeroContaPagamentoCredito;
    property tipoBeneficiario: TACBrPagamentosBBTipoBeneficiario read ftipoBeneficiario write ftipoBeneficiario;
    property cpfCnpjBeneficiario: Int64 read fcpfCnpjBeneficiario write fcpfCnpjBeneficiario;
    property nomeBeneficiario: String read fnomeBeneficiario write fnomeBeneficiario;
    property documentoCredito: Integer read fdocumentoCredito write fdocumentoCredito;
    property descricaoPagamentoInstantaneo: Integer read fdescricaoPagamentoInstantaneo write fdescricaoPagamentoInstantaneo;
    property tipoConta: TACBrPagamentosBBTipoConta read ftipoConta write ftipoConta;
    property formaIdentificacao: String read fformaIdentificacao write fformaIdentificacao;
    property dddTelefone: Integer read fdddTelefone write fdddTelefone;
    property telefone: Integer read ftelefone write ftelefone;
    property email: String read femail write femail;
    property identificacaoAleatoria: String read fidentificacaoAleatoria write fidentificacaoAleatoria;
    property textoPix: String read ftextoPix write ftextoPix;
  end;

  { TACBrPagamentosBBPixDetalhamentoObject }

  TACBrPagamentosBBPixDetalhamentoObject = class(TACBrAPISchema)
  private
    fDetalhamento: TACBrPagamentosBBPixDetalhamento;
  public
    property Detalhamento: TACBrPagamentosBBPixDetalhamento read fDetalhamento write fDetalhamento;
  end;

  { TACBrPagamentosBBPixDetalhamentoLista }

  TACBrPagamentosBBPixDetalhamentoLista = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBPixDetalhamentoObject;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBPixDetalhamentoObject);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBPixDetalhamentoObject): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBPixDetalhamentoObject);
    function New: TACBrPagamentosBBPixDetalhamentoObject;
    property Items[aIndex: Integer]: TACBrPagamentosBBPixDetalhamentoObject read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBPixEspecificoResposta }

  TACBrPagamentosBBPixEspecificoResposta = class(TACBrAPISchema)
  private
    fagenciaDebito: Integer;
    farquivoPagamento: String;
    fautenticacaoPagamento: String;
    fcontaDebito: Integer;
    fdataPagamento: Integer;
    fdescricaoPagamento: String;
    fdigitoContaDebito: String;
    festadoPagamento: TACBrPagamentosBBEstado;
    fid: Integer;
    flistaPix: TACBrPagamentosBBPixDetalhamentoLista;
    fnumeroCartaoFim: Integer;
    fnumeroCartaoInicio: Integer;
    fnumeroDocumentoDebito: Integer;
    fquantidadeOcorrenciaPix: Double;
    frequisicaoPagamento: Integer;
    fvalorPagamento: Double;
    function GetlistaPix: TACBrPagamentosBBPixDetalhamentoLista;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBPixEspecificoResposta);

    property id: Integer read fid write fid;
    property estadoPagamento: TACBrPagamentosBBEstado read festadoPagamento write festadoPagamento;
    property agenciaDebito: Integer read fagenciaDebito write fagenciaDebito;
    property contaDebito: Integer read fcontaDebito write fcontaDebito;
    property digitoContaDebito: String read fdigitoContaDebito write fdigitoContaDebito;
    property numeroCartaoInicio: Integer read fnumeroCartaoInicio write fnumeroCartaoInicio;
    property numeroCartaoFim: Integer read fnumeroCartaoFim write fnumeroCartaoFim;
    property requisicaoPagamento: Integer read frequisicaoPagamento write frequisicaoPagamento;
    property arquivoPagamento: String read farquivoPagamento write farquivoPagamento;
    property dataPagamento: Integer read fdataPagamento write fdataPagamento;
    property valorPagamento: Double read fvalorPagamento write fvalorPagamento;
    property numeroDocumentoDebito: Integer read fnumeroDocumentoDebito write fnumeroDocumentoDebito;
    property autenticacaoPagamento: String read fautenticacaoPagamento write fautenticacaoPagamento;
    property descricaoPagamento: String read fdescricaoPagamento write fdescricaoPagamento;
    property quantidadeOcorrenciaPix: Double read fquantidadeOcorrenciaPix write fquantidadeOcorrenciaPix;
    property listaPix: TACBrPagamentosBBPixDetalhamentoLista read GetlistaPix write flistaPix;
  end;

implementation

uses
  ACBrUtil.Base;

{ TACBrPagamentosBBPixDetalhamentoLista }

function TACBrPagamentosBBPixDetalhamentoLista.GetItem(aIndex: Integer
  ): TACBrPagamentosBBPixDetalhamentoObject;
begin
  Result := TACBrPagamentosBBPixDetalhamentoObject(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBPixDetalhamentoLista.SetItem(aIndex: Integer;
  aValue: TACBrPagamentosBBPixDetalhamentoObject);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBPixDetalhamentoLista.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBPixDetalhamentoLista.Add(aItem: TACBrPagamentosBBPixDetalhamentoObject): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBPixDetalhamentoLista.Insert(aIndex: Integer; aItem: TACBrPagamentosBBPixDetalhamentoObject);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBPixDetalhamentoLista.New: TACBrPagamentosBBPixDetalhamentoObject;
begin
  Result := TACBrPagamentosBBPixDetalhamentoObject.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBTransferenciasPIX }

function TACBrPagamentosBBTransferenciasPIX.GetItem(aIndex: Integer): TACBrPagamentosBBTransferenciaPIX;
begin
  Result := TACBrPagamentosBBTransferenciaPIX(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBTransferenciasPIX.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBTransferenciaPIX);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBTransferenciasPIX.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBTransferenciasPIX.Add(
  aItem: TACBrPagamentosBBTransferenciaPIX): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBTransferenciasPIX.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBTransferenciaPIX);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBTransferenciasPIX.New: TACBrPagamentosBBTransferenciaPIX;
begin
  Result := TACBrPagamentosBBTransferenciaPIX.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBTransferenciasPIXBase }

function TACBrPagamentosBBTransferenciasPIXBase.GetItem(aIndex: Integer): TACBrPagamentosBBTransferenciaPIXBase;
begin
  Result := TACBrPagamentosBBTransferenciaPIXBase(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBTransferenciasPIXBase.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBTransferenciaPIXBase);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBTransferenciasPIXBase.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBTransferenciasPIXBase.Add(
  aItem: TACBrPagamentosBBTransferenciaPIXBase): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBTransferenciasPIXBase.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBTransferenciaPIXBase);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBTransferenciasPIXBase.New: TACBrPagamentosBBTransferenciaPIXBase;
begin
  Result := TACBrPagamentosBBTransferenciaPIXBase.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBPixEspecificoResposta }

function TACBrPagamentosBBPixEspecificoResposta.GetlistaPix: TACBrPagamentosBBPixDetalhamentoLista;
begin
  if (not Assigned(flistaPix)) then
    flistaPix := TACBrPagamentosBBPixDetalhamentoLista.Create('listaPix');
  Result := flistaPix;
end;

procedure TACBrPagamentosBBPixEspecificoResposta.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBPixEspecificoResposta) then
    Assign(TACBrPagamentosBBPixEspecificoResposta(ASource));
end;

procedure TACBrPagamentosBBPixEspecificoResposta.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('id', fid)
    .AddPair('estadoPagamento', PagamentosBBEstadoToInteger(festadoPagamento))
    .AddPair('agenciaDebito', fagenciaDebito)
    .AddPair('contaDebito', fcontaDebito)
    .AddPair('digitoContaDebito', fdigitoContaDebito)
    .AddPair('numeroCartaoInicio', fnumeroCartaoInicio)
    .AddPair('numeroCartaoFim', fnumeroCartaoFim)
    .AddPair('requisicaoPagamento', frequisicaoPagamento)
    .AddPair('arquivoPagamento', farquivoPagamento)
    .AddPair('dataPagamento', fdataPagamento)
    .AddPair('valorPagamento', fvalorPagamento)
    .AddPair('numeroDocumentoDebito', fnumeroDocumentoDebito)
    .AddPair('autenticacaoPagamento', fautenticacaoPagamento)
    .AddPair('descricaoPagamento', fdescricaoPagamento)
    .AddPair('quantidadeOcorrenciaPix', fquantidadeOcorrenciaPix);

  if Assigned(flistaPix) then
    flistaPix.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBPixEspecificoResposta.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
begin
  aJSon
    .Value('id', fid)
    .Value('estadoPagamento', i)
    .Value('agenciaDebito', fagenciaDebito)
    .Value('contaDebito', fcontaDebito)
    .Value('digitoContaDebito', fdigitoContaDebito)
    .Value('numeroCartaoInicio', fnumeroCartaoInicio)
    .Value('numeroCartaoFim', fnumeroCartaoFim)
    .Value('requisicaoPagamento', frequisicaoPagamento)
    .Value('arquivoPagamento', farquivoPagamento)
    .Value('dataPagamento', fdataPagamento)
    .Value('valorPagamento', fvalorPagamento)
    .Value('numeroDocumentoDebito', fnumeroDocumentoDebito)
    .Value('autenticacaoPagamento', fautenticacaoPagamento)
    .Value('descricaoPagamento', fdescricaoPagamento)
    .Value('quantidadeOcorrenciaPix', fquantidadeOcorrenciaPix);

  if NaoEstaZerado(i) then
    festadoPagamento := IntegerToPagamentosBBEstado(i);

  if Assigned(flistaPix) then
    flistaPix.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBPixEspecificoResposta.Destroy;
begin
  if Assigned(flistaPix) then
    flistaPix.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBPixEspecificoResposta.Clear;
begin
  fid := 0;
  festadoPagamento := TACBrPagamentosBBEstado(0);
  fagenciaDebito := 0;
  fcontaDebito := 0;
  fdigitoContaDebito := EmptyStr;
  fnumeroCartaoInicio := 0;
  fnumeroCartaoFim := 0;
  frequisicaoPagamento := 0;
  farquivoPagamento := EmptyStr;
  fdataPagamento := 0;
  fvalorPagamento := 0;
  fnumeroDocumentoDebito := 0;
  fautenticacaoPagamento := EmptyStr;
  fdescricaoPagamento := EmptyStr;
  fquantidadeOcorrenciaPix := 0;

  if Assigned(flistaPix) then
    flistaPix.Clear;
end;

function TACBrPagamentosBBPixEspecificoResposta.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fid) and
    EstaZerado(Ord(festadoPagamento)) and
    EstaZerado(fagenciaDebito) and
    EstaZerado(fcontaDebito) and
    EstaVazio(fdigitoContaDebito) and
    EstaZerado(fnumeroCartaoInicio) and
    EstaZerado(fnumeroCartaoFim) and
    EstaZerado(frequisicaoPagamento) and
    EstaVazio(farquivoPagamento) and
    EstaZerado(fdataPagamento) and
    EstaZerado(fvalorPagamento) and
    EstaZerado(fnumeroDocumentoDebito) and
    EstaVazio(fautenticacaoPagamento) and
    EstaVazio(fdescricaoPagamento) and
    EstaZerado(fquantidadeOcorrenciaPix);

  if Assigned(flistaPix) then
    Result := Result and flistaPix.IsEmpty;
end;

procedure TACBrPagamentosBBPixEspecificoResposta.Assign(aSource: TACBrPagamentosBBPixEspecificoResposta);
begin
  fid := ASource.id;
  festadoPagamento := ASource.estadoPagamento;
  fagenciaDebito := ASource.agenciaDebito;
  fcontaDebito := ASource.contaDebito;
  fdigitoContaDebito := ASource.digitoContaDebito;
  fnumeroCartaoInicio := ASource.numeroCartaoInicio;
  fnumeroCartaoFim := ASource.numeroCartaoFim;
  frequisicaoPagamento := ASource.requisicaoPagamento;
  farquivoPagamento := ASource.arquivoPagamento;
  fdataPagamento := ASource.dataPagamento;
  fvalorPagamento := ASource.valorPagamento;
  fnumeroDocumentoDebito := ASource.numeroDocumentoDebito;
  fautenticacaoPagamento := ASource.autenticacaoPagamento;
  fdescricaoPagamento := ASource.descricaoPagamento;
  fquantidadeOcorrenciaPix := ASource.quantidadeOcorrenciaPix;

  if Assigned(flistaPix) then
    flistaPix.Assign(ASource.listaPix);
end;

{ TACBrPagamentosBBPixDetalhamento }

procedure TACBrPagamentosBBPixDetalhamento.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBPixDetalhamento) then
    Assign(TACBrPagamentosBBPixDetalhamento(ASource));
end;

procedure TACBrPagamentosBBPixDetalhamento.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('numeroCOMPE', fnumeroCOMPE)
    .AddPair('numeroISPB', fnumeroISPB)
    .AddPair('agenciaCredito', fagenciaCredito)
    .AddPair('contaCorrenteCredito', fcontaCorrenteCredito)
    .AddPair('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .AddPair('numeroContaPagamentoCredito', fnumeroContaPagamentoCredito)
    .AddPair('tipoBeneficiario', PagamentosBBTipoBeneficiarioToInteger(ftipoBeneficiario))
    .AddPair('cpfCnpjBeneficiario', fcpfCnpjBeneficiario)
    .AddPair('nomeBeneficiario', fnomeBeneficiario)
    .AddPair('documentoCredito', fdocumentoCredito)
    .AddPair('descricaoPagamentoInstantaneo', fdescricaoPagamentoInstantaneo)
    .AddPair('tipoConta', PagamentosBBTipoContaToInteger(ftipoConta))
    .AddPair('formaIdentificacao', fformaIdentificacao)
    .AddPair('dddTelefone', fdddTelefone)
    .AddPair('telefone', ftelefone)
    .AddPair('email', femail)
    .AddPair('identificacaoAleatoria', fidentificacaoAleatoria)
    .AddPair('textoPix', ftextoPix);
end;

procedure TACBrPagamentosBBPixDetalhamento.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i1, i2: Integer;
begin
  aJSon
    .Value('numeroCOMPE', fnumeroCOMPE)
    .Value('numeroISPB', fnumeroISPB)
    .Value('agenciaCredito', fagenciaCredito)
    .Value('contaCorrenteCredito', fcontaCorrenteCredito)
    .Value('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .Value('numeroContaPagamentoCredito', fnumeroContaPagamentoCredito)
    .Value('tipoBeneficiario', i1)
    .Value('cpfCnpjBeneficiario', fcpfCnpjBeneficiario)
    .Value('nomeBeneficiario', fnomeBeneficiario)
    .Value('documentoCredito', fdocumentoCredito)
    .Value('descricaoPagamentoInstantaneo', fdescricaoPagamentoInstantaneo)
    .Value('tipoConta', i2)
    .Value('formaIdentificacao', fformaIdentificacao)
    .Value('dddTelefone', fdddTelefone)
    .Value('telefone', ftelefone)
    .Value('email', femail)
    .Value('identificacaoAleatoria', fidentificacaoAleatoria)
    .Value('textoPix', ftextoPix);

  if NaoEstaZerado(i1) then
    ftipoBeneficiario := IntegerToPagamentosBBTipoBeneficiario(i1);

  if NaoEstaZerado(i2) then
    ftipoConta := IntegerToPagamentosBBTipoConta(i2);
end;

procedure TACBrPagamentosBBPixDetalhamento.Clear;
begin
  fnumeroCOMPE := 0;
  fnumeroISPB := 0;
  fagenciaCredito := 0;
  fcontaCorrenteCredito := 0;
  fdigitoVerificadorContaCorrente := EmptyStr;
  fnumeroContaPagamentoCredito := EmptyStr;
  ftipoBeneficiario := ptbNenhum;
  fcpfCnpjBeneficiario := 0;
  fnomeBeneficiario := EmptyStr;
  fdocumentoCredito := 0;
  fdescricaoPagamentoInstantaneo := 0;
  ftipoConta := ptcNenhum;
  fformaIdentificacao := EmptyStr;
  fdddTelefone := 0;
  ftelefone := 0;
  femail := EmptyStr;
  fidentificacaoAleatoria := EmptyStr;
  ftextoPix := EmptyStr;
end;

function TACBrPagamentosBBPixDetalhamento.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroCOMPE) and
    EstaZerado(fnumeroISPB) and
    EstaZerado(fagenciaCredito) and
    EstaZerado(fcontaCorrenteCredito) and
    EstaVazio(fdigitoVerificadorContaCorrente) and
    EstaVazio(fnumeroContaPagamentoCredito) and
    EstaZerado(Ord(ftipoBeneficiario)) and
    EstaZerado(fcpfCnpjBeneficiario) and
    EstaVazio(fnomeBeneficiario) and
    EstaZerado(fdocumentoCredito) and
    EstaZerado(fdescricaoPagamentoInstantaneo) and
    EstaZerado(Ord(ftipoConta)) and
    EstaVazio(fformaIdentificacao) and
    EstaZerado(fdddTelefone) and
    EstaZerado(ftelefone) and
    EstaVazio(femail) and
    EstaVazio(fidentificacaoAleatoria) and
    EstaVazio(ftextoPix);
end;

procedure TACBrPagamentosBBPixDetalhamento.Assign(aSource: TACBrPagamentosBBPixDetalhamento);
begin
  fnumeroCOMPE := ASource.numeroCOMPE;
  fnumeroISPB := ASource.numeroISPB;
  fagenciaCredito := ASource.agenciaCredito;
  fcontaCorrenteCredito := ASource.contaCorrenteCredito;
  fdigitoVerificadorContaCorrente := ASource.digitoVerificadorContaCorrente;
  fnumeroContaPagamentoCredito := ASource.numeroContaPagamentoCredito;
  ftipoBeneficiario := ASource.tipoBeneficiario;
  fcpfCnpjBeneficiario := ASource.cpfCnpjBeneficiario;
  fnomeBeneficiario := ASource.nomeBeneficiario;
  fdocumentoCredito := ASource.documentoCredito;
  fdescricaoPagamentoInstantaneo := ASource.descricaoPagamentoInstantaneo;
  ftipoConta := ASource.tipoConta;
  fformaIdentificacao := ASource.formaIdentificacao;
  fdddTelefone := ASource.dddTelefone;
  ftelefone := ASource.telefone;
  femail := ASource.email;
  fidentificacaoAleatoria := ASource.identificacaoAleatoria;
  ftextoPix := ASource.textoPix;
end;

{ TACBrPagamentosBBTransferenciaPIXResposta }

function TACBrPagamentosBBTransferenciaPIXResposta.GetlistaTransferencias: TACBrPagamentosBBTransferenciasPIX;
begin
  if (not Assigned(flistaTransferencias)) then
    flistaTransferencias := TACBrPagamentosBBTransferenciasPIX.Create('listaTransferencias');
  Result := flistaTransferencias;
end;

procedure TACBrPagamentosBBTransferenciaPIXResposta.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBTransferenciaPIXResposta) then
    Assign(TACBrPagamentosBBTransferenciaPIXResposta(ASource));
end;

procedure TACBrPagamentosBBTransferenciaPIXResposta.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('numeroRequisicao', fnumeroRequisicao)
    .AddPair('estadoRequisicao', PagamentosBBEstadoRequisicaoToInteger(festadoRequisicao))
    .AddPair('quantidadeTransferencias', fquantidadeTransferencias)
    .AddPair('valorTransferencias', fvalorTransferencias)
    .AddPair('quantidadeTransferenciasValidas', fquantidadeTransferenciasValidas)
    .AddPair('valorTransferenciasValidas', fvalorTransferenciasValidas);

  if Assigned(flistaTransferencias) then
    flistaTransferencias.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBTransferenciaPIXResposta.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
begin
  aJSon
    .Value('numeroRequisicao', fnumeroRequisicao)
    .Value('estadoRequisicao', i)
    .Value('quantidadeTransferencias', fquantidadeTransferencias)
    .Value('valorTransferencias', fvalorTransferencias)
    .Value('quantidadeTransferenciasValidas', fquantidadeTransferenciasValidas)
    .Value('valorTransferenciasValidas', fvalorTransferenciasValidas);

  if NaoEstaZerado(i) then
    festadoRequisicao := IntegerToPagamentosBBEstadoRequisicao(i);

  if Assigned(flistaTransferencias) then
    flistaTransferencias.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBTransferenciaPIXResposta.Destroy;
begin
  if Assigned(flistaTransferencias) then
    flistaTransferencias.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBTransferenciaPIXResposta.Clear;
begin
  fnumeroRequisicao := 0;
  festadoRequisicao := TACBrPagamentosBBEstadoRequisicao(0);
  fquantidadeTransferencias := 0;
  fvalorTransferencias := 0;
  fquantidadeTransferenciasValidas := 0;
  fvalorTransferenciasValidas := 0;

  if Assigned(flistaTransferencias) then
    flistaTransferencias.Clear;
end;

function TACBrPagamentosBBTransferenciaPIXResposta.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroRequisicao) and
    EstaZerado(Ord(festadoRequisicao)) and
    EstaZerado(fquantidadeTransferencias) and
    EstaZerado(fvalorTransferencias) and
    EstaZerado(fquantidadeTransferenciasValidas) and
    EstaZerado(fvalorTransferenciasValidas);

  if Assigned(flistaTransferencias) then
    Result := Result and flistaTransferencias.IsEmpty;
end;

procedure TACBrPagamentosBBTransferenciaPIXResposta.Assign(aSource: TACBrPagamentosBBTransferenciaPIXResposta);
begin
  fnumeroRequisicao := ASource.numeroRequisicao;
  festadoRequisicao := ASource.estadoRequisicao;
  fquantidadeTransferencias := ASource.quantidadeTransferencias;
  fvalorTransferencias := ASource.valorTransferencias;
  fquantidadeTransferenciasValidas := ASource.quantidadeTransferenciasValidas;
  fvalorTransferenciasValidas := ASource.valorTransferenciasValidas;

  if Assigned(flistaTransferencias) then
    flistaTransferencias.Assign(ASource.listaTransferencias);
end;

{ TACBrPagamentosBBTransferenciaPIXRequisicao }

function TACBrPagamentosBBTransferenciaPIXRequisicao.GetlistaTransferencias: TACBrPagamentosBBTransferenciasPIXBase;
begin
  if (not Assigned(flistaTransferencias)) then
    flistaTransferencias := TACBrPagamentosBBTransferenciasPIXBase.Create('listaTransferencias');
  Result := flistaTransferencias;
end;

procedure TACBrPagamentosBBTransferenciaPIXRequisicao.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBTransferenciaPIXRequisicao) then
    Assign(TACBrPagamentosBBTransferenciaPIXRequisicao(ASource));
end;

procedure TACBrPagamentosBBTransferenciaPIXRequisicao.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('numeroRequisicao', fnumeroRequisicao)
    .AddPair('numeroContrato', fnumeroContrato)
    .AddPair('agenciaDebito', fagenciaDebito)
    .AddPair('contaCorrenteDebito', fcontaCorrenteDebito)
    .AddPair('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .AddPair('tipoPagamento', PagamentosBBTipoPagamentoToInteger(ftipoPagamento));

  if Assigned(flistaTransferencias) then
    flistaTransferencias.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBTransferenciaPIXRequisicao.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
begin
  aJSon
    .Value('numeroRequisicao', fnumeroRequisicao)
    .Value('numeroContrato', fnumeroContrato)
    .Value('agenciaDebito', fagenciaDebito)
    .Value('contaCorrenteDebito', fcontaCorrenteDebito)
    .Value('digitoVerificadorContaCorrente', fdigitoVerificadorContaCorrente)
    .Value('tipoPagamento', i);

  if NaoEstaZerado(i) then
    ftipoPagamento := IntegerToPagamentosBBTipoPagamento(i);

  if Assigned(flistaTransferencias) then
    flistaTransferencias.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBTransferenciaPIXRequisicao.Destroy;
begin
  if Assigned(flistaTransferencias) then
    flistaTransferencias.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBTransferenciaPIXRequisicao.Clear;
begin
  fnumeroRequisicao := 0;
  fnumeroContrato := 0;
  fagenciaDebito := 0;
  fcontaCorrenteDebito := 0;
  fdigitoVerificadorContaCorrente := EmptyStr;
  ftipoPagamento := ppgNenhum;

  if Assigned(flistaTransferencias) then
    flistaTransferencias.Clear;
end;

function TACBrPagamentosBBTransferenciaPIXRequisicao.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroRequisicao) and
    EstaZerado(fnumeroContrato) and
    EstaZerado(fagenciaDebito) and
    EstaZerado(fcontaCorrenteDebito) and
    EstaVazio(fdigitoVerificadorContaCorrente) and
    EstaZerado(Ord(ftipoPagamento));

  if Assigned(flistaTransferencias) then
    Result := Result and flistaTransferencias.IsEmpty;
end;

procedure TACBrPagamentosBBTransferenciaPIXRequisicao.Assign(aSource: TACBrPagamentosBBTransferenciaPIXRequisicao);
begin
  fnumeroRequisicao := ASource.numeroRequisicao;
  fnumeroContrato := ASource.numeroContrato;
  fagenciaDebito := ASource.agenciaDebito;
  fcontaCorrenteDebito := ASource.contaCorrenteDebito;
  fdigitoVerificadorContaCorrente := ASource.digitoVerificadorContaCorrente;
  ftipoPagamento := ASource.tipoPagamento;

  if Assigned(flistaTransferencias) then
    flistaTransferencias.Assign(ASource.listaTransferencias);
end;

{ TACBrPagamentosBBTransferenciaPIX }

procedure TACBrPagamentosBBTransferenciaPIX.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBTransferenciaPIX) then
    Assign(TACBrPagamentosBBTransferenciaPIX(ASource));
end;

procedure TACBrPagamentosBBTransferenciaPIX.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('identificadorPagamento', fidentificadorPagamento)
    .AddPair('indicadorMovimentoAceito', findicadorMovimentoAceito);

  if Assigned(ferros) then
    ferros.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBTransferenciaPIX.DoReadFromJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('identificadorPagamento', fidentificadorPagamento)
    .Value('indicadorMovimentoAceito', findicadorMovimentoAceito);

  if Assigned(ferros) then
    ferros.ReadFromJSon(aJSon);
end;

procedure TACBrPagamentosBBTransferenciaPIX.Clear;
begin
  fidentificadorPagamento := 0;
  findicadorMovimentoAceito := EmptyStr;

  if Assigned(ferros) then
    ferros.Clear;
end;

function TACBrPagamentosBBTransferenciaPIX.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fidentificadorPagamento) and
    EstaVazio(findicadorMovimentoAceito);

  if Assigned(ferros) then
    Result := Result and ferros.IsEmpty;
end;

procedure TACBrPagamentosBBTransferenciaPIX.Assign(aSource: TACBrPagamentosBBTransferenciaPIX);
begin
  fidentificadorPagamento := ASource.identificadorPagamento;
  findicadorMovimentoAceito := ASource.indicadorMovimentoAceito;

  if Assigned(ferros) then
    ferros.Assign(ASource.erros);
end;

{ TACBrPagamentosBBTransferenciaPIXBase }

procedure TACBrPagamentosBBTransferenciaPIXBase.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBTransferenciaPIXBase) then
    Assign(TACBrPagamentosBBTransferenciaPIXBase(ASource));
end;

procedure TACBrPagamentosBBTransferenciaPIXBase.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('data', DateToStr(fdata))
    .AddPair('valor', fvalor)
    .AddPair('documentoDebito', fdocumentoDebito)
    .AddPair('documentoCredito', fdocumentoCredito)
    .AddPair('descricaoPagamento', fdescricaoPagamento)
    .AddPair('descricaoPagamentoInstantaneo', fdescricaoPagamentoInstantaneo)
    .AddPair('formaIdentificacao', PagamentosBBFormaIdentificacaoToInteger(fformaIdentificacao))
    .AddPair('dddTelefone', fdddTelefone)
    .AddPair('telefone', ftelefone)
    .AddPair('email', femail)
    .AddPair('cpf', fcpf)
    .AddPair('cnpj', fcnpj)
    .AddPair('identificacaoAleatoria', fidentificacaoAleatoria)
    .AddPair('numeroCOMPE', fnumeroCOMPE)
    .AddPair('numeroISPB', fnumeroISPB)
    .AddPair('tipoConta', PagamentosBBTipoContaToInteger(ftipoConta))
    .AddPair('agencia', fagencia)
    .AddPair('conta', fconta)
    .AddPair('digitoVerificadorConta', fdigitoVerificadorConta)
    .AddPair('contaPagamento', fcontaPagamento);
end;

procedure TACBrPagamentosBBTransferenciaPIXBase.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i1, i2: Integer;
begin
  aJSon
    .Value('data', fdata)
    .Value('valor', fvalor)
    .Value('documentoDebito', fdocumentoDebito)
    .Value('documentoCredito', fdocumentoCredito)
    .Value('descricaoPagamento', fdescricaoPagamento)
    .Value('descricaoPagamentoInstantaneo', fdescricaoPagamentoInstantaneo)
    .Value('formaIdentificacao', i1)
    .Value('dddTelefone', fdddTelefone)
    .Value('telefone', ftelefone)
    .Value('email', femail)
    .Value('cpf', fcpf)
    .Value('cnpj', fcnpj)
    .Value('identificacaoAleatoria', fidentificacaoAleatoria)
    .Value('numeroCOMPE', fnumeroCOMPE)
    .Value('numeroISPB', fnumeroISPB)
    .Value('tipoConta', i2)
    .Value('agencia', fagencia)
    .Value('conta', fconta)
    .Value('digitoVerificadorConta', fdigitoVerificadorConta)
    .Value('contaPagamento', fcontaPagamento);

  if NaoEstaZerado(i1) then
    fformaIdentificacao := IntegerToPagamentosBBFormaIdentificacao(i1);

  if NaoEstaZerado(i2) then
    ftipoConta := IntegerToPagamentosBBTipoConta(i2);
end;

procedure TACBrPagamentosBBTransferenciaPIXBase.Clear;
begin
  fdata := 0;
  fvalor := 0;
  fdocumentoDebito := 0;
  fdocumentoCredito := 0;
  fdescricaoPagamento := EmptyStr;
  fdescricaoPagamentoInstantaneo := EmptyStr;
  fformaIdentificacao := pfiNenhum;
  fdddTelefone := 0;
  ftelefone := 0;
  femail := EmptyStr;
  fcpf := 0;
  fcnpj := 0;
  fidentificacaoAleatoria := EmptyStr;
  fnumeroCOMPE := 0;
  fnumeroISPB := 0;
  ftipoConta := ptcNenhum;
  fagencia := 0;
  fconta := 0;
  fdigitoVerificadorConta := EmptyStr;
  fcontaPagamento := EmptyStr;
end;

function TACBrPagamentosBBTransferenciaPIXBase.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fdata) and
    EstaZerado(fvalor) and
    EstaZerado(fdocumentoDebito) and
    EstaZerado(fdocumentoCredito) and
    EstaVazio(fdescricaoPagamento) and
    EstaVazio(fdescricaoPagamentoInstantaneo) and
    EstaZerado(Ord(fformaIdentificacao)) and
    EstaZerado(fdddTelefone) and
    EstaZerado(ftelefone) and
    EstaVazio(femail) and
    EstaZerado(fcpf) and
    EstaZerado(fcnpj) and
    EstaVazio(fidentificacaoAleatoria) and
    EstaZerado(fnumeroCOMPE) and
    EstaZerado(fnumeroISPB) and
    EstaZerado(Ord(ftipoConta)) and
    EstaZerado(fagencia) and
    EstaZerado(fconta) and
    EstaVazio(fdigitoVerificadorConta) and
    EstaVazio(fcontaPagamento);
end;

procedure TACBrPagamentosBBTransferenciaPIXBase.Assign(aSource: TACBrPagamentosBBTransferenciaPIXBase);
begin
  fdata := ASource.data;
  fvalor := ASource.valor;
  fdocumentoDebito := ASource.documentoDebito;
  fdocumentoCredito := ASource.documentoCredito;
  fdescricaoPagamento := ASource.descricaoPagamento;
  fdescricaoPagamentoInstantaneo := ASource.descricaoPagamentoInstantaneo;
  fformaIdentificacao := ASource.formaIdentificacao;
  fdddTelefone := ASource.dddTelefone;
  ftelefone := ASource.telefone;
  femail := ASource.email;
  fcpf := ASource.cpf;
  fcnpj := ASource.cnpj;
  fidentificacaoAleatoria := ASource.identificacaoAleatoria;
  fnumeroCOMPE := ASource.numeroCOMPE;
  fnumeroISPB := ASource.numeroISPB;
  ftipoConta := ASource.tipoConta;
  fagencia := ASource.agencia;
  fconta := ASource.conta;
  fdigitoVerificadorConta := ASource.digitoVerificadorConta;
  fcontaPagamento := ASource.contaPagamento;
end;

end.

