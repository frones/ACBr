{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - José Junior                                                                }
{ - Antônio Júnior                                                             }
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

//{$I ACBr.inc}

unit ACBrSchemasPagamentosAPI;

interface

uses
  Classes, SysUtils, ACBrAPIBase, ACBrJSON;

type

  TACBrPagamentoTipoLancamento = (
    ptlNenhum,
    ptlGPS,
    ptlGRU,
    ptlDARF,
    ptlPagamentos,
    ptlCodigoBarras,
    ptlTransferencias,
    ptlBoletos,
    ptlTrasnferenciaPix,
    ptlPix
  );

  TACBrPagamentoTipoPessoa = (
    ptpNenhum,
    ptpFisica,
    ptpJuridica
  );

  { TACBrPagamentoLancamentos }

  TACBrPagamentoLancamentos = class(TACBrAPISchema)
  private
    fnumeroDocumentoDebito: Integer;
    fnumeroCodigoBarras: String;
    fdataPagamento: TDateTime;
    fvalorPagamento: Double;
    fdescricaoPagamento: String;
    fcodigoSeuDocumento: String;
    fcodigoNossoDocumento: String;
    fvalorNominal: Double;
    fvalorDesconto: Double;
    fvalorMoraMulta: Double;
    fcodigoTipoPagador: TACBrPagamentoTipoPessoa;
    fdocumentoPagador: String;
    fcodigoTipoBeneficiario: TACBrPagamentoTipoPessoa;
    fdocumentoBeneficiario: String;
    fcodigoTipoAvalista: TACBrPagamentoTipoPessoa;
    fdocumentoAvalista: String;

  protected
    procedure AssignSchema(ASource: TACBrAPISchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(ASource: TACBrPagamentoLancamentos);

    property numeroDocumentoDebito: Integer read fnumeroDocumentoDebito write fnumeroDocumentoDebito;
    property numeroCodigoBarras: String read fnumeroCodigoBarras write fnumeroCodigoBarras;
    property dataPagamento: TDateTime read fdataPagamento write fdataPagamento;
    property valorPagamento: Double read fvalorPagamento write fvalorPagamento;
    property descricaoPagamento: String read fdescricaoPagamento write fdescricaoPagamento;
    property codigoSeuDocumento: String read fcodigoSeuDocumento write fcodigoSeuDocumento;
    property codigoNossoDocumento: String read fcodigoNossoDocumento write fcodigoNossoDocumento;
    property valorNominal: Double read fvalorNominal write fvalorNominal;
    property valorDesconto: Double read fvalorDesconto write fvalorDesconto;
    property valorMoraMulta: Double read fvalorMoraMulta write fvalorMoraMulta;
    property codigoTipoPagador: TACBrPagamentoTipoPessoa read fcodigoTipoPagador write fcodigoTipoPagador;
    property documentoPagador: String read fdocumentoPagador write fdocumentoPagador;
    property codigoTipoBeneficiario: TACBrPagamentoTipoPessoa read fcodigoTipoBeneficiario write fcodigoTipoBeneficiario;
    property documentoBeneficiario: String read fdocumentoBeneficiario write fdocumentoBeneficiario;
    property codigoTipoAvalista: TACBrPagamentoTipoPessoa read fcodigoTipoAvalista write fcodigoTipoAvalista;
    property documentoAvalista: String read fdocumentoAvalista write fdocumentoAvalista;

  end;

  { TACBrPagamentoListaLancamento }

  TACBrPagamentoListaLancamento = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentoLancamentos;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentoLancamentos);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentoLancamentos): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentoLancamentos);
    function New: TACBrPagamentoLancamentos;
    property Items[aIndex: Integer]: TACBrPagamentoLancamentos read GetItem write SetItem; default;

  end;

  { TACBrPagamentoRemessa }

  TACBrPagamentoRemessa = class(TACBrAPISchema)
  private
    fnumeroRequisicao: Integer;
    fcodigoContrato: Integer;
    fnumeroAgenciaDebito: Integer;
    fnumeroContaCorrenteDebito: Integer;
    fdigitoVerificadorContaCorrenteDebito: String;
    flancamentos: TACBrPagamentoListaLancamento;

  protected
    procedure AssignSchema(ASource: TACBrAPISchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(ASource: TACBrPagamentoRemessa);

    property numeroRequisicao: Integer read fnumeroRequisicao write fnumeroRequisicao;
    property codigoContrato: Integer read fcodigoContrato write fcodigoContrato;
    property numeroAgenciaDebito: Integer read fnumeroAgenciaDebito write fnumeroAgenciaDebito;
    property numeroContaCorrenteDebito: Integer read fnumeroContaCorrenteDebito write fnumeroContaCorrenteDebito;
    property digitoVerificadorContaCorrenteDebito: String read fdigitoVerificadorContaCorrenteDebito write fdigitoVerificadorContaCorrenteDebito;
    property lancamentos: TACBrPagamentoListaLancamento read flancamentos write flancamentos;

  end;

  ////

  { TACBrPagamentoLancamentosResponse }

  TACBrPagamentoLancamentosResponse = class(TACBrAPISchema)
  private
    fcodigoIdentificadorPagamento: Integer;
    fnumeroDocumentoDebito: Integer;
    fnumeroCodigoBarras: String;
    fdataPagamento: TDateTime;
    fvalorPagamento: Double;
    fdescricaoPagamento: String;
    fcodigoSeuDocumento: String;
    fcodigoNossoDocumento: String;
    fvalorNominal: Double;
    fvalorDesconto: Double;
    fvalorMoraMulta: Double;
    fcodigoTipoPagador: TACBrPagamentoTipoPessoa;
    fdocumentoPagador: String;
    fcodigoTipoBeneficiario: TACBrPagamentoTipoPessoa;
    fdocumentoBeneficiario: String;
    fcodigoTipoAvalista: TACBrPagamentoTipoPessoa;
    fdocumentoAvalista: String;
    findicadorAceite: String;

  protected
    procedure AssignSchema(ASource: TACBrAPISchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(ASource: TACBrPagamentoLancamentosResponse);

    property codigoIdentificadorPagamento: Integer read fcodigoIdentificadorPagamento write fcodigoIdentificadorPagamento;
    property numeroDocumentoDebito: Integer read fnumeroDocumentoDebito write fnumeroDocumentoDebito;
    property numeroCodigoBarras: String read fnumeroCodigoBarras write fnumeroCodigoBarras;
    property dataPagamento: TDateTime read fdataPagamento write fdataPagamento;
    property valorPagamento: Double read fvalorPagamento write fvalorPagamento;
    property descricaoPagamento: String read fdescricaoPagamento write fdescricaoPagamento;
    property codigoSeuDocumento: String read fcodigoSeuDocumento write fcodigoSeuDocumento;
    property codigoNossoDocumento: String read fcodigoNossoDocumento write fcodigoNossoDocumento;
    property valorNominal: Double read fvalorNominal write fvalorNominal;
    property valorDesconto: Double read fvalorDesconto write fvalorDesconto;
    property valorMoraMulta: Double read fvalorMoraMulta write fvalorMoraMulta;
    property codigoTipoPagador: TACBrPagamentoTipoPessoa read fcodigoTipoPagador write fcodigoTipoPagador;
    property documentoPagador: String read fdocumentoPagador write fdocumentoPagador;
    property codigoTipoBeneficiario: TACBrPagamentoTipoPessoa read fcodigoTipoBeneficiario write fcodigoTipoBeneficiario;
    property documentoBeneficiario: String read fdocumentoBeneficiario write fdocumentoBeneficiario;
    property codigoTipoAvalista: TACBrPagamentoTipoPessoa read fcodigoTipoAvalista write fcodigoTipoAvalista;
    property documentoAvalista: String read fdocumentoAvalista write fdocumentoAvalista;
    property indicadorAceite: String read findicadorAceite write findicadorAceite;

  end;

  { TACBrPagamentoListaLancamentoResponse }

  TACBrPagamentoListaLancamentoResponse = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentoLancamentosResponse;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentoLancamentosResponse);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentoLancamentosResponse): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentoLancamentosResponse);
    function New: TACBrPagamentoLancamentosResponse;
    property Items[aIndex: Integer]: TACBrPagamentoLancamentosResponse read GetItem write SetItem; default;

  end;

  { TACBrPagamentoResponse }

  TACBrPagamentoResponse  = class(TACBrAPISchema)
  private
    fnumeroRequisicao: Integer;
    festadoRequisicao: Integer;
    fquantidadeLancamentos: Integer;
    fvalorLancamentos: Double;
    fquantidadeLancamentosValidos: Integer;
    fvalorLancamentosValidos: Double;
    flancamentos: TACBrPagamentoListaLancamentoResponse;

  protected
    procedure AssignSchema(ASource: TACBrAPISchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(ASource: TACBrPagamentoResponse );

    property numeroRequisicao: Integer read fnumeroRequisicao write fnumeroRequisicao;
    property estadoRequisicao: Integer read festadoRequisicao write festadoRequisicao;
    property quantidadeLancamentos: Integer read fquantidadeLancamentos write fquantidadeLancamentos;
    property valorLancamentos: Double read fvalorLancamentos write fvalorLancamentos;
    property quantidadeLancamentosValidos: Integer read fquantidadeLancamentosValidos write fquantidadeLancamentosValidos;
    property valorLancamentosValidos: Double read fvalorLancamentosValidos write fvalorLancamentosValidos;
    property lancamentos: TACBrPagamentoListaLancamentoResponse read flancamentos write flancamentos;

  end;

  { TACBrPagamentosErro }

  TACBrPagamentosErro = class(TACBrAPISchema)
  private
    fcodigo: String;
    fmensagem: String;
    focorrencia: String;
    fversao: String;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosErro);

    property codigo: String read fcodigo write fcodigo;
    property versao: String read fversao write fversao;
    property mensagem: String read fmensagem write fmensagem;
    property ocorrencia: String read focorrencia write focorrencia;
  end;


  function TipoPessoaToInt(const aTipo: TACBrPagamentoTipoPessoa): Integer;
  function IntToTipoPessoa(const aStr: Integer): TACBrPagamentoTipoPessoa;

implementation

uses
  synautil, synacode,
  ACBrSocket,
  ACBrUtil.DateTime,
  ACBrUtil.Base;

function TipoPessoaToInt(const aTipo: TACBrPagamentoTipoPessoa): Integer;
begin
  Result := 0;
    case aTipo of
      ptpNenhum: Result := 0;
      ptpFisica: Result := 1;
      ptpJuridica: Result := 2;
    end;
end;

function IntToTipoPessoa(const aStr: Integer): TACBrPagamentoTipoPessoa;
  var
  s: Integer;
begin
  Result := ptpNenhum;
  s := aStr;
  if (s = 1) then
    Result := ptpFisica
  else if (s = 2) then
    Result := ptpJuridica;
end;

{ TACBrPagamentoListaLancamentoResponse }

function TACBrPagamentoListaLancamentoResponse.GetItem(aIndex: Integer
  ): TACBrPagamentoLancamentosResponse;
begin
  Result := TACBrPagamentoLancamentosResponse(inherited Items[aIndex]);
end;

procedure TACBrPagamentoListaLancamentoResponse.SetItem(aIndex: Integer;
  aValue: TACBrPagamentoLancamentosResponse);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentoListaLancamentoResponse.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentoListaLancamentoResponse.Add(
  aLancamento: TACBrPagamentoLancamentosResponse): Integer;
begin
  Result := inherited Add(aLancamento);
end;

procedure TACBrPagamentoListaLancamentoResponse.Insert(aIndex: Integer;
  aLancamento: TACBrPagamentoLancamentosResponse);
begin
  inherited Insert(aIndex, aLancamento);
end;

function TACBrPagamentoListaLancamentoResponse.New: TACBrPagamentoLancamentosResponse;
begin
  Result := TACBrPagamentoLancamentosResponse.Create;
  Self.Add(Result);
end;

{ TACBrPagamentoLancamentosResponse }

procedure TACBrPagamentoLancamentosResponse.AssignSchema(
  ASource: TACBrAPISchema);
begin
  if (ASource is TACBrPagamentoLancamentosResponse) then
    Assign(TACBrPagamentoLancamentosResponse(ASource));
end;

procedure TACBrPagamentoLancamentosResponse.DoWriteToJSon(
  AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('codigoIdentificadorPagamento', fcodigoIdentificadorPagamento)
    .AddPair('numeroDocumentoDebito', fnumeroDocumentoDebito)
    .AddPair('numeroCodigoBarras', fnumeroCodigoBarras)
    .AddPair('dataPagamento', fdataPagamento)
    .AddPair('valorPagamento', fvalorPagamento)
    .AddPair('descricaoPagamento', fdescricaoPagamento)
    .AddPair('codigoSeuDocumento', fcodigoSeuDocumento)
    .AddPair('codigoNossoDocumento', fcodigoNossoDocumento)
    .AddPair('valorNominal', fvalorNominal)
    .AddPair('valorDesconto', fvalorDesconto)
    .AddPair('valorMoraMulta', fvalorMoraMulta)
    .AddPair('codigoTipoPagador', TipoPessoaToInt(fcodigoTipoPagador))
    .AddPair('documentoPagador', fdocumentoPagador)
    .AddPair('codigoTipoBeneficiario', TipoPessoaToInt(fcodigoTipoBeneficiario))
    .AddPair('documentoBeneficiario', fdocumentoBeneficiario)
    .AddPair('codigoTipoAvalista', TipoPessoaToInt(fcodigoTipoAvalista))
    .AddPair('documentoAvalista', fdocumentoAvalista)
    .AddPair('indicadorAceite', findicadorAceite);
end;

procedure TACBrPagamentoLancamentosResponse.DoReadFromJSon(
  AJSon: TACBrJSONObject);
var
  i1, i2, i3: Integer;
  s4: String;
  d, m, a: word;
begin
  AJSon
    .Value('codigoIdentificadorPagamento', fcodigoIdentificadorPagamento)
    .Value('numeroDocumentoDebito', fnumeroDocumentoDebito)
    .Value('numeroCodigoBarras', fnumeroCodigoBarras)
    .Value('dataPagamento', s4)
    .Value('valorPagamento', fvalorPagamento)
    .Value('descricaoPagamento', fdescricaoPagamento)
    .Value('codigoSeuDocumento', fcodigoSeuDocumento)
    .Value('codigoNossoDocumento', fcodigoNossoDocumento)
    .Value('valorNominal', fvalorNominal)
    .Value('valorDesconto', fvalorDesconto)
    .Value('valorMoraMulta', fvalorMoraMulta)
    .Value('codigoTipoPagador', i1)
    .Value('documentoPagador', fdocumentoPagador)
    .Value('codigoTipoBeneficiario', i2)
    .Value('documentoBeneficiario', fdocumentoBeneficiario)
    .Value('codigoTipoAvalista', i3)
    .Value('documentoAvalista', fdocumentoAvalista)
    .Value('indicadorAceite', findicadorAceite);
  fcodigoTipoPagador := IntToTipoPessoa(i1);
  fcodigoTipoBeneficiario := IntToTipoPessoa(i2);
  fcodigoTipoAvalista := IntToTipoPessoa(i3);
  if (Length(s4) = 8) then
  begin
    d := Copy(s4, 1, 2).ToInteger;
    m := Copy(s4, 3, 2).ToInteger;
    a := Copy(s4, 5, 4).ToInteger;
    fdataPagamento := EncodeDate(a, m, d);
  end;
end;

procedure TACBrPagamentoLancamentosResponse.Clear;
begin
  fcodigoIdentificadorPagamento := 0;
  fnumeroDocumentoDebito := 0;
  fnumeroCodigoBarras := EmptyStr;
  fdataPagamento := 0;
  fvalorPagamento := 0;
  fdescricaoPagamento := EmptyStr;
  fcodigoSeuDocumento := EmptyStr;
  fcodigoNossoDocumento := EmptyStr;
  fvalorNominal := 0;
  fvalorDesconto := 0;
  fvalorMoraMulta := 0;
  fcodigoTipoPagador := ptpNenhum;
  fdocumentoPagador := '0';
  fcodigoTipoBeneficiario := ptpNenhum;
  fdocumentoBeneficiario := '0';
  fcodigoTipoAvalista := ptpNenhum;
  fdocumentoAvalista := '0';
  findicadorAceite := EmptyStr;
end;

function TACBrPagamentoLancamentosResponse.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fcodigoIdentificadorPagamento) and
    EstaZerado(fnumeroDocumentoDebito) and
    EstaVazio(fnumeroCodigoBarras) and
    EstaZerado(fdataPagamento) and
    EstaZerado(fvalorPagamento) and
    EstaVazio(fdescricaoPagamento) and
    EstaVazio(fcodigoSeuDocumento) and
    EstaVazio(fcodigoNossoDocumento) and
    EstaZerado(fvalorNominal) and
    EstaZerado(fvalorDesconto) and
    EstaZerado(fvalorMoraMulta) and
    (fcodigoTipoPagador = ptpNenhum) and
    EstaVazio(fdocumentoPagador) and
    (fcodigoTipoBeneficiario = ptpNenhum) and
    EstaVazio(fdocumentoBeneficiario) and
    (fcodigoTipoAvalista = ptpNenhum) and
    EstaVazio(fdocumentoAvalista) and
    EstaVazio(findicadorAceite);
end;

procedure TACBrPagamentoLancamentosResponse.Assign(
  ASource: TACBrPagamentoLancamentosResponse);
begin
  fcodigoIdentificadorPagamento := ASource.fcodigoIdentificadorPagamento;
  fnumeroDocumentoDebito := ASource.numeroDocumentoDebito;
  fnumeroCodigoBarras := ASource.numeroCodigoBarras;
  fdataPagamento := ASource.dataPagamento;
  fvalorPagamento := ASource.valorPagamento;
  fdescricaoPagamento := ASource.descricaoPagamento;
  fcodigoSeuDocumento := ASource.codigoSeuDocumento;
  fcodigoNossoDocumento := ASource.codigoNossoDocumento;
  fvalorNominal := ASource.valorNominal;
  fvalorDesconto := ASource.valorDesconto;
  fvalorMoraMulta := ASource.valorMoraMulta;
  fcodigoTipoPagador := ASource.codigoTipoPagador;
  fdocumentoPagador := ASource.documentoPagador;
  fcodigoTipoBeneficiario := ASource.codigoTipoBeneficiario;
  fdocumentoBeneficiario := ASource.documentoBeneficiario;
  fcodigoTipoAvalista := ASource.codigoTipoAvalista;
  fdocumentoAvalista := ASource.documentoAvalista;
  findicadorAceite := ASource.findicadorAceite;
end;

{ TACBrPagamentoResponse }

procedure TACBrPagamentoResponse.AssignSchema(ASource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentoResponse) then
    Assign(TACBrPagamentoResponse(ASource));
end;

procedure TACBrPagamentoResponse.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('numeroRequisicao', fnumeroRequisicao)
    .AddPair('estadoRequisicao', festadoRequisicao)
    .AddPair('quantidadeLancamentos', fquantidadeLancamentos)
    .AddPair('valorLancamentos', fvalorLancamentos)
    .AddPair('quantidadeLancamentosValidos', fquantidadeLancamentosValidos)
    .AddPair('valorLancamentosValidos', fvalorLancamentosValidos);

  if Assigned(flancamentos) then
    flancamentos.WriteToJSon(aJSon);
end;

procedure TACBrPagamentoResponse.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  aJSon
    .Value('numeroRequisicao', fnumeroRequisicao)
    .Value('estadoRequisicao', festadoRequisicao)
    .Value('quantidadeLancamentos', fquantidadeLancamentos)
    .Value('valorLancamentos', fvalorLancamentos)
    .Value('quantidadeLancamentosValidos', fquantidadeLancamentosValidos)
    .Value('valorLancamentosValidos', fvalorLancamentosValidos);

  if Assigned(flancamentos) then
    flancamentos.ReadFromJSon(aJSon);
end;

constructor TACBrPagamentoResponse.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  flancamentos := TACBrPagamentoListaLancamentoResponse.Create('lancamentos');
end;

procedure TACBrPagamentoResponse.Clear;
begin
  fnumeroRequisicao:= 0;
  festadoRequisicao:= 0;
  fquantidadeLancamentos:= 0;
  fvalorLancamentos:= 0;
  fquantidadeLancamentosValidos:= 0;
  fvalorLancamentosValidos:= 0;
  if Assigned(flancamentos) then
    flancamentos.clear;
end;

function TACBrPagamentoResponse.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroRequisicao) and
    EstaZerado(festadoRequisicao) and
    EstaZerado(fquantidadeLancamentos) and
    EstaZerado(fvalorLancamentos) and
    EstaZerado(fquantidadeLancamentosValidos) and
    EstaZerado(fvalorLancamentosValidos) and
    flancamentos.IsEmpty;

end;

procedure TACBrPagamentoResponse.Assign(ASource: TACBrPagamentoResponse);
begin
  fnumeroRequisicao := ASource.numeroRequisicao;
  festadoRequisicao := ASource.estadoRequisicao;
  fquantidadeLancamentos := ASource.quantidadeLancamentos;
  fvalorLancamentos := ASource.valorLancamentos;
  fquantidadeLancamentosValidos := ASource.quantidadeLancamentosValidos;
  fvalorLancamentosValidos := ASource.valorLancamentosValidos;

  if Assigned(flancamentos) then
    flancamentos.Assign(ASource.lancamentos);
end;

{ TACBrPagamentosErro }

procedure TACBrPagamentosErro.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosErro) then
    Assign(TACBrPagamentosErro(aSource));
end;

procedure TACBrPagamentosErro.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('codigo', fcodigo)
    .AddPair('mensagem', fmensagem)
    .AddPair('ocorrencia', focorrencia)
    .AddPair('versao', fversao);
end;

procedure TACBrPagamentosErro.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('codigo', fcodigo)
    .Value('mensagem', fmensagem)
    .Value('ocorrencia', focorrencia)
    .Value('versao', fversao);
end;

procedure TACBrPagamentosErro.Clear;
begin
  fcodigo := EmptyStr;
  fmensagem := EmptyStr;
  focorrencia := EmptyStr;
  fversao := EmptyStr;
end;

function TACBrPagamentosErro.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fcodigo) and
    EstaVazio(fmensagem) and
    EstaVazio(focorrencia) and
    EstaVazio(fversao);
end;

procedure TACBrPagamentosErro.Assign(aSource: TACBrPagamentosErro);
begin
  fcodigo := aSource.codigo;
  fmensagem := aSource.mensagem;
  focorrencia := aSource.ocorrencia;
  fversao := aSource.versao;
end;

{ TACBrPagamentoRemessa }

procedure TACBrPagamentoRemessa.AssignSchema(ASource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentoRemessa) then
    Assign(TACBrPagamentoRemessa(ASource));
end;

procedure TACBrPagamentoRemessa.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('numeroRequisicao', fnumeroRequisicao)
    .AddPair('codigoContrato', fcodigoContrato)
    .AddPair('numeroAgenciaDebito', fnumeroAgenciaDebito)
    .AddPair('numeroContaCorrenteDebito', fnumeroContaCorrenteDebito)
    .AddPair('digitoVerificadorContaCorrenteDebito', fdigitoVerificadorContaCorrenteDebito);

  if Assigned(flancamentos) then
    lancamentos.WriteToJSon(aJSon);
end;

procedure TACBrPagamentoRemessa.DoReadFromJSon(AJSon: TACBrJSONObject);
begin
  aJSon
    .Value('numeroRequisicao', fnumeroRequisicao)
    .Value('codigoContrato', fcodigoContrato)
    .Value('numeroAgenciaDebito', fnumeroAgenciaDebito)
    .Value('numeroContaCorrenteDebito', fnumeroContaCorrenteDebito)
    .Value('digitoVerificadorContaCorrenteDebito', fdigitoVerificadorContaCorrenteDebito);

  if Assigned(flancamentos) then
    flancamentos.ReadFromJSon(aJSon);
end;

constructor TACBrPagamentoRemessa.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  flancamentos := TACBrPagamentoListaLancamento.Create('lancamentos');
end;

procedure TACBrPagamentoRemessa.Clear;
begin
  fnumeroRequisicao := 0;
  fcodigoContrato := 0;
  fnumeroAgenciaDebito := 0;
  fnumeroContaCorrenteDebito := 0;
  fdigitoVerificadorContaCorrenteDebito := '';

  if Assigned(flancamentos) then
    flancamentos.Clear;
end;

function TACBrPagamentoRemessa.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroRequisicao) and
    EstaZerado(fcodigoContrato) and
    EstaZerado(fnumeroAgenciaDebito) and
    EstaZerado(fnumeroContaCorrenteDebito) and
    EstaVazio(fdigitoVerificadorContaCorrenteDebito) and
    flancamentos.IsEmpty;
end;

procedure TACBrPagamentoRemessa.Assign(ASource: TACBrPagamentoRemessa);
begin
  fnumeroRequisicao := ASource.numeroRequisicao;
  fcodigoContrato := ASource.codigoContrato;
  fnumeroAgenciaDebito := ASource.numeroAgenciaDebito;
  fnumeroContaCorrenteDebito := ASource.numeroContaCorrenteDebito;
  fdigitoVerificadorContaCorrenteDebito := ASource.digitoVerificadorContaCorrenteDebito;

  if Assigned(flancamentos) then
    flancamentos.Assign(ASource.lancamentos);
end;

{ TACBrPagamentoListaLancamento }

function TACBrPagamentoListaLancamento.GetItem(aIndex: Integer
  ): TACBrPagamentoLancamentos;
begin
  Result := TACBrPagamentoLancamentos(inherited Items[aIndex]);
end;

procedure TACBrPagamentoListaLancamento.SetItem(aIndex: Integer;
  aValue: TACBrPagamentoLancamentos);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentoListaLancamento.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentoListaLancamento.Add(
  aLancamento: TACBrPagamentoLancamentos): Integer;
begin
  Result := inherited Add(aLancamento);
end;

procedure TACBrPagamentoListaLancamento.Insert(aIndex: Integer;
  aLancamento: TACBrPagamentoLancamentos);
begin
  inherited Insert(aIndex, aLancamento);
end;

function TACBrPagamentoListaLancamento.New: TACBrPagamentoLancamentos;
begin
  Result := TACBrPagamentoLancamentos.Create;
  Self.Add(Result);
end;

{ TACBrPagamentoLancamentos }

procedure TACBrPagamentoLancamentos.AssignSchema(ASource: TACBrAPISchema);
begin
  if (ASource is TACBrPagamentoLancamentos) then
    Assign(TACBrPagamentoLancamentos(ASource));
end;

procedure TACBrPagamentoLancamentos.DoWriteToJSon(AJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('numeroDocumentoDebito', fnumeroDocumentoDebito)
    .AddPair('numeroCodigoBarras', fnumeroCodigoBarras)
    .AddPair('dataPagamento', fdataPagamento)
    .AddPair('valorPagamento', fvalorPagamento)
    .AddPair('descricaoPagamento', fdescricaoPagamento)
    .AddPair('codigoSeuDocumento', fcodigoSeuDocumento)
    .AddPair('codigoNossoDocumento', fcodigoNossoDocumento)
    .AddPair('valorNominal', fvalorNominal)
    .AddPair('valorDesconto', fvalorDesconto)
    .AddPair('valorMoraMulta', fvalorMoraMulta)
    .AddPair('codigoTipoPagador', TipoPessoaToInt(fcodigoTipoPagador))
    .AddPair('documentoPagador', fdocumentoPagador)
    .AddPair('codigoTipoBeneficiario', TipoPessoaToInt(fcodigoTipoBeneficiario))
    .AddPair('documentoBeneficiario', fdocumentoBeneficiario)
    .AddPair('codigoTipoAvalista', TipoPessoaToInt(fcodigoTipoAvalista))
    .AddPair('documentoAvalista', fdocumentoAvalista);

end;

procedure TACBrPagamentoLancamentos.DoReadFromJSon(AJSon: TACBrJSONObject);
var
  i1, i2, i3: Integer;
  s4: String;
  d, m, a: word;
begin
  AJSon
    .Value('numeroDocumentoDebito', fnumeroDocumentoDebito)
    .Value('numeroCodigoBarras', fnumeroCodigoBarras)
    .Value('dataPagamento', s4)
    .Value('valorPagamento', fvalorPagamento)
    .Value('descricaoPagamento', fdescricaoPagamento)
    .Value('codigoSeuDocumento', fcodigoSeuDocumento)
    .Value('codigoNossoDocumento', fcodigoNossoDocumento)
    .Value('valorNominal', fvalorNominal)
    .Value('valorDesconto', fvalorDesconto)
    .Value('valorMoraMulta', fvalorMoraMulta)
    .Value('codigoTipoPagador', i1)
    .Value('documentoPagador', fdocumentoPagador)
    .Value('codigoTipoBeneficiario', i2)
    .Value('documentoBeneficiario', fdocumentoBeneficiario)
    .Value('codigoTipoAvalista', i3)
    .Value('documentoAvalista', fdocumentoAvalista);
  fcodigoTipoPagador := IntToTipoPessoa(i1);
  fcodigoTipoBeneficiario := IntToTipoPessoa(i2);
  fcodigoTipoAvalista := IntToTipoPessoa(i3);
  if (Length(s4) = 8) then
  begin
    d := Copy(s4, 1, 2).ToInteger;
    m := Copy(s4, 3, 2).ToInteger;
    a := Copy(s4, 5, 4).ToInteger;
    fdataPagamento := EncodeDate(a, m, d);
  end;
end;

procedure TACBrPagamentoLancamentos.Clear;
begin
  fnumeroDocumentoDebito := 0;
  fnumeroCodigoBarras := EmptyStr;
  fdataPagamento := 0;
  fvalorPagamento := 0;
  fdescricaoPagamento := EmptyStr;
  fcodigoSeuDocumento := EmptyStr;
  fcodigoNossoDocumento := EmptyStr;
  fvalorNominal := 0;
  fvalorDesconto := 0;
  fvalorMoraMulta := 0;
  fcodigoTipoPagador := ptpNenhum;
  fdocumentoPagador := '0';
  fcodigoTipoBeneficiario := ptpNenhum;
  fdocumentoBeneficiario := '0';
  fcodigoTipoAvalista := ptpNenhum;
  fdocumentoAvalista := '0';
end;

function TACBrPagamentoLancamentos.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroDocumentoDebito) and
    EstaVazio(fnumeroCodigoBarras) and
    EstaZerado(fdataPagamento) and
    EstaZerado(fvalorPagamento) and
    EstaVazio(fdescricaoPagamento) and
    EstaVazio(fcodigoSeuDocumento) and
    EstaVazio(fcodigoNossoDocumento) and
    EstaZerado(fvalorNominal) and
    EstaZerado(fvalorDesconto) and
    EstaZerado(fvalorMoraMulta) and
    (fcodigoTipoPagador = ptpNenhum) and
    EstaVazio(fdocumentoPagador) and
    (fcodigoTipoBeneficiario = ptpNenhum) and
    EstaVazio(fdocumentoBeneficiario) and
    (fcodigoTipoAvalista = ptpNenhum) and
    EstaVazio(fdocumentoAvalista);
end;

procedure TACBrPagamentoLancamentos.Assign(ASource: TACBrPagamentoLancamentos);
begin
  fnumeroDocumentoDebito := ASource.numeroDocumentoDebito;
  fnumeroCodigoBarras := ASource.numeroCodigoBarras;
  fdataPagamento := ASource.dataPagamento;
  fvalorPagamento := ASource.valorPagamento;
  fdescricaoPagamento := ASource.descricaoPagamento;
  fcodigoSeuDocumento := ASource.codigoSeuDocumento;
  fcodigoNossoDocumento := ASource.codigoNossoDocumento;
  fvalorNominal := ASource.valorNominal;
  fvalorDesconto := ASource.valorDesconto;
  fvalorMoraMulta := ASource.valorMoraMulta;
  fcodigoTipoPagador := ASource.codigoTipoPagador;
  fdocumentoPagador := ASource.documentoPagador;
  fcodigoTipoBeneficiario := ASource.codigoTipoBeneficiario;
  fdocumentoBeneficiario := ASource.documentoBeneficiario;
  fcodigoTipoAvalista := ASource.codigoTipoAvalista;
  fdocumentoAvalista := ASource.documentoAvalista;
end;

end.
