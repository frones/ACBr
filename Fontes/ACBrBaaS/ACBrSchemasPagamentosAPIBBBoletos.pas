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

unit ACBrSchemasPagamentosAPIBBBoletos;

interface

uses
  Classes, SysUtils, ACBrAPIBase, ACBrJSON,
  ACBrSchemasPagamentosAPIBB;

type

  { TACBrPagamentosBBBoletoBase }

  TACBrPagamentosBBBoletoBase = class(TACBrAPISchema)
  private
    fcodigo: String;
    fdataAgendamento: TDateTime;
    fdataVencimento: TDateTime;
    fdocumentoAvalista: Int64;
    fdocumentoBeneficiario: Int64;
    fdocumentoPagador: Int64;
    fnomeAvalista: String;
    fnomeBeneficiario: String;
    fnomePagador: String;
    fnossoDocumento: String;
    fseuDocumento: String;
    ftexto: String;
    ftipoPessoaAvalista: Integer;
    ftipoPessoaBeneficiario: Integer;
    ftipoPessoaPagador: Integer;
    fvalorDesconto: Double;
    fvalorMoraMulta: Double;
    fvalorNominal: Double;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBBoletoBase);

    property codigo: String read fcodigo write fcodigo;
    property nossoDocumento: String read fnossoDocumento write fnossoDocumento;
    property seuDocumento: String read fseuDocumento write fseuDocumento;
    property tipoPessoaBeneficiario: Integer read ftipoPessoaBeneficiario write ftipoPessoaBeneficiario;
    property documentoBeneficiario: Int64 read fdocumentoBeneficiario write fdocumentoBeneficiario;
    property nomeBeneficiario: String read fnomeBeneficiario write fnomeBeneficiario;
    property tipoPessoaPagador: Integer read ftipoPessoaPagador write ftipoPessoaPagador;
    property documentoPagador: Int64 read fdocumentoPagador write fdocumentoPagador;
    property nomePagador: String read fnomePagador write fnomePagador;
    property tipoPessoaAvalista: Integer read ftipoPessoaAvalista write ftipoPessoaAvalista;
    property documentoAvalista: Int64 read fdocumentoAvalista write fdocumentoAvalista;
    property nomeAvalista: String read fnomeAvalista write fnomeAvalista;
    property dataVencimento: TDateTime read fdataVencimento write fdataVencimento;
    property dataAgendamento: TDateTime read fdataAgendamento write fdataAgendamento;
    property valorNominal: Double read fvalorNominal write fvalorNominal;
    property valorMoraMulta: Double read fvalorMoraMulta write fvalorMoraMulta;
    property valorDesconto: Double read fvalorDesconto write fvalorDesconto;
    property texto: String read ftexto write ftexto;
  end;

  { TACBrPagamentosBBBoletoLista }

  TACBrPagamentosBBBoletoLista = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBBoletoBase;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBBoletoBase);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBBoletoBase): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBBoletoBase);
    function New: TACBrPagamentosBBBoletoBase;
    property Items[aIndex: Integer]: TACBrPagamentosBBBoletoBase read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBConsultaRespostaBoleto }

  TACBrPagamentosBBConsultaRespostaBoleto = class(TACBrPagamentosBBConsultaRespostaBase)
  private
    flistaDevolucao: TACBrPagamentosBBDevolucaoLista;
    flistaPagamentos: TACBrPagamentosBBBoletoLista;
    function GetlistaDevolucao: TACBrPagamentosBBDevolucaoLista;
    function GetlistaPagamentos: TACBrPagamentosBBBoletoLista;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBConsultaRespostaBoleto);

    property listaPagamentos: TACBrPagamentosBBBoletoLista read GetlistaPagamentos write flistaPagamentos;
    property listaDevolucao: TACBrPagamentosBBDevolucaoLista read GetlistaDevolucao write flistaDevolucao;
  end;

  { TACBrPagamentosBBLancamentoBoletoBase }

  TACBrPagamentosBBLancamentoBoletoBase = class(TACBrAPISchema)
  private
    fcodigoNossoDocumento: String;
    fcodigoSeuDocumento: String;
    fcodigoTipoAvalista: TACBrPagamentosBBTipoBeneficiario;
    fcodigoTipoBeneficiario: TACBrPagamentosBBTipoBeneficiario;
    fcodigoTipoPagador: Integer;
    fdataPagamento: TDateTime;
    fdescricaoPagamento: String;
    fdocumentoAvalista: Int64;
    fdocumentoBeneficiario: Int64;
    fdocumentoPagador: Int64;
    fnumeroCodigoBarras: String;
    fnumeroDocumentoDebito: Integer;
    fvalorDesconto: Double;
    fvalorMoraMulta: Double;
    fvalorNominal: Double;
    fvalorPagamento: Double;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLancamentoBoletoBase);

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
    property codigoTipoPagador: Integer read fcodigoTipoPagador write fcodigoTipoPagador;
    property documentoPagador: Int64 read fdocumentoPagador write fdocumentoPagador;
    property codigoTipoBeneficiario: TACBrPagamentosBBTipoBeneficiario read fcodigoTipoBeneficiario write fcodigoTipoBeneficiario;
    property documentoBeneficiario: Int64 read fdocumentoBeneficiario write fdocumentoBeneficiario;
    property codigoTipoAvalista: TACBrPagamentosBBTipoBeneficiario read fcodigoTipoAvalista write fcodigoTipoAvalista;
    property documentoAvalista: Int64 read fdocumentoAvalista write fdocumentoAvalista;
  end;

  { TACBrPagamentosBBLancamentosBoletoBase }

  TACBrPagamentosBBLancamentosBoletoBase = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBLancamentoBoletoBase;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBLancamentoBoletoBase);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBLancamentoBoletoBase): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBLancamentoBoletoBase);
    function New: TACBrPagamentosBBLancamentoBoletoBase;
    property Items[aIndex: Integer]: TACBrPagamentosBBLancamentoBoletoBase read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBLancamentoBoleto }

  TACBrPagamentosBBLancamentoBoleto = class(TACBrPagamentosBBLancamentoBoletoBase)
  private
    fcodigoIdentificadorPagamento: Integer;
    ferros: TACBrPagamenosBBTransferenciaErros;
    findicadorAceite: String;
    fnomeAvalista: String;
    fnomeBeneficiario: String;
    fnomePagador: String;
    function Geterros: TACBrPagamenosBBTransferenciaErros;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLancamentoBoleto);

    property codigoIdentificadorPagamento: Integer read fcodigoIdentificadorPagamento write fcodigoIdentificadorPagamento;
    property nomePagador: String read fnomePagador write fnomePagador;
    property nomeBeneficiario: String read fnomeBeneficiario write fnomeBeneficiario;
    property nomeAvalista: String read fnomeAvalista write fnomeAvalista;
    property indicadorAceite: String read findicadorAceite write findicadorAceite;
    property erros: TACBrPagamenosBBTransferenciaErros read Geterros write ferros;
  end;

  { TACBrPagamentosBBLancamentosBoleto }

  TACBrPagamentosBBLancamentosBoleto = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBLancamentoBoleto;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBLancamentoBoleto);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aItem: TACBrPagamentosBBLancamentoBoleto): Integer;
    procedure Insert(aIndex: Integer; aItem: TACBrPagamentosBBLancamentoBoleto);
    function New: TACBrPagamentosBBLancamentoBoleto;
    property Items[aIndex: Integer]: TACBrPagamentosBBLancamentoBoleto read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBConsultaLoteRespostaBoleto }

  TACBrPagamentosBBConsultaLoteRespostaBoleto = class(TACBrAPISchema)
  private
    festadoRequisicao: TACBrPagamentosBBEstadoRequisicao;
    flancamentos: TACBrPagamentosBBLancamentosBoleto;
    fquantidadeLancamentos: Integer;
    fquantidadeLancamentosValidos: Integer;
    fvalorLancamentos: Double;
    fvalorLancamentosValidos: Double;
    function Getlancamentos: TACBrPagamentosBBLancamentosBoleto;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBConsultaLoteRespostaBoleto);

    property estadoRequisicao: TACBrPagamentosBBEstadoRequisicao read festadoRequisicao write festadoRequisicao;
    property quantidadeLancamentos: Integer read fquantidadeLancamentos write fquantidadeLancamentos;
    property valorLancamentos: Double read fvalorLancamentos write fvalorLancamentos;
    property quantidadeLancamentosValidos: Integer read fquantidadeLancamentosValidos write fquantidadeLancamentosValidos;
    property valorLancamentosValidos: Double read fvalorLancamentosValidos write fvalorLancamentosValidos;
    property lancamentos: TACBrPagamentosBBLancamentosBoleto read Getlancamentos write flancamentos;
  end;

  { TACBrPagamentosBBLotePagamentosRequisicaoBoletos }

  TACBrPagamentosBBLotePagamentosRequisicaoBoletos = class(TACBrPagamentoBBLotePagamentosRequisicao)
  private
    flancamentos: TACBrPagamentosBBLancamentosBoletoBase;
    function Getlancamentos: TACBrPagamentosBBLancamentosBoletoBase;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLotePagamentosRequisicaoBoletos);

    property lancamentos: TACBrPagamentosBBLancamentosBoletoBase read Getlancamentos write flancamentos;
  end;

implementation

uses
  ACBrUtil.Base;

{ TACBrPagamentosBBLancamentosBoleto }

function TACBrPagamentosBBLancamentosBoleto.GetItem(aIndex: Integer): TACBrPagamentosBBLancamentoBoleto;
begin
  Result := TACBrPagamentosBBLancamentoBoleto(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBLancamentosBoleto.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBLancamentoBoleto);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBLancamentosBoleto.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBLancamentosBoleto.Add(
  aItem: TACBrPagamentosBBLancamentoBoleto): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBLancamentosBoleto.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBLancamentoBoleto);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBLancamentosBoleto.New: TACBrPagamentosBBLancamentoBoleto;
begin
  Result := TACBrPagamentosBBLancamentoBoleto.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBLancamentosBoletoBase }

function TACBrPagamentosBBLancamentosBoletoBase.GetItem(aIndex: Integer): TACBrPagamentosBBLancamentoBoletoBase;
begin
  Result := TACBrPagamentosBBLancamentoBoletoBase(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBLancamentosBoletoBase.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBLancamentoBoletoBase);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBLancamentosBoletoBase.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBLancamentosBoletoBase.Add(
  aItem: TACBrPagamentosBBLancamentoBoletoBase): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBLancamentosBoletoBase.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBLancamentoBoletoBase);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBLancamentosBoletoBase.New: TACBrPagamentosBBLancamentoBoletoBase;
begin
  Result := TACBrPagamentosBBLancamentoBoletoBase.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBBoletoLista }

function TACBrPagamentosBBBoletoLista.GetItem(aIndex: Integer): TACBrPagamentosBBBoletoBase;
begin
  Result := TACBrPagamentosBBBoletoBase(inherited Items[aIndex]);
end;

procedure TACBrPagamentosBBBoletoLista.SetItem(aIndex: Integer; aValue: TACBrPagamentosBBBoletoBase);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrPagamentosBBBoletoLista.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrPagamentosBBBoletoLista.Add(aItem: TACBrPagamentosBBBoletoBase
  ): Integer;
begin
  Result := inherited Add(aItem);
end;

procedure TACBrPagamentosBBBoletoLista.Insert(aIndex: Integer;
  aItem: TACBrPagamentosBBBoletoBase);
begin
  inherited Insert(aIndex, aItem);
end;

function TACBrPagamentosBBBoletoLista.New: TACBrPagamentosBBBoletoBase;
begin
  Result := TACBrPagamentosBBBoletoBase.Create;
  Self.Add(Result);
end;

{ TACBrPagamentosBBLotePagamentosRequisicaoBoletos }

function TACBrPagamentosBBLotePagamentosRequisicaoBoletos.Getlancamentos: TACBrPagamentosBBLancamentosBoletoBase;
begin
  if (not Assigned(flancamentos)) then
    flancamentos := TACBrPagamentosBBLancamentosBoletoBase.Create('lancamentos');
  Result := flancamentos;
end;

procedure TACBrPagamentosBBLotePagamentosRequisicaoBoletos.AssignSchema(
  aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBLotePagamentosRequisicaoBoletos) then
    Assign(TACBrPagamentosBBLotePagamentosRequisicaoBoletos(ASource));
end;

procedure TACBrPagamentosBBLotePagamentosRequisicaoBoletos.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if Assigned(flancamentos) then
    flancamentos.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBLotePagamentosRequisicaoBoletos.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  if Assigned(flancamentos) then
    flancamentos.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBLotePagamentosRequisicaoBoletos.Destroy;
begin
  if Assigned(flancamentos) then
    flancamentos.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBLotePagamentosRequisicaoBoletos.Clear;
begin
  if Assigned(flancamentos) then
    flancamentos.Clear;
end;

function TACBrPagamentosBBLotePagamentosRequisicaoBoletos.IsEmpty: Boolean;
begin
  Result := True;

  if Assigned(flancamentos) then
    Result := Result and flancamentos.IsEmpty;
end;

procedure TACBrPagamentosBBLotePagamentosRequisicaoBoletos.Assign(aSource: TACBrPagamentosBBLotePagamentosRequisicaoBoletos);
begin
  if Assigned(flancamentos) then
    flancamentos.Assign(ASource.lancamentos);
end;

{ TACBrPagamentosBBConsultaLoteRespostaBoleto }

function TACBrPagamentosBBConsultaLoteRespostaBoleto.Getlancamentos: TACBrPagamentosBBLancamentosBoleto;
begin
  if (not Assigned(flancamentos)) then
    flancamentos := TACBrPagamentosBBLancamentosBoleto.Create('lancamentos');
  Result := flancamentos;
end;

procedure TACBrPagamentosBBConsultaLoteRespostaBoleto.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBConsultaLoteRespostaBoleto) then
    Assign(TACBrPagamentosBBConsultaLoteRespostaBoleto(ASource));
end;

procedure TACBrPagamentosBBConsultaLoteRespostaBoleto.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('estadoRequisicao', PagamentosBBEstadoRequisicaoToInteger(festadoRequisicao))
    .AddPair('quantidadeLancamentos', fquantidadeLancamentos)
    .AddPair('valorLancamentos', fvalorLancamentos)
    .AddPair('quantidadeLancamentosValidos', fquantidadeLancamentosValidos)
    .AddPair('valorLancamentosValidos', fvalorLancamentosValidos);

  if Assigned(flancamentos) then
    flancamentos.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBConsultaLoteRespostaBoleto.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
begin
  aJSon
    .Value('estadoRequisicao', i)
    .Value('quantidadeLancamentos', fquantidadeLancamentos)
    .Value('valorLancamentos', fvalorLancamentos)
    .Value('quantidadeLancamentosValidos', fquantidadeLancamentosValidos)
    .Value('valorLancamentosValidos', fvalorLancamentosValidos);

  if NaoEstaZerado(i) then
    festadoRequisicao := IntegerToPagamentosBBEstadoRequisicao(i);

  if Assigned(flancamentos) then
    flancamentos.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBConsultaLoteRespostaBoleto.Destroy;
begin
  if Assigned(flancamentos) then
    flancamentos.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBConsultaLoteRespostaBoleto.Clear;
begin
  festadoRequisicao := TACBrPagamentosBBEstadoRequisicao(0);
  fquantidadeLancamentos := 0;
  fvalorLancamentos := 0;
  fquantidadeLancamentosValidos := 0;
  fvalorLancamentosValidos := 0;

  if Assigned(flancamentos) then
    flancamentos.Clear;
end;

function TACBrPagamentosBBConsultaLoteRespostaBoleto.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(Ord(festadoRequisicao)) and
    EstaZerado(fquantidadeLancamentos) and
    EstaZerado(fvalorLancamentos) and
    EstaZerado(fquantidadeLancamentosValidos) and
    EstaZerado(fvalorLancamentosValidos);

  if Assigned(flancamentos) then
    Result := Result and flancamentos.IsEmpty;
end;

procedure TACBrPagamentosBBConsultaLoteRespostaBoleto.Assign(aSource: TACBrPagamentosBBConsultaLoteRespostaBoleto);
begin 
  festadoRequisicao := ASource.estadoRequisicao;
  fquantidadeLancamentos := ASource.quantidadeLancamentos;
  fvalorLancamentos := ASource.valorLancamentos;
  fquantidadeLancamentosValidos := ASource.quantidadeLancamentosValidos;
  fvalorLancamentosValidos := ASource.valorLancamentosValidos;
  lancamentos.Assign(ASource.lancamentos);
end;

{ TACBrPagamentosBBLancamentoBoleto }

function TACBrPagamentosBBLancamentoBoleto.Geterros: TACBrPagamenosBBTransferenciaErros;
begin
  if (not Assigned(ferros)) then
    ferros := TACBrPagamenosBBTransferenciaErros.Create('erros');
  Result := ferros;
end;

procedure TACBrPagamentosBBLancamentoBoleto.AssignSchema(aSource: TACBrAPISchema
  );
begin
  if (aSource is TACBrPagamentosBBLancamentoBoleto) then
    Assign(TACBrPagamentosBBLancamentoBoleto(ASource));
end;

procedure TACBrPagamentosBBLancamentoBoleto.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('codigoIdentificadorPagamento', fcodigoIdentificadorPagamento)
    .AddPair('nomePagador', fnomePagador)
    .AddPair('nomeBeneficiario', fnomeBeneficiario)
    .AddPair('nomeAvalista', fnomeAvalista)
    .AddPair('indicadorAceite', findicadorAceite);

  if Assigned(ferros) then
    ferros.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBLancamentoBoleto.DoReadFromJSon(
  aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('codigoIdentificadorPagamento', fcodigoIdentificadorPagamento)
    .Value('nomePagador', fnomePagador)
    .Value('nomeBeneficiario', fnomeBeneficiario)
    .Value('nomeAvalista', fnomeAvalista)
    .Value('indicadorAceite', findicadorAceite);

  if Assigned(ferros) then
    ferros.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBLancamentoBoleto.Destroy;
begin
  if Assigned(ferros) then
    ferros.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBLancamentoBoleto.Clear;
begin
  fcodigoIdentificadorPagamento := 0;
  fnomePagador := EmptyStr;
  fnomeBeneficiario := EmptyStr;
  fnomeAvalista := EmptyStr;
  findicadorAceite := EmptyStr;

  if Assigned(ferros) then
    ferros.Clear;
end;

function TACBrPagamentosBBLancamentoBoleto.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fcodigoIdentificadorPagamento) and
    EstaVazio(fnomePagador) and
    EstaVazio(fnomeBeneficiario) and
    EstaVazio(fnomeAvalista) and
    EstaVazio(findicadorAceite);

  if Assigned(ferros) then
    Result := Result and ferros.IsEmpty;
end;

procedure TACBrPagamentosBBLancamentoBoleto.Assign(aSource: TACBrPagamentosBBLancamentoBoleto);
begin
  fcodigoIdentificadorPagamento := ASource.codigoIdentificadorPagamento;
  fnomePagador := ASource.nomePagador;
  fnomeBeneficiario := ASource.nomeBeneficiario;
  fnomeAvalista := ASource.nomeAvalista;
  findicadorAceite := ASource.indicadorAceite;
  erros.Assign(ASource.erros);
end;

{ TACBrPagamentosBBLancamentoBoletoBase }

procedure TACBrPagamentosBBLancamentoBoletoBase.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBLancamentoBoletoBase) then
    Assign(TACBrPagamentosBBLancamentoBoletoBase(ASource));
end;

procedure TACBrPagamentosBBLancamentoBoletoBase.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('numeroDocumentoDebito', fnumeroDocumentoDebito)
    .AddPair('numeroCodigoBarras', fnumeroCodigoBarras)
    .AddPair('dataPagamento', DateToStr(fdataPagamento))
    .AddPair('valorPagamento', fvalorPagamento)
    .AddPair('descricaoPagamento', fdescricaoPagamento)
    .AddPair('codigoSeuDocumento', fcodigoSeuDocumento)
    .AddPair('codigoNossoDocumento', fcodigoNossoDocumento)
    .AddPair('valorNominal', fvalorNominal)
    .AddPair('valorDesconto', fvalorDesconto)
    .AddPair('valorMoraMulta', fvalorMoraMulta)
    .AddPair('codigoTipoPagador', fcodigoTipoPagador)
    .AddPair('documentoPagador', fdocumentoPagador)
    .AddPair('codigoTipoBeneficiario', Integer(fcodigoTipoBeneficiario))
    .AddPair('documentoBeneficiario', fdocumentoBeneficiario)
    .AddPair('codigoTipoAvalista', Integer(fcodigoTipoAvalista))
    .AddPair('documentoAvalista', fdocumentoAvalista);
end;

procedure TACBrPagamentosBBLancamentoBoletoBase.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i1, i2: Integer;
begin
  aJSon
    .Value('numeroDocumentoDebito', fnumeroDocumentoDebito)
    .Value('numeroCodigoBarras', fnumeroCodigoBarras)
    .Value('dataPagamento', fdataPagamento)
    .Value('valorPagamento', fvalorPagamento)
    .Value('descricaoPagamento', fdescricaoPagamento)
    .Value('codigoSeuDocumento', fcodigoSeuDocumento)
    .Value('codigoNossoDocumento', fcodigoNossoDocumento)
    .Value('valorNominal', fvalorNominal)
    .Value('valorDesconto', fvalorDesconto)
    .Value('valorMoraMulta', fvalorMoraMulta)
    .Value('codigoTipoPagador', fcodigoTipoPagador)
    .Value('documentoPagador', fdocumentoPagador)
    .Value('codigoTipoBeneficiario', i1)
    .Value('documentoBeneficiario', fdocumentoBeneficiario)
    .Value('codigoTipoAvalista', i2)
    .Value('documentoAvalista', fdocumentoAvalista);

  if NaoEstaZerado(i1) then
    fcodigoTipoBeneficiario := IntegerToPagamentosBBTipoBeneficiario(i1);

  if NaoEstaZerado(i2) then
    fcodigoTipoAvalista := IntegerToPagamentosBBTipoBeneficiario(i2);
end;

procedure TACBrPagamentosBBLancamentoBoletoBase.Clear;
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
  fcodigoTipoPagador := 0;
  fdocumentoPagador := 0;
  fcodigoTipoBeneficiario := ptbNenhum;
  fdocumentoBeneficiario := 0;
  fcodigoTipoAvalista := ptbNenhum;
  fdocumentoAvalista := 0;
end;

function TACBrPagamentosBBLancamentoBoletoBase.IsEmpty: Boolean;
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
    EstaZerado(fcodigoTipoPagador) and
    EstaZerado(fdocumentoPagador) and
    EstaZerado(Ord(fcodigoTipoBeneficiario)) and
    EstaZerado(fdocumentoBeneficiario) and
    EstaZerado(Ord(fcodigoTipoAvalista)) and
    EstaZerado(fdocumentoAvalista);
end;

procedure TACBrPagamentosBBLancamentoBoletoBase.Assign(aSource: TACBrPagamentosBBLancamentoBoletoBase);
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

{ TACBrPagamentosBBConsultaRespostaBoleto }

function TACBrPagamentosBBConsultaRespostaBoleto.GetlistaDevolucao: TACBrPagamentosBBDevolucaoLista;
begin
  if (not Assigned(flistaDevolucao)) then
    flistaDevolucao := TACBrPagamentosBBDevolucaoLista.Create('listaDevolucao');
  Result := flistaDevolucao;
end;

function TACBrPagamentosBBConsultaRespostaBoleto.GetlistaPagamentos: TACBrPagamentosBBBoletoLista;
begin
  if (not Assigned(flistaPagamentos)) then
    flistaPagamentos := TACBrPagamentosBBBoletoLista.Create('listaPagamentos');
  Result := flistaPagamentos;
end;

procedure TACBrPagamentosBBConsultaRespostaBoleto.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBConsultaRespostaBoleto) then
    Assign(TACBrPagamentosBBConsultaRespostaBoleto(ASource));
end;

procedure TACBrPagamentosBBConsultaRespostaBoleto.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if Assigned(flistaPagamentos) then
    flistaPagamentos.WriteToJSon(aJSon);

  if Assigned(flistaDevolucao) then
    flistaDevolucao.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBConsultaRespostaBoleto.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  if Assigned(flistaPagamentos) then
    flistaPagamentos.ReadFromJSon(aJSon);

  if Assigned(flistaDevolucao) then
    flistaDevolucao.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBConsultaRespostaBoleto.Destroy;
begin
  if Assigned(flistaDevolucao) then
    flistaDevolucao.Free;
  if Assigned(flistaPagamentos) then
    flistaPagamentos.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBConsultaRespostaBoleto.Clear;
begin
  if Assigned(flistaPagamentos) then
    flistaPagamentos.Clear;

  if Assigned(flistaDevolucao) then
    flistaDevolucao.Clear;
end;

function TACBrPagamentosBBConsultaRespostaBoleto.IsEmpty: Boolean;
begin
  Result := True;

  if Assigned(flistaPagamentos) then
    Result := Result and flistaPagamentos.IsEmpty;

  if Assigned(flistaDevolucao) then
    Result := Result and flistaDevolucao.IsEmpty;
end;

procedure TACBrPagamentosBBConsultaRespostaBoleto.Assign(aSource: TACBrPagamentosBBConsultaRespostaBoleto);
begin
  listaPagamentos.Assign(ASource.listaPagamentos);
  listaDevolucao.Assign(ASource.listaDevolucao);
end;

{ TACBrPagamentosBBBoletoBase }

procedure TACBrPagamentosBBBoletoBase.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBBoletoBase) then
    Assign(TACBrPagamentosBBBoletoBase(ASource));
end;

procedure TACBrPagamentosBBBoletoBase.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('codigo', fcodigo)
    .AddPair('nossoDocumento', fnossoDocumento)
    .AddPair('seuDocumento', fseuDocumento)
    .AddPair('tipoPessoaBeneficiario', ftipoPessoaBeneficiario)
    .AddPair('documentoBeneficiario', fdocumentoBeneficiario)
    .AddPair('nomeBeneficiario', fnomeBeneficiario)
    .AddPair('tipoPessoaPagador', ftipoPessoaPagador)
    .AddPair('documentoPagador', fdocumentoPagador)
    .AddPair('nomePagador', fnomePagador)
    .AddPair('tipoPessoaAvalista', ftipoPessoaAvalista)
    .AddPair('documentoAvalista', fdocumentoAvalista)
    .AddPair('nomeAvalista', fnomeAvalista)
    .AddPair('dataVencimento', DateToStr(fdataVencimento))
    .AddPair('dataAgendamento', DateToStr(fdataAgendamento))
    .AddPair('valorNominal', fvalorNominal)
    .AddPair('valorMoraMulta', fvalorMoraMulta)
    .AddPair('valorDesconto', fvalorDesconto)
    .AddPair('texto', ftexto);
end;

procedure TACBrPagamentosBBBoletoBase.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('codigo', fcodigo)
    .Value('nossoDocumento', fnossoDocumento)
    .Value('seuDocumento', fseuDocumento)
    .Value('tipoPessoaBeneficiario', ftipoPessoaBeneficiario)
    .Value('documentoBeneficiario', fdocumentoBeneficiario)
    .Value('nomeBeneficiario', fnomeBeneficiario)
    .Value('tipoPessoaPagador', ftipoPessoaPagador)
    .Value('documentoPagador', fdocumentoPagador)
    .Value('nomePagador', fnomePagador)
    .Value('tipoPessoaAvalista', ftipoPessoaAvalista)
    .Value('documentoAvalista', fdocumentoAvalista)
    .Value('nomeAvalista', fnomeAvalista)
    .Value('dataVencimento', fdataVencimento)
    .Value('dataAgendamento', fdataAgendamento)
    .Value('valorNominal', fvalorNominal)
    .Value('valorMoraMulta', fvalorMoraMulta)
    .Value('valorDesconto', fvalorDesconto)
    .Value('texto', ftexto);
end;

procedure TACBrPagamentosBBBoletoBase.Clear;
begin
  fcodigo := EmptyStr;
  fnossoDocumento := EmptyStr;
  fseuDocumento := EmptyStr;
  ftipoPessoaBeneficiario := 0;
  fdocumentoBeneficiario := 0;
  fnomeBeneficiario := EmptyStr;
  ftipoPessoaPagador := 0;
  fdocumentoPagador := 0;
  fnomePagador := EmptyStr;
  ftipoPessoaAvalista := 0;
  fdocumentoAvalista := 0;
  fnomeAvalista := EmptyStr;
  fdataVencimento := 0;
  fdataAgendamento := 0;
  fvalorNominal := 0;
  fvalorMoraMulta := 0;
  fvalorDesconto := 0;
  ftexto := EmptyStr;
end;

function TACBrPagamentosBBBoletoBase.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fcodigo) and
    EstaVazio(fnossoDocumento) and
    EstaVazio(fseuDocumento) and
    EstaZerado(ftipoPessoaBeneficiario) and
    EstaZerado(fdocumentoBeneficiario) and
    EstaVazio(fnomeBeneficiario) and
    EstaZerado(ftipoPessoaPagador) and
    EstaZerado(fdocumentoPagador) and
    EstaVazio(fnomePagador) and
    EstaZerado(ftipoPessoaAvalista) and
    EstaZerado(fdocumentoAvalista) and
    EstaVazio(fnomeAvalista) and
    EstaZerado(fdataVencimento) and
    EstaZerado(fdataAgendamento) and
    EstaZerado(fvalorNominal) and
    EstaZerado(fvalorMoraMulta) and
    EstaZerado(fvalorDesconto) and
    EstaVazio(ftexto);
end;

procedure TACBrPagamentosBBBoletoBase.Assign(aSource: TACBrPagamentosBBBoletoBase);
begin
  fcodigo := ASource.codigo;
  fnossoDocumento := ASource.nossoDocumento;
  fseuDocumento := ASource.seuDocumento;
  ftipoPessoaBeneficiario := ASource.tipoPessoaBeneficiario;
  fdocumentoBeneficiario := ASource.documentoBeneficiario;
  fnomeBeneficiario := ASource.nomeBeneficiario;
  ftipoPessoaPagador := ASource.tipoPessoaPagador;
  fdocumentoPagador := ASource.documentoPagador;
  fnomePagador := ASource.nomePagador;
  ftipoPessoaAvalista := ASource.tipoPessoaAvalista;
  fdocumentoAvalista := ASource.documentoAvalista;
  fnomeAvalista := ASource.nomeAvalista;
  fdataVencimento := ASource.dataVencimento;
  fdataAgendamento := ASource.dataAgendamento;
  fvalorNominal := ASource.valorNominal;
  fvalorMoraMulta := ASource.valorMoraMulta;
  fvalorDesconto := ASource.valorDesconto;
  ftexto := ASource.texto;
end;

end.

