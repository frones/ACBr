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

unit ACBrSchemasPagamentosAPIBBGPS;

interface

uses
  Classes, SysUtils, ACBrAPIBase, ACBrJSON,
  ACBrSchemasPagamentosAPIBB;

type

  { TACBrPagamentosBBGPS }

  TACBrPagamentosBBGPS = class(TACBrAPISchema)
  private
    fcodigo: Integer;
    fidentificacaoContribuinte: Integer;
    fidentificacaoGPS: String;
    fmesAnoCompetencia: Integer;
    ftexto: String;
    ftipoContribuinte: Integer;
    fvalorAtualizacaoMonetaria: Double;
    fvalorINSS: Double;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBGPS);

    property codigo: Integer read fcodigo write fcodigo;
    property tipoContribuinte: Integer read ftipoContribuinte write ftipoContribuinte;
    property identificacaoContribuinte: Integer read fidentificacaoContribuinte write fidentificacaoContribuinte;
    property identificacaoGPS: String read fidentificacaoGPS write fidentificacaoGPS;
    property mesAnoCompetencia: Integer read fmesAnoCompetencia write fmesAnoCompetencia;
    property valorINSS: Double read fvalorINSS write fvalorINSS;
    property valorAtualizacaoMonetaria: Double read fvalorAtualizacaoMonetaria write fvalorAtualizacaoMonetaria;
    property texto: String read ftexto write ftexto;
  end;

  { TACBrPagamentosBBGPSLista }

  TACBrPagamentosBBGPSLista = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBGPS;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBGPS);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentosBBGPS): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentosBBGPS);
    function New: TACBrPagamentosBBGPS;
    property Items[aIndex: Integer]: TACBrPagamentosBBGPS read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBConsultaRespostaGPS }

  TACBrPagamentosBBConsultaRespostaGPS = class(TACBrPagamentosBBConsultaRespostaBase)
  private
    flistaDevolucao: TACBrPagamentosBBDevolucaoListaBase;
    flistaPagamentos: TACBrPagamentosBBGPSLista;
    function GetlistaDevolucao: TACBrPagamentosBBDevolucaoListaBase;
    function GetlistaPagamentos: TACBrPagamentosBBGPSLista;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBConsultaRespostaGPS);

    property listaPagamentos: TACBrPagamentosBBGPSLista read GetlistaPagamentos write flistaPagamentos;
    property listaDevolucao: TACBrPagamentosBBDevolucaoListaBase read GetlistaDevolucao write flistaDevolucao;
  end;

  { TACBrPagamentosBBLancamentoGPSBase }

  TACBrPagamentosBBLancamentoGPSBase = class(TACBrAPISchema)
  private
    fcodigoIdentificadorTributoGuiaPrevidenciaSocial: String;
    fcodigoReceitaTributoGuiaPrevidenciaSocial: Integer;
    fcodigoSeuDocumento: String;
    fcodigoTipoContribuinteGuiaPrevidenciaSocial: TACBrPagamentosBBTipoContribuinte;
    fdataPagamento: TDateTime;
    fmesAnoCompetenciaGuiaPrevidenciaSocial: Integer;
    fnumeroDocumentoDebito: Integer;
    fnumeroIdentificacaoContribuinteGuiaPrevidenciaSocial: Integer;
    ftextoDescricaoPagamento: String;
    fvalorAtualizacaoMonetarioGuiaPrevidenciaSocial: Double;
    fvalorOutroEntradaGuiaPrevidenciaSocial: Double;
    fvalorPagamento: Double;
    fvalorPrevistoInstNacSeguridadeSocialGuiaPrevidenciaSocial: Double;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLancamentoGPSBase);

    property dataPagamento: TDateTime read fdataPagamento write fdataPagamento;
    property valorPagamento: Double read fvalorPagamento write fvalorPagamento;
    property numeroDocumentoDebito: Integer read fnumeroDocumentoDebito write fnumeroDocumentoDebito;
    property codigoSeuDocumento: String read fcodigoSeuDocumento write fcodigoSeuDocumento;
    property textoDescricaoPagamento: String read ftextoDescricaoPagamento write ftextoDescricaoPagamento;
    property codigoReceitaTributoGuiaPrevidenciaSocial: Integer read fcodigoReceitaTributoGuiaPrevidenciaSocial write fcodigoReceitaTributoGuiaPrevidenciaSocial;
    property codigoTipoContribuinteGuiaPrevidenciaSocial: TACBrPagamentosBBTipoContribuinte read fcodigoTipoContribuinteGuiaPrevidenciaSocial write fcodigoTipoContribuinteGuiaPrevidenciaSocial;
    property numeroIdentificacaoContribuinteGuiaPrevidenciaSocial: Integer read fnumeroIdentificacaoContribuinteGuiaPrevidenciaSocial write fnumeroIdentificacaoContribuinteGuiaPrevidenciaSocial;
    property codigoIdentificadorTributoGuiaPrevidenciaSocial: String read fcodigoIdentificadorTributoGuiaPrevidenciaSocial write fcodigoIdentificadorTributoGuiaPrevidenciaSocial;
    property mesAnoCompetenciaGuiaPrevidenciaSocial: Integer read fmesAnoCompetenciaGuiaPrevidenciaSocial write fmesAnoCompetenciaGuiaPrevidenciaSocial;
    property valorPrevistoInstNacSeguridadeSocialGuiaPrevidenciaSocial: Double read fvalorPrevistoInstNacSeguridadeSocialGuiaPrevidenciaSocial write fvalorPrevistoInstNacSeguridadeSocialGuiaPrevidenciaSocial;
    property valorOutroEntradaGuiaPrevidenciaSocial: Double read fvalorOutroEntradaGuiaPrevidenciaSocial write fvalorOutroEntradaGuiaPrevidenciaSocial;
    property valorAtualizacaoMonetarioGuiaPrevidenciaSocial: Double read fvalorAtualizacaoMonetarioGuiaPrevidenciaSocial write fvalorAtualizacaoMonetarioGuiaPrevidenciaSocial;
  end;

  { TACBrPagamentosBBLancamentosGPSBase }

  TACBrPagamentosBBLancamentosGPSBase = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBLancamentoGPSBase;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBLancamentoGPSBase);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentosBBLancamentoGPSBase): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentosBBLancamentoGPSBase);
    function New: TACBrPagamentosBBLancamentoGPSBase;
    property Items[aIndex: Integer]: TACBrPagamentosBBLancamentoGPSBase read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBLotePagamentosRequisicaoGPS }

  TACBrPagamentosBBLotePagamentosRequisicaoGPS = class(TACBrPagamentoBBLotePagamentosRequisicao)
  private
    flancamentos: TACBrPagamentosBBLancamentosGPSBase;
    function Getlancamentos: TACBrPagamentosBBLancamentosGPSBase;
  protected
    procedure AssignSchema(aSource: TACBrPagamentoBBLotePagamentosRequisicao); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLotePagamentosRequisicaoGPS);

    property lancamentos: TACBrPagamentosBBLancamentosGPSBase read Getlancamentos write flancamentos;
  end;

  { TACBrPagamentosBBLancamentoGPS }

  TACBrPagamentosBBLancamentoGPS = class(TACBrPagamentosBBLancamentoGPSBase)
  private
    fcodigoIdentificadorPagamento: Integer;
    ferros: TACBrPagamentosBBLancamentoErros;
    findicadorMovimentoAceito: String;
    fnomeConvenente: String;
    ftextoGuiaPrevidenciaSocial: String;
    function Geterros: TACBrPagamentosBBLancamentoErros;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBLancamentoGPS);

    property codigoIdentificadorPagamento: Integer read fcodigoIdentificadorPagamento write fcodigoIdentificadorPagamento;
    property nomeConvenente: String read fnomeConvenente write fnomeConvenente;
    property textoGuiaPrevidenciaSocial: String read ftextoGuiaPrevidenciaSocial write ftextoGuiaPrevidenciaSocial;
    property indicadorMovimentoAceito: String read findicadorMovimentoAceito write findicadorMovimentoAceito;
    property erros: TACBrPagamentosBBLancamentoErros read Geterros write ferros;
  end;

  { TACBrPagamentosBBLancamentosGPS }

  TACBrPagamentosBBLancamentosGPS = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrPagamentosBBLancamentoGPS;
    procedure SetItem(aIndex: Integer; aValue: TACBrPagamentosBBLancamentoGPS);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrPagamentosBBLancamentoGPS): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrPagamentosBBLancamentoGPS);
    function New: TACBrPagamentosBBLancamentoGPS;
    property Items[aIndex: Integer]: TACBrPagamentosBBLancamentoGPS read GetItem write SetItem; default;
  end;

  { TACBrPagamentosBBConsultaLoteRespostaGPS }

  TACBrPagamentosBBConsultaLoteRespostaGPS = class(TACBrAPISchema)
  private
    fcodigoEstadoRequisicao: TACBrPagamentosBBEstadoRequisicao;
    flancamentos: TACBrPagamentosBBLancamentosGPS;
    fnumeroRequisicao: Integer;
    fquantidadeTotalLancamento: Integer;
    fquantidadeTotalValido: Integer;
    fvalorLancamentosValidos: Double;
    fvalorTotalLancamento: Double;
    function Getlancamentos: TACBrPagamentosBBLancamentosGPS;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrPagamentosBBConsultaLoteRespostaGPS);

    property numeroRequisicao: Integer read fnumeroRequisicao write fnumeroRequisicao;
    property codigoEstadoRequisicao: TACBrPagamentosBBEstadoRequisicao read fcodigoEstadoRequisicao write fcodigoEstadoRequisicao;
    property quantidadeTotalLancamento: Integer read fquantidadeTotalLancamento write fquantidadeTotalLancamento;
    property valorTotalLancamento: Double read fvalorTotalLancamento write fvalorTotalLancamento;
    property quantidadeTotalValido: Integer read fquantidadeTotalValido write fquantidadeTotalValido;
    property valorLancamentosValidos: Double read fvalorLancamentosValidos write fvalorLancamentosValidos;
    property lancamentos: TACBrPagamentosBBLancamentosGPS read Getlancamentos write flancamentos;
  end;

implementation

{ TACBrPagamentosBBConsultaLoteRespostaGPS }

function TACBrPagamentosBBConsultaLoteRespostaGPS.Getlancamentos: TACBrPagamentosBBLancamentosGPS;
begin
  if (not Assigned(flancamentos)) then
    flancamentos := TACBrPagamentosBBLancamentosGPSBase.Create('lancamentos');
  Result := flancamentos;
end;

procedure TACBrPagamentosBBConsultaLoteRespostaGPS.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBConsultaLoteRespostaGPS) then
    Assign(TACBrPagamentosBBConsultaLoteRespostaGPS(ASource));
end;

procedure TACBrPagamentosBBConsultaLoteRespostaGPS.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('numeroRequisicao', fnumeroRequisicao)
    .AddPair('codigoEstadoRequisicao', Integer(fcodigoEstadoRequisicao))
    .AddPair('quantidadeTotalLancamento', fquantidadeTotalLancamento)
    .AddPair('valorTotalLancamento', fvalorTotalLancamento)
    .AddPair('quantidadeTotalValido', fquantidadeTotalValido)
    .AddPair('valorLancamentosValidos', fvalorLancamentosValidos);

  if Assigned(flancamentos) then
    flancamentos.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBConsultaLoteRespostaGPS.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('numeroRequisicao', fnumeroRequisicao)
    .Value('codigoEstadoRequisicao', fcodigoEstadoRequisicao)
    .Value('quantidadeTotalLancamento', fquantidadeTotalLancamento)
    .Value('valorTotalLancamento', fvalorTotalLancamento)
    .Value('quantidadeTotalValido', fquantidadeTotalValido)
    .Value('valorLancamentosValidos', fvalorLancamentosValidos);

  if Assigned(flancamentos) then
    flancamentos.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBConsultaLoteRespostaGPS.Destroy;
begin
  if Assigned(flancamentos) then
    flancamentos.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBConsultaLoteRespostaGPS.Clear;
begin
  fnumeroRequisicao := 0;
  fcodigoEstadoRequisicao := TACBrPagamentosBBEstadoRequisicao(0);
  fquantidadeTotalLancamento := 0;
  fvalorTotalLancamento := 0;
  fquantidadeTotalValido := 0;
  fvalorLancamentosValidos := 0;

  if Assigned(flancamentos) then
    flancamentos.Clear;
end;

function TACBrPagamentosBBConsultaLoteRespostaGPS.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroRequisicao) and
    EstaZerado(Integer(fcodigoEstadoRequisicao)) and
    EstaZerado(fquantidadeTotalLancamento) and
    EstaZerado(fvalorTotalLancamento) and
    EstaZerado(fquantidadeTotalValido) and
    EstaZerado(fvalorLancamentosValidos);

  if Assigned(flancamentos) then
    Result := Result and flancamentos.IsEmpty;
end;

procedure TACBrPagamentosBBConsultaLoteRespostaGPS.Assign(aSource: TACBrPagamentosBBConsultaLoteRespostaGPS);
begin
  fnumeroRequisicao := ASource.numeroRequisicao;
  fcodigoEstadoRequisicao := ASource.codigoEstadoRequisicao;
  fquantidadeTotalLancamento := ASource.quantidadeTotalLancamento;
  fvalorTotalLancamento := ASource.valorTotalLancamento;
  fquantidadeTotalValido := ASource.quantidadeTotalValido;
  fvalorLancamentosValidos := ASource.valorLancamentosValidos;
  lancamentos.Assign(ASource.lancamentos);
end;

{ TACBrPagamentosBBLancamentoGPS }

function TACBrPagamentosBBLancamentoGPS.Geterros: TACBrPagamentosBBLancamentoErros;
begin
  if (not Assigned(ferros)) then
    ferros := TACBrPagamentosBBLancamentoErros.Create('erros');
  Result := ferros;
end;

procedure TACBrPagamentosBBLancamentoGPS.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBLancamentoGPS) then
    Assign(TACBrPagamentosBBLancamentoGPS(ASource));
end;

procedure TACBrPagamentosBBLancamentoGPS.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('codigoIdentificadorPagamento', fcodigoIdentificadorPagamento)
    .AddPair('nomeConvenente', fnomeConvenente)
    .AddPair('textoGuiaPrevidenciaSocial', ftextoGuiaPrevidenciaSocial)
    .AddPair('indicadorMovimentoAceito', findicadorMovimentoAceito);

  if Assigned(ferros) then
    ferros.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBLancamentoGPS.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('codigoIdentificadorPagamento', fcodigoIdentificadorPagamento)
    .Value('nomeConvenente', fnomeConvenente)
    .Value('textoGuiaPrevidenciaSocial', ftextoGuiaPrevidenciaSocial)
    .Value('indicadorMovimentoAceito', findicadorMovimentoAceito);

  if Assigned(ferros) then
    ferros.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBLancamentoGPS.Destroy;
begin
  if Assigned(ferros) then
    ferros.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBLancamentoGPS.Clear;
begin
  fcodigoIdentificadorPagamento := 0;
  fnomeConvenente := EmptyStr;
  ftextoGuiaPrevidenciaSocial := EmptyStr;
  findicadorMovimentoAceito := EmptyStr;

  if Assigned(ferros) then
    ferros.Clear;
end;

function TACBrPagamentosBBLancamentoGPS.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fcodigoIdentificadorPagamento) and
    EstaVazio(fnomeConvenente) and
    EstaVazio(ftextoGuiaPrevidenciaSocial) and
    EstaVazio(findicadorMovimentoAceito);

  if Assigned(ferros) then
    Result := Result and ferros.IsEmpty;
end;

procedure TACBrPagamentosBBLancamentoGPS.Assign(aSource: TACBrPagamentosBBLancamentoGPS);
begin
  fcodigoIdentificadorPagamento := ASource.codigoIdentificadorPagamento;
  fnomeConvenente := ASource.nomeConvenente;
  ftextoGuiaPrevidenciaSocial := ASource.textoGuiaPrevidenciaSocial;
  findicadorMovimentoAceito := ASource.indicadorMovimentoAceito;
  erros.Assign(ASource.erros);
end;

{ TACBrPagamentosBBLotePagamentosRequisicaoGPS }

function TACBrPagamentosBBLotePagamentosRequisicaoGPS.Getlancamentos: TACBrPagamentosBBLancamentosGPSBase;
begin
  if (not Assigned(flancamentos)) then
    flancamentos := TACBrPagamentosBBLancamentosGPSBase.Create('lancamentos');
  Result := flancamentos;
end;

procedure TACBrPagamentosBBLotePagamentosRequisicaoGPS.AssignSchema(aSource: TACBrPagamentoBBLotePagamentosRequisicao);
begin
  if (aSource is TACBrPagamentosBBLotePagamentosRequisicaoGPS) then
    Assign(TACBrPagamentosBBLotePagamentosRequisicaoGPS(ASource));
end;

procedure TACBrPagamentosBBLotePagamentosRequisicaoGPS.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if Assigned(flancamentos) then
    flancamentos.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBLotePagamentosRequisicaoGPS.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  if Assigned(flancamentos) then
    flancamentos.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBLotePagamentosRequisicaoGPS.Destroy;
begin
  if Assigned(flancamentos) then
    flancamentos.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBLotePagamentosRequisicaoGPS.Clear;
begin
  if Assigned(flancamentos) then
    flancamentos.Clear;
end;

function TACBrPagamentosBBLotePagamentosRequisicaoGPS.IsEmpty: Boolean;
begin
  Result := True;
  if Assigned(flancamentos) then
    Result := Result and flancamentos.IsEmpty;
end;

procedure TACBrPagamentosBBLotePagamentosRequisicaoGPS.Assign(aSource: TACBrPagamentosBBLotePagamentosRequisicaoGPS);
begin
  lancamentos.Assign(ASource.lancamentos);
end;

{ TACBrPagamentosBBLancamentoGPSBase }

procedure TACBrPagamentosBBLancamentoGPSBase.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBLancamentoGPSBase) then
    Assign(TACBrPagamentosBBLancamentoGPSBase(ASource));
end;

procedure TACBrPagamentosBBLancamentoGPSBase.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('dataPagamento', DateToStr(fdataPagamento))
    .AddPair('valorPagamento', fvalorPagamento)
    .AddPair('numeroDocumentoDebito', fnumeroDocumentoDebito)
    .AddPair('codigoSeuDocumento', fcodigoSeuDocumento)
    .AddPair('textoDescricaoPagamento', ftextoDescricaoPagamento)
    .AddPair('codigoReceitaTributoGuiaPrevidenciaSocial', fcodigoReceitaTributoGuiaPrevidenciaSocial)
    .AddPair('codigoTipoContribuinteGuiaPrevidenciaSocial', Integer(fcodigoTipoContribuinteGuiaPrevidenciaSocial))
    .AddPair('numeroIdentificacaoContribuinteGuiaPrevidenciaSocial', fnumeroIdentificacaoContribuinteGuiaPrevidenciaSocial)
    .AddPair('codigoIdentificadorTributoGuiaPrevidenciaSocial', fcodigoIdentificadorTributoGuiaPrevidenciaSocial)
    .AddPair('mesAnoCompetenciaGuiaPrevidenciaSocial', fmesAnoCompetenciaGuiaPrevidenciaSocial)
    .AddPair('valorPrevistoInstNacSeguridadeSocialGuiaPrevidenciaSocial', fvalorPrevistoInstNacSeguridadeSocialGuiaPrevidenciaSocial)
    .AddPair('valorOutroEntradaGuiaPrevidenciaSocial', fvalorOutroEntradaGuiaPrevidenciaSocial)
    .AddPair('valorAtualizacaoMonetarioGuiaPrevidenciaSocial', fvalorAtualizacaoMonetarioGuiaPrevidenciaSocial);
end;

procedure TACBrPagamentosBBLancamentoGPSBase.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('dataPagamento', fdataPagamento)
    .Value('valorPagamento', fvalorPagamento)
    .Value('numeroDocumentoDebito', fnumeroDocumentoDebito)
    .Value('codigoSeuDocumento', fcodigoSeuDocumento)
    .Value('textoDescricaoPagamento', ftextoDescricaoPagamento)
    .Value('codigoReceitaTributoGuiaPrevidenciaSocial', fcodigoReceitaTributoGuiaPrevidenciaSocial)
    .Value('codigoTipoContribuinteGuiaPrevidenciaSocial', fcodigoTipoContribuinteGuiaPrevidenciaSocial)
    .Value('numeroIdentificacaoContribuinteGuiaPrevidenciaSocial', fnumeroIdentificacaoContribuinteGuiaPrevidenciaSocial)
    .Value('codigoIdentificadorTributoGuiaPrevidenciaSocial', fcodigoIdentificadorTributoGuiaPrevidenciaSocial)
    .Value('mesAnoCompetenciaGuiaPrevidenciaSocial', fmesAnoCompetenciaGuiaPrevidenciaSocial)
    .Value('valorPrevistoInstNacSeguridadeSocialGuiaPrevidenciaSocial', fvalorPrevistoInstNacSeguridadeSocialGuiaPrevidenciaSocial)
    .Value('valorOutroEntradaGuiaPrevidenciaSocial', fvalorOutroEntradaGuiaPrevidenciaSocial)
    .Value('valorAtualizacaoMonetarioGuiaPrevidenciaSocial', fvalorAtualizacaoMonetarioGuiaPrevidenciaSocial);
end;

procedure TACBrPagamentosBBLancamentoGPSBase.Clear;
begin
  fdataPagamento := 0;
  fvalorPagamento := 0;
  fnumeroDocumentoDebito := 0;
  fcodigoSeuDocumento := EmptyStr;
  ftextoDescricaoPagamento := EmptyStr;
  fcodigoReceitaTributoGuiaPrevidenciaSocial := 0;
  fcodigoTipoContribuinteGuiaPrevidenciaSocial := pctNenhum;
  fnumeroIdentificacaoContribuinteGuiaPrevidenciaSocial := 0;
  fcodigoIdentificadorTributoGuiaPrevidenciaSocial := EmptyStr;
  fmesAnoCompetenciaGuiaPrevidenciaSocial := 0;
  fvalorPrevistoInstNacSeguridadeSocialGuiaPrevidenciaSocial := 0;
  fvalorOutroEntradaGuiaPrevidenciaSocial := 0;
  fvalorAtualizacaoMonetarioGuiaPrevidenciaSocial := 0;
end;

function TACBrPagamentosBBLancamentoGPSBase.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fdataPagamento) and
    EstaZerado(fvalorPagamento) and
    EstaZerado(fnumeroDocumentoDebito) and
    EstaVazio(fcodigoSeuDocumento) and
    EstaVazio(ftextoDescricaoPagamento) and
    EstaZerado(fcodigoReceitaTributoGuiaPrevidenciaSocial) and
    EstaZerado(Ord(fcodigoTipoContribuinteGuiaPrevidenciaSocial)) and
    EstaZerado(fnumeroIdentificacaoContribuinteGuiaPrevidenciaSocial) and
    EstaVazio(fcodigoIdentificadorTributoGuiaPrevidenciaSocial) and
    EstaZerado(fmesAnoCompetenciaGuiaPrevidenciaSocial) and
    EstaZerado(fvalorPrevistoInstNacSeguridadeSocialGuiaPrevidenciaSocial) and
    EstaZerado(fvalorOutroEntradaGuiaPrevidenciaSocial) and
    EstaZerado(fvalorAtualizacaoMonetarioGuiaPrevidenciaSocial);
end;

procedure TACBrPagamentosBBLancamentoGPSBase.Assign(aSource: TACBrPagamentosBBLancamentoGPSBase);
begin
  fdataPagamento := ASource.dataPagamento;
  fvalorPagamento := ASource.valorPagamento;
  fnumeroDocumentoDebito := ASource.numeroDocumentoDebito;
  fcodigoSeuDocumento := ASource.codigoSeuDocumento;
  ftextoDescricaoPagamento := ASource.textoDescricaoPagamento;
  fcodigoReceitaTributoGuiaPrevidenciaSocial := ASource.codigoReceitaTributoGuiaPrevidenciaSocial;
  fcodigoTipoContribuinteGuiaPrevidenciaSocial := ASource.codigoTipoContribuinteGuiaPrevidenciaSocial;
  fnumeroIdentificacaoContribuinteGuiaPrevidenciaSocial := ASource.numeroIdentificacaoContribuinteGuiaPrevidenciaSocial;
  fcodigoIdentificadorTributoGuiaPrevidenciaSocial := ASource.codigoIdentificadorTributoGuiaPrevidenciaSocial;
  fmesAnoCompetenciaGuiaPrevidenciaSocial := ASource.mesAnoCompetenciaGuiaPrevidenciaSocial;
  fvalorPrevistoInstNacSeguridadeSocialGuiaPrevidenciaSocial := ASource.valorPrevistoInstNacSeguridadeSocialGuiaPrevidenciaSocial;
  fvalorOutroEntradaGuiaPrevidenciaSocial := ASource.valorOutroEntradaGuiaPrevidenciaSocial;
  fvalorAtualizacaoMonetarioGuiaPrevidenciaSocial := ASource.valorAtualizacaoMonetarioGuiaPrevidenciaSocial;
end;

{ TACBrPagamentosBBConsultaRespostaGPS }

function TACBrPagamentosBBConsultaRespostaGPS.GetlistaDevolucao: TACBrPagamentosBBDevolucaoListaBase;
begin
  if (not Assigned(flistaDevolucao)) then
    flistaDevolucao := TACBrPagamentosBBDevolucaoListaBase.Create('listaDevolucao');
  Result := flistaDevolucao;
end;

function TACBrPagamentosBBConsultaRespostaGPS.GetlistaPagamentos: TACBrPagamentosBBGPSLista;
begin
  if (not Assigned(flistaPagamentos)) then
    flistaPagamentos := listaPagamentos.Create('listaPagamentos');
  Result := flistaPagamentos;
end;

procedure TACBrPagamentosBBConsultaRespostaGPS.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBConsultaRespostaGPS) then
    Assign(TACBrPagamentosBBConsultaRespostaGPS(ASource));
end;

procedure TACBrPagamentosBBConsultaRespostaGPS.DoWriteToJSon(
  aJSon: TACBrJSONObject);
begin
  if Assigned(flistaPagamentos) then
    flistaPagamentos.WriteToJSon(aJSon);

  if Assigned(flistaDevolucao) then
    flistaDevolucao.WriteToJSon(aJSon);
end;

procedure TACBrPagamentosBBConsultaRespostaGPS.DoReadFromJSon(
  aJSon: TACBrJSONObject);
begin
  if Assigned(flistaPagamentos) then
    flistaPagamentos.ReadFromJSon(aJSon);

  if Assigned(flistaDevolucao) then
    flistaDevolucao.ReadFromJSon(aJSon);
end;

destructor TACBrPagamentosBBConsultaRespostaGPS.Destroy;
begin
  if Assigned(flistaDevolucao) then
    flistaDevolucao.Free;
  if Assigned(flistaPagamentos) then
    flistaPagamentos.Free;
  inherited Destroy;
end;

procedure TACBrPagamentosBBConsultaRespostaGPS.Clear;
begin
  if Assigned(flistaPagamentos) then
    flistaPagamentos.Clear;

  if Assigned(flistaDevolucao) then
    flistaDevolucao.Clear;
end;

function TACBrPagamentosBBConsultaRespostaGPS.IsEmpty: Boolean;
begin
  Result := True;
  if Assigned(flistaPagamentos) then
    Result := Result and flistaPagamentos.IsEmpty;

  if Assigned(flistaDevolucao) then
    Result := Result and flistaDevolucao.IsEmpty;
end;

procedure TACBrPagamentosBBConsultaRespostaGPS.Assign(aSource: TACBrPagamentosBBConsultaRespostaGPS);
begin 
  if Assigned(flistaPagamentos) then
    flistaPagamentos.Assign(ASource.listaPagamentos);

  if Assigned(flistaDevolucao) then
    flistaDevolucao.Assign(ASource.listaDevolucao);
end;

{ TACBrPagamentosBBGPS }

procedure TACBrPagamentosBBGPS.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrPagamentosBBGPS) then
    Assign(TACBrPagamentosBBGPS(ASource));
end;

procedure TACBrPagamentosBBGPS.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('codigo', fcodigo)
    .AddPair('tipoContribuinte', ftipoContribuinte)
    .AddPair('identificacaoContribuinte', fidentificacaoContribuinte)
    .AddPair('identificacaoGPS', fidentificacaoGPS)
    .AddPair('mesAnoCompetencia', fmesAnoCompetencia)
    .AddPair('valorINSS', fvalorINSS)
    .AddPair('valorAtualizacaoMonetaria', fvalorAtualizacaoMonetaria)
    .AddPair('texto', ftexto);
end;

procedure TACBrPagamentosBBGPS.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('codigo', fcodigo)
    .Value('tipoContribuinte', ftipoContribuinte)
    .Value('identificacaoContribuinte', fidentificacaoContribuinte)
    .Value('identificacaoGPS', fidentificacaoGPS)
    .Value('mesAnoCompetencia', fmesAnoCompetencia)
    .Value('valorINSS', fvalorINSS)
    .Value('valorAtualizacaoMonetaria', fvalorAtualizacaoMonetaria)
    .Value('texto', ftexto);
end;

procedure TACBrPagamentosBBGPS.Clear;
begin
  fcodigo := 0;
  ftipoContribuinte := 0;
  fidentificacaoContribuinte := 0;
  fidentificacaoGPS := EmptyStr;
  fmesAnoCompetencia := 0;
  fvalorINSS := 0;
  fvalorAtualizacaoMonetaria := 0;
  ftexto := EmptyStr;
end;

function TACBrPagamentosBBGPS.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fcodigo) and
    EstaZerado(ftipoContribuinte) and
    EstaZerado(fidentificacaoContribuinte) and
    EstaVazio(fidentificacaoGPS) and
    EstaZerado(fmesAnoCompetencia) and
    EstaZerado(fvalorINSS) and
    EstaZerado(fvalorAtualizacaoMonetaria) and
    EstaVazio(ftexto);
end;

procedure TACBrPagamentosBBGPS.Assign(aSource: TACBrPagamentosBBGPS);
begin
  fcodigo := ASource.codigo;
  ftipoContribuinte := ASource.tipoContribuinte;
  fidentificacaoContribuinte := ASource.identificacaoContribuinte;
  fidentificacaoGPS := ASource.identificacaoGPS;
  fmesAnoCompetencia := ASource.mesAnoCompetencia;
  fvalorINSS := ASource.valorINSS;
  fvalorAtualizacaoMonetaria := ASource.valorAtualizacaoMonetaria;
  ftexto := ASource.texto;
end;

end.

