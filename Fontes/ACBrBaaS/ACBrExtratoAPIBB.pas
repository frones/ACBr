{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Daniel Infocotidiano                                                       }
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

unit ACBrExtratoAPIBB;

interface

uses
  Classes, SysUtils, ACBrAPIBase, ACBrJSON, ACBrExtratoAPI;

const
  cExtratoBBParamDevAppKey = 'gw-dev-app-key';
  cExtratoBBURLSandbox = 'https://api.hm.bb.com.br/extratos/v1';
  cExtratoBBURLProducao = 'https://api-extratos.bb.com.br/extratos/v1';
  cExtratoBBURLAuthSandbox = 'https://oauth.hm.bb.com.br/oauth/token';
  cExtratoBBURLAuthProducao = 'https://oauth.bb.com.br/oauth/token';
  cExtratoBBPathCC = 'conta-corrente';
  cExtratoBBPathAgencia = 'agencia';
  cExtratoBBPathConta = 'conta';

type

  {TACBrExtratoBBTipoLancamento = (
    etlNenhum,
    etlDebito,
    etlCredito
  );}

  TACBrExtratoBBTipoPessoa = (
    etpNenhum,
    etpFisica,
    etpJuridica
  );

  { TACBrExtratoBBLancamento }

  TACBrExtratoBBLancamento = class(TACBrAPISchema)
  private
    fcodigoAgenciaContrapartida: Integer;
    fcodigoAgenciaOrigem: Integer;
    fcodigoBancoContrapartida: Integer;
    fcodigoHistorico: String;
    fdataLancamento: TDateTime;
    fdataMovimento: TDateTime;
    findicadorTipoLancamento: Integer;
    fnumeroContaContrapartida: String;
    fnumeroCpfCnpjContrapartida: Integer;
    fnumeroDocumento: Integer;
    fnumeroLote: Integer;
    ftextoDescricaoHistorico: String;
    ftextoDvContaContrapartida: String;
    ftextoInformacaoComplementar: String;
    fvalorLancamento: Double;
    findicadorSinalLancamento: TACBrExtratoAPITipoOperacao;
    findicadorTipoPessoaContrapartida: TACBrExtratoBBTipoPessoa;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrExtratoBBLancamento);

    property indicadorTipoLancamento: Integer read findicadorTipoLancamento write findicadorTipoLancamento;
    property dataLancamento: TDateTime read fdataLancamento write fdataLancamento;
    property dataMovimento: TDateTime read fdataMovimento write fdataMovimento;
    property codigoAgenciaOrigem: Integer read fcodigoAgenciaOrigem write fcodigoAgenciaOrigem;
    property numeroLote: Integer read fnumeroLote write fnumeroLote;
    property numeroDocumento: Integer read fnumeroDocumento write fnumeroDocumento;
    property codigoHistorico: String read fcodigoHistorico write fcodigoHistorico;
    property textoDescricaoHistorico: String read ftextoDescricaoHistorico write ftextoDescricaoHistorico;
    property valorLancamento: Double read fvalorLancamento write fvalorLancamento;
    property indicadorSinalLancamento: TACBrExtratoAPITipoOperacao read findicadorSinalLancamento write findicadorSinalLancamento;
    property textoInformacaoComplementar: String read ftextoInformacaoComplementar write ftextoInformacaoComplementar;
    property numeroCpfCnpjContrapartida: Integer read fnumeroCpfCnpjContrapartida write fnumeroCpfCnpjContrapartida;
    property indicadorTipoPessoaContrapartida: TACBrExtratoBBTipoPessoa read findicadorTipoPessoaContrapartida write findicadorTipoPessoaContrapartida;
    property codigoBancoContrapartida: Integer read fcodigoBancoContrapartida write fcodigoBancoContrapartida;
    property codigoAgenciaContrapartida: Integer read fcodigoAgenciaContrapartida write fcodigoAgenciaContrapartida;
    property numeroContaContrapartida: String read fnumeroContaContrapartida write fnumeroContaContrapartida;
    property textoDvContaContrapartida: String read ftextoDvContaContrapartida write ftextoDvContaContrapartida;
  end;

  { TACBrExtratoBBListaLancamento }

  TACBrExtratoBBListaLancamento = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrExtratoBBLancamento;
    procedure SetItem(aIndex: Integer; aValue: TACBrExtratoBBLancamento);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrExtratoBBLancamento): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrExtratoBBLancamento);
    function New: TACBrExtratoBBLancamento;
    property Items[aIndex: Integer]: TACBrExtratoBBLancamento read GetItem write SetItem; default;
  end;

  { TACBrExtratoBBResult }

  TACBrExtratoBBResult = class(TACBrExtratoConsultado)
  private
    flistaLancamento: TACBrExtratoBBListaLancamento;
    fnumeroPaginaAnterior: Integer;
    fnumeroPaginaAtual: Integer;
    fnumeroPaginaProximo: Integer;
    fquantidadeRegistroPaginaAtual: Integer;
    fquantidadeTotalPagina: Integer;
    fquantidadeTotalRegistro: Integer;
  protected
    procedure AssignSchema(aSource: TACBrAPISchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
    procedure ConverterParaExtratoConsultado;
  public
    constructor Create(const ObjectName: String = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TACBrExtratoBBResult);

    property numeroPaginaAtual: Integer read fnumeroPaginaAtual write fnumeroPaginaAtual;
    property quantidadeRegistroPaginaAtual: Integer read fquantidadeRegistroPaginaAtual write fquantidadeRegistroPaginaAtual;
    property numeroPaginaAnterior: Integer read fnumeroPaginaAnterior write fnumeroPaginaAnterior;
    property numeroPaginaProximo: Integer read fnumeroPaginaProximo write fnumeroPaginaProximo;
    property quantidadeTotalPagina: Integer read fquantidadeTotalPagina write fquantidadeTotalPagina;
    property quantidadeTotalRegistro: Integer read fquantidadeTotalRegistro write fquantidadeTotalRegistro;
    property listaLancamento: TACBrExtratoBBListaLancamento read flistaLancamento write flistaLancamento;
  end;

  { TACBrExtratoBBErro } 

  TACBrExtratoBBErro = class(TACBrExtratoErro)
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
    procedure Assign(aSource: TACBrExtratoBBErro);

    property versao: String read fversao write fversao;
    property ocorrencia: String read focorrencia write focorrencia;
  end;

  { TACBrExtratoBBErros }

  TACBrExtratoBBErros = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrExtratoBBErro;
    procedure SetItem(aIndex: Integer; aValue: TACBrExtratoBBErro);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrExtratoBBErro): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrExtratoBBErro);
    function New: TACBrExtratoBBErro;
    property Items[aIndex: Integer]: TACBrExtratoBBErro read GetItem write SetItem; default;
  end;

  { TACBrExtratoAPIBB }

  TACBrExtratoAPIBB = class(TACBrExtratoAPIBancoClass)
  private
    fDeveloperApplicationKey: String;
    fExtratoBBErros: TACBrExtratoBBErros;
    fxMCITeste: String;
    function GetExtratoBBErros: TACBrExtratoBBErros;
  protected
    function GetRespostaErro: TACBrExtratoErro; override;
    function GetExtratoConsultado: TACBrExtratoConsultado; override;

    function CalcularURL: String; override;
    procedure Autenticar; override;
  public
    destructor Destroy; override;

    function ConsultarExtrato(
      const aAgencia, aConta: String;
      const aDataInicio: TDateTime = 0;
      const aDataFim: TDateTime = 0;
      const aPagina: Integer = 0;
      const aRegistrosPorPag: Integer = 0): Boolean; override;

    property ExtratoBBErros: TACBrExtratoBBErros read GetExtratoBBErros;
    property DeveloperApplicationKey: String read fDeveloperApplicationKey write fDeveloperApplicationKey;
    property xMCITeste: String read fxMCITeste write fxMCITeste; // Apenas para Homologação
  end;

  function TipoPessoaToString(const aTipo: TACBrExtratoBBTipoPessoa): String;
  function StringToTipoPessoa(const aStr: String): TACBrExtratoBBTipoPessoa;

implementation

uses
  synautil, synacode, DateUtils,
  ACBrSocket,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.Base;

function TipoPessoaToString(const aTipo: TACBrExtratoBBTipoPessoa): String;
begin
  Result := EmptyStr;
  case aTipo of
    etpFisica: Result := 'F';
    etpJuridica: Result := 'J';
  end;
end;

function StringToTipoPessoa(const aStr: String): TACBrExtratoBBTipoPessoa;
var
  s: String;
begin
  Result := etpNenhum;
  s := UpperCase(aStr);
  if (s = 'F') then
    Result := etpFisica
  else if (s = 'F') then
    Result := etpJuridica;
end;

{ TACBrExtratoBBErros }

function TACBrExtratoBBErros.GetItem(aIndex: Integer): TACBrExtratoBBErro;
begin
  Result := TACBrExtratoBBErro(inherited Items[aIndex]);
end;

procedure TACBrExtratoBBErros.SetItem(aIndex: Integer; aValue: TACBrExtratoBBErro);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrExtratoBBErros.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrExtratoBBErros.Add(aLancamento: TACBrExtratoBBErro): Integer;
begin
  Result := inherited Add(aLancamento);
end;

procedure TACBrExtratoBBErros.Insert(aIndex: Integer; aLancamento: TACBrExtratoBBErro);
begin
  inherited Insert(aIndex, aLancamento);
end;

function TACBrExtratoBBErros.New: TACBrExtratoBBErro;
begin
  Result := TACBrExtratoBBErro.Create;
  Self.Add(Result);
end;

{ TACBrExtratoBBErro }

procedure TACBrExtratoBBErro.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrExtratoBBErro) then
    Assign(TACBrExtratoBBErro(aSource));
end;

procedure TACBrExtratoBBErro.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('codigo', fpcodigo)
    .AddPair('mensagem', fpmensagem)
    .AddPair('ocorrencia', focorrencia)
    .AddPair('versao', fversao);
end;

procedure TACBrExtratoBBErro.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('codigo', fpcodigo)
    .Value('mensagem', fpmensagem)
    .Value('ocorrencia', focorrencia)
    .Value('versao', fversao);
end;

procedure TACBrExtratoBBErro.Clear;
begin
  inherited Clear;
  focorrencia := EmptyStr;
  fversao := EmptyStr;
end;

function TACBrExtratoBBErro.IsEmpty: Boolean;
begin
  Result :=
    EstaVazio(fpcodigo) and
    EstaVazio(fpmensagem) and
    EstaVazio(focorrencia) and
    EstaVazio(fversao);
end;

procedure TACBrExtratoBBErro.Assign(aSource: TACBrExtratoBBErro);
begin
  fpcodigo := aSource.codigo;
  fpmensagem := aSource.mensagem;
  focorrencia := aSource.ocorrencia;
  fversao := aSource.versao;
end;

{ TACBrExtratoAPIBB }

function TACBrExtratoAPIBB.CalcularURL: String;
begin
  Result := EmptyStr;
  if (not Assigned(fpOwner)) then
    Exit;

  if (fpOwner.Ambiente = eamProducao) then
    Result := cExtratoBBURLProducao
  else if (fpOwner.Ambiente = eamHomologacao) then
    Result := cExtratoBBURLSandbox;
end;

procedure TACBrExtratoAPIBB.Autenticar;
var
  wURL, Body, BasicAutentication: String;
  js: TACBrJSONObject;
  qp: TACBrHTTPQueryParams;
  sec: Integer;
begin
  LimparHTTP;

  if (fpOwner.Ambiente = eamProducao) then
    wURL := cExtratoBBURLAuthProducao
  else
    wURL := cExtratoBBURLAuthSandbox;

  qp := TACBrHTTPQueryParams.Create;
  try
    qp.Values['grant_type'] := 'client_credentials';
    qp.Values['scope'] := 'extrato-info';
    Body := qp.AsURL;
    WriteStrToStream(HTTPSend.Document, Body);
    HttpSend.MimeType := cContentTypeApplicationWwwFormUrlEncoded;
  finally
    qp.Free;
  end;

  BasicAutentication := 'Basic ' + EncodeBase64(ClientID + ':' + ClientSecret);
  HTTPSend.Headers.Add(cHTTPHeaderAuthorization + BasicAutentication);
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

function TACBrExtratoAPIBB.GetExtratoBBErros: TACBrExtratoBBErros;
begin
  if (not Assigned(fExtratoBBErros)) then
    fExtratoBBErros := TACBrExtratoBBErros.Create('erros');
  Result := fExtratoBBErros;
end;

function TACBrExtratoAPIBB.GetRespostaErro: TACBrExtratoErro;
begin
  if (not Assigned(fpRespostaErro)) then
    fpRespostaErro := TACBrExtratoBBErro.Create;
  Result := fpRespostaErro;
end;

function TACBrExtratoAPIBB.GetExtratoConsultado: TACBrExtratoConsultado;
begin
  if (not Assigned(fpExtratoConsultado)) then
    fpExtratoConsultado := TACBrExtratoBBResult.Create;
  Result := fpExtratoConsultado;
end;

destructor TACBrExtratoAPIBB.Destroy;
begin
  if Assigned(fExtratoBBErros) then
    fExtratoBBErros.Free;
  inherited Destroy;
end;

function TACBrExtratoAPIBB.ConsultarExtrato(const aAgencia, aConta: String;
  const aDataInicio: TDateTime; const aDataFim: TDateTime;
  const aPagina: Integer; const aRegistrosPorPag: Integer): Boolean;
begin
  Result := False;
  if EstaVazio(aAgencia) or EstaVazio(aConta) then
    Exit;
         
  PrepararHTTP;
  RegistrarLog('  TACBrExtratoAPIBB.ConsultarExtrato(Ag: ' + aAgencia +
    ' - Conta: ' + aConta + ')');
  
  HttpSend.Protocol := '1.1';
  URLQueryParams.Values['gw-dev-app-key'] := fDeveloperApplicationKey;

  if NaoEstaZerado(aPagina) then
    URLQueryParams.Values['numeroPaginaSolicitacao'] := IntToStr(aPagina);
  if NaoEstaZerado(aRegistrosPorPag) then
    URLQueryParams.Values['quantidadeRegistroPaginaSolicitacao'] := IntToStr(aRegistrosPorPag);
  if NaoEstaZerado(aDataInicio) then
    URLQueryParams.Values['dataInicioSolicitacao'] := FormatDateTime('ddmmyyyy', aDataInicio);
  if NaoEstaZerado(aDataFim) then
    URLQueryParams.Values['dataFimSolicitacao'] := FormatDateTime('ddmmyyyy', aDataFim);

  URLPathParams.Add(cExtratoBBPathCC);
  URLPathParams.Add(cExtratoBBPathAgencia);
  URLPathParams.Add(aAgencia);
  URLPathParams.Add(cExtratoBBPathConta);
  URLPathParams.Add(aConta);

  if NaoEstaVazio(fpToken) then
    HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer +' '+ fpToken);
  if (fpOwner.Ambiente = eamHomologacao) then
    HTTPSend.Headers.Add('x-br-com-bb-ipa-mciteste:' + fxMCITeste);

  HTTPMethod(cHTTPMethodGET, CalcularURL);

  Result := (HTTPResultCode = HTTP_OK);
  if Result then
    ExtratoConsultado.AsJSON := HTTPResponse
  else
  begin
    ExtratoBBErros.AsJSON := HTTPResponse;
    if NaoEstaZerado(ExtratoBBErros.Count) then
      RespostaErro.AsJSON := ExtratoBBErros[0].AsJSON;
  end;
end;

{ TACBrExtratoBBResult }

procedure TACBrExtratoBBResult.AssignSchema(aSource: TACBrAPISchema);
begin
  if (aSource is TACBrExtratoBBResult) then
    Assign(TACBrExtratoBBResult(aSource));
end;

procedure TACBrExtratoBBResult.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('numeroPaginaAnterior', fnumeroPaginaAnterior)
    .AddPair('numeroPaginaAtual', fnumeroPaginaAtual)
    .AddPair('numeroPaginaProximo', fnumeroPaginaProximo)
    .AddPair('quantidadeRegistroPaginaAtual', fquantidadeRegistroPaginaAtual)
    .AddPair('quantidadeTotalPagina', fquantidadeTotalPagina)
    .AddPair('quantidadeTotalRegistro', fquantidadeTotalRegistro);

  if Assigned(flistaLancamento) then
    flistaLancamento.WriteToJSon(aJSon);
end;

procedure TACBrExtratoBBResult.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('numeroPaginaAnterior', fnumeroPaginaAnterior)
    .Value('numeroPaginaAtual', fnumeroPaginaAtual)
    .Value('numeroPaginaProximo', fnumeroPaginaProximo)
    .Value('quantidadeRegistroPaginaAtual', fquantidadeRegistroPaginaAtual)
    .Value('quantidadeTotalPagina', fquantidadeTotalPagina)
    .Value('quantidadeTotalRegistro', fquantidadeTotalRegistro);

  if Assigned(flistaLancamento) then
    flistaLancamento.ReadFromJSon(aJSon);

  ConverterParaExtratoConsultado;
end;

procedure TACBrExtratoBBResult.ConverterParaExtratoConsultado;
var
  i: Integer;
begin
  fpTotalPaginas := fquantidadeTotalPagina;
  fpTotalRegistros := fquantidadeTotalRegistro;
  fpRegistrosPaginaAtual := fquantidadeRegistroPaginaAtual;

  if Assigned(flistaLancamento) then
  for i := 0 to flistaLancamento.Count - 1 do
    with Lancamentos.New do
    begin
      DataLancamento := flistaLancamento[i].dataLancamento;
      DataMovimento := flistaLancamento[i].dataMovimento;
      Valor := flistaLancamento[i].valorLancamento;
      TipoOperacao := flistaLancamento[i].indicadorSinalLancamento;
      Descricao := flistaLancamento[i].textoDescricaoHistorico;
      InfoComplementar := flistaLancamento[i].textoInformacaoComplementar;
    end;
end;

constructor TACBrExtratoBBResult.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  flistaLancamento := TACBrExtratoBBListaLancamento.Create('listaLancamento');
end;

procedure TACBrExtratoBBResult.Clear;
begin
  fnumeroPaginaAnterior := 0;
  fnumeroPaginaAtual := 0;
  fnumeroPaginaProximo := 0;
  fquantidadeRegistroPaginaAtual := 0;
  fquantidadeTotalPagina := 0;
  fquantidadeTotalRegistro := 0;

  if Assigned(flistaLancamento) then
    flistaLancamento.Clear;
end;

function TACBrExtratoBBResult.IsEmpty: Boolean;
begin
  Result :=
    EstaZerado(fnumeroPaginaAnterior) and
    EstaZerado(fnumeroPaginaAtual) and
    EstaZerado(fnumeroPaginaProximo) and
    EstaZerado(fquantidadeRegistroPaginaAtual) and
    EstaZerado(fquantidadeTotalPagina) and
    EstaZerado(fquantidadeTotalRegistro) and
    flistaLancamento.IsEmpty;
end;

procedure TACBrExtratoBBResult.Assign(aSource: TACBrExtratoBBResult);
begin
  fnumeroPaginaAnterior := aSource.numeroPaginaAnterior;
  fnumeroPaginaAtual := aSource.numeroPaginaAtual;
  fnumeroPaginaProximo := aSource.numeroPaginaProximo;
  fquantidadeRegistroPaginaAtual := aSource.quantidadeRegistroPaginaAtual;
  fquantidadeTotalPagina := aSource.quantidadeTotalPagina;
  fquantidadeTotalRegistro := aSource.quantidadeTotalRegistro;

  if Assigned(flistaLancamento) then
    flistaLancamento.Assign(aSource.listaLancamento);
end;

{ TACBrExtratoBBListaLancamento }

function TACBrExtratoBBListaLancamento.GetItem(aIndex: Integer): TACBrExtratoBBLancamento;
begin
  Result := TACBrExtratoBBLancamento(inherited Items[aIndex]);
end;

procedure TACBrExtratoBBListaLancamento.SetItem(aIndex: Integer; aValue: TACBrExtratoBBLancamento);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrExtratoBBListaLancamento.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrExtratoBBListaLancamento.Add(aLancamento: TACBrExtratoBBLancamento): Integer;
begin
  Result := inherited Add(aLancamento);
end;

procedure TACBrExtratoBBListaLancamento.Insert(aIndex: Integer; aLancamento: TACBrExtratoBBLancamento);
begin
  inherited Insert(aIndex, aLancamento);
end;

function TACBrExtratoBBListaLancamento.New: TACBrExtratoBBLancamento;
begin
  Result := TACBrExtratoBBLancamento.Create;
  Self.Add(Result);
end;

{ TACBrExtratoBBLancamento }

procedure TACBrExtratoBBLancamento.AssignSchema(aSource: TACBrAPISchema);
begin
  if (ASource is TACBrExtratoBBLancamento) then
    Assign(TACBrExtratoBBLancamento(ASource));
end;

procedure TACBrExtratoBBLancamento.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  AJSon
    .AddPair('codigoAgenciaContrapartida', fcodigoAgenciaContrapartida)
    .AddPair('codigoAgenciaOrigem', fcodigoAgenciaOrigem)
    .AddPair('codigoBancoContrapartida', fcodigoBancoContrapartida)
    .AddPair('codigoHistorico', fcodigoHistorico)
    .AddPair('dataLancamento', fdataLancamento)
    .AddPair('dataMovimento', fdataMovimento)
    .AddPair('indicadorSinalLancamento', TipoLancamentoToString(findicadorSinalLancamento))
    .AddPair('indicadorTipoLancamento', findicadorTipoLancamento)
    .AddPair('indicadorTipoPessoaContrapartida', TipoPessoaToString(findicadorTipoPessoaContrapartida))
    .AddPair('numeroContaContrapartida', fnumeroContaContrapartida)
    .AddPair('numeroCpfCnpjContrapartida', fnumeroCpfCnpjContrapartida)
    .AddPair('numeroDocumento', fnumeroDocumento)
    .AddPair('numeroLote', fnumeroLote)
    .AddPair('textoDescricaoHistorico', ftextoDescricaoHistorico)
    .AddPair('textoDvContaContrapartida', ftextoDvContaContrapartida)
    .AddPair('textoInformacaoComplementar', ftextoInformacaoComplementar)
    .AddPair('valorLancamento:Double', fvalorLancamento);
end;

procedure TACBrExtratoBBLancamento.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s1, s2, s3, s4: String;
  d, m, a: word;
begin
  {$IfDef FPC}
  s1 := EmptyStr;
  s2 := EmptyStr;
  s3 := EmptyStr;
  s4 := EmptyStr;
  {$EndIf}

  AJSon
    .Value('codigoAgenciaContrapartida', fcodigoAgenciaContrapartida)
    .Value('codigoAgenciaOrigem', fcodigoAgenciaOrigem)
    .Value('codigoBancoContrapartida', fcodigoBancoContrapartida)
    .Value('codigoHistorico', fcodigoHistorico) 
    .Value('indicadorSinalLancamento', s1)
    .Value('indicadorTipoPessoaContrapartida', s2)
    .Value('dataLancamento', s3)
    .Value('dataMovimento', s4)
    .Value('indicadorTipoLancamento', findicadorTipoLancamento)
    .Value('numeroContaContrapartida', fnumeroContaContrapartida)
    .Value('numeroCpfCnpjContrapartida', fnumeroCpfCnpjContrapartida)
    .Value('numeroDocumento', fnumeroDocumento)
    .Value('numeroLote', fnumeroLote)
    .Value('textoDescricaoHistorico', ftextoDescricaoHistorico)
    .Value('textoDvContaContrapartida', ftextoDvContaContrapartida)
    .Value('textoInformacaoComplementar', ftextoInformacaoComplementar)
    .Value('valorLancamento', fvalorLancamento);
  findicadorSinalLancamento := StringToTipoLancamento(s1);
  findicadorTipoPessoaContrapartida := StringToTipoPessoa(s2);

  if NaoEstaVazio(s3) and (s3 <> '0') then
  begin
    if (Length(s3) < 8) then
      s3 := PadLeft(s3, 8, '0');

    d := StrToIntDef(Copy(s3, 1, 2), 0);
    m := StrToIntDef(Copy(s3, 3, 2), 0);
    a := StrToIntDef(Copy(s3, 5, 4), 0);
    if NaoEstaZerado(d) and NaoEstaZerado(m) and NaoEstaZerado(a) then
      fdataLancamento := EncodeDate(a, m, d);
  end;

  if NaoEstaVazio(s4) and (s4 <> '0') then
  begin
    if (Length(s4) < 8) then
      s4 := PadLeft(s4, 8, '0');

    d := StrToIntDef(Copy(s4, 1, 2), 0);
    m := StrToIntDef(Copy(s4, 3, 2), 0);
    a := StrToIntDef(Copy(s4, 5, 4), 0);
    if NaoEstaZerado(d) and NaoEstaZerado(m) and NaoEstaZerado(a) then
      fdataMovimento := EncodeDate(a, m, d);
  end;
end;

procedure TACBrExtratoBBLancamento.Clear;
begin                  
  fnumeroLote := 0;
  fdataMovimento := 0;
  fdataLancamento := 0;
  fnumeroDocumento := 0;
  fvalorLancamento := 0;
  fcodigoAgenciaOrigem := 0;
  findicadorTipoLancamento := 0;
  fcodigoBancoContrapartida := 0;  
  fcodigoAgenciaContrapartida := 0;
  fnumeroCpfCnpjContrapartida := 0;
  fcodigoHistorico := EmptyStr;
  fnumeroContaContrapartida := EmptyStr;
  ftextoDescricaoHistorico := EmptyStr;
  ftextoDvContaContrapartida := EmptyStr;
  ftextoInformacaoComplementar := EmptyStr;
  findicadorSinalLancamento := etoNenhum;
  findicadorTipoPessoaContrapartida := etpNenhum;
end;

function TACBrExtratoBBLancamento.IsEmpty: Boolean;
begin
  Result :=                                      
    EstaZerado(fnumeroLote) and
    EstaZerado(fdataMovimento) and
    EstaZerado(fdataLancamento) and
    EstaZerado(fnumeroDocumento) and
    EstaZerado(fvalorLancamento) and
    EstaZerado(fcodigoAgenciaOrigem) and
    EstaZerado(findicadorTipoLancamento) and
    EstaZerado(fcodigoBancoContrapartida) and
    EstaZerado(fcodigoAgenciaContrapartida) and
    EstaZerado(fnumeroCpfCnpjContrapartida) and
    EstaVazio(fcodigoHistorico) and
    EstaVazio(fnumeroContaContrapartida) and
    EstaVazio(ftextoDescricaoHistorico) and
    EstaVazio(ftextoDvContaContrapartida) and
    EstaVazio(ftextoInformacaoComplementar) and
    (findicadorSinalLancamento = etoNenhum) and
    (findicadorTipoPessoaContrapartida = etpNenhum);
end;

procedure TACBrExtratoBBLancamento.Assign(aSource: TACBrExtratoBBLancamento);
begin
  fnumeroLote := aSource.numeroLote;
  fdataMovimento := aSource.dataMovimento;
  fdataLancamento := aSource.dataLancamento;
  fnumeroDocumento := aSource.numeroDocumento;
  fvalorLancamento := aSource.valorLancamento;
  fcodigoAgenciaOrigem := aSource.codigoAgenciaOrigem;
  findicadorTipoLancamento := aSource.indicadorTipoLancamento;
  fcodigoBancoContrapartida := aSource.codigoBancoContrapartida;
  fcodigoAgenciaContrapartida := aSource.codigoAgenciaContrapartida;
  fnumeroCpfCnpjContrapartida := aSource.numeroCpfCnpjContrapartida;
  fcodigoHistorico := aSource.codigoHistorico;
  fnumeroContaContrapartida := aSource.numeroContaContrapartida;
  ftextoDescricaoHistorico := aSource.textoDescricaoHistorico;
  ftextoDvContaContrapartida := aSource.textoDvContaContrapartida;
  ftextoInformacaoComplementar := aSource.textoInformacaoComplementar;
  findicadorSinalLancamento := aSource.indicadorSinalLancamento;
  findicadorTipoPessoaContrapartida := aSource.indicadorTipoPessoaContrapartida;
end;

end.

