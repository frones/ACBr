{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{ - Elias César Vieira                                                         }
{ - Daniel Infocotidiano                                                       }
{ - Delcio Sbeghen                                                             }
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
  https://developers.sicoob.com.br/portal/apis
  https://developers.sicoob.com.br/portal/documentacao?slugItem=apis&slugSubItem=conta-corrente


*)

{$I ACBr.inc}
unit ACBrExtratoAPISicoob;

interface

uses
  Classes, SysUtils, ACBrAPIBase, ACBrJSON, ACBrExtratoAPI;

const
  cExtratoSicoobURLSandbox =
    'https://sandbox.sicoob.com.br/sicoob/sandbox/conta-corrente/v4/extrato';
  cExtratoSicoobURLProducao    = 'https://api.sicoob.com.br/conta-corrente/v4/extrato';
  cExtratoSicoobURLAuthSandbox =
    'https://auth.sicoob.com.br/auth/realms/cooperado/protocol/openid-connect/token';
  cExtratoSicoobURLAuthProducao =
    'https://auth.sicoob.com.br/auth/realms/cooperado/protocol/openid-connect/token';
  cExtratoSicoobPathCC = 'conta-corrente';

type

  { TACBrExtratoSicoobLancamentos }

  TACBrExtratoSicoobLancamentos = class(TACBrExtratoLancamentos)
  protected
    procedure WriteToJson(aJSon: TACBrJSONObject);
    procedure ReadFromJson(aJSon: TACBrJSONObject);
  end;

  { TACBrExtratoSicoobResult }
  TACBrExtratoSicoobResult = class(TACBrExtratoConsultado)
  protected
    function GetLancamentos: TACBrExtratoLancamentos; override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  end;

  { TACBrExtratoSiccobErro }

  TACBrExtratoSiccobErro = class(TACBrExtratoErro)
 protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  end;

  { TACBrExtratoSicoobErros }

  TACBrExtratoSicoobErros = class(TACBrAPISchemaArray)
  private
    function GetItem(aIndex: Integer): TACBrExtratoSiccobErro;
    procedure SetItem(aIndex: Integer; aValue: TACBrExtratoSiccobErro);
  protected
    function NewSchema: TACBrAPISchema; override;
  public
    function Add(aLancamento: TACBrExtratoSiccobErro): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrExtratoSiccobErro);
    function New: TACBrExtratoSiccobErro;
    property Items[aIndex: Integer]: TACBrExtratoSiccobErro read GetItem write SetItem; default;
  end;

  { TACBrExtratoAPISicoob }

  TACBrExtratoAPISicoob = class(TACBrExtratoAPIBancoClass)
  private
    fExtratoSicoobErros: TACBrExtratoSicoobErros;
  protected
    function GetExtratoSicoobErros: TACBrExtratoSicoobErros;
    function GetRespostaErro: TACBrExtratoErro; override;
    function GetExtratoConsultado: TACBrExtratoConsultado; override;
    function CalcularURL: String; override;
    procedure Autenticar; override;
  public
    destructor Destroy; override;
    function ConsultarExtrato(const aAgencia, aConta: String; const aDataInicio: TDateTime = 0;
      const aDataFim: TDateTime = 0; const aPagina: Integer = 0;
      const aRegistrosPorPag: Integer = 0): Boolean; override;
    property ExtratoSicoobErros: TACBrExtratoSicoobErros read GetExtratoSicoobErros;
  end;


implementation

uses
  synautil, synacode, DateUtils, StrUtils,
  ACBrSocket,
  ACBrUtil.Strings,
  ACBrUtil.DateTime,
  ACBrUtil.Base;

{ TACBrExtratoSicoobErros }

function TACBrExtratoSicoobErros.GetItem(aIndex: Integer): TACBrExtratoSiccobErro;
begin
  Result := TACBrExtratoSiccobErro(inherited Items[aIndex]);
end;

procedure TACBrExtratoSicoobErros.SetItem(aIndex: Integer; aValue: TACBrExtratoSiccobErro);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrExtratoSicoobErros.NewSchema: TACBrAPISchema;
begin
  Result := New;
end;

function TACBrExtratoSicoobErros.Add(aLancamento: TACBrExtratoSiccobErro): Integer;
begin
  Result := inherited Add(aLancamento);
end;

procedure TACBrExtratoSicoobErros.Insert(aIndex: Integer; aLancamento: TACBrExtratoSiccobErro);
begin
  inherited Insert(aIndex, aLancamento);
end;

function TACBrExtratoSicoobErros.New: TACBrExtratoSiccobErro;
begin
  Result := TACBrExtratoSiccobErro.Create;
  Self.Add(Result);
end;

{ TACBrExtratoSiccobErro }

procedure TACBrExtratoSiccobErro.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon//
  .AddPair('codigo', fpcodigo)//
  .AddPair('mensagem', fpmensagem);
end;

procedure TACBrExtratoSiccobErro.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon//
  .Value('codigo', fpcodigo)//
  .Value('mensagem', fpmensagem);
end;


{ TACBrExtratoAPISicoob }

function TACBrExtratoAPISicoob.CalcularURL: String;
begin
  Result := EmptyStr;
  if (not Assigned(fpOwner)) then
    Exit;

  if (fpOwner.Ambiente = eamProducao) then
    Result := cExtratoSicoobURLProducao
  else if (fpOwner.Ambiente = eamHomologacao) then
    Result := cExtratoSicoobURLSandbox;
end;

procedure TACBrExtratoAPISicoob.Autenticar;
var
  wURL, Body, BasicAutentication: String;
  js                            : TACBrJSONObject;
  qp                            : TACBrHTTPQueryParams;
  sec                           : Integer;
begin
  LimparHTTP;

  if (fpOwner.Ambiente = eamProducao) then
    wURL := cExtratoSicoobURLAuthProducao
  else
    wURL := cExtratoSicoobURLAuthSandbox;

  qp := TACBrHTTPQueryParams.Create;
  try
    qp.Values['client_id']  := ClientID;
    qp.Values['grant_type'] := 'client_credentials';
    qp.Values['scope'] := 'cco_transferencias cco_consulta openid';
    Body               := qp.AsURL;
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
        fpToken  := js.AsString['access_token'];
        sec      := js.AsInteger['expires_in'];
      finally
        js.Free;
      end;

      if EstaVazio(Trim(fpToken)) then
        DispararExcecao(EACBrAPIException.Create(ACBrStr('Erro de Autenticação')));

      fpValidadeToken := IncSecond(Now, sec);
      fpAutenticado   := True;
    end
  else
    DispararExcecao(EACBrAPIException.CreateFmt('Erro HTTP: %d, Metodo: %s, URL: %s',
      [HTTPResultCode, cHTTPMethodPOST, wURL]));
end;

function TACBrExtratoAPISicoob.GetExtratoSicoobErros: TACBrExtratoSicoobErros;
begin
  if (not Assigned(fExtratoSicoobErros)) then
    fExtratoSicoobErros := TACBrExtratoSicoobErros.Create('mensagens');
  Result            := fExtratoSicoobErros;
end;

function TACBrExtratoAPISicoob.GetRespostaErro: TACBrExtratoErro;
begin
  if (not Assigned(fpRespostaErro)) then
    fpRespostaErro := TACBrExtratoSiccobErro.Create;
  Result           := fpRespostaErro;
end;

function TACBrExtratoAPISicoob.GetExtratoConsultado: TACBrExtratoConsultado;
begin
  if (not Assigned(fpExtratoConsultado)) then
    fpExtratoConsultado := TACBrExtratoSicoobResult.Create('resultado');
  Result                := fpExtratoConsultado;
end;

destructor TACBrExtratoAPISicoob.Destroy;
begin
  if Assigned(fExtratoSicoobErros) then
    fExtratoSicoobErros.Free;
  inherited Destroy;
end;

function TACBrExtratoAPISicoob.ConsultarExtrato(const aAgencia, aConta: String;
  const aDataInicio: TDateTime; const aDataFim: TDateTime; const aPagina: Integer;
  const aRegistrosPorPag: Integer): Boolean;
begin
  Result := False;
  if EstaVazio(aAgencia) or EstaVazio(aConta) then
    Exit;

  PrepararHTTP;
  RegistrarLog('  TACBrExtratoAPISicoob.ConsultarExtrato(Ag: ' + aAgencia + ' - Conta: ' +
    aConta + ')');

  if MonthOf(aDataInicio) <> MonthOf(aDataFim) then
    DispararExcecao(EACBrAPIException.Create
      (ACBrStr('Sicoob só permite intervalo de datas dentro do mesmo mês.')));

  URLPathParams.Add(IntToStr(MonthOf(aDataInicio)));
  URLPathParams.Add(IntToStr(YearOf(aDataInicio)));

  URLQueryParams.Values['agruparCNAB'] := 'false';
  URLQueryParams.Values['numeroContaCorrente'] := aConta;

  HTTPSend.Headers.Add('Content-Type:application/json');
  HTTPSend.Headers.Add('Accept:application/json');

  if NaoEstaVazio(fpToken) then
    HTTPSend.Headers.Insert(0, ChttpHeaderAuthorization + cHTTPAuthorizationBearer + ' ' + fpToken);

  HTTPSend.Headers.Add('client_id:' + ClientID);

  HTTPMethod(cHTTPMethodGET, CalcularURL);

  Result := (HTTPResultCode = HTTP_OK);
  if Result then
    ExtratoConsultado.AsJSON :=   DecodeToString(HTTPResponse, RespIsUTF8)
  else
    begin
      ExtratoSicoobErros.AsJSON := DecodeToString(HTTPResponse, RespIsUTF8);
      if NaoEstaZerado(ExtratoSicoobErros.Count) then
        RespostaErro.AsJSON := ExtratoSicoobErros[0].AsJSON;
    end;
end;

{ TACBrExtratoSicoobResult }

function TACBrExtratoSicoobResult.GetLancamentos: TACBrExtratoLancamentos;
begin
  if (not Assigned(fpLancamentos)) then
    fpLancamentos := TACBrExtratoSicoobLancamentos.Create;
  Result          := fpLancamentos;
end;

procedure TACBrExtratoSicoobResult.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon                                                  //
    .AddPair('saldoAtual', FSaldoAtual)                  //
    .AddPair('saldoBloqueado', FSaldoBloqueado)          //
    .AddPair('saldoLimite', FSaldoLimite)                //
    .AddPair('saldoAnterior', FSaldoAnterior)            //
    .AddPair('quantidadeTotalPagina', Lancamentos.Count) //
    .AddPair('quantidadeTotalRegistro', Lancamentos.Count);
  (Lancamentos as TACBrExtratoSicoobLancamentos).WriteToJSon(aJSon);
end;

procedure TACBrExtratoSicoobResult.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon                                       //
    .Value('saldoAtual', FSaldoAtual)         //
    .Value('saldoBloqueado', FSaldoBloqueado) //
    .Value('saldoLimite', FSaldoLimite)       //
    .Value('saldoAnterior', FSaldoAnterior);
  fpRegistrosPaginaAtual := aJSon.AsJSONArray['transacoes'].Count;
  fpTotalPaginas         := 1;
  fpTotalRegistros       := fpRegistrosPaginaAtual;
  (Lancamentos as TACBrExtratoSicoobLancamentos).ReadFromJSon(aJSon);
end;


procedure TACBrExtratoSicoobLancamentos.WriteToJson(aJSon: TACBrJSONObject);
var
  JTransacoes: TACBrJSONArray;
  JTransacao : TACBrJSONObject;
  Transacao  : TACBrExtratoLancamento;
  I          : Integer;
begin
  JTransacoes := aJSon.AsJSONArray['transacoes'];

  for I := 0 to Count - 1 do
    begin
      JTransacao := TACBrJSONObject.Create;;
      Transacao  := Items[I];
      JTransacao.AddPair('transactionId', Transacao.Identificador);
      case Transacao.TipoOperacao of
        etoDebito: JTransacao.AddPair('tipo', 'DEBITO');
        etoCredito: JTransacao.AddPair('tipo', 'CREDITO');
      else JTransacao.AddPair('tipo', '');
      end;
      JTransacao.AddPair('valor', Transacao.Valor);
      JTransacao.AddPair('data', Transacao.DataMovimento);
      JTransacao.AddPair('dataLote', Transacao.DataLancamento);
      JTransacao.AddPair('descricao', Transacao.Descricao);
      JTransacao.AddPair('numeroDocumento', Transacao.NumeroDocumento);
      JTransacao.AddPair('cpfCnpj', Transacao.CPFCNPJ);
      JTransacao.AddPair('descInfComplementar', Transacao.InfoComplementar);
    end;

end;

procedure TACBrExtratoSicoobLancamentos.ReadFromJson(aJSon: TACBrJSONObject);
var
  JTransacoes: TACBrJSONArray;
  JTransacao : TACBrJSONObject;
  Transacao  : TACBrExtratoLancamento;
  I          : Integer;
begin
  JTransacoes := aJSon.AsJSONArray['transacoes'];

  for I := 0 to JTransacoes.Count - 1 do
    begin
      JTransacao              := JTransacoes.ItemAsJSONObject[I];
      Transacao               := New;
      Transacao.Identificador := JTransacao.AsString['transactionId'];
      case AnsiIndexStr(JTransacao.AsString['tipo'], ['DEBITO', 'CREDITO']) of
        0: Transacao.TipoOperacao := etoDebito;
        1: Transacao.TipoOperacao := etoCredito;
      else Transacao.TipoOperacao := etoNenhum;
      end;
      Transacao.Valor            := JTransacao.AsCurrency['valor'];
      Transacao.DataMovimento    := JTransacao.AsISODateTime['data'];
      Transacao.DataLancamento   := JTransacao.AsISODate['dataLote'];
      Transacao.Descricao        := JTransacao.AsString['descricao'];
      Transacao.NumeroDocumento  := JTransacao.AsString['numeroDocumento'];
      Transacao.CPFCNPJ          := JTransacao.AsString['cpfCnpj'];
      Transacao.InfoComplementar := JTransacao.AsString['descInfComplementar'];
    end;
end;


end.
