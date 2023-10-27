{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2022 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
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
  https://pixpdv.com.br/api/index.htm

*)

{$I ACBr.inc}

unit ACBrPIXSchemasPixPDV;

interface

uses
  Classes, SysUtils,
  ACBrPIXBase, ACBrPIXSchemasDevedor, ACBrPIXSchemasCobV, ACBrJSON;

type 

  TPixPDVQrStatusTipo = (
    pqsNone,
    pqsApproved,
    pqsCreated,
    pqsRefunded,
    pqsRejected,
    pqsCanceling,
    pqsCanceled,
    pqsPartial,
    pqsExpired,
    pqsTimeOut,
    pqsOverFilled,
    pqsUnfinished,
    pqsError,
    pqsFatalError,
    pqsAuthorized,
    pqsCaptured
  );

  { TPixPDVError }

  TPixPDVError = class(TACBrPIXSchema)
  private
    fCode: Integer;
    fDescription: string;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: string = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TPixPDVError);

    property Code: Integer read fCode;
    property Description: string read fDescription;
  end;

  { TPixPDVStatusTokenDados }

  TPixPDVStatusTokenDados = class(TACBrPIXSchema)
  private
    fCnpj: string;
    fFantasia: string;
    fNome: string;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: string); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TPixPDVStatusTokenDados);

    property Cnpj: string read fCnpj;
    property Nome: string read fNome;
    property Fantasia: string read fFantasia;
  end;

  { TPixPDVQrDinamico }

  TPixPDVQrDinamico = class(TACBrPIXSchema)
  private
    fValor: Currency;
    fMinutos: Integer;
    fMensagem: string;
    fImagem: Boolean;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: string = ''); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TPixPDVQrDinamico);

    property Valor: Currency read fValor write fValor;
    property Minutos: Integer read fMinutos write fMinutos;
    property Mensagem: string read fMensagem write fMensagem;
    property Imagem: Boolean read fImagem write fImagem;
  end;

  { TPixPDVPagador }

  TPixPDVPagador = class(TACBrPIXDevedorRecebedorBase)
  private
    fBairro: string;
    fTelefone: string;
  protected
    procedure WriteToJSon(aJSon: TACBrJSONObject); reintroduce;
    procedure ReadFromJSon(aJSon: TACBrJSONObject);  reintroduce;
  public
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TPixPDVPagador);

    property Cpf;
    property Cnpj;
    property Nome;
    property NomeFantasia;
    property Email;
    property Logradouro;
    property Cidade;
    property Uf;
    property Cep;

    property Bairro: string read fBairro write fBairro;
    property Telefone: string read fTelefone write fTelefone;
  end;

  { TPixPDVJuros }

  TPixPDVJuros = class(TACBrPIXJuros)
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  end;

  { TPixPDVMulta }

  TPixPDVMulta = class(TACBrPIXModalidadeValor)
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
  end;

  { TPixPDVDescontoDataFixa }

  TPixPDVDescontoDataFixa = class(TACBrPIXDescontoDataFixa)
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  end;

  { TPixPDVDescontosDataFixa }

  TPixPDVDescontosDataFixa = class(TACBrPIXDescontosDataFixa)
  private
    function GetItem(Index: Integer): TPixPDVDescontoDataFixa; reintroduce;
    procedure SetItem(Index: Integer; aValue: TPixPDVDescontoDataFixa);  reintroduce;
  public
    function New: TPixPDVDescontoDataFixa;
    property Items[Index: Integer]: TPixPDVDescontoDataFixa read GetItem write SetItem; default;
  end;

  { TPixPDVDesconto }

  TPixPDVDesconto = class(TACBrPIXDesconto)
  private
    function GetDescontos: TPixPDVDescontosDataFixa;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: string); override;
    property descontosDataFixa: TPixPDVDescontosDataFixa read GetDescontos;
  end;

  { TPixPDVQrCobranca }

  TPixPDVQrCobranca = class(TACBrPIXSchema)
  private
    fValor: Currency;
    fVencimento: TDateTime;
    fExpira: Integer;
    fMensagem: string;
    fImagem: Boolean;
    fDocumento: string;
    fPagador: TPixPDVPagador;
    fJuros: TPixPDVJuros;
    fMulta: TPixPDVMulta;
    fDesconto: TPixPDVDesconto;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: string = ''); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TPixPDVQrCobranca);

    property Valor: Currency read fValor write fValor;
    property Vencimento: TDateTime read fVencimento write fVencimento;
    property Expira: Integer read fExpira write fExpira;
    property Mensagem: string read fMensagem write fMensagem;
    property Imagem: Boolean read fImagem write fImagem;
    property Documento: string read fDocumento write fDocumento;

    property Pagador: TPixPDVPagador read fPagador;
    property Juros: TPixPDVJuros read fJuros;
    property Multa: TPixPDVMulta read fMulta;
    property Desconto: TPixPDVDesconto read fDesconto;
  end;

  { TPixPDVQrGerado }

  TPixPDVQrGerado = class(TACBrPIXSchema)
  private
    fQrCodeId: string;
    fQrCode: string;
    fQrCodeBase64: string;
    fUrl: string;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TPixPDVQrGerado);

    property QrCodeId: string read fQrCodeId;
    property QrCode: string read fQrCode;
    property QrCodeBase64: string read fQrCodeBase64;
    property Url: string read fUrl;
  end;

  { TPixPDVQrStatusSender }

  TPixPDVQrStatusSender = class(TACBrPIXSchema)
  private
    fNome: string;
    fCpf_Cnpj: string;
    fData: TDateTime;
    fValor: Currency;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: string); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TPixPDVQrStatusSender);

    property Nome: string read fNome;
    property Cpf_Cnpj: string read fCpf_Cnpj;
    property Data: TDateTime read fData;
    property Valor: Currency read fValor;
  end;

  { TPixPDVQrStatus }

  TPixPDVQrStatus = class(TACBrPIXSchema)
  private
    fStatus: TPixPDVQrStatusTipo;
    fEndToEndId: string;
    fIdentificadorId: string;
    fSender: TPixPDVQrStatusSender;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: string); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TPixPDVQrStatus);

    property Status: TPixPDVQrStatusTipo read fStatus;
    property EndToEndId: string read fEndToEndId;
    property IdentificadorId: string read fIdentificadorId;
    property Sender: TPixPDVQrStatusSender read fSender;
  end;

  { TPixPDVQrRefund }

  TPixPDVQrRefund = class(TACBrPIXSchema)
  private
    fQrRefundId: string;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: string); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TPixPDVQrRefund);

    property QrRefundId: String read fQrRefundId;
  end;

  { TPixPDVQrResumoItem }

  TPixPDVQrResumoItem = class(TACBrPIXSchema)
  private
    fStatus: TPixPDVQrStatusTipo;
    fTransacaoId: string;
    fTransacaoTipo: string;
    fMensagem: string;
    fEndToEndId: string;
    fIdentificadorId: string;
    fSender: TPixPDVQrStatusSender;
    fExpirou: TDateTime;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(AJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(AJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: string); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TPixPDVQrResumoItem);

    property Status: TPixPDVQrStatusTipo read fStatus;
    property TransacaoId: string read fTransacaoId;
    property TransacaoTipo: string read fTransacaoTipo;
    property Mensagem: string read fMensagem;
    property EndToEndId: string read fEndToEndId;
    property IdentificadorId: string read fIdentificadorId;
    property Sender: TPixPDVQrStatusSender read fSender;
    property Expirou: TDateTime read fExpirou;
  end;

  { TPixPDVQrResumoItemArray }

  TPixPDVQrResumoItemArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TPixPDVQrResumoItem;
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(AItem: TPixPDVQrResumoItem): Integer;
    function New: TPixPDVQrResumoItem;
    property Items[Index: Integer]: TPixPDVQrResumoItem read GetItem; default;
  end;

  { TPixPDVQrResumo }

  TPixPDVQrResumo = class(TACBrPIXSchema)
  private
    fItens: TPixPDVQrResumoItemArray;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TPixPDVQrResumo);

    property Itens: TPixPDVQrResumoItemArray read fItens;
  end;

  { TPixPDVQrSimulaPagar }

  TPixPDVQrSimulaPagar = class(TACBrPIXSchema)
  public
    constructor Create(const aObjectName: string); override;
    destructor Destroy; override;
  end;

  { TPixPDVSaldo }

  TPixPDVSaldo = class(TACBrPIXSchema)
  private
    fTotal: Currency;
    fDisponivel: Currency;
    fBloqueado: Currency;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: string); override;
    procedure Clear; override;
    procedure Assign(aSource: TPixPDVSaldo);

    property Total: Currency read fTotal;
    property Disponivel: Currency read fDisponivel;
    property Bloqueado: Currency read fBloqueado;
  end;

  { TPixPDVRetirada }

  TPixPDVRetirada = class(TACBrPIXSchema)
  private
    fTransacaoId: string;
    fEndToEndId: string;
    fPspId: string;
    fPspNome: string;
    fAgencia: string;
    fConta: string;
    fContaNome: string;
    fContaTipo: string;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: string); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TPixPDVRetirada);

    property TransacaoId: string read fTransacaoId;
    property EndToEndId: string read fEndToEndId;
    property PspId: string read fPspId;
    property PspNome: string read fPspNome;
    property Agencia: string read fAgencia;
    property Conta: string read fConta;
    property ContaNome: string read fContaNome;
    property ContaTipo: string read fContaTipo;
  end;

  { TPixPDVExtratoItem }

  TPixPDVExtratoItem = class(TACBrPIXSchema)
  private
    fTransacaoId: string;
    fTransacaoTipo: string;
    fDescricao: string;
    fData: TDateTime;
    fValor: Currency;
    fTipo: string;
  protected
    procedure AssignSchema(aSource: TACBrPIXSchema); override;
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const ObjectName: string); override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TPixPDVExtratoItem);

    property TransacaoId: string read fTransacaoId;
    property TransacaoTipo: string read fTransacaoTipo;
    property Descricao: string read fDescricao;
    property Data: TDateTime read fData;
    property Valor: Currency read fValor;
    property Tipo: string read fTipo;
  end;

  { TPixPDVExtratoItemArray }

  TPixPDVExtratoItemArray = class(TACBrPIXSchemaArray)
  private
    function GetItem(Index: Integer): TPixPDVExtratoItem;
  protected
    function NewSchema: TACBrPIXSchema; override;
  public
    Function Add(AItem: TPixPDVExtratoItem): Integer;
    function New: TPixPDVExtratoItem;
    property Items[Index: Integer]: TPixPDVExtratoItem read GetItem; default;
  end;

  { TPixPDVExtrato }

  TPixPDVExtrato = class(TACBrPIXSchema)
  private
    fItens: TPixPDVExtratoItemArray;
  protected
    procedure DoWriteToJSon(aJSon: TACBrJSONObject); override;
    procedure DoReadFromJSon(aJSon: TACBrJSONObject); override;
  public
    constructor Create(const aObjectName: String); override;
    destructor Destroy; override;
    procedure Clear; override;
    function IsEmpty: Boolean; override;
    procedure Assign(aSource: TPixPDVExtrato);

    property Itens: TPixPDVExtratoItemArray read fItens;
  end;

  function PixPDVQrStatusToString(aStatus: TPixPDVQrStatusTipo): String;
  function StringToPixPDVQrStatus(const aString: String): TPixPDVQrStatusTipo;

implementation

uses
  synautil,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.DateTime;

{ TPixPDVError }

procedure TPixPDVError.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if (fCode > 0) then
    aJSon.AddPair('code', fCode);

  if NaoEstaVazio(fDescription) then
    aJSon.AddPair('description', fDescription);
end;

procedure TPixPDVError.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('code', fCode)
    .Value('description', fDescription);
end;

constructor TPixPDVError.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TPixPDVError.Clear;
begin
  fCode := 0;
  fDescription := EmptyStr;
end;

function TPixPDVError.IsEmpty: Boolean;
begin
  Result := (fCode = 0) and EstaVazio(fDescription);
end;

procedure TPixPDVError.Assign(aSource: TPixPDVError);
begin
  fCode := aSource.Code;
  fDescription := aSource.Description;
end;

{ TPixPDVStatusTokenDados }

procedure TPixPDVStatusTokenDados.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('cnpj', fCnpj)
    .AddPair('nome', fNome)
    .AddPair('fantasia', fFantasia);
end;

procedure TPixPDVStatusTokenDados.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('cnpj', fCnpj)
    .Value('nome', fNome)
    .Value('fantasia', fFantasia);
end;

constructor TPixPDVStatusTokenDados.Create(const aObjectName: string);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TPixPDVStatusTokenDados.Clear;
begin
  fCnpj := EmptyStr;
  fNome := EmptyStr;
  fFantasia := EmptyStr;
end;

function TPixPDVStatusTokenDados.IsEmpty: Boolean;
begin
  Result := EstaVazio(fCnpj) and
            EstaVazio(fNome) and
            EstaVazio(fFantasia);
end;

procedure TPixPDVStatusTokenDados.Assign(aSource: TPixPDVStatusTokenDados);
begin
  fCnpj := aSource.Cnpj;
  fNome := aSource.Nome;
  fFantasia := aSource.Fantasia;
end;

{ TPixPDVQrDinamico }

procedure TPixPDVQrDinamico.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('valor', fValor);
  if (fMinutos > 0) then
    aJSon.AddPair('minutos', fMinutos);
  if NaoEstaVazio(fMensagem) then
    aJSon.AddPair('mensagem', fMensagem);
  if fImagem then
    aJSon.AddPair('imagem', fImagem);
end;

procedure TPixPDVQrDinamico.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('valor', fValor)
    .Value('minutos', fMinutos)
    .Value('mensagem', fMensagem)
    .Value('imagem', fImagem);
end;

constructor TPixPDVQrDinamico.Create(const aObjectName: string);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TPixPDVQrDinamico.Clear;
begin
  fValor := 0;
  fMinutos := 0;
  fMensagem := EmptyStr;
  fImagem := False;
end;

function TPixPDVQrDinamico.IsEmpty: Boolean;
begin
  Result := (fValor = 0) and
            (fMinutos = 0) and
            (fMensagem = EmptyStr) and
            (not fImagem);
end;

procedure TPixPDVQrDinamico.Assign(aSource: TPixPDVQrDinamico);
begin
  fValor := aSource.Valor;
  fMinutos := aSource.Minutos;
  fMensagem := aSource.Mensagem;
  fImagem := aSource.Imagem;
end;

{ TPixPDVPagador }

procedure TPixPDVPagador.WriteToJSon(aJSon: TACBrJSONObject);
var
  jo: TACBrJSONObject;
begin
  jo := TACBrJSONObject.Create;

  if NaoEstaVazio(Nome) then
    jo.AddPair('nome', Nome);
  if NaoEstaVazio(nomeFantasia) then
    jo.AddPair('fantasia', NomeFantasia);
  if NaoEstaVazio(Cnpj) then
    jo.AddPair('cpf_cnpj', Cnpj)
  else if NaoEstaVazio(Cpf) then
    jo.AddPair('cpf_cnpj', Cpf);
  if NaoEstaVazio(Logradouro) then
    jo.AddPair('endereco', Logradouro);
  if NaoEstaVazio(Bairro) then
    jo.AddPair('bairro', fBairro);
  if NaoEstaVazio(Cidade) then
    jo.AddPair('cidade', Cidade);
  if NaoEstaVazio(Uf) then
    jo.AddPair('estado', Uf);
  if NaoEstaVazio(Cep) then
    jo.AddPair('cep', Cep);
  if NaoEstaVazio(Email) then
    jo.AddPair('email', Email);
  if NaoEstaVazio(Telefone) then
    jo.AddPair('telefone', fTelefone);

  aJSon.AddPair('pagador', jo);
end;

procedure TPixPDVPagador.ReadFromJSon(aJSon: TACBrJSONObject);
var
  s: String;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  aJSon
    .Value('bairro', fBairro)
    .Value('telefone', fTelefone)
    .Value('cpf_cnpj', s);
  Cnpj := s;
  Cpf := s;
  aJSon.Value('nome', s);
  Nome := s;
  aJSon.Value('fantasia', s);
  NomeFantasia := s;

  aJSon.Value('endereco', s);
  Logradouro := s;
  aJSon.Value('cidade', s);
  Cidade := s;
  aJSon.Value('estado', s);
  Uf := s;
  aJSon.Value('cep', s);
  Cep := s;
  aJSon.Value('email', s);
  Email := s;
end;

procedure TPixPDVPagador.Clear;
begin
  inherited Clear;
  fBairro := EmptyStr;
  fTelefone := EmptyStr;
end;

function TPixPDVPagador.IsEmpty: Boolean;
begin
  Result := inherited IsEmpty and EstaVazio(fTelefone) and EstaVazio(fBairro);
end;

procedure TPixPDVPagador.Assign(aSource: TPixPDVPagador);
begin
  inherited Assign(aSource);
  fBairro := aSource.Bairro;
  fTelefone := aSource.Telefone;
end;

{ TPixPDVJuros }

procedure TPixPDVJuros.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if (modalidade = pjmNenhum) then
    Exit;

  aJSon
    .AddPair('tipo', Ord(modalidade))
    .AddPair('valor', valorPerc);
end;

procedure TPixPDVJuros.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
  c: Currency;
begin
  {$IfDef FPC}
  c := 0;
  i := 0;
  {$EndIf}

  aJSon
    .Value('tipo', i)
    .Value('valor', c);

  modalidade := TACBrPIXJurosModalidade(i);
  valorPerc := c;
end;

{ TPixPDVMulta }

procedure TPixPDVMulta.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if (modalidade = pvmNenhum) then
    Exit;

  aJSon
    .AddPair('tipo', Ord(modalidade))
    .AddPair('valor', valorPerc);
end;

procedure TPixPDVMulta.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  i: Integer;
  c: Currency;
begin
  {$IfDef FPC}
  i := 0;
  c := 0;
  {$EndIf}

  aJSon
    .Value('tipo', i)
    .Value('valor', c);

  modalidade := TACBrPIXValoresModalidade(i);
  valorPerc := c;
end;

{ TPixPDVDescontosDataFixa }

function TPixPDVDescontosDataFixa.GetItem(Index: Integer): TPixPDVDescontoDataFixa;
begin
  Result := TPixPDVDescontoDataFixa(inherited Items[Index]);
end;

procedure TPixPDVDescontosDataFixa.SetItem(Index: Integer; aValue: TPixPDVDescontoDataFixa);
begin
  inherited Items[Index] := aValue;
end;

function TPixPDVDescontosDataFixa.New: TPixPDVDescontoDataFixa;
begin
  Result := TPixPDVDescontoDataFixa.Create;
  Self.Add(Result);
end;

{ TPixPDVDescontoDataFixa }

procedure TPixPDVDescontoDataFixa.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
(*
   aJSon.AddPair('data', FormatDateTime('yyyy-mm-dd', data));
   if (valorPerc > 0) then
     aJSon.AddPair('valor', valorPerc);
*)
end;

procedure TPixPDVDescontoDataFixa.DoReadFromJSon(aJSon: TACBrJSONObject);
(*
var
  s: String;
  c: Currency;
*)
begin
(*
  {$IfDef FPC}
  s := EmptyStr;
  c := 0;
  {$EndIf}

  aJSon
    .Value('data', s)
    .Value('valor', c);

  data :=  StringToDateTimeDef(s, 0, 'yyyy-mm-dd');
  valorPerc := c;
*)
end;

{ TPixPDVDesconto }

function TPixPDVDesconto.GetDescontos: TPixPDVDescontosDataFixa;
begin
  Result := TPixPDVDescontosDataFixa(fDescontosDataFixa);
end;

procedure TPixPDVDesconto.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if (modalidade = pdmNenhum) then
    Exit;

  aJSon.AddPair('tipo', Ord(modalidade));
  aJSon.AddPair('valor', DescontosDataFixa.Items[0].valorPerc);
  if (Ord(modalidade) in [1, 2]) then
    aJSon.AddPair('data', FormatDateBr(DescontosDataFixa.Items[0].data));
end;

procedure TPixPDVDesconto.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: string;
  i: Integer;
  c: Currency;
begin
  {$IfDef FPC}
  s := EmptyStr;
  i := 0;
  c := 0;
  {$EndIf}

  aJSon.Value('tipo', i);
  modalidade := TACBrPIXDescontoModalidade(i);
  aJSon.Value('valor', c);
  valorPerc := c;
  if (Ord(modalidade) in [1, 2]) then begin
    aJSon.Value('data', s);
    DescontosDataFixa.Items[0].data := StringToDateTimeDef(s, 0, 'dd/mm/yyyy');
  end;
end;

constructor TPixPDVDesconto.Create(const ObjectName: String);
begin
  inherited Create(ObjectName);
end;

{ TPixPDVQrCobranca }

procedure TPixPDVQrCobranca.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if NaoEstaVazio(documento) then
    aJSon.AddPair('documento', documento);
  if (expira > 0) then
    aJSon.AddPair('expira', expira);
  if imagem then
    aJSon.AddPair('imagem', imagem);
  if NaoEstaVazio(mensagem) then
    aJSon.AddPair('mensagem', mensagem);
  if (valor > 0) then
    aJSon.AddPair('valor', valor);
  if (vencimento > 0) then
    aJSon.AddPair('vencimento', FormatDateBr(vencimento));

  Pagador.WriteToJSon(aJSon);
  Juros.WriteToJSon(aJSon);
  Multa.WriteToJSon(aJSon);
  Desconto.WriteToJSon(aJSon);
end;

procedure TPixPDVQrCobranca.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('valor', fValor)
    .ValueISODate('vencimento', fVencimento)
    .Value('expira', fExpira)
    .Value('mensagem', fMensagem)
    .Value('imagem', fImagem)
    .Value('documento', fDocumento);

  Pagador.ReadFromJSon(aJSon);
  Juros.ReadFromJSon(aJSon);
  Multa.ReadFromJSon(aJSon);
  Desconto.ReadFromJSon(aJSon);
end;

constructor TPixPDVQrCobranca.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fPagador := TPixPDVPagador.Create('pagador');
  fJuros := TPixPDVJuros.Create('juros');
  fMulta := TPixPDVMulta.Create('multa');
  fDesconto := TPixPDVDesconto.Create('desconto');
  Clear;
end;

destructor TPixPDVQrCobranca.Destroy;
begin
  fPagador.Free;
  fJuros.Free;
  fMulta.Free;
  fDesconto.Free;
  inherited Destroy;
end;

procedure TPixPDVQrCobranca.Clear;
begin
  fValor := 0;
  fVencimento := 0;
  fExpira := 0;
  fMensagem := EmptyStr;
  fImagem := False;
  fDocumento := EmptyStr;
  fPagador.Clear;
  fJuros.Clear;
  fMulta.Clear;
  fDesconto.Clear;
end;

function TPixPDVQrCobranca.IsEmpty: Boolean;
begin
  Result := (fValor = 0) and
            (fExpira = 0) and
            (fVencimento = 0) and
            (not fImagem) and
            EstaVazio(fMensagem) and
            EstaVazio(fDocumento) and
            (Pagador.IsEmpty) and
            (Juros.IsEmpty) and
            (Multa.IsEmpty) and
            (Desconto.IsEmpty);
end;

procedure TPixPDVQrCobranca.Assign(aSource: TPixPDVQrCobranca);
begin
  fValor := aSource.Valor;
  fVencimento := aSource.Vencimento;
  fExpira := aSource.Expira;
  fMensagem := aSource.Mensagem;
  fImagem := aSource.Imagem;
  fDocumento := aSource.Documento;
  fPagador.Assign(aSource.Pagador);
  fJuros.Assign(aSource.Juros);
  fMulta.Assign(aSource.Multa);
  fDesconto.Assign(aSource.Desconto);
end;


{******************************************************************************}

{ TPixPDVQrGerado }

procedure TPixPDVQrGerado.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if NaoEstaVazio(fQrCodeId) then
    aJSon.AddPair('qrcodeId', fQrCodeId);
  if NaoEstaVazio(fQrCode) then
    aJSon.AddPair('qrcode', fQrCode);
  if NaoEstaVazio(fQrCodeBase64) then
    aJSon.AddPair('qrcodeBase64', fQrCodeBase64);
  if NaoEstaVazio(fUrl) then
    aJSon.AddPair('url', fUrl);
end;

procedure TPixPDVQrGerado.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('qrcodeId', fQrCodeId)
    .Value('qrcode', fQrCode)
    .Value('qrcodeBase64', fQrCodeBase64)
    .Value('url', fUrl);
end;

constructor TPixPDVQrGerado.Create(const aObjectName: string);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TPixPDVQrGerado.Clear;
begin
  fQrCodeId := EmptyStr;
  fQrCode := EmptyStr;
  fQrCodeBase64 := EmptyStr;
  fUrl := EmptyStr;
end;

function TPixPDVQrGerado.IsEmpty: Boolean;
begin
  Result := EstaVazio(fQrCodeId) and
            EstaVazio(fQrCode) and
            EstaVazio(fQrCodeBase64) and
            EstaVazio(fUrl);
end;

procedure TPixPDVQrGerado.Assign(aSource: TPixPDVQrGerado);
begin
  fQrCodeId := aSource.QrCodeId;
  fQrCode := aSource.QrCode;
  fQrCodeBase64 := aSource.QrCodeBase64;
  fUrl := aSource.Url;
end;

{ TPixPDVQrStatusSender }

procedure TPixPDVQrStatusSender.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if NaoEstaVazio(fNome) then
    aJSon.AddPair('nome', fNome);
  if NaoEstaVazio(fCpf_Cnpj) then
    aJSon.AddPair('cpf_cnpj', fCpf_Cnpj);
  if (fData > 0) then
    aJSon.AddPair('data', FormatDateBr(fData));
  if (fValor > 0) then
    aJSon.AddPair('valor', fValor);
end;

procedure TPixPDVQrStatusSender.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('nome', fNome)
    .Value('cpf_cnpj', fCpf_Cnpj)
    .ValueISODateTime('data', fData)
    .Value('valor', fValor);
end;

constructor TPixPDVQrStatusSender.Create(const aObjectName: string);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TPixPDVQrStatusSender.Clear;
begin
  fNome := EmptyStr;
  fCpf_Cnpj := EmptyStr;
  fData := 0;
  fValor := 0;
end;

function TPixPDVQrStatusSender.IsEmpty: Boolean;
begin
  Result := EstaVazio(fNome) and EstaVazio(fCpf_Cnpj) and (fData = 0) and (fValor = 0);
end;

procedure TPixPDVQrStatusSender.Assign(aSource: TPixPDVQrStatusSender);
begin
  fNome := aSource.Nome;
  fCpf_Cnpj := aSource.Cpf_Cnpj;
  fData := aSource.Data;
  fValor := aSource.Valor;
end;

{ TPixPDVQrStatus }

procedure TPixPDVQrStatus.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  if (fStatus <> pqsNone) then
    aJSon.AddPair('status', PixPDVQrStatusToString(fStatus));
  if NaoEstaVazio(fEndToEndId) then
    aJSon.AddPair('endToEndId', fEndToEndId);
  if NaoEstaVazio(fIdentificadorId) then
    aJSon.AddPair('identificadorId', fIdentificadorId);

  fSender.WriteToJSon(aJSon);
end;

procedure TPixPDVQrStatus.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: string;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  aJSon
    .Value('status', s)
    .Value('endToEndId', fEndToEndId)
    .Value('identificadorId', fIdentificadorId);

  fStatus := StringToPixPDVQrStatus(s);
  fSender.ReadFromJSon(aJSon);
end;

constructor TPixPDVQrStatus.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fSender := TPixPDVQrStatusSender.Create('sender');
  Clear;
end;

destructor TPixPDVQrStatus.Destroy;
begin
  fSender.Free;
  inherited Destroy;
end;

procedure TPixPDVQrStatus.Clear;
begin
  fStatus := pqsNone;
  fEndToEndId := EmptyStr;
  fIdentificadorId := EmptyStr;
  fSender.Clear;
end;

function TPixPDVQrStatus.IsEmpty: Boolean;
begin
  Result := (fStatus = pqsNone) and
            EstaVazio(fEndToEndId) and
            EstaVazio(fIdentificadorId) and
            (fSender.IsEmpty);
end;

procedure TPixPDVQrStatus.Assign(aSource: TPixPDVQrStatus);
begin
  fStatus := aSource.Status;
  fEndToEndId := aSource.EndToEndId;
  fIdentificadorId := aSource.IdentificadorId;
  fSender.Assign(aSource.Sender);
end;

{ TPixPDVQrRefund }

procedure TPixPDVQrRefund.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('refundId', fQrRefundId);
end;

procedure TPixPDVQrRefund.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon.Value('refundId', fQrRefundId)
end;

constructor TPixPDVQrRefund.Create(const aObjectName: string);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TPixPDVQrRefund.Clear;
begin
  fQrRefundId := EmptyStr;
end;

function TPixPDVQrRefund.IsEmpty: Boolean;
begin
  Result := EstaVazio(fQrRefundId);
end;

procedure TPixPDVQrRefund.Assign(aSource: TPixPDVQrRefund);
begin
  fQrRefundId := aSource.QrRefundId;
end;

{ TPixPDVQrResumoItem }

constructor TPixPDVQrResumoItem.Create(const ObjectName: string);
begin
  inherited Create(ObjectName);
  fSender := TPixPDVQrStatusSender.Create('sender');
  Clear;
end;

destructor TPixPDVQrResumoItem.Destroy;
begin
  fSender.Free;
  inherited Destroy;
end;

procedure TPixPDVQrResumoItem.Clear;
begin
  fStatus := pqsNone;
  fTransacaoId := EmptyStr;
  fTransacaoTipo := EmptyStr;
  fMensagem := EmptyStr;
  fEndToEndId := EmptyStr;
  fIdentificadorId := EmptyStr;
  fSender.Clear;
  fExpirou := 0;
end;

function TPixPDVQrResumoItem.IsEmpty: Boolean;
begin
  Result := (fStatus = pqsNone) and
            (fTransacaoId = EmptyStr) and
            (fTransacaoTipo = EmptyStr);
end;

procedure TPixPDVQrResumoItem.Assign(aSource: TPixPDVQrResumoItem);
begin
  fStatus := aSource.Status;
  fTransacaoId := aSource.TransacaoId;
  fTransacaoTipo := aSource.TransacaoTipo;
  fMensagem := aSource.Mensagem;
  fEndToEndId := aSource.EndToEndId;
  fIdentificadorId := aSource.IdentificadorId;
  fSender := aSource.Sender;
  fExpirou := aSource.Expirou;
end;

procedure TPixPDVQrResumoItem.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TPixPDVQrResumoItem) then
    Assign(TPixPDVQrResumoItem(aSource));
end;

procedure TPixPDVQrResumoItem.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon.AddPair('status', PixPDVQrStatusToString(fStatus));
  aJSon.AddPair('transacaoId', fTransacaoId);
  aJSon.AddPair('transacaoTipo', fTransacaoTipo);
  if (fStatus = pqsExpired) then
    aJSon.AddPairISODateTime('expirou', fExpirou)
  else if (fStatus = pqsApproved) then
  begin
    aJSon.AddPair('mensagem', fMensagem);
    aJSon.AddPair('endToEndId', fEndToEndId);
    aJSon.AddPair('identificadorId', fIdentificadorId);

    fSender.WriteToJSon(aJSon);
  end;
end;

procedure TPixPDVQrResumoItem.DoReadFromJSon(aJSon: TACBrJSONObject);
var
  s: string;
begin
  {$IFDEF FPC}s := EmptyStr;{$ENDIF}
  aJSon.Value('status', s);

  fStatus := StringToPixPDVQrStatus(s);

  aJSon.Value('transacaoId', fTransacaoId);
  aJSon.Value('transacaoTipo', fTransacaoTipo);
  if (fStatus = pqsExpired) then
    aJSon.ValueISODateTime('expirou', fExpirou)
  else if (fStatus = pqsApproved) then
  begin
    aJSon.Value('mensagem', fMensagem);
    aJSon.Value('endToEndId', fEndToEndId);
    aJSon.Value('identificadorId', fIdentificadorId);;

    fSender.ReadFromJSon(aJSon);
  end;
end;

{ TPixPDVQrResumoItemArray }

function TPixPDVQrResumoItemArray.GetItem(Index: Integer): TPixPDVQrResumoItem;
begin
  Result := TPixPDVQrResumoItem(inherited Items[Index]);
end;

function TPixPDVQrResumoItemArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TPixPDVQrResumoItemArray.Add(AItem: TPixPDVQrResumoItem): Integer;
begin
  Result := inherited Add(AItem);
end;

function TPixPDVQrResumoItemArray.New: TPixPDVQrResumoItem;
begin
  Result := TPixPDVQrResumoItem.Create('');
  Self.Add(Result);
end;

{ TPixPDVQrResumo }

procedure TPixPDVQrResumo.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  fitens.WriteToJSon(aJSon);
end;

procedure TPixPDVQrResumo.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  fitens.ReadFromJSon(aJSon);
end;

constructor TPixPDVQrResumo.Create(const aObjectName: string);
begin
  inherited Create(aObjectName);
  fItens := TPixPDVQrResumoItemArray.Create('resumo');
  Clear;
end;

destructor TPixPDVQrResumo.Destroy;
begin
  fItens.Free;
  inherited Destroy;
end;

procedure TPixPDVQrResumo.Clear;
begin
  fItens.Clear;
end;

function TPixPDVQrResumo.IsEmpty: Boolean;
begin
  Result := fItens.IsEmpty;
end;

procedure TPixPDVQrResumo.Assign(aSource: TPixPDVQrResumo);
begin
  fItens.Assign(aSource.Itens);
end;

{ TPixPDVQrSimulaPagar }

constructor TPixPDVQrSimulaPagar.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
end;

destructor TPixPDVQrSimulaPagar.Destroy;
begin
  inherited Destroy;
end;

{ TPixPDVSaldo }

procedure TPixPDVSaldo.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('total', fTotal)
    .AddPair('disponivel', fDisponivel)
    .AddPair('bloqueado', fBloqueado);
end;

procedure TPixPDVSaldo.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('total', fTotal)
    .Value('disponivel', fDisponivel)
    .Value('bloqueado', fBloqueado);
end;

constructor TPixPDVSaldo.Create(const aObjectName: string);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TPixPDVSaldo.Clear;
begin
  fTotal := 0;
  fDisponivel := 0;
  fBloqueado := 0;
end;

procedure TPixPDVSaldo.Assign(aSource: TPixPDVSaldo);
begin
  fTotal := aSource.Total;
  fDisponivel := aSource.Disponivel;
  fBloqueado := aSource.Bloqueado;
end;

{ TPixPDVRetirada }

procedure TPixPDVRetirada.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('transacaoId', fTransacaoId)
    .AddPair('endToEndId', fEndToEndId)
    .AddPair('pspId', fPspId)
    .AddPair('pspNome', fPspNome)
    .AddPair('agencia', fAgencia)
    .AddPair('conta', fConta)
    .AddPair('contaNome', fContaNome)
    .AddPair('contaTipo', fContaTipo);
end;

procedure TPixPDVRetirada.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('transacaoId', fTransacaoId)
    .Value('endToEndId', fEndToEndId)
    .Value('pspId', fPspId)
    .Value('pspNome', fPspNome)
    .Value('agencia', fAgencia)
    .Value('conta', fConta)
    .Value('contaNome', fContaNome)
    .Value('contaTipo', fContaTipo);
end;

constructor TPixPDVRetirada.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  Clear;
end;

procedure TPixPDVRetirada.Clear;
begin
  fTransacaoId := EmptyStr;
  fEndToEndId := EmptyStr;
  fPspId := EmptyStr;
  fPspNome := EmptyStr;
  fAgencia := EmptyStr;
  fConta := EmptyStr;
  fContaNome := EmptyStr;
  fContaTipo := EmptyStr;
end;

function TPixPDVRetirada.IsEmpty: Boolean;
begin
  Result := EstaVazio(fTransacaoId) and
            EstaVazio(fEndToEndId) and
            EstaVazio(fPspId) and
            EstaVazio(fPspNome) and
            EstaVazio(fAgencia) and
            EstaVazio(fConta) and
            EstaVazio(fContaNome) and
            EstaVazio(fContaTipo);
end;

procedure TPixPDVRetirada.Assign(aSource: TPixPDVRetirada);
begin
  fTransacaoId := aSource.TransacaoId;
  fEndToEndId := aSource.EndToEndId;
  fPspId := aSource.PspId;
  fPspNome := aSource.PspNome;
  fAgencia := aSource.Agencia;
  fConta := aSource.Conta;
  fContaNome := aSource.ContaNome;
  fContaTipo := aSource.ContaTipo;
end;

{ TPixPDVExtratoItem }

constructor TPixPDVExtratoItem.Create(const ObjectName: string);
begin
  inherited Create(ObjectName);
  Clear;
end;

procedure TPixPDVExtratoItem.Clear;
begin
  fTransacaoId := EmptyStr;
  fTransacaoTipo := EmptyStr;
  fDescricao := EmptyStr;
  fData := 0;
  fValor := 0;
  fTipo := EmptyStr;
end;

function TPixPDVExtratoItem.IsEmpty: Boolean;
begin
  Result := (fTransacaoId = EmptyStr) and
            (fTransacaoTipo = EmptyStr) and
            (fDescricao = EmptyStr) and
            (fData = 0) and
            (fValor = 0) and
            (fTipo = EmptyStr);
end;

procedure TPixPDVExtratoItem.Assign(aSource: TPixPDVExtratoItem);
begin
  fTransacaoId := aSource.TransacaoId;
  fTransacaoTipo := aSource.TransacaoTipo;
  fDescricao := aSource.Descricao;
  fData := aSource.Data;
  fValor := aSource.Valor;
  fTipo := aSource.Tipo;
end;

procedure TPixPDVExtratoItem.AssignSchema(aSource: TACBrPIXSchema);
begin
  if (aSource is TPixPDVExtratoItem) then
    Assign(TPixPDVExtratoItem(aSource));
end;

procedure TPixPDVExtratoItem.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .AddPair('transacaoId', fTransacaoId)
    .AddPair('transacaoTipo', fTransacaoTipo)
    .AddPair('descricao', fDescricao)
    .AddPair('data', FormatDateBr(fData))
    .AddPair('valor', FormatFloatBr(fValor))
    .AddPair('tipo', fTipo);
end;

procedure TPixPDVExtratoItem.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  aJSon
    .Value('transacaoId', ftransacaoId)
    .Value('transacaoTipo', fTransacaoTipo)
    .Value('descricao', fDescricao)
    .ValueISODateTime('data', fData)
    .Value('valor', fValor)
    .Value('tipo', fTipo);
end;

{ TPixPDVExtratoItemArray }

function TPixPDVExtratoItemArray.GetItem(Index: Integer): TPixPDVExtratoItem;
begin
  Result := TPixPDVExtratoItem(inherited Items[Index]);
end;

function TPixPDVExtratoItemArray.NewSchema: TACBrPIXSchema;
begin
  Result := New;
end;

function TPixPDVExtratoItemArray.Add(AItem: TPixPDVExtratoItem): Integer;
begin
  Result := inherited Add(AItem);
end;

function TPixPDVExtratoItemArray.New: TPixPDVExtratoItem;
begin
  Result := TPixPDVExtratoItem.Create('');
  Self.Add(Result);
end;

{ TPixPDVExtrato }

procedure TPixPDVExtrato.DoWriteToJSon(aJSon: TACBrJSONObject);
begin
  fItens.WriteToJSon(aJSon);
end;

procedure TPixPDVExtrato.DoReadFromJSon(aJSon: TACBrJSONObject);
begin
  fItens.ReadFromJSon(aJSon);
end;

constructor TPixPDVExtrato.Create(const aObjectName: String);
begin
  inherited Create(aObjectName);
  fItens := TPixPDVExtratoItemArray.Create('extrato');
  Clear;
end;

destructor TPixPDVExtrato.Destroy;
begin
  fItens.Free;
  inherited Destroy;
end;

procedure TPixPDVExtrato.Clear;
begin
  fItens.Clear;
end;

function TPixPDVExtrato.IsEmpty: Boolean;
begin
  Result := fItens.IsEmpty;
end;

procedure TPixPDVExtrato.Assign(aSource: TPixPDVExtrato);
begin
  fItens.Assign(aSource.fitens);
end;

function PixPDVQrStatusToString(aStatus: TPixPDVQrStatusTipo): String;
begin
  case aStatus of
    pqsApproved: Result := 'APPROVED';
    pqsCreated: Result := 'CREATED';
    pqsRefunded: Result := 'REFUNDED';
    pqsRejected: Result := 'REJECTED';
    pqsCanceling: Result := 'CANCELING';
    pqsCanceled: Result := 'CANCELED';
    pqsPartial: Result := 'PARTIAL';
    pqsExpired: Result := 'EXPIRED';
    pqsTimeOut: Result := 'TIMEOUT';
    pqsOverFilled: Result := 'OVERFILLED';
    pqsUnfinished: Result := 'UNFINISHED';
    pqsError: Result := 'ERROR';
    pqsFatalError: Result := 'FATAL_ERROR';
    pqsAuthorized: Result := 'AUTHORIZED';
    pqsCaptured: Result := 'CAPTURED';
  else
    Result := EmptyStr;
  end;
end;

function StringToPixPDVQrStatus(const aString: String): TPixPDVQrStatusTipo;
var
  wStatus: String;
begin
  wStatus := UpperCase(Trim(aString));
  if (wStatus = 'APPROVED') then
    Result := pqsApproved
  else if (wStatus = 'CREATED') then
    Result := pqsCreated
  else if (wStatus = 'REFUNDED') then
    Result := pqsRefunded
  else if (wStatus = 'REJECTED') then
    Result := pqsRejected
  else if (wStatus = 'CANCELING') then
    Result := pqsCanceling
  else if (wStatus = 'CANCELED') then
    Result := pqsCanceled
  else if (wStatus = 'PARTIAL') then
    Result := pqsPartial
  else if (wStatus = 'EXPIRED') then
    Result := pqsExpired
  else if (wStatus = 'TIMEOUT') then
    Result := pqsTimeOut
  else if (wStatus = 'OVERFILLED') then
    Result := pqsOverFilled
  else if (wStatus = 'UNFINISHED') then
    Result := pqsUnfinished
  else if (wStatus = 'ERROR') then
    Result := pqsError
  else if (wStatus = 'FATAL_ERROR') then
    Result := pqsFatalError
  else if (wStatus = 'AUTHORIZED') then
    Result := pqsAuthorized
  else if (wStatus = 'CAPTURED') then
    Result := pqsCaptured
  else
    Result := pqsNone;
end;

end.

