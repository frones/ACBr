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

{$I ACBr.inc}

unit ACBrExtratoAPI;

interface

uses
  Classes, SysUtils, ACBrSocket, ACBrBase, ACBrAPIBase;

type   

  TACBrExtratoAPIAmbiente = (
    eamNenhum,
    eamHomologacao,
    eamProducao);

  TACBrExtratoAPIBancoConsulta = (
    bccNenhum,
    bccBancoDoBrasil,
    bccInter
  );

  TACBrExtratoAPITipoOperacao = (
    etoNenhum,
    etoDebito,
    etoCredito
  );

  TACBrExtratoAPI = class;

  { TACBrExtratoLancamento }

  TACBrExtratoLancamento = class
  private
    fpDataLancamento: TDateTime;
    fpDataMovimento: TDateTime;
    fpDescricao: String;
    fpInfoComplementar: String;
    fpNumeroDocumento: String;
    fpTipoOperacao: TACBrExtratoAPITipoOperacao;
    fpValor: Double;
  public
    procedure Clear;
    property NumeroDocumento: String read fpNumeroDocumento write fpNumeroDocumento;
    property DataLancamento: TDateTime read fpDataLancamento write fpDataLancamento;
    property DataMovimento: TDateTime read fpDataMovimento write fpDataMovimento;
    property TipoOperacao: TACBrExtratoAPITipoOperacao read fpTipoOperacao write fpTipoOperacao;
    property Descricao: String read fpDescricao write fpDescricao;
    property InfoComplementar: String read fpInfoComplementar write fpInfoComplementar;
    property Valor: Double read fpValor write fpValor;
  end;

  { TACBrExtratoLancamentos }

  TACBrExtratoLancamentos = class(TACBrObjectList)
  private
    function GetItem(aIndex: Integer): TACBrExtratoLancamento;
    procedure SetItem(aIndex: Integer; aValue: TACBrExtratoLancamento);
  public
    function Add(aLancamento: TACBrExtratoLancamento): Integer;
    procedure Insert(aIndex: Integer; aLancamento: TACBrExtratoLancamento);
    function New: TACBrExtratoLancamento;
    property Items[aIndex: Integer]: TACBrExtratoLancamento read GetItem write SetItem; default;
  end;

  { TACBrExtratoConsultado }

  TACBrExtratoConsultado = class(TACBrAPISchema)
  private
    function GetLancamentos: TACBrExtratoLancamentos;
  protected
    fpLancamentos: TACBrExtratoLancamentos;
    fpRegistrosPaginaAtual: Integer;
    fpTotalPaginas: Integer;
    fpTotalRegistros: Integer;
  public
    destructor Destroy; override;
    procedure Clear; override;

    property TotalPaginas: Integer read fpTotalPaginas;
    property TotalRegistros: Integer read fpTotalRegistros;
    property RegistrosPaginaAtual: Integer read fpRegistrosPaginaAtual;
    property Lancamentos: TACBrExtratoLancamentos read GetLancamentos;
  end;

  { TACBrExtratoErro }

  TACBrExtratoErro = class(TACBrAPISchema)
  protected
    fpcodigo: String;
    fpmensagem: String;
    fptitulo: String;
  public
    procedure Clear; override;
    property codigo: String read fpcodigo;
    property titulo: String read fptitulo;
    property mensagem: String read fpmensagem;
  end;

  { TACBrExtratoAPIBancoClass }

  TACBrExtratoAPIBancoClass = class(TACBrHTTP)
  private
    fClientID: String;
    fClientSecret: String;

    procedure VerificarAutenticacao;
    procedure VerificarValidadeToken;
  protected
    fpToken: String;
    fpAutenticado: Boolean;
    fpValidadeToken: TDateTime;
    fpOwner: TACBrExtratoAPI;
    fpRespostaErro: TACBrExtratoErro;
    fpExtratoConsultado: TACBrExtratoConsultado;

    procedure Autenticar; virtual;
    procedure RenovarToken; virtual;
    procedure PrepararHTTP; virtual;
    procedure DispararExcecao(E: Exception);

    function CalcularURL: String; virtual;
    function GetRespostaErro: TACBrExtratoErro; virtual;
    function GetExtratoConsultado: TACBrExtratoConsultado; virtual;
  public
    constructor Create(aOwner: TACBrExtratoAPI); overload;
    destructor Destroy; override;

    function ConsultarExtrato(
      const aAgencia, aConta: String;
      const aDataInicio: TDateTime = 0;
      const aDataFim: TDateTime = 0;
      const aPagina: Integer = 0;
      const aRegistrosPorPag: Integer = 0): Boolean; virtual;

    property ExtratoConsultado: TACBrExtratoConsultado read GetExtratoConsultado;
    property RespostaErro: TACBrExtratoErro read GetRespostaErro;
  published
    property ClientID: String read fClientID write fClientID;
    property ClientSecret: String read fClientSecret write fClientSecret;
  end;

  { TACBrExtratoAPI }
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(piacbrAllPlatforms)]
  {$ENDIF RTL230_UP}
  TACBrExtratoAPI = class(TACBrComponent)
  private
    fAmbiente: TACBrExtratoAPIAmbiente;
    fBanco: TACBrExtratoAPIBancoClass;
    fBancoConsulta: TACBrExtratoAPIBancoConsulta;
    fLogArquivo: String;
    fLogNivel: Byte;
    function GetBanco: TACBrExtratoAPIBancoClass;
    function GetExtratoConsultado: TACBrExtratoConsultado;
    procedure SetBancoConsulta(aValue: TACBrExtratoAPIBancoConsulta);
  public
    destructor Destroy; override;
    function ConsultarExtrato(
      const aAgencia, aConta: String;
      const aDataInicio: TDateTime = 0;
      const aDataFim: TDateTime = 0;
      const aPagina: Integer = 0;
      const aRegistrosPorPag: Integer = 0): Boolean; virtual;

    property ExtratoConsultado: TACBrExtratoConsultado read GetExtratoConsultado;
  published
    property Banco: TACBrExtratoAPIBancoClass read GetBanco;
    property Ambiente: TACBrExtratoAPIAmbiente read fAmbiente write fAmbiente default eamNenhum;
    property BancoConsulta: TACBrExtratoAPIBancoConsulta read fBancoConsulta write SetBancoConsulta
      default bccNenhum;

    property LogArquivo: String read fLogArquivo write fLogArquivo;
    property LogNivel: Byte read fLogNivel write fLogNivel default 1;
  end;

  function TipoLancamentoToString(const aTipo: TACBrExtratoAPITipoOperacao): String;
  function StringToTipoLancamento(const aStr: String): TACBrExtratoAPITipoOperacao;

implementation

uses
  ACBrExtratoAPIBB, ACBrExtratoAPIInter; 

function TipoLancamentoToString(const aTipo: TACBrExtratoAPITipoOperacao): String;
begin
  Result := EmptyStr;
  case aTipo of
    etoDebito: Result := 'D';
    etoCredito: Result := 'C';
  end;
end;

function StringToTipoLancamento(const aStr: String): TACBrExtratoAPITipoOperacao;
var
  s: String;
begin
  s := UpperCase(aStr);
  Result := etoNenhum;
  if (s = 'D') then
    Result := etoDebito
  else if (s = 'C') then
    Result := etoCredito;;
end;

{ TACBrExtratoErro }

procedure TACBrExtratoErro.Clear;
begin
  fpcodigo := EmptyStr;
  fpmensagem := EmptyStr;
  fptitulo := EmptyStr;
end;

{ TACBrExtratoLancamento }

procedure TACBrExtratoLancamento.Clear;
begin
  fpDataLancamento := 0;
  fpDataMovimento := 0;
  fpDescricao := EmptyStr;
  fpInfoComplementar := EmptyStr;
  fpNumeroDocumento := EmptyStr;
  fpTipoOperacao := etoNenhum;
  fpValor := 0;
end;

{ TACBrExtratoConsultado }

function TACBrExtratoConsultado.GetLancamentos: TACBrExtratoLancamentos;
begin
  if (not Assigned(fpLancamentos)) then
    fpLancamentos := TACBrExtratoLancamentos.Create;
  Result := fpLancamentos;
end;

destructor TACBrExtratoConsultado.Destroy;
begin
  if Assigned(fpLancamentos) then
    fpLancamentos.Free;
  inherited Destroy;
end;

procedure TACBrExtratoConsultado.Clear;
begin
  if Assigned(fpLancamentos) then
    fpLancamentos.Clear;
  fpTotalPaginas := 0;
  fpTotalRegistros := 0;
  fpRegistrosPaginaAtual := 0;
end;

{ TACBrExtratoLancamentos }

function TACBrExtratoLancamentos.GetItem(aIndex: Integer): TACBrExtratoLancamento;
begin
  Result := TACBrExtratoLancamento(inherited Items[aIndex]);
end;

procedure TACBrExtratoLancamentos.SetItem(aIndex: Integer; aValue: TACBrExtratoLancamento);
begin
  inherited Items[aIndex] := aValue;
end;

function TACBrExtratoLancamentos.Add(aLancamento: TACBrExtratoLancamento): Integer;
begin
  Result := inherited Add(aLancamento);
end;

procedure TACBrExtratoLancamentos.Insert(aIndex: Integer; aLancamento: TACBrExtratoLancamento);
begin
  inherited Insert(aIndex, aLancamento);
end;

function TACBrExtratoLancamentos.New: TACBrExtratoLancamento;
begin
  Result := TACBrExtratoLancamento.Create;
  Self.Add(Result);
end;

{ TACBrExtratoAPIBancoClass }

procedure TACBrExtratoAPIBancoClass.VerificarAutenticacao;
begin
  if (not fpAutenticado) then
  begin
    RegistrarLog('Autenticar', 3);
    Autenticar;
  end;

  VerificarValidadeToken;
end;

function TACBrExtratoAPIBancoClass.GetExtratoConsultado: TACBrExtratoConsultado;
begin
  if (not Assigned(fpExtratoConsultado)) then
    fpExtratoConsultado := TACBrExtratoConsultado.Create;
  Result := fpExtratoConsultado;
end;

function TACBrExtratoAPIBancoClass.GetRespostaErro: TACBrExtratoErro;
begin
  if (not Assigned(fpRespostaErro)) then
    fpRespostaErro := TACBrExtratoErro.Create;
  Result := fpRespostaErro;
end;

procedure TACBrExtratoAPIBancoClass.VerificarValidadeToken;
begin
  if (fpValidadeToken <> 0) and (fpValidadeToken < Now) then
  begin
    RegistrarLog('RenovarToken', 3);
    RenovarToken;
  end;
end;

procedure TACBrExtratoAPIBancoClass.Autenticar;
begin
  fpAutenticado := True;
end;

procedure TACBrExtratoAPIBancoClass.RenovarToken;
begin
  Autenticar;
end;

procedure TACBrExtratoAPIBancoClass.PrepararHTTP;
begin
  RegistrarLog('PrepararHTTP', 3);
  VerificarAutenticacao;
  LimparHTTP;
end;

procedure TACBrExtratoAPIBancoClass.DispararExcecao(E: Exception);
begin
  if (not Assigned(E)) then
    Exit;

  RegistrarLog(E.ClassName + ': ' + E.Message);
  raise E;
end;

function TACBrExtratoAPIBancoClass.CalcularURL: String;
begin
  { Sobreescrever na Classe do Banco }
  Result := EmptyStr;
end;

constructor TACBrExtratoAPIBancoClass.Create(aOwner: TACBrExtratoAPI);
begin
  inherited Create(aOwner);
  fpOwner := aOwner;
  fpToken := EmptyStr;
  fpAutenticado := False;
  fpValidadeToken := 0;
  fClientID := EmptyStr;
  fClientSecret := EmptyStr;
end;

destructor TACBrExtratoAPIBancoClass.Destroy;
begin
  if Assigned(fpRespostaErro) then
    fpRespostaErro.Free;
  if Assigned(fpExtratoConsultado) then
    fpExtratoConsultado.Free;
  inherited Destroy;
end;

function TACBrExtratoAPIBancoClass.ConsultarExtrato(const aAgencia, aConta: String;
  const aDataInicio: TDateTime; const aDataFim: TDateTime; const aPagina: Integer;
  const aRegistrosPorPag: Integer): Boolean;
begin
  { Sobreescrever na Classe do Banco }
  Result := False;
end;

{ TACBrExtratoAPI }

function TACBrExtratoAPI.GetBanco: TACBrExtratoAPIBancoClass;
begin
  if (not Assigned(fBanco)) then
    fBanco := TACBrExtratoAPIBancoClass.Create(Self);
  Result := fBanco;
end;

function TACBrExtratoAPI.GetExtratoConsultado: TACBrExtratoConsultado;
begin
  Result := fBanco.ExtratoConsultado;
end;

procedure TACBrExtratoAPI.SetBancoConsulta(aValue: TACBrExtratoAPIBancoConsulta);
begin
  if (fBancoConsulta = aValue) then
    Exit;

  if Assigned(fBanco) then
    fBanco.Free;
                           
  fBancoConsulta := aValue;
  case aValue of
    bccBancoDoBrasil: fBanco := TACBrExtratoAPIBB.Create(Self);
    bccInter: fBanco := TACBrExtratoAPIInter.Create(Self);
  else
    fBanco := TACBrExtratoAPIBancoClass.Create(Self);
  end;

  fBanco.ArqLOG := fLogArquivo;
  fBanco.NivelLog := fLogNivel;
end;

destructor TACBrExtratoAPI.Destroy;
begin
  if Assigned(fBanco) then
    fBanco.Free;
  inherited Destroy;
end;

function TACBrExtratoAPI.ConsultarExtrato(const aAgencia, aConta: String;
  const aDataInicio: TDateTime; const aDataFim: TDateTime;
  const aPagina: Integer; const aRegistrosPorPag: Integer): Boolean;
begin
  Result := fBanco.ConsultarExtrato(aAgencia, aConta, aDataInicio, aDataFim, aPagina, aRegistrosPorPag);
end;

end.
