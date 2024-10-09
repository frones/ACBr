{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
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

unit ACBrANeParametros;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrXmlBase, ACBrANe.Conversao;

type

  { TACBrANeConfigParams }

  TACBrANeConfigParams = Class
  private
    fSL: TStringList;
    fParamsStr: string;

    procedure SetParamsStr(AValue: string);
  public
    constructor Create;
    destructor Destroy; override;

    function TemParametro(const AParam: string): Boolean;
    function ValorParametro(const AParam: string): string;
    function ParamTemValor(const AParam, AValor: string): Boolean;

    property AsString: string read fParamsStr write SetParamsStr;
  end;

  { TConfigGeral }
  TConfigGeral = class
  private
    // define como é o atributo ID: "Id" ou "id", se for fazio o atributo não é gerado
    FIdentificador: string;
    // define se vai usar certificado digital ou não
    FUseCertificateHTTP: boolean;
    // define se vai usar autorização no cabeçalho ou não
    FUseAuthorizationHeader: boolean;

    // uso diverso
    FParams: TACBrANeConfigParams;

    FSeguradora: TSeguradora;
    // Versão lido do arquivo ACBrANeServicos
    FVersao: TVersaoANe;
    // Ambiente setando na configuração do componente
    FAmbiente: TACBrTipoAmbiente;

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadParams(AINI: TCustomIniFile; const ASession: string);

    property Identificador: string read FIdentificador write FIdentificador;
    property UseCertificateHTTP: boolean read FUseCertificateHTTP write FUseCertificateHTTP;
    property UseAuthorizationHeader: boolean read FUseAuthorizationHeader write FUseAuthorizationHeader;

    // Parametros lidos no arquivo .Res ou .ini
    property Params: TACBrANeConfigParams read FParams;

    property Seguradora: TSeguradora read FSeguradora write FSeguradora;
    property Versao: TVersaoANe read FVersao write FVersao;
    property Ambiente: TACBrTipoAmbiente read FAmbiente write FAmbiente;
  end;

  { TWebserviceInfo }
  TWebserviceInfo = class
  private
    FLinkURL: string;
    FNameSpace: string;
    FXMLNameSpace: string;
    FSoapAction: string;

    FEnviar: string;
    FConsultar: string;
  public
    property LinkURL: string read FLinkURL;
    property NameSpace: string read FNameSpace;
    property XMLNameSpace: string read FXMLNameSpace;
    property SoapAction: string read FSoapAction;

    property Enviar: string read FEnviar;
    property Consultar: string read FConsultar;
  end;

  { TConfigWebServices }
  TConfigWebServices = class
  private
    // Versão dos dados informado na tag <VersaoDados>
    FVersaoDados: string;
    // Versão informada no atributo versao=
    FVersaoAtrib: string;
    // Grafia do atributo usado como versão no Lote de Rps
    FAtribVerLote: string;
    // Grupo de URLs do Ambiente de Produção
    FProducao: TWebserviceInfo;
    // Grupo de URLs do Ambiente de Homologação
    FHomologacao: TWebserviceInfo;

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadUrlProducao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadUrlHomologacao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadLinkUrlProducao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadLinkUrlHomologacao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadXMLNameSpaceProducao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadXMLNameSpaceHomologacao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadNameSpaceProducao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadNameSpaceHomologacao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadSoapActionProducao(AINI: TCustomIniFile; const ASession: string);
    procedure LoadSoapActionHomologacao(AINI: TCustomIniFile; const ASession: string);

    property VersaoDados: string read FVersaoDados write FVersaoDados;
    property VersaoAtrib: string read FVersaoAtrib write FVersaoAtrib;
    property AtribVerLote: string read FAtribVerLote write FAtribVerLote;
    property Producao: TWebserviceInfo read FProducao;
    property Homologacao: TWebserviceInfo read FHomologacao;

  end;

  { TDocElement }
  TDocElement = class
  private
    // contem o namespace a ser incluido no XML
    Fxmlns: string;
    // nome do elemento a ser utilizado na assinatura digital que contem o atributo ID
    FInfElemento: string;
    // nome do elemento do documento a ser assinado
    FDocElemento: string;
  public
    property xmlns: string read Fxmlns write Fxmlns;
    property InfElemento: string read FInfElemento write FInfElemento;
    property DocElemento: string read FDocElemento write FDocElemento;
  end;

  { TConfigMsgDados }
  TConfigMsgDados = class
  private
    FDadosCabecalho: string;

    FEnviar: TDocElement;
    FConsultar: TDocElement;
  public
    constructor Create;
    destructor Destroy; override;

    property DadosCabecalho: string read FDadosCabecalho write FDadosCabecalho;

    property Enviar: TDocElement read FEnviar;
    property Consultar: TDocElement read FConsultar;
  end;

  { TConfigAssinar }
  TConfigAssinar = class
  private
    FEnviar: boolean;
    FConsultar: boolean;
  public
    property Enviar: boolean read FEnviar write FEnviar;
    property Consultar: boolean read FConsultar write FConsultar;
  end;

  { TConfigSchemas }
  TConfigSchemas = class
  private
    FEnviar: string;
    FConsultar: string;

    // Se True realiza a validação do XML com os Schemas
    FValidar: boolean;
  public
    property Enviar: string read FEnviar write FEnviar;
    property Consultar: string read FConsultar write FConsultar;

    property Validar: boolean read FValidar write FValidar;
  end;

implementation

uses
  ACBrUtil.Strings;

{ TACBrANeConfigParams }

constructor TACBrANeConfigParams.Create;
begin
  inherited Create;

  fSL := TStringList.Create;
end;

destructor TACBrANeConfigParams.Destroy;
begin
  fSL.Free;

  inherited Destroy;
end;

function TACBrANeConfigParams.ParamTemValor(const AParam,
  AValor: string): Boolean;
begin
  Result := (Pos(lowercase(AValor), lowercase(ValorParametro(AParam))) > 0);
end;

function TACBrANeConfigParams.TemParametro(const AParam: string): Boolean;
var
  p: Integer;
begin
  p := fSL.IndexOfName(Trim(AParam));
  Result := (p >= 0);
end;

function TACBrANeConfigParams.ValorParametro(const AParam: string): string;
begin
  Result := fSL.Values[AParam];
end;

procedure TACBrANeConfigParams.SetParamsStr(AValue: string);
var
  s: string;
begin
  if fParamsStr = AValue then Exit;
  fParamsStr := Trim(AValue);
  s := StringReplace(fParamsStr, ':', '=', [rfReplaceAll]);
  AddDelimitedTextToList(s, '|', fSL, #0);
end;

{ TConfigWebServices }

constructor TConfigWebServices.Create;
begin
  FProducao := TWebserviceInfo.Create;
  FHomologacao := TWebserviceInfo.Create;
end;

destructor TConfigWebServices.Destroy;
begin
  FProducao.Free;
  FHomologacao.Free;

  inherited Destroy;
end;

procedure TConfigWebServices.LoadLinkUrlHomologacao(AINI: TCustomIniFile;
  const ASession: string);
begin
  Homologacao.FLinkURL := AINI.ReadString(ASession, 'HomLinkURL', '');
end;

procedure TConfigWebServices.LoadLinkUrlProducao(AINI: TCustomIniFile;
  const ASession: string);
begin
  Producao.FLinkURL := AINI.ReadString(ASession, 'ProLinkURL', '');
end;

procedure TConfigWebServices.LoadNameSpaceHomologacao(AINI: TCustomIniFile;
  const ASession: string);
begin
  Homologacao.FNameSpace := AINI.ReadString(ASession, 'HomNameSpace', '');
end;

procedure TConfigWebServices.LoadNameSpaceProducao(AINI: TCustomIniFile;
  const ASession: string);
begin
  Producao.FNameSpace := AINI.ReadString(ASession, 'ProNameSpace', '');
end;

procedure TConfigWebServices.LoadXMLNameSpaceHomologacao(
  AINI: TCustomIniFile; const ASession: string);
begin
  Homologacao.FXMLNameSpace := AINI.ReadString(ASession, 'HomXMLNameSpace', '');
end;

procedure TConfigWebServices.LoadXMLNameSpaceProducao(AINI: TCustomIniFile;
  const ASession: string);
begin
  Producao.FXMLNameSpace := AINI.ReadString(ASession, 'ProXMLNameSpace', '');
end;

procedure TConfigWebServices.LoadSoapActionHomologacao(AINI: TCustomIniFile;
  const ASession: string);
begin
  Homologacao.FSoapAction := AINI.ReadString(ASession, 'HomSoapAction', '');
end;

procedure TConfigWebServices.LoadSoapActionProducao(AINI: TCustomIniFile;
  const ASession: string);
begin
  Producao.FSoapAction := AINI.ReadString(ASession, 'ProSoapAction', '');
end;

procedure TConfigWebServices.LoadUrlHomologacao(AINI: TCustomIniFile;
  const ASession: string);
begin
  with Homologacao do
  begin
    FEnviar    := AINI.ReadString(ASession, 'HomEnviar'   , '');
    FConsultar := AINI.ReadString(ASession, 'HomConsultar', FEnviar);
  end;
end;

procedure TConfigWebServices.LoadUrlProducao(AINI: TCustomIniFile;
  const ASession: string);
begin
  with Producao do
  begin
    FEnviar    := AINI.ReadString(ASession, 'ProEnviar'   , '');
    FConsultar := AINI.ReadString(ASession, 'ProConsultar', FEnviar);
  end;
end;

{ TConfigMsgDados }

constructor TConfigMsgDados.Create;
begin
  FEnviar := TDocElement.Create;
  FConsultar := TDocElement.Create;
end;

destructor TConfigMsgDados.Destroy;
begin
  FEnviar.Free;
  FConsultar.Free;

  inherited Destroy;
end;

{ TConfigGeral }

constructor TConfigGeral.Create;
begin
  inherited Create;

  FParams := TACBrANeConfigParams.Create;
end;

destructor TConfigGeral.Destroy;
begin
  FParams.Free;

  inherited Destroy;
end;

procedure TConfigGeral.LoadParams(AINI: TCustomIniFile; const ASession: string);
begin
  FParams.AsString := AINI.ReadString(ASession, 'Params', '');
end;

end.
