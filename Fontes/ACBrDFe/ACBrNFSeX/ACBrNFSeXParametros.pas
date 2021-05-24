{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2020 Daniel Simoes de Almeida               }
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

unit ACBrNFSeXParametros;

interface

uses
  Classes, SysUtils, IniFiles,
  ACBrXmlBase, ACBrNFSeXConversao;

type

  { TConfigGeral }
  TConfigGeral = class
  private
    // define como é o atributo ID: "Id" ou "id", se for fazio o atributo não é gerado
    FIdentificador: string;
    // define o caracter ou caracteres a serem usados como quebra de linha
    FQuebradeLinha: string;
    // define se vai usar certificado digital ou não
    FUseCertificateHTTP: boolean;
    // define se vai usar autorização no cabeçalho ou não
    FUseAuthorizationHeader: boolean;
    // define o numero maximo de Rps a serem incluidos no GerarNfse
    FNumMaxRpsGerar: integer;
    // define o numero maximo de Rps a serem incluidos no EnviarLoteRpsEnvio e EnviarLoteRpsSincronoEnvio
    FNumMaxRpsEnviar: integer;
    // define o formato que o código do ItemListaServico deve ser gerado
    FFormItemLServico: TFormatoItemListaServico;
    // define se vai ser utilizado uma tabela externa de serviço ou não
    FTabServicosExt: Boolean;
    // define o modo de envio dos Rps para o webservice
    FModoEnvio: TmodoEnvio;
    // uso diverso
    FParams1: string;
    // uso diverso
    FParams2: string;

  public
    procedure LoadParams1(AINI: TCustomIniFile; ASession: string);
    procedure LoadParams2(AINI: TCustomIniFile; ASession: string);

    property Identificador: string read FIdentificador write FIdentificador;
    property QuebradeLinha: string read FQuebradeLinha write FQuebradeLinha;
    property UseCertificateHTTP: boolean read FUseCertificateHTTP write FUseCertificateHTTP;
    property UseAuthorizationHeader: boolean read FUseAuthorizationHeader write FUseAuthorizationHeader;
    property NumMaxRpsGerar: integer read FNumMaxRpsGerar write FNumMaxRpsGerar;
    property NumMaxRpsEnviar: integer read FNumMaxRpsEnviar write FNumMaxRpsEnviar;
    property FormatoItemListaServico: TFormatoItemListaServico read FFormItemLServico write FFormItemLServico;
    property TabServicosExt: Boolean read FTabServicosExt write FTabServicosExt;
    property ModoEnvio: TmodoEnvio read FModoEnvio write FModoEnvio;
    // Parametros lidos no arquivo .Res ou .ini
    property Params1: string read FParams1;
    property Params2: string read FParams2;

  end;

  { TWebserviceInfo }
  TWebserviceInfo = class
  private
    FLinkURL: string;
    FNameSpace: string;
    FXMLNameSpace: string;
    FRecepcionar: string;
    FConsultarLote: string;
    FConsultarNFSeRps: string;
    FConsultarSituacao: string;
    FConsultarNFSe: string;
    FConsultarNFSeURL: string;
    FConsultarNFSePorFaixa: string;
    FConsultarNFSeServicoPrestado: string;
    FConsultarNFSeServicoTomado: string;
    FCancelarNFSe: string;
    FGerarNFSe: string;
    FRecepcionarSincrono: string;
    FSubstituirNFSe: string;
    FAbrirSessao: string;
    FFecharSessao: string;
    FTesteEnvio: string;
    FSoapAction: string;

  public
    property LinkURL: string read FLinkURL;
    property NameSpace: string read FNameSpace;
    property XMLNameSpace: string read FXMLNameSpace;
    property Recepcionar: string read FRecepcionar;
    property ConsultarLote: string read FConsultarLote;
    property ConsultarNFSeRps: string read FConsultarNFSeRps;
    property ConsultarSituacao: string read FConsultarSituacao;
    property ConsultarNFSe: string read FConsultarNFSe;
    property ConsultarNFSeURL: string read FConsultarNFSeURL;
    property ConsultarNFSePorFaixa: string read FConsultarNFSePorFaixa;
    property ConsultarNFSeServicoPrestado: string read FConsultarNFSeServicoPrestado;
    property ConsultarNFSeServicoTomado: string read FConsultarNFSeServicoTomado;
    property CancelarNFSe: string read FCancelarNFSe;
    property GerarNFSe: string read FGerarNFSe;
    property RecepcionarSincrono: string read FRecepcionarSincrono;
    property SubstituirNFSe: string read FSubstituirNFSe;
    property AbrirSessao: string read FAbrirSessao;
    property FecharSessao: string read FFecharSessao;
    property TesteEnvio: string read FTesteEnvio;
    property SoapAction: string read FSoapAction;

  end;

  { TConfigWebServices }
  TConfigWebServices = class
  private
    FVersaoDados: string;
    FVersaoAtrib: string;
    FAtribVerLote: string;
    FProducao: TWebserviceInfo;
    FHomologacao: TWebserviceInfo;

  public
    constructor Create;
    destructor Destroy; override;

    procedure LoadUrl(AINI: TCustomIniFile; ASession: string);

    property VersaoDados: string read FVersaoDados write FVersaoDados;
    property VersaoAtrib: string read FVersaoAtrib write FVersaoAtrib;
    property AtribVerLote: string read FAtribVerLote write FAtribVerLote;
    property Producao: TWebserviceInfo read FProducao;
    property Homologacao: TWebserviceInfo read FHomologacao;

  end;

  { TDocElement }
  TDocElement = class
  private
    Fxmlns: string;
    FInfElemento: string;
    FDocElemento: string;

  public
    // contem o namespace a ser incluido no XML
    property xmlns: string read Fxmlns write Fxmlns;
    // nome do elemento a ser utilizado na assinatura digital que contem o atributo ID
    property InfElemento: string read FInfElemento write FInfElemento;
    // nome do elemento do documento a ser assinado
    property DocElemento: string read FDocElemento write FDocElemento;
  end;

  { TConfigMsgDados }
  TConfigMsgDados = class
  private
    FPrefixo: String;
    FPrefixoTS: String;
    FDadosCabecalho: String;

    FXmlRps: TDocElement;
    FLoteRps: TDocElement;
    FLoteRpsSincrono: TDocElement;
    FConsultarSituacao: TDocElement;
    FConsultarLote: TDocElement;
    FConsultarNFSeRps: TDocElement;
    FConsultarNFSe: TDocElement;
    FConsultarNFSePorFaixa: TDocElement;
    FConsultarNFSeServicoPrestado: TDocElement;
    FConsultarNFSeServicoTomado: TDocElement;
    FConsultarNFSeURL: TDocElement;
    FCancelarNFSe: TDocElement;
    FGerarNFSe: TDocElement;
    FSubstituirNFSe: TDocElement;
    FAbrirSessao: TDocElement;
    FFecharSessao: TDocElement;

    FGerarNSLoteRps: Boolean;
    FGerarPrestadorLoteRps: Boolean;

  public
    constructor Create;
    destructor Destroy; override;

    property Prefixo: String read FPrefixo write FPrefixo;
    property PrefixoTS: String read FPrefixoTS write FPrefixoTS;
    property DadosCabecalho: String read FDadosCabecalho write FDadosCabecalho;

    property XmlRps: TDocElement read FXmlRps;
    property LoteRps: TDocElement read FLoteRps;
    property LoteRpsSincrono: TDocElement read FLoteRpsSincrono;
    property ConsultarSituacao: TDocElement read FConsultarSituacao;
    property ConsultarLote: TDocElement read FConsultarLote;
    property ConsultarNFSeRps: TDocElement read FConsultarNFSeRps;
    property ConsultarNFSe: TDocElement read FConsultarNFSe;
    property ConsultarNFSePorFaixa: TDocElement read FConsultarNFSePorFaixa;
    property ConsultarNFSeServicoPrestado: TDocElement read FConsultarNFSeServicoPrestado;
    property ConsultarNFSeServicoTomado: TDocElement read FConsultarNFSeServicoTomado;
    property ConsultarNFSeURL: TDocElement read FConsultarNFSeURL;
    property CancelarNFSe: TDocElement read FCancelarNFSe;
    property GerarNFSe: TDocElement read FGerarNFSe;
    property SubstituirNFSe: TDocElement read FSubstituirNFSe;
    property AbrirSessao: TDocElement read FAbrirSessao;
    property FecharSessao: TDocElement read FFecharSessao;

    property GerarNSLoteRps: Boolean read FGerarNSLoteRps write FGerarNSLoteRps;
    property GerarPrestadorLoteRps: Boolean read FGerarPrestadorLoteRps write FGerarPrestadorLoteRps;
  end;

  { TConfigAssinar }
  TConfigAssinar = class
  private
    FRps: boolean;
    FLoteRps: boolean;
    FConsultarSituacao: boolean;
    FConsultarLote: boolean;
    FConsultarNFSeRps: boolean;
    FConsultarNFSe: boolean;
    FConsultarNFSePorFaixa: boolean;
    FConsultarNFSeServicoPrestado: boolean;
    FConsultarNFSeServicoTomado: boolean;
    FCancelarNFSe: boolean;
    FRpsGerarNFSe: boolean;
    FLoteGerarNFSe: boolean;
    FRpsSubstituirNFSe: boolean;
    FSubstituirNFSe: boolean;
    FAbrirSessao: boolean;
    FFecharSessao: boolean;
    FIncluirURI: boolean;
    FAssinaturaAdicional: boolean;

  public
    property Rps: boolean read FRps write FRps;
    property LoteRps: boolean read FLoteRps write FLoteRps;
    property ConsultarSituacao: boolean read FConsultarSituacao write FConsultarSituacao;
    property ConsultarLote: boolean read FConsultarLote write FConsultarLote;
    property ConsultarNFSeRps: boolean read FConsultarNFSeRps write FConsultarNFSeRps;
    property ConsultarNFSe: boolean read FConsultarNFSe write FConsultarNFSe;
    property ConsultarNFSePorFaixa: boolean read FConsultarNFSePorFaixa write FConsultarNFSePorFaixa;
    property ConsultarNFSeServicoPrestado: boolean read FConsultarNFSeServicoPrestado write FConsultarNFSeServicoPrestado;
    property ConsultarNFSeServicoTomado: boolean read FConsultarNFSeServicoTomado write FConsultarNFSeServicoTomado;
    property CancelarNFSe: boolean read FCancelarNFSe write FCancelarNFSe;
    property RpsGerarNFSe: boolean read FRpsGerarNFSe write FRpsGerarNFSe;
    property LoteGerarNFSe: boolean read FLoteGerarNFSe write FLoteGerarNFSe;
    property RpsSubstituirNFSe: boolean read FRpsSubstituirNFSe write FRpsSubstituirNFSe;
    property SubstituirNFSe: boolean read FSubstituirNFSe write FSubstituirNFSe;
    property AbrirSessao: boolean read FAbrirSessao write FAbrirSessao;
    property FecharSessao: boolean read FFecharSessao write FFecharSessao;
    property IncluirURI: boolean read FIncluirURI write FIncluirURI;
    property AssinaturaAdicional: boolean read FAssinaturaAdicional write FAssinaturaAdicional;

  end;

  { TConfigSchemas }
  TConfigSchemas = class
  private
    FRecepcionar: string;
    FConsultarSituacao: string;
    FConsultarLote: string;
    FConsultarNFSeRps: string;
    FConsultarNFSe: string;
    FConsultarNFSeURL: string;
    FConsultarNFSePorFaixa: string;
    FConsultarNFSeServicoPrestado: string;
    FConsultarNFSeServicoTomado: string;
    FCancelarNFSe: string;
    FGerarNFSe: string;
    FRecepcionarSincrono: string;
    FSubstituirNFSe: string;
    FAbrirSessao: string;
    FFecharSessao: string;
    FTeste: string;
    FValidar: boolean;

  public
    property Recepcionar: string read FRecepcionar write FRecepcionar;
    property ConsultarSituacao: string read FConsultarSituacao write FConsultarSituacao;
    property ConsultarLote: string read FConsultarLote write FConsultarLote;
    property ConsultarNFSeRps: string read FConsultarNFSeRps write FConsultarNFSeRps;
    property ConsultarNFSe: string read FConsultarNFSe write FConsultarNFSe;
    property ConsultarNFSeURL: string read FConsultarNFSeURL write FConsultarNFSeURL;
    property ConsultarNFSePorFaixa: string read FConsultarNFSePorFaixa write FConsultarNFSePorFaixa;
    property ConsultarNFSeServicoPrestado: string read FConsultarNFSeServicoPrestado write FConsultarNFSeServicoPrestado;
    property ConsultarNFSeServicoTomado: string read FConsultarNFSeServicoTomado write FConsultarNFSeServicoTomado;
    property CancelarNFSe: string read FCancelarNFSe write FCancelarNFSe;
    property GerarNFSe: string read FGerarNFSe write FGerarNFSe;
    property RecepcionarSincrono: string read FRecepcionarSincrono write FRecepcionarSincrono;
    property SubstituirNFSe: string read FSubstituirNFSe write FSubstituirNFSe;
    property AbrirSessao: string read FAbrirSessao write FAbrirSessao;
    property FecharSessao: string read FFecharSessao write FFecharSessao;
    property Teste: string read FTeste write FTeste;
    property Validar: boolean read FValidar write FValidar;

  end;

implementation

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

procedure TConfigWebServices.LoadUrl(AINI: TCustomIniFile; ASession: string);
begin
  with Producao do
  begin
    FRecepcionar         := AINI.ReadString(ASession, 'ProRecepcionar'        , '');
    FConsultarSituacao   := AINI.ReadString(ASession, 'ProConsultarSituacao'  , FRecepcionar);
    FConsultarLote       := AINI.ReadString(ASession, 'ProConsultarLote'      , FRecepcionar);
    FConsultarNFSeRPS    := AINI.ReadString(ASession, 'ProConsultarNFSeRps'   , FRecepcionar);
    FConsultarNFSe       := AINI.ReadString(ASession, 'ProConsultarNFSe'      , FRecepcionar);
    FConsultarNFSeURL    := AINI.ReadString(ASession, 'ProConsultarNFSeURL'   , FRecepcionar);
    FCancelarNFSe        := AINI.ReadString(ASession, 'ProCancelarNFSe'       , FRecepcionar);
    FGerarNFSe           := AINI.ReadString(ASession, 'ProGerarNFSe'          , FRecepcionar);
    FRecepcionarSincrono := AINI.ReadString(ASession, 'ProRecepcionarSincrono', FRecepcionar);
    FSubstituirNFSe      := AINI.ReadString(ASession, 'ProSubstituirNFSe'     , FRecepcionar);
    FAbrirSessao         := AINI.ReadString(ASession, 'ProAbrirSessao'        , FRecepcionar);
    FFecharSessao        := AINI.ReadString(ASession, 'ProFecharSessao'       , FRecepcionar);

    FConsultarNFSePorFaixa        := AINI.ReadString(ASession, 'ProConsultarNFSePorFaixa'       , FRecepcionar);
    FConsultarNFSeServicoPrestado := AINI.ReadString(ASession, 'ProConsultarNFSeServicoPrestado', FRecepcionar);
    FConsultarNFSeServicoTomado   := AINI.ReadString(ASession, 'ProConsultarNFSeServicoTomado'  , FRecepcionar);

    FXMLNameSpace := AINI.ReadString(ASession, 'ProXMLNameSpace', '');

    FNameSpace := AINI.ReadString(ASession, 'ProNameSpace', '');

    FSoapAction := AINI.ReadString(ASession, 'ProSoapAction', '');

    FLinkURL := AINI.ReadString(ASession, 'ProLinkURL', '');
  end;

  with Homologacao do
  begin
    FRecepcionar         := AINI.ReadString(ASession, 'HomRecepcionar'        , '');
    FConsultarSituacao   := AINI.ReadString(ASession, 'HomConsultarSituacao'  , FRecepcionar);
    FConsultarLote       := AINI.ReadString(ASession, 'HomConsultarLote'      , FRecepcionar);
    FConsultarNFSeRPS    := AINI.ReadString(ASession, 'HomConsultarNFSeRps'   , FRecepcionar);
    FConsultarNFSe       := AINI.ReadString(ASession, 'HomConsultarNFSe'      , FRecepcionar);
    FConsultarNFSeURL    := AINI.ReadString(ASession, 'HomConsultarNFSeURL'   , FRecepcionar);
    FCancelarNFSe        := AINI.ReadString(ASession, 'HomCancelarNFSe'       , FRecepcionar);
    FGerarNFSe           := AINI.ReadString(ASession, 'HomGerarNFSe'          , FRecepcionar);
    FRecepcionarSincrono := AINI.ReadString(ASession, 'HomRecepcionarSincrono', FRecepcionar);
    FSubstituirNFSe      := AINI.ReadString(ASession, 'HomSubstituirNFSe'     , FRecepcionar);
    FAbrirSessao         := AINI.ReadString(ASession, 'HomAbrirSessao'        , FRecepcionar);
    FFecharSessao        := AINI.ReadString(ASession, 'HomFecharSessao'       , FRecepcionar);

    FConsultarNFSePorFaixa        := AINI.ReadString(ASession, 'HomConsultarNFSePorFaixa'       , FRecepcionar);
    FConsultarNFSeServicoPrestado := AINI.ReadString(ASession, 'HomConsultarNFSeServicoPrestado', FRecepcionar);
    FConsultarNFSeServicoTomado   := AINI.ReadString(ASession, 'HomConsultarNFSeServicoTomado'  , FRecepcionar);

    FXMLNameSpace := AINI.ReadString(ASession, 'HomXMLNameSpace', '');

    FNameSpace := AINI.ReadString(ASession, 'HomNameSpace', '');

    FSoapAction := AINI.ReadString(ASession, 'HomSoapAction', '');

    FLinkURL := AINI.ReadString(ASession, 'HomLinkURL', '');

    if FRecepcionar = '' then
    begin
      FRecepcionar         := Producao.Recepcionar;
      FConsultarSituacao   := Producao.ConsultarSituacao;
      FConsultarLote       := Producao.ConsultarLote;
      FConsultarNFSeRps    := Producao.ConsultarNFSeRps;
      FConsultarNFSe       := Producao.ConsultarNFSe;
      FConsultarNFSeURL    := Producao.ConsultarNFSeURL;
      FCancelarNFSe        := Producao.CancelarNFSe;
      FGerarNFSe           := Producao.GerarNFSe;
      FRecepcionarSincrono := Producao.RecepcionarSincrono;
      FSubstituirNFSe      := Producao.SubstituirNFSe;
      FAbrirSessao         := Producao.AbrirSessao;
      FFecharSessao        := Producao.FecharSessao;

      FConsultarNFSePorFaixa        := Producao.ConsultarNFSePorFaixa;
      FConsultarNFSeServicoPrestado := Producao.ConsultarNFSeServicoPrestado;
      FConsultarNFSeServicoTomado   := Producao.ConsultarNFSeServicoTomado;
    end;

    if FXMLNameSpace = '' then
      FXMLNameSpace := Producao.XMLNameSpace;

    if FNameSpace = '' then
      FNameSpace := Producao.NameSpace;

    if FSoapAction = '' then
      FSoapAction := Producao.SoapAction;

    if FLinkURL = '' then
      FLinkURL := Producao.LinkURL;
  end;
end;

{ TConfigMsgDados }

constructor TConfigMsgDados.Create;
begin
  FXmlRps := TDocElement.Create;
  FLoteRps := TDocElement.Create;
  FLoteRpsSincrono := TDocElement.Create;
  FConsultarSituacao := TDocElement.Create;
  FConsultarLote := TDocElement.Create;
  FConsultarNFSeRps := TDocElement.Create;
  FConsultarNFSe := TDocElement.Create;
  FConsultarNFSePorFaixa := TDocElement.Create;
  FConsultarNFSeServicoPrestado := TDocElement.Create;
  FConsultarNFSeServicoTomado := TDocElement.Create;
  FConsultarNFSeURL := TDocElement.Create;
  FCancelarNFSe := TDocElement.Create;
  FGerarNFSe := TDocElement.Create;
  FSubstituirNFSe := TDocElement.Create;
  FAbrirSessao := TDocElement.Create;
  FFecharSessao := TDocElement.Create;
end;

destructor TConfigMsgDados.Destroy;
begin
  FXmlRps.Free;
  FLoteRps.Free;
  FLoteRpsSincrono.Free;
  FConsultarSituacao.Free;
  FConsultarLote.Free;
  FConsultarNFSeRps.Free;
  FConsultarNFSe.Free;
  FConsultarNFSePorFaixa.Free;
  FConsultarNFSeServicoPrestado.Free;
  FConsultarNFSeServicoTomado.Free;
  FConsultarNFSeURL.Free;
  FCancelarNFSe.Free;
  FGerarNFSe.Free;
  FSubstituirNFSe.Free;
  FAbrirSessao.Free;
  FFecharSessao.Free;

  inherited Destroy;
end;

{ TConfigGeral }

procedure TConfigGeral.LoadParams1(AINI: TCustomIniFile; ASession: string);
begin
  FParams1 := AINI.ReadString(ASession, 'Params1', '');
end;

procedure TConfigGeral.LoadParams2(AINI: TCustomIniFile; ASession: string);
begin
  FParams2 := AINI.ReadString(ASession, 'Params2', '');
end;

end.
