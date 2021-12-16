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

unit ACBrNFSeXProviderBase;

interface

uses
  SysUtils, Classes, IniFiles, ACBrUtil, ACBrDFe,
  ACBrXmlDocument,
  ACBrNFSeXParametros, ACBrNFSeXInterface, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXLerXml, ACBrNFSeXGravarXml, ACBrNFSeXNotasFiscais,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXProvider = class(TInterfacedObject, IACBrNFSeXProvider)
  private
    FConfigGeral: TConfigGeral;
    FConfigWebServices: TConfigWebServices;
    FConfigMsgDados: TConfigMsgDados;
    FConfigAssinar: TConfigAssinar;
    FConfigSchemas: TConfigSchemas;

    function GetConfigGeral: TConfigGeral;
    function GetConfigWebServices: TConfigWebServices;
    function GetConfigMsgDados: TConfigMsgDados;
    function GetConfigAssinar: TConfigAssinar;
    function GetConfigSchemas: TConfigSchemas;

    function GetGerarResponse: TNFSeEmiteResponse;
    function GetEmiteResponse: TNFSeEmiteResponse;
    function GetConsultaSituacaoResponse: TNFSeConsultaSituacaoResponse;
    function GetConsultaLoteRpsResponse: TNFSeConsultaLoteRpsResponse;
    function GetConsultaNFSeporRpsResponse: TNFSeConsultaNFSeporRpsResponse;
    function GetConsultaNFSeResponse: TNFSeConsultaNFSeResponse;
    function GetCancelaNFSeResponse: TNFSeCancelaNFSeResponse;
    function GetSubstituiNFSeResponse: TNFSeSubstituiNFSeResponse;

  protected
    FAOwner: TACBrDFe;
    PrefixoTS: string;

    procedure Configuracao; virtual;
    procedure CarregarURL; virtual;
    procedure SetNomeXSD(const aNome: string);
    procedure SetXmlNameSpace(const aNameSpace: string);
    procedure SalvarXmlRps(aNota: NotaFiscal);
    procedure SalvarXmlNfse(aNota: NotaFiscal);

    function GetWebServiceURL(const AMetodo: TMetodo): string;
    function GetSchemaPath: string; virtual;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; virtual; abstract;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; virtual; abstract;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; virtual; abstract;
    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); virtual;

    function PrepararRpsParaLote(const aXml: string): string; virtual;
    function GetCpfCnpj(const CpfCnpj: string; const Prefixo: string = ''): string;
    function GetInscMunic(const InscMunic: string; const Prefixo: string = ''): string;
    function GetCabecalho(const Xmlns: string = ''): string;
    function DefinirIDLote(const ID: string): string; virtual;
    function DefinirIDCancelamento(const CNPJ: string; const InscMunic: string;
                                   const NumNfse: string): string; virtual;

    //metodos para geração e tratamento dos dados do metodo emitir
    procedure PrepararEmitir(Response: TNFSeEmiteResponse); virtual; abstract;
    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarEmitir(Response: TNFSeEmiteResponse); virtual;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultaSituacao
    procedure PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); virtual; abstract;
    procedure GerarMsgDadosConsultaSituacao(Response: TNFSeConsultaSituacaoResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); virtual;
    procedure TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultaLoteRps
    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); virtual; abstract;
    procedure GerarMsgDadosConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); virtual;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultaNFSeporRps
    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); virtual; abstract;
    procedure GerarMsgDadosConsultaporRps(Response: TNFSeConsultaNFSeporRpsResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); virtual;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultaNFSe
    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); virtual; abstract;
    procedure GerarMsgDadosConsultaNFSe(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarConsultaNFSe(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo CancelaNFSe
    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); virtual; abstract;
    procedure GerarMsgDadosCancelaNFSe(Response: TNFSeCancelaNFSeResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarCancelaNFSe(Response: TNFSeCancelaNFSeResponse); virtual;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo CancelaNFSe
    procedure PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); virtual; abstract;
    procedure GerarMsgDadosSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse;
      Params: TNFSeParamsResponse); virtual; abstract;
    procedure AssinarSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); virtual;
    procedure TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); virtual; abstract;

  public
    constructor Create(AOwner: TACBrDFe);
    destructor Destroy; override;

    function GerarXml(const aNFSe: TNFSe; var aXml, aAlerts: string): Boolean; virtual;
    function LerXML(const aXML: String; var aNFSe: TNFSe): Boolean; virtual;

    procedure GeraLote; virtual;
    procedure Emite; virtual;
    procedure ConsultaSituacao; virtual;
    procedure ConsultaLoteRps; virtual;
    procedure ConsultaNFSeporRps; virtual;
    procedure ConsultaNFSe; virtual;
    procedure CancelaNFSe; virtual;
    procedure SubstituiNFSe; virtual;

    property ConfigGeral: TConfigGeral read GetConfigGeral;
    property ConfigWebServices: TConfigWebServices read GetConfigWebServices;
    property ConfigMsgDados: TConfigMsgDados read GetConfigMsgDados;
    property ConfigAssinar: TConfigAssinar read GetConfigAssinar;
    property ConfigSchemas: TConfigSchemas read GetConfigSchemas;

    property GerarResponse: TNFSeEmiteResponse read GetGerarResponse;
    property EmiteResponse: TNFSeEmiteResponse read GetEmiteResponse;
    property ConsultaSituacaoResponse: TNFSeConsultaSituacaoResponse read GetConsultaSituacaoResponse;
    property ConsultaLoteRpsResponse: TNFSeConsultaLoteRpsResponse read GetConsultaLoteRpsResponse;
    property ConsultaNFSeporRpsResponse: TNFSeConsultaNFSeporRpsResponse read GetConsultaNFSeporRpsResponse;
    property ConsultaNFSeResponse: TNFSeConsultaNFSeResponse read GetConsultaNFSeResponse;
    property CancelaNFSeResponse: TNFSeCancelaNFSeResponse read GetCancelaNFSeResponse;
    property SubstituiNFSeResponse: TNFSeSubstituiNFSeResponse read GetSubstituiNFSeResponse;
  end;

implementation

uses
  Math, pcnAuxiliar, ACBrXmlBase, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  ACBrDFeUtil, ACBrDFeException, ACBrNFSeX;

{ TACBrNFSeXProvider }

constructor TACBrNFSeXProvider.Create(AOwner: TACBrDFe);
begin
  inherited Create;

  FAOwner := AOwner;
  if not Assigned(FAOwner) then
    raise EACBrDFeException.Create('Componente ACBrNFSeX não informado');

  FConfigGeral := TConfigGeral.Create;
  FConfigWebServices := TConfigWebServices.Create;
  FConfigMsgDados := TConfigMsgDados.Create;
  FConfigAssinar := TConfigAssinar.Create;
  FConfigSchemas := TConfigSchemas.Create;

  Configuracao;
end;

function TACBrNFSeXProvider.DefinirIDCancelamento(const CNPJ: string;
  const InscMunic: string; const NumNfse: string): string;
begin
  if ConfigGeral.Identificador <> '' then
    Result := ' ' + ConfigGeral.Identificador + '="Canc_' + CNPJ + InscMunic +
              NumNfse + '"'
  else
    Result := '';
end;

function TACBrNFSeXProvider.DefinirIDLote(const ID: string): string;
begin
  if ConfigGeral.Identificador <> '' then
    Result := ' ' + ConfigGeral.Identificador + '="Lote_' + ID + '"'
  else
    Result := '';
end;

destructor TACBrNFSeXProvider.Destroy;
begin
  FConfigGeral.Free;
  FConfigWebServices.Free;
  FConfigMsgDados.Free;
  FConfigAssinar.Free;
  FConfigSchemas.Free;

  inherited Destroy;
end;

function TACBrNFSeXProvider.GetConfigGeral: TConfigGeral;
begin
  Result := FConfigGeral;
end;

function TACBrNFSeXProvider.GetConfigWebServices: TConfigWebServices;
begin
  Result := FConfigWebServices;
end;

function TACBrNFSeXProvider.GetGerarResponse: TNFSeEmiteResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.Gerar;
end;

function TACBrNFSeXProvider.GetEmiteResponse: TNFSeEmiteResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.Emite;
end;

function TACBrNFSeXProvider.GetConsultaSituacaoResponse: TNFSeConsultaSituacaoResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.ConsultaSituacao;
end;

function TACBrNFSeXProvider.GetConsultaLoteRpsResponse: TNFSeConsultaLoteRpsResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.ConsultaLoteRps;
end;

function TACBrNFSeXProvider.GetConsultaNFSeporRpsResponse: TNFSeConsultaNFSeporRpsResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.ConsultaNFSeporRps;
end;

function TACBrNFSeXProvider.GetConsultaNFSeResponse: TNFSeConsultaNFSeResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.ConsultaNFSe;
end;

function TACBrNFSeXProvider.GetCancelaNFSeResponse: TNFSeCancelaNFSeResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.CancelaNFSe;
end;

function TACBrNFSeXProvider.GetSchemaPath: string;
begin
  with TACBrNFSeX(FAOwner).Configuracoes do
  begin
    Result := PathWithDelim(Arquivos.PathSchemas + Geral.xProvedor);
    Result := PathWithDelim(Result + VersaoNFSeToStr(Geral.Versao));
  end;
end;

function TACBrNFSeXProvider.GetSubstituiNFSeResponse: TNFSeSubstituiNFSeResponse;
begin
  Result := TACBrNFSeX(FAOwner).Webservice.SubstituiNFSe;
end;

function TACBrNFSeXProvider.GetCpfCnpj(const CpfCnpj: string; const Prefixo: string): string;
var
  xCpfCnpj: string;
begin
  xCpfCnpj := OnlyNumber(CpfCnpj);

  if xCpfCnpj <> '' then
  begin
    if Length(xCpfCnpj) = 14 then
      Result := '<' + Prefixo + 'Cnpj>' + xCpfCnpj + '</' + Prefixo + 'Cnpj>'
    else
      Result := '<' + Prefixo + 'Cpf>' + xCpfCnpj + '</' + Prefixo + 'Cpf>';
  end
  else
    Result := '';
end;

function TACBrNFSeXProvider.GetInscMunic(const InscMunic: string; const Prefixo: string): string;
begin
  if NaoEstaVazio(InscMunic) then
    Result := '<' + Prefixo + 'InscricaoMunicipal>' + InscMunic +
              '</' + Prefixo + 'InscricaoMunicipal>'
  else
    Result := '';
end;

function TACBrNFSeXProvider.GetWebServiceURL(const AMetodo: TMetodo): string;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        // Métodos padrões da versão 1 do layout da ABRASF
        tmRecepcionar: Result := Recepcionar;
        tmConsultarLote: Result := ConsultarLote;
        tmConsultarSituacao: Result := ConsultarSituacao;
        tmConsultarNFSePorRps: Result := ConsultarNFSeRps;
        tmConsultarNFSe: Result := ConsultarNFSe;
        tmCancelarNFSe: Result := CancelarNFSe;

        // Métodos padrões da versão 2 do layout da ABRASF
        tmRecepcionarSincrono: Result := RecepcionarSincrono;
        tmGerar: Result := GerarNFSe;
        tmSubstituirNFSe: Result := SubstituirNFSe;
        tmConsultarNFSePorFaixa: Result := ConsultarNFSePorFaixa;
        tmConsultarNFSeServicoPrestado: Result := ConsultarNFSeServicoPrestado;
        tmConsultarNFSeServicoTomado: Result := ConsultarNFSeServicoTomado;

        // Métodos que por padrão não existem na versão 1 e 2 do layout da ABRASF
        tmAbrirSessao: Result := AbrirSessao;
        tmFecharSessao: Result := FecharSessao;
        tmTeste: Result := TesteEnvio;
      else
        Result := '';
      end;
    end;
  end
  else
  begin
    with ConfigWebServices.Producao do
    begin
      case AMetodo of
        // Métodos padrões da versão 1 do layout da ABRASF
        tmRecepcionar: Result := Recepcionar;
        tmConsultarLote: Result := ConsultarLote;
        tmConsultarSituacao: Result := ConsultarSituacao;
        tmConsultarNFSePorRps: Result := ConsultarNFSeRps;
        tmConsultarNFSe: Result := ConsultarNFSe;
        tmCancelarNFSe: Result := CancelarNFSe;

        // Métodos padrões da versão 2 do layout da ABRASF
        tmRecepcionarSincrono: Result := RecepcionarSincrono;
        tmGerar: Result := GerarNFSe;
        tmSubstituirNFSe: Result := SubstituirNFSe;
        tmConsultarNFSePorFaixa: Result := ConsultarNFSePorFaixa;
        tmConsultarNFSeServicoPrestado: Result := ConsultarNFSeServicoPrestado;
        tmConsultarNFSeServicoTomado: Result := ConsultarNFSeServicoTomado;

        // Métodos que por padrão não existem na versão 1 e 2 do layout da ABRASF
        tmAbrirSessao: Result := AbrirSessao;
        tmFecharSessao: Result := FecharSessao;
        tmTeste: Result := TesteEnvio;
      else
        Result := '';
      end;
    end;
  end;
end;

function TACBrNFSeXProvider.GetConfigMsgDados: TConfigMsgDados;
begin
  Result := FConfigMsgDados;
end;

function TACBrNFSeXProvider.GetCabecalho(const Xmlns: string): string;
var
  Versao, NameSpace: string;
begin
  Versao := ' versao="' + ConfigWebServices.VersaoAtrib + '"';

  NameSpace := Xmlns;

  if NameSpace = '' then
    NameSpace := ConfigMsgDados.XmlRps.xmlns;

  NameSpace := ' xmlns="' + NameSpace + '"';

  Result := '<cabecalho' + Versao + NameSpace + '>' +
            '<versaoDados>' + ConfigWebServices.VersaoDados + '</versaoDados>' +
            '</cabecalho>';
end;

function TACBrNFSeXProvider.GetConfigAssinar: TConfigAssinar;
begin
  Result := FConfigAssinar;
end;

function TACBrNFSeXProvider.GetConfigSchemas: TConfigSchemas;
begin
  Result := FConfigSchemas;
end;

procedure TACBrNFSeXProvider.Configuracao;
begin
  // Inicializa os parâmetros de configuração: Geral
  with ConfigGeral do
  begin
    UseCertificateHTTP := True;
    UseAuthorizationHeader := False;
    NumMaxRpsGerar  := 1;
    NumMaxRpsEnviar := 50;
    ModoEnvio := meAutomatico;
    TabServicosExt := False;
    Identificador := 'Id';
    ConsultaSitLote := False;
    ConsultaLote := True;
    ConsultaNFSe := True;
    QuebradeLinha := ';';
    CancPreencherMotivo := False;
    CancPreencherSerieNfse := False;
    CancPreencherCodVerificacao := False;

    with TACBrNFSeX(FAOwner) do
    begin
      Provedor := Configuracoes.Geral.Provedor;
      Versao := Configuracoes.Geral.Versao;

      if Configuracoes.WebServices.AmbienteCodigo = 1 then
        Ambiente := taProducao
      else
        Ambiente := taHomologacao;

      CodIBGE := IntToStr(Configuracoes.Geral.CodigoMunicipio);
    end;
  end;

  // Inicializa os parâmetros de configuração: MsgDados
  with ConfigMsgDados do
  begin
    // Usado na tag raiz dos XML de envio do Lote, Consultas, etc.
    Prefixo := '';
    PrefixoTS := '';

    UsarNumLoteConsLote := False;

    // Usado para geração do Xml do Rps
    with XmlRps do
    begin
      xmlns := '';
      InfElemento := 'InfRps';
      DocElemento := 'Rps';
    end;

    // Usado para geração do Envio do Lote em modo assíncrono
    with LoteRps do
    begin
      xmlns := '';
      InfElemento := 'LoteRps';
      DocElemento := 'EnviarLoteRpsEnvio';
    end;

    // Usado para geração do Envio do Lote em modo Sincrono
    with LoteRpsSincrono do
    begin
      xmlns := '';
      InfElemento := 'LoteRps';
      DocElemento := 'EnviarLoteRpsSincronoEnvio';
    end;

    // Usado para geração da Consulta a Situação do Lote
    with ConsultarSituacao do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarSituacaoLoteRpsEnvio';
    end;

    // Usado para geração da Consulta do Lote
    with ConsultarLote do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarLoteRpsEnvio';
    end;

    // Usado para geração da Consulta da NFSe por RPS
    with ConsultarNFSeRps do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarNfseRpsEnvio';
    end;

    // Usado para geração da Consulta da NFSe
    with ConsultarNFSe do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarNfseEnvio';
    end;

    // Usado para geração da Consulta da NFSe Por Faixa
    with ConsultarNFSePorFaixa do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarNfseFaixaEnvio';
    end;

    // Usado para geração da Consulta da NFSe Servico Prestado
    with ConsultarNFSeServicoPrestado do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarNfseServicoPrestadoEnvio';
    end;

    // Usado para geração da Consulta da NFSe Servico Tomado
    with ConsultarNFSeServicoTomado do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'ConsultarNfseServicoTomadoEnvio';
    end;

    // Usado para geração do Cancelamento
    with CancelarNFSe do
    begin
      xmlns := '';
      InfElemento := 'InfPedidoCancelamento';
      DocElemento := 'Pedido';
    end;

    // Usado para geração do Gerar
    with GerarNFSe do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := 'GerarNfseEnvio';
    end;

    // Usado para geração do Substituir
    with SubstituirNFSe do
    begin
      xmlns := '';
      InfElemento := 'SubstituicaoNfse';
      DocElemento := 'SubstituirNfseEnvio';
    end;

    // Usado para geração da Abertura de Sessão
    with AbrirSessao do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := '';
    end;

    // Usado para geração do Fechamento de Sessão
    with FecharSessao do
    begin
      xmlns := '';
      InfElemento := '';
      DocElemento := '';
    end;
  end;

  // Inicializa os parâmetros de configuração: Assinar
  with ConfigAssinar do
  begin
    Rps := False;
    LoteRps := False;
    ConsultarSituacao := False;
    ConsultarLote := False;
    ConsultarNFSeRps := False;
    ConsultarNFSe := False;
    ConsultarNFSePorFaixa := False;
    ConsultarNFSeServicoPrestado := False;
    ConsultarNFSeServicoTomado := False;
    CancelarNFSe := False;
    RpsGerarNFSe := False;
    LoteGerarNFSe := False;
    RpsSubstituirNFSe := False;
    SubstituirNFSe := False;
    AbrirSessao := False;
    FecharSessao := False;

    IncluirURI := True;

    AssinaturaAdicional := False;
  end;

  SetNomeXSD('nfse.xsd');

  ConfigWebServices.AtribVerLote := '';

  CarregarURL;
end;

procedure TACBrNFSeXProvider.CarregarURL;
var
  IniParams: TMemIniFile;
  Sessao: String;
begin
  IniParams := TMemIniFile.Create('');

  with TACBrNFSeX(FAOwner) do
  begin
    IniParams.SetStrings(Configuracoes.WebServices.Params);
  end;

  try
    with TACBrNFSeX(FAOwner) do
    begin
      // Primeiro verifica as URLs definidas para a cidade
      Sessao := IntToStr(Configuracoes.Geral.CodigoMunicipio);
      ConfigWebServices.LoadUrlProducao(IniParams, Sessao);
      ConfigWebServices.LoadUrlHomologacao(IniParams, Sessao);
      ConfigGeral.LoadParams1(IniParams, Sessao);
      ConfigGeral.LoadParams2(IniParams, Sessao);

      // Depois verifica as URls definidas para o provedor
      if ConfigWebServices.Producao.Recepcionar = '' then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigWebServices.LoadUrlProducao(IniParams, Sessao);
      end;

      if ConfigWebServices.Homologacao.Recepcionar = '' then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigWebServices.LoadUrlHomologacao(IniParams, Sessao);
      end;

      // Se Params1 estiver vazio usar o que foi definido para o provedor
      if ConfigGeral.Params1 = '' then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigGeral.LoadParams1(IniParams, Sessao);
      end;

      // Se Params2 estiver vazio usar o que foi definido para o provedor
      if ConfigGeral.Params2 = '' then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigGeral.LoadParams2(IniParams, Sessao);
      end;

      {
      Sessao := Configuracoes.Geral.xProvedor;
      ConfigWebServices.LoadUrlProducao(IniParams, Sessao);
      ConfigWebServices.LoadUrlHomologacao(IniParams, Sessao);
      ConfigGeral.LoadParams1(IniParams, Sessao);
      ConfigGeral.LoadParams2(IniParams, Sessao);

      if ConfigWebServices.Producao.Recepcionar = '' then
      begin
        Sessao := IntToStr(Configuracoes.Geral.CodigoMunicipio);
        ConfigWebServices.LoadUrlProducao(IniParams, Sessao);
      end;

      if ConfigWebServices.Homologacao.Recepcionar = '' then
      begin
        Sessao := IntToStr(Configuracoes.Geral.CodigoMunicipio);
        ConfigWebServices.LoadUrlHomologacao(IniParams, Sessao);
      end;

      if ConfigWebServices.Homologacao.Recepcionar = '' then
      begin
        Sessao := Configuracoes.Geral.xProvedor;
        ConfigWebServices.LoadUrlHomologacao(IniParams, Sessao);
      end;

      if ConfigWebServices.Homologacao.Recepcionar = '' then
      begin
        Sessao := IntToStr(Configuracoes.Geral.CodigoMunicipio);
        ConfigWebServices.HomologacaoIgualProducao(IniParams, Sessao);
      end;

      if ConfigGeral.Params1 = '' then
      begin
        Sessao := IntToStr(Configuracoes.Geral.CodigoMunicipio);
        ConfigGeral.LoadParams1(IniParams, Sessao);
      end;

      if ConfigGeral.Params2 = '' then
      begin
        Sessao := IntToStr(Configuracoes.Geral.CodigoMunicipio);
        ConfigGeral.LoadParams2(IniParams, Sessao);
      end;
      }
    end;
  finally
    IniParams.Free;
  end;
end;

procedure TACBrNFSeXProvider.SalvarXmlRps(aNota: NotaFiscal);
begin
  if FAOwner.Configuracoes.Arquivos.Salvar then
  begin
    if NaoEstaVazio(aNota.NomeArqRps) then
      TACBrNFSeX(FAOwner).Gravar(aNota.NomeArqRps, aNota.XMLOriginal)
    else
    begin
      aNota.NomeArqRps := aNota.CalcularNomeArquivoCompleto(aNota.NomeArqRps, '');
      TACBrNFSeX(FAOwner).Gravar(aNota.NomeArqRps, aNota.XMLOriginal);
    end;
  end;
end;

procedure TACBrNFSeXProvider.SalvarXmlNfse(aNota: NotaFiscal);
var
  aPath, NomeArq: string;
  aConfig: TConfiguracoesNFSe;
begin
  aConfig := TConfiguracoesNFSe(FAOwner.Configuracoes);
  aPath := aConfig.Arquivos.GetPathNFSe;

  NomeArq := TACBrNFSeX(FAOwner).GetNumID(aNota.NFSe) + '-nfse.xml';
  aNota.NomeArq := NomeArq;
  aNota.Confirmada := True;

  if FAOwner.Configuracoes.Arquivos.Salvar then
    TACBrNFSeX(FAOwner).Gravar(NomeArq, aNota.XML, aPath);
end;

procedure TACBrNFSeXProvider.SetNomeXSD(const aNome: string);
begin
  with ConfigSchemas do
  begin
    Recepcionar := aNome;
    ConsultarSituacao := aNome;
    ConsultarLote := aNome;
    ConsultarNFSeRps := aNome;
    ConsultarNFSe := aNome;
    ConsultarNFSePorFaixa := aNome;
    ConsultarNFSeServicoPrestado := aNome;
    ConsultarNFSeServicoTomado := aNome;
    CancelarNFSe := aNome;
    GerarNFSe := aNome;
    RecepcionarSincrono := aNome;
    SubstituirNFSe := aNome;
    AbrirSessao := aNome;
    FecharSessao := aNome;
    Teste := aNome;
    Validar := True;
  end;
end;

procedure TACBrNFSeXProvider.SetXmlNameSpace(const aNameSpace: string);
begin
  with ConfigMsgDados do
  begin
    XmlRps.xmlns := aNameSpace;
    LoteRps.xmlns := aNameSpace;
    LoteRpsSincrono.xmlns := aNameSpace;
    ConsultarSituacao.xmlns := aNameSpace;
    ConsultarLote.xmlns := aNameSpace;
    ConsultarNFSeRps.xmlns := aNameSpace;
    ConsultarNFSe.xmlns := aNameSpace;
    ConsultarNFSePorFaixa.xmlns := aNameSpace;
    ConsultarNFSeServicoPrestado.xmlns := aNameSpace;
    ConsultarNFSeServicoTomado.xmlns := aNameSpace;
    CancelarNFSe.xmlns := aNameSpace;
    GerarNFSe.xmlns := aNameSpace;
    SubstituirNFSe.xmlns := aNameSpace;
    AbrirSessao.xmlns := aNameSpace;
    FecharSessao.xmlns := aNameSpace;
  end;
end;

function TACBrNFSeXProvider.GerarXml(const aNFSe: TNFSe; var aXml,
  aAlerts: string): Boolean;
var
  AWriter: TNFSeWClass;
begin
  AWriter := CriarGeradorXml(aNFSe);

  try
    with TACBrNFSeX(FAOwner) do
    begin
      if Configuracoes.WebServices.AmbienteCodigo = 1 then
        AWriter.Ambiente := taProducao
      else
        AWriter.Ambiente := taHomologacao;

      AWriter.CodMunEmit     := Configuracoes.Geral.CodigoMunicipio;
      AWriter.CNPJPrefeitura := Configuracoes.Geral.CNPJPrefeitura;

      AWriter.Usuario      := Configuracoes.Geral.Emitente.WSUser;
      AWriter.Senha        := Configuracoes.Geral.Emitente.WSSenha;
      AWriter.ChaveAcesso  := Configuracoes.Geral.Emitente.WSChaveAcesso;
      AWriter.ChaveAutoriz := Configuracoes.Geral.Emitente.WSChaveAutoriz;
      AWriter.FraseSecreta := Configuracoes.Geral.Emitente.WSFraseSecr;
      AWriter.Provedor     := Configuracoes.Geral.Provedor;
      AWriter.VersaoNFSe   := Configuracoes.Geral.Versao;

      if AWriter.Ambiente = taProducao then
        AWriter.Municipio := ConfigGeral.Params1
      else
        AWriter.Municipio := ConfigGeral.Params2;

      pcnAuxiliar.TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );

      AWriter.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
      AWriter.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
      AWriter.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
      AWriter.Opcoes.IdentarXML     := Configuracoes.Geral.IdentarXML;
    end;

    Result := AWriter.GerarXml;
    aXml := AWriter.Document.Xml;
    aAlerts := ACBrStr(AWriter.ListaDeAlertas.Text);
  finally
    AWriter.Destroy;
  end;
end;

function TACBrNFSeXProvider.LerXML(const aXML: String; var aNFSe: TNFSe): Boolean;
var
  AReader: TNFSeRClass;
begin
  AReader := CriarLeitorXml(aNFSe);
  AReader.Arquivo := aXML;

  try
    AReader.Provedor := TACBrNFSeX(FAOwner).Configuracoes.Geral.Provedor;

    Result := AReader.LerXml;
  finally
    AReader.Destroy;
  end;
end;

function TACBrNFSeXProvider.PrepararRpsParaLote(const aXml: string): string;
var
  i: Integer;
  Prefixo: string;
begin
  i := Pos('>', aXml) + 1;

  if ConfigMsgDados.PrefixoTS = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.PrefixoTS + ':';

  Result := '<' + Prefixo + 'Rps>' + Copy(aXml, i, Length(aXml));
end;

procedure TACBrNFSeXProvider.ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
var
  AErro: TNFSeEventoCollectionItem;
  Erros, Schema: string;
begin
  if not ConfigSchemas.Validar Then Exit;

  Erros := '';
  Schema := '';

  case aMetodo of
    tmRecepcionar: Schema := ConfigSchemas.Recepcionar;
    tmConsultarSituacao: Schema := ConfigSchemas.ConsultarSituacao;
    tmConsultarLote: Schema := ConfigSchemas.ConsultarLote;
    tmConsultarNFSePorRps: Schema := ConfigSchemas.ConsultarNFSeRps;

    tmConsultarNFSe: Schema := ConfigSchemas.ConsultarNFSe;
    tmConsultarNFSePorFaixa: Schema := ConfigSchemas.ConsultarNFSePorFaixa;
    tmConsultarNFSeServicoPrestado: Schema := ConfigSchemas.ConsultarNFSeServicoPrestado;
    tmConsultarNFSeServicoTomado: Schema := ConfigSchemas.ConsultarNFSeServicoTomado;

    tmCancelarNFSe: Schema := ConfigSchemas.CancelarNFSe;
    tmGerar: Schema := ConfigSchemas.GerarNFSe;
    tmRecepcionarSincrono: Schema := ConfigSchemas.RecepcionarSincrono;
    tmSubstituirNFSe: Schema := ConfigSchemas.SubstituirNFSe;
    tmAbrirSessao: Schema := ConfigSchemas.AbrirSessao;
    tmFecharSessao: Schema := ConfigSchemas.FecharSessao;
  else
    // tmTeste
    Schema := ConfigSchemas.Teste;
  end;

  if TACBrNFSeX(FAOwner).Configuracoes.Geral.MontarPathSchema then
    Schema := PathWithDelim(GetSchemaPath) + Schema
  else
    Schema := FAOwner.Configuracoes.Arquivos.PathSchemas + Schema;

  FAOwner.SSL.Validar(Response.XmlEnvio, Schema, Erros);

  if NaoEstaVazio(Erros) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod800;
    AErro.Descricao := 'Erro de Validação: ' + Erros;
  end;
end;

procedure TACBrNFSeXProvider.GeraLote;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeRecepcao);

  if GerarResponse.ModoEnvio = meAutomatico then
    GerarResponse.ModoEnvio := ConfigGeral.ModoEnvio;

  if GerarResponse.ModoEnvio <> meUnitario then
    GerarResponse.MaxRps := ConfigGeral.NumMaxRpsEnviar
  else
    GerarResponse.MaxRps := ConfigGeral.NumMaxRpsGerar;

  PrepararEmitir(GerarResponse);
  if (GerarResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarEmitir(GerarResponse);
  if (GerarResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  case GerarResponse.ModoEnvio of
    meLoteAssincrono,
    meTeste: ValidarSchema(GerarResponse, tmRecepcionar);
    meLoteSincrono: ValidarSchema(GerarResponse, tmRecepcionarSincrono);
  else
    // meUnitario
    ValidarSchema(GerarResponse, tmGerar);
  end;

  if (GerarResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  GerarResponse.NomeArq := GerarResponse.Lote + '-env-lot.xml';

  FAOwner.Gravar(GerarResponse.NomeArq, GerarResponse.XmlEnvio);

  GerarResponse.NomeArq := PathWithDelim(TACBrNFSeX(FAOwner).Configuracoes.Arquivos.PathSalvar) +
                           GerarResponse.NomeArq;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.Emite;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeRecepcao);

  if EmiteResponse.ModoEnvio = meAutomatico then
    EmiteResponse.ModoEnvio := ConfigGeral.ModoEnvio;

  if EmiteResponse.ModoEnvio <> meUnitario then
    EmiteResponse.MaxRps := ConfigGeral.NumMaxRpsEnviar
  else
    EmiteResponse.MaxRps := ConfigGeral.NumMaxRpsGerar;

  PrepararEmitir(EmiteResponse);
  if (EmiteResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarEmitir(EmiteResponse);
  if (EmiteResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  case EmiteResponse.ModoEnvio of
    meLoteAssincrono,
    meTeste: ValidarSchema(EmiteResponse, tmRecepcionar);
    meLoteSincrono: ValidarSchema(EmiteResponse, tmRecepcionarSincrono);
  else
    // meUnitario
    ValidarSchema(EmiteResponse, tmGerar);
  end;

  if (EmiteResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      case EmiteResponse.ModoEnvio of
        meLoteAssincrono:
          begin
            AService := CriarServiceClient(tmRecepcionar);
            AService.Prefixo := EmiteResponse.Lote;
            EmiteResponse.XmlRetorno := AService.Recepcionar(ConfigMsgDados.DadosCabecalho, EmiteResponse.XmlEnvio);
          end;

        meTeste:
          begin
            AService := CriarServiceClient(tmRecepcionar);
            AService.Prefixo := EmiteResponse.Lote;
            EmiteResponse.XmlRetorno := AService.TesteEnvio(ConfigMsgDados.DadosCabecalho, EmiteResponse.XmlEnvio);
          end;

        meLoteSincrono:
          begin
            AService := CriarServiceClient(tmRecepcionarSincrono);
            AService.Prefixo := EmiteResponse.Lote;
            EmiteResponse.XmlRetorno := AService.RecepcionarSincrono(ConfigMsgDados.DadosCabecalho, EmiteResponse.XmlEnvio);
          end;
      else
        // meUnitario
        begin
          AService := CriarServiceClient(tmGerar);
          AService.Prefixo := EmiteResponse.Lote;
          EmiteResponse.XmlRetorno := AService.GerarNFSe(ConfigMsgDados.DadosCabecalho, EmiteResponse.XmlEnvio);
        end;
      end;

      EmiteResponse.Sucesso := True;
      EmiteResponse.EnvelopeEnvio := AService.Envio;
      EmiteResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        AErro := EmiteResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not EmiteResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoEmitir(EmiteResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.ConsultaSituacao;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsultaSituacao);

  PrepararConsultaSituacao(ConsultaSituacaoResponse);
  if (ConsultaSituacaoResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultaSituacao(ConsultaSituacaoResponse);
  if (ConsultaSituacaoResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(ConsultaSituacaoResponse, tmConsultarSituacao);
  if (ConsultaSituacaoResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmConsultarSituacao);
      AService.Prefixo := ConsultaSituacaoResponse.Protocolo;
      ConsultaSituacaoResponse.XmlRetorno := AService.ConsultarSituacao(ConfigMsgDados.DadosCabecalho,
                                                                        ConsultaSituacaoResponse.XmlEnvio);

      ConsultaSituacaoResponse.Sucesso := True;
      ConsultaSituacaoResponse.EnvelopeEnvio := AService.Envio;
      ConsultaSituacaoResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        AErro := ConsultaSituacaoResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not ConsultaSituacaoResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultaSituacao(ConsultaSituacaoResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.ConsultaLoteRps;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsulta);

  PrepararConsultaLoteRps(ConsultaLoteRpsResponse);
  if (ConsultaLoteRpsResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultaLoteRps(ConsultaLoteRpsResponse);
  if (ConsultaLoteRpsResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(ConsultaLoteRpsResponse, tmConsultarLote);
  if (ConsultaLoteRpsResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmConsultarLote);
      AService.Prefixo := ConsultaLoteRpsResponse.Protocolo;
      ConsultaLoteRpsResponse.XmlRetorno := AService.ConsultarLote(ConfigMsgDados.DadosCabecalho, ConsultaLoteRpsResponse.XmlEnvio);

      ConsultaLoteRpsResponse.Sucesso := True;
      ConsultaLoteRpsResponse.EnvelopeEnvio := AService.Envio;
      ConsultaLoteRpsResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        AErro := ConsultaLoteRpsResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not ConsultaLoteRpsResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultaLoteRps(ConsultaLoteRpsResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.ConsultaNFSeporRps;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsulta);

  PrepararConsultaNFSeporRps(ConsultaNFSeporRpsResponse);
  if (ConsultaNFSeporRpsResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultaNFSeporRps(ConsultaNFSeporRpsResponse);
  if (ConsultaNFSeporRpsResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(ConsultaNFSeporRpsResponse, tmConsultarNFSePorRps);
  if (ConsultaNFSeporRpsResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmConsultarNFSePorRps);
      AService.Prefixo := ConsultaNFSeporRpsResponse.NumRPS + ConsultaNFSeporRpsResponse.Serie;
      ConsultaNFSeporRpsResponse.XmlRetorno := AService.ConsultarNFSePorRps(ConfigMsgDados.DadosCabecalho,
                                                                            ConsultaNFSeporRpsResponse.XmlEnvio);

      ConsultaNFSeporRpsResponse.Sucesso := True;
      ConsultaNFSeporRpsResponse.EnvelopeEnvio := AService.Envio;
      ConsultaNFSeporRpsResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        AErro := ConsultaNFSeporRpsResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not ConsultaNFSeporRpsResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultaNFSeporRps(ConsultaNFSeporRpsResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.ConsultaNFSe;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
  Prefixo: string;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsulta);

  PrepararConsultaNFSe(ConsultaNFSeResponse);
  if (ConsultaNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultaNFSe(ConsultaNFSeResponse);
  if (ConsultaNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(ConsultaNFSeResponse, ConsultaNFSeResponse.Metodo);
  if (ConsultaNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);
      AService := CriarServiceClient(ConsultaNFSeResponse.Metodo);

      if (ConsultaNFSeResponse.InfConsultaNFSe.DataInicial > 0) and (ConsultaNFSeResponse.InfConsultaNFSe.DataFinal > 0) then
        Prefixo := FormatDateTime('yyyymmdd', ConsultaNFSeResponse.InfConsultaNFSe.DataInicial) +
                   FormatDateTime('yyyymmdd', ConsultaNFSeResponse.InfConsultaNFSe.DataFinal)
      else
        Prefixo := FormatFloat('000000000000000', StrToIntDef(ConsultaNFSeResponse.InfConsultaNFSe.NumeroIniNFSe, 0)) +
                   FormatFloat('000000000000000', StrToIntDef(ConsultaNFSeResponse.InfConsultaNFSe.NumeroFinNFSe, 0)) +
                   FormatFloat('000000', ConsultaNFSeResponse.InfConsultaNFSe.Pagina);

      AService.Prefixo := Prefixo;

      case ConsultaNFSeResponse.Metodo of
        tmConsultarNFSePorFaixa:
          ConsultaNFSeResponse.XmlRetorno := AService.ConsultarNFSePorFaixa(ConfigMsgDados.DadosCabecalho,
                                                                            ConsultaNFSeResponse.XmlEnvio);

        tmConsultarNFSeServicoPrestado:
          ConsultaNFSeResponse.XmlRetorno := AService.ConsultarNFSeServicoPrestado(ConfigMsgDados.DadosCabecalho,
                                                                                   ConsultaNFSeResponse.XmlEnvio);

        tmConsultarNFSeServicoTomado:
          ConsultaNFSeResponse.XmlRetorno := AService.ConsultarNFSeServicoTomado(ConfigMsgDados.DadosCabecalho,
                                                                                 ConsultaNFSeResponse.XmlEnvio);

      else
        ConsultaNFSeResponse.XmlRetorno := AService.ConsultarNFSe(ConfigMsgDados.DadosCabecalho,
                                                                  ConsultaNFSeResponse.XmlEnvio);
      end;

      ConsultaNFSeResponse.Sucesso := True;
      ConsultaNFSeResponse.EnvelopeEnvio := AService.Envio;
      ConsultaNFSeResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        AErro := ConsultaNFSeResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not ConsultaNFSeResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultaNFSe(ConsultaNFSeResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.CancelaNFSe;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeCancelamento);

  PrepararCancelaNFSe(CancelaNFSeResponse);
  if (CancelaNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarCancelaNFSe(CancelaNFSeResponse);
  if (CancelaNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(CancelaNFSeResponse, tmCancelarNFSe);
  if (CancelaNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmCancelarNFSe);

      if CancelaNFSeResponse.InfCancelamento.NumeroNFSe <> '' then
        AService.Prefixo := CancelaNFSeResponse.InfCancelamento.NumeroNFSe
      else
        AService.Prefixo := CancelaNFSeResponse.InfCancelamento.ChaveNFSe;

      CancelaNFSeResponse.XmlRetorno := AService.Cancelar(ConfigMsgDados.DadosCabecalho, CancelaNFSeResponse.XmlEnvio);

      CancelaNFSeResponse.Sucesso := True;
      CancelaNFSeResponse.EnvelopeEnvio := AService.Envio;
      CancelaNFSeResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        AErro := CancelaNFSeResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not CancelaNFSeResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoCancelaNFSe(CancelaNFSeResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.SubstituiNFSe;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
  Cancelamento: TNFSeCancelaNFSeResponse;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeSubstituicao);

  Cancelamento := TNFSeCancelaNFSeResponse.Create;
  try
    with Cancelamento.InfCancelamento do
    begin
      NumeroNFSe := SubstituiNFSeResponse.InfCancelamento.NumeroNFSe;
      SerieNFSe := SubstituiNFSeResponse.InfCancelamento.SerieNFSe;
      CodCancelamento := SubstituiNFSeResponse.InfCancelamento.CodCancelamento;
      MotCancelamento := SubstituiNFSeResponse.InfCancelamento.MotCancelamento;
      NumeroLote := SubstituiNFSeResponse.InfCancelamento.NumeroLote;
      CodVerificacao := SubstituiNFSeResponse.InfCancelamento.CodVerificacao;
    end;

    PrepararCancelaNFSe(Cancelamento);
    if (Cancelamento.Erros.Count > 0) then
    begin
      TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
      Exit;
    end;

    AssinarCancelaNFSe(Cancelamento);
    if (Cancelamento.Erros.Count > 0) then
    begin
      TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
      Exit;
    end;

    SubstituiNFSeResponse.PedCanc := Cancelamento.XmlEnvio;
    SubstituiNFSeResponse.PedCanc := SepararDados(SubstituiNFSeResponse.PedCanc, 'CancelarNfseEnvio', False);
  finally
    FreeAndNil(Cancelamento);
  end;

  PrepararSubstituiNFSe(SubstituiNFSeResponse);
  if (SubstituiNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarSubstituiNFSe(SubstituiNFSeResponse);
  if (SubstituiNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(SubstituiNFSeResponse, tmSubstituirNFSe);
  if (SubstituiNFSeResponse.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmSubstituirNFSe);
      AService.Prefixo := SubstituiNFSeResponse.InfCancelamento.NumeroNFSe;
      SubstituiNFSeResponse.XmlRetorno := AService.SubstituirNFSe(ConfigMsgDados.DadosCabecalho,
                                                                  SubstituiNFSeResponse.XmlEnvio);

      SubstituiNFSeResponse.Sucesso := True;
      SubstituiNFSeResponse.EnvelopeEnvio := AService.Envio;
      SubstituiNFSeResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        AErro := SubstituiNFSeResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := Desc999 + E.Message;
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not SubstituiNFSeResponse.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoSubstituiNFSe(SubstituiNFSeResponse);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

procedure TACBrNFSeXProvider.AssinarCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.CancelarNFSe then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.XmlEnvio := FAOwner.SSL.Assinar(Response.XmlEnvio,
      Prefixo + ConfigMsgDados.CancelarNFSe.DocElemento,
      ConfigMsgDados.CancelarNFSe.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := Desc801 + E.Message;
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.ConsultarLote then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.XmlEnvio := FAOwner.SSL.Assinar(Response.XmlEnvio,
      Prefixo + ConfigMsgDados.ConsultarLote.DocElemento,
      ConfigMsgDados.ConsultarLote.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := Desc801 + E.Message;
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.ConsultarNFSe then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.XmlEnvio := FAOwner.SSL.Assinar(Response.XmlEnvio,
      Prefixo + ConfigMsgDados.ConsultarNFSe.DocElemento,
      ConfigMsgDados.ConsultarNFSe.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := Desc801 + E.Message;
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.ConsultarNFSeRps then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.XmlEnvio := FAOwner.SSL.Assinar(Response.XmlEnvio,
      Prefixo + ConfigMsgDados.ConsultarNFSeRps.DocElemento,
      ConfigMsgDados.ConsultarNFSeRps.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := Desc801 + E.Message;
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarConsultaSituacao(Response: TNFSeConsultaSituacaoResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.ConsultarSituacao then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.XmlEnvio := FAOwner.SSL.Assinar(Response.XmlEnvio,
      Prefixo + ConfigMsgDados.ConsultarSituacao.DocElemento,
      ConfigMsgDados.ConsultarSituacao.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := Desc801 + E.Message;
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarEmitir(Response: TNFSeEmiteResponse);
var
  IdAttr, Prefixo, PrefixoTS: string;
  AErro: TNFSeEventoCollectionItem;
begin
  case Response.ModoEnvio of
    meTeste,
    meLoteSincrono,
    meLoteAssincrono:
      if not ConfigAssinar.LoteRps then Exit;
  else
    if not ConfigAssinar.LoteGerarNFSe then Exit;
  end;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  if ConfigMsgDados.PrefixoTS = '' then
    PrefixoTS := ''
  else
    PrefixoTS := ConfigMsgDados.PrefixoTS + ':';

  try
    case Response.ModoEnvio of
      meLoteSincrono:
        Response.XmlEnvio := FAOwner.SSL.Assinar(Response.XmlEnvio,
          Prefixo + ConfigMsgDados.LoteRpsSincrono.DocElemento,
          Prefixo + ConfigMsgDados.LoteRpsSincrono.InfElemento, '', '', '', IdAttr);

      meTeste,
      meLoteAssincrono:
        Response.XmlEnvio := FAOwner.SSL.Assinar(Response.XmlEnvio,
          Prefixo + ConfigMsgDados.LoteRps.DocElemento,
          {Prefixo + }ConfigMsgDados.LoteRps.InfElemento, '', '', '', IdAttr);
    else
      Response.XmlEnvio := FAOwner.SSL.Assinar(Response.XmlEnvio,
        Prefixo + ConfigMsgDados.GerarNFSe.DocElemento,
        PrefixoTS + ConfigMsgDados.GerarNFSe.InfElemento, '', '', '', IdAttr);
    end;
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := Desc801 + E.Message;
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.SubstituirNFSe then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if ConfigMsgDados.Prefixo = '' then
    Prefixo := ''
  else
    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.XmlEnvio := FAOwner.SSL.Assinar(Response.XmlEnvio,
      Prefixo + ConfigMsgDados.SubstituirNFSe.DocElemento,
      ConfigMsgDados.SubstituirNFSe.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := Desc801 + E.Message;
    end;
  end;
end;

end.
