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

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; virtual; abstract;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; virtual; abstract;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; virtual; abstract;
    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); virtual;

    function PrepararRpsParaLote(const aXml: string): string; virtual;
    function GetCpfCnpj(const CpfCnpj: string; const Prefixo: string = ''): string;
    function GetInscMunic(const InscMunic: string; const Prefixo: string = ''): string;
    function GetCabecalho(const Xmlns: string = ''): string;

    //metodos para geração e tratamento dos dados do metodo emitir
    procedure PrepararEmitir(Response: TNFSeEmiteResponse); virtual; abstract;
    procedure AssinarEmitir(Response: TNFSeEmiteResponse); virtual;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultaSituacao
    procedure PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); virtual; abstract;
    procedure AssinarConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); virtual;
    procedure TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultaLoteRps
    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); virtual; abstract;
    procedure AssinarConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); virtual;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultaNFSeporRps
    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); virtual; abstract;
    procedure AssinarConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); virtual;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo ConsultaNFSe
    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); virtual; abstract;
    procedure AssinarConsultaNFSe(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo CancelaNFSe
    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); virtual; abstract;
    procedure AssinarCancelaNFSe(Response: TNFSeCancelaNFSeResponse); virtual;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo CancelaNFSe
    procedure PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); virtual; abstract;
    procedure AssinarSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); virtual;
    procedure TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); virtual; abstract;

  public
    constructor Create(AOwner: TACBrDFe);
    destructor Destroy; override;

    function GerarXml(const aNFSe: TNFSe; var aXml, aAlerts: string): Boolean; virtual;
    function LerXML(const aXML: String; var aNFSe: TNFSe): Boolean; virtual;

    function GeraLote(const aLote: String; aqMaxRps: Integer; aModoEnvio: TmodoEnvio): TNFSeEmiteResponse; virtual;
    function Emite(const aLote: String; aModoEnvio: TmodoEnvio): TNFSeEmiteResponse; virtual;
    function ConsultaSituacao(const aProtocolo, aNumLote: String): TNFSeConsultaSituacaoResponse; virtual;
    function ConsultaLoteRps(const aProtocolo, aNumLote: String): TNFSeConsultaLoteRpsResponse; virtual;
    function ConsultaNFSeporRps(const aNumRPS, aSerie, aTipo,
      aCodVerificacao: String): TNFSeConsultaNFSeporRpsResponse; virtual;
    function ConsultaNFSe(aInfConsultaNFSe: TInfConsultaNFSe): TNFSeConsultaNFSeResponse; virtual;
    function CancelaNFSe(aInfCancelamento: TInfCancelamento): TNFSeCancelaNFSeResponse; virtual;
    function SubstituiNFSe(const aNumNFSe, aSerieNFSe, aCodCancelamento,
      aMotCancelamento,
      aNumLote, aCodVerificacao: String): TNFSeSubstituiNFSeResponse; virtual;

    property ConfigGeral: TConfigGeral read GetConfigGeral;
    property ConfigWebServices: TConfigWebServices read GetConfigWebServices;
    property ConfigMsgDados: TConfigMsgDados read GetConfigMsgDados;
    property ConfigAssinar: TConfigAssinar read GetConfigAssinar;
    property ConfigSchemas: TConfigSchemas read GetConfigSchemas;
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
    Result := '<' + Prefixo + 'InscricaoMunicipal>' + OnlyNumber(InscMunic) +
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
        tmConsultarNFSeURL: Result := ConsultarNFSeURL;
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
        tmConsultarNFSeURL: Result := ConsultarNFSeURL;
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
    FormatoItemListaServico := filsComFormatacao;
    TabServicosExt := False;
    Identificador := 'Id';
    ConsultaSitLote := False;
    ConsultaLote := True;
  end;

  // Inicializa os parâmetros de configuração: MsgDados
  with ConfigMsgDados do
  begin
    // Usado na tag raiz dos XML de envio do Lote, Consultas, etc.
    Prefixo := '';
    PrefixoTS := '';

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
      Sessao := Configuracoes.Geral.xProvedor;
      ConfigWebServices.LoadUrl(IniParams, Sessao);
      ConfigGeral.LoadParams1(IniParams, Sessao);
      ConfigGeral.LoadParams2(IniParams, Sessao);

      if ConfigWebServices.Producao.Recepcionar = '' then
      begin
        Sessao := IntToStr(Configuracoes.Geral.CodigoMunicipio);
        ConfigWebServices.LoadUrl(IniParams, Sessao);
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
  if FAOwner.Configuracoes.Arquivos.Salvar then
  begin
    aConfig := TConfiguracoesNFSe(FAOwner.Configuracoes);
    aPath := aConfig.Arquivos.GetPathNFSe;

    if aConfig.Arquivos.NomeLongoNFSe then
      NomeArq := GerarNomeNFSe(aConfig.WebServices.UFCodigo,
                               aNota.NFSe.DataEmissao,
                               aNota.NFSe.Prestador.IdentificacaoPrestador.Cnpj,
                               StrToInt64Def(aNota.NFSe.Numero, 0)) + '-nfse.xml'
    else
      NomeArq := aNota.NFSe.Numero +
                 aNota.NFSe.IdentificacaoRps.Serie +
                 '-nfse.xml';

    aNota.NomeArq := NomeArq;

    TACBrNFSeX(FAOwner).Gravar(NomeArq, aNota.XML, aPath);
  end;
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
    ConsultarNFSeURL := aNome;
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
    with TACBrNFSeX(FAOwner) do
    begin
      AReader.Provedor     := Configuracoes.Geral.Provedor;
      AReader.ProvedorConf := Configuracoes.Geral.Provedor;
    end;

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
    tmConsultarNFSeURL: Schema := ConfigSchemas.ConsultarNFSeURL;

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

  Schema := FAOwner.Configuracoes.Arquivos.PathSchemas + Schema;

  FAOwner.SSL.Validar(Response.XmlEnvio, Schema, Erros);

  if NaoEstaVazio(Erros) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod999;
    AErro.Descricao := Erros;
  end;
end;

function TACBrNFSeXProvider.GeraLote(const aLote: String; aqMaxRps: Integer;
  aModoEnvio: TmodoEnvio): TNFSeEmiteResponse;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeRecepcao);

  Result := TNFSeEmiteResponse.Create;
  Result.Lote := ALote;
  Result.ModoEnvio := aModoEnvio;
  Result.MaxRps := aqMaxRps;

  PrepararEmitir(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarEmitir(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  if Result.ModoEnvio = meAutomatico then
    Result.ModoEnvio := ConfigGeral.ModoEnvio;

  case Result.ModoEnvio of
    meLoteAssincrono,
    meTeste: ValidarSchema(Result, tmRecepcionar);
    meLoteSincrono: ValidarSchema(Result, tmRecepcionarSincrono);
  else
    // meUnitario
    ValidarSchema(Result, tmGerar);
  end;

  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  if not Result.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  FAOwner.Gravar(aLote + 'env-lot.xml', Result.XmlEnvio);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

function TACBrNFSeXProvider.Emite(const aLote: String; aModoEnvio: TmodoEnvio): TNFSeEmiteResponse;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
  Protocolo: string;
  RetornoConsSit: TNFSeConsultaSituacaoResponse;
  RetornoConsLote: TNFSeConsultaLoteRpsResponse;
  qTentativas, Intervalo, Situacao: Integer;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeRecepcao);

  Result := TNFSeEmiteResponse.Create;
  Result.Lote := aLote;

  if aModoEnvio = meAutomatico then
    Result.ModoEnvio := ConfigGeral.ModoEnvio
  else
    Result.ModoEnvio := aModoEnvio;

  if Result.ModoEnvio <> meUnitario then
    Result.MaxRps := ConfigGeral.NumMaxRpsEnviar
  else
    Result.MaxRps := ConfigGeral.NumMaxRpsGerar;

  PrepararEmitir(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarEmitir(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  case Result.ModoEnvio of
    meLoteAssincrono,
    meTeste: ValidarSchema(Result, tmRecepcionar);
    meLoteSincrono: ValidarSchema(Result, tmRecepcionarSincrono);
  else
    // meUnitario
    ValidarSchema(Result, tmGerar);
  end;

  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      case Result.ModoEnvio of
        meLoteAssincrono:
          begin
            AService := CriarServiceClient(tmRecepcionar);
            AService.Prefixo := Result.Lote;
            Result.XmlRetorno := AService.Recepcionar(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);
          end;

        meTeste:
          begin
            AService := CriarServiceClient(tmRecepcionar);
            AService.Prefixo := Result.Lote;
            Result.XmlRetorno := AService.TesteEnvio(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);
          end;

        meLoteSincrono:
          begin
            AService := CriarServiceClient(tmRecepcionarSincrono);
            AService.Prefixo := Result.Lote;
            Result.XmlRetorno := AService.RecepcionarSincrono(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);
          end;
      else
        // meUnitario
        begin
          AService := CriarServiceClient(tmGerar);
          AService.Prefixo := Result.Lote;
          Result.XmlRetorno := AService.GerarNFSe(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);
        end;
      end;

      Result.Sucesso := True;
      Result.EnvelopeEnvio := AService.Envio;
      Result.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        AErro := Result.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not Result.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoEmitir(Result);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);

  if TACBrNFSeX(FAOwner).Configuracoes.Geral.ConsultaLoteAposEnvio and
     (Result.ModoEnvio = meLoteAssincrono) then
  begin
    Protocolo := Result.Protocolo;

    if Protocolo <> '' then
    begin
      if ConfigGeral.ConsultaSitLote then
      begin
//        RetornoConsSit := TNFSeConsultaSituacaoResponse.Create;

        with TACBrNFSeX(FAOwner).Configuracoes.WebServices do
        begin
          try
            Sleep(AguardarConsultaRet);

            qTentativas := 0;
            Situacao := 0;
            Intervalo := max(IntervaloTentativas, 1000);

            while (Situacao < 3) and (qTentativas < Tentativas) do
            begin
              RetornoConsSit := ConsultaSituacao(Protocolo, aLote);

              Situacao := StrToIntDef(RetornoConsSit.Situacao, 0);
              Inc(qTentativas);
              sleep(Intervalo);
            end;
          finally
            Result.InfRetorno.Situacao := RetornoConsSit.Situacao;
          end;
        end;
      end;

      if ConfigGeral.ConsultaLote then
      begin
//        RetornoConsLote := TNFSeConsultaLoteRpsResponse.Create;

        RetornoConsLote := ConsultaLoteRps(Protocolo, aLote);
      end;
    end;
  end;
end;

function TACBrNFSeXProvider.ConsultaSituacao(const aProtocolo, aNumLote: String): TNFSeConsultaSituacaoResponse;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsultaSituacao);

  Result := TNFSeConsultaSituacaoResponse.Create;
  Result.Protocolo := aProtocolo;
  Result.Lote := aNumLote;

  PrepararConsultaSituacao(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultaSituacao(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(Result, tmConsultarSituacao);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmConsultarSituacao);
      AService.Prefixo := Result.Protocolo;
      Result.XmlRetorno := AService.ConsultarSituacao(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);

      Result.Sucesso := True;
      Result.EnvelopeEnvio := AService.Envio;
      Result.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        AErro := Result.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not Result.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultaSituacao(Result);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

function TACBrNFSeXProvider.ConsultaLoteRps(const aProtocolo, aNumLote: String): TNFSeConsultaLoteRpsResponse;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsulta);

  Result := TNFSeConsultaLoteRpsResponse.Create;
  Result.Protocolo := aProtocolo;
  Result.Lote := aNumLote;

  PrepararConsultaLoteRps(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultaLoteRps(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(Result, tmConsultarLote);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmConsultarLote);
      AService.Prefixo := Result.Protocolo;
      Result.XmlRetorno := AService.ConsultarLote(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);

      Result.Sucesso := True;
      Result.EnvelopeEnvio := AService.Envio;
      Result.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        AErro := Result.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not Result.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultaLoteRps(Result);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

function TACBrNFSeXProvider.ConsultaNFSeporRps(const aNumRPS, aSerie, aTipo,
  aCodVerificacao: String): TNFSeConsultaNFSeporRpsResponse;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsulta);

  Result := TNFSeConsultaNFSeporRpsResponse.Create;
  Result.NumRPS := aNumRPS;
  Result.Serie := aSerie;
  Result.Tipo := aTipo;
  Result.CodVerificacao := aCodVerificacao;

  PrepararConsultaNFSeporRps(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultaNFSeporRps(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(Result, tmConsultarNFSePorRps);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmConsultarNFSePorRps);
      AService.Prefixo := Result.NumRPS + Result.Serie;
      Result.XmlRetorno := AService.ConsultarNFSePorRps(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);

      Result.Sucesso := True;
      Result.EnvelopeEnvio := AService.Envio;
      Result.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        AErro := Result.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not Result.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultaNFSeporRps(Result);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

function TACBrNFSeXProvider.ConsultaNFSe(aInfConsultaNFSe: TInfConsultaNFSe): TNFSeConsultaNFSeResponse;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
  Prefixo: string;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeConsulta);

  Result := TNFSeConsultaNFSeResponse.Create;
  Result.InfConsultaNFSe := aInfConsultaNFSe;

  PrepararConsultaNFSe(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarConsultaNFSe(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(Result, Result.Metodo);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);
      AService := CriarServiceClient(Result.Metodo);

      if (Result.InfConsultaNFSe.DataInicial > 0) and (Result.InfConsultaNFSe.DataFinal > 0) then
        Prefixo := FormatDateTime('yyyymmdd', Result.InfConsultaNFSe.DataInicial) +
                   FormatDateTime('yyyymmdd', Result.InfConsultaNFSe.DataFinal)
      else
        Prefixo := FormatFloat('000000000000000', StrToIntDef(Result.InfConsultaNFSe.NumeroIniNFSe, 0)) +
                   FormatFloat('000000000000000', StrToIntDef(Result.InfConsultaNFSe.NumeroFinNFSe, 0)) +
                   FormatFloat('000000', Result.InfConsultaNFSe.Pagina);

      AService.Prefixo := Prefixo;

      case Result.Metodo of
        tmConsultarNFSe:
          Result.XmlRetorno := AService.ConsultarNFSe(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);

        tmConsultarNFSePorFaixa:
          Result.XmlRetorno := AService.ConsultarNFSePorFaixa(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);

        tmConsultarNFSeServicoPrestado:
          Result.XmlRetorno := AService.ConsultarNFSeServicoPrestado(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);

        tmConsultarNFSeServicoTomado:
          Result.XmlRetorno := AService.ConsultarNFSeServicoTomado(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);

      else
        // tmConsultarNFSeURL
        Result.XmlRetorno := AService.ConsultarNFSeUrl(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);
      end;

      Result.Sucesso := True;
      Result.EnvelopeEnvio := AService.Envio;
      Result.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        AErro := Result.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not Result.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoConsultaNFSe(Result);
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
      AErro.Codigo := Cod999;
      AErro.Descricao := E.Message;
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
      AErro.Codigo := Cod999;
      AErro.Descricao := E.Message;
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
      AErro.Codigo := Cod999;
      AErro.Descricao := E.Message;
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
      AErro.Codigo := Cod999;
      AErro.Descricao := E.Message;
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse);
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
      AErro.Codigo := Cod999;
      AErro.Descricao := E.Message;
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
      AErro.Codigo := Cod999;
      AErro.Descricao := E.Message;
    end;
  end;
end;

procedure TACBrNFSeXProvider.AssinarSubstituiNFSe(
  Response: TNFSeSubstituiNFSeResponse);
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
      AErro.Codigo := Cod999;
      AErro.Descricao := E.Message;
    end;
  end;
end;

function TACBrNFSeXProvider.CancelaNFSe(aInfCancelamento: TInfCancelamento): TNFSeCancelaNFSeResponse;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeCancelamento);

  Result := TNFSeCancelaNFSeResponse.Create;
  Result.InfCancelamento := aInfCancelamento;

  PrepararCancelaNFSe(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarCancelaNFSe(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(Result, tmCancelarNFSe);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmCancelarNFSe);

      if Result.InfCancelamento.NumeroNFSe <> '' then
        AService.Prefixo := Result.InfCancelamento.NumeroNFSe
      else
        AService.Prefixo := Result.InfCancelamento.ChaveNFSe;

      Result.XmlRetorno := AService.Cancelar(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);

      Result.Sucesso := True;
      Result.EnvelopeEnvio := AService.Envio;
      Result.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        AErro := Result.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not Result.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoCancelaNFSe(Result);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

function TACBrNFSeXProvider.SubstituiNFSe(const aNumNFSe, aSerieNFSe, aCodCancelamento,
  aMotCancelamento, aNumLote, aCodVerificacao: String): TNFSeSubstituiNFSeResponse;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
  Cancelamento: TNFSeCancelaNFSeResponse;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeSubstituicao);

  Result := TNFSeSubstituiNFSeResponse.Create;

  with Result.InfCancelamento do
  begin
    NumeroNFSe := aNumNFSe;
    SerieNFSe := aSerieNFSe;
    CodCancelamento := aCodCancelamento;
    MotCancelamento := aMotCancelamento;
    NumeroLote := aNumLote;
    CodVerificacao := aCodVerificacao;
  end;

  Cancelamento := TNFSeCancelaNFSeResponse.Create;
  try
    with Cancelamento.InfCancelamento do
    begin
      NumeroNFSe := Result.InfCancelamento.NumeroNFSe;
      SerieNFSe := Result.InfCancelamento.SerieNFSe;
      CodCancelamento := Result.InfCancelamento.CodCancelamento;
      MotCancelamento := Result.InfCancelamento.MotCancelamento;
      NumeroLote := Result.InfCancelamento.NumeroLote;
      CodVerificacao := Result.InfCancelamento.CodVerificacao;
    end;

    PrepararCancelaNFSe(Cancelamento);
    if (Result.Erros.Count > 0) then
    begin
      TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
      Exit;
    end;

    AssinarCancelaNFSe(Cancelamento);
    if (Result.Erros.Count > 0) then
    begin
      TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
      Exit;
    end;

    Result.PedCanc := Cancelamento.XmlEnvio;
    Result.PedCanc := SepararDados(Result.PedCanc, 'CancelarNfseEnvio', False);
  finally
//    Cancelamento := nil;  //italo Não sei se esta correto
    FreeAndNil(Cancelamento);
  end;

  PrepararSubstituiNFSe(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  AssinarSubstituiNFSe(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  ValidarSchema(Result, tmSubstituirNFSe);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmSubstituirNFSe);
      AService.Prefixo := aNumNFSe;
      Result.XmlRetorno := AService.SubstituirNFSe(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);

      Result.Sucesso := True;
      Result.EnvelopeEnvio := AService.Envio;
      Result.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        AErro := Result.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not Result.Sucesso then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
  TratarRetornoSubstituiNFSe(Result);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

end.
