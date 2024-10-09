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

unit ACBrANe.ProviderBase;

interface

uses
  SysUtils, Classes,
  ACBrDFe,
  ACBrANeParametros, ACBrANeInterface, ACBrANe.Classes, ACBrANe.Conversao,
  ACBrANeDocumentos,
  ACBrANe.WebServicesBase, ACBrANe.WebServicesResponse;

type
  TACBrANeProvider = class(TInterfacedObject, IACBrANeProvider)
  private
    FConfigGeral: TConfigGeral;
    FConfigWebServices: TConfigWebServices;
    FConfigMsgDados: TConfigMsgDados;
    FConfigAssinar: TConfigAssinar;
    FConfigSchemas: TConfigSchemas;
    FDefaultNameSpaceURI: string;

    function GetConfigGeral: TConfigGeral;
    function GetConfigWebServices: TConfigWebServices;
    function GetConfigMsgDados: TConfigMsgDados;
    function GetConfigAssinar: TConfigAssinar;
    function GetConfigSchemas: TConfigSchemas;

    function GetEnviarResponse: TANeEnviarResponse;
    function GetConsultarResponse: TANeConsultarResponse;

  protected
    FAOwner: TACBrDFe;
    FPrefixoTS: string;

    procedure Configuracao; virtual;
    procedure CarregarURL; virtual;
    procedure SetNomeXSD(const aNome: string);
    procedure SetXmlNameSpace(const aNameSpace: string);
    procedure SetNameSpaceURI(const aMetodo: TMetodo);
    procedure SalvarXmlANe(aNota: TDocumento);

    function CarregarXmlANe(aNota: TDocumento; const aXml: string): TDocumento;

    function GetWebServiceURL(const AMetodo: TMetodo): string;

//    function CriarGeradorXml(const AANe: TANe): TANeWClass; virtual; abstract;
//    function CriarLeitorXml(const AANe: TANe): TANeRClass; virtual; abstract;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrANeWebservice; virtual; abstract;
    procedure ValidarSchema(Response: TANeWebserviceResponse; aMetodo: TMetodo); virtual;

    //metodos para geração e tratamento dos dados do metodo Enviar
    procedure PrepararEnviar(Response: TANeEnviarResponse); virtual; abstract;
    procedure GerarMsgDadosEnviar(Response: TANeEnviarResponse;
      Params: TANeParamsResponse); virtual; abstract;
    procedure AssinarEnviar(Response: TANeEnviarResponse); virtual;
    procedure TratarRetornoEnviar(Response: TANeEnviarResponse); virtual; abstract;

    //metodos para geração e tratamento dos dados do metodo Consultar
    procedure PrepararConsultar(Response: TANeConsultarResponse); virtual; abstract;
    procedure GerarMsgDadosConsultar(Response: TANeConsultarResponse;
      Params: TANeParamsResponse); virtual; abstract;
    procedure AssinarConsultar(Response: TANeConsultarResponse); virtual;
    procedure TratarRetornoConsultar(Response: TANeConsultarResponse); virtual; abstract;

  public
    constructor Create(AOwner: TACBrDFe);
    destructor Destroy; override;

    function GetSchemaPath: string; virtual;

    function GerarXml(const aANe: TANe; var aXml, aAlerts: string): Boolean; virtual;
    function LerXML(const aXML: string; var aANe: TANe;
      var aXmlTratado: string): Boolean; virtual;

    procedure Enviar; virtual;
    procedure Consultar; virtual;

    property ConfigGeral: TConfigGeral read GetConfigGeral;
    property ConfigWebServices: TConfigWebServices read GetConfigWebServices;
    property ConfigMsgDados: TConfigMsgDados read GetConfigMsgDados;
    property ConfigAssinar: TConfigAssinar read GetConfigAssinar;
    property ConfigSchemas: TConfigSchemas read GetConfigSchemas;

    property EnviarResponse: TANeEnviarResponse read GetEnviarResponse;
    property ConsultarResponse: TANeConsultarResponse read GetConsultarResponse;
  end;

implementation

uses
  IniFiles,
  ACBrConsts,
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.FilesIO, ACBrUtil.XMLHTML,
  ACBrXmlBase, ACBrDFeException,
  ACBrANe, ACBrANeConfiguracoes, ACBrANe.Consts;

{ TACBrANeProvider }

constructor TACBrANeProvider.Create(AOwner: TACBrDFe);
begin
  inherited Create;

  FAOwner := AOwner;
  if not Assigned(FAOwner) then
    raise EACBrDFeException.Create('Componente ACBrANe não informado');

  FConfigGeral := TConfigGeral.Create;
  FConfigWebServices := TConfigWebServices.Create;
  FConfigMsgDados := TConfigMsgDados.Create;
  FConfigAssinar := TConfigAssinar.Create;
  FConfigSchemas := TConfigSchemas.Create;

  Configuracao;
end;

destructor TACBrANeProvider.Destroy;
begin
  FConfigGeral.Free;
  FConfigWebServices.Free;
  FConfigMsgDados.Free;
  FConfigAssinar.Free;
  FConfigSchemas.Free;

  inherited Destroy;
end;

function TACBrANeProvider.GetConfigGeral: TConfigGeral;
begin
  Result := FConfigGeral;
end;

function TACBrANeProvider.GetConfigWebServices: TConfigWebServices;
begin
  Result := FConfigWebServices;
end;

function TACBrANeProvider.GetEnviarResponse: TANeEnviarResponse;
begin
  Result := TACBrANe(FAOwner).Webservice.Enviar;
end;

function TACBrANeProvider.GetConsultarResponse: TANeConsultarResponse;
begin
  Result := TACBrANe(FAOwner).Webservice.Consultar;
end;

function TACBrANeProvider.GetSchemaPath: string;
begin
  with TACBrANe(FAOwner).Configuracoes do
  begin
    Result := PathWithDelim(Result + VersaoANeToStr(Geral.VersaoDF));
  end;
end;

function TACBrANeProvider.GetWebServiceURL(const AMetodo: TMetodo): string;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmEnviar: Result := Enviar;
        tmConsultar: Result := Consultar;
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
        tmEnviar: Result := Enviar;
        tmConsultar: Result := Consultar;
      else
        Result := '';
      end;
    end;
  end;
end;

function TACBrANeProvider.GetConfigMsgDados: TConfigMsgDados;
begin
  Result := FConfigMsgDados;
end;

function TACBrANeProvider.GetConfigAssinar: TConfigAssinar;
begin
  Result := FConfigAssinar;
end;

function TACBrANeProvider.GetConfigSchemas: TConfigSchemas;
begin
  Result := FConfigSchemas;
end;

procedure TACBrANeProvider.Configuracao;
begin
  // Inicializa os parâmetros de configuração: Geral
  with ConfigGeral do
  begin
    UseCertificateHTTP := True;
    UseAuthorizationHeader := False;
    Identificador := 'Id';

    Seguradora := TACBrANe(FAOwner).Configuracoes.Geral.Seguradora;
    Versao := TACBrANe(FAOwner).Configuracoes.Geral.VersaoDF;

    if TACBrANe(FAOwner).Configuracoes.WebServices.AmbienteCodigo = 1 then
      Ambiente := taProducao
    else
      Ambiente := taHomologacao;
  end;

  // Inicializa os parâmetros de configuração: MsgDados
  with ConfigMsgDados do
  begin
    Enviar.xmlns := '';
    Enviar.InfElemento := '';
    Enviar.DocElemento := '';

    Consultar.xmlns := '';
    Consultar.InfElemento := '';
    Consultar.DocElemento := '';;
  end;

  // Inicializa os parâmetros de configuração: Assinar
  with ConfigAssinar do
  begin
    Enviar := False;
    Consultar := False;
  end;

  SetNomeXSD('ANe.xsd');

  ConfigWebServices.AtribVerLote := '';

  CarregarURL;
end;

procedure TACBrANeProvider.CarregarURL;
var
  IniParams: TMemIniFile;
  Sessao: string;
begin
  IniParams := TMemIniFile.Create('');

  with TACBrANe(FAOwner) do
  begin
    IniParams.SetStrings(Configuracoes.WebServices.Params);
  end;

  try
    with TACBrANe(FAOwner) do
    begin
      Sessao := Configuracoes.Geral.xSeguradora;

      // Primeiro verifica as URLs definidas para a cidade
      ConfigWebServices.LoadUrlProducao(IniParams, Sessao);
      ConfigWebServices.LoadUrlHomologacao(IniParams, Sessao);
      ConfigWebServices.LoadLinkUrlProducao(IniParams, Sessao);
      ConfigWebServices.LoadLinkUrlHomologacao(IniParams, Sessao);
      ConfigWebServices.LoadXMLNameSpaceProducao(IniParams, Sessao);
      ConfigWebServices.LoadXMLNameSpaceHomologacao(IniParams, Sessao);
      ConfigWebServices.LoadNameSpaceProducao(IniParams, Sessao);
      ConfigWebServices.LoadNameSpaceHomologacao(IniParams, Sessao);
      ConfigWebServices.LoadSoapActionProducao(IniParams, Sessao);
      ConfigWebServices.LoadSoapActionHomologacao(IniParams, Sessao);

      ConfigGeral.LoadParams(IniParams, Sessao);
    end;
  finally
    IniParams.Free;
  end;
end;

function TACBrANeProvider.CarregarXmlANe(aNota: TDocumento; const aXml: string): TDocumento;
begin
  if Assigned(ANota) then
    ANota.Xml := aXml
  else
  begin
    TACBrANe(FAOwner).Documentos.LoadFromString(aXml, False);
    ANota := TACBrANe(FAOwner).Documentos.Items[TACBrANe(FAOwner).Documentos.Count-1];
  end;

  Result := aNota;
end;

procedure TACBrANeProvider.SalvarXmlANe(aNota: TDocumento);
var
  aPath, aNomeArq, Extensao: string;
  aConfig: TConfiguracoesANe;
  ConteudoEhXml: Boolean;
begin
  aConfig := TConfiguracoesANe(FAOwner.Configuracoes);

  aPath := aConfig.Arquivos.GetPathANe(0, aConfig.Geral.CNPJEmitente, '');

//  aNomeArq := TACBrANe(FAOwner).GetNumID(aNota.ANe) + '-ANe.xml';
  aNota.NomeArq := PathWithDelim(aPath) + aNomeArq;
  aNota.Confirmada := True;

  if FAOwner.Configuracoes.Arquivos.Salvar then
  begin
    Extensao := '.xml';

    ConteudoEhXml := True;

    if not ConteudoEhXml then
      aNota.NomeArq := StringReplace(aNota.NomeArq, '.xml', Extensao, [rfReplaceAll]);

    TACBrANe(FAOwner).Gravar(aNota.NomeArq, aNota.Xml, '', ConteudoEhXml);
  end;
end;

procedure TACBrANeProvider.SetNameSpaceURI(const aMetodo: TMetodo);
var
  xNameSpaceURI: string;
begin
  case aMetodo of
    tmEnviar: xNameSpaceURI := ConfigMsgDados.Enviar.xmlns;
    tmConsultar: xNameSpaceURI := ConfigMsgDados.Consultar.xmlns;
  else
    xNameSpaceURI := FDefaultNameSpaceURI;
  end;
  TACBrANe(FAOwner).SSL.NameSpaceURI := xNameSpaceURI
end;

procedure TACBrANeProvider.SetNomeXSD(const aNome: string);
begin
  with ConfigSchemas do
  begin
    Enviar := aNome;
    Consultar := aNome;

    Validar := True;
  end;
end;

procedure TACBrANeProvider.SetXmlNameSpace(const aNameSpace: string);
begin
  with ConfigMsgDados do
  begin
    Enviar.xmlns := aNameSpace;
    Consultar.xmlns := aNameSpace;
  end;

  FDefaultNameSpaceURI := aNameSpace;
end;

function TACBrANeProvider.GerarXml(const aANe: TANe; var aXml,
  aAlerts: string): Boolean;
//var
//  AWriter: TANeWClass;
begin
{
  AWriter := CriarGeradorXml(aANe);

  try
    with TACBrANe(FAOwner) do
    begin
      if Configuracoes.WebServices.AmbienteCodigo = 1 then
        AWriter.Ambiente := taProducao
      else
        AWriter.Ambiente := taHomologacao;

      AWriter.Usuario      := Configuracoes.Geral.Usuario;
      AWriter.Senha        := Configuracoes.Geral.Senha;
      AWriter.Seguradora   := Configuracoes.Geral.Seguradora;
      AWriter.VersaoANe    := Configuracoes.Geral.VersaoDF;

      TimeZoneConf.Assign( Configuracoes.WebServices.TimeZoneConf );

      AWriter.Opcoes.FormatoAlerta  := Configuracoes.Geral.FormatoAlerta;
      AWriter.Opcoes.RetirarAcentos := Configuracoes.Geral.RetirarAcentos;
      AWriter.Opcoes.RetirarEspacos := Configuracoes.Geral.RetirarEspacos;
      AWriter.Opcoes.IdentarXML     := Configuracoes.Geral.IdentarXML;
    end;

    Result := AWriter.GerarXml;

    aXml := AWriter.ConteudoTxt;

    if aXml = '' then
      aXml := AWriter.Document.Xml;

    aAlerts := ACBrStr(AWriter.ListaDeAlertas.Text);
  finally
    AWriter.Destroy;
  end;
  }
end;

function TACBrANeProvider.LerXML(const aXML: string; var aANe: TANe;
  var aXmlTratado: string): Boolean;
//var
//  AReader: TANeRClass;
begin
{
  AReader := CriarLeitorXml(aANe);
  AReader.Arquivo := aXML;

  try
    with TACBrANe(FAOwner) do
    begin
      if Configuracoes.WebServices.AmbienteCodigo = 1 then
        AReader.Ambiente := taProducao
      else
        AReader.Ambiente := taHomologacao;

      AReader.Seguradora := Configuracoes.Geral.Seguradora;
    end;

    Result := AReader.LerXml;
    aXmlTratado := AReader.Arquivo;
  finally
    AReader.Destroy;
  end;
  }
end;

procedure TACBrANeProvider.ValidarSchema(Response: TANeWebserviceResponse; aMetodo: TMetodo);
var
  AErro: TANeEventoCollectionItem;
  Erros, Schema: string;
begin
  if not ConfigSchemas.Validar Then Exit;

  SetNameSpaceURI(aMetodo);

  Erros := '';
  Schema := '';

  case aMetodo of
    tmEnviar: Schema := ConfigSchemas.Enviar;
    tmConsultar: Schema := ConfigSchemas.Consultar;
  else
    Schema := '';
  end;

  if Schema = '***' then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod001;
    AErro.Descricao := ACBrStr(Desc001);
    Exit;
  end;

//  if TACBrANe(FAOwner).Configuracoes.Geral.MontarPathSchema then
//    Schema := PathWithDelim(GetSchemaPath) + Schema
//  else
    Schema := FAOwner.Configuracoes.Arquivos.PathSchemas + Schema;

  FAOwner.SSL.Validar(Response.ArquivoEnvio, Schema, Erros);

  if NaoEstaVazio(Erros) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod800;
    AErro.Descricao := ACBrStr(Desc800 + Erros);
  end;
end;

procedure TACBrANeProvider.Enviar;
var
  AService: TACBrANeWebservice;
  AErro: TANeEventoCollectionItem;
begin
  EnviarResponse.Erros.Clear;
  EnviarResponse.Alertas.Clear;
  EnviarResponse.Resumos.Clear;

  TACBrANe(FAOwner).SetStatus(stANeEnviar);

  PrepararEnviar(EnviarResponse);
  if (EnviarResponse.Erros.Count > 0) then
  begin
    TACBrANe(FAOwner).SetStatus(stANeIdle);
    Exit;
  end;

  AssinarEnviar(EnviarResponse);
  if (EnviarResponse.Erros.Count > 0) then
  begin
    TACBrANe(FAOwner).SetStatus(stANeIdle);
    Exit;
  end;

  ValidarSchema(EnviarResponse, tmEnviar);

  if (EnviarResponse.Erros.Count > 0) then
  begin
    TACBrANe(FAOwner).SetStatus(stANeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrANe(FAOwner).SetStatus(stANeEnviar);

      AService := CriarServiceClient(tmEnviar);
      AService.Prefixo := EnviarResponse.Numero;
      EnviarResponse.ArquivoRetorno := AService.Enviar(ConfigMsgDados.DadosCabecalho, EnviarResponse.ArquivoEnvio);

      EnviarResponse.Sucesso := True;
      EnviarResponse.EnvelopeEnvio := AService.Envio;
      EnviarResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          EnviarResponse.EnvelopeEnvio := AService.Envio;
          EnviarResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := EnviarResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not EnviarResponse.Sucesso then
  begin
    TACBrANe(FAOwner).SetStatus(stANeIdle);
    Exit;
  end;

  TACBrANe(FAOwner).SetStatus(stANeAguardaProcesso);
  TratarRetornoEnviar(EnviarResponse);
  TACBrANe(FAOwner).SetStatus(stANeIdle);
end;

procedure TACBrANeProvider.Consultar;
var
  AService: TACBrANeWebservice;
  AErro: TANeEventoCollectionItem;
begin
  ConsultarResponse.Erros.Clear;
  ConsultarResponse.Alertas.Clear;
  ConsultarResponse.Resumos.Clear;

  TACBrANe(FAOwner).SetStatus(stANeConsultar);

  PrepararConsultar(ConsultarResponse);
  if (ConsultarResponse.Erros.Count > 0) then
  begin
    TACBrANe(FAOwner).SetStatus(stANeIdle);
    Exit;
  end;

  AssinarConsultar(ConsultarResponse);
  if (ConsultarResponse.Erros.Count > 0) then
  begin
    TACBrANe(FAOwner).SetStatus(stANeIdle);
    Exit;
  end;

  ValidarSchema(ConsultarResponse, tmConsultar);
  if (ConsultarResponse.Erros.Count > 0) then
  begin
    TACBrANe(FAOwner).SetStatus(stANeIdle);
    Exit;
  end;

  AService := nil;

  try
    try
      TACBrANe(FAOwner).SetStatus(stANeAguardaProcesso);

      AService := CriarServiceClient(tmConsultar);
      AService.Prefixo := ConsultarResponse.Protocolo;
      ConsultarResponse.ArquivoRetorno := AService.Consultar(ConfigMsgDados.DadosCabecalho,
                                                             ConsultarResponse.ArquivoEnvio);

      ConsultarResponse.Sucesso := True;
      ConsultarResponse.EnvelopeEnvio := AService.Envio;
      ConsultarResponse.EnvelopeRetorno := AService.Retorno;
    except
      on E:Exception do
      begin
        if AService <> nil then
        begin
          ConsultarResponse.EnvelopeEnvio := AService.Envio;
          ConsultarResponse.EnvelopeRetorno := AService.Retorno;
        end;

        AErro := ConsultarResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(AService);
  end;

  if not ConsultarResponse.Sucesso then
  begin
    TACBrANe(FAOwner).SetStatus(stANeIdle);
    Exit;
  end;

  TACBrANe(FAOwner).SetStatus(stANeAguardaProcesso);
  TratarRetornoConsultar(ConsultarResponse);
  TACBrANe(FAOwner).SetStatus(stANeIdle);
end;

procedure TACBrANeProvider.AssinarEnviar(Response: TANeEnviarResponse);
var
  IdAttr, Prefixo, PrefixoTS: string;
  AErro: TANeEventoCollectionItem;
begin
  if not ConfigAssinar.Enviar then Exit;

//  if ConfigAssinar.IncluirURI then
//    IdAttr := ConfigGeral.Identificador
//  else
//    IdAttr := 'ID';

//  if ConfigMsgDados.Prefixo = '' then
//    Prefixo := ''
//  else
//    Prefixo := ConfigMsgDados.Prefixo + ':';

//  if ConfigMsgDados.PrefixoTS = '' then
//    PrefixoTS := ''
//  else
//    PrefixoTS := ConfigMsgDados.PrefixoTS + ':';

  try
    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
        Prefixo + ConfigMsgDados.Enviar.DocElemento,
        PrefixoTS + ConfigMsgDados.Enviar.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

procedure TACBrANeProvider.AssinarConsultar(Response: TANeConsultarResponse);
var
  IdAttr, Prefixo: string;
  AErro: TANeEventoCollectionItem;
begin
  if not ConfigAssinar.Consultar then Exit;

//  if ConfigAssinar.IncluirURI then
//    IdAttr := ConfigGeral.Identificador
//  else
//    IdAttr := 'ID';

//  if ConfigMsgDados.Prefixo = '' then
//    Prefixo := ''
//  else
//    Prefixo := ConfigMsgDados.Prefixo + ':';

  try
    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.Consultar.DocElemento,
      ConfigMsgDados.Consultar.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

end.
