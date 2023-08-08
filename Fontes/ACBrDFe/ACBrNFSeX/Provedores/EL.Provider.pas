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

unit EL.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio, ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type

  TACBrNFSeXWebserviceEL = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function AbrirSessao(ACabecalho, AMSG: String): string; override;
    function FecharSessao(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderEL = class (TACBrNFSeProviderProprio)
  private
    FPHash: string;

  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function GerarXmlNota(const aXmlRps, aXmlRetorno: string): string;

    function AbreSessao(const aLote: String): Boolean;
    function FechaSessao(const aLote: String): Boolean;

    function PrepararRpsParaLote(const aXml: string): string; override;

    procedure PrepararAbrirSessao(Response: TNFSeAbreSessaoResponse);
    procedure TratarRetornoAbrirSessao(Response: TNFSeAbreSessaoResponse);

    procedure PrepararFecharSessao(Response: TNFSeFechaSessaoResponse);
    procedure TratarRetornoFecharSessao(Response: TNFSeFechaSessaoResponse);

    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); override;
    procedure TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); override;

    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;

    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;

    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = '';
                                     const AMessageTag: string = ''); override;

  public
    procedure Emite; override;

    function RegimeEspecialTributacaoToStr(const t: TnfseRegimeEspecialTributacao): string; override;
    function StrToRegimeEspecialTributacao(out ok: boolean; const s: string): TnfseRegimeEspecialTributacao; override;

    function SituacaoTributariaToStr(const t: TnfseSituacaoTributaria): string; override;
    function StrToSituacaoTributaria(out ok: boolean; const s: string): TnfseSituacaoTributaria; override;
    function SituacaoTributariaDescricao(const t: TnfseSituacaoTributaria): string; override;

  end;

  TACBrNFSeXWebserviceEL204 = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderEL204 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  ACBrNFSeXNotasFiscais, EL.GravarXml, EL.LerXml;

{ TACBrNFSeProviderEL204 }

procedure TACBrNFSeProviderEL204.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.ConsultaPorFaixaPreencherNumNfseFinal := True;

  ConfigAssinar.Rps := True;
  ConfigAssinar.LoteRps := True;

  with ConfigWebServices do
  begin
    VersaoDados := '2.04';
    VersaoAtrib := '2.04';
  end;

  with ConfigMsgDados do
  begin
    GerarPrestadorLoteRps := True;
    DadosCabecalho := GetCabecalho('');
  end;
end;

function TACBrNFSeProviderEL204.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_EL204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderEL204.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_EL204.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderEL204.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceEL204.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

{ TACBrNFSeXWebserviceEL204 }

function TACBrNFSeXWebserviceEL204.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRps>';
  Request := Request + '<nfse:RecepcionarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:RecepcionarLoteRpsRequest>';
  Request := Request + '</nfse:RecepcionarLoteRps>';

  Result := Executar('http://nfse.abrasf.org.br/RecepcionarLoteRps', Request,
                     ['RecepcionarLoteRpsResponse', 'outputXML', 'EnviarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceEL204.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRpsSincrono>';
  Request := Request + '<nfse:RecepcionarLoteRpsSincronoRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:RecepcionarLoteRpsSincronoRequest>';
  Request := Request + '</nfse:RecepcionarLoteRpsSincrono>';

  Result := Executar('http://nfse.abrasf.org.br/RecepcionarLoteRpsSincrono', Request,
                     ['RecepcionarLoteRpsSincronoResponse', 'outputXML', 'EnviarLoteRpsSincronoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceEL204.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:GerarNfse>';
  Request := Request + '<nfse:GerarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:GerarNfseRequest>';
  Request := Request + '</nfse:GerarNfse>';

  Result := Executar('http://nfse.abrasf.org.br/GerarNfse', Request,
                     ['GerarNfseResponse', 'outputXML', 'GerarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceEL204.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarLoteRps>';
  Request := Request + '<nfse:ConsultarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarLoteRpsRequest>';
  Request := Request + '</nfse:ConsultarLoteRps>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarLoteRps', Request,
                     ['ConsultarLoteRpsResponse', 'outputXML', 'ConsultarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceEL204.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorFaixa>';
  Request := Request + '<nfse:ConsultarNfsePorFaixaRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfsePorFaixaRequest>';
  Request := Request + '</nfse:ConsultarNfsePorFaixa>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfsePorFaixa', Request,
                     ['ConsultarNfsePorFaixaResponse', 'outputXML', 'ConsultarNfseFaixaResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceEL204.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorRps>';
  Request := Request + '<nfse:ConsultarNfsePorRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfsePorRpsRequest>';
  Request := Request + '</nfse:ConsultarNfsePorRps>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfsePorRps', Request,
                     ['ConsultarNfsePorRpsResponse', 'outputXML', 'ConsultarNfseRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceEL204.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoPrestado>';
  Request := Request + '<nfse:ConsultarNfseServicoPrestadoRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoPrestadoRequest>';
  Request := Request + '</nfse:ConsultarNfseServicoPrestado>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfseServicoPrestado', Request,
                     ['ConsultarNfseServicoPrestadoResponse', 'outputXML', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceEL204.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoTomado>';
  Request := Request + '<nfse:ConsultarNfseServicoTomadoRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:ConsultarNfseServicoTomadoRequest>';
  Request := Request + '</nfse:ConsultarNfseServicoTomado>';

  Result := Executar('http://nfse.abrasf.org.br/ConsultarNfseServicoTomado', Request,
                     ['ConsultarNfseServicoTomadoResponse', 'outputXML', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceEL204.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:CancelarNfse>';
  Request := Request + '<nfse:CancelarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:CancelarNfseRequest>';
  Request := Request + '</nfse:CancelarNfse>';

  Result := Executar('http://nfse.abrasf.org.br/CancelarNfse', Request,
                     ['CancelarNfseResponse', 'outputXML', 'CancelarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceEL204.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:SubstituirNfse>';
  Request := Request + '<nfse:SubstituirNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</nfse:SubstituirNfseRequest>';
  Request := Request + '</nfse:SubstituirNfse>';

  Result := Executar('http://nfse.abrasf.org.br/SubstituirNfse', Request,
                     ['SubstituirNfseResponse', 'outputXML', 'SubstituirNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceEL204.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(AnsiString(Result), True, {$IfDef FPC}True{$Else}False{$EndIf});
  Result := RemoverDeclaracaoXML(Result);
end;

{ TACBrNFSeProviderEL }

function TACBrNFSeProviderEL.AbreSessao(
  const aLote: String): Boolean;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
  AAbreSessao: TNFSeAbreSessaoResponse;
begin
  Result := False;
  TACBrNFSeX(FAOwner).SetStatus(stNFSeAbrirSessao);

  AAbreSessao := TNFSeAbreSessaoResponse.Create;

  try
    AAbreSessao.NumeroLote := aLote;

    PrepararAbrirSessao(AAbreSessao);

    if (AAbreSessao.Erros.Count > 0) then
    begin
      TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
      Exit;
    end;

    AService := nil;

    try
      try
        TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

        AService := CriarServiceClient(tmAbrirSessao);
        AService.Prefixo := AAbreSessao.NumeroLote;
        AAbreSessao.ArquivoRetorno := AService.AbrirSessao(ConfigMsgDados.DadosCabecalho, AAbreSessao.ArquivoEnvio);

        AAbreSessao.Sucesso := True;
        AAbreSessao.EnvelopeEnvio := AService.Envio;
        AAbreSessao.EnvelopeRetorno := AService.Retorno;
      except
        on E:Exception do
        begin
          AErro := EmiteResponse.Erros.New;
          AErro.Codigo := Cod999;
          AErro.Descricao := ACBrStr(Desc999 + E.Message);
        end;
      end;
    finally
      FreeAndNil(AService);
    end;

    if not AAbreSessao.Sucesso then
    begin
      TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
      Exit;
    end;

    TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
    TratarRetornoAbrirSessao(AAbreSessao);
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);

    Result := AAbreSessao.Sucesso;
  finally
    AAbreSessao.Free;
  end;
end;

function TACBrNFSeProviderEL.FechaSessao(const aLote: String): Boolean;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
  AFechaSessao: TNFSeFechaSessaoResponse;
begin
  Result := False;
  TACBrNFSeX(FAOwner).SetStatus(stNFSeFecharSessao);

  AFechaSessao := TNFSeFechaSessaoResponse.Create;

  try
    AFechaSessao.NumeroLote := aLote;
    AFechaSessao.HashIdent := FPHash;

    PrepararFecharSessao(AFechaSessao);

    if (AFechaSessao.Erros.Count > 0) then
    begin
      TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
      Exit;
    end;

    AService := nil;

    try
      try
        TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

        AService := CriarServiceClient(tmFecharSessao);
        AService.Prefixo := AFechaSessao.NumeroLote;
        AFechaSessao.ArquivoRetorno := AService.FecharSessao(ConfigMsgDados.DadosCabecalho, AFechaSessao.ArquivoEnvio);

        AFechaSessao.Sucesso := True;
        AFechaSessao.EnvelopeEnvio := AService.Envio;
        AFechaSessao.EnvelopeRetorno := AService.Retorno;
      except
        on E:Exception do
        begin
          AErro := EmiteResponse.Erros.New;
          AErro.Codigo := Cod999;
          AErro.Descricao := ACBrStr(Desc999 + E.Message);
        end;
      end;
    finally
      FreeAndNil(AService);
    end;

    if not AFechaSessao.Sucesso then
    begin
      TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
      Exit;
    end;

    TACBrNFSeX(FAOwner).SetStatus(stNFSeAguardaProcesso);
    TratarRetornoFecharSessao(AFechaSessao);
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);

    Result := AFechaSessao.Sucesso;
  finally
    AFechaSessao.Free;
  end;
end;

procedure TACBrNFSeProviderEL.Emite;
begin
  AbreSessao(EmiteResponse.NumeroLote);

  inherited Emite;

  FechaSessao(EmiteResponse.NumeroLote);
end;

function TACBrNFSeProviderEL.PrepararRpsParaLote(const aXml: string): string;
begin
  Result := '<Rps>' + SeparaDados(aXml, 'Rps') + '</Rps>';
end;

procedure TACBrNFSeProviderEL.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := '';
    AErro.Descricao := ACBrStr(ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('mensagens'), tcStr));

    if AErro.Descricao = '' then
      AErro.Descricao := ACBrStr(ANodeArray[I].AsString);

    AErro.Correcao := '';
  end;
end;

function TACBrNFSeProviderEL.RegimeEspecialTributacaoToStr(
  const t: TnfseRegimeEspecialTributacao): string;
begin
  Result := EnumeradoToStr(t,
                       ['0', '1', '2', '3', '4', '5', '6'],
                       [retNenhum, retMicroempresaMunicipal, retEstimativa,
                       retSociedadeProfissionais, retCooperativa,
                       retMicroempresarioIndividual, retMicroempresarioEmpresaPP
                       ]);
end;

function TACBrNFSeProviderEL.SituacaoTributariaDescricao(
  const t: TnfseSituacaoTributaria): string;
begin
  case t of
    stNormal   : Result := 'Não Retido' ;
    stRetencao : Result := 'Retido' ;
  else
    Result := '';
  end;
end;

function TACBrNFSeProviderEL.SituacaoTributariaToStr(
  const t: TnfseSituacaoTributaria): string;
begin
  Result := EnumeradoToStr(t,['1', '2'],
                             [stNormal, stRetencao]);
end;

function TACBrNFSeProviderEL.StrToSituacaoTributaria(out ok: boolean;
  const s: string): TnfseSituacaoTributaria;
begin
  Result := StrToEnumerado(ok, s,
                             ['1', '2'],
                             [stNormal, stRetencao]);
end;

function TACBrNFSeProviderEL.StrToRegimeEspecialTributacao(out ok: boolean;
  const s: string): TnfseRegimeEspecialTributacao;
begin
  Result := StrToEnumerado(ok, s,
                       ['0', '1', '2', '3', '4', '5', '6'],
                       [retNenhum, retMicroempresaMunicipal, retEstimativa,
                       retSociedadeProfissionais, retCooperativa,
                       retMicroempresarioIndividual, retMicroempresarioEmpresaPP
                       ]);
end;

procedure TACBrNFSeProviderEL.PrepararAbrirSessao(
  Response: TNFSeAbreSessaoResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.NumeroLote) then
  begin
    AErro := EmiteResponse.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := ACBrStr(Desc111);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.ArquivoEnvio := '<el:autenticarContribuinte>' +
                             '<identificacaoPrestador>' +
                                OnlyNumber(Emitente.CNPJ) +
                             '</identificacaoPrestador>' +
                             '<senha>' +
                                Emitente.WSSenha +
                             '</senha>' +
                           '</el:autenticarContribuinte>';
end;

procedure TACBrNFSeProviderEL.TratarRetornoAbrirSessao(
  Response: TNFSeAbreSessaoResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := EmiteResponse.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ProcessarMensagemErros(Document.Root, Response, 'return', 'mensagens');

      Response.Sucesso := (Response.Erros.Count = 0);

      FPHash := ObterConteudoTag(Document.Root.Childrens.FindAnyNs('return'), tcStr);
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderEL.PrepararFecharSessao(
  Response: TNFSeFechaSessaoResponse);
begin
  Response.ArquivoEnvio := '<el:finalizarSessao>' +
                             '<hashIdentificador>' +
                                FPHash +
                             '</hashIdentificador>' +
                           '</el:finalizarSessao>';
end;

procedure TACBrNFSeProviderEL.TratarRetornoFecharSessao(
  Response: TNFSeFechaSessaoResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := EmiteResponse.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ProcessarMensagemErros(Document.Root, Response, 'return', 'mensagens');

      Response.Sucesso := (Response.Erros.Count = 0);
    except
      on E:Exception do
      begin
        AErro := EmiteResponse.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderEL.GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
  Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  xTipoDoc, Arquivo: string;
  lote : string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    if Length(OnlyNumber(Emitente.CNPJ)) = 14 then
      xTipoDoc := '2'
    else
      xTipoDoc := '1';

    if Length(Response.NumeroLote) < 13 then
      lote := PadLeft(Response.NumeroLote, 13, '0')
    else
      lote := Response.NumeroLote;


    Arquivo := '<LoteRps xmlns="http://www.el.com.br/nfse/xsd/el-nfse.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema" xsi:schemaLocation="http://www.el.com.br/nfse/xsd/el-nfse.xsd el-nfse.xsd ">' +
                 '<Id>' +
                      lote+
                 '</Id>' +
                 '<NumeroLote>' +
                    Response.NumeroLote +
                 '</NumeroLote>' +
                 '<QuantidadeRps>' +
                    IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
                 '</QuantidadeRps>' +
                 '<IdentificacaoPrestador>' +
                   '<CpfCnpj>' +
                     OnlyNumber(Emitente.CNPJ) +
                   '</CpfCnpj>' +
                   '<IndicacaoCpfCnpj>' +
                     xTipoDoc +
                   '</IndicacaoCpfCnpj>' +
                   '<InscricaoMunicipal>' +
                     OnlyNumber(Emitente.InscMun) +
                   '</InscricaoMunicipal>' +
                 '</IdentificacaoPrestador>' +
                 '<ListaRps>' +
                   Xml +
                 '</ListaRps>' +
               '</LoteRps>';

    Response.ArquivoEnvio := '<el:EnviarLoteRpsEnvio>' +
                               '<identificacaoPrestador>' +
                                  OnlyNumber(Emitente.CNPJ) +
                               '</identificacaoPrestador>' +
                               '<hashIdentificador>' +
                                  FPHash +
                               '</hashIdentificador>' +
                               '<arquivo>' +
                                  IncluirCDATA(Arquivo) +
                               '</arquivo>' +
                             '</el:EnviarLoteRpsEnvio>';
  end;
end;

procedure TACBrNFSeProviderEL.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  AuxNode: TACBrXmlNode;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('return');

      if AuxNode <> nil then
      begin
        ProcessarMensagemErros(ANode, Response, 'return', 'mensagens');

        Response.Sucesso := (Response.Erros.Count = 0);

        with Response do
        begin
          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dataRecebimento'), tcDatHor);
          NumeroLote := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numeroLote'), tcStr);
          Protocolo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numeroProtocolo'), tcStr);
        end;
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderEL.PrepararConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := ACBrStr(Desc101);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.ArquivoEnvio := '<el:ConsultarSituacaoLoteRpsEnvio>' +
                             '<identificacaoPrestador>' +
                                OnlyNumber(Emitente.CNPJ) +
                             '</identificacaoPrestador>' +
                             '<numeroProtocolo>' +
                                Response.Protocolo +
                             '</numeroProtocolo>' +
                           '</el:ConsultarSituacaoLoteRpsEnvio>';
end;

procedure TACBrNFSeProviderEL.TratarRetornoConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  AuxNode: TACBrXmlNode;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, 'return', 'mensagens');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('return');

      if AuxNode <> nil then
      begin
        with Response do
        begin
          NumeroLote := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numeroLote'), tcStr);
          Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacaoLoteRps'), tcStr);
        end;
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderEL.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := ACBrStr(Desc101);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.ArquivoEnvio := '<el:ConsultarLoteRpsEnvio>' +
                             '<identificacaoPrestador>' +
                                OnlyNumber(Emitente.CNPJ) +
                             '</identificacaoPrestador>' +
                             '<numeroProtocolo>' +
                                Response.Protocolo +
                             '</numeroProtocolo>' +
                           '</el:ConsultarLoteRpsEnvio>';
end;

procedure TACBrNFSeProviderEL.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AResumo: TNFSeResumoCollectionItem;
  i, j, k: Integer;
  ANumRps, ANumNfse, ASituacao, AidNota, aXmlNota, aXmlRetorno: string;
  ADataHora: TDateTime;
  ANota: TNotaFiscal;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, 'return', 'mensagens');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('return');

      if (AuxNode <> nil) and (Pos('<notasFiscais>', AuxNode.OuterXml) > 0) then
        AuxNode := AuxNode.Childrens.FindAnyNs('notasFiscais');

      j := TACBrNFSeX(FAOwner).NotasFiscais.Count;

      if AuxNode <> nil then
      begin
        with Response do
        begin
          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dataProcessamento'), tcDatHor);
          idNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('idNota'), tcStr);
          NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numero'), tcStr);
          Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao'), tcStr);
          NumeroRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('rpsNumero'), tcStr);
        end;

        if j > 0 then
        begin
          if AuxNode <> nil then
          begin
            ANodeArray := AuxNode.Childrens.FindAllAnyNs('nfeRpsNotaFiscal');

            if not Assigned(ANodeArray) then
            begin
              // O retorno muitas vezes é apresentado sem o a tag <nfeRpsNotaFiscal>

              if Response.NumeroNota <> '' then
              begin
                AResumo := Response.Resumos.New;
                AResumo.idNota := Response.idNota;
                AResumo.NumeroNota := Response.NumeroNota;
                AResumo.Data := Response.Data;
                AResumo.Situacao :=  Response.Situacao;
                AResumo.NumeroRps := Response.NumeroRps;
              end
              else
              begin
              AErro := Response.Erros.New;
              AErro.Codigo := Cod203;
              AErro.Descricao := ACBrStr(Desc203);
              end;
              Exit;
            end;

            for i := Low(ANodeArray) to High(ANodeArray) do
            begin
              AuxNode := ANodeArray[i];

              if j > 0 then
              begin
                AidNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('idNota'), tcStr);
                ANumNfse := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numero'), tcStr);
                ADataHora := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dataProcessamento'), tcDatHor);
                ASituacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao'), tcStr);
                ANumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('rpsNumero'), tcStr);

                AResumo := Response.Resumos.New;
                AResumo.idNota := AidNota;
                AResumo.NumeroNota := ANumNfse;
                AResumo.Data := ADataHora;
                AResumo.Situacao := ASituacao;
                AResumo.NumeroRps := ANumRps;

                aXmlRetorno := AuxNode.OuterXml;

                for k := 0 to j-1 do
                begin
                  ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[k];

                  if ANota.NFSe.IdentificacaoRps.Numero = ANumRps  then
                  begin
                    if ANota.XmlRps = '' then
                      aXmlNota := GerarXmlNota(ANota.XmlNfse, aXmlRetorno)
                    else
                      aXmlNota := GerarXmlNota(ANota.XmlRps, aXmlRetorno);

                    ANota.XmlNfse := aXmlNota;

                    SalvarXmlNfse(ANota);
                    Exit;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderEL.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.NumeroRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := ACBrStr(Desc102);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.ArquivoEnvio := '<el:ConsultarNfseRpsEnvio>' +
                             '<identificacaoRps>' +
                                Response.NumeroRps +
                             '</identificacaoRps>' +
                             '<identificacaoPrestador>' +
                                OnlyNumber(Emitente.CNPJ) +
                             '</identificacaoPrestador>' +
                           '</el:ConsultarNfseRpsEnvio>';
end;

procedure TACBrNFSeProviderEL.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  AuxNode: TACBrXmlNode;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, 'return', 'mensagens');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('return');

      if (AuxNode <> nil) and (Pos('<nfeRpsNotaFiscal>', AuxNode.OuterXml) > 0) then
        AuxNode := AuxNode.Childrens.FindAnyNs('nfeRpsNotaFiscal');

      if AuxNode <> nil then
      begin
        with Response do
        begin
          // Verificar o que é retornado
          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dataProcessamento'), tcDatHor);
          idNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('idNota'), tcStr);
          NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numero'), tcStr);
          Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao'), tcStr);
        end;
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderEL.PrepararConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.InfConsultaNFSe.NumeroIniNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.Metodo := tmConsultarNFSe;

  Response.ArquivoEnvio := '<el:ConsultarNfseEnvio>' +
                             '<identificacaoPrestador>' +
                                OnlyNumber(Emitente.CNPJ) +
                             '</identificacaoPrestador>' +
                             '<numeroNfse>' +
                                Response.InfConsultaNFSe.NumeroIniNFSe +
                             '</numeroNfse>' +
                           '</el:ConsultarNfseEnvio>';
end;

procedure TACBrNFSeProviderEL.TratarRetornoConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  AuxNode: TACBrXmlNode;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, 'return', 'mensagens');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('return');

      if AuxNode <> nil then
      begin
        with Response do
        begin
          // Verificar o que é retornado
          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dataProcessamento'), tcDatHor);
          idNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('idNota'), tcStr);
          NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numero'), tcStr);
          Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao'), tcStr);
        end;
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderEL.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.ArquivoEnvio := '<el:CancelarNfseEnvio>' +
                             '<identificacaoPrestador>' +
                                OnlyNumber(Emitente.CNPJ) +
                             '</identificacaoPrestador>' +
                             '<numeroNfse>' +
                                Response.InfCancelamento.NumeroNFSe +
                             '</numeroNfse>' +
                           '</el:CancelarNfseEnvio>';
end;

procedure TACBrNFSeProviderEL.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  AuxNode: TACBrXmlNode;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, 'return', 'mensagens');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('return');

      if (AuxNode <> nil) and (Pos('<nfeRpsNotaFiscal>', AuxNode.OuterXml) > 0) then
        AuxNode := AuxNode.Childrens.FindAnyNs('nfeRpsNotaFiscal');

      if AuxNode <> nil then
      begin
        with Response do
        begin
          // Verificar o que é retornado
          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('dataProcessamento'), tcDatHor);
          idNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('idNota'), tcStr);
          NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('numero'), tcStr);
          Situacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('situacao'), tcStr);
        end;
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderEL.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;
    ModoEnvio := meLoteAssincrono;
    NumMaxRpsEnviar := 5;
    DetalharServico := True;
  end;

  SetXmlNameSpace('http://www.el.com.br/nfse/xsd/el-nfse.xsd');

  SetNomeXSD('el-nfse.xsd');
  {
    Apesar de ter o xsd não é possível usa-lo pois esta incompleto, não tem
    a definição dos serviços, somente do LoteRps.
  }
  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderEL.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_EL.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderEL.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_EL.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderEL.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceEL.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

function TACBrNFSeProviderEL.GerarXmlNota(const aXmlRps,
  aXmlRetorno: string): string;
var
  aRPS, aRPSp1, aRPSp2, IDNota, Numero, NumeroRPS, Tipo: string;
begin
  aRPS := SeparaDados(aXmlRps, 'Rps', False);

  aRPSp1 := SeparaDados(aRPS, 'LocalPrestacao', True) +
            SeparaDados(aRPS, 'IssRetido', True) +
            SeparaDados(aRPS, 'DataEmissao', True);

  aRPSp2 := SeparaDados(aRPS, 'DadosPrestador', True) +
            SeparaDados(aRPS, 'DadosTomador', True) +
            SeparaDados(aRPS, 'Servicos', True) +
            SeparaDados(aRPS, 'Valores', True) +
            SeparaDados(aRPS, 'Status', True);

  Tipo := SeparaDados(aRPS, 'Tipo', True);

  IDNota := SeparaDados(aXmlRetorno, 'idNota', False);
  Numero := SeparaDados(aXmlRetorno, 'numero', False);
  NumeroRPS := SeparaDados(aXmlRetorno, 'rpsnumero', False);

  Result := '<notasFiscais xmlns="http://www.el.com.br/nfse/xsd/el-nfse.xsd">' +
              '<Nfse>' +
                '<Id>' + IDNota + '</Id>' +
                aRPSp1 +
                '<IdentificacaoNfse>' +
                  '<Numero>' + Numero + '</Numero>' +
                  '<NumeroRps>' + NumeroRPS + '</NumeroRps>' +
                  '<Serie>NFS-e</Serie>' +
                  Tipo +
                '</IdentificacaoNfse>' +
                aRPSp2 +
              '</Nfse>' +
            '</notasFiscais>';
end;

{ TACBrNFSeXWebserviceEL }

function TACBrNFSeXWebserviceEL.TratarXmlRetornado(const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(AnsiString(Result), True, {$IfDef FPC}True{$Else}False{$EndIf});
  Result := RemoverDeclaracaoXML(Result);
end;

function TACBrNFSeXWebserviceEL.Recepcionar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

function TACBrNFSeXWebserviceEL.AbrirSessao(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

function TACBrNFSeXWebserviceEL.FecharSessao(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

function TACBrNFSeXWebserviceEL.ConsultarSituacao(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

function TACBrNFSeXWebserviceEL.ConsultarLote(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

function TACBrNFSeXWebserviceEL.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

function TACBrNFSeXWebserviceEL.ConsultarNFSe(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

function TACBrNFSeXWebserviceEL.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

end.
