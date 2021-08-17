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
  end;

  TACBrNFSeProviderEL = class (TACBrNFSeProviderProprio)
  private
    FPHash: string;

  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function AbreSessao(const aLote: String): TNFSeAbreSessaoResponse;
    function FechaSessao(const aLote: String): TNFSeFechaSessaoResponse;

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

    procedure ProcessarMensagemErros(const RootNode: TACBrXmlNode;
                                     const Response: TNFSeWebserviceResponse;
                                     AListTag: string = '';
                                     AMessageTag: string = ''); override;

  public
    function Emite(const aLote: String; aModoEnvio: TmodoEnvio): TNFSeEmiteResponse; override;
  end;

  TACBrNFSeXWebserviceELv2 = class(TACBrNFSeXWebserviceSoap11)
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

  end;

  TACBrNFSeProviderELv2 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  ACBrNFSeXNotasFiscais, EL.GravarXml, EL.LerXml;

{ TACBrNFSeProviderELv2 }

procedure TACBrNFSeProviderELv2.Configuracao;
begin
  inherited Configuracao;

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

function TACBrNFSeProviderELv2.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_ELv2.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderELv2.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_ELv2.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderELv2.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceELv2.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

{ TACBrNFSeXWebserviceELv2 }

function TACBrNFSeXWebserviceELv2.Recepcionar(ACabecalho,
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

function TACBrNFSeXWebserviceELv2.RecepcionarSincrono(ACabecalho,
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

function TACBrNFSeXWebserviceELv2.GerarNFSe(ACabecalho,
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

function TACBrNFSeXWebserviceELv2.ConsultarLote(ACabecalho,
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

function TACBrNFSeXWebserviceELv2.ConsultarNFSePorFaixa(ACabecalho,
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

function TACBrNFSeXWebserviceELv2.ConsultarNFSePorRps(ACabecalho,
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

function TACBrNFSeXWebserviceELv2.ConsultarNFSeServicoPrestado(ACabecalho,
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

function TACBrNFSeXWebserviceELv2.ConsultarNFSeServicoTomado(ACabecalho,
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

function TACBrNFSeXWebserviceELv2.Cancelar(ACabecalho, AMSG: String): string;
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

function TACBrNFSeXWebserviceELv2.SubstituirNFSe(ACabecalho,
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

{ TACBrNFSeProviderEL }

function TACBrNFSeProviderEL.AbreSessao(
  const aLote: String): TNFSeAbreSessaoResponse;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeAbrirSessao);

  Result := TNFSeAbreSessaoResponse.Create;
  Result.Lote := aLote;

  PrepararAbrirSessao(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmAbrirSessao);
      AService.Prefixo := Result.Lote;
      Result.XmlRetorno := AService.AbrirSessao(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);

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
  TratarRetornoAbrirSessao(Result);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

function TACBrNFSeProviderEL.FechaSessao(const aLote: String): TNFSeFechaSessaoResponse;
var
  AService: TACBrNFSeXWebservice;
  AErro: TNFSeEventoCollectionItem;
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeFecharSessao);

  Result := TNFSeFechaSessaoResponse.Create;
  Result.Lote := aLote;
  Result.HashIdent := FPHash;

  PrepararFecharSessao(Result);
  if (Result.Erros.Count > 0) then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    Exit;
  end;

  try
    try
      TACBrNFSeX(FAOwner).SetStatus(stNFSeEnvioWebService);

      AService := CriarServiceClient(tmFecharSessao);
      AService.Prefixo := Result.Lote;
      Result.XmlRetorno := AService.FecharSessao(ConfigMsgDados.DadosCabecalho, Result.XmlEnvio);

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
  TratarRetornoFecharSessao(Result);
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
end;

function TACBrNFSeProviderEL.Emite(const aLote: String;
  aModoEnvio: TmodoEnvio): TNFSeEmiteResponse;
begin
  AbreSessao(aLote);

  Result := inherited Emite(aLote, aModoEnvio);

  FechaSessao(aLote);
end;

function TACBrNFSeProviderEL.PrepararRpsParaLote(const aXml: string): string;
begin
  Result := '<Rps>' + SeparaDados(aXml, 'Rps') + '</Rps>';
end;

procedure TACBrNFSeProviderEL.ProcessarMensagemErros(
  const RootNode: TACBrXmlNode; const Response: TNFSeWebserviceResponse;
  AListTag, AMessageTag: string);
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
    AErro.Descricao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('mensagens'), tcStr);

    if AErro.Descricao = '' then
      AErro.Descricao := ANodeArray[I].AsString;

    AErro.Correcao := '';
  end;
end;

procedure TACBrNFSeProviderEL.PrepararAbrirSessao(
  Response: TNFSeAbreSessaoResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.Lote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := Desc111;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.XmlEnvio := '<el:autenticarContribuinte>' +
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
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response, '', 'mensagens');

      Response.Sucesso := (Response.Erros.Count = 0);

      FPHash := ProcessarConteudoXml(Document.Root.Childrens.FindAnyNs('return'), tcStr);
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderEL.PrepararFecharSessao(
  Response: TNFSeFechaSessaoResponse);
begin
  Response.XmlEnvio := '<el:finalizarSessao>' +
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
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response, '', 'mensagens');

      Response.Sucesso := (Response.Erros.Count = 0);
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
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
  xTipoDoc: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    if Length(OnlyNumber(Emitente.CNPJ)) = 14 then
      xTipoDoc := '2'
    else
      xTipoDoc := '1';

    Response.XmlEnvio := '<el:EnviarLoteRpsEnvio>' +
                           '<identificacaoPrestador>' +
                              OnlyNumber(Emitente.CNPJ) +
                           '</identificacaoPrestador>' +
                           '<hashIdentificador>' +
                              FPHash +
                           '</hashIdentificador>' +
                           '<arquivo>' +
                             '<LoteRps>' +
                               '<Id>' +
                                  Response.Lote +
                               '</Id>' +
                               '<NumeroLote>' +
                                  Response.Lote +
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
                             '</LoteRps>' +
                           '</arquivo>' +
                         '</el:EnviarLoteRpsEnvio>';
  end;
end;

procedure TACBrNFSeProviderEL.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
//  ANode: TACBrXmlNode;
//  AuxNode, AuxNodeCPFCNPJ, AuxNodeChave: TACBrXmlNode;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response, '', 'mensagens');

      Response.Sucesso := (Response.Erros.Count = 0);
      {
      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('Sucesso'), tcStr);

          AuxNode := AuxNode.Childrens.FindAnyNs('InformacoesLote');

          if AuxNode <> nil then
          begin
            with InformacoesLote do
            begin
              NumeroLote := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('NumeroLote'), tcStr);
              InscricaoPrestador := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('InscricaoPrestador'), tcStr);

              AuxNodeCPFCNPJ := AuxNode.Childrens.FindAnyNs('CPFCNPJRemetente');

              if AuxNodeCPFCNPJ <> nil then
              begin
                CPFCNPJRemetente := ProcessarConteudoXml(AuxNodeCPFCNPJ.Childrens.FindAnyNs('CNPJ'), tcStr);

                if CPFCNPJRemetente = '' then
                  CPFCNPJRemetente := ProcessarConteudoXml(AuxNodeCPFCNPJ.Childrens.FindAnyNs('CPF'), tcStr);
              end;

              DataEnvioLote := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('DataEnvioLote'), tcDatHor);
              QtdNotasProcessadas := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('QtdNotasProcessadas'), tcInt);
              TempoProcessamento := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('TempoProcessamento'), tcInt);
              ValorTotalServico := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('ValorTotalServico'), tcDe2);
            end;
          end;
        end;
      end;

      AuxNode := ANode.Childrens.FindAnyNs('ChaveNFeRPS');

      if AuxNode <> nil then
      begin
        AuxNodeChave := AuxNode.Childrens.FindAnyNs('ChaveRPS');

        if (AuxNodeChave <> nil) then
        begin
          with Response.InfRetorno.ChaveNFeRPS do
          begin
            InscricaoPrestador := ProcessarConteudoXml(AuxNodeChave.Childrens.FindAnyNs('InscricaoPrestador'), tcStr);
            SerieRPS := ProcessarConteudoXml(AuxNodeChave.Childrens.FindAnyNs('SerieRPS'), tcStr);
            NumeroRPS := ProcessarConteudoXml(AuxNodeChave.Childrens.FindAnyNs('NumeroRPS'), tcStr);
          end;
        end;

        AuxNodeChave := AuxNode.Childrens.FindAnyNs('ChaveNFe');

        if (AuxNodeChave <> nil) then
        begin
          with Response.InfRetorno.ChaveNFeRPS do
          begin
            InscricaoPrestador := ProcessarConteudoXml(AuxNodeChave.Childrens.FindAnyNs('InscricaoPrestador'), tcStr);
            Numero := ProcessarConteudoXml(AuxNodeChave.Childrens.FindAnyNs('NumeroNFe'), tcStr);
            CodigoVerificacao := ProcessarConteudoXml(AuxNodeChave.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
          end;
        end;
      end;
      }
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
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
    AErro.Descricao := Desc101;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.XmlEnvio := '<el:ConsultarSituacaoLoteRpsEnvio>' +
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
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response, '', 'mensagens');

      Response.Sucesso := (Response.Erros.Count = 0);
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
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
    AErro.Descricao := Desc101;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.XmlEnvio := '<el:ConsultarLoteRpsEnvio>' +
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
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response, '', 'mensagens');

      Response.Sucesso := (Response.Erros.Count = 0);
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
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
  if EstaVazio(Response.NumRPS) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := Desc102;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.XmlEnvio := '<el:ConsultarNfseRpsEnvio>' +
                         '<identificacaoRps>' +
                            Response.NumRPS +
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
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response, '', 'mensagens');

      Response.Sucesso := (Response.Erros.Count = 0);
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
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
    AErro.Descricao := Desc108;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.Metodo := tmConsultarNFSe;

  Response.XmlEnvio := '<el:ConsultarNfseEnvio>' +
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
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response, '', 'mensagens');

      Response.Sucesso := (Response.Erros.Count = 0);
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
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
    AErro.Descricao := Desc108;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.XmlEnvio := '<el:CancelarNfseEnvio>' +
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
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response, '', 'mensagens');

      Response.Sucesso := (Response.Erros.Count = 0);
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod999;
        AErro.Descricao := E.Message;
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
  end;

  SetXmlNameSpace('http://www.el.com.br/nfse/xsd/el-nfse.xsd');

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
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

{ TACBrNFSeXWebserviceEL }

function TACBrNFSeXWebserviceEL.Recepcionar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, ['return'],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

function TACBrNFSeXWebserviceEL.AbrirSessao(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, ['return'],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

function TACBrNFSeXWebserviceEL.FecharSessao(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, ['return'],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

function TACBrNFSeXWebserviceEL.ConsultarSituacao(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, ['return'],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

function TACBrNFSeXWebserviceEL.ConsultarLote(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, ['return'],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

function TACBrNFSeXWebserviceEL.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, ['return'],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

function TACBrNFSeXWebserviceEL.ConsultarNFSe(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, ['return'],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

function TACBrNFSeXWebserviceEL.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, ['return'],
                     ['xmlns:el="http://des36.el.com.br:8080/el-issonline/"']);
end;

end.
