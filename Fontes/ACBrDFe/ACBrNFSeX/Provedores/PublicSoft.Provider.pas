{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2023 Daniel Simoes de Almeida               }
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

unit PublicSoft.Provider;

interface

uses
  SysUtils, Classes,
  ACBrBase, ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXClass, ACBrNFSeXConversao, ACBrNFSeXConsts,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2, ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebservicePublicSoft203 = class(TACBrNFSeXWebserviceRest)
  private
    FSetHeaderTokenPrefeitura: Boolean;

  protected
    procedure SetHeaders(aHeaderReq: THTTPHeader); override;
//    function GerarXmlHeader: string;
  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(const ACabecalho, AMSG: String): string; override;
    function GerarNFSe(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(const ACabecalho, AMSG: String): string; override;
    function GerarToken(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderPublicSoft203 = class (TACBrNFSeProviderABRASFv2)
  private
    FpPath: string;
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); override;

    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure PrepararConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse); override;
    procedure PrepararConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse); override;
    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); override;

    procedure PrepararGerarToken(Response: TNFSeGerarTokenResponse); override;
    procedure TratarRetornoGerarToken(Response: TNFSeGerarTokenResponse); override;

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;
  end;

implementation

uses
  ACBrDFeException,
  ACBrUtil.XMLHTML, ACBrUtil.Strings, ACBrUtil.FilesIO,
  ACBrNFSeX,
  ACBrNFSeXConfiguracoes,
  PublicSoft.GravarXml, PublicSoft.LerXml;

{ TACBrNFSeProviderPublicSoft203 }

procedure TACBrNFSeProviderPublicSoft203.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.Autenticacao.RequerLogin := True;
  ConfigGeral.Autenticacao.RequerChaveAcesso := True;

  ConfigGeral.ServicosDisponibilizados.GerarToken := True;

  with ConfigAssinar do
  begin
    Rps               := True;
    LoteRps           := True;
    CancelarNFSe      := True;
    RpsGerarNFSe      := True;
    RpsSubstituirNFSe := True;
    SubstituirNFSe    := True;
  end;

  ConfigSchemas.Validar := False;
  (*
  with ConfigGeral do
  begin
    ConsultaLote := True;
    ConsultaNFSe := True;

    ConsultaPorFaixa := True;
    ConsultaPorFaixaPreencherNumNfseFinal := False;

    CancPreencherMotivo := False;
    CancPreencherSerieNfse := False;
    CancPreencherCodVerificacao := False;
  end;

  with ConfigMsgDados do
  begin
    UsarNumLoteConsLote := False;
  end;
  *)
end;

function TACBrNFSeProviderPublicSoft203.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_PublicSoft203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderPublicSoft203.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_PublicSoft203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderPublicSoft203.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
  begin
    URL := URL + FpPath;
    Result := TACBrNFSeXWebservicePublicSoft203.Create(FAOwner, AMetodo, URL,
      'POST', 'application/xml');
  end
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderPublicSoft203.PrepararEmitir(
  Response: TNFSeEmiteResponse);
begin
  inherited PrepararEmitir(Response);

  case Response.ModoEnvio of
    meLoteSincrono:
      FpPath := '/api/enviar-lote-rps-sincrono-envio';

    meUnitario:
      FpPath := '/api/gerar-nfse-envio';
  else
    FpPath := '/api/enviar-lote-rps-envio';
  end;
end;

procedure TACBrNFSeProviderPublicSoft203.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Prestador: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    if Response.ModoEnvio in [meLoteAssincrono, meLoteSincrono, meTeste] then
    begin
      if ConfigMsgDados.GerarPrestadorLoteRps then
      begin
        Prestador := '<' + Prefixo2 + 'Prestador>' +
                       '<' + Prefixo2 + 'CpfCnpj>' +
                         GetCpfCnpj(Emitente.CNPJ, Prefixo2) +
                       '</' + Prefixo2 + 'CpfCnpj>' +
                       GetInscMunic(Emitente.InscMun, Prefixo2) +
                     '</' + Prefixo2 + 'Prestador>'
      end
      else
        Prestador := '<' + Prefixo2 + 'CpfCnpj>' +
                       GetCpfCnpj(Emitente.CNPJ, Prefixo2) +
                     '</' + Prefixo2 + 'CpfCnpj>' +
                     GetInscMunic(Emitente.InscMun, Prefixo2);

      Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + ' ' + NameSpace + '>' +
                             '<' + Prefixo + 'LoteRps' + Versao + '>' +
                               '<' + Prefixo2 + 'NumeroLote>' +
                                  Response.NumeroLote +
                               '</' + Prefixo2 + 'NumeroLote>' +
                               Prestador +
                               '<' + Prefixo2 + 'QuantidadeRps>' +
                                  IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
                               '</' + Prefixo2 + 'QuantidadeRps>' +
                               '<' + Prefixo2 + 'ListaRps>' +
                                  Xml +
                               '</' + Prefixo2 + 'ListaRps>' +
                             '</' + Prefixo + 'LoteRps>' +
                           '</' + Prefixo + TagEnvio + '>';
    end
    else
      Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + '>' +
                              Xml +
                           '</' + Prefixo + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderPublicSoft203.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
begin
  inherited PrepararConsultaLoteRps(Response);

  FpPath := '/api/consultar-lote-rps-envio';
end;

procedure TACBrNFSeProviderPublicSoft203.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
begin
  inherited PrepararConsultaNFSeporRps(Response);

  FpPath := '/api/consultar-nfse-rps-envio';
end;

procedure TACBrNFSeProviderPublicSoft203.PrepararConsultaNFSeServicoPrestado(
  Response: TNFSeConsultaNFSeResponse);
begin
  inherited PrepararConsultaNFSeServicoPrestado(Response);

  FpPath := '/api/consultar-nfse-servico-prestado-envio';
end;

procedure TACBrNFSeProviderPublicSoft203.PrepararConsultaNFSeServicoTomado(
  Response: TNFSeConsultaNFSeResponse);
begin
  inherited PrepararConsultaNFSeServicoTomado(Response);

  FpPath := '/api/consultar-nfse-servico-tomado-envio';
end;

procedure TACBrNFSeProviderPublicSoft203.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
begin
  inherited PrepararCancelaNFSe(Response);

  FpPath := '/api/cancelar-nfse-envio';
end;

procedure TACBrNFSeProviderPublicSoft203.PrepararSubstituiNFSe(
  Response: TNFSeSubstituiNFSeResponse);
begin
  inherited PrepararSubstituiNFSe(Response);

  FpPath := '/api/substituir-nfse-envio';
end;

procedure TACBrNFSeProviderPublicSoft203.PrepararGerarToken(
  Response: TNFSeGerarTokenResponse);
begin
  Response.Clear;

  Response.ArquivoEnvio := '';
  FpPath := '/api/gerar-token';
end;

procedure TACBrNFSeProviderPublicSoft203.TratarRetornoGerarToken(
  Response: TNFSeGerarTokenResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  xRetorno: string;
begin
  xRetorno := SeparaDados(Response.ArquivoRetorno, 'Mensagem');

  if Pos('erro', xRetorno) > 0 then
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

        ProcessarMensagemErros(ANode, Response);

        Response.Sucesso := (Response.Erros.Count = 0);
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
  end
  else
    Response.Token := xRetorno;
end;

procedure TACBrNFSeProviderPublicSoft203.ValidarSchema(
  Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
begin
  if aMetodo <> tmGerarToken then
  begin
    inherited ValidarSchema(Response, aMetodo);
  end;
end;

{ TACBrNFSeXWebservicePublicSoft203 }

{
function TACBrNFSeXWebservicePublicSoft203.GerarXmlHeader: string;
var
  Producao: Boolean;
  aToken, aCodigo: string;
begin
  Producao := (TConfiguracoesNFSe(FPConfiguracoes).WebServices.AmbienteCodigo = 1);

  if FSetHeaderTokenPrefeitura then
    aToken := TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente.WSChaveAcesso
  else
    aToken := TACBrNFSeX(FPDFeOwner).WebService.GerarToken.Token;

  aCodigo := TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente.WSSenha;

  Result := '<producao xsi:type="xsd:boolean">' +
              LowerCase(BoolToStr(Producao, True)) +
            '</producao>' +
            '<token xsi:type="xsd:string">' + aToken + '</token>' +
            '<codigoCidade xsi:type="xsd:string">' +
              aCodigo +
            '</codigoCidade>';
end;
}
procedure TACBrNFSeXWebservicePublicSoft203.SetHeaders(aHeaderReq: THTTPHeader);
var
  aToken: string;
begin
  if FSetHeaderTokenPrefeitura then
    aToken := TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente.WSChaveAcesso
  else
    aToken := TACBrNFSeX(FPDFeOwner).WebService.GerarToken.Token;

  aHeaderReq.AddHeader('codigoCidade',
                    TConfiguracoesNFSe(FPConfiguracoes).Geral.Emitente.WSSenha);

  aHeaderReq.AddHeader('token', aToken);

  if (TConfiguracoesNFSe(FPConfiguracoes).WebServices.AmbienteCodigo = 1) then
    aHeaderReq.AddHeader('producao', 'true');
end;

function TACBrNFSeXWebservicePublicSoft203.Recepcionar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FSetHeaderTokenPrefeitura := False;
  FPMsgOrig := AMSG;

  Request := '<?xml version="1.0" encoding="UTF-8"?>' + AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebservicePublicSoft203.RecepcionarSincrono(const ACabecalho,
  AMSG: String): string;
//var
//  Request, xHeader: string;
begin
  FSetHeaderTokenPrefeitura := False;
  FPMsgOrig := AMSG;
  {
  xHeader := GerarXmlHeader;

  Request := '<urn:EnviarLoteRpsSincronoEnvio soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</urn:EnviarLoteRpsSincronoEnvio>';

  Result := Executar('urn:index.EnviarLoteRpsSincronoEnvio#EnviarLoteRpsSincronoEnvio',
                     Request, xHeader,
                     ['return', 'EnviarLoteRpsResposta'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:urn="urn:index.EnviarLoteRpsSincronoEnvio"']);
  }

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebservicePublicSoft203.GerarNFSe(const ACabecalho,
  AMSG: String): string;
//var
//  Request, xHeader: string;
begin
  FSetHeaderTokenPrefeitura := False;
  FPMsgOrig := AMSG;
  {
  xHeader := GerarXmlHeader;

  Request := '<urn:GerarNfseEnvio soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</urn:GerarNfseEnvio>';

  Result := Executar('urn:index.GerarNfseEnvio#GerarNfseEnvio', Request,
                     xHeader,
                     ['return', 'EnviarLoteRpsResposta'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:urn="urn:index.GerarNfseEnvio"']);
  }

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebservicePublicSoft203.ConsultarLote(const ACabecalho,
  AMSG: String): string;
//var
//  Request, xHeader: string;
begin
  FSetHeaderTokenPrefeitura := False;
  FPMsgOrig := AMSG;
  {
  xHeader := GerarXmlHeader;

  Request := '<urn:ConsultarLoteRpsEnvio soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</urn:ConsultarLoteRpsEnvio>';

  Result := Executar('urn:index.ConsultarLoteRpsEnvio#ConsultarLoteRpsEnvio',
                     Request, xHeader,
                     ['return', 'ConsultarLoteRpsResposta'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:urn="urn:index.ConsultarLoteRpsEnvio"']);
  }

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebservicePublicSoft203.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
//var
//  Request, xHeader: string;
begin
  FSetHeaderTokenPrefeitura := False;
  FPMsgOrig := AMSG;
  {
  xHeader := GerarXmlHeader;

  Request := '<urn:ConsultarNfseRpsEnvio soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</urn:ConsultarNfseRpsEnvio>';

  Result := Executar('urn:index.ConsultarNfseRpsEnvio#ConsultarNfseRpsEnvio',
                     Request, xHeader,
                     ['return', 'ConsultarNfseRpsResposta'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:urn="urn:index.ConsultarNfseRpsEnvio"']);
  }

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebservicePublicSoft203.ConsultarNFSeServicoPrestado(const ACabecalho,
  AMSG: String): string;
//var
//  Request, xHeader: string;
begin
  FSetHeaderTokenPrefeitura := False;
  FPMsgOrig := AMSG;
  {
  xHeader := GerarXmlHeader;

  Request := '<urn:ConsultarNfseServicoPrestadoEnvio soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</urn:ConsultarNfseServicoPrestadoEnvio>';

  Result := Executar('urn:index.ConsultarNfseServicoPrestadoEnvio#ConsultarNfseServicoPrestadoEnvio',
                     Request, xHeader,
                     ['return', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:urn="urn:index.ConsultarNfseServicoPrestadoEnvio"']);
  }

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebservicePublicSoft203.ConsultarNFSeServicoTomado(const ACabecalho,
  AMSG: String): string;
//var
//  Request, xHeader: string;
begin
  FSetHeaderTokenPrefeitura := False;
  FPMsgOrig := AMSG;
  {
  xHeader := GerarXmlHeader;

  Request := '<urn:ConsultarNfseServicoTomadoEnvio soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</urn:ConsultarNfseServicoTomadoEnvio>';

  Result := Executar('urn:index.ConsultarNfseServicoTomadoEnvio#ConsultarNfseServicoTomadoEnvio',
                     Request, xHeader,
                     ['return', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:urn="urn:index.ConsultarNfseServicoTomadoEnvio"']);
  }

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebservicePublicSoft203.Cancelar(const ACabecalho, AMSG: String): string;
//var
//  Request, xHeader: string;
begin
  FSetHeaderTokenPrefeitura := False;
  FPMsgOrig := AMSG;
  {
  xHeader := GerarXmlHeader;

  Request := '<urn:CancelarNfseEnvio soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</urn:CancelarNfseEnvio>';

  Result := Executar('urn:index.CancelarNfseEnvio#CancelarNfseEnvio',
                     Request, xHeader,
                     ['return', 'CancelarNfseResposta'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:urn="urn:index.CancelarNfseEnvio"']);
  }

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebservicePublicSoft203.SubstituirNFSe(const ACabecalho,
  AMSG: String): string;
//var
//  Request, xHeader: string;
begin
  FSetHeaderTokenPrefeitura := False;
  FPMsgOrig := AMSG;
  {
  xHeader := GerarXmlHeader;

  Request := '<urn:SubstituirNfseEnvio soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/">';
  Request := Request + '<xml>' + XmlToStr(AMSG) + '</xml>';
  Request := Request + '</urn:SubstituirNfseEnvio>';

  Result := Executar('urn:index.SubstituirNfseEnvio#SubstituirNfseEnvio',
                     Request, xHeader,
                     ['return', 'SubstituirNfseResposta'],
                     ['xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"',
                      'xmlns:urn="urn:index.SubstituirNfseEnvio"']);
  }

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebservicePublicSoft203.GerarToken(const ACabecalho,
  AMSG: String): string;
begin
  FSetHeaderTokenPrefeitura := True;
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [], []);
end;

function TACBrNFSeXWebservicePublicSoft203.TratarXmlRetornado(
  const aXML: string): string;
begin
  if StringIsXML(aXML) then
  begin
    Result := inherited TratarXmlRetornado(aXML);

    Result := ParseText(Result);
    Result := RemoverDeclaracaoXML(Result);
    Result := RemoverIdentacao(Result);
    Result := RemoverCaracteresDesnecessarios(Result);
    Result := RemoverPrefixosDesnecessarios(Result);
  end
  else
  begin
    Result := '<a>' +
                '<ListaMensagemRetorno>' +
                  '<MensagemRetorno>' +
                    '<Codigo>' + '</Codigo>' +
                    '<Mensagem>' + aXML + '</Mensagem>' +
                    '<Correcao>' + '</Correcao>' +
                  '</MensagemRetorno>' +
                '</ListaMensagemRetorno>' +
              '</a>';

    Result := ParseText(Result);
  end;
end;

end.
