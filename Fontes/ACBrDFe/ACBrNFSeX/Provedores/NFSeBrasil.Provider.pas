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

unit NFSeBrasil.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml, ACBrNFSeXWebservicesResponse,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceNFSeBrasil = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetDadosUsuario: string;
  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function TesteEnvio(const ACabecalho, AMSG: string): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;

    property DadosUsuario: string read GetDadosUsuario;
  end;

  TACBrNFSeProviderNFSeBrasil = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;

    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;

    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'erros';
                                     const AMessageTag: string = 'erro'); override;

    function MontarMensagemErros(const ARetorno: string): string;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrXmlBase,
  ACBrNFSeXNotasFiscais, NFSeBrasil.GravarXml, NFSeBrasil.LerXml,
  ACBrNFSeXConsts;

const
  encodingStyle = ' soapenv:encodingStyle="http://schemas.xmlsoap.org/soap/encoding/"';
  xsi = 'xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"';

{ TACBrNFSeXWebserviceNFSeBrasil }

function TACBrNFSeXWebserviceNFSeBrasil.GetDadosUsuario: string;
begin
  with TACBrNFSeX(FPDFeOwner).Configuracoes.Geral do
  begin
    Result := '<codMunicipio xsi:type="xsd:string">' +
                 IntToStr(CodigoMunicipio) +
              '</codMunicipio>' +
              '<cnpjPrestador xsi:type="xsd:string">' +
                 OnlyNumber(Emitente.CNPJ) +
              '</cnpjPrestador>' +
              '<hashValidador xsi:type="xsd:string">' +
                 LowerCase(Emitente.WSChaveAcesso) +
              '</hashValidador>';
  end;
end;

function TACBrNFSeXWebserviceNFSeBrasil.Recepcionar(const ACabecalho, AMSG: String): string;
var
  Request, AMSGaux: string;
begin
  FPMsgOrig := AMSG;

  // Tratamento de nomes com &
  AMSGaux := StringReplace(AMSG, '&amp;', '&amp;amp;', [rfReplaceAll]);

  Request := '<urn:tm_lote_rps_service.importarLoteRPS' + encodingStyle +'>';
  Request := Request + '<xml xsi:type="xsd:string">' + XmlToStr(AMSGaux) + '</xml>';
  Request := Request + DadosUsuario;
  Request := Request + '</urn:tm_lote_rps_service.importarLoteRPS>';

  Result := Executar('urn:loterpswsdl#tm_lote_rps_service.importarLoteRPS', Request,
                     ['return', 'RespostaLoteRps'],
                     ['xmlns:urn="urn:loterpswsdl"', xsi]);
end;

function TACBrNFSeXWebserviceNFSeBrasil.ConsultarLote(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:tm_lote_rps_service.consultarLoteRPS' + encodingStyle +'>';
  Request := Request + '<protocolo>' + XmlToStr(AMSG) + '</protocolo>';
  Request := Request + DadosUsuario;
  Request := Request + '</urn:tm_lote_rps_service.consultarLoteRPS>';

  Result := Executar('urn:loterpswsdl#tm_lote_rps_service.consultarLoteRPS', Request,
//                     ['return', 'ConsultarLoteRpsResposta'],
                     ['return'],
                     ['xmlns:urn="urn:loterpswsdl"', xsi]);
end;

function TACBrNFSeXWebserviceNFSeBrasil.ConsultarNFSePorRps(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:tm_lote_rps_service.consultarRPS' + encodingStyle +'>';
  Request := Request + '<numeroRPS>' + XmlToStr(AMSG) + '</numeroRPS>';
  Request := Request + DadosUsuario;
  Request := Request + '</urn:tm_lote_rps_service.consultarRPS>';

  Result := Executar('urn:loterpswsdl#tm_lote_rps_service.consultarRPS', Request,
//                     ['return', 'ConsultarRpsResposta'],
                     ['return'],
                     ['xmlns:urn="urn:loterpswsdl"', xsi]);
end;

function TACBrNFSeXWebserviceNFSeBrasil.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:tm_lote_rps_service.consultarNFSE' + encodingStyle +'>';
  Request := Request + '<numeroNFSE>' + XmlToStr(AMSG) + '</numeroNFSE>';
  Request := Request + DadosUsuario;
  Request := Request + '</urn:tm_lote_rps_service.consultarNFSE>';

  Result := Executar('urn:loterpswsdl#tm_lote_rps_service.consultarNFSE', Request,
                     ['return', 'ConsultarNfseResposta'],
                     ['xmlns:urn="urn:loterpswsdl"', xsi]);
end;

function TACBrNFSeXWebserviceNFSeBrasil.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<urn:tm_lote_rps_service.cancelarNFSE' + encodingStyle +'>';
  Request := Request + '<numeroNFSE>' + XmlToStr(AMSG) + '</numeroNFSE>';
  Request := Request + DadosUsuario;
  Request := Request + '</urn:tm_lote_rps_service.cancelarNFSE>';

  Result := Executar('urn:loterpswsdl#tm_lote_rps_service.cancelarNFSE', Request,
                     ['return', 'ConsultarNfseResposta'],
                     ['xmlns:urn="urn:loterpswsdl"', xsi]);
end;

function TACBrNFSeXWebserviceNFSeBrasil.TesteEnvio(const ACabecalho,
  AMSG: string): string;
var
  Request, AMSGaux: string;
begin
  FPMsgOrig := AMSG;

  // Tratamento de nomes com &
  AMSGaux := StringReplace(AMSG, '&amp;', '&amp;amp;', [rfReplaceAll]);

  Request := '<urn:tm_lote_rps_service.testarLoteRPSRequest' + encodingStyle +'>';
  Request := Request + '<xml xsi:type="xsd:string">' + XmlToStr(AMSGaux) + '</xml>';
  Request := Request + DadosUsuario;
  Request := Request + '</urn:tm_lote_rps_service.testarLoteRPSRequest>';

  Result := Executar('urn:loterpswsdl#tm_lote_rps_service.testarLoteRPS', Request,
                     ['return', 'RespostaLoteRps'],
                     ['xmlns:urn="urn:loterpswsdl"', xsi]);
end;

function TACBrNFSeXWebserviceNFSeBrasil.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := StringReplace(Result, '&amp;amp;', 'e',[rfReplaceAll]);
  Result := ParseText(Result);
  Result := RemoverDeclaracaoXML(Result, True);
  Result := RemoverCDATA(Result);
  Result := RemoverIdentacao(Result);
  Result := RemoverPrefixosDesnecessarios(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
  Result := StringReplace(Result, 'R$', '', [rfReplaceAll]);
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, ']]', '', [rfReplaceAll]);
end;

{ TACBrNFSeProviderNFSeBrasil }

procedure TACBrNFSeProviderNFSeBrasil.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.UseCertificateHTTP := False;
  ConfigGeral.ConsultaSitLote := False;
  ConfigGeral.Autenticacao.RequerCertificado := False;
  ConfigGeral.Autenticacao.RequerChaveAcesso := True;

  with ConfigGeral.ServicosDisponibilizados do
  begin
    ConsultarSituacao := False;
    TestarEnvio := True;
  end;

  ConfigMsgDados.Prefixo := 'xs';

  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 1 then
    SetXmlNameSpace(ConfigWebServices.Producao.XMLNameSpace)
  else
    SetXmlNameSpace(ConfigWebServices.Homologacao.XMLNameSpace);

  ConfigMsgDados.ConsultarNFSe.DocElemento := 'ConsultarNfsePorRpsEnvio';

  ConfigWebServices.AtribVerLote := 'versao="1"';

  SetNomeXSD('rps.xsd');
  // Não tem todos os XSD para todos os serviços
  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderNFSeBrasil.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_NFSeBrasil.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderNFSeBrasil.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_NFSeBrasil.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderNFSeBrasil.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceNFSeBrasil.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderNFSeBrasil.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  aParams: TNFSeParamsResponse;
  Nota: TNotaFiscal;
  Versao, IdAttr, NameSpace, NameSpaceLote, ListaRps, xRps,
  TagEnvio, Prefixo, PrefixoTS: string;
  I: Integer;
begin
  if Response.ModoEnvio in [meLoteSincrono, meUnitario] then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod001;
    AErro.Descricao := ACBrStr(Desc001);
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count <= 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod002;
    AErro.Descricao := ACBrStr(Desc002);
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count > Response.MaxRps then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod003;
    AErro.Descricao := ACBrStr('Conjunto de RPS transmitidos (máximo de ' +
                       IntToStr(Response.MaxRps) + ' RPS)' +
                       ' excedido. Quantidade atual: ' +
                       IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count));
  end;

  if Response.Erros.Count > 0 then Exit;

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  ListaRps := '';
  Prefixo := '';
  PrefixoTS := '';

  case Response.ModoEnvio of
    meUnitario:
    begin
      TagEnvio := ConfigMsgDados.GerarNFSe.DocElemento;

      if EstaVazio(ConfigMsgDados.GerarNFSe.xmlns) then
        NameSpace := ''
      else
      begin
        if ConfigMsgDados.Prefixo = '' then
          NameSpace := ' xmlns="' + ConfigMsgDados.GerarNFSe.xmlns + '"'
        else
        begin
          NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' +
                                   ConfigMsgDados.GerarNFSe.xmlns + '"';
          Prefixo := ConfigMsgDados.Prefixo + ':';
        end;
      end;
    end;
  else
    begin
      TagEnvio := ConfigMsgDados.LoteRps.DocElemento;

      if EstaVazio(ConfigMsgDados.LoteRps.xmlns) then
        NameSpace := ''
      else
      begin
        if ConfigMsgDados.Prefixo = '' then
          NameSpace := ' xmlns="' + ConfigMsgDados.LoteRps.xmlns + '"'
        else
        begin
          NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' +
                                   ConfigMsgDados.LoteRps.xmlns + '"';
          Prefixo := ConfigMsgDados.Prefixo + ':';
        end;
      end;
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if (ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.LoteRps.xmlns) and
       ((ConfigMsgDados.Prefixo <> '') or (ConfigMsgDados.PrefixoTS <> '')) then
    begin
      if ConfigMsgDados.PrefixoTS = '' then
        NameSpace := NameSpace + ' xmlns="' + ConfigMsgDados.XmlRps.xmlns + '"'
      else
      begin
        NameSpace := NameSpace+ ' xmlns:' + ConfigMsgDados.PrefixoTS + '="' +
                                            ConfigMsgDados.XmlRps.xmlns + '"';
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
      end;
    end
    else
    begin
      if ConfigMsgDados.PrefixoTS <> '' then
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
    end;
  end;

  for I := 0 to TACBrNFSeX(FAOwner).NotasFiscais.Count -1 do
  begin
    Nota := TACBrNFSeX(FAOwner).NotasFiscais.Items[I];

    Nota.GerarXML;

    Nota.XmlRps := ConverteXMLtoUTF8(Nota.XmlRps);
    Nota.XmlRps := ChangeLineBreak(Nota.XmlRps, '');

    if ConfigAssinar.Rps then
    begin
      Nota.XmlRps := FAOwner.SSL.Assinar(Nota.XmlRps,
                                         PrefixoTS + ConfigMsgDados.XmlRps.DocElemento,
                                         ConfigMsgDados.XmlRps.InfElemento, '', '', '', IdAttr);
    end;

    SalvarXmlRps(Nota);

    xRps := RemoverDeclaracaoXML(Nota.XmlRps);
    xRps := PrepararRpsParaLote(xRps);

    ListaRps := ListaRps + xRps;
  end;

  if ConfigMsgDados.GerarNSLoteRps then
    NameSpaceLote := NameSpace
  else
    NameSpaceLote := '';

  Versao := ' versao="1"';

  if ConfigGeral.Identificador <> '' then
    IdAttr := ' ' + ConfigGeral.Identificador + '="' + Response.NumeroLote + '"'
  else
    IdAttr := '';

  ListaRps := ChangeLineBreak(ListaRps, '');

  aParams := TNFSeParamsResponse.Create;
  try
    aParams.Clear;
    aParams.Xml := ListaRps;
    aParams.TagEnvio := TagEnvio;
    aParams.Prefixo := Prefixo;
    aParams.Prefixo2 := PrefixoTS;
    aParams.NameSpace := NameSpace;
    aParams.NameSpace2 := NameSpaceLote;
    aParams.IdAttr := IdAttr;
    aParams.Versao := Versao;

    GerarMsgDadosEmitir(Response, aParams);
  finally
    aParams.Free;
  end;
end;

procedure TACBrNFSeProviderNFSeBrasil.GerarMsgDadosEmitir(Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  CodMun: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  CodMun := IntToStr(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio);

  with Params do
  begin
    if Response.ModoEnvio in [meLoteAssincrono] then
      Response.ArquivoEnvio := '<' + TagEnvio + NameSpace + '>' +
                                 '<' + 'LoteRps codMunicipio="' + CodMun + '"' + Versao + NameSpace2 + IdAttr + '>' +
                                   '<' + Prefixo2 + 'NumeroLote>' + Response.NumeroLote + '</' + Prefixo2 + 'NumeroLote>' +
                                   '<' + Prefixo2 + 'Cnpj>' + OnlyNumber(Emitente.CNPJ) + '</' + Prefixo2 + 'Cnpj>' +
                                   GetInscMunic(Emitente.InscMun, Prefixo2) +
                                   '<' + Prefixo2 + 'QuantidadeRps>' +
                                      IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
                                   '</' + Prefixo2 + 'QuantidadeRps>' +
                                   '<' + Prefixo2 + 'ListaRps>' +
                                     Xml +
                                   '</' + Prefixo2 + 'ListaRps>' +
                                 '</' + 'LoteRps>' +
                               '</' + TagEnvio + '>'
    else
      Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                                  Xml +
                               '</' + Prefixo + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderNFSeBrasil.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;
      Response.Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('status'), tcStr);
      Response.Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('protocolo'), tcStr);

      if OnlyNumber(Response.Protocolo) = '' then
        Response.Protocolo := '';
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

procedure TACBrNFSeProviderNFSeBrasil.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
begin
  Response.ArquivoEnvio := Response.Protocolo;
end;

procedure TACBrNFSeProviderNFSeBrasil.TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse);
var
  Document{, DocumentoNota}: TACBrXmlDocument;
  ANode, AuxNode, AuxNodeNota: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  ANota: TNotaFiscal;
  NumRps: String;
  I: Integer;
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

      Response.Situacao := '3'; // Processado com Falhas

      Response.ArquivoRetorno := MontarMensagemErros(Response.ArquivoRetorno);

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root.Childrens.FindAnyNs('RespostaLoteRps');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := ACBrStr(Desc201);
        Exit;
      end;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      Response.Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('status'), tcStr);

      ANode := ANode.Childrens.FindAnyNs('notas');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := ACBrStr(Desc202);
        Exit;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('nota');
      if not Assigned(ANodeArray) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      for I := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[I];
        AuxNode := ANode.Childrens.FindAnyNs('xml');

        if AuxNode <> nil then
        begin
          AuxNodeNota := AuxNode.Childrens.FindAnyNs('CompNfse');
          AuxNodeNota := AuxNodeNota.Childrens.FindAnyNs('Nfse');
          AuxNodeNota := AuxNodeNota.Childrens.FindAnyNs('InfNfse');
          NumRps := ObterConteudoTag(AuxNodeNota.Childrens.FindAnyNs('Numero'), tcStr);

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumRps);

          ANota := CarregarXmlNfse(ANota, AuxNode.OuterXml);
          SalvarXmlNfse(ANota);

          Response.Situacao := '4'; // Processado com sucesso pois retornou a nota
        end
        else
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit;
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

procedure TACBrNFSeProviderNFSeBrasil.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
begin
  Response.ArquivoEnvio := Response.NumeroRps;
end;

procedure TACBrNFSeProviderNFSeBrasil.TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document{, DocumentoNota}: TACBrXmlDocument;
  ANode, AuxNode, AuxNodeNota: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  ANota: TNotaFiscal;
  NumNFSe, InfNfseID: String;
  I: Integer;
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

      Response.Situacao := '3'; // Processado com Falhas

      Response.ArquivoRetorno := MontarMensagemErros(Response.ArquivoRetorno);

      Document.LoadFromXml(Response.ArquivoRetorno);

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root.Childrens.FindAnyNs('RespostaLoteRps');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := ACBrStr(Desc202);
        Exit;
      end;

      Response.Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('status'), tcStr);

      Response.Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('protocolo'), tcStr);

      ANode := ANode.Childrens.FindAnyNs('notas');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := ACBrStr(Desc202);
        Exit;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('nota');
      if not Assigned(ANodeArray) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      for I := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[I];
        AuxNode := ANode.Childrens.FindAnyNs('xml');

        if AuxNode <> nil then
        begin
          AuxNodeNota := AuxNode.Childrens.FindAnyNs('CompNfse');
          AuxNodeNota := AuxNodeNota.Childrens.FindAnyNs('Nfse');
          AuxNodeNota := AuxNodeNota.Childrens.FindAnyNs('InfNfse');

          InfNfseID := ObterConteudoTag(AuxNodeNota.Attributes.Items['Id']);
          NumNFSe := ObterConteudoTag(AuxNodeNota.Childrens.FindAnyNs('Numero'), tcStr);

          with Response do
          begin
            NumeroNota := NumNFSe;
            idNota := InfNfseID;
            CodigoVerificacao := ObterConteudoTag(AuxNodeNota.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
            Data := ObterConteudoTag(AuxNodeNota.Childrens.FindAnyNs('DataEmissao'), tcDatVcto);
          end;

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

          ANota := CarregarXmlNfse(ANota, AuxNode.OuterXml);
          SalvarXmlNfse(ANota);

          Response.Situacao := '4'; // Processado com sucesso pois retornou a nota
        end
        else
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit;
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

procedure TACBrNFSeProviderNFSeBrasil.PrepararConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
begin
  Response.Metodo := tmConsultarNFSe;
  Response.ArquivoEnvio := Response.InfConsultaNFSe.NumeroIniNFSe;
end;

procedure TACBrNFSeProviderNFSeBrasil.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
begin
  Response.ArquivoEnvio := Response.InfCancelamento.NumeroNFSe;
end;

procedure TACBrNFSeProviderNFSeBrasil.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I: Integer;
  ANodeArray, ANodeArrayAlerta: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  AAlerta: TNFSeEventoCollectionItem;
  Mensagem: string;
begin
  ANodeArray := RootNode.Childrens.FindAllAnyNs(AListTag);

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    Mensagem := ObterConteudoTag(ANodeArray[i].Childrens.FindAnyNs(AMessageTag), tcStr);
    if Mensagem = '' then
      Continue;

    AErro := Response.Erros.New;
    AErro.Codigo := '';
    AErro.Descricao := Mensagem;
    AErro.Correcao := '';
  end;

  ANodeArrayAlerta := RootNode.Childrens.FindAllAnyNs('avisos');

  for I := Low(ANodeArrayAlerta) to High(ANodeArrayAlerta) do
  begin
    Mensagem := ObterConteudoTag(ANodeArrayAlerta[i].Childrens.FindAnyNs('aviso'), tcStr);
    if Mensagem = '' then
      Continue;

    AAlerta := Response.Alertas.New;
    AAlerta.Codigo := '';
    AAlerta.Descricao := Mensagem;
    AAlerta.Correcao := '';
  end;
end;

function TACBrNFSeProviderNFSeBrasil.MontarMensagemErros(
  const ARetorno: string): string;
begin
  if Pos('RespostaLoteRps', ARetorno) = 0 then
  begin
    Result := '<a>' +'<RespostaLoteRps>' +
                '<erros>' +
                  '<erro>' + SeparaDados(ARetorno, 'return') + '</erro>' +
                '</erros>' +
              '</RespostaLoteRps>' + '</a>';

    Result := ParseText(Result);
  end
  else
    Result := ARetorno;
end;

end.
