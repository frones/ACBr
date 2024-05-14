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

unit BHISS.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml, ACBrNFSeXWebservicesResponse,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXWebserviceBase;

type
  TACBrNFSeXWebserviceBHISS = class(TACBrNFSeXWebserviceSoap11)

  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function GerarNFSe(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderBHISS = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXNotasFiscais, ACBrNFSeXConsts,
  BHISS.GravarXml, BHISS.LerXml;

{ TACBrNFSeXWebserviceBHISS }

function TACBrNFSeXWebserviceBHISS.Recepcionar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:RecepcionarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ws:RecepcionarLoteRpsRequest>';

  Result := Executar('http://ws.bhiss.pbh.gov.br/RecepcionarLoteRps', Request,
                     ['outputXML', 'EnviarLoteRpsResposta'],
                     ['xmlns:ws="http://ws.bhiss.pbh.gov.br"']);
end;

function TACBrNFSeXWebserviceBHISS.GerarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:GerarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ws:GerarNfseRequest>';

  Result := Executar('http://ws.bhiss.pbh.gov.br/GerarNfse', Request,
                     ['outputXML', 'GerarNfseResposta'],
                     ['xmlns:ws="http://ws.bhiss.pbh.gov.br"']);
end;

function TACBrNFSeXWebserviceBHISS.ConsultarLote(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ConsultarLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ws:ConsultarLoteRpsRequest>';

  Result := Executar('http://ws.bhiss.pbh.gov.br/ConsultarLoteRps', Request,
                     ['outputXML', 'ConsultarLoteRpsResposta'],
                     ['xmlns:ws="http://ws.bhiss.pbh.gov.br"']);
end;

function TACBrNFSeXWebserviceBHISS.ConsultarSituacao(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ConsultarSituacaoLoteRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ws:ConsultarSituacaoLoteRpsRequest>';

  Result := Executar('http://ws.bhiss.pbh.gov.br/ConsultarSituacaoLoteRps', Request,
                     ['outputXML', 'ConsultarSituacaoLoteRpsResposta'],
                     ['xmlns:ws="http://ws.bhiss.pbh.gov.br"']);
end;

function TACBrNFSeXWebserviceBHISS.ConsultarNFSePorRps(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ConsultarNfsePorRpsRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ws:ConsultarNfsePorRpsRequest>';

  Result := Executar('http://ws.bhiss.pbh.gov.br/ConsultarNfsePorRps', Request,
                     ['outputXML', 'ConsultarNfseRpsResposta'],
                     ['xmlns:ws="http://ws.bhiss.pbh.gov.br"']);
end;

function TACBrNFSeXWebserviceBHISS.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:ConsultarNfsePorFaixaRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ws:ConsultarNfsePorFaixaRequest>';

  Result := Executar('http://ws.bhiss.pbh.gov.br/ConsultarNfse', Request,
                     ['outputXML', 'ConsultarNfseFaixaResposta'],
                     ['xmlns:ws="http://ws.bhiss.pbh.gov.br"']);
end;

function TACBrNFSeXWebserviceBHISS.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ws:CancelarNfseRequest>';
  Request := Request + '<nfseCabecMsg>' + IncluirCDATA(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + IncluirCDATA(AMSG) + '</nfseDadosMsg>';
  Request := Request + '</ws:CancelarNfseRequest>';

  Result := Executar('http://ws.bhiss.pbh.gov.br/CancelarNfse', Request,
                     ['outputXML', 'CancelarNfseResposta'],
                     ['xmlns:ws="http://ws.bhiss.pbh.gov.br"']);
end;

function TACBrNFSeXWebserviceBHISS.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := RemoverCaracteresDesnecessarios(Result);
  Result := ParseText(Result);
  Result := RemoverDeclaracaoXML(Result);
end;

{ TACBrNFSeProviderBHISS }

procedure TACBrNFSeProviderBHISS.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    QuebradeLinha := '|';
    NumMaxRpsGerar := 3;

    ServicosDisponibilizados.EnviarUnitario := True;
  end;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    CancelarNFSe := True;
    RpsGerarNFSe := True;
    LoteGerarNFSe := True;
  end;

  SetXmlNameSpace('http://www.abrasf.org.br/nfse.xsd');

  with ConfigMsgDados do
  begin
    ConsultarNFSe.DocElemento := 'ConsultarNfseFaixaEnvio';

    GerarNFSe.InfElemento := 'LoteRps';
    GerarNFSe.DocElemento := 'GerarNfseEnvio';

    DadosCabecalho := GetCabecalho('');
  end;

  ConfigWebServices.AtribVerLote := 'versao';
end;

function TACBrNFSeProviderBHISS.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_BHISS.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderBHISS.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_BHISS.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderBHISS.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceBHISS.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderBHISS.PrepararConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  XmlConsulta, NameSpace, Prefixo: string;
begin
  if Response.InfConsultaNFSe.tpConsulta in [tcPorFaixa, tcServicoTomado] then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod001;
    AErro.Descricao := ACBrStr(Desc101);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.Metodo := tmConsultarNFSe;

  XmlConsulta := '<Faixa>' +
                   '<NumeroNfseInicial>' +
                      OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) +
                   '</NumeroNfseInicial>' +
                   '<NumeroNfseFinal>' +
                      OnlyNumber(Response.InfConsultaNFSe.NumeroFinNFSe) +
                   '</NumeroNfseFinal>' +
                 '</Faixa>';

  if EstaVazio(ConfigMsgDados.ConsultarNFSe.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarNFSe.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' + ConfigMsgDados.ConsultarNFSe.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  Response.ArquivoEnvio := '<' + Prefixo + 'ConsultarNfseFaixaEnvio' +
                                NameSpace + '>' +
                             '<Prestador>' +
                               '<Cnpj>' + OnlyNumber(Emitente.CNPJ) + '</Cnpj>' +
                               '<InscricaoMunicipal>' +
                                  OnlyNumber(Emitente.InscMun) +
                               '</InscricaoMunicipal>' +
                             '</Prestador>' +
                             XmlConsulta +
                             '<Pagina>' +
                                IntToStr(Response.InfConsultaNFSe.Pagina) +
                             '</Pagina>' +
                           '</' + Prefixo + 'ConsultarNfseFaixaEnvio>';
end;

procedure TACBrNFSeProviderBHISS.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Nota: TNotaFiscal;
  Versao, IdAttr, NameSpace, NameSpaceLote, xRps, ListaRps, Prefixo: string;
  I: Integer;
begin
  if Response.ModoEnvio <> meUnitario then
  begin
    inherited PrepararEmitir(Response);
    Exit;
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
  for I := 0 to TACBrNFSeX(FAOwner).NotasFiscais.Count -1 do
  begin
    Nota := TACBrNFSeX(FAOwner).NotasFiscais.Items[I];

    Nota.GerarXML;

    Nota.XmlRps := ConverteXMLtoUTF8(Nota.XmlRps);
    Nota.XmlRps := ChangeLineBreak(Nota.XmlRps, '');

    if ConfigAssinar.RpsGerarNFSe then
    begin
      Nota.XmlRps := FAOwner.SSL.Assinar(Nota.XmlRps,
                                         ConfigMsgDados.XmlRps.DocElemento,
                                         ConfigMsgDados.XmlRps.InfElemento, '', '', '', IdAttr);
    end;

    SalvarXmlRps(Nota);

    xRps := RemoverDeclaracaoXML(Nota.XmlRps);
    xRps := PrepararRpsParaLote(xRps);

    ListaRps := ListaRps + xRps;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(ConfigMsgDados.GerarNFSe.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.GerarNFSe.xmlns + '"';

  if ConfigMsgDados.GerarNSLoteRps then
    NameSpaceLote := NameSpace
  else
    NameSpaceLote := '';

  if ConfigWebServices.AtribVerLote <> '' then
    Versao := ' ' + ConfigWebServices.AtribVerLote + '="' +
              ConfigWebServices.VersaoDados + '"'
  else
    Versao := '';

  if ConfigAssinar.IncluirURI then
    IdAttr := ' ' + ConfigGeral.Identificador + '="Lote_' + Response.NumeroLote + '"'
  else
    IdAttr := '';

  ListaRps := ChangeLineBreak(ListaRps, '');

  Response.ArquivoEnvio := '<' + Prefixo + 'GerarNfseEnvio' + NameSpace + '>' +
                             '<LoteRps' + NameSpaceLote + IdAttr  + Versao + '>' +
                               '<NumeroLote>' + Response.NumeroLote + '</NumeroLote>' +
                               '<Cnpj>' + OnlyNumber(Emitente.CNPJ) + '</Cnpj>' +
                               '<InscricaoMunicipal>' +
                                  OnlyAlphaNum(Emitente.InscMun) +
                               '</InscricaoMunicipal>' +
                               '<QuantidadeRps>' +
                                  IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
                               '</QuantidadeRps>' +
                               '<ListaRps>' + ListaRps + '</ListaRps>' +
                             '</LoteRps>' +
                           '</' + Prefixo + 'GerarNfseEnvio>';
end;

procedure TACBrNFSeProviderBHISS.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  ANota: TNotaFiscal;
  NumRps: String;
  I: Integer;
begin
  if Response.ModoEnvio <> meUnitario then
  begin
    inherited TratarRetornoEmitir(Response);
    Exit;
  end;

  Document := TACBrXmlDocument.Create;
  try
    try
      Document.LoadFromXml(Response.ArquivoRetorno);

      ProcessarMensagemErros(Document.Root, Response);
      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetornoLote');

      ANode := Document.Root;

      Response.Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('DataRecebimento'), tcDatHor);
      Response.Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('Protocolo'), tcStr);
      Response.CodigoVerificacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);

      ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := ACBrStr(Desc202);
        Exit;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('CompNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      for I := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[I];
        AuxNode := ANode.Childrens.FindAnyNs('Nfse');
        AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
        AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
        AuxNode := AuxNode.Childrens.FindAnyNs('Numero');
        NumRps := AuxNode.AsString;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

        ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
        SalvarXmlNfse(ANota);
      end;

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
end;

end.
