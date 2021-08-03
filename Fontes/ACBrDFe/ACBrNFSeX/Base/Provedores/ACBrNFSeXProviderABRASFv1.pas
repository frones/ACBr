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

unit ACBrNFSeXProviderABRASFv1;

interface

uses
  SysUtils, Classes,
  ACBrNFSeXClass, ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXLerXml, ACBrNFSeXGravarXml,
  ACBrNFSeXWebserviceBase, ACBrNFSeXProviderBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeProviderABRASFv1 = class(TACBrNFSeXProvider)
  protected
    procedure Configuracao; override;

    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
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

    procedure PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); override;
    procedure TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); override;

    procedure ProcessarMensagemErros(const RootNode: TACBrXmlNode;
                                     const Response: TNFSeWebserviceResponse;
                                     AListTag: string = 'ListaMensagemRetorno';
                                     AMessageTag: string = 'MensagemRetorno'); virtual;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrXmlWriter, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXConsts, ACBrNFSeXNotasFiscais, ACBrNFSeXConversao;

{ TACBrNFSeProviderABRASFv1 }

procedure TACBrNFSeProviderABRASFv1.Configuracao;
const
  NameSpace = 'http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd';
begin
  inherited Configuracao;

  // Todos os provedores que seguem a versão 1 do layout da ABRASF só tem
  // um serviço para recepcionar o RPS e é assíncrono.
  with ConfigGeral do
  begin
    ModoEnvio := meLoteAssincrono;
    ConsultaSitLote := True;
  end;

  SetXmlNameSpace(NameSpace);

  with ConfigWebServices do
  begin
    VersaoDados := '1.00';
    VersaoAtrib := '1.00';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');
end;

procedure TACBrNFSeProviderABRASFv1.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Nota: NotaFiscal;
  Versao, IdAttr, NameSpace, NameSpaceLote, ListaRps, xRps,
  TagEnvio, Prefixo, PrefixoTS: string;
  I: Integer;
begin
  if Response.ModoEnvio in [meLoteSincrono, meUnitario, meTeste] then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod001;
    AErro.Descricao := Desc001;
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count <= 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod002;
    AErro.Descricao := Desc002;
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count > Response.MaxRps then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod003;
    AErro.Descricao := 'Conjunto de RPS transmitidos (máximo de ' +
                       IntToStr(Response.MaxRps) + ' RPS)' +
                       ' excedido. Quantidade atual: ' +
                       IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count);
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
    if ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.LoteRps.xmlns then
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

    if EstaVazio(Nota.XMLAssinado) then
    begin
      Nota.GerarXML;
      if ConfigAssinar.Rps then
      begin
        Nota.XMLOriginal := FAOwner.SSL.Assinar(ConverteXMLtoUTF8(Nota.XMLOriginal),
                                                PrefixoTS + ConfigMsgDados.XmlRps.DocElemento,
                                                ConfigMsgDados.XmlRps.InfElemento, '', '', '', IdAttr);
      end;
    end;

    SalvarXmlRps(Nota);

    xRps := RemoverDeclaracaoXML(Nota.XMLOriginal);
    xRps := PrepararRpsParaLote(xRps);

    ListaRps := ListaRps + xRps;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if ConfigMsgDados.GerarNSLoteRps then
    NameSpaceLote := NameSpace
  else
    NameSpaceLote := '';

  if ConfigWebServices.AtribVerLote <> '' then
    Versao := ' ' + ConfigWebServices.AtribVerLote + '="' +
              ConfigWebServices.VersaoDados + '"'
  else
    Versao := '';

  if ConfigGeral.Identificador <> '' then
    IdAttr := ' ' + ConfigGeral.Identificador + '="Lote_' + Response.Lote + '"'
  else
    IdAttr := '';

  ListaRps := ChangeLineBreak(ListaRps, '');

  if Response.ModoEnvio in [meLoteAssincrono] then
    Response.XmlEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                           '<' + Prefixo + 'LoteRps' + NameSpaceLote + IdAttr  + Versao + '>' +
                             '<' + PrefixoTS + 'NumeroLote>' + Response.Lote + '</' + PrefixoTS + 'NumeroLote>' +
                             '<' + PrefixoTS + 'Cnpj>' + OnlyNumber(Emitente.CNPJ) + '</' + PrefixoTS + 'Cnpj>' +
                             GetInscMunic(Emitente.InscMun, PrefixoTS) +
                             '<' + PrefixoTS + 'QuantidadeRps>' +
                                IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
                             '</' + PrefixoTS + 'QuantidadeRps>' +
                             '<' + PrefixoTS + 'ListaRps>' +
                               ListaRps +
                             '</' + PrefixoTS + 'ListaRps>' +
                           '</' + Prefixo + 'LoteRps>' +
                         '</' + Prefixo + TagEnvio + '>'
  else
    Response.XmlEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                            ListaRps +
                         '</' + Prefixo + TagEnvio + '>';
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;
      Response.Data := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('DataRecebimento'), tcDatHor);
      Response.Protocolo := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('Protocolo'), tcStr);
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

procedure TACBrNFSeProviderABRASFv1.PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, Prefixo, PrefixoTS: string;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := Desc101;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  Prefixo := '';
  PrefixoTS := '';

  if EstaVazio(ConfigMsgDados.ConsultarSituacao.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarSituacao.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' + ConfigMsgDados.ConsultarSituacao.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.ConsultarSituacao.xmlns then
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

  Response.XmlEnvio := '<' + Prefixo + 'ConsultarSituacaoLoteRpsEnvio' + NameSpace + '>' +
                         '<' + Prefixo + 'Prestador>' +
                           '<' + PrefixoTS + 'Cnpj>' +
                             OnlyNumber(Emitente.CNPJ) +
                           '</' + PrefixoTS + 'Cnpj>' +
                           GetInscMunic(Emitente.InscMun, PrefixoTS) +
                         '</' + Prefixo + 'Prestador>' +
                         '<' + Prefixo + 'Protocolo>' +
                           Response.Protocolo +
                         '</' + Prefixo + 'Protocolo>' +
                       '</' + Prefixo + 'ConsultarSituacaoLoteRpsEnvio>';
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      Document.LoadFromXml(Response.XmlRetorno);
      ProcessarMensagemErros(Document.Root, Response);

      if Response.Erros.Count > 0 then Exit;

      Response.Sucesso := True;
      Response.Situacao := ProcessarConteudoXml(Document.Root.Childrens.FindAnyNs('Situacao'), tcStr);
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

procedure TACBrNFSeProviderABRASFv1.PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, Prefixo, PrefixoTS: string;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := Desc101;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  Prefixo := '';
  PrefixoTS := '';

  if EstaVazio(ConfigMsgDados.ConsultarLote.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarLote.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' + ConfigMsgDados.ConsultarLote.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.ConsultarLote.xmlns then
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

  Response.XmlEnvio := '<' + Prefixo + 'ConsultarLoteRpsEnvio' + NameSpace + '>' +
                         '<' + Prefixo + 'Prestador>' +
                           '<' + PrefixoTS + 'Cnpj>' +
                             OnlyNumber(Emitente.CNPJ) +
                           '</' + PrefixoTS + 'Cnpj>' +
                           GetInscMunic(Emitente.InscMun, PrefixoTS) +
                         '</' + Prefixo + 'Prestador>' +
                         '<' + Prefixo + 'Protocolo>' +
                           Response.Protocolo +
                         '</' + Prefixo + 'Protocolo>' +
                       '</' + Prefixo + 'ConsultarLoteRpsEnvio>';
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  ANota: NotaFiscal;
  NumRps: String;
  I: Integer;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response);
      if Response.Erros.Count > 0 then Exit;

      Response.Situacao := ProcessarConteudoXml(Document.Root.Childrens.FindAnyNs('SituacaoLoteRps'), tcStr);

      ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := Desc202;
        Exit;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('CompNfse');
      if ANodeArray = nil then
        ANodeArray := ANode.Childrens.FindAllAnyNs('ComplNfse');

      if not Assigned(ANodeArray) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := Desc203;
        Exit;
      end;

      for I := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[I];
        AuxNode := ANode.Childrens.FindAnyNs('tcCompNfse');

        if AuxNode = nil then
          AuxNode := ANode.Childrens.FindAnyNs('Nfse')
        else
          AuxNode := AuxNode.Childrens.FindAnyNs('Nfse');

        if AuxNode <> nil then
        begin
          AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
          AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
          AuxNode := AuxNode.Childrens.FindAnyNs('Numero');
          NumRps := ProcessarConteudoXml(AuxNode, tcStr);

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

          if Assigned(ANota) then
            ANota.XML := ANode.OuterXml
          else
          begin
            TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
          end;

          SalvarXmlNfse(ANota);

          Response.Situacao := '4';
        end
        else
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := Desc203;
          Exit;
        end;
      end;

      Response.Sucesso := True;
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

procedure TACBrNFSeProviderABRASFv1.PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, Prefixo, PrefixoTS: string;
begin
  if EstaVazio(Response.NumRPS) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := Desc102;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  Prefixo := '';
  PrefixoTS := '';

  if EstaVazio(ConfigMsgDados.ConsultarNFSeRps.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarNFSeRps.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' + ConfigMsgDados.ConsultarNFSeRps.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.ConsultarNFSeRps.xmlns then
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

  Response.XmlEnvio := '<' + Prefixo + ConfigMsgDados.ConsultarNFSeRps.DocElemento + NameSpace + '>' +
                         '<' + Prefixo + 'IdentificacaoRps>' +
                           '<' + PrefixoTS + 'Numero>' +
                             Response.NumRPS +
                           '</' + PrefixoTS + 'Numero>' +
                           '<' + PrefixoTS + 'Serie>' +
                             Response.Serie +
                           '</' + PrefixoTS + 'Serie>' +
                           '<' + PrefixoTS + 'Tipo>' +
                             Response.Tipo +
                           '</' + PrefixoTS + 'Tipo>' +
                         '</' + Prefixo + 'IdentificacaoRps>' +
                         '<' + Prefixo + 'Prestador>' +
                           '<' + PrefixoTS + 'Cnpj>' +
                             OnlyNumber(Emitente.CNPJ) +
                           '</' + PrefixoTS + 'Cnpj>' +
                           GetInscMunic(Emitente.InscMun, PrefixoTS) +
                         '</' + Prefixo + 'Prestador>' +
                       '</' + Prefixo + ConfigMsgDados.ConsultarNFSeRps.DocElemento + '>';
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  AErro: TNFSeEventoCollectionItem;
  ANota: NotaFiscal;
  NumNFSe: String;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response);
      if Response.Erros.Count > 0 then Exit;

      ANode := Document.Root.Childrens.FindAnyNs('CompNfse');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := Desc203;
        Exit;
      end;

      AuxNode := ANode.Childrens.FindAnyNs('Nfse');
      AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
//      AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
      AuxNode := AuxNode.Childrens.FindAnyNs('Numero');
      NumNFSe := ProcessarConteudoXml(AuxNode, tcStr);

      ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

      if Assigned(ANota) then
        ANota.XML := ANode.OuterXml
      else
      begin
        TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
        ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
      end;

      SalvarXmlNfse(ANota);

      Response.Sucesso := True;
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

procedure TACBrNFSeProviderABRASFv1.PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  XmlConsulta, RazaoInter, NameSpace, Prefixo, PrefixoTS: string;
begin
  if Response.InfConsultaNFSe.tpConsulta in [tcPorFaixa, tcServicoTomado, tcPorNumeroURLRetornado] then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod001;
    AErro.Descricao := Desc001;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  Prefixo := '';
  PrefixoTS := '';

  Response.Metodo := tmConsultarNFSe;

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

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.ConsultarNFSe.xmlns then
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

  if OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) <> '' then
    XmlConsulta := '<' + Prefixo + 'NumeroNfse>' +
                      OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) +
                   '</' + Prefixo + 'NumeroNfse>'
  else
    XmlConsulta := '';

  if (Response.InfConsultaNFSe.DataInicial > 0) and (Response.InfConsultaNFSe.DataFinal > 0) then
    XmlConsulta := XmlConsulta +
                     '<' + Prefixo + 'PeriodoEmissao>' +
                       '<' + PrefixoTS + 'DataInicial>' +
                          FormatDateTime('yyyy-mm-dd', Response.InfConsultaNFSe.DataInicial) +
                       '</' + PrefixoTS + 'DataInicial>' +
                       '<' + PrefixoTS + 'DataFinal>' +
                          FormatDateTime('yyyy-mm-dd', Response.InfConsultaNFSe.DataFinal) +
                       '</' + PrefixoTS + 'DataFinal>' +
                     '</' + Prefixo + 'PeriodoEmissao>';

  if NaoEstaVAzio(Response.InfConsultaNFSe.CNPJTomador) then
  begin
    XmlConsulta := XmlConsulta +
                     '<' + Prefixo + 'Tomador>' +
                       '<' + PrefixoTS + 'CpfCnpj>' +
                          GetCpfCnpj(Response.InfConsultaNFSe.CNPJTomador, PrefixoTS) +
                       '</' + PrefixoTS + 'CpfCnpj>' +
                       GetInscMunic(Response.InfConsultaNFSe.IMTomador, PrefixoTS) +
                     '</' + Prefixo + 'Tomador>';
  end;

  if NaoEstaVAzio(Response.InfConsultaNFSe.CNPJInter) then
  begin
    if NaoEstaVazio(Response.InfConsultaNFSe.RazaoInter) then
      RazaoInter := '<' + PrefixoTS + 'RazaoSocial>' +
                       OnlyNumber(Response.InfConsultaNFSe.RazaoInter) +
                    '</' + PrefixoTS + 'RazaoSocial>'
    else
      RazaoInter := '';

    XmlConsulta := XmlConsulta +
                     '<' + Prefixo + 'IntermediarioServico>' +
                       RazaoInter +
                       '<' + PrefixoTS + 'CpfCnpj>' +
                          GetCpfCnpj(Response.InfConsultaNFSe.CNPJInter, PrefixoTS) +
                       '</' + PrefixoTS + 'CpfCnpj>' +
                       GetInscMunic(Response.InfConsultaNFSe.IMInter, PrefixoTS) +
                     '</' + Prefixo + 'IntermediarioServico>';
  end;

  Response.XmlEnvio := '<' + Prefixo + ConfigMsgDados.ConsultarNFSe.DocElemento + NameSpace + '>' +
                         '<' + Prefixo + 'Prestador>' +
                           '<' + PrefixoTS + 'Cnpj>' +
                             OnlyNumber(Emitente.CNPJ) +
                           '</' + PrefixoTS + 'Cnpj>' +
                           GetInscMunic(Emitente.InscMun, PrefixoTS) +
                         '</' + Prefixo + 'Prestador>' +
                         XmlConsulta +
                       '</' + Prefixo + ConfigMsgDados.ConsultarNFSe.DocElemento + '>';
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  ANota: NotaFiscal;
  NumNFSe: String;
  I: Integer;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response);
      if Response.Erros.Count > 0 then Exit;

      ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := Desc202;
        Exit;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('CompNfse');
      if not Assigned(ANodeArray) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := Desc203;
        Exit;
      end;

      for I := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANode := ANodeArray[I];
        AuxNode := ANode.Childrens.FindAnyNs('Nfse');
        AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
//        AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
        AuxNode := AuxNode.Childrens.FindAnyNs('Numero');
        NumNFSe := ProcessarConteudoXml(AuxNode, tcStr);

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

        if Assigned(ANota) then
          ANota.XML := ANode.OuterXml
        else
        begin
          TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
        end;

        SalvarXmlNfse(ANota);
      end;

      Response.Sucesso := True;
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

procedure TACBrNFSeProviderABRASFv1.PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  InfoCanc: TInfCancelamento;
  IdAttr, NameSpace, Prefixo, PrefixoTS, xMotivo: string;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := Desc108;
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.CodCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod109;
    AErro.Descricao := Desc109;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  Prefixo := '';
  PrefixoTS := '';

  InfoCanc := Response.InfCancelamento;

  if ConfigGeral.Identificador <> '' then
    IdAttr := ' ' + ConfigGeral.Identificador + '="Canc_' +
                    OnlyNumber(Emitente.CNPJ) + OnlyNumber(Emitente.InscMun) +
                    InfoCanc.NumeroNFSe + '"'
  else
    IdAttr := '';

  if EstaVazio(ConfigMsgDados.CancelarNFSe.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.CancelarNFSe.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' + ConfigMsgDados.CancelarNFSe.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.CancelarNFSe.xmlns then
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

  if InfoCanc.MotCancelamento <> '' then
    xMotivo := '<' + PrefixoTS + 'MotivoCancelamento>' +
                 InfoCanc.MotCancelamento +
               '</' + PrefixoTS + 'MotivoCancelamento>'
  else
    xMotivo := '';

  Response.XmlEnvio := '<' + Prefixo + 'CancelarNfseEnvio' + NameSpace + '>' +
                         '<' + Prefixo + 'Pedido>' +
                           '<' + PrefixoTS + 'InfPedidoCancelamento' + IdAttr + '>' +
                             '<' + PrefixoTS + 'IdentificacaoNfse>' +
                               '<' + PrefixoTS + 'Numero>' +
                                 InfoCanc.NumeroNFSe +
                               '</' + PrefixoTS + 'Numero>' +
                               '<' + PrefixoTS + 'Cnpj>' +
                                 OnlyNumber(Emitente.CNPJ) +
                               '</' + PrefixoTS + 'Cnpj>' +
                               GetInscMunic(Emitente.InscMun, PrefixoTS) +
                               '<' + PrefixoTS + 'CodigoMunicipio>' +
                                  IntToStr(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                               '</' + PrefixoTS + 'CodigoMunicipio>' +
                             '</' + PrefixoTS + 'IdentificacaoNfse>' +
                             '<' + PrefixoTS + 'CodigoCancelamento>' +
                                InfoCanc.CodCancelamento +
                             '</' + PrefixoTS + 'CodigoCancelamento>' +
                             xMotivo +
                           '</' + PrefixoTS + 'InfPedidoCancelamento>' +
                         '</' + Prefixo + 'Pedido>' +
                       '</' + Prefixo + 'CancelarNfseEnvio>';
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Document: TACBrXmlDocument;
  ANode: TACBrXmlNode;
  Ret: TRetCancelamento;
  IdAttr: string;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response);
      if Response.Erros.Count > 0 then Exit;

      ANode := Document.Root.Childrens.FindAnyNs('Cancelamento');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod204;
        AErro.Descricao := Desc204;
        Exit;
      end;

      ANode := Document.Root.Childrens.FindAnyNs('Confirmacao');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod204;
        AErro.Descricao := Desc204;
        Exit;
      end;

      Ret :=  Response.RetCancelamento;
      Ret.DataHora := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('DataHoraCancelamento'), tcDatHor);

      if ConfigAssinar.IncluirURI then
        IdAttr := ConfigGeral.Identificador
      else
        IdAttr := 'ID';

      ANode := Document.Root.Childrens.FindAnyNs('Pedido').Childrens.FindAnyNs('InfPedidoCancelamento');

      Ret.Pedido.InfID.ID := ANode.Attributes.Items[IdAttr].Content;
      Ret.Pedido.CodigoCancelamento := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('CodigoCancelamento'), tcStr);

      ANode := Document.Root.Childrens.FindAnyNs('IdentificacaoNfse');

      with  Ret.Pedido.IdentificacaoNfse do
      begin
        Numero := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('Numero'), tcStr);
        Cnpj := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('Cnpj'), tcStr);
        InscricaoMunicipal := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
        CodigoMunicipio := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
      end;
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

procedure TACBrNFSeProviderABRASFv1.PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  AErro := Response.Erros.New;
  AErro.Codigo := Cod001;
  AErro.Descricao := Desc001;

  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);

  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse);
begin
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.ProcessarMensagemErros(const RootNode: TACBrXmlNode;
  const Response: TNFSeWebserviceResponse; AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if ANode = nil then
    ANode := RootNode.Childrens.FindAnyNs('Listamensagemretorno');

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if ANodeArray = nil then
    ANodeArray := ANode.Childrens.FindAllAnyNs('tcMensagemRetorno');

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Codigo'), tcStr);
    AErro.Descricao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Mensagem'), tcStr);
    AErro.Correcao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Correcao'), tcStr);
  end;
end;

end.
