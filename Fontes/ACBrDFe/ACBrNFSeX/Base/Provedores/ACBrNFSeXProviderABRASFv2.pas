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

unit ACBrNFSeXProviderABRASFv2;

interface

uses
  SysUtils, Classes,
  ACBrNFSeXClass, ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXLerXml, ACBrNFSeXGravarXml,
  ACBrNFSeXWebserviceBase, ACBrNFSeXProviderBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeProviderABRASFv2 = class(TACBrNFSeXProvider)
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
    procedure AssinarConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure AssinarConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure TratarRetornoConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); virtual;

    procedure PrepararConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure AssinarConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure TratarRetornoConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse); virtual;

    procedure PrepararConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure AssinarConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure TratarRetornoConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse); virtual;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); override;
    procedure TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); override;

    procedure ProcessarMensagemErros(const RootNode: TACBrXmlNode;
                                     const Response: TNFSeWebserviceResponse;
                                     AListTag: string = 'ListaMensagemRetorno';
                                     AMessageTag: string = 'MensagemRetorno'); virtual;

    function GerarRequerente(const CNPJ: string; const InscMunc: string;
      const Senha: string): string;
  end;

implementation

uses
  ACBrUtil, ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes,
  ACBrNFSeXConsts, ACBrNFSeXNotasFiscais, ACBrNFSeXConversao;

{ TACBrNFSeProviderABRASFv2 }

procedure TACBrNFSeProviderABRASFv2.Configuracao;
const
  NameSpace = 'http://www.abrasf.org.br/nfse.xsd';
begin
  inherited Configuracao;

  // Os provedores que seguem a versão 2 do layout da ABRASF podem ter até
  // três serviços para recepcionar o RPS: assíncrono, síncrono e unitário.
  // Por padrão vamos adotar o serviço Síncrono, caso o provedor não tenha
  // esse serviço na Unit ACBrNFSeProviderxxxx (xxxx = Provedor) devemos
  // configurar o serviço disponibilizado.
  ConfigGeral.ModoEnvio := meLoteSincrono;

  SetXmlNameSpace(NameSpace);

  with ConfigWebServices do
  begin
    VersaoDados := '2.00';
    VersaoAtrib := '2.00';
    AtribVerLote := 'versao';
  end;

  with ConfigMsgDados do
  begin
    with XmlRps do
    begin
      InfElemento := 'InfDeclaracaoPrestacaoServico';
      DocElemento := 'Rps';
    end;

    DadosCabecalho := GetCabecalho('');

    GerarNSLoteRps := False;
  end;
end;

function TACBrNFSeProviderABRASFv2.GerarRequerente(const CNPJ, InscMunc,
  Senha: string): string;
var
  Homologacao: Boolean;
begin
  Homologacao := (TACBrNFSeX(FAOwner).Configuracoes.WebServices.AmbienteCodigo = 2);

  Result := '<IdentificacaoRequerente>' +
              '<CpfCnpj>' +
                '<Cnpj>' + CNPJ + '</Cnpj>' +
              '</CpfCnpj>' +
              '<InscricaoMunicipal>' + InscMunc + '</InscricaoMunicipal>' +
              '<Senha>' + Senha + '</Senha>' +
              '<Homologa>' +
                 LowerCase(booltostr(Homologacao, True)) +
              '</Homologa>' +
            '</IdentificacaoRequerente>';
end;

procedure TACBrNFSeProviderABRASFv2.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Nota: NotaFiscal;
  Versao, IdAttr, NameSpace, NameSpaceLote, ListaRps, xRps,
  TagEnvio, Prestador, Prefixo, PrefixoTS, Requerente: string;
  I: Integer;
begin
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

  ListaRps := '';
  Prefixo := '';
  PrefixoTS := '';
  Requerente := '';

  case Response.ModoEnvio of
    meLoteSincrono:
    begin
      TagEnvio := ConfigMsgDados.LoteRpsSincrono.DocElemento;

      if EstaVazio(ConfigMsgDados.LoteRpsSincrono.xmlns) then
        NameSpace := ''
      else
      begin
        if ConfigMsgDados.Prefixo = '' then
          NameSpace := ' xmlns="' + ConfigMsgDados.LoteRpsSincrono.xmlns + '"'
        else
        begin
          NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' +
                                   ConfigMsgDados.LoteRpsSincrono.xmlns + '"';
          Prefixo := ConfigMsgDados.Prefixo + ':';
        end;
      end;
    end;

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

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  for I := 0 to TACBrNFSeX(FAOwner).NotasFiscais.Count -1 do
  begin
    Nota := TACBrNFSeX(FAOwner).NotasFiscais.Items[I];

    if EstaVazio(Nota.XMLAssinado) then
    begin
      Nota.GerarXML;
      if (ConfigAssinar.Rps and (Response.ModoEnvio in [meLoteAssincrono, meLoteSincrono])) or
         (ConfigAssinar.RpsGerarNFSe and (Response.ModoEnvio = meUnitario)) then
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

  if Response.ModoEnvio in [meLoteAssincrono, meLoteSincrono] then
  begin
    if ConfigMsgDados.GerarPrestadorLoteRps then
    begin
      Prestador := '<' + PrefixoTS + 'Prestador>' +
                     '<' + PrefixoTS + 'CpfCnpj>' +
                       GetCpfCnpj(Emitente.CNPJ, PrefixoTS) +
                     '</' + PrefixoTS + 'CpfCnpj>' +
                     GetInscMunic(Emitente.InscMun, PrefixoTS) +
                   '</' + PrefixoTS + 'Prestador>'
    end
    else
      Prestador := '<' + PrefixoTS + 'CpfCnpj>' +
                     GetCpfCnpj(Emitente.CNPJ, PrefixoTS) +
                   '</' + PrefixoTS + 'CpfCnpj>' +
                   GetInscMunic(Emitente.InscMun, PrefixoTS);

    if ConfigMsgDados.GerarIdentificacaoRequerente then
      Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);

    Response.XmlEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                           Requerente +
                           '<' + Prefixo + 'LoteRps' + NameSpaceLote + IdAttr  + Versao + '>' +
                             '<' + PrefixoTS + 'NumeroLote>' + Response.Lote + '</' + PrefixoTS + 'NumeroLote>' +
                             Prestador +
                             '<' + PrefixoTS + 'QuantidadeRps>' +
                                IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
                             '</' + PrefixoTS + 'QuantidadeRps>' +
                             '<' + PrefixoTS + 'ListaRps>' + ListaRps + '</' + PrefixoTS + 'ListaRps>' +
                           '</' + Prefixo + 'LoteRps>' +
                         '</' + Prefixo + TagEnvio + '>';
  end
  else
    Response.XmlEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                            ListaRps +
                         '</' + Prefixo + TagEnvio + '>';
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  NumNFSe: String;
  ANota: NotaFiscal;
  I: Integer;
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

      ANode := Document.Root;
      Response.Data := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('DataRecebimento'), tcDatHor);
      Response.Protocolo := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('Protocolo'), tcStr);

      if Response.ModoEnvio in [meLoteSincrono, meUnitario] then
      begin
        // Retorno do EnviarLoteRpsSincrono e GerarNfse
        ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');
        if not Assigned(ANode) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod202;
          AErro.Descricao := Desc202;
          Exit;
        end;

        ProcessarMensagemErros(ANode, Response);

        Response.Sucesso := (Response.Erros.Count = 0);

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
          AuxNode := AuxNode.Childrens.FindAnyNs('Numero');
          NumNFSe := AuxNode.AsString;

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

procedure TACBrNFSeProviderABRASFv2.PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse);
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse);
begin
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, TagEnvio, Prefixo, PrefixoTS, Prestador, Requerente,
  NumeroLote: string;
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
  NumeroLote := '';
  Requerente := '';

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
    if (ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.ConsultarLote.xmlns) and
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

  TagEnvio := ConfigMsgDados.ConsultarLote.DocElemento;

  Prestador :='<' + Prefixo + 'Prestador>' +
                '<' + PrefixoTS + 'CpfCnpj>' +
                  GetCpfCnpj(Emitente.CNPJ, PrefixoTS) +
                '</' + PrefixoTS + 'CpfCnpj>' +
                GetInscMunic(Emitente.InscMun, PrefixoTS) +
              '</' + Prefixo + 'Prestador>' +
              '<' + Prefixo + 'Protocolo>' +
                Response.Protocolo +
              '</' + Prefixo + 'Protocolo>';

  if Response.Lote <> '' then
    NumeroLote := '<' + Prefixo + 'NumeroLote>' +
                    Response.Lote +
                  '</' + Prefixo + 'NumeroLote>';

  if ConfigMsgDados.GerarIdentificacaoRequerente then
  begin
    Prestador := '';
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);
  end;

  Response.XmlEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                         Prestador +
                         Requerente +
                         NumeroLote +
                       '</' + Prefixo + TagEnvio + '>';
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse);
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
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Response.Situacao := '3'; // Processado com Falhas

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response);

      Response.Situacao := ProcessarConteudoXml(Document.Root.Childrens.FindAnyNs('Situacao'), tcStr);

      ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := Desc202;
        Exit;
      end;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

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
        AuxNode := AuxNode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
        AuxNode := AuxNode.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');
        AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
        AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
        AuxNode := AuxNode.Childrens.FindAnyNs('Numero');
        NumRps := AuxNode.AsString;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

        if Assigned(ANota) then
          ANota.XML := ANode.OuterXml
        else
        begin
          TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
        end;

        SalvarXmlNfse(ANota);

        Response.Situacao := '4'; // Processado com sucesso pois retornou a nota
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

procedure TACBrNFSeProviderABRASFv2.PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, TagEnvio, Prefixo, PrefixoTS, Prestador, Requerente: string;
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
    if (ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.ConsultarNFSeRps.xmlns) and
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

  TagEnvio := ConfigMsgDados.ConsultarNFSeRps.DocElemento;

  Prestador :='<' + Prefixo + 'Prestador>' +
                '<' + PrefixoTS + 'CpfCnpj>' +
                  GetCpfCnpj(Emitente.CNPJ, PrefixoTS) +
                '</' + PrefixoTS + 'CpfCnpj>' +
                GetInscMunic(Emitente.InscMun, PrefixoTS) +
              '</' + Prefixo + 'Prestador>';

  if ConfigMsgDados.GerarIdentificacaoRequerente then
  begin
    Prestador := '';
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);
  end;

  Response.XmlEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
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
                         Prestador +
                         Requerente +
                       '</' + Prefixo + TagEnvio + '>';
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse);
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

      ANode := Document.Root.Childrens.FindAnyNs('CompNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := Desc203;
        Exit;
      end;

      AuxNode := ANode.Childrens.FindAnyNs('Nfse');

      if AuxNode <> nil then
      begin
        AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
        {
        AuxNode := AuxNode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
        AuxNode := AuxNode.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');
        AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
        AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
        }
        AuxNode := AuxNode.Childrens.FindAnyNs('Numero');
        NumNFSe := AuxNode.AsString;

//        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumNFSe);
        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

        if Assigned(ANota) then
          ANota.XML := ANode.OuterXml
        else
        begin
          TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
        end;

        SalvarXmlNfse(ANota);
      end
      else
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := Desc203;
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

procedure TACBrNFSeProviderABRASFv2.PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse);
Var
  AErro: TNFSeEventoCollectionItem;
begin
  case Response.InfConsultaNFSe.tpConsulta of
    tcPorPeriodo,
    tcPorFaixa: PrepararConsultaNFSeporFaixa(Response);
    tcServicoPrestado: PrepararConsultaNFSeServicoPrestado(Response);
    tcServicoTomado: PrepararConsultaNFSeServicoTomado(Response);
  else
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod001;
      AErro.Descricao := Desc001;
    end;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.AssinarConsultaNFSe(Response: TNFSeConsultaNFSeResponse);
Var
  AErro: TNFSeEventoCollectionItem;
begin
  case Response.InfConsultaNFSe.tpConsulta of
    tcPorPeriodo,
    tcPorFaixa: AssinarConsultaNFSeporFaixa(Response);
    tcServicoPrestado: AssinarConsultaNFSeServicoPrestado(Response);
    tcServicoTomado: AssinarConsultaNFSeServicoTomado(Response);
  else
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod001;
      AErro.Descricao := Desc001;
    end;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse);
Var
  AErro: TNFSeEventoCollectionItem;
begin
  case Response.InfConsultaNFSe.tpConsulta of
    tcPorPeriodo,
    tcPorFaixa: TratarRetornoConsultaNFSeporFaixa(Response);
    tcServicoPrestado: TratarRetornoConsultaNFSeServicoPrestado(Response);
    tcServicoTomado: TratarRetornoConsultaNFSeServicoTomado(Response);
  else
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod001;
      AErro.Descricao := Desc001;
    end;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.PrepararConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  XmlConsulta, NameSpace, Prefixo, PrefixoTS, Prestador, Requerente: string;
begin
  if Response.InfConsultaNFSe.tpConsulta in [tcPorNumeroURLRetornado] then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod001;
    AErro.Descricao := Desc001;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  Prefixo := '';
  PrefixoTS := '';

  if EstaVazio(ConfigMsgDados.ConsultarNFSePorFaixa.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarNFSePorFaixa.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' + ConfigMsgDados.ConsultarNFSePorFaixa.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if (ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.ConsultarNFSePorFaixa.xmlns) and
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

  Response.Metodo := tmConsultarNFSePorFaixa;

  XmlConsulta := '<' + Prefixo + 'Faixa>' +
                   '<' + PrefixoTS + 'NumeroNfseInicial>' +
                      OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) +
                   '</' + PrefixoTS + 'NumeroNfseInicial>' +
                   '<' + PrefixoTS + 'NumeroNfseFinal>' +
                      OnlyNumber(Response.InfConsultaNFSe.NumeroFinNFSe) +
                   '</' + PrefixoTS + 'NumeroNfseFinal>' +
                 '</' + Prefixo + 'Faixa>';

  Prestador :='<' + Prefixo + 'Prestador>' +
                '<' + PrefixoTS + 'CpfCnpj>' +
                  GetCpfCnpj(Emitente.CNPJ, PrefixoTS) +
                '</' + PrefixoTS + 'CpfCnpj>' +
                GetInscMunic(Emitente.InscMun, PrefixoTS) +
              '</' + Prefixo + 'Prestador>';

  if ConfigMsgDados.GerarIdentificacaoRequerente then
  begin
    Prestador := '';
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);
  end;

  Response.XmlEnvio := '<' + Prefixo + 'ConsultarNfseFaixaEnvio' + NameSpace + '>' +
                         Prestador +
                         Requerente +
                         XmlConsulta +
                         '<' + Prefixo + 'Pagina>' +
                            IntToStr(Response.InfConsultaNFSe.Pagina) +
                         '</' + Prefixo + 'Pagina>' +
                       '</' + Prefixo + 'ConsultarNfseFaixaEnvio>';
end;

procedure TACBrNFSeProviderABRASFv2.AssinarConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.ConsultarNFSePorFaixa then Exit;

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
      Prefixo + ConfigMsgDados.ConsultarNFSePorFaixa.DocElemento,
      ConfigMsgDados.ConsultarNFSePorFaixa.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod999;
      AErro.Descricao := E.Message;
    end;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse);
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
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response);

      ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := Desc202;
        Exit;
      end;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

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
        {
        AuxNode := AuxNode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
        AuxNode := AuxNode.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');
        AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
        AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
        }
        AuxNode := AuxNode.Childrens.FindAnyNs('Numero');
        NumNFSe := AuxNode.AsString;

//        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumNFSe);
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

procedure TACBrNFSeProviderABRASFv2.PrepararConsultaNFSeServicoPrestado(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  XmlConsulta, NameSpace, Prefixo, PrefixoTS, Prestador, Requerente: string;
begin
  if Response.InfConsultaNFSe.tpConsulta in [tcPorNumeroURLRetornado] then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod001;
    AErro.Descricao := Desc001;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  Prefixo := '';
  PrefixoTS := '';

  if EstaVazio(ConfigMsgDados.ConsultarNFSeServicoPrestado.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarNFSeServicoPrestado.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' + ConfigMsgDados.ConsultarNFSeServicoPrestado.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if (ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.ConsultarNFSeServicoPrestado.xmlns) and
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

  Response.Metodo := tmConsultarNFSeServicoPrestado;

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
    XmlConsulta := XmlConsulta +
                     '<' + Prefixo + 'Intermediario>' +
                       '<' + PrefixoTS + 'CpfCnpj>' +
                          GetCpfCnpj(Response.InfConsultaNFSe.CNPJInter, PrefixoTS) +
                       '</' + PrefixoTS + 'CpfCnpj>' +
                       GetInscMunic(Response.InfConsultaNFSe.IMInter, PrefixoTS) +
                     '</' + Prefixo + 'Intermediario>';
  end;

  Prestador :='<' + Prefixo + 'Prestador>' +
                '<' + PrefixoTS + 'CpfCnpj>' +
                  GetCpfCnpj(Emitente.CNPJ, PrefixoTS) +
                '</' + PrefixoTS + 'CpfCnpj>' +
                GetInscMunic(Emitente.InscMun, PrefixoTS) +
              '</' + Prefixo + 'Prestador>';

  if ConfigMsgDados.GerarIdentificacaoRequerente then
  begin
    Prestador := '';
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);
  end;

  Response.XmlEnvio := '<' + Prefixo + 'ConsultarNfseServicoPrestadoEnvio' + NameSpace + '>' +
                         Prestador +
                         Requerente +
                         XmlConsulta +
                         '<' + Prefixo + 'Pagina>' +
                            IntToStr(Response.InfConsultaNFSe.Pagina) +
                         '</' + Prefixo + 'Pagina>' +
                       '</' + Prefixo + 'ConsultarNfseServicoPrestadoEnvio>';
end;

procedure TACBrNFSeProviderABRASFv2.AssinarConsultaNFSeServicoPrestado(
  Response: TNFSeConsultaNFSeResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.ConsultarNFSeServicoPrestado then Exit;

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
      Prefixo + ConfigMsgDados.ConsultarNFSeServicoPrestado.DocElemento,
      ConfigMsgDados.ConsultarNFSeServicoPrestado.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod999;
      AErro.Descricao := E.Message;
    end;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultaNFSeServicoPrestado(
  Response: TNFSeConsultaNFSeResponse);
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
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response);

      ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := Desc202;
        Exit;
      end;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

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
        {
        AuxNode := AuxNode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
        AuxNode := AuxNode.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');
        AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
        AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
        }
        AuxNode := AuxNode.Childrens.FindAnyNs('Numero');
        NumNFSe := AuxNode.AsString;

//        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumNFSe);
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

procedure TACBrNFSeProviderABRASFv2.PrepararConsultaNFSeServicoTomado(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  XmlConsulta, NameSpace, Prefixo, PrefixoTS, Consulente, Requerente: string;
begin
  if Response.InfConsultaNFSe.tpConsulta in [tcPorNumeroURLRetornado] then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod001;
    AErro.Descricao := desc001;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  Prefixo := '';
  PrefixoTS := '';

  if EstaVazio(ConfigMsgDados.ConsultarNFSeServicoTomado.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarNFSeServicoTomado.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' + ConfigMsgDados.ConsultarNFSeServicoTomado.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if (ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.ConsultarNFSeServicoTomado.xmlns) and
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

  Response.Metodo := tmConsultarNFSeServicoTomado;

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

  if NaoEstaVAzio(Response.InfConsultaNFSe.CNPJPrestador) then
  begin
    XmlConsulta := XmlConsulta +
                     '<' + Prefixo + 'Prestador>' +
                       '<' + PrefixoTS + 'CpfCnpj>' +
                          GetCpfCnpj(Response.InfConsultaNFSe.CNPJPrestador, PrefixoTS) +
                       '</' + PrefixoTS + 'CpfCnpj>' +
                       GetInscMunic(Response.InfConsultaNFSe.IMPrestador, PrefixoTS) +
                     '</' + Prefixo + 'Prestador>';
  end;

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
    XmlConsulta := XmlConsulta +
                     '<' + Prefixo + 'Intermediario>' +
                       '<' + PrefixoTS + 'CpfCnpj>' +
                          GetCpfCnpj(Response.InfConsultaNFSe.CNPJInter, PrefixoTS) +
                       '</' + PrefixoTS + 'CpfCnpj>' +
                       GetInscMunic(Response.InfConsultaNFSe.IMInter, PrefixoTS) +
                     '</' + Prefixo + 'Intermediario>';
  end;

  Consulente :='<' + Prefixo + 'Consulente>' +
                 '<' + PrefixoTS + 'CpfCnpj>' +
                   GetCpfCnpj(Emitente.CNPJ, PrefixoTS) +
                 '</' + PrefixoTS + 'CpfCnpj>' +
                 GetInscMunic(Emitente.InscMun, PrefixoTS) +
               '</' + Prefixo + 'Consulente>';

  if ConfigMsgDados.GerarIdentificacaoRequerente then
  begin
    Consulente := '';
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);
  end;

  Response.XmlEnvio := '<' + Prefixo + 'ConsultarNfseServicoTomadoEnvio' + NameSpace + '>' +
                         Consulente +
                         Requerente +
                         XmlConsulta +
                         '<' + Prefixo + 'Pagina>' +
                            IntToStr(Response.InfConsultaNFSe.Pagina) +
                         '</' + Prefixo + 'Pagina>' +
                       '</' + Prefixo + 'ConsultarNfseServicoTomadoEnvio>';
end;

procedure TACBrNFSeProviderABRASFv2.AssinarConsultaNFSeServicoTomado(
  Response: TNFSeConsultaNFSeResponse);
var
  IdAttr, Prefixo: string;
  AErro: TNFSeEventoCollectionItem;
begin
  if not ConfigAssinar.ConsultarNFSeServicoTomado then Exit;

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
      Prefixo + ConfigMsgDados.ConsultarNFSeServicoTomado.DocElemento,
      ConfigMsgDados.ConsultarNFSeServicoTomado.InfElemento, '', '', '', IdAttr);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod999;
      AErro.Descricao := E.Message;
    end;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultaNFSeServicoTomado(
  Response: TNFSeConsultaNFSeResponse);
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
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response);

      ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := Desc202;
        Exit;
      end;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

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
        {
        AuxNode := AuxNode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
        AuxNode := AuxNode.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');
        AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
        AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
        }
        AuxNode := AuxNode.Childrens.FindAnyNs('Numero');
        NumNFSe := AuxNode.AsString;

//        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumNFSe);
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

procedure TACBrNFSeProviderABRASFv2.PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  InfoCanc: TInfCancelamento;
  IdAttr, NameSpace, NameSpaceCanc, xMotivo, xCodVerif, Prefixo, PrefixoTS,
  xSerie, Requerente, ChavedeAcesso: string;
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
  InfoCanc := Response.InfCancelamento;
  Prefixo := '';
  PrefixoTS := '';

  if EstaVazio(ConfigMsgDados.CancelarNFSe.xmlns) then
  begin
    NameSpace := '';
    NameSpaceCanc := '';
  end
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.CancelarNFSe.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' + ConfigMsgDados.CancelarNFSe.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;

    NameSpaceCanc := NameSpace;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if (ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.CancelarNFSe.xmlns) and
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

  if ConfigGeral.Identificador <> '' then
    IdAttr := ' ' + ConfigGeral.Identificador + '="Canc_' +
                    OnlyNumber(Emitente.CNPJ) + OnlyNumber(Emitente.InscMun) +
                    InfoCanc.NumeroNFSe + '"'
  else
    IdAttr := '';

  xSerie := Trim(InfoCanc.SerieNFSe);

  if xSerie <> '' then
    xSerie := '<' + PrefixoTS + 'Serie>' +
                 xSerie +
              '</' + PrefixoTS + 'Serie>';

  xMotivo := Trim(InfoCanc.MotCancelamento);

  if xMotivo <> '' then
    xMotivo := '<' + Prefixo + 'MotivoCancelamento>' +
                 xMotivo +
               '</' + Prefixo + 'MotivoCancelamento>';

  xCodVerif := Trim(InfoCanc.CodVerificacao);

  if xCodVerif <> '' then
    xCodVerif := '<CodigoVerificacao>' + xCodVerif + '</CodigoVerificacao>';

  if ConfigMsgDados.GerarIdentificacaoRequerente then
  begin
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);
    ChavedeAcesso := '<ChaveAcesso>' + Trim(InfoCanc.CodVerificacao) + '</ChaveAcesso>';
    xCodVerif := '';
  end;

  Response.XmlEnvio := '<' + Prefixo + 'CancelarNfseEnvio' + NameSpace + '>' +
                         Requerente +
                         '<' + PrefixoTS + 'Pedido>' +
                           '<' + PrefixoTS + 'InfPedidoCancelamento' + IdAttr + NameSpaceCanc + '>' +
                             '<' + PrefixoTS + 'IdentificacaoNfse>' +
                               '<' + PrefixoTS + 'Numero>' +
                                  InfoCanc.NumeroNFSe +
                               '</' + PrefixoTS + 'Numero>' +
                               xSerie +
                               '<' + PrefixoTS + 'CpfCnpj>' +
                                 GetCpfCnpj(Emitente.CNPJ, PrefixoTS) +
                               '</' + PrefixoTS + 'CpfCnpj>' +
                               GetInscMunic(Emitente.InscMun, PrefixoTS) +
                               '<' + PrefixoTS + 'CodigoMunicipio>' +
                                  IntToStr(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                               '</' + PrefixoTS + 'CodigoMunicipio>' +
                               xCodVerif +
                             '</' + PrefixoTS + 'IdentificacaoNfse>' +
                             ChavedeAcesso +
                             '<' + PrefixoTS + 'CodigoCancelamento>' +
                                InfoCanc.CodCancelamento +
                             '</' + PrefixoTS + 'CodigoCancelamento>' +
                             xMotivo +
                           '</' + PrefixoTS + 'InfPedidoCancelamento>' +
                         '</' + PrefixoTS + 'Pedido>' +
                       '</' + Prefixo + 'CancelarNfseEnvio>';
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  Ret: TRetCancelamento;
  IdAttr: string;
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

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root.Childrens.FindAnyNs('RetCancelamento');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod209;
        AErro.Descricao := Desc209;
        Exit;
      end;

      ANode := ANode.Childrens.FindAnyNs('NfseCancelamento');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod210;
        AErro.Descricao := Desc210;
        Exit;
      end;

      ANode := ANode.Childrens.FindAnyNs('Confirmacao');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod204;
        AErro.Descricao := Desc204;
        Exit;
      end;

      Ret :=  Response.RetCancelamento;
      Ret.DataHora := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('DataHora'), tcDatHor);

      if ConfigAssinar.IncluirURI then
        IdAttr := ConfigGeral.Identificador
      else
        IdAttr := 'ID';

      ANode := ANode.Childrens.FindAnyNs('Pedido').Childrens.FindAnyNs('InfPedidoCancelamento');
      Ret.Pedido.InfID.ID := ANode.Attributes.Items[IdAttr].Content;
      Ret.Pedido.CodigoCancelamento := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('CodigoCancelamento'), tcStr);

      ANode := ANode.Childrens.FindAnyNs('IdentificacaoNfse');

      with  Ret.Pedido.IdentificacaoNfse do
      begin
        Numero := ProcessarConteudoXml(ANode.Childrens.FindAnyNs('Numero'), tcStr);

        AuxNode := ANode.Childrens.FindAnyNs('CpfCnpj');
        Cnpj := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('Cnpj'), tcStr);

        if Cnpj = '' then
          Cnpj := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('Cpf'), tcStr);

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

procedure TACBrNFSeProviderABRASFv2.PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse);
var
  IdAttr, xRps, NameSpace, NumRps, TagEnvio, Prefixo, PrefixoTS,
  Requerente: string;
  Emitente: TEmitenteConfNFSe;
  AErro: TNFSeEventoCollectionItem;
  Nota: NotaFiscal;
begin
  if EstaVazio(Response.PedCanc) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod107;
    AErro.Descricao := Desc107;
    Exit;
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count <= 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod002;
    AErro.Descricao := Desc002;
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count > 1 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod003;
    AErro.Descricao := 'Conjunto de RPS transmitidos (máximo de 1 RPS)' +
                       ' excedido. Quantidade atual: ' +
                       IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count);
  end;

  if Response.Erros.Count > 0 then Exit;

  Prefixo := '';
  PrefixoTS := '';

  if EstaVazio(ConfigMsgDados.SubstituirNFSe.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.SubstituirNFSe.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' + ConfigMsgDados.SubstituirNFSe.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if (ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.SubstituirNFSe.xmlns) and
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

  Nota := TACBrNFSeX(FAOwner).NotasFiscais.Items[0];

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if EstaVazio(Nota.XMLAssinado) then
  begin
    Nota.GerarXML;
    if ConfigAssinar.RpsSubstituirNFSe then
    begin
      Nota.XMLOriginal := FAOwner.SSL.Assinar(ConverteXMLtoUTF8(Nota.XMLOriginal),
                                              PrefixoTS + ConfigMsgDados.XmlRps.DocElemento,
                                              ConfigMsgDados.XmlRps.InfElemento, '', '', '', IdAttr);
    end;
  end;

  if FAOwner.Configuracoes.Arquivos.Salvar then
  begin
    if NaoEstaVazio(Nota.NomeArqRps) then
      TACBrNFSeX(FAOwner).Gravar(Nota.NomeArqRps, Nota.XMLOriginal)
    else
    begin
      Nota.NomeArqRps := Nota.CalcularNomeArquivoCompleto(Nota.NomeArqRps, '');
      TACBrNFSeX(FAOwner).Gravar(Nota.NomeArqRps, Nota.XMLOriginal);
    end;
  end;

  NumRps := Nota.NFSe.IdentificacaoRps.Numero;

  xRps := RemoverDeclaracaoXML(Nota.XMLOriginal);
  xRps := PrepararRpsParaLote(xRps);

  if ConfigGeral.Identificador <> '' then
    IdAttr := ' ' + ConfigGeral.Identificador + '="Sub_' + OnlyNumber(NumRps) + '"'
  else
    IdAttr := '';

  {
    No serviço de Substituição de NFS-e temos o pedido de cancelamento de uma
    NFS-e mais o RPS que vai ser convertido na NFS-e substituta.

    A NFS-e substituta substitui a NFS-e Cancelada.

    (Response.PedCanc) contem o pedido de cancelamento da NFS-e existente.
    (xRps) contem o RPS que será convertido na NFS-e substituta.
  }

  TagEnvio := ConfigMsgDados.SubstituirNFSe.DocElemento;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if ConfigMsgDados.GerarIdentificacaoRequerente then
  begin
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);
    Response.PedCanc := RetornarConteudoEntre(Response.PedCanc,
                                                 '<Pedido>', '</Pedido>', True);
  end;

  Response.XmlEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                         Requerente +
                         '<' + Prefixo + 'SubstituicaoNfse' + IdAttr + '>' +
                           Response.PedCanc +
                           xRps +
                         '</' + Prefixo + 'SubstituicaoNfse>' +
                       '</' + Prefixo + TagEnvio + '>';
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse);
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
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := Desc201;
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response);

      ANode := Document.Root.Childrens.FindAnyNs('RetSubstituicao');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod205;
        AErro.Descricao := Desc205;
        Exit;
      end;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := ANode.Childrens.FindAnyNs('NfseSubstituida');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod206;
        AErro.Descricao := Desc206;
        Exit;
      end
      else
      begin
        ANode := ANode.Childrens.FindAnyNs('CompNfse');
        if not Assigned(ANode) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := Desc203;
          Exit;
        end;

        AuxNode := ANode.Childrens.FindAnyNs('Nfse');
        AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
        AuxNode := AuxNode.Childrens.FindAnyNs('Numero');

        if AuxNode <> nil then
        begin
          NumNFSe := AuxNode.AsString;

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
      end;

      ANode := Document.Root.Childrens.FindAnyNs('NfseSubstituidora');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod207;
        AErro.Descricao := Desc207;
        Exit;
      end
      else
      begin
        ANode := ANode.Childrens.FindAnyNs('CompNfse');
        if not Assigned(ANode) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := Desc203;
          Exit;
        end;

        AuxNode := ANode.Childrens.FindAnyNs('Nfse');
        AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
        AuxNode := AuxNode.Childrens.FindAnyNs('Numero');

        if AuxNode <> nil then
        begin
          NumNFSe := AuxNode.AsString;

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

procedure TACBrNFSeProviderABRASFv2.ProcessarMensagemErros(const RootNode: TACBrXmlNode;
  const Response: TNFSeWebserviceResponse; AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode.Childrens.FindAnyNs('MensagemRetorno');

  if (ANode = nil) then Exit;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if Assigned(ANodeArray) then
  begin
    for I := Low(ANodeArray) to High(ANodeArray) do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Codigo'), tcStr);
      AErro.Descricao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Mensagem'), tcStr);
      AErro.Correcao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Correcao'), tcStr);
    end;
  end;

  {
    Para os serviços que recepcionam o Lote de Rps tanto no modo assíncrono
    quanto no modo síncrono do provedor RLZ o nome da tag é diferente.
  }
  ANode := RootNode.Childrens.FindAnyNs('ListaMensagemRetornoLote');

  if (ANode = nil) then Exit;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);
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
