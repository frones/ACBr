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
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXProviderBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeProviderABRASFv1 = class(TACBrNFSeXProvider)
  protected
    FpFormatoDataRecebimento: TACBrTipoCampo;
    FpFormatoDataEmissao: TACBrTipoCampo;
    FpFormatoDataHora: TACBrTipoCampo;

    function PreencherNotaRespostaConsultaLoteRps(Node, parentNode: TACBrXmlNode;
      Response: TNFSeConsultaLoteRpsResponse): Boolean;
    function PreencherNotaRespostaConsultaNFSe(Node, parentNode: TACBrXmlNode;
      Response: TNFSeConsultaNFSeResponse): Boolean;
    procedure LerCancelamento(ANode: TACBrXmlNode;
      Response: TNFSeConsultaNFSeporRpsResponse);

    procedure Configuracao; override;

    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); override;
    procedure GerarMsgDadosConsultaSituacao(Response: TNFSeConsultaSituacaoResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); override;

    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    procedure GerarMsgDadosConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;

    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure GerarMsgDadosConsultaporRps(Response: TNFSeConsultaNFSeporRpsResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;

    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
    procedure GerarMsgDadosConsultaNFSe(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); override;
    procedure GerarMsgDadosConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure GerarMsgDadosCancelaNFSe(Response: TNFSeCancelaNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); override;
    procedure GerarMsgDadosSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); override;

    procedure PrepararGerarToken(Response: TNFSeGerarTokenResponse); override;
    procedure GerarMsgDadosGerarToken(Response: TNFSeGerarTokenResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoGerarToken(Response: TNFSeGerarTokenResponse); override;

    procedure PrepararEnviarEvento(Response: TNFSeEnviarEventoResponse); override;
    procedure GerarMsgDadosEnviarEvento(Response: TNFSeEnviarEventoResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoEnviarEvento(Response: TNFSeEnviarEventoResponse); override;

    procedure PrepararConsultarEvento(Response: TNFSeConsultarEventoResponse); override;
    procedure GerarMsgDadosConsultarEvento(Response: TNFSeConsultarEventoResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoConsultarEvento(Response: TNFSeConsultarEventoResponse); override;

    procedure PrepararConsultarDFe(Response: TNFSeConsultarDFeResponse); override;
    procedure GerarMsgDadosConsultarDFe(Response: TNFSeConsultarDFeResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoConsultarDFe(Response: TNFSeConsultarDFeResponse); override;

    procedure PrepararConsultarParam(Response: TNFSeConsultarParamResponse); override;
    procedure GerarMsgDadosConsultarParam(Response: TNFSeConsultarParamResponse;
      Params: TNFSeParamsResponse); override;
    procedure TratarRetornoConsultarParam(Response: TNFSeConsultarParamResponse); override;

    procedure PrepararConsultarSeqRps(Response: TNFSeConsultarSeqRpsResponse); override;
    procedure GerarMsgDadosConsultarSeqRps(Response: TNFSeConsultarSeqRpsResponse); override;
    procedure TratarRetornoConsultarSeqRps(Response: TNFSeConsultarSeqRpsResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'ListaMensagemRetorno';
                                     const AMessageTag: string = 'MensagemRetorno'); virtual;

  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXNotasFiscais, ACBrNFSeXConsts,
  ACBrNFSeXConversao, ACBrNFSeXWebserviceBase;

{ TACBrNFSeProviderABRASFv1 }

procedure TACBrNFSeProviderABRASFv1.Configuracao;
const
  NameSpace = 'http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd';
begin
  inherited Configuracao;

  FpFormatoDataRecebimento := tcDatHor;
  FpFormatoDataEmissao := tcDatHor;
  FpFormatoDataHora := tcDatHor;

  // Todos os provedores que seguem a versão 1 do layout da ABRASF só tem
  // um serviço para recepcionar o RPS e é assíncrono.
  with ConfigGeral do
  begin
    Layout := loABRASF;
    ModoEnvio := meLoteAssincrono;
    ConsultaSitLote := True;
    ConsultaPorFaixa := False;
  end;

  SetXmlNameSpace(NameSpace);

  with ConfigWebServices do
  begin
    VersaoDados := '1.00';
    VersaoAtrib := '1.00';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');
end;

function TACBrNFSeProviderABRASFv1.PreencherNotaRespostaConsultaLoteRps(Node,
  parentNode: TACBrXmlNode; Response: TNFSeConsultaLoteRpsResponse): Boolean;
var
  NumNFSe, CodVerif, NumRps, SerieRps: String;
  ANota: TNotaFiscal;
  AResumo: TNFSeResumoCollectionItem;
  NumeroRps: Integer;
begin
  Result := False;
  NumeroRps := 0;

  if Node <> nil then
  begin
    Node := Node.Childrens.FindAnyNs('InfNfse');
    if not Assigned(Node) or (Node = nil) then Exit;

    NumNFSe := ObterConteudoTag(Node.Childrens.FindAnyNs('Numero'), tcStr);
    CodVerif := ObterConteudoTag(Node.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);

    Node := Node.Childrens.FindAnyNs('IdentificacaoRps');

    NumRps := '';
    SerieRps := '';

    if Node <> nil then
    begin
      NumRps := ObterConteudoTag(Node.Childrens.FindAnyNs('Numero'), tcStr);
      NumeroRps := StrToIntDef(NumRps, 0);
      SerieRps := ObterConteudoTag(Node.Childrens.FindAnyNs('Serie'), tcStr);
    end;

    AResumo := Response.Resumos.New;
    AResumo.NumeroNota := NumNFSe;
    AResumo.CodigoVerificacao := CodVerif;
    AResumo.NumeroRps := NumRps;
    AResumo.SerieRps := SerieRps;

    with Response do
    begin
      NumeroNota := NumNFSe;
      CodigoVerificacao := CodVerif;
      NumeroRps := NumRps;
      SerieRps := SerieRps;
    end;

    if NumeroRps > 0 then
      ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps)
    else
      ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

    ANota := CarregarXmlNfse(ANota, parentNode.OuterXml);
    SalvarXmlNfse(ANota);
    Result := True; // Processado com sucesso pois retornou a nota
  end;
end;

function TACBrNFSeProviderABRASFv1.PreencherNotaRespostaConsultaNFSe(Node,
  parentNode: TACBrXmlNode; Response: TNFSeConsultaNFSeResponse): Boolean;
var
  NumNFSe, CodVerif, NumRps, SerieRps: String;
  ANota: TNotaFiscal;
  AResumo: TNFSeResumoCollectionItem;
  NumeroRps: Integer;
begin
  Result := False;
  NumeroRps := 0;

  if Node <> nil then
  begin
    Node := Node.Childrens.FindAnyNs('InfNfse');
    if not Assigned(Node) or (Node = nil) then Exit;

    NumNFSe := ObterConteudoTag(Node.Childrens.FindAnyNs('Numero'), tcStr);
    CodVerif := ObterConteudoTag(Node.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);

    Node := Node.Childrens.FindAnyNs('IdentificacaoRps');

    NumRps := '';
    SerieRps := '';

    if Node <> nil then
    begin
      NumRps := ObterConteudoTag(Node.Childrens.FindAnyNs('Numero'), tcStr);
      NumeroRps := StrToIntDef(NumRps, 0);
      SerieRps := ObterConteudoTag(Node.Childrens.FindAnyNs('Serie'), tcStr);
    end;

    AResumo := Response.Resumos.New;
    AResumo.NumeroNota := NumNFSe;
    AResumo.CodigoVerificacao := CodVerif;
    AResumo.NumeroRps := NumRps;
    AResumo.SerieRps := SerieRps;

    with Response do
    begin
      NumeroNota := NumNFSe;
      CodigoVerificacao := CodVerif;
      NumeroRps := NumRps;
      SerieRps := SerieRps;
    end;

    if NumeroRps > 0 then
      ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps)
    else
      ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

    ANota := CarregarXmlNfse(ANota, parentNode.OuterXml);
    SalvarXmlNfse(ANota);
    Result := True; // Processado com sucesso pois retornou a nota
  end;
end;

procedure TACBrNFSeProviderABRASFv1.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  aParams: TNFSeParamsResponse;
  Nota: TNotaFiscal;
  Versao, IdAttr, NameSpace, NameSpaceLote, ListaRps, xRps,
  TagEnvio, Prefixo, PrefixoTS: string;
  I: Integer;
begin
  if Response.ModoEnvio in [meLoteSincrono, meUnitario, meTeste] then
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

  if TACBrNFSeX(FAOwner).NotasFiscais.Count < Response.MinRps then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod005;
    AErro.Descricao := ACBrStr('Conjunto de RPS transmitidos (mínimo de ' +
                       IntToStr(Response.MinRps) + ' RPS)' +
                       '. Quantidade atual: ' +
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

  if ConfigWebServices.AtribVerLote <> '' then
    Versao := ' ' + ConfigWebServices.AtribVerLote + '="' +
              ConfigWebServices.VersaoDados + '"'
  else
    Versao := '';

  IdAttr := DefinirIDLote(Response.NumeroLote);

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

procedure TACBrNFSeProviderABRASFv1.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    if Response.ModoEnvio in [meLoteAssincrono] then
      Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                             '<' + Prefixo + 'LoteRps' + NameSpace2 + IdAttr  + Versao + '>' +
                               '<' + Prefixo2 + 'NumeroLote>' + Response.NumeroLote + '</' + Prefixo2 + 'NumeroLote>' +
                               '<' + Prefixo2 + 'Cnpj>' + OnlyNumber(Emitente.CNPJ) + '</' + Prefixo2 + 'Cnpj>' +
                               GetInscMunic(Emitente.InscMun, Prefixo2) +
                               '<' + Prefixo2 + 'QuantidadeRps>' +
                                  IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
                               '</' + Prefixo2 + 'QuantidadeRps>' +
                               '<' + Prefixo2 + 'ListaRps>' +
                                 Xml +
                               '</' + Prefixo2 + 'ListaRps>' +
                             '</' + Prefixo + 'LoteRps>' +
                           '</' + Prefixo + TagEnvio + '>'
    else
      Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                              Xml +
                           '</' + Prefixo + TagEnvio + '>';
  end;
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

      Response.Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('DataRecebimento'), FpFormatoDataRecebimento);
      Response.Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('Protocolo'), tcStr);
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

procedure TACBrNFSeProviderABRASFv1.PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse);
var
  AErro: TNFSeEventoCollectionItem;
  aParams: TNFSeParamsResponse;
  NameSpace, Prefixo, PrefixoTS: string;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := ACBrStr(Desc101);
    Exit;
  end;

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
    if (ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.ConsultarSituacao.xmlns) and
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

  aParams := TNFSeParamsResponse.Create;
  try
    aParams.Clear;
    aParams.Xml := '';
    aParams.TagEnvio := '';
    aParams.Prefixo := Prefixo;
    aParams.Prefixo2 := PrefixoTS;
    aParams.NameSpace := NameSpace;
    aParams.NameSpace2 := '';
    aParams.IdAttr := '';
    aParams.Versao := '';

    GerarMsgDadosConsultaSituacao(Response, aParams);
  finally
    aParams.Free;
  end;
end;

procedure TACBrNFSeProviderABRASFv1.GerarMsgDadosConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Response.ArquivoEnvio := '<' + Prefixo + 'ConsultarSituacaoLoteRpsEnvio' + NameSpace + '>' +
                           '<' + Prefixo + 'Prestador>' +
                             '<' + Prefixo2 + 'Cnpj>' +
                               OnlyNumber(Emitente.CNPJ) +
                             '</' + Prefixo2 + 'Cnpj>' +
                             GetInscMunic(Emitente.InscMun, Prefixo2) +
                           '</' + Prefixo + 'Prestador>' +
                           '<' + Prefixo + 'Protocolo>' +
                             Response.Protocolo +
                           '</' + Prefixo + 'Protocolo>' +
                         '</' + Prefixo + 'ConsultarSituacaoLoteRpsEnvio>';
  end;
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
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

      Response.NumeroLote := ObterConteudoTag(Document.Root.Childrens.FindAnyNs('NumeroLote'), tcStr);
      Response.Situacao := ObterConteudoTag(Document.Root.Childrens.FindAnyNs('Situacao'), tcStr);

      if not Response.Sucesso then
        Response.Situacao := '3';

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

procedure TACBrNFSeProviderABRASFv1.PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  aParams: TNFSeParamsResponse;
  NameSpace, Prefixo, PrefixoTS: string;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := ACBrStr(Desc101);
    Exit;
  end;

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

  aParams := TNFSeParamsResponse.Create;
  try
    aParams.Clear;
    aParams.Xml := '';
    aParams.TagEnvio := '';
    aParams.Prefixo := Prefixo;
    aParams.Prefixo2 := PrefixoTS;
    aParams.NameSpace := NameSpace;
    aParams.NameSpace2 := '';
    aParams.IdAttr := '';
    aParams.Versao := '';

    GerarMsgDadosConsultaLoteRps(Response, aParams);
  finally
    aParams.Free;
  end;
end;

procedure TACBrNFSeProviderABRASFv1.GerarMsgDadosConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Response.ArquivoEnvio := '<' + Prefixo + 'ConsultarLoteRpsEnvio' + NameSpace + '>' +
                           '<' + Prefixo + 'Prestador>' +
                             '<' + Prefixo2 + 'Cnpj>' +
                               OnlyNumber(Emitente.CNPJ) +
                             '</' + Prefixo2 + 'Cnpj>' +
                             GetInscMunic(Emitente.InscMun, Prefixo2) +
                           '</' + Prefixo + 'Prestador>' +
                           '<' + Prefixo + 'Protocolo>' +
                             Response.Protocolo +
                           '</' + Prefixo + 'Protocolo>' +
                         '</' + Prefixo + 'ConsultarLoteRpsEnvio>';
  end;
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  ANode, ANode2, AuxNode: TACBrXmlNode;
  ANodeArray, AuxNodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  I, J: Integer;
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

      Document.LoadFromXml(Response.ArquivoRetorno);

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      if Response.Sucesso then
      begin
        Response.Situacao := ObterConteudoTag(Document.Root.Childrens.FindAnyNs('SituacaoLoteRps'), tcStr);

        ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');

        if not Assigned(ANode) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod202;
          AErro.Descricao := ACBrStr(Desc202);
          Exit;
        end;

        ANodeArray := ANode.Childrens.FindAllAnyNs('CompNfse');
        if ANodeArray = nil then
          ANodeArray := ANode.Childrens.FindAllAnyNs('ComplNfse');

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
          AuxNode := ANode.Childrens.FindAnyNs('tcCompNfse');

          if AuxNode = nil then
          begin
            AuxNode := ANode.Childrens.FindAnyNs('Nfse');

            if PreencherNotaRespostaConsultaLoteRps(AuxNode, ANode, Response) then
              Response.Situacao := '4' // Processado com sucesso pois retornou a nota
            else
            begin
              AErro := Response.Erros.New;
              AErro.Codigo := Cod203;
              AErro.Descricao := ACBrStr(Desc203);
              Exit;
            end;
          end
          else
          begin
            AuxNodeArray := ANode.Childrens.FindAllAnyNs('tcCompNfse');

            for J := Low(AuxNodeArray) to High(AuxNodeArray) do
            begin
              ANode2 := AuxNodeArray[J];
              AuxNode := ANode2.Childrens.FindAnyNs('Nfse');

              if PreencherNotaRespostaConsultaLoteRps(AuxNode, ANode2, Response) then
                Response.Situacao := '4' // Processado com sucesso pois retornou a nota
              else
              begin
                AErro := Response.Erros.New;
                AErro.Codigo := Cod203;
                AErro.Descricao := ACBrStr(Desc203);
                Exit;
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

procedure TACBrNFSeProviderABRASFv1.PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  aParams: TNFSeParamsResponse;
  NameSpace, Prefixo, PrefixoTS, TagEnvio: string;
begin
  if EstaVazio(Response.NumeroRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := ACBrStr(Desc102);
    Exit;
  end;

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

  aParams := TNFSeParamsResponse.Create;
  try
    aParams.Clear;
    aParams.Xml := '';
    aParams.TagEnvio := TagEnvio;
    aParams.Prefixo := Prefixo;
    aParams.Prefixo2 := PrefixoTS;
    aParams.NameSpace := NameSpace;
    aParams.NameSpace2 := '';
    aParams.IdAttr := '';
    aParams.Versao := '';

    GerarMsgDadosConsultaporRps(Response, aParams);
  finally
    aParams.Free;
  end;
end;

procedure TACBrNFSeProviderABRASFv1.GerarMsgDadosConsultaporRps(
  Response: TNFSeConsultaNFSeporRpsResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                           '<' + Prefixo + 'IdentificacaoRps>' +
                             '<' + Prefixo2 + 'Numero>' +
                               Response.NumeroRps +
                             '</' + Prefixo2 + 'Numero>' +
                             '<' + Prefixo2 + 'Serie>' +
                               Response.SerieRps +
                             '</' + Prefixo2 + 'Serie>' +
                             '<' + Prefixo2 + 'Tipo>' +
                               Response.TipoRps +
                             '</' + Prefixo2 + 'Tipo>' +
                           '</' + Prefixo + 'IdentificacaoRps>' +
                           '<' + Prefixo + 'Prestador>' +
                             '<' + Prefixo2 + 'Cnpj>' +
                               OnlyNumber(Emitente.CNPJ) +
                             '</' + Prefixo2 + 'Cnpj>' +
                             GetInscMunic(Emitente.InscMun, Prefixo2) +
                           '</' + Prefixo + 'Prestador>' +
                         '</' + Prefixo + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  AErro: TNFSeEventoCollectionItem;
  ANota: TNotaFiscal;
  NumNFSe, InfNfseID: String;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      TACBrNFSeX(FAOwner).NotasFiscais.Clear;

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

      if Response.Sucesso then
      begin
        ANode := Document.Root.Childrens.FindAnyNs('CompNfse');
        if ANode = nil then
          ANode := Document.Root.Childrens.FindAnyNs('ComplNfse');

        if not Assigned(ANode) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit;
        end;

        AuxNode := ANode.Childrens.FindAnyNs('tcCompNfse');

        if AuxNode = nil then
        begin
          LerCancelamento(ANode, Response);

          AuxNode := ANode.Childrens.FindAnyNs('Nfse')
        end
        else
        begin
          LerCancelamento(AuxNode, Response);

          AuxNode := AuxNode.Childrens.FindAnyNs('Nfse');
        end;

        if AuxNode <> nil then
        begin
          AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
          if not Assigned(AuxNode) or (AuxNode = nil) then Exit;

          InfNfseID := ObterConteudoTag(AuxNode.Attributes.Items['Id']);
          NumNFSe := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

          with Response do
          begin
            NumeroNota := NumNFSe;
            idNota := InfNfseID;
            CodigoVerificacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
            Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('DataEmissao'), FpFormatoDataEmissao);
          end;

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

          ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
          SalvarXmlNfse(ANota);
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

procedure TACBrNFSeProviderABRASFv1.PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  aParams: TNFSeParamsResponse;
  XmlConsulta, RazaoInter, NameSpace, Prefixo, PrefixoTS, TagEnvio: string;
begin
  if Response.InfConsultaNFSe.tpConsulta in [tcPorFaixa, tcServicoTomado] then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod001;
    AErro.Descricao := ACBrStr(Desc001);
    Exit;
  end;

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
    if (ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.ConsultarNFSe.xmlns) and
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

  if OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) <> '' then
    XmlConsulta := '<' + Prefixo + 'NumeroNfse>' +
                      OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) +
                   '</' + Prefixo + 'NumeroNfse>'
  else
    XmlConsulta := '';

  if (Response.InfConsultaNFSe.DataInicial > 0) and (Response.InfConsultaNFSe.DataFinal > 0) then
    XmlConsulta := XmlConsulta +
                     '<' + Prefixo + 'PeriodoEmissao>' +
                       '<' + Prefixo + 'DataInicial>' +
                          FormatDateTime('yyyy-mm-dd', Response.InfConsultaNFSe.DataInicial) +
                       '</' + Prefixo + 'DataInicial>' +
                       '<' + Prefixo + 'DataFinal>' +
                          FormatDateTime('yyyy-mm-dd', Response.InfConsultaNFSe.DataFinal) +
                       '</' + Prefixo + 'DataFinal>' +
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

  TagEnvio := ConfigMsgDados.ConsultarNFSe.DocElemento;

  aParams := TNFSeParamsResponse.Create;
  try
    aParams.Clear;
    aParams.Xml := XmlConsulta;
    aParams.TagEnvio := TagEnvio;
    aParams.Prefixo := Prefixo;
    aParams.Prefixo2 := PrefixoTS;
    aParams.NameSpace := NameSpace;
    aParams.NameSpace2 := '';
    aParams.IdAttr := '';
    aParams.Versao := '';

    GerarMsgDadosConsultaNFSe(Response, aParams);
  finally
    aParams.Free;
  end;
end;

procedure TACBrNFSeProviderABRASFv1.GerarMsgDadosConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                           '<' + Prefixo + 'Prestador>' +
                             '<' + Prefixo2 + 'Cnpj>' +
                               OnlyNumber(Emitente.CNPJ) +
                             '</' + Prefixo2 + 'Cnpj>' +
                             GetInscMunic(Emitente.InscMun, Prefixo2) +
                           '</' + Prefixo + 'Prestador>' +
                           Xml +
                         '</' + Prefixo + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  ANode, ANode2, AuxNode: TACBrXmlNode;
  ANodeArray, AuxNodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  I, J: Integer;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      TACBrNFSeX(FAOwner).NotasFiscais.Clear;

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

      ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := ACBrStr(Desc202);
        Exit;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('CompNfse');
      if ANodeArray = nil then
        ANodeArray := ANode.Childrens.FindAllAnyNs('ComplNfse');

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
        AuxNode := ANode.Childrens.FindAnyNs('tcCompNfse');

        if AuxNode = nil then
        begin
          AuxNode := ANode.Childrens.FindAnyNs('Nfse');

          if PreencherNotaRespostaConsultaNFSe(AuxNode, ANode, Response) then
            Response.Situacao := '4' // Processado com sucesso pois retornou a nota
          else
          begin
            AErro := Response.Erros.New;
            AErro.Codigo := Cod203;
            AErro.Descricao := ACBrStr(Desc203);
            Exit;
          end;
        end
        else
        begin
          AuxNodeArray := ANode.Childrens.FindAllAnyNs('tcCompNfse');

          for J := Low(AuxNodeArray) to High(AuxNodeArray) do
          begin
            ANode2 := AuxNodeArray[J];
            AuxNode := ANode2.Childrens.FindAnyNs('Nfse');

            if PreencherNotaRespostaConsultaNFSe(AuxNode, ANode2, Response) then
              Response.Situacao := '4' // Processado com sucesso pois retornou a nota
            else
            begin
              AErro := Response.Erros.New;
              AErro.Codigo := Cod203;
              AErro.Descricao := ACBrStr(Desc203);
              Exit;
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

procedure TACBrNFSeProviderABRASFv1.PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  aParams: TNFSeParamsResponse;
  Emitente: TEmitenteConfNFSe;
  InfoCanc: TInfCancelamento;
  IdAttr, NameSpace, Prefixo, PrefixoTS, xMotivo: string;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.CodCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod109;
    AErro.Descricao := ACBrStr(Desc109);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  Prefixo := '';
  PrefixoTS := '';

  InfoCanc := Response.InfCancelamento;

  IdAttr := DefinirIDCancelamento(OnlyNumber(Emitente.CNPJ),
                                  OnlyNumber(Emitente.InscMun),
                                  InfoCanc.NumeroNFSe);

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

  if ConfigGeral.CancPreencherMotivo then
  begin
    if EstaVazio(InfoCanc.MotCancelamento) then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod110;
      AErro.Descricao := ACBrStr(Desc110);
      Exit;
    end;

    xMotivo := '<' + PrefixoTS + 'MotivoCancelamento>' +
                 Trim(InfoCanc.MotCancelamento) +
               '</' + PrefixoTS + 'MotivoCancelamento>';
  end
  else
    xMotivo := '';

  aParams := TNFSeParamsResponse.Create;
  try
    aParams.Clear;
    aParams.Xml := '';
    aParams.TagEnvio := '';
    aParams.Prefixo := Prefixo;
    aParams.Prefixo2 := PrefixoTS;
    aParams.NameSpace := NameSpace;
    aParams.NameSpace2 := '';
    aParams.IdAttr := IdAttr;
    aParams.Versao := '';
    aParams.Serie := '';
    aParams.Motivo := xMotivo;
    aParams.CodigoVerificacao := '';

    GerarMsgDadosCancelaNFSe(Response, aParams);
  finally
    aParams.Free;
  end;
end;

procedure TACBrNFSeProviderABRASFv1.GerarMsgDadosCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  InfoCanc: TInfCancelamento;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  InfoCanc := Response.InfCancelamento;

  with Params do
  begin
    Response.ArquivoEnvio := '<' + Prefixo + 'CancelarNfseEnvio' + NameSpace + '>' +
                           '<' + Prefixo + 'Pedido>' +
                             '<' + Prefixo2 + 'InfPedidoCancelamento' + IdAttr + '>' +
                               '<' + Prefixo2 + 'IdentificacaoNfse>' +
                                 '<' + Prefixo2 + 'Numero>' +
                                   InfoCanc.NumeroNFSe +
                                 '</' + Prefixo2 + 'Numero>' +
                                 '<' + Prefixo2 + 'Cnpj>' +
                                   OnlyNumber(Emitente.CNPJ) +
                                 '</' + Prefixo2 + 'Cnpj>' +
                                 GetInscMunic(Emitente.InscMun, Prefixo2) +
                                 '<' + Prefixo2 + 'CodigoMunicipio>' +
                                    ConfigGeral.CodIBGE +
                                 '</' + Prefixo2 + 'CodigoMunicipio>' +
                               '</' + Prefixo2 + 'IdentificacaoNfse>' +
                               '<' + Prefixo2 + 'CodigoCancelamento>' +
                                  InfoCanc.CodCancelamento +
                               '</' + Prefixo2 + 'CodigoCancelamento>' +
                               Motivo +
                             '</' + Prefixo2 + 'InfPedidoCancelamento>' +
                           '</' + Prefixo + 'Pedido>' +
                         '</' + Prefixo + 'CancelarNfseEnvio>';
  end;
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Document: TACBrXmlDocument;
  ANode, AuxNode, ANodePed, ANodeInfCon: TACBrXmlNode;
  Ret: TRetCancelamento;
  IdAttr: string;
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

      ANode := Document.Root.Childrens.FindAnyNs('Cancelamento');

      if ANode = nil then
        ANode := Document.Root.Childrens.FindAnyNs('RetCancelamento');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod209;
        AErro.Descricao := ACBrStr(Desc209);
        Exit;
      end;

      AuxNode := ANode.Childrens.FindAnyNs('NfseCancelamento');

      if AuxNode <> nil then
        ANode := AuxNode;

      ANode := ANode.Childrens.FindAnyNs('Confirmacao');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod204;
        AErro.Descricao := ACBrStr(Desc204);
        Exit;
      end;

      Ret := Response.RetCancelamento;
      Ret.DataHora := ObterConteudoTag(ANode.Childrens.FindAnyNs('DataHoraCancelamento'), FpFormatoDataHora);

      if Ret.DataHora = 0 then
        Ret.DataHora := ObterConteudoTag(ANode.Childrens.FindAnyNs('DataHora'), FpFormatoDataHora);

      if ConfigAssinar.IncluirURI then
        IdAttr := ConfigGeral.Identificador
      else
        IdAttr := 'ID';

      ANodePed := ANode.Childrens.FindAnyNs('Pedido');
      if not Assigned(ANodePed) or (ANodePed = nil) then Exit;

      ANodePed := ANodePed.Childrens.FindAnyNs('InfPedidoCancelamento');
      if not Assigned(ANodePed) or (ANodePed = nil) then Exit;

      Ret.Pedido.InfID.ID := ObterConteudoTag(ANodePed.Attributes.Items[IdAttr]);
      Ret.Pedido.CodigoCancelamento := ObterConteudoTag(ANodePed.Childrens.FindAnyNs('CodigoCancelamento'), tcStr);

      ANodePed := ANodePed.Childrens.FindAnyNs('IdentificacaoNfse');
      if not Assigned(ANodePed) or (ANodePed = nil) then Exit;

      with Ret.Pedido.IdentificacaoNfse do
      begin
        Numero := ObterConteudoTag(ANodePed.Childrens.FindAnyNs('Numero'), tcStr);
        Cnpj := ObterConteudoTag(ANodePed.Childrens.FindAnyNs('Cnpj'), tcStr);
        InscricaoMunicipal := ObterConteudoTag(ANodePed.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
        CodigoMunicipio := ObterConteudoTag(ANodePed.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
      end;

      ANodeInfCon := ANode.Childrens.FindAnyNs('InfConfirmacaoCancelamento');

      if ANodeInfCon <> nil then
      begin
        Ret.Sucesso := ObterConteudoTag(ANodeInfCon.Childrens.FindAnyNs('Sucesso'), tcStr);
        Ret.DataHora := ObterConteudoTag(ANodeInfCon.Childrens.FindAnyNs('DataHora'), FpFormatoDataHora);
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

procedure TACBrNFSeProviderABRASFv1.PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  AErro := Response.Erros.New;
  AErro.Codigo := Cod001;
  AErro.Descricao := ACBrStr(Desc001);

  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);

  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.GerarMsgDadosSubstituiNFSe(
  Response: TNFSeSubstituiNFSeResponse; Params: TNFSeParamsResponse);
begin
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.LerCancelamento(ANode: TACBrXmlNode;
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  AuxNodeCanc: TACBrXmlNode;
begin
  AuxNodeCanc := ANode.Childrens.FindAnyNs('NfseCancelamento');

  if AuxNodeCanc <> nil then
  begin
    AuxNodeCanc := AuxNodeCanc.Childrens.FindAnyNs('Confirmacao');

    if AuxNodeCanc = nil then
      AuxNodeCanc := ANode.Childrens.FindAnyNs('ConfirmacaoCancelamento');

    if AuxNodeCanc <> nil then
    begin
      AuxNodeCanc := AuxNodeCanc.Childrens.FindAnyNs('InfConfirmacaoCancelamento');

      if AuxNodeCanc <> nil then
        Response.DataCanc := ObterConteudoTag(AuxNodeCanc.Childrens.FindAnyNs('DataHora'), FpFormatoDataHora);

      if Response.DataCanc = 0 then
        Response.DataCanc := ObterConteudoTag(ANode.Childrens.FindAnyNs('DataHoraCancelamento'), FpFormatoDataHora);

      if Response.DataCanc = 0 then
        Response.DataCanc := ObterConteudoTag(ANode.Childrens.FindAnyNs('DataHora'), FpFormatoDataHora);
    end;

    Response.DescSituacao := '';

    if Response.DataCanc > 0 then
      Response.DescSituacao := 'Nota Cancelada';
  end;
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse);
begin
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.GerarMsgDadosGerarToken(
  Response: TNFSeGerarTokenResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.PrepararGerarToken(
  Response: TNFSeGerarTokenResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoGerarToken(
  Response: TNFSeGerarTokenResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderABRASFv1.PrepararEnviarEvento(
  Response: TNFSeEnviarEventoResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.GerarMsgDadosEnviarEvento(
  Response: TNFSeEnviarEventoResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoEnviarEvento(
  Response: TNFSeEnviarEventoResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderABRASFv1.PrepararConsultarEvento(
  Response: TNFSeConsultarEventoResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.GerarMsgDadosConsultarEvento(
  Response: TNFSeConsultarEventoResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoConsultarEvento(
  Response: TNFSeConsultarEventoResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderABRASFv1.PrepararConsultarDFe(
  Response: TNFSeConsultarDFeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.GerarMsgDadosConsultarDFe(
  Response: TNFSeConsultarDFeResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoConsultarDFe(
  Response: TNFSeConsultarDFeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderABRASFv1.PrepararConsultarParam(
  Response: TNFSeConsultarParamResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.GerarMsgDadosConsultarParam(
  Response: TNFSeConsultarParamResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoConsultarParam(
  Response: TNFSeConsultarParamResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderABRASFv1.PrepararConsultarSeqRps(Response: TNFSeConsultarSeqRpsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.GerarMsgDadosConsultarSeqRps(Response: TNFSeConsultarSeqRpsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoConsultarSeqRps(Response: TNFSeConsultarSeqRpsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderABRASFv1.PrepararConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv1.GerarMsgDadosConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderABRASFv1.TratarRetornoConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderABRASFv1.ProcessarMensagemErros(RootNode: TACBrXmlNode;
  Response: TNFSeWebserviceResponse; const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  Mensagem: string;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode.Childrens.FindAnyNs('ListaMensagemRetornoLote');

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
    Mensagem := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Mensagem'), tcStr);

    if Mensagem <> '' then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Codigo'), tcStr);
      AErro.Descricao := ACBrStr(Mensagem);
      AErro.Correcao := ACBrStr(ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Correcao'), tcStr));
    end;
  end;
end;

end.
