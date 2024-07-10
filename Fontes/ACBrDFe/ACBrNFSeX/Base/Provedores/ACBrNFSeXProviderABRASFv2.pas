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
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXProviderBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeProviderABRASFv2 = class(TACBrNFSeXProvider)
  protected
    FpFormatoDataRecebimento: TACBrTipoCampo;
    FpFormatoDataEmissao: TACBrTipoCampo;
    FpFormatoDataHora: TACBrTipoCampo;

    procedure Configuracao; override;

    procedure LerCancelamento(const ANode: TACBrXmlNode; const Response: TNFSeWebServiceResponse);

    procedure LerSubstituicao(const ANode: TACBrXmlNode; const Response: TNFSeWebServiceResponse);

    function PreencherNotaRespostaConsultaLoteRps(Node, parentNode: TACBrXmlNode;
      Response: TNFSeConsultaLoteRpsResponse): Boolean;
    function PreencherNotaRespostaConsultaNFSe(Node, parentNode: TACBrXmlNode;
      Response: TNFSeConsultaNFSeResponse): Boolean;

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
    procedure AssinarConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure GerarMsgDadosConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); virtual;
    procedure AssinarConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure TratarRetornoConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); virtual;

    procedure PrepararConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure GerarMsgDadosConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); virtual;
    procedure AssinarConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure TratarRetornoConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse); virtual;

    procedure PrepararConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure GerarMsgDadosConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); virtual;
    procedure AssinarConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure TratarRetornoConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse); virtual;

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

    function VerificarAlerta(const ACodigo, AMensagem: string): Boolean; virtual;
    function VerificarErro(const ACodigo, AMensagem: string): Boolean; virtual;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'ListaMensagemRetorno';
                                     const AMessageTag: string = 'MensagemRetorno'); virtual;

  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML, ACBrUtil.DateTime,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXNotasFiscais, ACBrNFSeXConsts,
  ACBrNFSeXConversao, ACBrNFSeXWebserviceBase;

{ TACBrNFSeProviderABRASFv2 }

procedure TACBrNFSeProviderABRASFv2.Configuracao;
const
  NameSpace = 'http://www.abrasf.org.br/nfse.xsd';
begin
  inherited Configuracao;

  FpFormatoDataRecebimento := tcDatHor;
  FpFormatoDataEmissao := tcDatHor;
  FpFormatoDataHora := tcDatHor;

  // Os provedores que seguem a versão 2 do layout da ABRASF podem ter até
  // três serviços para recepcionar o RPS: assíncrono, síncrono e unitário.
  // Por padrão vamos adotar o serviço Síncrono, caso o provedor não tenha
  // esse serviço na Unit xxxx.Provider (xxxx = Provedor) devemos
  // configurar o serviço disponibilizado.
  with ConfigGeral do
  begin
    Layout := loABRASF;
    ModoEnvio := meLoteSincrono;
    ConsultaPorFaixa := True;
    ConsultaPorFaixaPreencherNumNfseFinal := False;

    ServicosDisponibilizados.EnviarLoteAssincrono := True;
    ServicosDisponibilizados.EnviarLoteSincrono := True;
    ServicosDisponibilizados.EnviarUnitario := True;
    ServicosDisponibilizados.ConsultarLote := True;
    ServicosDisponibilizados.ConsultarRps := True;
    ServicosDisponibilizados.ConsultarFaixaNfse := True;
    ServicosDisponibilizados.ConsultarServicoPrestado := True;
    ServicosDisponibilizados.ConsultarServicoTomado := True;
    ServicosDisponibilizados.CancelarNfse := True;
    ServicosDisponibilizados.SubstituirNfse := True;
  end;

  SetXmlNameSpace(NameSpace);

  with ConfigWebServices do
  begin
    VersaoDados := '2.00';
    VersaoAtrib := '2.00';
    AtribVerLote := 'versao';
  end;

  with ConfigMsgDados do
  begin
    XmlRps.InfElemento := 'InfDeclaracaoPrestacaoServico';
    XmlRps.DocElemento := 'Rps';

    DadosCabecalho := GetCabecalho('');

    GerarNSLoteRps := False;
  end;
end;

function TACBrNFSeProviderABRASFv2.PreencherNotaRespostaConsultaLoteRps(Node,
  parentNode: TACBrXmlNode; Response: TNFSeConsultaLoteRpsResponse): Boolean;
var
  NumNFSe, CodVerif, NumRps, SerieRps: String;
  DataAut: TDateTime;
  ANota: TNotaFiscal;
  AResumo: TNFSeResumoCollectionItem;
  Node2: TACBrXmlNode;
  NumeroRps: Integer;
begin
  Result := False;
  NumeroRps := 0;

  if Node <> nil then
  begin
    Node := Node.Childrens.FindAnyNs('InfNfse');

    if not Assigned(Node) then Exit;

    NumNFSe := ObterConteudoTag(Node.Childrens.FindAnyNs('Numero'), tcStr);
    CodVerif := ObterConteudoTag(Node.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
    DataAut := ObterConteudoTag(Node.Childrens.FindAnyNs('DataEmissao'), FpFormatoDataEmissao);

    Node2 := Node.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');

    // Tem provedor que mudou a tag de <DeclaracaoPrestacaoServico>
    // para <Rps>
    if Node2 = nil then
      Node2 := Node.Childrens.FindAnyNs('Rps');

    if not Assigned(Node2) then Exit;

    Node := Node2.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');
    if not Assigned(Node) then Exit;

    Node := Node.Childrens.FindAnyNs('Rps');

    NumRps := '';
    SerieRps := '';

    if Node <> nil then
    begin
      Node := Node.Childrens.FindAnyNs('IdentificacaoRps');

      if Node <> nil then
      begin
        NumRps := ObterConteudoTag(Node.Childrens.FindAnyNs('Numero'), tcStr);
        NumeroRps := StrToIntDef(NumRps, 0);
        SerieRps := ObterConteudoTag(Node.Childrens.FindAnyNs('Serie'), tcStr);
      end;
    end;

    AResumo := Response.Resumos.New;
    AResumo.NumeroNota := NumNFSe;
    AResumo.Data := DataAut;
    AResumo.CodigoVerificacao := CodVerif;
    AResumo.NumeroRps := NumRps;
    AResumo.SerieRps := SerieRps;

    Response.NumeroNota := NumNFSe;
    Response.CodigoVerificacao := CodVerif;
    Response.NumeroRps := NumRps;
    Response.SerieRps := SerieRps;

    if NumeroRps > 0 then
      ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps)
    else
      ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

    ANota := CarregarXmlNfse(ANota, parentNode.OuterXml);
    SalvarXmlNfse(ANota);

    AResumo.NomeArq := ANota.NomeArq;
    AResumo.Link := ANota.NFSe.Link;

    Response.Link := ANota.NFSe.Link;

    Result := True; // Processado com sucesso pois retornou a nota
  end;
end;

function TACBrNFSeProviderABRASFv2.PreencherNotaRespostaConsultaNFSe(
  Node, parentNode: TACBrXmlNode; Response: TNFSeConsultaNFSeResponse): Boolean;
var
  NumNFSe, CodVerif, NumRps, SerieRps: String;
  DataAut: TDateTime;
  ANota: TNotaFiscal;
  AResumo: TNFSeResumoCollectionItem;
  Node2: TACBrXmlNode;
  NumeroRps: Integer;
begin
  Result := False;
  NumeroRps := 0;

  if Node <> nil then
  begin
    Node := Node.Childrens.FindAnyNs('InfNfse');

    if not Assigned(Node) then Exit;

    NumNFSe := ObterConteudoTag(Node.Childrens.FindAnyNs('Numero'), tcStr);
    CodVerif := ObterConteudoTag(Node.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
    DataAut := ObterConteudoTag(Node.Childrens.FindAnyNs('DataEmissao'), FpFormatoDataEmissao);

    Node2 := Node.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');

    // Tem provedor que mudou a tag de <DeclaracaoPrestacaoServico>
    // para <Rps>
    if Node2 = nil then
      Node2 := Node.Childrens.FindAnyNs('Rps');

    if not Assigned(Node2) then Exit;

    Node := Node2.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');
    if not Assigned(Node) then Exit;

    Node := Node.Childrens.FindAnyNs('Rps');

    NumRps := '';
    SerieRps := '';

    if Node <> nil then
    begin
      Node := Node.Childrens.FindAnyNs('IdentificacaoRps');

      if Node <> nil then
      begin
        NumRps := ObterConteudoTag(Node.Childrens.FindAnyNs('Numero'), tcStr);
        NumeroRps := StrToIntDef(NumRps, 0);
        SerieRps := ObterConteudoTag(Node.Childrens.FindAnyNs('Serie'), tcStr);
      end;
    end;

    AResumo := Response.Resumos.New;
    AResumo.NumeroNota := NumNFSe;
    AResumo.Data := DataAut;
    AResumo.CodigoVerificacao := CodVerif;
    AResumo.NumeroRps := NumRps;
    AResumo.SerieRps := SerieRps;

    Response.NumeroNota := NumNFSe;
    Response.CodigoVerificacao := CodVerif;
    Response.NumeroRps := NumRps;
    Response.SerieRps := SerieRps;

    if NumeroRps > 0 then
      ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps)
    else
      ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

    ANota := CarregarXmlNfse(ANota, parentNode.OuterXml);
    SalvarXmlNfse(ANota);

    AResumo.NomeArq := ANota.NomeArq;
    AResumo.Link := ANota.NFSe.Link;

    Response.Link := ANota.NFSe.Link;

    Result := True; // Processado com sucesso pois retornou a nota
  end;
end;

procedure TACBrNFSeProviderABRASFv2.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  aParams: TNFSeParamsResponse;
  Nota: TNotaFiscal;
  Versao, IdAttr, NameSpace, NameSpaceLote, ListaRps, xRps, IdAttrSig,
  TagEnvio, Prefixo, PrefixoTS: string;
  I: Integer;
begin
  if EstaVazio(Response.NumeroLote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := ACBrStr(Desc111);
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

  if (TACBrNFSeX(FAOwner).NotasFiscais.Count < Response.MinRps) and
     (Response.ModoEnvio <> meUnitario) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod005;
    AErro.Descricao := ACBrStr('Conjunto de RPS transmitidos (mínimo de ' +
                       IntToStr(Response.MinRps) + ' RPS)' +
                       '. Quantidade atual: ' +
                       IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count));
  end;

  if Response.Erros.Count > 0 then Exit;

  ListaRps := '';
  Prefixo := '';
  PrefixoTS := '';

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

    Nota.GerarXML;

    Nota.XmlRps := ConverteXMLtoUTF8(Nota.XmlRps);
    Nota.XmlRps := ChangeLineBreak(Nota.XmlRps, '');

    if (ConfigAssinar.Rps and (Response.ModoEnvio in [meLoteAssincrono, meLoteSincrono, meTeste])) or
       (ConfigAssinar.RpsGerarNFSe and (Response.ModoEnvio = meUnitario)) then
    begin
      IdAttrSig := SetIdSignatureValue(Nota.XmlRps,
                                     ConfigMsgDados.XmlRps.DocElemento, IdAttr);

      Nota.XmlRps := FAOwner.SSL.Assinar(Nota.XmlRps,
                                         PrefixoTS + ConfigMsgDados.XmlRps.DocElemento,
                                         ConfigMsgDados.XmlRps.InfElemento, '', '', '',
                                         IdAttr, IdAttrSig);
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

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosEmitir(
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

      Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                             '<' + Prefixo + 'LoteRps' + NameSpace2 + IdAttr  + Versao + '>' +
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
      Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                              Xml +
                           '</' + Prefixo + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  NumNFSe, CodVerif, NumRps, SerieRps: string;
  DataAut: TDateTime;
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  AResumo: TNFSeResumoCollectionItem;
  ANode, AuxNode, AuxNode2: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  ANota: TNotaFiscal;
  I: Integer;
begin
  Document := TACBrXmlDocument.Create;
  NumRps := '';

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

      with Response do
      begin
        Data := ObterConteudoTag(ANode.Childrens.FindAnyNs('DataRecebimento'), FpFormatoDataRecebimento);
        Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('Protocolo'), tcStr);
      end;

      if Response.ModoEnvio in [meLoteSincrono, meUnitario] then
      begin
        // Retorno do EnviarLoteRpsSincrono e GerarNfse
        ANode := ANode.Childrens.FindAnyNs('ListaNfse');

        if not Assigned(ANode) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod202;
          AErro.Descricao := ACBrStr(Desc202);
          Exit;
        end;

        ProcessarMensagemErros(ANode, Response);

        ANodeArray := ANode.Childrens.FindAllAnyNs('CompNfse');

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
          AuxNode := ANode.Childrens.FindAnyNs('Nfse');
          if not Assigned(AuxNode) then Exit;

          AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');

          NumNFSe := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
          CodVerif := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
          DataAut := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('DataEmissao'), FpFormatoDataEmissao);

          with Response do
          begin
            NumeroNota := NumNFSe;
            CodigoVerificacao := CodVerif;
            Data := DataAut;
          end;

          AuxNode2 := AuxNode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');

          // Tem provedor que mudou a tag de <DeclaracaoPrestacaoServico>
          // para <Rps>
          if AuxNode2 = nil then
            AuxNode2 := AuxNode.Childrens.FindAnyNs('Rps');
          if not Assigned(AuxNode2) then Exit;

          AuxNode := AuxNode2.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');
          if not Assigned(AuxNode) then Exit;

          AuxNode := AuxNode.Childrens.FindAnyNs('Rps');

          if AuxNode <> nil then
          begin
            AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
            if not Assigned(AuxNode) then Exit;

            NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
            SerieRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Serie'), tcStr);
          end;

          if NumRps <> '' then
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps)
          else
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(Response.NumeroNota);

          AResumo := Response.Resumos.New;
          AResumo.NumeroNota := NumNFSe;
          AResumo.CodigoVerificacao := CodVerif;
          AResumo.NumeroRps := NumRps;
          AResumo.SerieRps := SerieRps;

          ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
          SalvarXmlNfse(ANota);

          AResumo.NomeArq := ANota.NomeArq;
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

procedure TACBrNFSeProviderABRASFv2.PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse);
begin
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse; Params: TNFSeParamsResponse);
begin
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse);
begin
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  aParams: TNFSeParamsResponse;
  NameSpace, TagEnvio, Prefixo, PrefixoTS: string;
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

  TagEnvio := ConfigMsgDados.ConsultarLote.DocElemento;

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

    GerarMsgDadosConsultaLoteRps(Response, aParams);
  finally
    aParams.Free;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Prestador, NumeroLote: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Prestador :='<' + Prefixo + 'Prestador>' +
                  '<' + Prefixo2 + 'CpfCnpj>' +
                    GetCpfCnpj(Emitente.CNPJ, Prefixo2) +
                  '</' + Prefixo2 + 'CpfCnpj>' +
                  GetInscMunic(Emitente.InscMun, Prefixo2) +
                '</' + Prefixo + 'Prestador>' +
                '<' + Prefixo + 'Protocolo>' +
                  Response.Protocolo +
                '</' + Prefixo + 'Protocolo>';

    if ConfigMsgDados.UsarNumLoteConsLote then
      NumeroLote := '<' + Prefixo + 'NumeroLote>' +
                      Response.NumeroLote +
                    '</' + Prefixo + 'NumeroLote>';

    Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                           Prestador +
                           NumeroLote +
                         '</' + Prefixo + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
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

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      Response.Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('Situacao'), tcStr);

      if Response.Situacao = '' then
        Response.Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('SituacaoLoteRps'), tcStr);

      ANode := ANode.Childrens.FindAnyNs('ListaNfse');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := ACBrStr(Desc202);
        Exit;
      end;

      ProcessarMensagemErros(ANode, Response);

      ANodeArray := ANode.Childrens.FindAllAnyNs('CompNfse');

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

        LerCancelamento(ANode, Response);

        LerSubstituicao(ANode, Response);

        AuxNode := ANode.Childrens.FindAnyNs('Nfse');

        if AuxNode = nil then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit;
        end
        else
        begin
          if PreencherNotaRespostaConsultaLoteRps(AuxNode, ANode, Response) then
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

procedure TACBrNFSeProviderABRASFv2.PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  aParams: TNFSeParamsResponse;
  NameSpace, TagEnvio, Prefixo, PrefixoTS: string;
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

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosConsultaporRps(
  Response: TNFSeConsultaNFSeporRpsResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Prestador: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Prestador :='<' + Prefixo + 'Prestador>' +
                  '<' + Prefixo2 + 'CpfCnpj>' +
                    GetCpfCnpj(Emitente.CNPJ, Prefixo2) +
                  '</' + Prefixo2 + 'CpfCnpj>' +
                  GetInscMunic(Emitente.InscMun, Prefixo2) +
                '</' + Prefixo + 'Prestador>';

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
                           Prestador +
                         '</' + Prefixo + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode, AuxNode2: TACBrXmlNode;
  AErro: TNFSeEventoCollectionItem;
  ANota: TNotaFiscal;
  NumNFSe, NumRps: String;
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

      ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');

      if ANode = nil then
        ANode := Document.Root.Childrens.FindAnyNs('CompNfse')
      else
        ANode := ANode.Childrens.FindAnyNs('CompNfse');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      LerCancelamento(ANode, Response);

      LerSubstituicao(ANode, Response);

      AuxNode := ANode.Childrens.FindAnyNs('Nfse');

      if AuxNode <> nil then
      begin
        AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
        if not Assigned(AuxNode) then Exit;

        NumNFSe := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

        with Response do
        begin
          NumeroNota := NumNFSe;
          CodigoVerificacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('DataEmissao'), FpFormatoDataEmissao);
        end;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

        if ANota = nil then
        begin
          AuxNode2 := AuxNode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');

          // Tem provedor que mudou a tag de <DeclaracaoPrestacaoServico>
          // para <Rps>
          if AuxNode2 = nil then
            AuxNode2 := AuxNode.Childrens.FindAnyNs('Rps');
          if not Assigned(AuxNode2) then Exit;

          AuxNode := AuxNode2.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');
          if not Assigned(AuxNode) then Exit;

          AuxNode := AuxNode.Childrens.FindAnyNs('Rps');

          if AuxNode <> nil then
          begin
            AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
            if not Assigned(AuxNode) then Exit;

            NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);
          end;
        end;

        ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
        SalvarXmlNfse(ANota);
      end
      else
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
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

procedure TACBrNFSeProviderABRASFv2.PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  case Response.InfConsultaNFSe.tpConsulta of
    tcPorPeriodo,
    tcPorFaixa,
    tcPorNumero: Response.Metodo := tmConsultarNFSePorFaixa;
    tcServicoTomado: Response.Metodo := tmConsultarNFSeServicoTomado;
    tcServicoPrestado: Response.Metodo := tmConsultarNFSeServicoPrestado;
    tcPorChave: Response.Metodo := tmConsultarNFSePorChave;
  else
    Response.Metodo := tmConsultarNFSe;
  end;

  case Response.InfConsultaNFSe.tpConsulta of
    tcPorPeriodo,
    tcPorFaixa,
    tcPorNumero: PrepararConsultaNFSeporFaixa(Response);
    tcServicoPrestado: PrepararConsultaNFSeServicoPrestado(Response);
    tcServicoTomado: PrepararConsultaNFSeServicoTomado(Response);
  else
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod001;
      AErro.Descricao := ACBrStr(Desc001);
    end;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  case Response.InfConsultaNFSe.tpConsulta of
    tcPorPeriodo,
    tcPorFaixa: GerarMsgDadosConsultaNFSeporFaixa(Response, Params);
    tcServicoPrestado: GerarMsgDadosConsultaNFSeServicoPrestado(Response, Params);
    tcServicoTomado: GerarMsgDadosConsultaNFSeServicoTomado(Response, Params);
  else
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod001;
      AErro.Descricao := ACBrStr(Desc001);
    end;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.AssinarConsultaNFSe(Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  case Response.InfConsultaNFSe.tpConsulta of
    tcPorPeriodo,
    tcPorFaixa,
    tcPorNumero: AssinarConsultaNFSeporFaixa(Response);
    tcServicoPrestado: AssinarConsultaNFSeServicoPrestado(Response);
    tcServicoTomado: AssinarConsultaNFSeServicoTomado(Response);
  else
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod001;
      AErro.Descricao := ACBrStr(Desc001);
    end;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  case Response.InfConsultaNFSe.tpConsulta of
    tcPorPeriodo,
    tcPorFaixa,
    tcPorNumero: TratarRetornoConsultaNFSeporFaixa(Response);
    tcServicoPrestado: TratarRetornoConsultaNFSeServicoPrestado(Response);
    tcServicoTomado: TratarRetornoConsultaNFSeServicoTomado(Response);
  else
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod001;
      AErro.Descricao := ACBrStr(Desc001);
    end;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.PrepararConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse);
var
  aParams: TNFSeParamsResponse;
  XmlConsulta, xNumFinal, NameSpace, Prefixo, PrefixoTS: string;
begin
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

  if (OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) <> OnlyNumber(Response.InfConsultaNFSe.NumeroFinNFSe)) or
     ConfigGeral.ConsultaPorFaixaPreencherNumNfseFinal then
    xNumFinal := '<' + PrefixoTS + 'NumeroNfseFinal>' +
                    OnlyNumber(Response.InfConsultaNFSe.NumeroFinNFSe) +
                 '</' + PrefixoTS + 'NumeroNfseFinal>'
  else
    xNumFinal := '';

  XmlConsulta := '<' + Prefixo + 'Faixa>' +
                   '<' + PrefixoTS + 'NumeroNfseInicial>' +
                      OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) +
                   '</' + PrefixoTS + 'NumeroNfseInicial>' +
                   xNumFinal +
                 '</' + Prefixo + 'Faixa>';

  aParams := TNFSeParamsResponse.Create;
  try
    aParams.Clear;
    aParams.Xml := XmlConsulta;
    aParams.TagEnvio := '';
    aParams.Prefixo := Prefixo;
    aParams.Prefixo2 := PrefixoTS;
    aParams.NameSpace := NameSpace;
    aParams.NameSpace2 := '';
    aParams.IdAttr := '';
    aParams.Versao := '';

    GerarMsgDadosConsultaNFSeporFaixa(Response, aParams);
  finally
    aParams.Free;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosConsultaNFSeporFaixa(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Prestador: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Prestador :='<' + Prefixo + 'Prestador>' +
                  '<' + Prefixo2 + 'CpfCnpj>' +
                    GetCpfCnpj(Emitente.CNPJ, Prefixo2) +
                  '</' + Prefixo2 + 'CpfCnpj>' +
                  GetInscMunic(Emitente.InscMun, Prefixo2) +
                '</' + Prefixo + 'Prestador>';

    Response.ArquivoEnvio := '<' + Prefixo + 'ConsultarNfseFaixaEnvio' + NameSpace + '>' +
                           Prestador +
                           Xml +
                           '<' + Prefixo + 'Pagina>' +
                              IntToStr(Response.InfConsultaNFSe.Pagina) +
                           '</' + Prefixo + 'Pagina>' +
                         '</' + Prefixo + 'ConsultarNfseFaixaEnvio>';
  end;
end;

procedure TACBrNFSeProviderABRASFv2.AssinarConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse);
var
  IdAttr, Prefixo, IdAttrSig: string;
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
    IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
                      ConfigMsgDados.ConsultarNFSePorFaixa.DocElemento, IdAttr);

    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarNFSePorFaixa.DocElemento,
      ConfigMsgDados.ConsultarNFSePorFaixa.InfElemento, '', '', '', IdAttr,
      IdAttrSig);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
    end;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  I: Integer;
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

      ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := ACBrStr(Desc202);
        Exit;
      end;

      ProcessarMensagemErros(ANode, Response);

      ANodeArray := ANode.Childrens.FindAllAnyNs('CompNfse');

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

        LerCancelamento(ANode, Response);

        LerSubstituicao(ANode, Response);

        AuxNode := ANode.Childrens.FindAnyNs('Nfse');
        if not Assigned(AuxNode) then Exit;

        if not PreencherNotaRespostaConsultaNFSe(AuxNode, ANode, Response) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit;
        end;
        {
        AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
        if not Assigned(AuxNode) then Exit;

        AuxNode := AuxNode.Childrens.FindAnyNs('Numero');
        if not Assigned(AuxNode) then Exit;

        NumNFSe := AuxNode.AsString;

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

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

        ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
        SalvarXmlNfse(ANota);
        }
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

procedure TACBrNFSeProviderABRASFv2.PrepararConsultaNFSeServicoPrestado(
  Response: TNFSeConsultaNFSeResponse);
var
  aParams: TNFSeParamsResponse;
  XmlConsulta, NameSpace, Prefixo, PrefixoTS: string;
begin
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

  aParams := TNFSeParamsResponse.Create;
  try
    aParams.Clear;
    aParams.Xml := XmlConsulta;
    aParams.TagEnvio := '';
    aParams.Prefixo := Prefixo;
    aParams.Prefixo2 := PrefixoTS;
    aParams.NameSpace := NameSpace;
    aParams.NameSpace2 := '';
    aParams.IdAttr := '';
    aParams.Versao := '';

    GerarMsgDadosConsultaNFSeServicoPrestado(Response, aParams);
  finally
    aParams.Free;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosConsultaNFSeServicoPrestado(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Prestador: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Prestador :='<' + Prefixo + 'Prestador>' +
                  '<' + Prefixo2 + 'CpfCnpj>' +
                    GetCpfCnpj(Emitente.CNPJ, Prefixo2) +
                  '</' + Prefixo2 + 'CpfCnpj>' +
                  GetInscMunic(Emitente.InscMun, Prefixo2) +
                '</' + Prefixo + 'Prestador>';

    Response.ArquivoEnvio := '<' + Prefixo + 'ConsultarNfseServicoPrestadoEnvio' + NameSpace + '>' +
                           Prestador +
                           Xml +
                           '<' + Prefixo + 'Pagina>' +
                              IntToStr(Response.InfConsultaNFSe.Pagina) +
                           '</' + Prefixo + 'Pagina>' +
                         '</' + Prefixo + 'ConsultarNfseServicoPrestadoEnvio>';
  end;
end;

procedure TACBrNFSeProviderABRASFv2.AssinarConsultaNFSeServicoPrestado(
  Response: TNFSeConsultaNFSeResponse);
var
  IdAttr, Prefixo, IdAttrSig: string;
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
    IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
               ConfigMsgDados.ConsultarNFSeServicoPrestado.DocElemento, IdAttr);

    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarNFSeServicoPrestado.DocElemento,
      ConfigMsgDados.ConsultarNFSeServicoPrestado.InfElemento, '', '', '',
      IdAttr, IdAttrSig);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
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
  I: Integer;
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

      ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod202;
        AErro.Descricao := ACBrStr(Desc202);
        Exit;
      end;

      ProcessarMensagemErros(ANode, Response);

      ANodeArray := ANode.Childrens.FindAllAnyNs('CompNfse');

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

        LerCancelamento(ANode, Response);

        LerSubstituicao(ANode, Response);

        AuxNode := ANode.Childrens.FindAnyNs('Nfse');
        if not Assigned(AuxNode) then Exit;

        if not PreencherNotaRespostaConsultaNFSe(AuxNode, ANode, Response) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit;
        end;
        {
        AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
        if not Assigned(AuxNode) then Exit;

        AuxNode := AuxNode.Childrens.FindAnyNs('Numero');
        if not Assigned(AuxNode) then Exit;

        NumNFSe := AuxNode.AsString;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

        ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
        SalvarXmlNfse(ANota);
        }
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

procedure TACBrNFSeProviderABRASFv2.PrepararConsultaNFSeServicoTomado(
  Response: TNFSeConsultaNFSeResponse);
var
  aParams: TNFSeParamsResponse;
  XmlConsulta, NameSpace, Prefixo, PrefixoTS: string;
begin
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

  aParams := TNFSeParamsResponse.Create;
  try
    aParams.Clear;
    aParams.Xml := XmlConsulta;
    aParams.TagEnvio := '';
    aParams.Prefixo := Prefixo;
    aParams.Prefixo2 := PrefixoTS;
    aParams.NameSpace := NameSpace;
    aParams.NameSpace2 := '';
    aParams.IdAttr := '';
    aParams.Versao := '';

    GerarMsgDadosConsultaNFSeServicoTomado(Response, aParams);
  finally
    aParams.Free;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosConsultaNFSeServicoTomado(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Consulente: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Consulente :='<' + Prefixo + 'Consulente>' +
                   '<' + Prefixo2 + 'CpfCnpj>' +
                     GetCpfCnpj(Emitente.CNPJ, Prefixo2) +
                   '</' + Prefixo2 + 'CpfCnpj>' +
                   GetInscMunic(Emitente.InscMun, Prefixo2) +
                 '</' + Prefixo + 'Consulente>';

    Response.ArquivoEnvio := '<' + Prefixo + 'ConsultarNfseServicoTomadoEnvio' + NameSpace + '>' +
                           Consulente +
                           Xml +
                           '<' + Prefixo + 'Pagina>' +
                              IntToStr(Response.InfConsultaNFSe.Pagina) +
                           '</' + Prefixo + 'Pagina>' +
                         '</' + Prefixo + 'ConsultarNfseServicoTomadoEnvio>';
  end;
end;

procedure TACBrNFSeProviderABRASFv2.AssinarConsultaNFSeServicoTomado(
  Response: TNFSeConsultaNFSeResponse);
var
  IdAttr, Prefixo, IdAttrSig: string;
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
    IdAttrSig := SetIdSignatureValue(Response.ArquivoEnvio,
               ConfigMsgDados.ConsultarNFSeServicoTomado.DocElemento, IdAttr);

    Response.ArquivoEnvio := FAOwner.SSL.Assinar(Response.ArquivoEnvio,
      Prefixo + ConfigMsgDados.ConsultarNFSeServicoTomado.DocElemento,
      ConfigMsgDados.ConsultarNFSeServicoTomado.InfElemento, '', '', '', IdAttr,
      IdAttrSig);
  except
    on E:Exception do
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod801;
      AErro.Descricao := ACBrStr(Desc801 + E.Message);
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
  I: Integer;
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

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      ANode := ANode.Childrens.FindAnyNs('ListaNfse');

      if ANode = nil then
        ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      ANodeArray := ANode.Childrens.FindAllAnyNs('CompNfse');

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

        LerCancelamento(ANode, Response);

        LerSubstituicao(ANode, Response);

        AuxNode := ANode.Childrens.FindAnyNs('Nfse');
        if not Assigned(AuxNode) then Exit;

        if not PreencherNotaRespostaConsultaNFSe(AuxNode, ANode, Response) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit;
        end;
        {
        AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
        if not Assigned(AuxNode) then Exit;

        AuxNode := AuxNode.Childrens.FindAnyNs('Numero');
        if not Assigned(AuxNode) then Exit;

        NumNFSe := AuxNode.AsString;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

        ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
        SalvarXmlNfse(ANota);
        }
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

procedure TACBrNFSeProviderABRASFv2.PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  aParams: TNFSeParamsResponse;
  Emitente: TEmitenteConfNFSe;
  InfoCanc: TInfCancelamento;
  IdAttr, NameSpace, NameSpaceCanc, xMotivo, xCodVerif, Prefixo, PrefixoTS,
  xSerie: string;
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

  IdAttr := DefinirIDCancelamento(OnlyNumber(Emitente.CNPJ),
                                  OnlyNumber(Emitente.InscMun),
                                  InfoCanc.NumeroNFSe);

  if ConfigGeral.CancPreencherSerieNfse then
  begin
    if EstaVazio(InfoCanc.SerieNFSe) then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod112;
      AErro.Descricao := ACBrStr(Desc112);
      Exit;
    end;

    xSerie := '<' + PrefixoTS + 'Serie>' +
                 Trim(InfoCanc.SerieNFSe) +
               '</' + PrefixoTS + 'Serie>';
  end
  else
    xSerie := '';

  if ConfigGeral.CancPreencherMotivo then
  begin
    if EstaVazio(InfoCanc.MotCancelamento) then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod110;
      AErro.Descricao := ACBrStr(Desc110);
      Exit;
    end;

    xMotivo := '<' + Prefixo + 'MotivoCancelamento>' +
                 Trim(InfoCanc.MotCancelamento) +
               '</' + Prefixo + 'MotivoCancelamento>';
  end
  else
    xMotivo := '';

  if ConfigGeral.CancPreencherCodVerificacao then
  begin
    if EstaVazio(InfoCanc.CodVerificacao) then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod117;
      AErro.Descricao := ACBrStr(Desc117);
      Exit;
    end;

    xCodVerif := '<' + Prefixo + 'CodigoVerificacao>' +
                   Trim(InfoCanc.CodVerificacao) +
                 '</' + Prefixo + 'CodigoVerificacao>';
  end
  else
    xCodVerif := '';

  aParams := TNFSeParamsResponse.Create;
  try
    aParams.Clear;
    aParams.Xml := '';
    aParams.TagEnvio := '';
    aParams.Prefixo := Prefixo;
    aParams.Prefixo2 := PrefixoTS;
    aParams.NameSpace := NameSpace;
    aParams.NameSpace2 := NameSpaceCanc;
    aParams.IdAttr := IdAttr;
    aParams.Versao := '';
    aParams.Serie := xSerie;
    aParams.Motivo := xMotivo;
    aParams.CodigoVerificacao := xCodVerif;

    GerarMsgDadosCancelaNFSe(Response, aParams);
  finally
    aParams.Free;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosCancelaNFSe(
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
                           '<' + Prefixo2 + 'Pedido>' +
                             '<' + Prefixo2 + 'InfPedidoCancelamento' + IdAttr + NameSpace2 + '>' +
                               '<' + Prefixo2 + 'IdentificacaoNfse>' +
                                 '<' + Prefixo2 + 'Numero>' +
                                    InfoCanc.NumeroNFSe +
                                 '</' + Prefixo2 + 'Numero>' +
                                 Serie +
                                 '<' + Prefixo2 + 'CpfCnpj>' +
                                   GetCpfCnpj(Emitente.CNPJ, Prefixo2) +
                                 '</' + Prefixo2 + 'CpfCnpj>' +
                                 GetInscMunic(Emitente.InscMun, Prefixo2) +
                                 '<' + Prefixo2 + 'CodigoMunicipio>' +
                                    IntToStr(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                                 '</' + Prefixo2 + 'CodigoMunicipio>' +
                                 CodigoVerificacao +
                               '</' + Prefixo2 + 'IdentificacaoNfse>' +
                               '<' + Prefixo2 + 'CodigoCancelamento>' +
                                  InfoCanc.CodCancelamento +
                               '</' + Prefixo2 + 'CodigoCancelamento>' +
                               Motivo +
                             '</' + Prefixo2 + 'InfPedidoCancelamento>' +
                           '</' + Prefixo2 + 'Pedido>' +
                         '</' + Prefixo + 'CancelarNfseEnvio>';
  end;
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
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ProcessarMensagemErros(Document.Root, Response);

      ANode := Document.Root.Childrens.FindAnyNs('RetCancelamento');

      if ANode = nil then
        ANode := Document.Root.Childrens.FindAnyNs('RetornoCancelamento');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod209;
        AErro.Descricao := ACBrStr(Desc209);
        Exit;
      end;

      ANode := ANode.Childrens.FindAnyNs('NfseCancelamento');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod210;
        AErro.Descricao := ACBrStr(Desc210);
        Exit;
      end;

      AuxNode := ANode.Childrens.FindAnyNs('Confirmacao');

      if AuxNode = nil then
        AuxNode := ANode.Childrens.FindAnyNs('ConfirmacaoCancelamento');

      if AuxNode <> nil then
        ANode := AuxNode;

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod204;
        AErro.Descricao := ACBrStr(Desc204);
        Exit;
      end;

      Ret := Response.RetCancelamento;
      Ret.DataHora := ObterConteudoTag(ANode.Childrens.FindAnyNs('DataHora'), FpFormatoDataHora);

      if Ret.DataHora > 0 then
      begin
        Ret.Sucesso := 'Sim';
        Ret.Situacao := 'Cancelado';
      end
      else
      begin
        Ret.Sucesso := '';
        Ret.Situacao := '';
      end;

      if ConfigAssinar.IncluirURI then
        IdAttr := ConfigGeral.Identificador
      else
        IdAttr := 'ID';

      AuxNode := ANode.Childrens.FindAnyNs('Pedido');

      if AuxNode = nil then
        AuxNode := ANode.Childrens.FindAnyNs('PedidoCancelamento');

      if AuxNode <> nil then
        ANode := AuxNode;

      ANode := ANode.Childrens.FindAnyNs('InfPedidoCancelamento');
      if not Assigned(ANode) then Exit;

      Ret.Pedido.InfID.ID := ObterConteudoTag(ANode.Attributes.Items[IdAttr]);
      Ret.Pedido.CodigoCancelamento := ObterConteudoTag(ANode.Childrens.FindAnyNs('CodigoCancelamento'), tcStr);

      ANode := ANode.Childrens.FindAnyNs('IdentificacaoNfse');
      if not Assigned(ANode) then Exit;

      with Ret.Pedido.IdentificacaoNfse do
      begin
        Numero := ObterConteudoTag(ANode.Childrens.FindAnyNs('Numero'), tcStr);

        AuxNode := ANode.Childrens.FindAnyNs('CpfCnpj');

        if AuxNode <> nil then
        begin
          Cnpj := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Cnpj'), tcStr);

          if Cnpj = '' then
            Cnpj := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Cpf'), tcStr);
        end
        else
          Cnpj := ObterConteudoTag(ANode.Childrens.FindAnyNs('Cnpj'), tcStr);

        InscricaoMunicipal := ObterConteudoTag(ANode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
        CodigoMunicipio := ObterConteudoTag(ANode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
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

procedure TACBrNFSeProviderABRASFv2.PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  aParams: TNFSeParamsResponse;
  Nota: TNotaFiscal;
  IdAttr, xRps, NameSpace, NumRps, TagEnvio, Prefixo, PrefixoTS,
  IdAttrSig: string;
begin
  if EstaVazio(Response.PedCanc) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod107;
    AErro.Descricao := ACBrStr(Desc107);
    Exit;
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count <= 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod002;
    AErro.Descricao := ACBrStr(Desc002);
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count > 1 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod003;
    AErro.Descricao := ACBrStr('Conjunto de RPS transmitidos (máximo de 1 RPS)' +
                       ' excedido. Quantidade atual: ' +
                       IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count));
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

  Nota.GerarXML;

  Nota.XmlRps := ConverteXMLtoUTF8(Nota.XmlRps);
  Nota.XmlRps := ChangeLineBreak(Nota.XmlRps, '');

  if ConfigAssinar.RpsSubstituirNFSe then
  begin
    IdAttrSig := SetIdSignatureValue(Nota.XmlRps,
               ConfigMsgDados.XmlRps.DocElemento, IdAttr);

    Nota.XmlRps := FAOwner.SSL.Assinar(Nota.XmlRps,
                                       PrefixoTS + ConfigMsgDados.XmlRps.DocElemento,
                                       ConfigMsgDados.XmlRps.InfElemento, '', '', '',
                                       IdAttr, IdAttrSig);
  end;

  SalvarXmlRps(Nota);

  NumRps := Nota.NFSe.IdentificacaoRps.Numero;

  xRps := RemoverDeclaracaoXML(Nota.XmlRps);
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

  aParams := TNFSeParamsResponse.Create;
  try
    aParams.Clear;
    aParams.Xml := xRps;
    aParams.TagEnvio := TagEnvio;
    aParams.Prefixo := Prefixo;
    aParams.Prefixo2 := PrefixoTS;
    aParams.NameSpace := NameSpace;
    aParams.NameSpace2 := '';
    aParams.IdAttr := IdAttr;
    aParams.Versao := '';
    aParams.Serie := '';
    aParams.Motivo := '';
    aParams.CodigoVerificacao := '';

    GerarMsgDadosSubstituiNFSe(Response, aParams);
  finally
    aParams.Free;
  end;
end;

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosSubstituiNFSe(
  Response: TNFSeSubstituiNFSeResponse; Params: TNFSeParamsResponse);
begin
  with Params do
  begin
    Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                           '<' + Prefixo + 'SubstituicaoNfse' + IdAttr + '>' +
                             Response.PedCanc +
                             Xml +
                           '</' + Prefixo + 'SubstituicaoNfse>' +
                         '</' + Prefixo + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderABRASFv2.LerCancelamento(const ANode: TACBrXmlNode;
  const Response: TNFSeWebserviceResponse);
var
  AuxNode, ANodeNfseCancelamento: TACBrXmlNode;
begin
  ANodeNfseCancelamento := ANode.Childrens.FindAnyNs('NfseCancelamento');

  if ANodeNfseCancelamento <> nil then
  begin
    AuxNode := ANodeNfseCancelamento.Childrens.FindAnyNs('Confirmacao');

    if AuxNode = nil then
      AuxNode := ANodeNfseCancelamento.Childrens.FindAnyNs('ConfirmacaoCancelamento');

    if Assigned(AuxNode) then
    begin
      Response.DataCanc := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('DataHora'), FpFormatoDataHora);
      Response.DescSituacao := '';
    end;

    if Response.DataCanc > 0 then
      Response.DescSituacao := 'Nota Cancelada';

  end;
end;

procedure TACBrNFSeProviderABRASFv2.LerSubstituicao(const ANode: TACBrXmlNode;
  const Response: TNFSeWebServiceResponse);
var
  AuxNode, AuxNodeSubs: TACBrXmlNode;
begin
  AuxNode := ANode.Childrens.FindAnyNs('NfseSubstituicao');

  if AuxNode <> nil then
  begin
    AuxNodeSubs := AuxNode.Childrens.FindAnyNs('SubstituicaoNfse');

    if AuxNodeSubs <> nil then
      Response.NumNotaSubstituidora := ObterConteudoTag(AuxNodeSubs.Childrens.FindAnyNs('NfseSubstituidora'), tcStr);

    if Response.NumNotaSubstituidora <> '' then
      Response.DescSituacao := 'Nota Substituida';
  end;
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode, ANodeSubstituida: TACBrXmlNode;
  AErro: TNFSeEventoCollectionItem;

  function LocalizarNFSeRetorno(const RootNode: TACBrXmlNode): string;
  var
    ANode, AuxNode: TACBrXmlNode;
    NumNFSe: String;
    ANota: TNotaFiscal;
  begin
    Result := '';
    ANode := RootNode.Childrens.FindAnyNs('CompNfse');

    if not Assigned(ANode) then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Cod203;
      AErro.Descricao := ACBrStr(Desc203);
      Exit;
    end;

    AuxNode := ANode.Childrens.FindAnyNs('Nfse');
    if not Assigned(AuxNode) then Exit;

    AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
    if not Assigned(AuxNode) then Exit;

    AuxNode := AuxNode.Childrens.FindAnyNs('Numero');

    if AuxNode <> nil then
    begin
      NumNFSe := AuxNode.AsString;

      ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

      ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
      SalvarXmlNfse(ANota);
      Result := NumNFSe;
    end;
  end;

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

      ANode := Document.Root.Childrens.FindAnyNs('RetSubstituicao');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod205;
        AErro.Descricao := ACBrStr(Desc205);
        Exit;
      end;

      ProcessarMensagemErros(ANode, Response);

      AuxNode := ANode.Childrens.FindAnyNs('NfseSubstituida');

      if not Assigned(AuxNode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod206;
        AErro.Descricao := ACBrStr(Desc206);
        Exit;
      end
      else
      begin
        Response.NumNotaSubstituida := LocalizarNFSeRetorno(AuxNode);

        ANodeSubstituida := AuxNode.Childrens.FindAnyNs('CompNfse');

        if not Assigned(ANodeSubstituida) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit;
        end;

        ANodeSubstituida := ANodeSubstituida.Childrens.FindAnyNs('Nfse');
        if not Assigned(ANodeSubstituida) then Exit;

        ANodeSubstituida := ANodeSubstituida.Childrens.FindAnyNs('InfNfse');

        with Response do
        begin
          CodigoVerificacao := ObterConteudoTag(ANodeSubstituida.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
          Data := ObterConteudoTag(ANodeSubstituida.Childrens.FindAnyNs('DataEmissao'), FpFormatoDataEmissao);
        end;
      end;

      AuxNode := ANode.Childrens.FindAnyNs('NfseSubstituidora');

      if not Assigned(AuxNode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod207;
        AErro.Descricao := ACBrStr(Desc207);
        Exit;
      end
      else
      begin
        Response.NumNotaSubstituidora := LocalizarNFSeRetorno(AuxNode);
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

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosGerarToken(
  Response: TNFSeGerarTokenResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.PrepararGerarToken(
  Response: TNFSeGerarTokenResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoGerarToken(
  Response: TNFSeGerarTokenResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderABRASFv2.PrepararEnviarEvento(
  Response: TNFSeEnviarEventoResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosEnviarEvento(
  Response: TNFSeEnviarEventoResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoEnviarEvento(
  Response: TNFSeEnviarEventoResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderABRASFv2.PrepararConsultarEvento(
  Response: TNFSeConsultarEventoResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosConsultarEvento(
  Response: TNFSeConsultarEventoResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultarEvento(
  Response: TNFSeConsultarEventoResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderABRASFv2.PrepararConsultarDFe(
  Response: TNFSeConsultarDFeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosConsultarDFe(
  Response: TNFSeConsultarDFeResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultarDFe(
  Response: TNFSeConsultarDFeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderABRASFv2.PrepararConsultarParam(
  Response: TNFSeConsultarParamResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosConsultarParam(
  Response: TNFSeConsultarParamResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultarParam(
  Response: TNFSeConsultarParamResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderABRASFv2.PrepararConsultarSeqRps(Response: TNFSeConsultarSeqRpsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosConsultarSeqRps(Response: TNFSeConsultarSeqRpsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultarSeqRps(Response: TNFSeConsultarSeqRpsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderABRASFv2.PrepararConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderABRASFv2.GerarMsgDadosConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderABRASFv2.TratarRetornoConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

function TACBrNFSeProviderABRASFv2.VerificarAlerta(const ACodigo,
  AMensagem: string): Boolean;
begin
  Result := (AMensagem <> '') and (Pos('L000', ACodigo) > 0);
end;

function TACBrNFSeProviderABRASFv2.VerificarErro(const ACodigo,
  AMensagem: string): Boolean;
begin
  Result := (AMensagem <> '') and (Pos('L000', ACodigo) = 0);
end;

procedure TACBrNFSeProviderABRASFv2.ProcessarMensagemErros(RootNode: TACBrXmlNode;
  Response: TNFSeWebserviceResponse; const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AAlerta: TNFSeEventoCollectionItem;
  Codigo, Mensagem: string;

procedure ProcessarErro(ErrorNode: TACBrXmlNode; const ACodigo, AMensagem: string);
var
  Item: TNFSeEventoCollectionItem;
begin
  if AMensagem = '' then
    Exit;

  if VerificarAlerta(ACodigo, AMensagem) then
    Item := Response.Alertas.New
  else if VerificarErro(ACodigo, AMensagem) then
    Item := Response.Erros.New
  else
    Exit;

  Item.Codigo := ACodigo;
  Item.Descricao := AMensagem;
  Item.Correcao := ObterConteudoTag(ErrorNode.Childrens.FindAnyNs('Correcao'), tcStr);
end;

procedure ProcessarErros;
var
  I: Integer;
begin
  if Assigned(ANode) then
  begin
    ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

    if Assigned(ANodeArray) then
    begin
      for I := Low(ANodeArray) to High(ANodeArray) do
      begin
        Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Codigo'), tcStr);
        Mensagem := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Mensagem'), tcStr);

        ProcessarErro(ANodeArray[I], Codigo, Mensagem);
      end;
    end
    else
    begin
      Codigo := ObterConteudoTag(ANode.Childrens.FindAnyNs('Codigo'), tcStr);
      Mensagem := ObterConteudoTag(ANode.Childrens.FindAnyNs('Mensagem'), tcStr);

      ProcessarErro(ANode, Codigo, Mensagem);
    end;
  end;
end;

begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode.Childrens.FindAnyNs('MensagemRetorno');

  ProcessarErros;

  ANode := RootNode.Childrens.FindAnyNs('ListaMensagemRetornoLote');

  ProcessarErros;

  ANode := RootNode.Childrens.FindAnyNs('ListaMensagemAlertaRetorno');

  if Assigned(ANode) then
  begin
    ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

    if Assigned(ANodeArray) then
    begin
      for I := Low(ANodeArray) to High(ANodeArray) do
      begin
        Mensagem := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Mensagem'), tcStr);

        if Mensagem <> '' then
        begin
          AAlerta := Response.Alertas.New;
          AAlerta.Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Codigo'), tcStr);
          AAlerta.Descricao := Mensagem;
          AAlerta.Correcao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Correcao'), tcStr);
        end;
      end;
    end
    else
    begin
      Mensagem := ObterConteudoTag(ANode.Childrens.FindAnyNs('Mensagem'), tcStr);

      if Mensagem <> '' then
      begin
        AAlerta := Response.Alertas.New;
        AAlerta.Codigo := ObterConteudoTag(ANode.Childrens.FindAnyNs('Codigo'), tcStr);
        AAlerta.Descricao := Mensagem;
        AAlerta.Correcao := ObterConteudoTag(ANode.Childrens.FindAnyNs('Correcao'), tcStr);
      end;
    end;
  end;
end;

end.
