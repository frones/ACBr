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

unit ACBrNFSeXProviderProprio;

interface

uses
  SysUtils, Classes,
  ACBrXmlDocument,
  ACBrNFSeXProviderBase, ACBrNFSeXWebservicesResponse;

type

  { TACBrNFSeProviderProprio }

  TACBrNFSeProviderProprio = class(TACBrNFSeXProvider)
  protected
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

    procedure PrepararConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure GerarMsgDadosConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); virtual;
    procedure TratarRetornoConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); virtual;

    procedure PrepararConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure GerarMsgDadosConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); virtual;
    procedure TratarRetornoConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse); virtual;

    procedure PrepararConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure GerarMsgDadosConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); virtual;
    procedure TratarRetornoConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse); virtual;

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

    function AplicarXMLtoUTF8(AXMLRps: String): String; virtual;
    function AplicarLineBreak(AXMLRps: String; const ABreak: String): String; virtual;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'ListaMensagemRetorno';
                                     const AMessageTag: string = 'MensagemRetorno'); virtual;

  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXNotasFiscais, ACBrNFSeXConsts, ACBrNFSeXConversao,
  ACBrNFSeXWebserviceBase;

{ TACBrNFSeProviderProprio }

procedure TACBrNFSeProviderProprio.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.Layout := loProprio;
end;

procedure TACBrNFSeProviderProprio.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  aParams: TNFSeParamsResponse;
  Nota: TNotaFiscal;
  Versao, IdAttr, NameSpace, NameSpaceLote, ListaRps, xRps,
  TagEnvio, Prefixo, PrefixoTS: string;
  I: Integer;
begin
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

    Nota.XmlRps := AplicarXMLtoUTF8(Nota.XmlRps);
    Nota.XmlRps := AplicarLineBreak(Nota.XmlRps, '');

    if (ConfigAssinar.Rps and (Response.ModoEnvio in [meLoteAssincrono, meLoteSincrono])) or
       (ConfigAssinar.RpsGerarNFSe and (Response.ModoEnvio = meUnitario)) then
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

  ListaRps := AplicarLineBreak(ListaRps, '');

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

procedure TACBrNFSeProviderProprio.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.GerarMsgDadosConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.GerarMsgDadosConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.GerarMsgDadosConsultaporRps(
  Response: TNFSeConsultaNFSeporRpsResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
begin
  if Response.InfConsultaNFSe.tpConsulta = tcPorNumero then
  begin
    TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
    raise EACBrDFeException.Create(ERR_NAO_IMP);
  end
  else
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
        AErro.Descricao := ACBrStr(Desc001);
      end;
    end;
  end;
end;

procedure TACBrNFSeProviderProprio.GerarMsgDadosConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse);
begin
  case Response.InfConsultaNFSe.tpConsulta of
    tcPorPeriodo,
    tcPorFaixa:
      TratarRetornoConsultaNFSeporFaixa(Response);
    tcServicoPrestado:
      TratarRetornoConsultaNFSeServicoPrestado(Response);
    tcServicoTomado:
      TratarRetornoConsultaNFSeServicoTomado(Response);
  end;
end;

procedure TACBrNFSeProviderProprio.PrepararConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.GerarMsgDadosConsultaNFSeporFaixa(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararConsultaNFSeServicoPrestado(
  Response: TNFSeConsultaNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.GerarMsgDadosConsultaNFSeServicoPrestado(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultaNFSeServicoPrestado(
  Response: TNFSeConsultaNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararConsultaNFSeServicoTomado(
  Response: TNFSeConsultaNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.GerarMsgDadosConsultaNFSeServicoTomado(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultaNFSeServicoTomado(
  Response: TNFSeConsultaNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.GerarMsgDadosCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  aParams: TNFSeParamsResponse;
  Nota: TNotaFiscal;
  IdAttr, xRps, NameSpace, NumRps, TagEnvio, Prefixo, PrefixoTS: string;
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

  Nota.XmlRps := AplicarXMLtoUTF8(Nota.XmlRps);
  Nota.XmlRps := AplicarLineBreak(Nota.XmlRps, '');

  if ConfigAssinar.RpsSubstituirNFSe then
  begin
    Nota.XmlRps := FAOwner.SSL.Assinar(Nota.XmlRps,
                                       PrefixoTS + ConfigMsgDados.XmlRps.DocElemento,
                                       ConfigMsgDados.XmlRps.InfElemento, '', '', '', IdAttr);
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

procedure TACBrNFSeProviderProprio.GerarMsgDadosSubstituiNFSe(
  Response: TNFSeSubstituiNFSeResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

function TACBrNFSeProviderProprio.AplicarXMLtoUTF8(AXMLRps: String): String;
begin
  Result := ConverteXMLtoUTF8(AXMLRps);
end;

function TACBrNFSeProviderProprio.AplicarLineBreak(AXMLRps: String; const ABreak: String): String;
begin
  Result := ChangeLineBreak(AXMLRps, ABreak);
end;

procedure TACBrNFSeProviderProprio.GerarMsgDadosGerarToken(
  Response: TNFSeGerarTokenResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.PrepararGerarToken(
  Response: TNFSeGerarTokenResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.TratarRetornoGerarToken(
  Response: TNFSeGerarTokenResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararEnviarEvento(
  Response: TNFSeEnviarEventoResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.GerarMsgDadosEnviarEvento(
  Response: TNFSeEnviarEventoResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.TratarRetornoEnviarEvento(
  Response: TNFSeEnviarEventoResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararConsultarEvento(
  Response: TNFSeConsultarEventoResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.GerarMsgDadosConsultarEvento(
  Response: TNFSeConsultarEventoResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultarEvento(
  Response: TNFSeConsultarEventoResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararConsultarDFe(
  Response: TNFSeConsultarDFeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.GerarMsgDadosConsultarDFe(
  Response: TNFSeConsultarDFeResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultarDFe(
  Response: TNFSeConsultarDFeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararConsultarParam(
  Response: TNFSeConsultarParamResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.GerarMsgDadosConsultarParam(
  Response: TNFSeConsultarParamResponse; Params: TNFSeParamsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultarParam(
  Response: TNFSeConsultarParamResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararConsultarSeqRps(
  Response: TNFSeConsultarSeqRpsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.GerarMsgDadosConsultarSeqRps(
  Response: TNFSeConsultarSeqRpsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
  TACBrNFSeX(FAOwner).SetStatus(stNFSeIdle);
  raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultarSeqRps(
  Response: TNFSeConsultarSeqRpsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag: string; const AMessageTag: string);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

end.
