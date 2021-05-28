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
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXProviderBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeProviderProprio = class(TACBrNFSeXProvider)
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
//    procedure AssinarConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure TratarRetornoConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); virtual;

    procedure PrepararConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure TratarRetornoConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse); virtual;

    procedure PrepararConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse); virtual;
    procedure TratarRetornoConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse); virtual;

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
  ACBrUtil,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXNotasFiscais, ACBrNFSeXConversao;

{ TACBrNFSeProviderProprio }

procedure TACBrNFSeProviderProprio.Configuracao;
begin
  inherited Configuracao;

end;

procedure TACBrNFSeProviderProprio.PrepararEmitir(Response: TNFSeEmiteResponse);
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
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse);
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
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  XmlConsulta, NameSpace: string;
begin
  if Response.InfConsultaNFSe.tpConsulta in [tcPorNumeroURLRetornado] then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := '999';
    AErro.Descricao := 'Consulta não disponivel neste provedor.';
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.Metodo := tmConsultarNFSePorFaixa;

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
    NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarNFSe.xmlns + '"';

  Response.XmlEnvio := '<ConsultarNfseFaixaEnvio' + NameSpace + '>' +
                         '<Prestador>' +
                           '<CpfCnpj>' +
                             '<Cnpj>' + OnlyNumber(Emitente.CNPJ) + '</Cnpj>' +
                           '</CpfCnpj>' +
                           '<InscricaoMunicipal>' +
                              OnlyNumber(Emitente.InscMun) +
                           '</InscricaoMunicipal>' +
                         '</Prestador>' +
                         XmlConsulta +
                         '<Pagina>' +
                            IntToStr(Response.InfConsultaNFSe.Pagina) +
                         '</Pagina>' +
                       '</ConsultarNfseFaixaEnvio>';
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse);
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
        AErro.Codigo := '999';
        AErro.Descricao := 'WebService retornou um XML vazio.';
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := '999';
        AErro.Descricao := 'Lista de NFSe não encontrada! (ListaNfse)';
        Exit;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('CompNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := '999';
        AErro.Descricao := 'Não foi retornado nenhuma NFSe';
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
        AErro.Codigo := '999';
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderProprio.PrepararConsultaNFSeServicoPrestado(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  XmlConsulta, NameSpace, InscMun: string;
begin
  if Response.InfConsultaNFSe.tpConsulta in [tcPorNumeroURLRetornado] then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := '999';
    AErro.Descricao := 'Consulta não disponivel neste provedor.';
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.Metodo := tmConsultarNFSeServicoPrestado;

  if OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) <> '' then
    XmlConsulta := '<NumeroNfse>' +
                      OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) +
                   '</NumeroNfse>'
  else
    XmlConsulta := '';

  if (Response.InfConsultaNFSe.DataInicial > 0) and (Response.InfConsultaNFSe.DataFinal > 0) then
    XmlConsulta := XmlConsulta +
                     '<PeriodoEmissao>' +
                       '<DataInicial>' +
                          FormatDateTime('yyyy-mm-dd', Response.InfConsultaNFSe.DataInicial) +
                       '</DataInicial>' +
                       '<DataFinal>' +
                          FormatDateTime('yyyy-mm-dd', Response.InfConsultaNFSe.DataFinal) +
                       '</DataFinal>' +
                     '</PeriodoEmissao>';

  if NaoEstaVAzio(Response.InfConsultaNFSe.CNPJTomador) then
  begin
    if NaoEstaVazio(Response.InfConsultaNFSe.IMTomador) then
      InscMun := '<InscricaoMunicipal>' +
                    OnlyNumber(Response.InfConsultaNFSe.IMTomador) +
                 '</InscricaoMunicipal>'
    else
      InscMun := '';

    XmlConsulta := XmlConsulta +
                     '<Tomador>' +
                       '<CpfCnpj>' +
                          OnlyNumber(Response.InfConsultaNFSe.CNPJTomador) +
                       '</CpfCnpj>' +
                       InscMun +
                     '</Tomador>';
  end;

  if NaoEstaVAzio(Response.InfConsultaNFSe.CNPJInter) then
  begin
    if NaoEstaVazio(Response.InfConsultaNFSe.IMInter) then
      InscMun := '<InscricaoMunicipal>' +
                    OnlyNumber(Response.InfConsultaNFSe.IMInter) +
                 '</InscricaoMunicipal>'
    else
      InscMun := '';

    XmlConsulta := XmlConsulta +
                     '<Intermediario>' +
                       '<CpfCnpj>' +
                          OnlyNumber(Response.InfConsultaNFSe.CNPJInter) +
                       '</CpfCnpj>' +
                       InscMun +
                     '</Intermediario>';
  end;

  if EstaVazio(ConfigMsgDados.ConsultarNFSe.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarNFSe.xmlns + '"';

  Response.XmlEnvio := '<ConsultarNfseServicoPrestadoEnvio' + NameSpace + '>' +
                         '<Prestador>' +
                           '<CpfCnpj>' +
                             '<Cnpj>' + OnlyNumber(Emitente.CNPJ) + '</Cnpj>' +
                           '</CpfCnpj>' +
                           '<InscricaoMunicipal>' +
                              OnlyNumber(Emitente.InscMun) +
                           '</InscricaoMunicipal>' +
                         '</Prestador>' +
                         XmlConsulta +
                         '<Pagina>' +
                            IntToStr(Response.InfConsultaNFSe.Pagina) +
                         '</Pagina>' +
                       '</ConsultarNfseServicoPrestadoEnvio>';
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultaNFSeServicoPrestado(
  Response: TNFSeConsultaNFSeResponse);
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
        AErro.Codigo := '999';
        AErro.Descricao := 'WebService retornou um XML vazio.';
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := '999';
        AErro.Descricao := 'Lista de NFSe não encontrada! (ListaNfse)';
        Exit;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('CompNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := '999';
        AErro.Descricao := 'Não foi retornado nenhuma NFSe';
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
        AErro.Codigo := '999';
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderProprio.PrepararConsultaNFSeServicoTomado(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  XmlConsulta, NameSpace, InscMun: string;
begin
  if Response.InfConsultaNFSe.tpConsulta in [tcPorNumeroURLRetornado] then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := '999';
    AErro.Descricao := 'Consulta não disponivel neste provedor.';
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.Metodo := tmConsultarNFSeServicoTomado;

  if OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) <> '' then
    XmlConsulta := '<NumeroNfse>' +
                      OnlyNumber(Response.InfConsultaNFSe.NumeroIniNFSe) +
                   '</NumeroNfse>'
  else
    XmlConsulta := '';

  if (Response.InfConsultaNFSe.DataInicial > 0) and (Response.InfConsultaNFSe.DataFinal > 0) then
    XmlConsulta := XmlConsulta +
                     '<PeriodoEmissao>' +
                       '<DataInicial>' +
                          FormatDateTime('yyyy-mm-dd', Response.InfConsultaNFSe.DataInicial) +
                       '</DataInicial>' +
                       '<DataFinal>' +
                          FormatDateTime('yyyy-mm-dd', Response.InfConsultaNFSe.DataFinal) +
                       '</DataFinal>' +
                     '</PeriodoEmissao>';

  if NaoEstaVAzio(Response.InfConsultaNFSe.CNPJPrestador) then
  begin
    if NaoEstaVazio(Response.InfConsultaNFSe.IMPrestador) then
      InscMun := '<InscricaoMunicipal>' +
                    OnlyNumber(Response.InfConsultaNFSe.IMPrestador) +
                 '</InscricaoMunicipal>'
    else
      InscMun := '';

    XmlConsulta := XmlConsulta +
                     '<Prestador>' +
                       '<CpfCnpj>' +
                          OnlyNumber(Response.InfConsultaNFSe.CNPJPrestador) +
                       '</CpfCnpj>' +
                       InscMun +
                     '</Prestador>';
  end;

  if NaoEstaVAzio(Response.InfConsultaNFSe.CNPJTomador) then
  begin
    if NaoEstaVazio(Response.InfConsultaNFSe.IMTomador) then
      InscMun := '<InscricaoMunicipal>' +
                    OnlyNumber(Response.InfConsultaNFSe.IMTomador) +
                 '</InscricaoMunicipal>'
    else
      InscMun := '';

    XmlConsulta := XmlConsulta +
                     '<Tomador>' +
                       '<CpfCnpj>' +
                          OnlyNumber(Response.InfConsultaNFSe.CNPJTomador) +
                       '</CpfCnpj>' +
                       InscMun +
                     '</Tomador>';
  end;

  if NaoEstaVAzio(Response.InfConsultaNFSe.CNPJInter) then
  begin
    if NaoEstaVazio(Response.InfConsultaNFSe.IMInter) then
      InscMun := '<InscricaoMunicipal>' +
                    OnlyNumber(Response.InfConsultaNFSe.IMInter) +
                 '</InscricaoMunicipal>'
    else
      InscMun := '';

    XmlConsulta := XmlConsulta +
                     '<Intermediario>' +
                       '<CpfCnpj>' +
                          OnlyNumber(Response.InfConsultaNFSe.CNPJInter) +
                       '</CpfCnpj>' +
                       InscMun +
                     '</Intermediario>';
  end;

  if EstaVazio(ConfigMsgDados.ConsultarNFSe.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarNFSe.xmlns + '"';

  Response.XmlEnvio := '<ConsultarNfseServicoTomadoEnvio' + NameSpace + '>' +
                         '<Consulente>' +
                           '<CpfCnpj>' +
                             '<Cnpj>' + OnlyNumber(Emitente.CNPJ) + '</Cnpj>' +
                           '</CpfCnpj>' +
                           '<InscricaoMunicipal>' +
                              OnlyNumber(Emitente.InscMun) +
                           '</InscricaoMunicipal>' +
                         '</Consulente>' +
                         XmlConsulta +
                         '<Pagina>' +
                            IntToStr(Response.InfConsultaNFSe.Pagina) +
                         '</Pagina>' +
                       '</ConsultarNfseServicoTomadoEnvio>';
end;

procedure TACBrNFSeProviderProprio.TratarRetornoConsultaNFSeServicoTomado(
  Response: TNFSeConsultaNFSeResponse);
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
        AErro.Codigo := '999';
        AErro.Descricao := 'WebService retornou um XML vazio.';
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root.Childrens.FindAnyNs('ListaNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := '999';
        AErro.Descricao := 'Lista de NFSe não encontrada! (ListaNfse)';
        Exit;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('CompNfse');
      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := '999';
        AErro.Descricao := 'Não foi retornado nenhuma NFSe';
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
        AErro.Codigo := '999';
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderProprio.PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse);
begin
  // Deve ser implementado para cada provedor que tem o seu próprio layout
end;

procedure TACBrNFSeProviderProprio.ProcessarMensagemErros(const RootNode: TACBrXmlNode;
  const Response: TNFSeWebserviceResponse; AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  {
    Para os serviços que recepcionam o Lote de Rps tanto no modo assíncrono
    quanto no modo síncrono do provedor RLZ o nome da tag é diferente.
  }
  if (ANode = nil) then
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
