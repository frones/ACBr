{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
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

unit ATM.Provider;

interface

uses
  SysUtils, Classes, Variants,
  ACBrDFeSSL,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrANeDocumentos,
  ACBrANe.Classes, ACBrANe.Conversao,
  ACBrANe.ProviderProprio,
  ACBrANe.WebServicesBase, ACBrANe.WebServicesResponse;

type
  TACBrANeWebserviceATM = class(TACBrANeWebserviceSoap11)
  public
    function Enviar(const ACabecalho, AMSG: string): string; override;

  end;

  TACBrANeProviderATM = class (TACBrANeProviderProprio)
  private
    function RemoverGrupoInfSuplementares(const XML, Grupo: string): string;
  protected
    procedure Configuracao; override;

    function CriarServiceClient(const AMetodo: TMetodo): TACBrANeWebservice; override;

    procedure PrepararEnviar(Response: TANeEnviarResponse); override;
    procedure TratarRetornoEnviar(Response: TANeEnviarResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TANeWebserviceResponse;
                                     const AListTag: string = 'Erros';
                                     const AMessageTag: string = 'Erro'); override;

    procedure ProcessarMensagemAlertas(RootNode: TACBrXmlNode;
                                       Response: TANeWebserviceResponse;
                                       const AListTag: string = 'Infos';
                                       const AMessageTag: string = 'Info');
  end;

implementation

uses
  ACBrDFeException,
  ACBrUtil.XMLHTML, ACBrUtil.Strings,
  ACBrANe, ACBrANe.Consts;

{ TACBrANeProviderATM }

procedure TACBrANeProviderATM.Configuracao;
begin
  inherited Configuracao;

  with ConfigSchemas do
  begin
    Enviar := 'ANe.xsd';
    Consultar := '***';

    Validar := False;
  end;
end;

function TACBrANeProviderATM.CriarServiceClient(
  const AMetodo: TMetodo): TACBrANeWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrANeWebserviceATM.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrANeProviderATM.ProcessarMensagemErros(RootNode: TACBrXmlNode;
  Response: TANeWebserviceResponse; const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TANeEventoCollectionItem;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Codigo'), tcStr);
    AErro.Descricao := ACBrStr(ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Descricao'), tcStr));
    AErro.Correcao := '';
  end;
end;

procedure TACBrANeProviderATM.ProcessarMensagemAlertas(RootNode: TACBrXmlNode;
  Response: TANeWebserviceResponse; const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AAlerta: TANeEventoCollectionItem;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    AAlerta := Response.Alertas.New;
    AAlerta.Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Codigo'), tcStr);
    AAlerta.Descricao := ACBrStr(ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Descricao'), tcStr));
    AAlerta.Correcao := '';
  end;
end;

function TACBrANeProviderATM.RemoverGrupoInfSuplementares(const XML,
  Grupo: string): string;
var
  IniGrupo, FimGrupo: Integer;
begin
  IniGrupo := Pos('<' + Grupo + '>', XML);
  if IniGrupo > 0 then
  begin
    FimGrupo := Pos('</' + Grupo + '>', XML) + Length(Grupo) + 3;

    Result := Copy(XML, 1, IniGrupo -1) + Copy(XML, FimGrupo, Length(XML));
  end
  else
    Result := XML;
end;

procedure TACBrANeProviderATM.PrepararEnviar(Response: TANeEnviarResponse);
var
//  AErro: TANeEventoCollectionItem;
  Documento: TDocumento;
begin
  Documento := TACBrANe(FAOwner).Documentos.Items[0];

  case TACBrANe(FAOwner).Configuracoes.Geral.TipoDoc of
    tdCTe:
      begin
        Documento.ANe.xmlDFe :=
             RemoverGrupoInfSuplementares(Documento.ANe.xmlDFe, 'infCTeSupl');
        Documento.ANe.xmlDFe := '<xmlCTe><![CDATA[' +
                                   RemoverDeclaracaoXML(Documento.ANe.xmlDFe) +
                                ']]></xmlCTe>';
      end;

    tdNFe:
      begin
        Documento.ANe.xmlDFe := Documento.ANe.xmlDFe;
        Documento.ANe.xmlDFe := '<xmlNFe><![CDATA[' +
                                   RemoverDeclaracaoXML(Documento.ANe.xmlDFe) +
                                ']]></xmlNFe>';
      end;

    tdMDFe:
      begin
        Documento.ANe.xmlDFe :=
              RemoverGrupoInfSuplementares(Documento.ANe.xmlDFe, 'infMDFeSupl');
        Documento.ANe.xmlDFe := '<xmlMDFe><![CDATA[' +
                                   RemoverDeclaracaoXML(Documento.ANe.xmlDFe) +
                                ']]></xmlMDFe>';
      end;
  else
    Documento.ANe.xmlDFe := '';
  end;

  if TACBrANe(FAOwner).Configuracoes.Geral.TipoDoc <> tdAddBackMail then
    Response.ArquivoEnvio := '<usuario>' +
                                TACBrANe(FAOwner).Configuracoes.Geral.Usuario +
                             '</usuario>' +
                             '<senha>' +
                                TACBrANe(FAOwner).Configuracoes.Geral.Senha +
                             '</senha>' +
                             '<codatm>' +
                                TACBrANe(FAOwner).Configuracoes.Geral.CodATM +
                             '</codatm>' +
                             Documento.ANe.xmlDFe
  else
    Response.ArquivoEnvio := '<usuario>' +
                                TACBrANe(FAOwner).Configuracoes.Geral.Usuario +
                             '</usuario>' +
                             '<senha>' +
                                TACBrANe(FAOwner).Configuracoes.Geral.Senha +
                             '</senha>' +
                             '<codatm>' +
                                TACBrANe(FAOwner).Configuracoes.Geral.CodATM +
                             '</codatm>' +
                             '<aplicacao>' +
                                Documento.ANe.aplicacao +
                             '</aplicacao>' +
                             '<assunto>' +
                                Documento.ANe.assunto +
                             '</assunto>' +
                             '<remetentes>' +
                                Documento.ANe.remetentes +
                             '</remetentes>' +
                             '<destinatarios>' +
                                Documento.ANe.destinatarios +
                             '</destinatarios>' +
                             '<corpo>' +
                                Documento.ANe.corpo +
                             '</corpo>' +
                             '<chave>' +
                                Documento.ANe.chave +
                             '</chave>' +
                             '<chaveresp>' +
                                Documento.ANe.chaveresp +
                             '</chaveresp>';
end;

procedure TACBrANeProviderATM.TratarRetornoEnviar(Response: TANeEnviarResponse);
var
  Document: TACBrXmlDocument;
  AErro: TANeEventoCollectionItem;
  ANode, ANodeAux: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  I: Integer;
  ADadosSeguro: TDadosSeguroCollectionItem;
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

      ANode := ANode.Childrens.FindAnyNs('Response');

      if ANode = nil then
        ANode := ANode.Childrens.FindAnyNs('return');

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ProcessarMensagemAlertas(ANode, Response);

      Response.Numero := ObterConteudoTag(ANode.Childrens.FindAnyNs('Numero'), tcStr);
      Response.Serie := ObterConteudoTag(ANode.Childrens.FindAnyNs('Serie'), tcStr);
      Response.Filial := ObterConteudoTag(ANode.Childrens.FindAnyNs('Filial'), tcStr);
      Response.CNPJCliente := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJCli'), tcStr);
      Response.tpDoc := ObterConteudoTag(ANode.Childrens.FindAnyNs('tpDoc'), tcStr);

      ANodeAux := ANode.Childrens.FindAnyNs('Averbado');

      if ANodeAux <> nil then
      begin
        Response.DataHora := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('dhAverbacao'), tcDatHor);
        Response.Protocolo := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('Protocolo'), tcStr);
      end;

      ANodeAux := ANode.Childrens.FindAnyNs('Declaracao');

      if ANodeAux <> nil then
      begin
        Response.DataHora := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('dhChancela'), tcDatHor);
        Response.Protocolo := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('Protocolo'), tcStr);
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('DadosSeguro');

      if ANodeAux <> nil then
      begin
      end;
      for I := Low(ANodeArray) to High(ANodeArray) do
      begin
        ANodeAux := ANodeArray[I];

        ADadosSeguro := Response.DadosSeguro.New;
        ADadosSeguro.NumeroAverbacao := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('NumeroAverbacao'), tcStr);
        ADadosSeguro.CNPJSeguradora := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('CNPJSeguradora'), tcStr);
        ADadosSeguro.NomeSeguradora := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('NomeSeguradora'), tcStr);
        ADadosSeguro.NumApolice := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('NumApolice'), tcStr);
        ADadosSeguro.TpMov := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('TpMov'), tcStr);
        ADadosSeguro.TpDDR := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('TpDDR'), tcStr);
        ADadosSeguro.ValorAverbado := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('ValorAverbado'), tcDe2);
        ADadosSeguro.RamoAverbado := ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('RamoAverbado'), tcStr);
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

{ TACBrANeWebserviceATM }

function TACBrANeWebserviceATM.Enviar(const ACabecalho, AMSG: string): string;
var
  Request, sTipoDoc: string;
begin
  case TACBrANe(FPDFeOwner).Configuracoes.Geral.TipoDoc of
    tdCTe: sTipoDoc := 'averbaCTe';
    tdNFe: sTipoDoc := 'averbaNFe';
    tdMDFe: sTipoDoc := 'declaraMDFe';
  else
    sTipoDoc := 'AddBackMail';
  end;

  FPMsgOrig := '<' + sTipoDoc + '>' + AMSG + '</' + sTipoDoc + '>';

  Request := '<urn:' + sTipoDoc + 'Request>';
  Request := Request + AMSG;
  Request := Request + '</urn:' + sTipoDoc + 'Request>';

  Result := Executar('urn:ATMWebSvr#' + sTipoDoc, Request,
                     [''], ['xmlns:urn="urn:ATMWebSvr"']);
end;

end.
