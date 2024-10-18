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

unit PortoSeguro.Provider;

interface

uses
  SysUtils, Classes, Variants,
  ACBrDFeSSL,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrANeDocumentos,
  ACBrANe.Classes, ACBrANe.Conversao,
  ACBrANe.ProviderProprio,
  ACBrANe.WebservicesBase, ACBrANe.WebServicesResponse;

type
  TACBrANeWebservicePortoSeguro = class(TACBrANeWebserviceRest)
  public
    function Enviar(const ACabecalho, AMSG: string): string; override;
    function Consultar(const ACabecalho, AMSG: string): string; override;

  end;

  TACBrANeProviderPortoSeguro = class (TACBrANeProviderProprio)
  private
    FpPath: string;
    FpMethod: string;
  protected
    procedure Configuracao; override;

    function CriarServiceClient(const AMetodo: TMetodo): TACBrANeWebservice; override;

    procedure PrepararEnviar(Response: TANeEnviarResponse); override;
    procedure TratarRetornoEnviar(Response: TANeEnviarResponse); override;

    procedure PrepararConsultar(Response: TANeConsultarResponse); override;
    procedure TratarRetornoConsultar(Response: TANeConsultarResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TANeWebserviceResponse;
                                     const AListTag: string = '';
                                     const AMessageTag: string = ''); override;
  end;

var
  XmlDocumento: string;

implementation

uses
  synacode,
  ACBrUtil.Strings,
  ACBrUtil.Base,
  ACBrANe.Consts,
  ACBrDFeException,
  ACBrANe;

{ TACBrANeProviderPortoSeguro }

procedure TACBrANeProviderPortoSeguro.Configuracao;
begin
  inherited Configuracao;
(*
  with ConfigGeral do
  begin
    UseCertificateHTTP := True;
    UseAuthorizationHeader := False;

    Identificador := 'Id';
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '1.00';
    VersaoAtrib := '1.00';
    AtribVerLote := '';
  end;

  // Define o NameSpace utilizado por todos os serviços disponibilizados pelo
  // provedor
  SetXmlNameSpace('');

  with ConfigAssinar do
  begin
    Enviar    := False;
    Consultar := False;
  end;
*)
  with ConfigSchemas do
  begin
    Enviar := 'ANe.xsd';
    Consultar := '***';

    Validar := False;
  end;
end;

function TACBrANeProviderPortoSeguro.CriarServiceClient(
  const AMetodo: TMetodo): TACBrANeWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
  begin
    URL := URL + FpPath;
    Result := TACBrANeWebservicePortoSeguro.Create(FAOwner, AMetodo, URL, FpMethod);
  end
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrANeProviderPortoSeguro.ProcessarMensagemErros(RootNode: TACBrXmlNode;
  Response: TANeWebserviceResponse; const AListTag, AMessageTag: string);
var
  ANode: TACBrXmlNode;
  AErro: TANeEventoCollectionItem;
  AAlerta: TANeEventoCollectionItem;
  Codigo, Mensagem: string;
begin
  ANode := RootNode;

  if ANode = nil then
    Exit;

  Codigo := ObterConteudoTag(ANode.Childrens.FindAnyNs('Codigo'), tcStr);
  Mensagem := ACBrStr(ObterConteudoTag(ANode.Childrens.FindAnyNs('Resultado'), tcStr));

  if (Codigo = '3') or (Codigo = '7') then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Codigo;
    AErro.Descricao := ACBrStr(Mensagem);
    AErro.Correcao := '';
  end;

  if Codigo = '1' then
  begin
    AAlerta := Response.Alertas.New;
    AAlerta.Codigo := Codigo;
    AAlerta.Descricao := ACBrStr(Mensagem);
    AAlerta.Correcao := '';
  end;
end;

procedure TACBrANeProviderPortoSeguro.PrepararEnviar(Response: TANeEnviarResponse);
var
//  AErro: TANeEventoCollectionItem;
  Documento: TDocumento;
begin
  Documento := TACBrANe(FAOwner).Documentos.Items[0];

  XmlDocumento := Documento.ANe.xmlDFe;

  with ConfigMsgDados do
  begin
    DadosCabecalho := '<tem:Length>' +
                         IntToStr(Length(XmlDocumento)) +
                      '</tem:Length>' +
                      '<tem:FileName>' +
                         Documento.ANe.NomeArq +
                      '</tem:FileName>' +
                      '<tem:CNPJ>' +
                         Documento.ANe.CNPJ +
                      '</tem:CNPJ>';
  end;

  Response.ArquivoEnvio := '<tem:FileByteStream>' +
                              EncodeBase64(XmlDocumento) +
                           '</tem:FileByteStream>';

  FpPath := '';
  FpMethod := 'POST';
end;

procedure TACBrANeProviderPortoSeguro.TratarRetornoEnviar(Response: TANeEnviarResponse);
var
  Document: TACBrXmlDocument;
  AErro: TANeEventoCollectionItem;
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

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      Response.CNPJCliente := ObterConteudoTag(ANode.Childrens.FindAnyNs('CNPJ'), tcStr);
      Response.CTe := ObterConteudoTag(ANode.Childrens.FindAnyNs('CTE'), tcStr);
      Response.DataHora := ObterConteudoTag(ANode.Childrens.FindAnyNs('DataHora'), tcDatHor);
      Response.Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('Protocolo'), tcStr);
      Response.Status := ObterConteudoTag(ANode.Childrens.FindAnyNs('status'), tcStr);
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

procedure TACBrANeProviderPortoSeguro.PrepararConsultar(
  Response: TANeConsultarResponse);
var
  AErro: TANeEventoCollectionItem;
begin
  if EstaVazio(Response.Chave) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod126;
    AErro.Descricao := ACBrStr(Desc126);
    Exit;
  end;

  FpPath := '?out=json&download=1&chave[]=' + Response.Chave;
  Response.ArquivoEnvio := FpPath;
  FpMethod := 'GET';
end;

procedure TACBrANeProviderPortoSeguro.TratarRetornoConsultar(
  Response: TANeConsultarResponse);
begin
  inherited;
(*
<data>
 <item0>
  <chave>12345678901234567890123456789012345678901234</chave>
  <protocolo>1234567890123456789012345678901234567890</protocolo>
 </item0>
</data>
*)
end;

{ TACBrANeWebservicePortoSeguro }

function TACBrANeWebservicePortoSeguro.Enviar(const ACabecalho, AMSG: string): string;
var
  Request: string;
begin
  FPMsgOrig := '<ANe xmlns:tem="http://tempuri.org/">' +
                  XmlDocumento +
               '</ANe>';

  Request := '<tem:RemoteFileInfo>';
  Request := Request + AMSG;
  Request := Request + '</tem:RemoteFileInfo>';

  Result := Executar('http://tempuri.org/IELTAverbaService/FileUpload', Request,
                     ACabecalho, [''], ['xmlns:tem="http://tempuri.org/"']);
end;

function TACBrANeWebservicePortoSeguro.Consultar(const ACabecalho,
  AMSG: string): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

end.
