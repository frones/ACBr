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

unit eGoverneISS.Provider;

interface

uses
  SysUtils, Classes, Variants,
  ACBrDFeSSL,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXNotasFiscais,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceeGoverneISS = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProvidereGoverneISS = class (TACBrNFSeProviderProprio)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    //metodos para geração e tratamento dos dados do metodo emitir
    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    //metodos para geração e tratamento dos dados do metodo ConsultaLoteRps
    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;

    //metodos para geração e tratamento dos dados do metodo CancelaNFSe
    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(const RootNode: TACBrXmlNode;
                                     const Response: TNFSeWebserviceResponse;
                                     AListTag: string = '';
                                     AMessageTag: string = 'Erro'); override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  eGoverneISS.GravarXml, eGoverneISS.LerXml;

{ TACBrNFSeProvidereGoverneISS }

procedure TACBrNFSeProvidereGoverneISS.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';
    ModoEnvio := meLoteAssincrono;
  end;

  SetXmlNameSpace('');

  ConfigMsgDados.Prefixo := 'eis';

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProvidereGoverneISS.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_eGoverneISS.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProvidereGoverneISS.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_eGoverneISS.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProvidereGoverneISS.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceeGoverneISS.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProvidereGoverneISS.ProcessarMensagemErros(
  const RootNode: TACBrXmlNode; const Response: TNFSeWebserviceResponse;
  AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := '';
    AErro.Descricao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('MensagemErro'), tcStr);
    AErro.Correcao := '';
  end;
end;

procedure TACBrNFSeProvidereGoverneISS.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Nota: NotaFiscal;
  IdAttr, ListaRps, xRps: string;
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
      if ConfigAssinar.Rps or ConfigAssinar.RpsGerarNFSe then
      begin
        Nota.XMLOriginal := FAOwner.SSL.Assinar(ConverteXMLtoUTF8(Nota.XMLOriginal), ConfigMsgDados.XmlRps.DocElemento,
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

    xRps := RemoverDeclaracaoXML(Nota.XMLOriginal);

    xRps := '<eis:NotaFiscal>' +
                 SeparaDados(xRps, 'eis:NotaFiscal') +
              '</eis:NotaFiscal>';

    ListaRps := ListaRps + xRps;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  ListaRps := ChangeLineBreak(ListaRps, '');

  if Response.ModoEnvio = meLoteAssincrono then
  begin
    ListaRps := StringReplace(ListaRps, 'eis:NotaFiscal', 'eis1:NotaFiscalLoteDTO', [rfReplaceAll]);

    Response.XmlEnvio := '<eis:Notas>' +
                           '<eis1:ChaveAutenticacao>' +
                              Emitente.WSChaveAcesso +
                           '</eis1:ChaveAutenticacao>' +
                           '<eis1:EmailContato>' +
                              Emitente.DadosEmitente.Email +
                           '</eis1:EmailContato>' +
                           '<eis1:Notas>' +
                              ListaRps +
                           '</eis1:Notas>' +
                         '</eis:Notas>';
  end
  else
    Response.XmlEnvio := ListaRps;
end;

procedure TACBrNFSeProvidereGoverneISS.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
begin
  Document := TACBrXmlDocument.Create;

  try
    try
      if Response.XmlRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := '201';
        AErro.Descricao := 'WebService retornou um XML vazio.';
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      if Response.ModoEnvio = meLoteAssincrono then
        ProcessarMensagemErros(Document.Root, Response, '', 'EmitirEmLoteResult')
      else
        ProcessarMensagemErros(Document.Root, Response, '', 'EmitirResult');

      Response.Sucesso := (Response.Erros.Count = 0);
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

procedure TACBrNFSeProvidereGoverneISS.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.Lote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := Desc111;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.XmlEnvio := '<eis:ChaveAutenticacao>' +
                          Emitente.WSChaveAcesso +
                       '</eis:ChaveAutenticacao>' +
                       '<eis:CodigoLote>' +
                          Response.Lote +
                       '</eis:CodigoLote>';
end;

procedure TACBrNFSeProvidereGoverneISS.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
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

      ProcessarMensagemErros(Document.Root, Response, '', 'ConsultarLoteResult');

      Response.Sucesso := (Response.Erros.Count = 0);
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

procedure TACBrNFSeProvidereGoverneISS.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Transacao: Boolean;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := Desc108;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  // Se Transacao for True o Ambiente é de Homologação
  Transacao := (TACBrNFSeX(FAOwner).Configuracoes.WebServices.AmbienteCodigo = 2);

  Response.XmlEnvio := '<eis:ChaveAutenticacao>' +
                          Emitente.WSChaveAcesso +
                       '</eis:ChaveAutenticacao>' +
                       '<eis:Homologacao>' +
                         LowerCase(BoolToStr(not Transacao, True)) +
                       '</eis:Homologacao>' +
                       '<eis:NumeroNota>' +
                          Response.InfCancelamento.NumeroNFSe +
                       '</eis:NumeroNota>';
end;

procedure TACBrNFSeProvidereGoverneISS.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
//  ANode: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response, '', 'CancelarResult');

      Response.Sucesso := (Response.Erros.Count = 0);
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

{ TACBrNFSeXWebserviceeGoverneISS }

function TACBrNFSeXWebserviceeGoverneISS.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:EmitirEmLote>';
  Request := Request + '<tem:request>' + AMSG + '</tem:request>';
  Request := Request + '</tem:EmitirEmLote>';

  Result := Executar('http://tempuri.org/INotaFiscalEletronicaServico/EmitirEmLote', Request,
                     [''],
                     ['xmlns:tem="http://tempuri.org/"',
                      'xmlns:eis="http://schemas.datacontract.org/2004/07/Eissnfe.Negocio.WebServices.Mensagem"',
                      'xmlns:eis1="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Prestador"',
                      'xmlns:eis2="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Contribuinte"']);
end;

function TACBrNFSeXWebserviceeGoverneISS.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:Emitir>';
  Request := Request + '<tem:request>' + AMSG + '</tem:request>';
  Request := Request + '</tem:Emitir>';

  Result := Executar('http://tempuri.org/INotaFiscalEletronicaServico/Emitir', Request,
                     [''],
                     ['xmlns:tem="http://tempuri.org/"',
                      'xmlns:eis="http://schemas.datacontract.org/2004/07/Eissnfe.Negocio.WebServices.Mensagem"',
                      'xmlns:eis1="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Prestador"',
                      'xmlns:eis2="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Contribuinte"']);
end;

function TACBrNFSeXWebserviceeGoverneISS.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:ConsultarLote>';
  Request := Request + '<tem:request>' + AMSG + '</tem:request>';
  Request := Request + '</tem:ConsultarLote>';

  Result := Executar('http://tempuri.org/INotaFiscalEletronicaServico/ConsultarLote', Request,
                     [''],
                     ['xmlns:tem="http://tempuri.org/"',
                      'xmlns:eis="http://schemas.datacontract.org/2004/07/Eissnfe.Negocio.WebServices.Mensagem"',
                      'xmlns:eis1="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Prestador"',
                      'xmlns:eis2="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Contribuinte"']);
end;

function TACBrNFSeXWebserviceeGoverneISS.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<tem:Cancelar>';
  Request := Request + '<tem:request>' + AMSG + '</tem:request>';
  Request := Request + '</tem:Cancelar>';

  Result := Executar('http://tempuri.org/INotaFiscalEletronicaServico/Cancelar', Request,
                     [''],
                     ['xmlns:tem="http://tempuri.org/"',
                      'xmlns:eis="http://schemas.datacontract.org/2004/07/Eissnfe.Negocio.WebServices.Mensagem"',
                      'xmlns:eis1="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Prestador"',
                      'xmlns:eis2="http://schemas.datacontract.org/2004/07/Eissnfe.Dominio.DataTransferObject.Contribuinte"']);
end;

end.
