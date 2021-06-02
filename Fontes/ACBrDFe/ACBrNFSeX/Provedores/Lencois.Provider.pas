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

unit Lencois.Provider;

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
  TACBrNFSeXWebserviceLencois = class(TACBrNFSeXWebserviceSoap12)
  public
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderLencois = class (TACBrNFSeProviderProprio)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    //metodos para geração e tratamento dos dados do metodo emitir
    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

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
  ACBrNFSeX, ACBrNFSeXConfiguracoes,
  Lencois.GravarXml, Lencois.LerXml;

{ TACBrNFSeProviderLencois }

procedure TACBrNFSeProviderLencois.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';

    UseCertificateHTTP := False;
    ModoEnvio := meUnitario;
    {
    TagRaizNFSe := 'Nota';
    TagRaizRps  := 'Nota';
    }
  end;

  SetXmlNameSpace('NotaFiscal-Geracao.xsd');

  with ConfigMsgDados do
  begin
    with LoteRps do
    begin
      InfElemento := 'InfDeclaracaoPrestacaoServico';
      DocElemento := 'Rps';
    end;

    CancelarNFSe.xmlns := 'NotaFiscal-Cancelamento.xsd';
  end;

  with ConfigSchemas do
  begin
    CancelarNFSe := 'NotaFiscal-Cancelamento.xsd';
    GerarNFSe := 'NotaFiscal-Geracao.xsd';
  end;
end;

function TACBrNFSeProviderLencois.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Lencois.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderLencois.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Lencois.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderLencois.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceLencois.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceLencois.Create(FAOwner, AMetodo, GerarNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end
  else
  begin
    with ConfigWebServices.Producao do
    begin
      case AMetodo of
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceLencois.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceLencois.Create(FAOwner, AMetodo, GerarNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

procedure TACBrNFSeProviderLencois.ProcessarMensagemErros(
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
    AErro.Codigo := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Codigo'), tcStr);
    AErro.Descricao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Descricao'), tcStr);
    AErro.Correcao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('AvisoTecnico'), tcStr);

    if AErro.Descricao = '' then
      AErro.Descricao := ANodeArray[I].AsString;
  end;
end;

procedure TACBrNFSeProviderLencois.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Nota: NotaFiscal;
  IdAttr, ListaRps, xRps: string;
  I: Integer;
begin
  if TACBrNFSeX(FAOwner).NotasFiscais.Count <= 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := '999';
    AErro.Descricao := 'ERRO: Nenhum RPS adicionado ao componente';
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count > Response.MaxRps then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := '999';
    AErro.Descricao := 'ERRO: Conjunto de RPS transmitidos (máximo de ' +
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

    ListaRps := ListaRps + xRps;
  end;

  Response.XmlEnvio := ListaRps;
end;

procedure TACBrNFSeProviderLencois.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
//  ANota: NotaFiscal;
  Xml: string;
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

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, 'Erros', 'Erro');

      Response.Sucesso := (Response.Erros.Count = 0);

      with Response.InfRetorno do
      begin
        Protocolo := ProcessarConteudoXml(ANode.Childrens.Find('validacao'), tcStr);
      end;

      Xml := ProcessarConteudoXml(ANode.Childrens.Find('xml'), tcStr);
      {
      ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps();

        if Assigned(ANota) then
          ANota.XML := ANode.OuterXml
        else
        begin
          TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
        end;

        SalvarXmlNfse(ANota);
      }
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

procedure TACBrNFSeProviderLencois.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := '999';
    AErro.Descricao := 'Numero da NFSe não informada.';
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.MotCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := '999';
    AErro.Descricao := 'Motivo do Canelamento não informado.';
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.CodCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := '999';
    AErro.Descricao := 'Código do Canelamento não informado.';
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.CodVerificacao) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := '999';
    AErro.Descricao := 'Código de Validação não informado.';
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.XmlEnvio := '<Nota xmlns="NotaFiscal-Cancelamento.xsd">' +
                         '<Versao>1.1</Versao>' +
                         '<InscricaoMunicipal>' +
                           Trim(Emitente.InscMun) +
                         '</InscricaoMunicipal>' +
                         '<PASNF>' +
                           '<Numero>' +
                             Response.InfCancelamento.NumeroNFSe +
                           '</Numero>' +
                           '<Data>' +
                             FormatDateTime('yyyy-mm-dd', now) +
                           '</Data>' +
                         '</PASNF>' +
                         '<NotaNumero>' +
                           Response.InfCancelamento.NumeroNFSe +
                         '</NotaNumero>' +
                         '<CodigoValidacao>' +
                           Response.InfCancelamento.CodVerificacao +
                         '</CodigoValidacao>' +
                         '<DescricaoCancelamento>' +
                           Response.InfCancelamento.MotCancelamento +
                         '</DescricaoCancelamento>' +
                         '<CodigoCancelamento>' +
                           Response.InfCancelamento.CodCancelamento +
                         '</CodigoCancelamento>' +
                       '</Nota>';
end;

procedure TACBrNFSeProviderLencois.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode{, AuxNode}: TACBrXmlNode;
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

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, 'Erros', 'Erro');

      Response.Sucesso := (Response.Erros.Count = 0);

      {
      AuxNode := ANode.Childrens.Find('RetornoNota');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.Find('Resultado'), tcStr);
          NumeroNota := ProcessarConteudoXml(AuxNode.Childrens.Find('Nota'), tcInt);
          Link := ProcessarConteudoXml(AuxNode.Childrens.Find('LinkImpressao'), tcStr);
        end;
      end;
     }
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

{ TACBrNFSeXWebserviceLencois }

function TACBrNFSeXWebserviceLencois.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request, DadosUsuario: string;
begin
  FPMsgOrig := AMSG;

  with TACBrNFSeX(FPDFeOwner).Configuracoes.Geral do
  begin
    DadosUsuario := '<apl2:inscricaoMunicipal>' + Emitente.InscMun + '</apl2:inscricaoMunicipal>' +
                    '<apl2:validacao>' + Emitente.WSSenha + '</apl2:validacao>';
  end;

  Request := '<apl2:GerarNotaFiscal>';
  Request := Request + DadosUsuario;
  Request := Request + '<apl2:xml>' + XmlToStr(AMSG) + '</apl2:xml>';
  Request := Request + '</apl2:GerarNotaFiscal>';

  Result := Executar('http://apl2.lencoispaulista.sp.gov.br/GerarNotaFiscal',
                     Request,
                     ['GerarNotaFiscalResult'],
                     ['xmlns:apl2="http://apl2.lencoispaulista.sp.gov.br/"']);
end;

function TACBrNFSeXWebserviceLencois.Cancelar(ACabecalho, AMSG: String): string;
var
  Request, DadosUsuario: string;
begin
  FPMsgOrig := AMSG;

  with TACBrNFSeX(FPDFeOwner).Configuracoes.Geral do
  begin
    DadosUsuario := '<apl2:inscricao>' + Emitente.InscMun + '</apl2:inscricao>' +
                    '<apl2:validacao>' + Emitente.WSSenha + '</apl2:validacao>';
  end;

  Request := '<apl2:CancelarNotaFiscal>';
  Request := Request + DadosUsuario;
  Request := Request + '<apl2:xml>' + XmlToStr(AMSG) + '</apl2:xml>';
  Request := Request + '</apl2:CancelarNotaFiscal>';

  Result := Executar('http://apl2.lencoispaulista.sp.gov.br/CancelarNotaFiscal',
                     Request,
                     ['CancelarNotaFiscalResult'],
                     ['xmlns:apl2="http://apl2.lencoispaulista.sp.gov.br/"']);
end;

end.
