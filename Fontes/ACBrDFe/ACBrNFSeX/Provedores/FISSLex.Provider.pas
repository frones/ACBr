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

{ Observação sobre o funcionamento deste provedor
     Funcionou informando httpWinINet para HTTPLib
}

{$I ACBr.inc}

unit FISSLex.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv1, ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceFISSLex = class(TACBrNFSeXWebserviceSoap11)

  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderFISSLex = class (TACBrNFSeProviderABRASFv1)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;

    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
  end;

implementation

uses
  ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  ACBrNFSeXNotasFiscais, FISSLex.GravarXml, FISSLex.LerXml;

{ TACBrNFSeXWebserviceFISSLex }

function TACBrNFSeXWebserviceFISSLex.Recepcionar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<fiss:WS_RecepcionarLoteRps.Execute>';
  Request := Request + '<fiss:Enviarloterpsenvio>' + XmlToStr(AMSG) + '</fiss:Enviarloterpsenvio>';
  Request := Request + '</fiss:WS_RecepcionarLoteRps.Execute>';

  Result := Executar('FISS-LEXaction/AWS_RECEPCIONARLOTERPS.Execute', Request,
                     ['Enviarloterpsresposta', 'EnviarLoteRpsResposta'],
                     ['xmlns:fiss="FISS-LEX"']);
end;

function TACBrNFSeXWebserviceFISSLex.ConsultarLote(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<fiss:WS_ConsultaLoteRps.Execute>';
  Request := Request + '<fiss:Consultarloterpsenvio>' + AMSG + '</fiss:Consultarloterpsenvio>';
  Request := Request + '</fiss:WS_ConsultaLoteRps.Execute>';

  Result := Executar('FISS-LEXaction/AWS_CONSULTALOTERPS.Execute', Request,
                     [], ['xmlns:fiss="FISS-LEX"']);
end;

function TACBrNFSeXWebserviceFISSLex.ConsultarSituacao(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<fiss:WS_ConsultarSituacaoLoteRps.Execute>';
  Request := Request + '<fiss:Consultarsituacaoloterpsenvio>' + AMSG + '</fiss:Consultarsituacaoloterpsenvio>';
  Request := Request + '</fiss:WS_ConsultarSituacaoLoteRps.Execute>';

  Result := Executar('FISS-LEXaction/AWS_CONSULTARSITUACAOLOTERPS.Execute', Request,
                     ['Consultarsituacaoloterpsresposta'],
                     ['xmlns:fiss="FISS-LEX"']);
end;

function TACBrNFSeXWebserviceFISSLex.ConsultarNFSePorRps(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<fiss:WS_ConsultaNfsePorRps.Execute>';
  Request := Request + '<fiss:Consultarnfserpsenvio>' + AMSG + '</fiss:Consultarnfserpsenvio>';
  Request := Request + '</fiss:WS_ConsultaNfsePorRps.Execute>';

  Result := Executar('FISS-LEXaction/AWS_CONSULTANFSEPORRPS.Execute', Request,
                     [], ['xmlns:fiss="FISS-LEX"']);
end;

function TACBrNFSeXWebserviceFISSLex.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<fiss:WS_ConsultaNfse.Execute>';
  Request := Request + '<fiss:Consultarnfseenvio>' + AMSG + '</fiss:Consultarnfseenvio>';
  Request := Request + '</fiss:WS_ConsultaNfse.Execute>';

  Result := Executar('FISS-LEXaction/AWS_CONSULTANFSE.Execute', Request,
                     [], ['xmlns:fiss="FISS-LEX"']);
end;

function TACBrNFSeXWebserviceFISSLex.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<fiss:WS_CancelarNfse.Execute>';
  Request := Request + '<fiss:Cancelarnfseenvio>' + XmlToStr(AMSG) + '</fiss:Cancelarnfseenvio>';
  Request := Request + '</fiss:WS_CancelarNfse.Execute>';

  Result := Executar('FISS-LEXaction/AWS_CANCELARNFSE.Execute', Request,
                     ['Cancelarnfseresposta', 'CancelarNfseResposta'],
                     ['xmlns:fiss="FISS-LEX"']);
end;

function TACBrNFSeXWebserviceFISSLex.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
  Result := RemoverIdentacao(Result);
end;

{ TACBrNFSeProviderFISSLex }

procedure TACBrNFSeProviderFISSLex.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.Identificador := 'id';

  ConfigAssinar.LoteRps := True;
end;

function TACBrNFSeProviderFISSLex.CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_FISSLex.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderFISSLex.CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_FISSLex.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderFISSLex.CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceFISSLex.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderFISSLex.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
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
      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      Response.Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('SituacaoLoteRps'), tcStr);

      ANode := ANode.Childrens.FindAnyNs('Consultarloterpsresposta');

      ANode := ANode.Childrens.FindAnyNs('ListaNfse');
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

procedure TACBrNFSeProviderFISSLex.TratarRetornoConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  ANota: TNotaFiscal;
  NumNFSe: String;
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

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := ANode.Childrens.FindAnyNs('Consultarnfseresposta');

      ANode := ANode.Childrens.FindAnyNs('ListaNfse');
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
          AuxNode := ANode.Childrens.FindAnyNs('Nfse')
        else
          AuxNode := AuxNode.Childrens.FindAnyNs('Nfse');

        if AuxNode = nil then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit;
        end;

        AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
        NumNFSe := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

        ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
        SalvarXmlNfse(ANota);
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

procedure TACBrNFSeProviderFISSLex.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
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
      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := ANode.Childrens.FindAnyNs('Consultarnfserpsresposta');

      ANode := ANode.Childrens.FindAnyNs('CompNfse');
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
        InfNfseID := ObterConteudoTag(AuxNode.Attributes.Items['Id']);
        NumNFSe := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

        with Response do
        begin
          NumeroNota := NumNFSe;
          idNota := InfNfseID;
          CodigoVerificacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('DataEmissao'), tcDatHor);
        end;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

        ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
        SalvarXmlNfse(ANota);
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

procedure TACBrNFSeProviderFISSLex.ValidarSchema(
  Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
var
  xXml: string;
begin
  inherited ValidarSchema(Response, aMetodo);

  xXml := Response.ArquivoEnvio;

  case aMetodo of
    tmConsultarSituacao:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<ConsultarSituacaoLoteRpsEnvio xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd">',
          '</ConsultarSituacaoLoteRpsEnvio>', False);
      end;

    tmConsultarLote:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<ConsultarLoteRpsEnvio xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd">',
          '</ConsultarLoteRpsEnvio>', False);
      end;

    tmConsultarNFSePorRps:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<ConsultarNfseRpsEnvio xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd">',
          '</ConsultarNfseRpsEnvio>', False);
      end;

    tmConsultarNFSe:
      begin
        xXml := RetornarConteudoEntre(xXml,
          '<ConsultarNfseEnvio xmlns="http://www.abrasf.org.br/ABRASF/arquivos/nfse.xsd">',
          '</ConsultarNfseEnvio>', False);
      end;

  else
    Response.ArquivoEnvio := xXml;
  end;

  Response.ArquivoEnvio := xXml;
end;

end.
