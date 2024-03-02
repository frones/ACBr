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

unit DataSmart.Provider;

interface

uses
  SysUtils, Classes,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml, ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceDataSmart202 = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetDadosUsuario: string;

  public
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;

    property DadosUsuario: string read GetDadosUsuario;
  end;

  TACBrNFSeProviderDataSmart202 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure TratarRetornoConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
  end;

implementation

uses
  ACBrUtil.XMLHTML, ACBrUtil.Strings,
  ACBrDFeException,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeX, ACBrNFSeXNotasFiscais, ACBrNFSeXConsts,
  DataSmart.GravarXml, DataSmart.LerXml;

{ TACBrNFSeProviderDataSmart202 }

procedure TACBrNFSeProviderDataSmart202.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;
    ModoEnvio := meUnitario;

    Autenticacao.RequerCertificado := False;
    Autenticacao.RequerLogin := True;

    with ServicosDisponibilizados do
    begin
      EnviarLoteAssincrono := False;
      EnviarLoteSincrono := False;
      ConsultarLote := False;
      ConsultarServicoPrestado := False;
      ConsultarServicoTomado := False;
      SubstituirNfse := False;
    end;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.02';
    VersaoAtrib := '2.02';
  end;

  ConfigMsgDados.DadosCabecalho := GetCabecalho('');
end;

function TACBrNFSeProviderDataSmart202.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_DataSmart202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderDataSmart202.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_DataSmart202.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderDataSmart202.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceDataSmart202.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderDataSmart202.TratarRetornoEmitir(
  Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  NumRps: String;
  ANota: TNotaFiscal;
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

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('gerarNfseResponse');

      if AuxNode <> nil then
      begin
        AuxNode := AuxNode.Childrens.FindAnyNs('GerarNfseResposta');

        ProcessarMensagemErros(AuxNode, Response);
      end
      else
      begin
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
          AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');

          with Response do
          begin
            NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
            CodigoVerificacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
            Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('DataEmissao'), tcDat);
          end;

          AuxNode := AuxNode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
          AuxNode := AuxNode.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');
          AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
          AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
          NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

          ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
          SalvarXmlNfse(ANota);
        end;
      end;

      Response.Sucesso := (Response.Erros.Count = 0);
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

procedure TACBrNFSeProviderDataSmart202.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode, AuxNode2, ANodeNota: TACBrXmlNode;
  AErro: TNFSeEventoCollectionItem;
  ANota: TNotaFiscal;
  NumRps: String;
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

      AuxNode := ANode.Childrens.FindAnyNs('gerarNfseResponse');

      if AuxNode <> nil then
      begin
        AuxNode := AuxNode.Childrens.FindAnyNs('GerarNfseResposta');

        ProcessarMensagemErros(AuxNode, Response);
      end
      else
      begin
        AuxNode := ANode.Childrens.FindAnyNs('CompNfse');

        if not Assigned(AuxNode) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit;
        end;

        ANodeNota := AuxNode;

        AuxNode2 := AuxNode.Childrens.FindAnyNs('NfseCancelamento');

        if AuxNode2 <> nil then
        begin
          AuxNode2 := AuxNode2.Childrens.FindAnyNs('Confirmacao');

          Response.Data := ObterConteudoTag(AuxNode2.Childrens.FindAnyNs('DataHora'), tcDatHor);
          Response.DescSituacao := 'Nota Cancelada';
        end;

        AuxNode := AuxNode.Childrens.FindAnyNs('Nfse');
        AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');

        with Response do
        begin
          NumeroNota := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
          CodigoVerificacao := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('DataEmissao'), tcDat);
        end;

        AuxNode := AuxNode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
        AuxNode := AuxNode.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');
        AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
        AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
        NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

        ANota := CarregarXmlNfse(ANota, ANodeNota.OuterXml);
        SalvarXmlNfse(ANota);
      end;

      Response.Sucesso := (Response.Erros.Count = 0);
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

procedure TACBrNFSeProviderDataSmart202.TratarRetornoConsultaNFSeporFaixa(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  NumRps: String;
  ANota: TNotaFiscal;
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

      Document.LoadFromXml(Response.ArquivoRetorno);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('gerarNfseResponse');

      if AuxNode <> nil then
      begin
        AuxNode := AuxNode.Childrens.FindAnyNs('GerarNfseResposta');

        ProcessarMensagemErros(AuxNode, Response);
      end
      else
      begin
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
          AuxNode := AuxNode.Childrens.FindAnyNs('InfNfse');
          AuxNode := AuxNode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
          AuxNode := AuxNode.Childrens.FindAnyNs('InfDeclaracaoPrestacaoServico');
          AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
          AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
          NumRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

          ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
          SalvarXmlNfse(ANota);
        end;
      end;

      Response.Sucesso := (Response.Erros.Count = 0);
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

procedure TACBrNFSeProviderDataSmart202.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  Ret: TRetCancelamento;
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

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('gerarNfseResponse');

      if AuxNode <> nil then
      begin
        AuxNode := AuxNode.Childrens.FindAnyNs('GerarNfseResposta');

        ProcessarMensagemErros(AuxNode, Response);
      end
      else
      begin
        AuxNode := ANode.Childrens.FindAnyNs('cancelarNfseResponse');

        if not Assigned(AuxNode) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod209;
          AErro.Descricao := ACBrStr(Desc209);
          Exit;
        end;

        AuxNode := AuxNode.Childrens.FindAnyNs('CancelarNfseResposta');
        AuxNode := AuxNode.Childrens.FindAnyNs('RetCancelamento');

        AuxNode := AuxNode.Childrens.FindAnyNs('NfseCancelamento');

        if not Assigned(AuxNode) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod210;
          AErro.Descricao := ACBrStr(Desc210);
          Exit;
        end;

        AuxNode := AuxNode.Childrens.FindAnyNs('Confirmacao');

        if not Assigned(AuxNode) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod204;
          AErro.Descricao := ACBrStr(Desc204);
          Exit;
        end;

        Ret :=  Response.RetCancelamento;
        Ret.DataHora := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('DataHora'), tcDatHor);

        AuxNode := AuxNode.Childrens.FindAnyNs('Pedido');
        AuxNode := AuxNode.Childrens.FindAnyNs('InfPedidoCancelamento');

        Ret.Pedido.CodigoCancelamento := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CodigoCancelamento'), tcStr);

        AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoNfse');

        with  Ret.Pedido.IdentificacaoNfse do
        begin
          Numero := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);

          InscricaoMunicipal := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
          CodigoMunicipio := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);

          AuxNode := AuxNode.Childrens.FindAnyNs('CpfCnpj');

          if AuxNode <> nil then
          begin
            Cnpj := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Cnpj'), tcStr);

            if Cnpj = '' then
              Cnpj := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Cpf'), tcStr);
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

{ TACBrNFSeXWebserviceDataSmart202 }

function TACBrNFSeXWebserviceDataSmart202.GetDadosUsuario: string;
var
  xPref: string;
begin
  if TACBrNFSeX(FPDFeOwner).Configuracoes.WebServices.AmbienteCodigo = 1 then
    xPref := TACBrNFSeX(FPDFeOwner).Provider.ConfigGeral.Params.ValorParametro('AliasCidade')
  else
    xPref := 'BANCO_DEMONSTRACAO';

  with TACBrNFSeX(FPDFeOwner).Configuracoes.Geral do
  begin
    Result := '<Username>' + Emitente.WSUser + '</Username>' +
              '<Password>' + Emitente.WSSenha + '</Password>' +
              '<Prefeitura>' + xPref + '</Prefeitura>';
  end;
end;

function TACBrNFSeXWebserviceDataSmart202.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<dat:GerarNfse><parameters href="#1"/></dat:GerarNfse>';
  Request := Request + '<nfse:GerarNfseEnvio id="1">';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + DadosUsuario;
  Request := Request + '</nfse:GerarNfseEnvio>';

  Result := Executar('http://www.datasmart.com.br/GerarNfse', Request,
                     ['return', 'outputXML'],
                     ['xmlns:dat="http://www.datasmart.com.br/"',
                      'xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceDataSmart202.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<dat:ConsultarNfseFaixa><parameters href="#1"/></dat:ConsultarNfseFaixa>';
  Request := Request + '<nfse:ConsultarNfseFaixaEnvio id="1">';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + DadosUsuario;
  Request := Request + '</nfse:ConsultarNfseFaixaEnvio>';

  Result := Executar('http://www.datasmart.com.br/ConsultarNfseFaixa', Request,
                     ['return', 'outputXML', 'gerarNfseResponse', 'GerarNfseResposta'],
                     ['xmlns:dat="http://www.datasmart.com.br/"',
                      'xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceDataSmart202.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<dat:ConsultarNfsePorRps><parameters href="#1"/></dat:ConsultarNfsePorRps>';
  Request := Request + '<nfse:ConsultarNfsePorRpsEnvio id="1">';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + DadosUsuario;
  Request := Request + '</nfse:ConsultarNfsePorRpsEnvio>';

  Result := Executar('http://www.datasmart.com.br/ConsultarNfsePorRps', Request,
                     ['return', 'outputXML'],
                     ['xmlns:dat="http://www.datasmart.com.br/"',
                      'xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceDataSmart202.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<dat:CancelarNfse><parameters href="#1"/></dat:CancelarNfse>';
  Request := Request + '<nfse:CancelarNfseEnvio id="1">';
  Request := Request + '<nfseCabecMsg>' + XmlToStr(ACabecalho) + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + XmlToStr(AMSG) + '</nfseDadosMsg>';
  Request := Request + DadosUsuario;
  Request := Request + '</nfse:CancelarNfseEnvio>';

  Result := Executar('http://www.datasmart.com.br/CancelarNfse', Request,
                     ['return', 'outputXML'],
                     ['xmlns:dat="http://www.datasmart.com.br/"',
                      'xmlns:nfse="http://www.abrasf.org.br/nfse.xsd"']);
end;

function TACBrNFSeXWebserviceDataSmart202.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
  Result := RemoverDeclaracaoXML(Result);
end;

end.
