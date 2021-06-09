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

unit Agili.Provider;

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
  TACBrNFSeXWebserviceAgili = class(TACBrNFSeXWebserviceNoSoap)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderAgili = class (TACBrNFSeProviderProprio)
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

    //metodos para geração e tratamento dos dados do metodo ConsultaNFSeporRps
    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;

    //metodos para geração e tratamento dos dados do metodo ConsultaNFSe
    procedure PrepararConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSe(Response: TNFSeConsultaNFSeResponse); override;

    //metodos para geração e tratamento dos dados do metodo CancelaNFSe
    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    //metodos para geração e tratamento dos dados do metodo SubstituiNFSe
    procedure PrepararSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); override;
    procedure TratarRetornoSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse); override;

    procedure ProcessarMensagemErros(const RootNode: TACBrXmlNode;
                                     const Response: TNFSeWebserviceResponse;
                                     AListTag: string = '';
                                     AMessageTag: string = 'Erro'); override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  Agili.GravarXml, Agili.LerXml;

{ TACBrNFSeProviderAgili }

procedure TACBrNFSeProviderAgili.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';

    UseCertificateHTTP := False;
    ModoEnvio := meLoteAssincrono;
    {
    TagRaizNFSe := 'Nfse';
    TagRaizRps  := 'Rps';
    }
  end;

  SetXmlNameSpace('http://www.agili.com.br/nfse_v_1.00.xsd');

  SetNomeXSD('nfse_v_1.00.xsd');
end;

function TACBrNFSeProviderAgili.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Agili.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAgili.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Agili.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderAgili.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceAgili.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceAgili.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceAgili.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceAgili.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceAgili.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceAgili.Create(FAOwner, AMetodo, GerarNFSe);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceAgili.Create(FAOwner, AMetodo, SubstituirNFSe);
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
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceAgili.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceAgili.Create(FAOwner, AMetodo, ConsultarLote);
        tmConsultarNFSePorRps:
          Result := TACBrNFSeXWebserviceAgili.Create(FAOwner, AMetodo, ConsultarNFSeRps);
        tmConsultarNFSePorFaixa:
          Result := TACBrNFSeXWebserviceAgili.Create(FAOwner, AMetodo, ConsultarNFSePorFaixa);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceAgili.Create(FAOwner, AMetodo, CancelarNFSe);
        tmGerar:
          Result := TACBrNFSeXWebserviceAgili.Create(FAOwner, AMetodo, GerarNFSe);
        tmSubstituirNFSe:
          Result := TACBrNFSeXWebserviceAgili.Create(FAOwner, AMetodo, SubstituirNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

procedure TACBrNFSeProviderAgili.ProcessarMensagemErros(
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
    AErro.Descricao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Mensagem'), tcStr);
    AErro.Correcao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Correcao'), tcStr);
  end;
end;

procedure TACBrNFSeProviderAgili.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Nota: NotaFiscal;
  IdAttr, NameSpace, ListaRps, xRps, TagEnvio, xCabecalho: string;
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

    xRps := '<DeclaracaoPrestacaoServico>' +
                 SeparaDados(xRps, 'InfDeclaracaoPrestacaoServico') +
              '</DeclaracaoPrestacaoServico>';

    ListaRps := ListaRps + xRps;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  case Response.ModoEnvio of
    meUnitario:
    begin
      TagEnvio := 'GerarNfseEnvio';

      xCabecalho := ListaRps;

      if EstaVazio(ConfigMsgDados.GerarNFSe.xmlns) then
        NameSpace := ''
      else
        NameSpace := ' xmlns="' + ConfigMsgDados.GerarNFSe.xmlns + '"';
    end;
  else
    begin
      TagEnvio := 'EnviarLoteRpsEnvio';

      xCabecalho := '<LoteRps>' +
                      '<NumeroLote>' +
                         Response.Lote +
                      '</NumeroLote>' +
                      '<IdentificacaoPrestador>' +
                        '<ChaveDigital>' +
                           Emitente.WSChaveAcesso +
                        '</ChaveDigital>' +
                        '<CpfCnpj>' +
                           GetCpfCnpj(Emitente.Cnpj) +
                        '</CpfCnpj>' +
                        '<InscricaoMunicipal>' +
                           Emitente.InscMun +
                        '</InscricaoMunicipal>' +
                      '</IdentificacaoPrestador>' +
                      '<QuantidadeRps>' +
                         IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
                      '</QuantidadeRps>' +
                      '<ListaRps>' +
                        ListaRps +
                      '</ListaRps>' +
                    '</LoteRps>' +
                    '<Versao>1.00</Versao>';

      if EstaVazio(ConfigMsgDados.LoteRps.xmlns) then
        NameSpace := ''
      else
        NameSpace := ' xmlns="' + ConfigMsgDados.LoteRps.xmlns + '"';
    end;
  end;

  Response.XmlEnvio := '<' + TagEnvio + NameSpace + '>' +
                         '<UnidadeGestora>' +
                            TACBrNFSeX(FAOwner).Configuracoes.Geral.CnpjPrefeitura +
                         '</UnidadeGestora>' +
                         xCabecalho +
                       '</' + TagEnvio + '>';
end;

procedure TACBrNFSeProviderAgili.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno', 'MensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response.InfRetorno do
        begin
          DataRecebimento := ProcessarConteudoXml(ANode.Childrens.Find('DataRecebimento'), tcDatHor);
          Protocolo := ProcessarConteudoXml(ANode.Childrens.Find('Protocolo'), tcStr);
        end;
      end;
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

procedure TACBrNFSeProviderAgili.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace: string;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := Desc101;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(ConfigMsgDados.ConsultarLote.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarLote.xmlns + '"';

  Response.XmlEnvio := '<ConsultarLoteRpsEnvio' + NameSpace + '>' +
                         '<UnidadeGestora>' +
                            TACBrNFSeX(FAOwner).Configuracoes.Geral.CnpjPrefeitura +
                         '</UnidadeGestora>' +
                         '<IdentificacaoPrestador>' +
                           '<ChaveDigital>' +
                              Emitente.WSChaveAcesso +
                           '</ChaveDigital>' +
                           '<CpfCnpj>' +
                              GetCpfCnpj(Emitente.Cnpj) +
                           '</CpfCnpj>' +
                           '<InscricaoMunicipal>' +
                              Emitente.InscMun +
                           '</InscricaoMunicipal>' +
                         '</IdentificacaoPrestador>' +
                         '<Protocolo>' +
                            Response.Protocolo +
                         '</Protocolo>' +
                         '<Versao>1.00</Versao>' +
                       '</ConsultarLoteRpsEnvio>';
end;

procedure TACBrNFSeProviderAgili.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  NumRps: String;
  ANota: NotaFiscal;
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

      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno', 'MensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Situacao := ProcessarConteudoXml(ANode.Childrens.Find('Situacao'), tcStr);

          AuxNode := ANode.Childrens.Find('ListaNfse');

          if AuxNode <> nil then
          begin
            ANodeArray := AuxNode.Childrens.FindAllAnyNs('Nfse');
            if not Assigned(ANodeArray) then
            begin
              AErro := Response.Erros.New;
              AErro.Codigo := Cod203;
              AErro.Descricao := Desc203;
              Exit;
            end;

            for i := Low(ANodeArray) to High(ANodeArray) do
            begin
              ANode := ANodeArray[i];
              AuxNode := ANode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
              AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
              AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
              AuxNode := AuxNode.Childrens.FindAnyNs('Numero');

              if AuxNode <> nil then
              begin
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
            end;
          end;
        end;
      end;
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

procedure TACBrNFSeProviderAgili.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace: string;
begin
  if EstaVazio(Response.NumRPS) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := Desc102;
    Exit;
  end;

  if EstaVazio(Response.Serie) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod103;
    AErro.Descricao := Desc103;
    Exit;
  end;

  if EstaVazio(Response.Tipo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod104;
    AErro.Descricao := Desc104;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(ConfigMsgDados.ConsultarNFSeRps.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarNFSeRps.xmlns + '"';

  Response.XmlEnvio := '<ConsultarNfseRpsEnvio' + NameSpace + '>' +
                         '<UnidadeGestora>' +
                            TACBrNFSeX(FAOwner).Configuracoes.Geral.CnpjPrefeitura +
                         '</UnidadeGestora>' +
                         '<IdentificacaoRps>' +
                           '<Numero>' +
                              Response.NumRPS +
                           '</Numero>' +
                           '<Serie>' +
                              Response.Serie +
                           '</Serie>' +
                           '<Tipo>' +
                              Response.Tipo +
                           '</Tipo>' +
                         '</IdentificacaoRps>' +
                         '<IdentificacaoPrestador>' +
                           '<ChaveDigital>' +
                              Emitente.WSChaveAcesso +
                           '</ChaveDigital>' +
                           '<CpfCnpj>' +
                              GetCpfCnpj(Emitente.Cnpj) +
                           '</CpfCnpj>' +
                           '<InscricaoMunicipal>' +
                              Emitente.InscMun +
                           '</InscricaoMunicipal>' +
                         '</IdentificacaoPrestador>' +
                         '<Versao>1.00</Versao>' +
                       '</ConsultarNfseRpsEnvio>';
end;

procedure TACBrNFSeProviderAgili.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  NumRps: String;
  ANota: NotaFiscal;
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

      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno', 'MensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response.InfRetorno do
        begin
          ANodeArray := ANode.Childrens.FindAllAnyNs('Nfse');
          if not Assigned(ANodeArray) then
          begin
            AErro := Response.Erros.New;
            AErro.Codigo := Cod203;
            AErro.Descricao := Desc203;
            Exit;
          end;

          for i := Low(ANodeArray) to High(ANodeArray) do
          begin
            ANode := ANodeArray[i];
            AuxNode := ANode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
            AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
            AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
            AuxNode := AuxNode.Childrens.FindAnyNs('Numero');

            if AuxNode <> nil then
            begin
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
          end;
        end;
      end;
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

procedure TACBrNFSeProviderAgili.PrepararConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace: string;
begin
  if EstaVazio(Response.InfConsultaNFSe.NumeroIniNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod105;
    AErro.Descricao := Desc105;
    Exit;
  end;

  if EstaVazio(Response.InfConsultaNFSe.NumeroFinNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod106;
    AErro.Descricao := Desc106;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.Metodo := tmConsultarNFSePorFaixa;

  if EstaVazio(ConfigMsgDados.ConsultarNFSe.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarNFSe.xmlns + '"';

  Response.XmlEnvio := '<ConsultarNfseFaixaEnvio' + NameSpace + '>' +
                         '<UnidadeGestora>' +
                            TACBrNFSeX(FAOwner).Configuracoes.Geral.CnpjPrefeitura +
                         '</UnidadeGestora>' +
                         '<IdentificacaoPrestador>' +
                           '<ChaveDigital>' +
                              Emitente.WSChaveAcesso +
                           '</ChaveDigital>' +
                           '<CpfCnpj>' +
                              GetCpfCnpj(Emitente.Cnpj) +
                           '</CpfCnpj>' +
                           '<InscricaoMunicipal>' +
                              Emitente.InscMun +
                           '</InscricaoMunicipal>' +
                         '</IdentificacaoPrestador>' +
                         '<NumeroNfseInicial>' +
                            Response.InfConsultaNFSe.NumeroIniNFSe +
                         '</NumeroNfseInicial>' +
                         '<NumeroNfseFinal>' +
                            Response.InfConsultaNFSe.NumeroFinNFSe +
                         '</NumeroNfseFinal>' +
                         '<Versao>1.00</Versao>' +
                       '</ConsultarNfseFaixaEnvio>';
end;

procedure TACBrNFSeProviderAgili.TratarRetornoConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  NumRps: String;
  ANota: NotaFiscal;
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

      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno', 'MensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response.InfRetorno do
        begin
          AuxNode := ANode.Childrens.Find('ListaNfse');

          if AuxNode <> nil then
          begin
            ANodeArray := AuxNode.Childrens.FindAllAnyNs('Nfse');
            if not Assigned(ANodeArray) then
            begin
              AErro := Response.Erros.New;
              AErro.Codigo := Cod203;
              AErro.Descricao := Desc203;
              Exit;
            end;

            for i := Low(ANodeArray) to High(ANodeArray) do
            begin
              ANode := ANodeArray[i];
              AuxNode := ANode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
              AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
              AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
              AuxNode := AuxNode.Childrens.FindAnyNs('Numero');

              if AuxNode <> nil then
              begin
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
            end;
          end;
        end;
      end;
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

procedure TACBrNFSeProviderAgili.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace: string;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := Desc108;
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.CodCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod109;
    AErro.Descricao := Desc109;
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.MotCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod110;
    AErro.Descricao := Desc110;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(ConfigMsgDados.CancelarNFSe.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.CancelarNFSe.xmlns + '"';

  Response.XmlEnvio := '<CancelarNfseEnvio' + NameSpace + '>' +
                         '<UnidadeGestora>' +
                            TACBrNFSeX(FAOwner).Configuracoes.Geral.CnpjPrefeitura +
                         '</UnidadeGestora>' +
                         '<PedidoCancelamento>' +
                           '<IdentificacaoNfse>' +
                             '<Numero>' +
                                Response.InfCancelamento.NumeroNFSe +
                             '</Numero>' +
                             '<IdentificacaoPrestador>' +
                               '<ChaveDigital>' +
                                  Emitente.WSChaveAcesso +
                               '</ChaveDigital>' +
                               '<CpfCnpj>' +
                                  GetCpfCnpj(Emitente.Cnpj) +
                               '</CpfCnpj>' +
                               '<InscricaoMunicipal>' +
                                  Emitente.InscMun +
                               '</InscricaoMunicipal>' +
                             '</IdentificacaoPrestador>' +
                           '</IdentificacaoNfse>' +
                           '<CodigoCancelamento>' +
                              Response.InfCancelamento.CodCancelamento +
                           '</CodigoCancelamento>' +
                           '<JustificativaCancelamento>' +
                              Response.InfCancelamento.MotCancelamento +
                           '</JustificativaCancelamento>' +
                           '<Versao>1.00</Versao>' +
                         '</PedidoCancelamento>' +
                       '</CancelarNfseEnvio>';
end;

procedure TACBrNFSeProviderAgili.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno', 'MensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response.InfRetorno do
        begin
          DataRecebimento := ProcessarConteudoXml(ANode.Childrens.Find('DataHora'), tcDatHor);
          Protocolo := ProcessarConteudoXml(ANode.Childrens.Find('ProtocoloRequerimentoCancelamento'), tcStr);
        end;
      end;
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

procedure TACBrNFSeProviderAgili.PrepararSubstituiNFSe(
  Response: TNFSeSubstituiNFSeResponse);
var
  IdAttr, xRps, NameSpace: string;
  AErro: TNFSeEventoCollectionItem;
  Nota: NotaFiscal;
begin
  if EstaVazio(Response.PedCanc) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod107;
    AErro.Descricao := Desc107;
    Exit;
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count <= 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod002;
    AErro.Descricao := Desc002;
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count > 1 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod003;
    AErro.Descricao := 'Conjunto de RPS transmitidos (máximo de 1 RPS)' +
                       ' excedido. Quantidade atual: ' +
                       IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count);
  end;

  if Response.Erros.Count > 0 then Exit;

  Nota := TACBrNFSeX(FAOwner).NotasFiscais.Items[0];

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  if EstaVazio(Nota.XMLAssinado) then
  begin
    Nota.GerarXML;
    if ConfigAssinar.RpsSubstituirNFSe then
    begin
      Nota.XMLOriginal := FAOwner.SSL.Assinar(ConverteXMLtoUTF8(Nota.XMLOriginal),
                                              ConfigMsgDados.XmlRps.DocElemento,
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

  xRps := '<DeclaracaoPrestacaoServico>' +
             SeparaDados(xRps, 'InfDeclaracaoPrestacaoServico') +
          '</DeclaracaoPrestacaoServico>';

  {
    No serviço de Substituição de NFS-e temos o pedido de cancelamento de uma
    NFS-e mais o RPS que vai ser convertido na NFS-e substituta.

    A NFS-e substituta substitui a NFS-e Cancelada.

    (Response.PedCanc) contem o pedido de cancelamento da NFS-e existente.
    (xRps) contem o RPS que será convertido na NFS-e substituta.
  }

  if EstaVazio(ConfigMsgDados.SubstituirNFSe.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.SubstituirNFSe.xmlns + '"';

  Response.XmlEnvio := '<SubstituirNfseEnvio' + NameSpace + '>' +
                         Response.PedCanc +
                         xRps +
                       '</SubstituirNfseEnvio>';
end;

procedure TACBrNFSeProviderAgili.TratarRetornoSubstituiNFSe(
  Response: TNFSeSubstituiNFSeResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  ANota: NotaFiscal;
  NumRps: String;
  i: Integer;
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

      ProcessarMensagemErros(Document.Root, Response, 'ListaMensagemRetorno', 'MensagemRetorno');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        AuxNode := ANode.Childrens.Find('RetSubstituicao');

        if AuxNode <> nil then
        begin
          AuxNode := AuxNode.Childrens.Find('NfseSubstituida');

          if AuxNode <> nil then
          begin
            with Response.RetCancelamento do
             DataHora := ProcessarConteudoXml(ANode.Childrens.Find('DataHora'), tcDatHor);
          end;

          AuxNode := AuxNode.Childrens.Find('NfseSubstituidora');

          if AuxNode <> nil then
          begin
            ANodeArray := AuxNode.Childrens.FindAllAnyNs('Nfse');
            if not Assigned(ANodeArray) then
            begin
              AErro := Response.Erros.New;
              AErro.Codigo := Cod203;
              AErro.Descricao := Desc203;
              Exit;
            end;

            for i := Low(ANodeArray) to High(ANodeArray) do
            begin
              ANode := ANodeArray[i];
              AuxNode := ANode.Childrens.FindAnyNs('DeclaracaoPrestacaoServico');
              AuxNode := AuxNode.Childrens.FindAnyNs('Rps');
              AuxNode := AuxNode.Childrens.FindAnyNs('IdentificacaoRps');
              AuxNode := AuxNode.Childrens.FindAnyNs('Numero');

              if AuxNode <> nil then
              begin
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
            end;
          end;
        end;
      end;
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

{ TACBrNFSeXWebserviceAgili }

function TACBrNFSeXWebserviceAgili.Recepcionar(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], ['']);
end;

function TACBrNFSeXWebserviceAgili.GerarNFSe(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], ['']);
end;

function TACBrNFSeXWebserviceAgili.ConsultarLote(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], ['']);
end;

function TACBrNFSeXWebserviceAgili.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], ['']);
end;

function TACBrNFSeXWebserviceAgili.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], ['']);
end;

function TACBrNFSeXWebserviceAgili.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], ['']);
end;

function TACBrNFSeXWebserviceAgili.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [''], ['']);
end;

end.
