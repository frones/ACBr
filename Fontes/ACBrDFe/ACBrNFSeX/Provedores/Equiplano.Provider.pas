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

unit Equiplano.Provider;

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
  TACBrNFSeXWebserviceEquiplano = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderEquiplano = class (TACBrNFSeProviderProprio)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    //metodos para geração e tratamento dos dados do metodo emitir
    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    //metodos para geração e tratamento dos dados do metodo ConsultaSituacao
    procedure PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); override;
    procedure TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); override;

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

    procedure ProcessarMensagemErros(const RootNode: TACBrXmlNode;
                                     const Response: TNFSeWebserviceResponse;
                                     AListTag: string = '';
                                     AMessageTag: string = 'Erro'); override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  Equiplano.GravarXml, Equiplano.LerXml;

{ TACBrNFSeProviderEquiplano }

procedure TACBrNFSeProviderEquiplano.Configuracao;
begin
  inherited Configuracao;

  ConfigGeral.ModoEnvio := meLoteAssincrono;

  with ConfigAssinar do
  begin
    LoteRps := True;
    ConsultarSituacao := True;
    ConsultarLote := True;
    ConsultarNFSeRps := True;
    ConsultarNFSe := True;
    CancelarNFSe := True;
  end;

  SetXmlNameSpace('http://www.equiplano.com.br/esnfs');

  with ConfigMsgDados do
  begin
    Prefixo := 'es';

    with LoteRps do
    begin
      InfElemento := 'lote';
      DocElemento := 'enviarLoteRpsEnvio';
    end;

    with ConsultarSituacao do
    begin
      InfElemento := 'prestador';
      DocElemento := 'esConsultarSituacaoLoteRpsEnvio';
    end;

    with ConsultarLote do
    begin
      InfElemento := 'prestador';
      DocElemento := 'esConsultarLoteRpsEnvio';
    end;

    with ConsultarNFSeRps do
    begin
      InfElemento := 'rps';
      DocElemento := 'esConsultarNfsePorRpsEnvio';
    end;

    with ConsultarNFSe do
    begin
      InfElemento := 'prestador';
      DocElemento := 'esConsultarNfseEnvio';
    end;

    with CancelarNFSe do
    begin
      InfElemento := 'prestador';
      DocElemento := 'esCancelarNfseEnvio';
    end;

    DadosCabecalho   := '1';
  end;

  with ConfigSchemas do
  begin
    Recepcionar := 'esRecepcionarLoteRpsEnvio_v01.xsd';
    ConsultarSituacao := 'esConsultarSituacaoLoteRpsEnvio_v01.xsd';
    ConsultarLote := 'esConsultarLoteRpsEnvio_v01.xsd';
    ConsultarNFSeRps := 'esConsultarNfsePorRpsEnvio_v01.xsd';
    ConsultarNFSe := 'esConsultarNfseEnvio_v01.xsd';
    CancelarNFSe := 'esCancelarNfseEnvio_v01.xsd';
  end;
end;

function TACBrNFSeProviderEquiplano.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Equiplano.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderEquiplano.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Equiplano.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderEquiplano.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceEquiplano.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderEquiplano.ProcessarMensagemErros(
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
    AErro.Codigo := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('cdMensagem'), tcStr);
    AErro.Descricao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('dsMensagem'), tcStr);
    AErro.Correcao := '';
  end;
end;

procedure TACBrNFSeProviderEquiplano.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Nota: NotaFiscal;
  IdAttr, NameSpace, ListaRps, xRps: string;
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

    xRps := '<rps>' + SeparaDados(xRps, 'rps') + '</rps>';

    ListaRps := ListaRps + xRps;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  ListaRps := ChangeLineBreak(ListaRps, '');

  NameSpace := ' xmlns:es="' + ConfigMsgDados.LoteRps.xmlns + '"';

  Response.XmlEnvio := '<es:enviarLoteRpsEnvio' + NameSpace + '>' +
                         '<lote>' +
                           '<nrLote>' +
                              Response.Lote +
                           '</nrLote>' +
                           '<qtRps>' +
                              IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
                           '</qtRps>' +
                           '<nrVersaoXml>' +
                              '1' +
                           '</nrVersaoXml>' +
                           '<prestador>' +
                             '<nrCnpj>' +
                                OnlyNumber(Emitente.CNPJ) +
                             '</nrCnpj>' +
                             '<nrInscricaoMunicipal>' +
                                OnlyNumber(Emitente.InscMun) +
                             '</nrInscricaoMunicipal>' +
                             '<isOptanteSimplesNacional>' +
                                SimNaoToStr(TACBrNFSeX(FAOwner).NotasFiscais.items[0].NFSe.OptanteSimplesNacional) +
                             '</isOptanteSimplesNacional>' +
                             '<idEntidade>' +
                                TACBrNFSeX(FAOwner).Provider.ConfigGeral.Params1 +
                             '</idEntidade>' +
                           '</prestador>' +
                           '<listaRps>' +
                              ListaRps +
                           '</listaRps>' +
                         '</lote>' +
                       '</es:enviarLoteRpsEnvio>';
end;

procedure TACBrNFSeProviderEquiplano.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  AuxNode: TACBrXmlNode;
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

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('mensagemRetorno');

      if AuxNode <> nil then
      begin
        ProcessarMensagemErros(AuxNode, Response, 'listaErros', 'erro');

        Response.Sucesso := (Response.Erros.Count = 0);

        with Response.InfRetorno do
        begin
          Protocolo := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('protocolo'), tcStr);
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

procedure TACBrNFSeProviderEquiplano.PrepararConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, xConsulta: string;
begin
  if EstaVazio(Response.Lote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := Desc111;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  NameSpace := ' xmlns:es="' + ConfigMsgDados.ConsultarSituacao.xmlns + '"';

  if Response.Protocolo <> '' then
    xConsulta := '<nrProtocolo>' +
                   Response.Protocolo +
                 '</nrProtocolo>'
  else
    xConsulta := '<nrLoteRps>' +
                   Response.Lote +
                 '</nrLoteRps>';

  Response.XmlEnvio := '<es:esConsultarSituacaoLoteRpsEnvio' + NameSpace + '>' +
                         '<prestador>' +
                           '<nrInscricaoMunicipal>' +
                              OnlyNumber(Emitente.InscMun) +
                           '</nrInscricaoMunicipal>' +
                           '<cnpj>' +
                              OnlyNumber(Emitente.CNPJ) +
                           '</cnpj>' +
                           '<idEntidade>' +
                              TACBrNFSeX(FAOwner).Provider.ConfigGeral.Params1 +
                           '</idEntidade>' +
                         '</prestador>' +
                         xConsulta +
                       '</es:esConsultarSituacaoLoteRpsEnvio>';
end;

procedure TACBrNFSeProviderEquiplano.TratarRetornoConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
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

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('mensagemRetorno');

      if AuxNode <> nil then
      begin
        ProcessarMensagemErros(AuxNode, Response, 'listaErros', 'erro');
      end;

      Response.Sucesso := (Response.Erros.Count = 0);

      with Response.InfRetorno do
      begin
        NumeroLote := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('nrLoteRps'), tcStr);
        Situacao := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('stLote'), tcStr);
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

procedure TACBrNFSeProviderEquiplano.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, xConsulta: string;
begin
  if EstaVazio(Response.Lote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := Desc111;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  NameSpace := ' xmlns:es="' + ConfigMsgDados.ConsultarLote.xmlns + '"';

  if Response.Protocolo <> '' then
    xConsulta := '<nrProtocolo>' +
                   Response.Protocolo +
                 '</nrProtocolo>'
  else
    xConsulta := '<nrLoteRps>' +
                   Response.Lote +
                 '</nrLoteRps>';

  Response.XmlEnvio := '<es:esConsultarLoteRpsEnvio' + NameSpace + '>' +
                         '<prestador>' +
                           '<nrInscricaoMunicipal>' +
                              OnlyNumber(Emitente.InscMun) +
                           '</nrInscricaoMunicipal>' +
                           '<cnpj>' +
                              OnlyNumber(Emitente.CNPJ) +
                           '</cnpj>' +
                           '<idEntidade>' +
                              TACBrNFSeX(FAOwner).Provider.ConfigGeral.Params1 +
                           '</idEntidade>' +
                         '</prestador>' +
                         xConsulta +
                       '</es:esConsultarLoteRpsEnvio>';
end;

procedure TACBrNFSeProviderEquiplano.TratarRetornoConsultaLoteRps(
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

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('mensagemRetorno');

      if AuxNode <> nil then
      begin
        ProcessarMensagemErros(AuxNode, Response, 'listaErros', 'erro');
      end;

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := AuxNode.Childrens.FindAnyNs('nrProtocolo');

      if AuxNode <> nil then
        Response.Protocolo := AuxNode.AsString;

      AuxNode := ANode.Childrens.FindAnyNs('listaNfse');

      if AuxNode <> nil then
      begin
        ANodeArray := ANode.Childrens.FindAllAnyNs('nfse');
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
          AuxNode := ANode.Childrens.FindAnyNs('nfse');
          AuxNode := AuxNode.Childrens.FindAnyNs('nrRps');

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

procedure TACBrNFSeProviderEquiplano.PrepararConsultaNFSeporRps(
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

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  NameSpace := ' xmlns:es="' + ConfigMsgDados.ConsultarNFSeRps.xmlns + '"';

  Response.XmlEnvio := '<es:esConsultarNfsePorRpsEnvio' + NameSpace + '>' +
                         '<rps>' +
                           '<nrRps>' +
                              Response.NumRPS +
                           '</nrRps>' +
                           '<nrEmissorRps>' +
                              '1' +
                           '</nrEmissorRps>' +
                         '</rps>' +
                         '<prestador>' +
                           '<nrInscricaoMunicipal>' +
                              OnlyNumber(Emitente.InscMun) +
                           '</nrInscricaoMunicipal>' +
                           '<cnpj>' +
                              OnlyNumber(Emitente.CNPJ) +
                           '</cnpj>' +
                           '<idEntidade>' +
                              TACBrNFSeX(FAOwner).Provider.ConfigGeral.Params1 +
                           '</idEntidade>' +
                         '</prestador>' +
                       '</es:esConsultarNfsePorRpsEnvio>';
end;

procedure TACBrNFSeProviderEquiplano.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
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

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('mensagemRetorno');

      if AuxNode <> nil then
      begin
        ProcessarMensagemErros(AuxNode, Response, 'listaErros', 'erro');
      end;

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('nfse');

      if AuxNode <> nil then
      begin
        Response.CodVerificacao := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('cdAutenticacao'), tcStr);

        with Response.InfRetorno do
        begin
          NumeroNota := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('nrNfse'), tcInt);
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

procedure TACBrNFSeProviderEquiplano.PrepararConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace: string;
begin
  if EstaVazio(Response.InfConsultaNFSe.NumeroIniNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := Desc108;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.Metodo := tmConsultarNFSe;

  NameSpace := ' xmlns:es="' + ConfigMsgDados.ConsultarNFSe.xmlns + '"';

  Response.XmlEnvio := '<es:esConsultarNfseEnvio' + NameSpace + '>' +
                         '<prestador>' +
                           '<nrInscricaoMunicipal>' +
                              OnlyNumber(Emitente.InscMun) +
                           '</nrInscricaoMunicipal>' +
                           '<cnpj>' +
                              OnlyNumber(Emitente.CNPJ) +
                           '</cnpj>' +
                           '<idEntidade>' +
                              TACBrNFSeX(FAOwner).Provider.ConfigGeral.Params1 +
                           '</idEntidade>' +
                         '</prestador>' +
                         '<nrNfse>' +
                            Response.InfConsultaNFSe.NumeroIniNFSe +
                         '</nrNfse>' +
                       '</es:esConsultarNfseEnvio>';
end;

procedure TACBrNFSeProviderEquiplano.TratarRetornoConsultaNFSe(
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

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('mensagemRetorno');

      if AuxNode <> nil then
      begin
        ProcessarMensagemErros(AuxNode, Response, 'listaErros', 'erro');
      end;

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('listaNfse');

      if AuxNode <> nil then
      begin
        ANodeArray := ANode.Childrens.FindAllAnyNs('nfse');
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
          AuxNode := ANode.Childrens.FindAnyNs('nfse');
          AuxNode := AuxNode.Childrens.FindAnyNs('nrRps');

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

procedure TACBrNFSeProviderEquiplano.PrepararCancelaNFSe(
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

  if EstaVazio(Response.InfCancelamento.MotCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod110;
    AErro.Descricao := Desc110;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  NameSpace := ' xmlns:es="' + ConfigMsgDados.ConsultarNFSe.xmlns + '"';

  Response.XmlEnvio := '<es:esCancelarNfseEnvio' + NameSpace + '>' +
                         '<prestador>' +
                           '<nrInscricaoMunicipal>' +
                              OnlyNumber(Emitente.InscMun) +
                           '</nrInscricaoMunicipal>' +
                           '<cnpj>' +
                              OnlyNumber(Emitente.CNPJ) +
                           '</cnpj>' +
                           '<idEntidade>' +
                              TACBrNFSeX(FAOwner).Provider.ConfigGeral.Params1 +
                           '</idEntidade>' +
                         '</prestador>' +
                         '<nrNfse>' +
                            Response.InfCancelamento.NumeroNFSe +
                         '</nrNfse>' +
                         '<dsMotivoCancelamento>' +
                            Response.InfCancelamento.MotCancelamento +
                         '</dsMotivoCancelamento>' +
                       '</es:esCancelarNfseEnvio>';
end;

procedure TACBrNFSeProviderEquiplano.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
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

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('mensagemRetorno');

      if AuxNode <> nil then
      begin
        ProcessarMensagemErros(AuxNode, Response, 'listaErros', 'erro');
      end;

      Response.Sucesso := (Response.Erros.Count = 0);

      if ANode <> nil then
      begin
        with Response.RetCancelamento do
        begin
          Situacao := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('sucesso'), tcStr);
          DataHora := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('dtCancelamento'), tcDatHor);
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

{ TACBrNFSeXWebserviceSP }

function TACBrNFSeXWebserviceEquiplano.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ser:esRecepcionarLoteRps>';
  Request := Request + '<ser:nrVersaoXml>' + ACabecalho + '</ser:nrVersaoXml>';
  Request := Request + '<ser:xml>' + XmlToStr(AMSG) + '</ser:xml>';
  Request := Request + '</ser:esRecepcionarLoteRps>';

  Result := Executar('urn:esRecepcionarLoteRps', Request,
                     ['return', 'esEnviarLoteRpsResposta'],
                     ['xmlns:ser="http://services.enfsws.es"']);
end;

function TACBrNFSeXWebserviceEquiplano.ConsultarSituacao(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ser:esConsultarSituacaoLoteRps>';
  Request := Request + '<ser:nrVersaoXml>' + ACabecalho + '</ser:nrVersaoXml>';
  Request := Request + '<ser:xml>' + XmlToStr(AMSG) + '</ser:xml>';
  Request := Request + '</ser:esConsultarSituacaoLoteRps>';

  Result := Executar('urn:esConsultarSituacaoLoteRps', Request,
                     ['return', 'esConsultarSituacaoLoteRpsResposta'],
                     ['xmlns:ser="http://services.enfsws.es"']);
end;

function TACBrNFSeXWebserviceEquiplano.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ser:esConsultarLoteRps>';
  Request := Request + '<ser:nrVersaoXml>' + ACabecalho + '</ser:nrVersaoXml>';
  Request := Request + '<ser:xml>' + XmlToStr(AMSG) + '</ser:xml>';
  Request := Request + '</ser:esConsultarLoteRps>';

  Result := Executar('urn:esConsultarLoteRps', Request,
                     ['return', 'esConsultarLoteRpsResposta'],
                     ['xmlns:ser="http://services.enfsws.es"']);
end;

function TACBrNFSeXWebserviceEquiplano.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ser:esConsultarNfsePorRps>';
  Request := Request + '<ser:nrVersaoXml>' + ACabecalho + '</ser:nrVersaoXml>';
  Request := Request + '<ser:xml>' + XmlToStr(AMSG) + '</ser:xml>';
  Request := Request + '</ser:esConsultarNfsePorRps>';

  Result := Executar('urn:esConsultarNfsePorRps', Request,
                     ['return', 'esConsultarNfsePorRpsResposta'],
                     ['xmlns:ser="http://services.enfsws.es"']);
end;

function TACBrNFSeXWebserviceEquiplano.ConsultarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ser:esConsultarNfse>';
  Request := Request + '<ser:nrVersaoXml>' + ACabecalho + '</ser:nrVersaoXml>';
  Request := Request + '<ser:xml>' + XmlToStr(AMSG) + '</ser:xml>';
  Request := Request + '</ser:esConsultarNfse>';

  Result := Executar('urn:esConsultarNfse', Request,
                     ['return', 'esConsultarNfseResposta'],
                     ['xmlns:ser="http://services.enfsws.es"']);
end;

function TACBrNFSeXWebserviceEquiplano.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ser:esCancelarNfse>';
  Request := Request + '<ser:nrVersaoXml>' + ACabecalho + '</ser:nrVersaoXml>';
  Request := Request + '<ser:xml>' + XmlToStr(AMSG) + '</ser:xml>';
  Request := Request + '</ser:esCancelarNfse>';

  Result := Executar('urn:esCancelarNfse', Request,
                     ['return', 'esCancelarNfseResposta'],
                     ['xmlns:ser="http://services.enfsws.es"']);
end;

end.
