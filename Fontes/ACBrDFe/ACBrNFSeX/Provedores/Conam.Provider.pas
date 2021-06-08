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

{
     Para que a conexão com o webservice do provedor Conam ocorra é preciso
     configurar a propriedade HttpLib com o valor HttpWinINet
}

{$I ACBr.inc}

unit Conam.Provider;

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
  TACBrNFSeXWebserviceConam = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderConam = class (TACBrNFSeProviderProprio)
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
  Conam.GravarXml, Conam.LerXml;

{ TACBrNFSeProviderConam }

procedure TACBrNFSeProviderConam.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';
    QuebradeLinha := '\\';

    UseCertificateHTTP := False;
    ModoEnvio := meLoteAssincrono;
    {
    TagRaizNFSe := '';  // Verificar
    TagRaizRps  := 'nfe:Reg20Item';
    }
  end;

  SetXmlNameSpace('');

  with ConfigMsgDados do
  begin
    Prefixo := 'nfe';
    PrefixoTS := 'nfe';
  end;

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderConam.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Conam.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderConam.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Conam.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderConam.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar:
          Result := TACBrNFSeXWebserviceConam.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarSituacao:
          Result := TACBrNFSeXWebserviceConam.Create(FAOwner, AMetodo, ConsultarSituacao);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceConam.Create(FAOwner, AMetodo, ConsultarLote);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceConam.Create(FAOwner, AMetodo, CancelarNFSe);
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
          Result := TACBrNFSeXWebserviceConam.Create(FAOwner, AMetodo, Recepcionar);
        tmConsultarSituacao:
          Result := TACBrNFSeXWebserviceConam.Create(FAOwner, AMetodo, ConsultarSituacao);
        tmConsultarLote:
          Result := TACBrNFSeXWebserviceConam.Create(FAOwner, AMetodo, ConsultarLote);
        tmCancelarNFSe:
          Result := TACBrNFSeXWebserviceConam.Create(FAOwner, AMetodo, CancelarNFSe);
      else
        raise EACBrDFeException.Create(ERR_NAO_IMP);
      end;
    end;
  end;
end;

procedure TACBrNFSeProviderConam.ProcessarMensagemErros(
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
    AErro.Codigo := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Id'), tcStr);
    AErro.Descricao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Description'), tcStr);
    AErro.Correcao := '';
  end;
end;

procedure TACBrNFSeProviderConam.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Nota: NotaFiscal;
  IdAttr, ListaRps, xRps, xOptante, xReg90: string;
  I, QtdTributos: Integer;
  vTotServicos, vTotISS, vTotISSRetido, vTotDeducoes, vTotTributos,
  AliquotaSN: Double;
  OptanteSimples: TnfseSimNao;
  ExigibilidadeISS: TnfseExigibilidadeISS;
  DataOptanteSimples, DataInicial, DataFinal: TDateTime;
begin
  if TACBrNFSeX(FAOwner).NotasFiscais.Count <= 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := 'X002';
    AErro.Descricao := 'Nenhum RPS adicionado ao componente.';
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count > Response.MaxRps then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := 'X203';
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

  DataInicial := 0;
  DataFinal := 0;
  DataOptanteSimples := 0;
  OptanteSimples := snSim;
  AliquotaSN := 0;
  ExigibilidadeISS := exiExigivel;
  QtdTributos   := 0;
  vTotServicos  := 0;
  vTotISS       := 0;
  vTotISSRetido := 0;
  vTotDeducoes  := 0;
  vTotTributos  := 0;

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

    if i = 0 then
    begin
      OptanteSimples := Nota.NFSe.OptanteSimplesNacional;
      ExigibilidadeISS := Nota.NFSe.Servico.ExigibilidadeISS;
      DataOptanteSimples := Nota.NFSe.DataOptanteSimplesNacional;
      DataInicial := Nota.NFSe.DataEmissao;
      DataFinal := DataInicial;
      AliquotaSN := Nota.NFSe.Servico.Valores.AliquotaSN;
    end;

    if Nota.NFSe.DataEmissao < DataInicial then
      DataInicial := Nota.NFSe.DataEmissao;

    if Nota.NFSe.DataEmissao > DataFinal then
      DataFinal := Nota.NFSe.DataEmissao;

    if Nota.NFSe.Servico.Valores.AliquotaPis > 0 then
      QtdTributos := QtdTributos + 1;

    if Nota.NFSe.Servico.Valores.AliquotaCofins > 0 then
      QtdTributos := QtdTributos + 1;

    if Nota.NFSe.Servico.Valores.AliquotaCsll > 0 then
      QtdTributos := QtdTributos + 1;

    if Nota.NFSe.Servico.Valores.AliquotaInss > 0 then
      QtdTributos := QtdTributos + 1;

    if Nota.NFSe.Servico.Valores.AliquotaIr > 0 then
      QtdTributos := QtdTributos + 1;

    vTotServicos := vTotServicos + Nota.NFSe.Servico.Valores.ValorServicos;
    vTotDeducoes := vTotDeducoes + Nota.NFSe.Servico.Valores.ValorDeducoes;
    vTotISS      := vTotISS + Nota.NFSe.Servico.Valores.ValorIss;
    vTotISSRetido := vTotISSRetido +  Nota.NFSe.Servico.Valores.ValorIssRetido;
    vTotTributos  := vTotTributos +
             Nota.NFSe.Servico.Valores.ValorIr +
             Nota.NFSe.Servico.Valores.ValorCofins +
             Nota.NFSe.Servico.Valores.ValorPis +
             Nota.NFSe.Servico.Valores.ValorInss +
             Nota.NFSe.Servico.Valores.ValorCsll;

    xRps := RemoverDeclaracaoXML(Nota.XMLOriginal);

    xRps := '<nfe:Reg20Item>' + SeparaDados(xRps, 'nfe:Reg20Item') + '</nfe:Reg20Item>';

    ListaRps := ListaRps + xRps;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if OptanteSimples = snSim then
  begin
    xOptante := '<nfe:TipoTrib>4</nfe:TipoTrib>' +
                '<nfe:DtAdeSN>' +
                   FormatDateTime('dd/mm/yyyy', DataOptanteSimples) +
                '</nfe:DtAdeSN>' +
                '<nfe:AlqIssSN_IP>' +
                   FormatFloat('#,00', AliquotaSN) +
                '</nfe:AlqIssSN_IP>';
  end
  else
  begin
    case ExigibilidadeISS of
      exiExigivel:
        xOptante := '<nfe:TipoTrib>1</nfe:TipoTrib>';

      exiNaoIncidencia,
      exiIsencao,
      exiImunidade:
        xOptante := '<nfe:TipoTrib>2</nfe:TipoTrib>';

      exiSuspensaDecisaoJudicial,
      exiSuspensaProcessoAdministrativo:
        xOptante := '<nfe:TipoTrib>3</nfe:TipoTrib>';

      exiExportacao:
        xOptante := '<nfe:TipoTrib>5</nfe:TipoTrib>';
    end;
  end;

  xReg90 := '<nfe:Reg90>' +
              '<nfe:QtdRegNormal>' +
                 IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
              '</nfe:QtdRegNormal>' +
              '<nfe:ValorNFS>' +
                 FormatFloat('#,00', vTotServicos) +
              '</nfe:ValorNFS>' +
              '<nfe:ValorISS>' +
                 FormatFloat('#,00', vTotISS) +
              '</nfe:ValorISS>' +
              '<nfe:ValorDed>' +
                 FormatFloat('#,00', vTotDeducoes) +
              '</nfe:ValorDed>' +
              '<nfe:ValorIssRetTom>' +
                 FormatFloat('#,00', vTotISSRetido) +
              '</nfe:ValorIssRetTom>' +
              '<nfe:QtdReg30>' +
                 IntToStr(QtdTributos) +
              '</nfe:QtdReg30>' +
              '<nfe:ValorTributos>' +
                 FormatFloat('#,00', vTotTributos) +
              '</nfe:ValorTributos>' +
            '</nfe:Reg90>';

  Response.XmlEnvio := '<nfe:Sdt_processarpsin>' +
                         '<nfe:Login>' +
                           '<nfe:CodigoUsuario>' +
                              Emitente.WSUser +
                           '</nfe:CodigoUsuario>' +
                           '<nfe:CodigoContribuinte>' +
                              Emitente.WSSenha +
                           '</nfe:CodigoContribuinte>' +
                         '</nfe:Login>' +
                         '<nfe:SDTRPS>' +
                           '<nfe:Ano>' +
                              FormatDateTime('yyyy', DataInicial) +
                           '</nfe:Ano>' +
                           '<nfe:Mes>' +
                              FormatDateTime('mm', DataInicial) +
                           '</nfe:Mes>' +
                           '<nfe:CPFCNPJ>' +
                              Emitente.CNPJ +
                           '</nfe:CPFCNPJ>' +
                           '<nfe:DTIni>' +
                              FormatDateTime('dd/mm/yyyy', DataInicial) +
                           '</nfe:DTIni>' +
                           '<nfe:DTFin>' +
                              FormatDateTime('dd/mm/yyyy', DataFinal) +
                           '</nfe:DTFin>' +
                           xOptante +
                           '<nfe:Versao>2.00</nfe:Versao>' +
                           '<nfe:Reg20>' +
                              ListaRps +
                           '</nfe:Reg20>' +
                           xReg90 +
                         '</nfe:SDTRPS>' +
                       '</nfe:Sdt_processarpsin>';
end;

procedure TACBrNFSeProviderConam.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
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
        AErro.Codigo := 'X201';
        AErro.Descricao := 'WebService retornou um XML vazio.';
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response, 'Messages', 'Message');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Protocolo := ProcessarConteudoXml(ANode.Childrens.Find('Protocolo'), tcStr);
        end;
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := 'X999';
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderConam.PrepararConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := 'X101';
    AErro.Descricao := 'Número do Protocolo não informado.';
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.XmlEnvio := '<nfe:Sdt_consultaprotocoloin>' +
                         '<nfe:Protocolo>' +
                            Response.Protocolo +
                         '</nfe:Protocolo>' +
                         '<nfe:Login>' +
                           '<nfe:CodigoUsuario>' +
                              Emitente.WSUser +
                           '</nfe:CodigoUsuario>' +
                           '<nfe:CodigoContribuinte>' +
                              Emitente.WSSenha +
                           '</nfe:CodigoContribuinte>' +
                         '</nfe:Login>' +
                       '</nfe:Sdt_consultaprotocoloin>';
end;

procedure TACBrNFSeProviderConam.TratarRetornoConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse);
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
        AErro.Codigo := 'X201';
        AErro.Descricao := 'WebService retornou um XML vazio.';
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response, 'Messages', 'Message');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Protocolo := ProcessarConteudoXml(ANode.Childrens.Find('PrtCSerRps'), tcStr);
        end;
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := 'X999';
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderConam.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := 'X101';
    AErro.Descricao := 'Número do Protocolo não informado.';
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.XmlEnvio := '<nfe:Sdt_consultanotasprotocoloin>' +
                         '<nfe:Protocolo>' +
                            Response.Protocolo +
                         '</nfe:Protocolo>' +
                         '<nfe:Login>' +
                           '<nfe:CodigoUsuario>' +
                              Emitente.WSUser +
                           '</nfe:CodigoUsuario>' +
                           '<nfe:CodigoContribuinte>' +
                              Emitente.WSSenha +
                           '</nfe:CodigoContribuinte>' +
                         '</nfe:Login>' +
                       '</nfe:Sdt_consultanotasprotocoloin>';
end;

procedure TACBrNFSeProviderConam.TratarRetornoConsultaLoteRps(
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
        AErro.Codigo := 'X201';
        AErro.Descricao := 'WebService retornou um XML vazio.';
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response, 'Messages', 'Message');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.Find('XML_Notas');

      if AuxNode <> nil then
      begin
        ANodeArray := AuxNode.Childrens.FindAllAnyNs('Reg20');
        if not Assigned(ANodeArray) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := 'X203';
          AErro.Descricao := 'Não foi retornado nenhuma NFSe.';
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
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := 'X999';
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderConam.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := 'X108';
    AErro.Descricao := 'Número da NFSe não informado.';
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.SerieNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := 'X112';
    AErro.Descricao := 'Série da NFSe não informada.';
    Exit;
  end;

  if Response.InfCancelamento.NumeroRps = 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := 'X102';
    AErro.Descricao := 'Número do Rps não informado.';
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.SerieRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := 'X103';
    AErro.Descricao := 'Série do Rps não informada.';
    Exit;
  end;

  if Response.InfCancelamento.ValorNFSe = 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := 'X113';
    AErro.Descricao := 'Valor da NFSe não informado.';
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.MotCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := 'X110';
    AErro.Descricao := 'Motivo do Cancelamento não informado.';
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  Response.XmlEnvio := '<nfe:Sdt_cancelanfe>' +
                         '<nfe:Login>' +
                           '<nfe:CodigoUsuario>' +
                              Emitente.WSUser +
                           '</nfe:CodigoUsuario>' +
                           '<nfe:CodigoContribuinte>' +
                              Emitente.WSSenha +
                           '</nfe:CodigoContribuinte>' +
                         '</nfe:Login>' +
                         '<nfe:Nota>' +
                           '<nfe:SerieNota>' +
                              Response.InfCancelamento.SerieNFSe +
                           '</nfe:SerieNota>' +
                           '<nfe:NumeroNota>' +
                              Response.InfCancelamento.NumeroNFSe +
                           '</nfe:NumeroNota>' +
                           '<nfe:SerieRPS>' +
                              Response.InfCancelamento.SerieRps +
                           '</nfe:SerieRPS>' +
                           '<nfe:NumeroRps>' +
                              IntToStr(Response.InfCancelamento.NumeroRps) +
                           '</nfe:NumeroRps>' +
                           '<nfe:ValorNota>' +
                              FormatFloat('#.00', Response.InfCancelamento.ValorNFSe) +
                           '</nfe:ValorNota>' +
                           '<nfe:MotivoCancelamento>' +
                              Response.InfCancelamento.MotCancelamento +
                           '</nfe:MotivoCancelamento>' +
                           '<nfe:PodeCancelarGuia>S</nfe:PodeCancelarGuia>' +
                         '</nfe:Nota>' +
                       '</nfe:Sdt_cancelanfe>';
end;

procedure TACBrNFSeProviderConam.TratarRetornoCancelaNFSe(
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
        AErro.Codigo := 'X201';
        AErro.Descricao := 'WebService retornou um XML vazio.';
        Exit
      end;

      Document.LoadFromXml(Response.XmlRetorno);

      ProcessarMensagemErros(Document.Root, Response, 'Messages', 'Message');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Protocolo := ProcessarConteudoXml(ANode.Childrens.Find('PrtCSerRps'), tcStr);
        end;
      end;
    except
      on E:Exception do
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := 'X999';
        AErro.Descricao := E.Message;
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

{ TACBrNFSeXWebserviceConam }

function TACBrNFSeXWebserviceConam.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:ws_nfe.PROCESSARPS>';
  Request := Request + AMSG;
  Request := Request + '</nfe:ws_nfe.PROCESSARPS>';

  Result := Executar('NFeaction/AWS_NFE.PROCESSARPS', Request,
                     ['Sdt_processarpsout'],
                     ['xmlns:nfe="NFe"']);
end;

function TACBrNFSeXWebserviceConam.ConsultarSituacao(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:ws_nfe.CONSULTAPROTOCOLO>';
  Request := Request + AMSG;
  Request := Request + '</nfe:ws_nfe.CONSULTAPROTOCOLO>';

  Result := Executar('NFeaction/AWS_NFE.CONSULTAPROTOCOLO', Request,
                     ['Sdt_consultaprotocoloout'],
                     ['xmlns:nfe="NFe"']);
end;

function TACBrNFSeXWebserviceConam.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:ws_nfe.CONSULTANOTASPROTOCOLO>';
  Request := Request + AMSG;
  Request := Request + '</nfe:ws_nfe.CONSULTANOTASPROTOCOLO>';

  Result := Executar('NFeaction/AWS_NFE.CONSULTANOTASPROTOCOLO', Request,
                     ['Sdt_consultanotasprotocoloout'],
                     ['xmlns:nfe="NFe"']);
end;

function TACBrNFSeXWebserviceConam.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:ws_nfe.CANCELANOTAELETRONICA>';
  Request := Request + AMSG;
  Request := Request + '</nfe:ws_nfe.CANCELANOTAELETRONICA>';

  Result := Executar('NFeaction/AWS_NFE.CANCELANOTAELETRONICA', Request,
                     ['Sdt_retornocancelanfe'],
                     ['xmlns:nfe="NFe"']);
end;

end.
