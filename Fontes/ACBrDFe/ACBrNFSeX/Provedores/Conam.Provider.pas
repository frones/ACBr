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
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarLinkNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderConam = class (TACBrNFSeProviderProprio)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); override;
    procedure TratarRetornoConsultaSituacao(Response: TNFSeConsultaSituacaoResponse); override;

    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;

    procedure PrepararConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); override;
    procedure TratarRetornoConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = '';
                                     const AMessageTag: string = 'Erro'); override;
  public
    function SituacaoLoteRpsToStr(const t: TSituacaoLoteRps): string; override;
    function StrToSituacaoLoteRps(out ok: boolean; const s: string): TSituacaoLoteRps; override;
    function SituacaoLoteRpsToDescr(const t: TSituacaoLoteRps): string; override;
  end;

implementation

uses
  DateUtils,
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
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
    ConsultaNFSe := False;

    Autenticacao.RequerCertificado := False;
    Autenticacao.RequerLogin := True;

    with ServicosDisponibilizados do
    begin
      EnviarLoteAssincrono := True;
      ConsultarSituacao := True;
      ConsultarLote := True;
      ConsultarRps := True;
      ConsultarLinkNfse := True;
      CancelarNfse := True;
    end;
  end;

  SetXmlNameSpace('');
{
  with ConfigMsgDados do
  begin
    Prefixo := 'nfe';
    PrefixoTS := 'nfe';
  end;
}
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
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceConam.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

procedure TACBrNFSeProviderConam.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  AAlerta: TNFSeEventoCollectionItem;
  xId: string;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    xId := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Id'), tcStr);

    if (xId <> 'OK') and (xId <> 'EXITO') and (xId <> 'Arquivo Aceito') then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := xId;
      AErro.Descricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Description'), tcStr);
      AErro.Correcao := '';
    end;

    if (xId = 'Arquivo Aceito') then
    begin
      AAlerta := Response.Alertas.New;
      AAlerta.Codigo := xId;
      AAlerta.Descricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Description'), tcStr);
      AAlerta.Correcao := '';
    end;
  end;
end;

function TACBrNFSeProviderConam.SituacaoLoteRpsToStr(const t: TSituacaoLoteRps): string;
begin
  Result := EnumeradoToStr(t,
                           ['1', '2', '3', '4', '5'],
                           [sLoteNaoProcessado, sLoteEmProcessamento,
                            sLoteProcessadoErro, sLoteProcessadoAviso,
                            sLoteProcessadoSucesso]);
end;

function TACBrNFSeProviderConam.StrToSituacaoLoteRps(out ok: boolean; const s: string): TSituacaoLoteRps;
begin
  Result := StrToEnumerado(ok, s,
                           ['1', '2', '3', '4', '5'],
                           [sLoteNaoProcessado, sLoteEmProcessamento,
                            sLoteProcessadoErro, sLoteProcessadoAviso,
                            sLoteProcessadoSucesso]);
end;

function TACBrNFSeProviderConam.SituacaoLoteRpsToDescr(const t: TSituacaoLoteRps): string;
begin
  Result := EnumeradoToStr(t,
                           ['Aguardando Processamento', 'Em Processamento',
                            'Lote Processado com Erro',
                            'Lote Processado com Aviso',
                            'Lote Processado com Sucesso'],
                           [sLoteNaoProcessado, sLoteEmProcessamento,
                            sLoteProcessadoErro, sLoteProcessadoAviso,
                            sLoteProcessadoSucesso]);
end;

procedure TACBrNFSeProviderConam.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Nota: TNotaFiscal;
  IdAttr, ListaRps, xRps, xOptante, xReg90, Aliquota: string;
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
    AErro.Codigo := Cod002;
    AErro.Descricao := ACBrStr(Desc002);
  end;

  if TACBrNFSeX(FAOwner).NotasFiscais.Count > Response.MaxRps then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod003;
    AErro.Descricao := ACBrStr('Conjunto de RPS transmitidos (máximo de ' +
                       IntToStr(Response.MaxRps) + ' RPS)' +
                       ' excedido. Quantidade atual: ' +
                       IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count));
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

    Nota.GerarXML;

    Nota.XmlRps := ConverteXMLtoUTF8(Nota.XmlRps);
    Nota.XmlRps := ChangeLineBreak(Nota.XmlRps, '');

    if ConfigAssinar.Rps or ConfigAssinar.RpsGerarNFSe then
    begin
      Nota.XmlRps := FAOwner.SSL.Assinar(Nota.XmlRps,
                                         ConfigMsgDados.XmlRps.DocElemento,
                                         ConfigMsgDados.XmlRps.InfElemento, '', '', '', IdAttr);
    end;

    SalvarXmlRps(Nota);

    if i = 0 then
    begin
      OptanteSimples := Nota.NFSe.OptanteSimplesNacional;
      ExigibilidadeISS := Nota.NFSe.Servico.ExigibilidadeISS;
      DataOptanteSimples := Nota.NFSe.DataOptanteSimplesNacional;
      DataInicial := Nota.NFSe.DataEmissao;
      DataFinal := DataInicial;
      AliquotaSN := Nota.NFSe.Servico.Valores.AliquotaSN;
      Aliquota := FormatFloat('#.00', AliquotaSN);
      Aliquota := StringReplace(Aliquota, '.', ',', [rfReplaceAll]);
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

    xRps := RemoverDeclaracaoXML(Nota.XmlRps);

    xRps := '<Reg20Item>' + SeparaDados(xRps, 'Reg20Item') + '</Reg20Item>';

    ListaRps := ListaRps + xRps;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(Emitente.WSUser) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod119;
    AErro.Descricao := ACBrStr(Desc119);
    Exit;
  end;

  if EstaVazio(Emitente.WSSenha) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod120;
    AErro.Descricao := ACBrStr(Desc120);
    Exit;
  end;

  ListaRps := ChangeLineBreak(ListaRps, '');

  if OptanteSimples = snSim then
  begin
    {
    xOptante := '<nfe:TipoTrib>4</nfe:TipoTrib>' +
                '<nfe:DtAdeSN>' +
                   FormatDateTime('dd/mm/yyyy', DataOptanteSimples) +
                '</nfe:DtAdeSN>' +
                '<nfe:AlqIssSN_IP>' +
                   Aliquota +
                '</nfe:AlqIssSN_IP>';
    }
    xOptante := '<TipoTrib>4</TipoTrib>' +
                '<DtAdeSN>' +
                   FormatDateTime('dd/mm/yyyy', DataOptanteSimples) +
                '</DtAdeSN>' +
                '<AlqIssSN_IP>' +
                   Aliquota +
                '</AlqIssSN_IP>';
  end
  else
  begin
    {
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
    }
    case ExigibilidadeISS of
      exiExigivel:
        xOptante := '<TipoTrib>1</TipoTrib>';

      exiNaoIncidencia,
      exiIsencao,
      exiImunidade:
        xOptante := '<TipoTrib>2</TipoTrib>';

      exiSuspensaDecisaoJudicial,
      exiSuspensaProcessoAdministrativo:
        xOptante := '<TipoTrib>3</TipoTrib>';

      exiExportacao:
        xOptante := '<TipoTrib>5</TipoTrib>';
    end;
  end;
{
  xReg90 := '<nfe:Reg90>' +
              '<nfe:QtdRegNormal>' +
                 IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
              '</nfe:QtdRegNormal>' +
              '<nfe:ValorNFS>' +
                 StringReplace(FormatFloat('#.00', vTotServicos), '.', ',', [rfReplaceAll]) +
              '</nfe:ValorNFS>' +
              '<nfe:ValorISS>' +
                 StringReplace(FormatFloat('#.00', vTotISS), '.', ',', [rfReplaceAll]) +
              '</nfe:ValorISS>' +
              '<nfe:ValorDed>' +
                 StringReplace(FormatFloat('#.00', vTotDeducoes), '.', ',', [rfReplaceAll]) +
              '</nfe:ValorDed>' +
              '<nfe:ValorIssRetTom>' +
                 StringReplace(FormatFloat('#.00', vTotISSRetido), '.', ',', [rfReplaceAll]) +
              '</nfe:ValorIssRetTom>' +
              '<nfe:QtdReg30>' +
                 IntToStr(QtdTributos) +
              '</nfe:QtdReg30>' +
              '<nfe:ValorTributos>' +
                 StringReplace(FormatFloat('#.00', vTotTributos), '.', ',', [rfReplaceAll]) +
              '</nfe:ValorTributos>' +
            '</nfe:Reg90>';
}
  xReg90 := '<Reg90>' +
              '<QtdRegNormal>' +
                 IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
              '</QtdRegNormal>' +
              '<ValorNFS>' +
                 StringReplace(FormatFloat('#.00', vTotServicos), '.', ',', [rfReplaceAll]) +
              '</ValorNFS>' +
              '<ValorISS>' +
                 StringReplace(FormatFloat('#.00', vTotISS), '.', ',', [rfReplaceAll]) +
              '</ValorISS>' +
              '<ValorDed>' +
                 StringReplace(FormatFloat('#.00', vTotDeducoes), '.', ',', [rfReplaceAll]) +
              '</ValorDed>' +
              '<ValorIssRetTom>' +
                 StringReplace(FormatFloat('#.00', vTotISSRetido), '.', ',', [rfReplaceAll]) +
              '</ValorIssRetTom>' +
              '<QtdReg30>' +
                 IntToStr(QtdTributos) +
              '</QtdReg30>' +
              '<ValorTributos>' +
                 StringReplace(FormatFloat('#.00', vTotTributos), '.', ',', [rfReplaceAll]) +
              '</ValorTributos>' +
            '</Reg90>';
{
  Response.ArquivoEnvio := '<nfe:Sdt_processarpsin>' +
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
}
  Response.ArquivoEnvio := '<Sdt_processarpsin>' +
                         '<Login>' +
                           '<CodigoUsuario>' +
                              Emitente.WSUser +
                           '</CodigoUsuario>' +
                           '<CodigoContribuinte>' +
                              Emitente.WSSenha +
                           '</CodigoContribuinte>' +
                         '</Login>' +
                         '<SDTRPS>' +
                           '<Ano>' +
                              FormatDateTime('yyyy', DataInicial) +
                           '</Ano>' +
                           '<Mes>' +
                              FormatDateTime('mm', DataInicial) +
                           '</Mes>' +
                           '<CPFCNPJ>' +
                              Emitente.CNPJ +
                           '</CPFCNPJ>' +
                           '<DTIni>' +
                              FormatDateTime('dd/mm/yyyy', DataInicial) +
                           '</DTIni>' +
                           '<DTFin>' +
                              FormatDateTime('dd/mm/yyyy', DataFinal) +
                           '</DTFin>' +
                           xOptante +
                           '<Versao>2.00</Versao>' +
                           '<Reg20>' +
                              ListaRps +
                           '</Reg20>' +
                           xReg90 +
                         '</SDTRPS>' +
                       '</Sdt_processarpsin>';
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
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ProcessarMensagemErros(Document.Root, Response, 'Messages', 'Message');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response do
        begin
          Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('Protocolo'), tcStr);
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

procedure TACBrNFSeProviderConam.PrepararConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := ACBrStr(Desc101);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(Emitente.WSUser) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod119;
    AErro.Descricao := ACBrStr(Desc119);
    Exit;
  end;

  if EstaVazio(Emitente.WSSenha) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod120;
    AErro.Descricao := ACBrStr(Desc120);
    Exit;
  end;
{
  Response.ArquivoEnvio := '<nfe:Sdt_consultaprotocoloin>' +
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
}
  Response.ArquivoEnvio := '<Sdt_consultaprotocoloin>' +
                         '<Protocolo>' +
                            Response.Protocolo +
                         '</Protocolo>' +
                         '<Login>' +
                           '<CodigoUsuario>' +
                              Emitente.WSUser +
                           '</CodigoUsuario>' +
                           '<CodigoContribuinte>' +
                              Emitente.WSSenha +
                           '</CodigoContribuinte>' +
                         '</Login>' +
                       '</Sdt_consultaprotocoloin>';
end;

procedure TACBrNFSeProviderConam.TratarRetornoConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  Ok: Boolean;
  Situacao: TSituacaoLoteRps;
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

      ProcessarMensagemErros(Document.Root, Response, 'Messages', 'Message');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response do
        begin
          Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('PrtCSerRps'), tcStr);
          Situacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('PrtXSts'), tcStr);
        end;

        Situacao := TACBrNFSeX(FAOwner).Provider.StrToSituacaoLoteRps(Ok, Response.Situacao);
        Response.DescSituacao := TACBrNFSeX(FAOwner).Provider.SituacaoLoteRpsToDescr(Situacao);
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

procedure TACBrNFSeProviderConam.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.Protocolo) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod101;
    AErro.Descricao := ACBrStr(Desc101);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(Emitente.WSUser) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod119;
    AErro.Descricao := ACBrStr(Desc119);
    Exit;
  end;

  if EstaVazio(Emitente.WSSenha) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod120;
    AErro.Descricao := ACBrStr(Desc120);
    Exit;
  end;
 {
  Response.ArquivoEnvio := '<nfe:Sdt_consultanotasprotocoloin>' +
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
}
  Response.ArquivoEnvio := '<Sdt_consultanotasprotocoloin>' +
                         '<Protocolo>' +
                            Response.Protocolo +
                         '</Protocolo>' +
                         '<Login>' +
                           '<CodigoUsuario>' +
                              Emitente.WSUser +
                           '</CodigoUsuario>' +
                           '<CodigoContribuinte>' +
                              Emitente.WSSenha +
                           '</CodigoContribuinte>' +
                         '</Login>' +
                       '</Sdt_consultanotasprotocoloin>';
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
  ANota: TNotaFiscal;
  AResumo: TNFSeResumoCollectionItem;
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

      ProcessarMensagemErros(Document.Root, Response, 'Messages', 'Message');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('XML_Notas');

      if AuxNode <> nil then
      begin
        ANodeArray := AuxNode.Childrens.FindAllAnyNs('Reg20');

        if not Assigned(ANodeArray) then
        begin
          AErro := Response.Erros.New;
          AErro.Codigo := Cod203;
          AErro.Descricao := ACBrStr(Desc203);
          Exit;
        end;

        for i := Low(ANodeArray) to High(ANodeArray) do
        begin
          ANode := ANodeArray[i];
          AuxNode := ANode.Childrens.FindAnyNs('Reg20Item');

          if AuxNode <> nil then
          begin
            AuxNode := AuxNode.Childrens.FindAnyNs('NumRps');

            if AuxNode <> nil then
            begin
              NumRps := AuxNode.AsString;

              ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

              ANota := CarregarXmlNfse(ANota, ANode.OuterXml);

              AResumo := Response.Resumos.New;
              AResumo.NumeroNota := ANota.NFSe.Numero;
              AResumo.Data := ANota.NFSe.DataEmissao;
              AResumo.Link := ANota.NFSe.Link;
              AResumo.CodigoVerificacao := ANota.NFSe.CodigoVerificacao;
              AResumo.Situacao := IntToStr(ANota.NFSe.Situacao);

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
        AErro.Descricao := ACBrStr(Desc999 + E.Message);
      end;
    end;
  finally
    FreeAndNil(Document);
  end;
end;

procedure TACBrNFSeProviderConam.PrepararConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if Response.InfConsultaLinkNFSe.Competencia = 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod131;
    AErro.Descricao := ACBrStr(Desc131);
    Exit;
  end;

  if EstaVazio(Response.InfConsultaLinkNFSe.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  if EstaVazio(Response.InfConsultaLinkNFSe.SerieNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod112;
    AErro.Descricao := ACBrStr(Desc112);
    Exit;
  end;

  if Response.InfConsultaLinkNFSe.NumeroRps = 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := ACBrStr(Desc102);
    Exit;
  end;

  if EstaVazio(Response.InfConsultaLinkNFSe.SerieRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod103;
    AErro.Descricao := ACBrStr(Desc103);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(Emitente.WSUser) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod119;
    AErro.Descricao := ACBrStr(Desc119);
    Exit;
  end;

  if EstaVazio(Emitente.WSSenha) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod120;
    AErro.Descricao := ACBrStr(Desc120);
    Exit;
  end;

  Response.ArquivoEnvio :=
    '<SDT_IMPRESSAO_IN>' +
      '<Login>' +
        '<CodigoUsuario>' +
           Emitente.WSUser +
        '</CodigoUsuario>' +
        '<CodigoContribuinte>' +
           Emitente.WSSenha +
        '</CodigoContribuinte>' +
        '<Versao>2.00</Versao>' +
      '</Login>' +
      '<Nota>' +
        '<Competencia_Mes>' +
          IntToStr(MonthOf(Response.InfConsultaLinkNFSe.Competencia)) +
        '</Competencia_Mes>' +
        '<Competencia_Ano>' +
          IntToStr(YearOf(Response.InfConsultaLinkNFSe.Competencia)) +
        '</Competencia_Ano>' +
        '<RPS_Serie>' +
          Response.InfConsultaLinkNFSe.SerieRps +
        '</RPS_Serie>' +
        '<RPS_Numero>' +
          IntToStr(Response.InfConsultaLinkNFSe.NumeroRps) +
        '</RPS_Numero>' +
        '<Nota_Serie>' +
          Response.InfConsultaLinkNFSe.SerieNFSe +
        '</Nota_Serie>' +
        '<Nota_Numero>' +
          Response.InfConsultaLinkNFSe.NumeroNFSe +
        '</Nota_Numero>' +
      '</Nota>' +
    '</SDT_IMPRESSAO_IN>';
end;

procedure TACBrNFSeProviderConam.TratarRetornoConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
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
      if ANode <> nil then
        Response.Sucesso := ObterConteudoTag(ANode.Childrens.FindAnyNs('Sucesso'), tcBool);

      if not Response.Sucesso then
        ProcessarMensagemErros(Document.Root, Response, 'Messages', 'Message')
      else
      begin
        AuxNode := ANode.Childrens.FindAnyNs('Lista_Notas');
        if AuxNode <> nil then
        begin
          ANodeArray := AuxNode.Childrens.FindAllAnyNs('Nota');
          if not Assigned(ANodeArray) then
          begin
            AErro := Response.Erros.New;
            AErro.Codigo := Cod203;
            AErro.Descricao := ACBrStr(Desc203);
            Exit;
          end;

          ANode := ANodeArray[0];

          Response.Link := ObterConteudoTag(ANode.Childrens.FindAnyNs('LinkImpressao'), tcStr);
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

procedure TACBrNFSeProviderConam.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.SerieNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod112;
    AErro.Descricao := ACBrStr(Desc112);
    Exit;
  end;

  if Response.InfCancelamento.NumeroRps = 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := ACBrStr(Desc102);
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.SerieRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod103;
    AErro.Descricao := ACBrStr(Desc103);
    Exit;
  end;

  if Response.InfCancelamento.ValorNFSe = 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod113;
    AErro.Descricao := ACBrStr(Desc113);
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.MotCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod110;
    AErro.Descricao := ACBrStr(Desc110);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(Emitente.WSUser) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod119;
    AErro.Descricao := ACBrStr(Desc119);
    Exit;
  end;

  if EstaVazio(Emitente.WSSenha) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod120;
    AErro.Descricao := ACBrStr(Desc120);
    Exit;
  end;
{
  Response.ArquivoEnvio := '<nfe:Sdt_cancelanfe>' +
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
}
  Response.ArquivoEnvio := '<Sdt_cancelanfe>' +
                         '<Login>' +
                           '<CodigoUsuario>' +
                              Emitente.WSUser +
                           '</CodigoUsuario>' +
                           '<CodigoContribuinte>' +
                              Emitente.WSSenha +
                           '</CodigoContribuinte>' +
                         '</Login>' +
                         '<Nota>' +
                           '<SerieNota>' +
                              Response.InfCancelamento.SerieNFSe +
                           '</SerieNota>' +
                           '<NumeroNota>' +
                              Response.InfCancelamento.NumeroNFSe +
                           '</NumeroNota>' +
                           '<SerieRPS>' +
                              Response.InfCancelamento.SerieRps +
                           '</SerieRPS>' +
                           '<NumeroRps>' +
                              IntToStr(Response.InfCancelamento.NumeroRps) +
                           '</NumeroRps>' +
                           '<ValorNota>' +
                              StringReplace( FormatFloat('#.00', Response.InfCancelamento.ValorNFSe), ',', '.', [rfReplaceAll] ) +
                           '</ValorNota>' +
                           '<MotivoCancelamento>' +
                              Response.InfCancelamento.MotCancelamento +
                           '</MotivoCancelamento>' +
                           '<PodeCancelarGuia>S</PodeCancelarGuia>' +
                         '</Nota>' +
                       '</Sdt_cancelanfe>';
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
      if Response.ArquivoRetorno = '' then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod201;
        AErro.Descricao := ACBrStr(Desc201);
        Exit
      end;

      Document.LoadFromXml(Response.ArquivoRetorno);

      ProcessarMensagemErros(Document.Root, Response, 'Messages', 'Message');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      if ANode <> nil then
      begin
        with Response do
        begin
          Protocolo := ObterConteudoTag(ANode.Childrens.FindAnyNs('PrtCSerRps'), tcStr);
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

{ TACBrNFSeXWebserviceConam }

function TACBrNFSeXWebserviceConam.Recepcionar(const ACabecalho,
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

function TACBrNFSeXWebserviceConam.ConsultarSituacao(const ACabecalho,
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

function TACBrNFSeXWebserviceConam.ConsultarLote(const ACabecalho,
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

function TACBrNFSeXWebserviceConam.ConsultarLinkNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:ws_nfe.IMPRESSAOLINKNFSE>';
  Request := Request + '<Xml_entrada>' + XmlToStr(AMSG) + '</Xml_entrada>';
  Request := Request + '</nfe:ws_nfe.IMPRESSAOLINKNFSE>';

  Result := Executar('NFeaction/AWS_NFE.IMPRESSAOLINKNFSE', Request,
                     ['Xml_saida', 'SDT_IMPRESSAO_OUT'],
                     ['xmlns:nfe="NFe"']);
end;

function TACBrNFSeXWebserviceConam.Cancelar(const ACabecalho, AMSG: String): string;
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

function TACBrNFSeXWebserviceConam.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverIdentacao(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
  Result := RemoverPrefixosDesnecessarios(Result);
  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);
end;

end.
