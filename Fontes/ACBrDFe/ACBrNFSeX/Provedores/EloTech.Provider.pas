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

unit EloTech.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceEloTech203 = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderEloTech203 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    function GerarRequerente(const CNPJ: string; const InscMunc: string;
      const Senha: string): string;

    procedure GerarMsgDadosEmitir(Response: TNFSeEmiteResponse;
      Params: TNFSeParamsResponse); override;
    procedure GerarMsgDadosConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse;
      Params: TNFSeParamsResponse); override;
    procedure GerarMsgDadosConsultaporRps(Response: TNFSeConsultaNFSeporRpsResponse;
      Params: TNFSeParamsResponse); override;
    procedure GerarMsgDadosConsultaNFSeporFaixa(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure GerarMsgDadosConsultaNFSeServicoPrestado(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure GerarMsgDadosConsultaNFSeServicoTomado(Response: TNFSeConsultaNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure GerarMsgDadosCancelaNFSe(Response: TNFSeCancelaNFSeResponse;
      Params: TNFSeParamsResponse); override;
    procedure GerarMsgDadosSubstituiNFSe(Response: TNFSeSubstituiNFSeResponse;
      Params: TNFSeParamsResponse); override;

    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
  public
    function TipoDeducaoToStr(const t: TTipoDeducao): string; override;
    function StrToTipoDeducao(out ok: Boolean; const s: string): TTipoDeducao; override;
  end;

implementation

uses
  ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException, ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  ACBrNFSeXNotasFiscais, EloTech.GravarXml, EloTech.LerXml;

{ TACBrNFSeProviderEloTech203 }

procedure TACBrNFSeProviderEloTech203.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    UseCertificateHTTP := False;
    Identificador := '';
    CancPreencherCodVerificacao := True;
    DetalharServico := True;

    Autenticacao.RequerLogin := True;

    ServicosDisponibilizados.EnviarUnitario := False;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.03';
    VersaoAtrib := '2.03';
  end;

  ConfigMsgDados.UsarNumLoteConsLote := True;

  SetXmlNameSpace('http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd');

  SetNomeXSD('nfse_v2_03.xsd');
end;

function TACBrNFSeProviderEloTech203.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_EloTech203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderEloTech203.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_EloTech203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderEloTech203.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceEloTech203.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

function TACBrNFSeProviderEloTech203.GerarRequerente(const CNPJ, InscMunc,
  Senha: string): string;
var
  Homologacao: Boolean;
begin
  Homologacao := (TACBrNFSeX(FAOwner).Configuracoes.WebServices.AmbienteCodigo = 2);

  Result := '<IdentificacaoRequerente>' +
              '<CpfCnpj>' +
                '<Cnpj>' + CNPJ + '</Cnpj>' +
              '</CpfCnpj>' +
              '<InscricaoMunicipal>' + InscMunc + '</InscricaoMunicipal>' +
              '<Senha>' + Senha + '</Senha>' +
              '<Homologa>' +
                 LowerCase(booltostr(Homologacao, True)) +
              '</Homologa>' +
            '</IdentificacaoRequerente>';
end;

procedure TACBrNFSeProviderEloTech203.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  ANode, AuxNode: TACBrXmlNode;
  Ret: TRetCancelamento;
  IdAttr: string;
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

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root.Childrens.FindAnyNs('tcRetCancelamento');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod209;
        AErro.Descricao := ACBrStr(Desc209);
        Exit;
      end;

      ANode := ANode.Childrens.FindAnyNs('NfseCancelamento');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod210;
        AErro.Descricao := ACBrStr(Desc210);
        Exit;
      end;

      ANode := ANode.Childrens.FindAnyNs('ConfirmacaoCancelamento');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod204;
        AErro.Descricao := ACBrStr(Desc204);
        Exit;
      end;

      Ret :=  Response.RetCancelamento;
      Ret.DataHora := ObterConteudoTag(ANode.Childrens.FindAnyNs('DataHora'), tcDatHor);

      if ConfigAssinar.IncluirURI then
        IdAttr := ConfigGeral.Identificador
      else
        IdAttr := 'ID';

      ANode := ANode.Childrens.FindAnyNs('Pedido').Childrens.FindAnyNs('InfPedidoCancelamento');

      Ret.Pedido.CodigoCancelamento := ObterConteudoTag(ANode.Childrens.FindAnyNs('CodigoCancelamento'), tcStr);

      ANode := ANode.Childrens.FindAnyNs('IdentificacaoNfse');

      with  Ret.Pedido.IdentificacaoNfse do
      begin
        Numero := ObterConteudoTag(ANode.Childrens.FindAnyNs('Numero'), tcStr);

        AuxNode := ANode.Childrens.FindAnyNs('CpfCnpj');

        if AuxNode <> nil then
        begin
          Cnpj := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Cnpj'), tcStr);

          if Cnpj = '' then
            Cnpj := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Cpf'), tcStr);
        end
        else
          Cnpj := ObterConteudoTag(ANode.Childrens.FindAnyNs('Cnpj'), tcStr);

        InscricaoMunicipal := ObterConteudoTag(ANode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
        CodigoMunicipio := ObterConteudoTag(ANode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);
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

procedure TACBrNFSeProviderEloTech203.GerarMsgDadosEmitir(
  Response: TNFSeEmiteResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Requerente, Prestador: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    if Response.ModoEnvio in [meLoteAssincrono, meLoteSincrono] then
    begin
      Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);

      Prestador := '<' + Prefixo2 + 'CpfCnpj>' +
                     GetCpfCnpj(Emitente.CNPJ, Prefixo2) +
                   '</' + Prefixo2 + 'CpfCnpj>' +
                   GetInscMunic(Emitente.InscMun, Prefixo2);

      Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                             Requerente +
                             '<' + Prefixo + 'LoteRps' + NameSpace2 + IdAttr  + Versao + '>' +
                               '<' + Prefixo2 + 'NumeroLote>' +
                                  Response.NumeroLote +
                               '</' + Prefixo2 + 'NumeroLote>' +
                                 Prestador +
                               '<' + Prefixo2 + 'QuantidadeRps>' +
                                  IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
                               '</' + Prefixo2 + 'QuantidadeRps>' +
                               '<' + Prefixo2 + 'ListaRps>' +
                                  Xml +
                               '</' + Prefixo2 + 'ListaRps>' +
                             '</' + Prefixo + 'LoteRps>' +
                           '</' + Prefixo + TagEnvio + '>';
    end
    else
      Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                              Xml +
                           '</' + Prefixo + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderEloTech203.GerarMsgDadosConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Requerente: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);

    Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                           Requerente +
                           '<' + Prefixo + 'NumeroLote>' +
                             Response.NumeroLote +
                           '</' + Prefixo + 'NumeroLote>' +
                         '</' + Prefixo + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderEloTech203.GerarMsgDadosConsultaporRps(
  Response: TNFSeConsultaNFSeporRpsResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Requerente: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);

    Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                           '<' + Prefixo + 'IdentificacaoRps>' +
                             '<' + Prefixo2 + 'Numero>' +
                               Response.NumeroRps +
                             '</' + Prefixo2 + 'Numero>' +
                             '<' + Prefixo2 + 'Serie>' +
                               Response.SerieRps +
                             '</' + Prefixo2 + 'Serie>' +
                             '<' + Prefixo2 + 'Tipo>' +
                               Response.TipoRps +
                             '</' + Prefixo2 + 'Tipo>' +
                           '</' + Prefixo + 'IdentificacaoRps>' +
                           Requerente +
                         '</' + Prefixo + TagEnvio + '>';
  end;
end;

procedure TACBrNFSeProviderEloTech203.GerarMsgDadosConsultaNFSeporFaixa(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Requerente: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);

    Response.ArquivoEnvio := '<' + Prefixo + 'ConsultarNfseFaixaEnvio' + NameSpace + '>' +
                           Requerente +
                           Xml +
                           '<' + Prefixo + 'Pagina>' +
                              IntToStr(Response.InfConsultaNFSe.Pagina) +
                           '</' + Prefixo + 'Pagina>' +
                         '</' + Prefixo + 'ConsultarNfseFaixaEnvio>';
  end;
end;

procedure TACBrNFSeProviderEloTech203.GerarMsgDadosConsultaNFSeServicoPrestado(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Requerente: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);

    Response.ArquivoEnvio := '<' + Prefixo + 'ConsultarNfseServicoPrestadoEnvio' + NameSpace + '>' +
                           Requerente +
                           Xml +
                           '<' + Prefixo + 'Pagina>' +
                              IntToStr(Response.InfConsultaNFSe.Pagina) +
                           '</' + Prefixo + 'Pagina>' +
                         '</' + Prefixo + 'ConsultarNfseServicoPrestadoEnvio>';
  end;
end;

procedure TACBrNFSeProviderEloTech203.GerarMsgDadosConsultaNFSeServicoTomado(
  Response: TNFSeConsultaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Requerente: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);

    Response.ArquivoEnvio := '<' + Prefixo + 'ConsultarNfseServicoTomadoEnvio' + NameSpace + '>' +
                           Requerente +
                           Xml +
                           '<' + Prefixo + 'Pagina>' +
                              IntToStr(Response.InfConsultaNFSe.Pagina) +
                           '</' + Prefixo + 'Pagina>' +
                         '</' + Prefixo + 'ConsultarNfseServicoTomadoEnvio>';
  end;
end;

procedure TACBrNFSeProviderEloTech203.GerarMsgDadosCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  InfoCanc: TInfCancelamento;
  Requerente, ChavedeAcesso: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  InfoCanc := Response.InfCancelamento;

  with Params do
  begin
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);

    ChavedeAcesso := '<ChaveAcesso>' +
                        Trim(InfoCanc.ChaveNFSe) +
                     '</ChaveAcesso>';

    Response.ArquivoEnvio := '<' + Prefixo + 'CancelarNfseEnvio' + NameSpace + '>' +
                           Requerente +
                           '<' + Prefixo2 + 'Pedido>' +
                             '<' + Prefixo2 + 'InfPedidoCancelamento' + IdAttr + NameSpace2 + '>' +
                               '<' + Prefixo2 + 'IdentificacaoNfse>' +
                                 '<' + Prefixo2 + 'Numero>' +
                                    InfoCanc.NumeroNFSe +
                                 '</' + Prefixo2 + 'Numero>' +
                                 Serie +
                                 '<' + Prefixo2 + 'CpfCnpj>' +
                                   GetCpfCnpj(Emitente.CNPJ, Prefixo2) +
                                 '</' + Prefixo2 + 'CpfCnpj>' +
                                 GetInscMunic(Emitente.InscMun, Prefixo2) +
                                 '<' + Prefixo2 + 'CodigoMunicipio>' +
                                    IntToStr(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                                 '</' + Prefixo2 + 'CodigoMunicipio>' +
                               '</' + Prefixo2 + 'IdentificacaoNfse>' +
                               ChavedeAcesso +
                               '<' + Prefixo2 + 'CodigoCancelamento>' +
                                  InfoCanc.CodCancelamento +
                               '</' + Prefixo2 + 'CodigoCancelamento>' +
                               Motivo +
                             '</' + Prefixo2 + 'InfPedidoCancelamento>' +
                           '</' + Prefixo2 + 'Pedido>' +
                         '</' + Prefixo + 'CancelarNfseEnvio>';
  end;
end;

procedure TACBrNFSeProviderEloTech203.GerarMsgDadosSubstituiNFSe(
  Response: TNFSeSubstituiNFSeResponse; Params: TNFSeParamsResponse);
var
  Emitente: TEmitenteConfNFSe;
  Requerente: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  with Params do
  begin
    Requerente := GerarRequerente(Emitente.CNPJ, Emitente.InscMun, Emitente.WSSenha);
    Response.PedCanc := RetornarConteudoEntre(Response.PedCanc,
                                                 '<Pedido>', '</Pedido>', True);

    Response.ArquivoEnvio := '<' + Prefixo + TagEnvio + NameSpace + '>' +
                           Requerente +
                           '<' + Prefixo + 'SubstituicaoNfse' + IdAttr + '>' +
                             Response.PedCanc +
                             Xml +
                           '</' + Prefixo + 'SubstituicaoNfse>' +
                         '</' + Prefixo + TagEnvio + '>';
  end;
end;

function TACBrNFSeProviderEloTech203.TipoDeducaoToStr(
  const t: TTipoDeducao): string;
begin
  result := EnumeradoToStr(t,
                           ['M', 'S', 'E'],
                           [tdMateriais, tdSubEmpreitada, tdEquipamento]);
end;

function TACBrNFSeProviderEloTech203.StrToTipoDeducao(out ok: Boolean;
  const s: string): TTipoDeducao;
begin
  result := StrToEnumerado(ok, s,
                           ['M', 'S', 'E'],
                           [tdMateriais, tdSubEmpreitada, tdEquipamento]);
end;

{ TACBrNFSeXWebserviceEloTech203 }

function TACBrNFSeXWebserviceEloTech203.Recepcionar(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech203.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech203.ConsultarLote(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech203.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech203.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech203.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech203.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech203.Cancelar(ACabecalho, AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech203.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
begin
  FPMsgOrig := AMSG;

  Result := Executar('', AMSG, [],
        ['xmlns:nfse="http://shad.elotech.com.br/schemas/iss/nfse_v2_03.xsd"']);
end;

function TACBrNFSeXWebserviceEloTech203.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
  Result := RemoverIdentacao(Result);
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverPrefixosDesnecessarios(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
end;

end.
