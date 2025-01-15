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

unit ISSMap.Provider;

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
  TACBrNFSeXWebserviceISSMap = class(TACBrNFSeXWebserviceNoSoap)
  private
//    function GetDadosUsuario: string;
  public
    function GerarNFSe(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;

//    property DadosUsuario: string read GetDadosUsuario;
  end;

  TACBrNFSeProviderISSMap = class (TACBrNFSeProviderProprio)
  private
    FpCodigoCidade: string;
    FpPath: string;
    FpMethod: string;
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;

    function PrepararRpsParaLote(const aXml: string): string; override;

    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;

    procedure PrepararConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); override;
    procedure TratarRetornoConsultaLinkNFSe(Response: TNFSeConsultaLinkNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    function DescricaoErro(Codigo: Integer): string;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = '';
                                     const AMessageTag: string = 'Erro'); override;

  end;

implementation

uses
  synacrypt,
  synacode,
  ACBrUtil.Base, ACBrUtil.XMLHTML, ACBrUtil.Math, ACBrUtil.Strings,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  ISSMap.GravarXml, ISSMap.LerXml;

{ TACBrNFSeProviderISSMap }

procedure TACBrNFSeProviderISSMap.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    FpCodigoCidade := Params.ValorParametro('CodigoCidade');

    UseCertificateHTTP := False;
    ModoEnvio := meUnitario;

    Autenticacao.RequerCertificado := False;
    Autenticacao.RequerLogin := True;
    Autenticacao.RequerChaveAcesso := True;
    Autenticacao.RequerChaveAutorizacao := True;

    ServicosDisponibilizados.EnviarUnitario := True;
    ServicosDisponibilizados.ConsultarRps := True;
    ServicosDisponibilizados.ConsultarLinkNfse := True;
    ServicosDisponibilizados.CancelarNfse := True;
  end;

  SetXmlNameSpace('');

  ConfigSchemas.Validar := False;
end;

function TACBrNFSeProviderISSMap.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_ISSMap.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSMap.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_ISSMap.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSMap.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
  begin
    URL := URL + FpPath;
    Result := TACBrNFSeXWebserviceISSMap.Create(FAOwner, AMetodo, URL, FpMethod);
  end
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

function TACBrNFSeProviderISSMap.DescricaoErro(Codigo: Integer): string;
begin
  case Codigo of
    100: Result := 'Processo concluído com sucesso';
    200: Result := 'Erro no recebimento dos dados';
    201: Result := 'Código da chave de criptografia (campo Key) Inválida';
    202: Result := 'Erro de criptografia';
    203: Result := 'Cadastro Contribuinte Mobiliário (CCM) não existe';
    204: Result := 'CNPJ/CPF do prestador não é a mesma do CCM dono da chave';
    205: Result := 'Senha Incorreta ou Inválida';
    206: Result := 'RPS Inválida (Não encontrou a RPS)';
    207: Result := 'RPS foi cancelada';
    208: Result := 'RPS possui uma solicitação de cancelamento';
    209: Result := 'O prazo para cancelamento da nota expirou';
    210: Result := 'Bloqueio de Competencia - Notas em aberto para geração de guia de ISSQN';
    212: Result := 'Contribuinte não liberado para emitir de RPS';
    213: Result := 'Contribuinte está bloqueado no sistema IssMap';
    221: Result := 'Erro na validação do campo Número da RPS';
    222: Result := 'Erro na validação do campo Data/Hora de Emissão da Nota';
    223: Result := 'Erro na validação do campo Descrição do Serviço da Nota';
    224: Result := 'Erro na validação do campo Flag de Cancelamento da Nota';
    225: Result := 'Erro na validação do campo Motivo do Cancelamento da Nota';
    226: Result := 'Erro na validação do campo Flag Retido na Fonte';
    227: Result := 'Erro na validação do campo Local de Execução';
    228: Result := 'Erro na validação do campo Código do Serviço Prestado';
    229: Result := 'Erro na validação do campo Serviço Prestado';
    230: Result := 'Erro na validação do campo Valor da Nota';
    231: Result := 'Erro na validação do campo Valor das Deduções';
    232: Result := 'Erro na validação do campo Valor da Base de Cálculo';
    233: Result := 'Erro na validação do campo Alíquota';
    234: Result := 'Erro na validação do campo Valor do ISS';
    235: Result := 'Retenção não permitida';
    236: Result := 'Retenção não permitida para pessoa física';
    237: Result := 'E-mail Tomador Obrigatório em retenções';
    241: Result := 'Erro na validação do campo CPF/CNPJ do Prestador';
    242: Result := 'Erro na validação do campo Nome/Razão do Prestador';
    243: Result := 'Erro na validação do campo Endereço do Prestador';
    244: Result := 'Erro na validação do campo Cidade do Prestador';
    245: Result := 'Erro na validação do campo de Inscrição Municipal do Prestador';
    246: Result := 'Erro na validação do campo Inscrição Estadual/Registro Geral do Prestador';
    247: Result := 'Erro na validação do campo Estado do Prestador';
    248: Result := 'Erro na validação do campo CEP do Prestador';
    249: Result := 'Erro na validação do campo Email do Prestador';
    251: Result := 'Erro na validação do campo CPF/CNPJ do Tomador';
    252: Result := 'Erro na validação do campo Nome/Razão do Tomador';
    253: Result := 'Erro na validação do campo Endereço do Tomador';
    254: Result := 'Erro na validação do campo Cidade do Tomador';
    255: Result := 'Erro na validação do campo Inscrição Municipal do Tomador';
    256: Result := 'Erro na validação do campo Inscrição Estadual/Registro Geral do Tomador';
    257: Result := 'Erro na validação do campo Estado do Tomador';
    258: Result := 'Erro na validação do campo CEP do Tomador';
    259: Result := 'Erro na validação do campo Email do Tomador';
    261: Result := 'Erro na validação da Pass';
    262: Result := 'Erro na validação da Key';
    271: Result := 'Erro de validação na porcentagem do campo porcentagemPIS';
    272: Result := 'Erro de validação na porcentagem do campo porcentagemCOFINS';
    273: Result := 'Erro de validação na porcentagem do campo porcentagemCSLL';
    274: Result := 'Erro de validação na porcentagem do campo porcentagemIRRF';
    275: Result := 'Erro de validação na porcentagem do campo porcentagemINSS';
    276: Result := 'Erro de validação na porcentagem do campo porcentagemOutros';
    281: Result := 'Erro de validação no valor do campo valorPIS';
    282: Result := 'Erro de validação no valor do campo valorCOFINS';
    283: Result := 'Erro de validação no valor do campo valorCSLL';
    284: Result := 'Erro de validação no valor do campo valorIRRF';
    285: Result := 'Erro de validação no valor do campo valorINSS';
    286: Result := 'Erro de validação no valor do campo valorOutros';
    290: Result := 'Erro na validação do serviço';
    294: Result := 'Sua cidade não permite este serviço';
    295: Result := 'Solicitação de Integração Pendente';
    296: Result := 'Solicitação Indeferida';
    297: Result := 'Integração Bloqueada';
    298: Result := 'Emissão de RPS não permitida em Homologação';
    299: Result := 'Sua cidade não possui emissão de RPS';
  else
    Result := 'Código de erro não catalogado';
  end;
end;

procedure TACBrNFSeProviderISSMap.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
//  I: Integer;
  ANode: TACBrXmlNode;
//  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
begin
//  ANode := RootNode.Childrens.FindAnyNs(AListTag);

//  if (ANode = nil) then
    ANode := RootNode;

//  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

//  if not Assigned(ANodeArray) then Exit;

  AErro := Response.Erros.New;
  AErro.Codigo :=  ObterConteudoTag(ANode.Childrens.FindAnyNs('codigo'), tcStr);
  AErro.Descricao := DescricaoErro(StrToIntDef(AErro.Codigo, 0));
  AErro.Correcao := '';
  {
  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    AErro := Response.Erros.New;
    AErro.Codigo :=  ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('codigo'), tcStr);
    AErro.Descricao := '';
    AErro.Correcao := '';
  end;
  }
end;

procedure TACBrNFSeProviderISSMap.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Nota: TNotaFiscal;
  IdAttr, ListaDps: string;
  I: Integer;
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
    AErro.Descricao := ACBrStr('Conjunto de DPS transmitidos (máximo de ' +
                       IntToStr(Response.MaxRps) + ' DPS)' +
                       ' excedido. Quantidade atual: ' +
                       IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count));
  end;

  if Response.Erros.Count > 0 then Exit;

  ListaDps := '';

  if ConfigAssinar.IncluirURI then
    IdAttr := ConfigGeral.Identificador
  else
    IdAttr := 'ID';

  for I := 0 to TACBrNFSeX(FAOwner).NotasFiscais.Count -1 do
  begin
    Nota := TACBrNFSeX(FAOwner).NotasFiscais.Items[I];

    Nota.GerarXML;

    Nota.XmlRps := ConverteXMLtoUTF8(Nota.XmlRps);
    Nota.XmlRps := ChangeLineBreak(Nota.XmlRps, '');

    if (ConfigAssinar.Rps and (Response.ModoEnvio in [meLoteAssincrono, meLoteSincrono])) or
       (ConfigAssinar.RpsGerarNFSe and (Response.ModoEnvio = meUnitario)) then
    begin
      Nota.XmlRps := FAOwner.SSL.Assinar(Nota.XmlRps,
                                         ConfigMsgDados.XmlRps.DocElemento,
                                         ConfigMsgDados.XmlRps.InfElemento, '', '', '', IdAttr);

      Response.ArquivoEnvio := Nota.XmlRps;
    end;

    SalvarXmlRps(Nota);

    ListaDps := ListaDps + Nota.XmlRps;
  end;

  Response.ArquivoEnvio := ListaDps;

  FpMethod := 'POST';

  if ConfigGeral.Ambiente = taProducao then
    FpPath := '/novo/enviar/' + FpCodigoCidade
  else
    FpPath := '/teste/enviar/' + FpCodigoCidade;
end;

function TACBrNFSeProviderISSMap.PrepararRpsParaLote(
  const aXml: string): string;
begin
  Result := '<rps>' + SeparaDados(aXml, 'rps') + '</rps>';
end;

procedure TACBrNFSeProviderISSMap.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
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

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, '', '');

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

procedure TACBrNFSeProviderISSMap.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.NumeroRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := ACBrStr(Desc102);
    Exit;
  end;

  if EstaVazio(Response.CNPJCPFTomador) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod127;
    AErro.Descricao := ACBrStr(Desc127);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

//  Response.Metodo := tmConsultarNFSe;

  FpMethod := 'GET';

  FpPath := '/consulta/' + FpCodigoCidade +'/' + Emitente.CNPJ + '/' +
            Response.CNPJCPFTomador + '/' + Response.NumeroRps;
end;

procedure TACBrNFSeProviderISSMap.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
//  ANodeArray: TACBrXmlNodeArray;
//  i: Integer;
//  NumNFSe: String;
//  ANota: TNotaFiscal;
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

//      ANode := ANode.Childrens.FindAnyNs('Mensagem');

      ProcessarMensagemErros(ANode, Response, '', '');

      Response.Sucesso := (Response.Erros.Count = 0);
      (*
      ANode := ANode.Childrens.FindAnyNs('NFSE');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('NOTA');

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

        NumNFSe := ObterConteudoTag(ANode.Childrens.FindAnyNs('COD'), tcStr);

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

        ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
        SalvarXmlNfse(ANota);
      end;
      *)
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

procedure TACBrNFSeProviderISSMap.PrepararConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
begin
  if EstaVazio(Response.NumeroRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := ACBrStr(Desc102);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

//  Response.Metodo := tmConsultarNFSe;

  FpMethod := 'GET';

  FpPath := '/QRCode/' + FpCodigoCidade +'/' + Emitente.CNPJ + '/' +
            Response.NumeroRps;
end;

procedure TACBrNFSeProviderISSMap.TratarRetornoConsultaLinkNFSe(
  Response: TNFSeConsultaLinkNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
//  ANodeArray: TACBrXmlNodeArray;
//  i: Integer;
//  NumNFSe: String;
//  ANota: TNotaFiscal;
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

//      ANode := ANode.Childrens.FindAnyNs('Mensagem');

      ProcessarMensagemErros(ANode, Response, '', '');

      Response.Sucesso := (Response.Erros.Count = 0);
      (*
      ANode := ANode.Childrens.FindAnyNs('NFSE');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('NOTA');

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

        NumNFSe := ObterConteudoTag(ANode.Childrens.FindAnyNs('COD'), tcStr);

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

        ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
        SalvarXmlNfse(ANota);
      end;
      *)
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

procedure TACBrNFSeProviderISSMap.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  aes: TSynaAes;

function cryptAES(aDado: string): string;
begin
  if aDado <> '' then
    Result := EncodeBase64(aes.EncryptECB(aDado))
  else
    Result := '';
end;

begin
  if Response.InfCancelamento.NumeroRps = 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := ACBrStr(Desc102);
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.CNPJCPFTomador) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod127;
    AErro.Descricao := ACBrStr(Desc127);
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

  Response.ArquivoEnvio := '<cartaCancelamento>' +
                             '<cpf_cnpj_pre>' +
                                  cryptAES(Emitente.CNPJ) +
                             '</cpf_cnpj_pre>' +
                             '<cpf_cnpj_tom>' +
                                cryptAES(Response.InfCancelamento.CNPJCPFTomador) +
                             '</cpf_cnpj_tom>' +
                             '<Key>' +
                                Emitente.WSChaveAcesso +
                             '</Key>' +
                             '<motivoCancelamento>' +
                                cryptAES(Response.InfCancelamento.MotCancelamento) +
                             '</motivoCancelamento>' +
                             '<num_RPS>' +
                                cryptAES(IntToStr(Response.InfCancelamento.NumeroRps)) +
                             '</num_RPS>' +
                             '<pass>' +
                                cryptAES(Emitente.WSSenha) +
                             '</pass>' +
                           '</cartaCancelamento>';

  FpMethod := 'POST';

  FpPath := '/cancela/' + FpCodigoCidade;
end;

procedure TACBrNFSeProviderISSMap.TratarRetornoCancelaNFSe(
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

      ANode := Document.Root;

      ProcessarMensagemErros(ANode, Response, '', '');

      Response.Sucesso := (Response.Erros.Count = 0);
      {
      if ANode <> nil then
        ANode := ANode.Childrens.FindAnyNs('NFSE')
      else
        ANode := Document.Root.Childrens.FindAnyNs('NFSE');

      if ANode <> nil then
      begin
        ProcessarMensagemErros(ANode, Response, '', 'INCONSISTENCIA');

        Response.Sucesso := (Response.Erros.Count = 0);
      end;
      }
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

{ TACBrNFSeXWebserviceISSMap }

{
function TACBrNFSeXWebserviceISSMap.GetDadosUsuario: string;
begin
  with TACBrNFSeX(FPDFeOwner).Configuracoes.Geral do
  begin
    Result := '<nfse:Usuario>' + Emitente.WSUser + '</nfse:Usuario>' +
              '<nfse:Senha>' +
                LowerCase(AsciiToHex(MD5(AnsiString(Emitente.WSSenha)))) +
              '</nfse:Senha>';
  end;
end;
}
function TACBrNFSeXWebserviceISSMap.GerarNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebserviceISSMap.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebserviceISSMap.ConsultarNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebserviceISSMap.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := AMSG;

  Result := Executar('', Request, [], []);
end;

function TACBrNFSeXWebserviceISSMap.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
end;

end.
