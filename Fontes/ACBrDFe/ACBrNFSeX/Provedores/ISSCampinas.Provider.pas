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

unit ISSCampinas.Provider;

interface

uses
  SysUtils, Classes, Variants,
  ACBrDFeSSL,
  ACBrXmlBase, ACBrXmlDocument,
  ACBrNFSeXNotasFiscais,
  ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio, ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type
  TACBrNFSeXWebserviceISSCampinas = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetNameSpace: string;
  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(const ACabecalho, AMSG: String): string; override;
    function TesteEnvio(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;
    function ConsultarSeqRps(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;

    property NameSpace: string read GetNameSpace;
  end;

  TACBrNFSeProviderISSCampinas = class (TACBrNFSeProviderProprio)
  protected
    procedure Configuracao; override;

    procedure AssinaturaAdicional(Nota: TNotaFiscal);

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure PrepararEmitir(Response: TNFSeEmiteResponse); override;
    procedure TratarRetornoEmitir(Response: TNFSeEmiteResponse); override;

    procedure PrepararConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;
    procedure TratarRetornoConsultaLoteRps(Response: TNFSeConsultaLoteRpsResponse); override;

    procedure PrepararConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;
    procedure TratarRetornoConsultaNFSeporRps(Response: TNFSeConsultaNFSeporRpsResponse); override;

    procedure PrepararConsultaNFSeporNumero(Response: TNFSeConsultaNFSeResponse); override;
    procedure TratarRetornoConsultaNFSeporNumero(Response: TNFSeConsultaNFSeResponse); override;

    procedure PrepararCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;
    procedure TratarRetornoCancelaNFSe(Response: TNFSeCancelaNFSeResponse); override;

    procedure PrepararConsultarSeqRps(Response: TNFSeConsultarSeqRpsResponse); override;
    procedure TratarRetornoConsultarSeqRps(Response: TNFSeConsultarSeqRpsResponse); override;

    procedure ProcessarMensagemErros(RootNode: TACBrXmlNode;
                                     Response: TNFSeWebserviceResponse;
                                     const AListTag: string = 'Erros';
                                     const AMessageTag: string = 'Erro'); override;

    function LerChaveNFe(ANode: TACBrXmlNode): string;
    function LerChaveRPS(ANode: TACBrXmlNode): string;
  end;

  TACBrNFSeXWebserviceISSCampinas203 = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(const ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(const ACabecalho, AMSG: String): string; override;
    function GerarNFSe(const ACabecalho, AMSG: String): string; override;
    function ConsultarLote(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(const ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(const ACabecalho, AMSG: String): string; override;
    function Cancelar(const ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(const ACabecalho, AMSG: String): string; override;

    function TratarXmlRetornado(const aXML: string): string; override;
  end;

  TACBrNFSeProviderISSCampinas203 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;
  end;

implementation

uses
  ACBrUtil.Base, ACBrUtil.Strings, ACBrUtil.XMLHTML,
  ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  ISSCampinas.GravarXml, ISSCampinas.LerXml;

{ TACBrNFSeProviderISSCampinas }

procedure TACBrNFSeProviderISSCampinas.AssinaturaAdicional(Nota: TNotaFiscal);
var
  sSituacao, sISSRetido, sCPFCNPJTomador, sIndTomador, sTomador,
  sCPFCNPJInter, sIndInter, sISSRetidoInter, sInter, sAssinatura: String;
begin
  with Nota do
  begin
    sSituacao := EnumeradoToStr(NFSe.SituacaoNfse, ['N', 'C'], [snNormal, snCancelado]);

    sISSRetido := EnumeradoToStr(NFSe.Servico.Valores.IssRetido,
                                 ['N', 'S'], [stNormal, stRetencao]);

    // Tomador do Serviço
    sCPFCNPJTomador := OnlyNumber(NFSe.Tomador.IdentificacaoTomador.CpfCnpj);

    if Length(sCPFCNPJTomador) = 11 then
      sIndTomador := '1'
    else
      if Length(sCPFCNPJTomador) = 14 then
        sIndTomador := '2'
      else
        sIndTomador := '3';

    sTomador := sIndTomador + Poem_Zeros(sCPFCNPJTomador, 14);

    // Prestador Intermediario
    sCPFCNPJInter := OnlyNumber(NFSe.Intermediario.Identificacao.CpfCnpj);

    if Length(sCPFCNPJInter) = 11 then
      sIndInter := '1'
    else
      if Length(sCPFCNPJInter) = 14 then
        sIndInter := '2'
      else
        sIndInter := '3';

    sISSRetidoInter := EnumeradoToStr(NFSe.Intermediario.IssRetido,
                                      ['N', 'S'], [stNormal, stRetencao]);

    if sIndInter <> '3' then
      sInter := sIndInter + Poem_Zeros(sCPFCNPJInter, 14) + sISSRetidoInter
    else
      sInter := '';

    sAssinatura := Poem_Zeros(NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, 11) +
                   PadRight(NFSe.IdentificacaoRps.Serie, 5 , ' ') +
                   Poem_Zeros(NFSe.IdentificacaoRps.Numero, 12) +
                   FormatDateTime('yyyymmdd', NFse.DataEmissao) +
                   PadRight(TipoTributacaoRPSToStr(NFSe.TipoTributacaoRPS), 2, ' ') +
                   sSituacao +
                   sISSRetido +
                   Poem_Zeros(OnlyNumber(FormatFloat('#0.00', NFSe.Servico.Valores.ValorServicos)), 15) +
                   Poem_Zeros(OnlyNumber(FormatFloat('#0.00', NFSe.Servico.Valores.ValorDeducoes)), 15) +
                   Poem_Zeros(OnlyNumber(NFSe.Servico.CodigoCnae), 10) +
                   sTomador;

    NFSe.Assinatura := string(TACBrNFSeX(FAOwner).SSL.CalcHash(AnsiString(sAssinatura), dgstSHA1, outBase64, True));
  end;
end;

procedure TACBrNFSeProviderISSCampinas.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    QuebradeLinha := '<br >';
    ModoEnvio := meLoteSincrono;
    DetalharServico := True;
    CancPreencherMotivo := True;

    ServicosDisponibilizados.EnviarLoteAssincrono := True;
    ServicosDisponibilizados.EnviarLoteSincrono := True;
    ServicosDisponibilizados.ConsultarLote := True;
    ServicosDisponibilizados.ConsultarRps := True;
    ServicosDisponibilizados.ConsultarNfse := True;
    ServicosDisponibilizados.ConsultarSeqRps := True;
    ServicosDisponibilizados.CancelarNfse := True;
    ServicosDisponibilizados.TestarEnvio := True;

    Particularidades.PermiteTagOutrasInformacoes := True;
    Particularidades.PermiteMaisDeUmServico := True;
  end;

  with ConfigAssinar do
  begin
    LoteRps := True;
    ConsultarLote := False;
    ConsultarNFSeRps := True;
    ConsultarNFSe := True;
    CancelarNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '1.00';
    VersaoAtrib := '1.00';
  end;

  SetXmlNameSpace('http://localhost:8080/WsNFe2/lote');

  with ConfigMsgDados do
  begin
    Prefixo := 'ns1';
    PrefixoTS := 'tipos';
    UsarNumLoteConsLote := True;

    XmlRps.xmlns := 'http://localhost:8080/WsNFe2/tp';

    LoteRps.InfElemento := 'Lote';
    LoteRps.DocElemento := 'ReqEnvioLoteRPS';

    LoteRpsSincrono.InfElemento := 'Lote';
    LoteRpsSincrono.DocElemento := 'ReqEnvioLoteRPS';

    ConsultarLote.InfElemento := '';
    ConsultarLote.DocElemento := 'ReqConsultaLote';

    ConsultarNFSeRps.InfElemento := 'Lote';
    ConsultarNFSeRps.DocElemento := 'ReqConsultaNFSeRPS';

    ConsultarNFSe.InfElemento := 'Lote';
    ConsultarNFSe.DocElemento := 'ReqConsultaNotas';

    CancelarNFSe.InfElemento := 'Lote';
    CancelarNFSe.DocElemento := 'ReqCancelamentoNFSe';
  end;

  SetNomeXSD('***');

  with ConfigSchemas do
  begin
    Recepcionar := 'ReqEnvioLoteRPS.xsd';
    ConsultarLote := 'ReqConsultaLote.xsd';
    ConsultarNFSeRps := 'ReqConsultaNFSeRPS.xsd';
    ConsultarNFSe := 'ReqConsultaNotas.xsd';
    CancelarNFSe := 'ReqCancelamentoNFSe.xsd';
    RecepcionarSincrono := 'ReqEnvioLoteRPS.xsd';
    Teste := 'ReqEnvioLoteRPS.xsd';
    ConsultarSeqRps := 'ConsultaSeqRps.xsd';
  end;
end;

function TACBrNFSeProviderISSCampinas.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_ISSCampinas.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSCampinas.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_ISSCampinas.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSCampinas.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceISSCampinas.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

function TACBrNFSeProviderISSCampinas.LerChaveNFe(ANode: TACBrXmlNode): string;
var
  AuxNode: TACBrXmlNode;
begin
  Result := '';
  if ANode = nil then
    Exit;

  AuxNode := ANode.Childrens.FindAnyNs('ChaveNFe');

  if AuxNode <> nil then
    Result := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('NumeroNFe'), tcStr);
end;

function TACBrNFSeProviderISSCampinas.LerChaveRPS(ANode: TACBrXmlNode): string;
var
  AuxNode: TACBrXmlNode;
begin
  Result := '';
  if ANode = nil then
    Exit;

  AuxNode := ANode.Childrens.FindAnyNs('ChaveRPS');

  if AuxNode <> nil then
    Result := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('NumeroRPS'), tcStr);
end;

procedure TACBrNFSeProviderISSCampinas.ProcessarMensagemErros(
  RootNode: TACBrXmlNode; Response: TNFSeWebserviceResponse;
  const AListTag, AMessageTag: string);
var
  I: Integer;
  ANode, ANodeAux: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
  AAlerta: TNFSeEventoCollectionItem;
  Codigo, Descricao, RPS: string;
begin
  ANode := RootNode.Childrens.FindAnyNs(AListTag);

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs(AMessageTag);

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Codigo'), tcStr);
    Descricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Descricao'), tcStr);

    ANodeAux := ANodeArray[I].Childrens.FindAnyNs('ChaveRPS');
    if (ANodeAux <> nil) then
      RPS := 'RPS '+ ObterConteudoTag(ANodeAux.Childrens.FindAnyNs('NumeroRPS'), tcStr)
    else
      RPS := '';

    if (Codigo <> '') or (Descricao <> '') then
    begin
      AErro := Response.Erros.New;
      AErro.Codigo := Codigo;
      AErro.Descricao := Descricao;
      AErro.Correcao := RPS;

      if AErro.Descricao = '' then
        AErro.Descricao := ANodeArray[I].AsString;
    end;
  end;

  ANode := RootNode.Childrens.FindAnyNs('Alertas');

  if (ANode = nil) then
    ANode := RootNode;

  ANodeArray := ANode.Childrens.FindAllAnyNs('Alerta');

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    Codigo := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Codigo'), tcStr);
    Descricao := ObterConteudoTag(ANodeArray[I].Childrens.FindAnyNs('Descricao'), tcStr);

    if (Codigo <> '') or (Descricao <> '') then
    begin
      AAlerta := Response.Alertas.New;
      AAlerta.Codigo := Codigo;
      AAlerta.Descricao := Descricao;
      AAlerta.Correcao := '';

      if AAlerta.Descricao = '' then
        AAlerta.Descricao := ANodeArray[I].AsString;
    end;
  end;
end;

procedure TACBrNFSeProviderISSCampinas.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Nota: TNotaFiscal;
  IdAttr, NameSpace, ListaRps, xRps, Prefixo, PrefixoTS,
  xCabecalho, xDataI, xDataF, xTotServicos, xTotDeducoes: string;
  i, j: Integer;
  DataInicial, DataFinal: TDateTime;
  vTotServicos, vTotDeducoes: Double;
  wAno, wMes, wDia: Word;
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
  vTotServicos := 0;
  vTotDeducoes := 0;

  for i := 0 to TACBrNFSeX(FAOwner).NotasFiscais.Count -1 do
  begin
    Nota := TACBrNFSeX(FAOwner).NotasFiscais.Items[i];

    AssinaturaAdicional(Nota);

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
      DataInicial := Nota.NFSe.DataEmissao;
      DataFinal := DataInicial;
    end;

    if Nota.NFSe.DataEmissao < DataInicial then
      DataInicial := Nota.NFSe.DataEmissao;

    if Nota.NFSe.DataEmissao > DataFinal then
      DataFinal := Nota.NFSe.DataEmissao;

    for j := 0 to Nota.NFSe.Servico.ItemServico.Count -1 do
    begin
      vTotServicos := vTotServicos + Nota.NFSe.Servico.ItemServico.Items[j].ValorTotal;
      vTotDeducoes := vTotDeducoes + Nota.NFSe.Servico.ItemServico.Items[j].ValorDeducoes;
    end;

    xRps := RemoverDeclaracaoXML(Nota.XmlRps);

    ListaRps := ListaRps + xRps;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  Prefixo := '';
  PrefixoTS := '';

  DecodeDate(VarToDateTime(DataInicial), wAno, wMes, wDia);
  xDataI := FormatFloat('0000', wAno) + '-' +
            FormatFloat('00', wMes) + '-' + FormatFloat('00', wDia);

  DecodeDate(VarToDateTime(DataFinal), wAno, wMes, wDia);
  xDataF := FormatFloat('0000', wAno) + '-' +
            FormatFloat('00', wMes) + '-' + FormatFloat('00', wDia);

  {
    Tag <transacao>
    Informe se os RPS a serem substituídos por NFS-e farão parte de uma
    mesma transação.

    True - Os RPS só serão substituídos por NFS-e se não ocorrer nenhum
           evento de erro durante o processamento de todo o lote;

    False - Os RPS válidos serão substituídos por NFS-e, mesmo que ocorram
            eventos de erro durante processamento de outros RPS deste lote.
  }

  xTotServicos := FloatToString(vTotServicos, '.', FloatMask(2, False));
  xTotServicos := StringReplace(xTotServicos, '.00', '', []);
  xTotDeducoes := FloatToString(vTotDeducoes, '.', FloatMask(2, False));
  xTotDeducoes := StringReplace(xTotDeducoes, '.00', '', []);

  ListaRps := ChangeLineBreak(ListaRps, '');

  xCabecalho := '<Cabecalho>' +
                  '<CodCidade>' +
                    CodIBGEToCodTOM(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                  '</CodCidade>' +
                  '<CPFCNPJRemetente>' +
                    OnlyNumber(Emitente.CNPJ) +
                  '</CPFCNPJRemetente>' +
                  '<RazaoSocialRemetente>' +
                    TiraAcentos(Trim(Emitente.RazSocial)) +
                  '</RazaoSocialRemetente>' +
                  '<transacao>' +
                     LowerCase(BoolToStr(TACBrNFSeX(FAOwner).NotasFiscais.Transacao, True)) +
                  '</transacao>' +
                  '<dtInicio>' + xDataI + '</dtInicio>' +
                  '<dtFim>' + xDataF + '</dtFim>' +
                  '<QtdRPS>' +
                     IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
                  '</QtdRPS>' +
                  '<ValorTotalServicos>' +
                     xTotServicos +
                  '</ValorTotalServicos>' +
                  '<ValorTotalDeducoes>' +
                     xTotDeducoes +
                  '</ValorTotalDeducoes>' +
                  '<Versao>1</Versao>' +
                  '<MetodoEnvio>WS</MetodoEnvio>' +
                '</Cabecalho>';

  if EstaVazio(ConfigMsgDados.LoteRps.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.LoteRps.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' +
                               ConfigMsgDados.LoteRps.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.LoteRps.xmlns then
    begin
      if ConfigMsgDados.PrefixoTS = '' then
        NameSpace := NameSpace + ' xmlns="' + ConfigMsgDados.XmlRps.xmlns + '"'
      else
      begin
        NameSpace := NameSpace + ' xmlns:' + ConfigMsgDados.PrefixoTS + '="' +
                                             ConfigMsgDados.XmlRps.xmlns + '"';
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
      end;
    end
    else
    begin
      if ConfigMsgDados.PrefixoTS <> '' then
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
    end;
  end;

  IdAttr := ' ' + ConfigGeral.Identificador + '="Lote_' + Response.NumeroLote + '"';

  NameSpace := NameSpace +
    ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
    ' xsi:schemaLocation="http://localhost:8080/WsNFe2/lote' +
    ' http://localhost:8080/WsNFe2/xsd/ReqEnvioLoteRPS.xsd"';

  Response.ArquivoEnvio := '<' + Prefixo + 'ReqEnvioLoteRPS' + NameSpace + '>' +
                              xCabecalho +
                              '<Lote' + IdAttr + '>' + ListaRps + '</Lote>' +
                           '</' + Prefixo + 'ReqEnvioLoteRPS>';
end;

procedure TACBrNFSeProviderISSCampinas.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNodeChave, AuxNode: TACBrXmlNode;
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

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0) and (Response.Alertas.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response do
        begin
          Sucesso := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Sucesso'), tcStr) = 'true';
          Protocolo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('NumeroLote'), tcStr);

          { Verificar se mais alguma dessas informações são necessárias
          with InformacoesLote do
          begin
            InscricaoPrestador := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('InscricaoPrestador'), tcStr);
            CPFCNPJRemetente := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CPFCNPJRemetente'), tcStr);
            DataEnvioLote := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('DataEnvioLote'), tcDatHor);
            QtdNotasProcessadas := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('QtdNotasProcessadas'), tcInt);
            TempoProcessamento := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('TempoProcessamento'), tcInt);
            ValorTotalServico := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('ValorTotalServico'), tcDe2);
          end;
          }
          ProcessarMensagemErros(AuxNode, Response);

          Response.Sucesso := (Response.Erros.Count = 0) and (Response.Alertas.Count = 0);
        end;
      end;

      AuxNode := ANode.Childrens.FindAnyNs('ChavesNFSeRPS');

      if AuxNode <> nil then
      begin
        AuxNode := AuxNode.Childrens.FindAnyNs('ChaveNFSeRPS');

        if AuxNode <> nil then
        begin
          AuxNodeChave := AuxNode.Childrens.FindAnyNs('ChaveRPS');

          if (AuxNodeChave <> nil) then
          begin
            with Response do
            begin
              SerieRPS := ObterConteudoTag(AuxNodeChave.Childrens.FindAnyNs('SerieRPS'), tcStr);
              NumeroRPS := ObterConteudoTag(AuxNodeChave.Childrens.FindAnyNs('NumeroRPS'), tcStr);
            end;
          end;

          AuxNodeChave := AuxNode.Childrens.FindAnyNs('ChaveNFe');

          if (AuxNodeChave <> nil) then
          begin
            with Response do
            begin
              NumeroNota := ObterConteudoTag(AuxNodeChave.Childrens.FindAnyNs('NumeroNFe'), tcStr);
              CodigoVerificacao := ObterConteudoTag(AuxNodeChave.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
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

procedure TACBrNFSeProviderISSCampinas.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, Prefixo, PrefixoTS: string;
begin
  if EstaVazio(Response.NumeroLote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := ACBrStr(Desc111);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  Prefixo := '';
  PrefixoTS := '';

  if EstaVazio(ConfigMsgDados.LoteRps.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.LoteRps.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' +
                               ConfigMsgDados.LoteRps.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.LoteRps.xmlns then
    begin
      if ConfigMsgDados.PrefixoTS = '' then
        NameSpace := NameSpace + ' xmlns="' + ConfigMsgDados.XmlRps.xmlns + '"'
      else
      begin
        NameSpace := NameSpace + ' xmlns:' + ConfigMsgDados.PrefixoTS + '="' +
                                             ConfigMsgDados.XmlRps.xmlns + '"';
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
      end;
    end
    else
    begin
      if ConfigMsgDados.PrefixoTS <> '' then
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
    end;
  end;

  NameSpace := NameSpace +
    ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
    ' xsi:schemaLocation="http://localhost:8080/WsNFe2/lote' +
    ' http://localhost:8080/WsNFe2/xsd/ReqConsultaLote.xsd"';

  Response.ArquivoEnvio := '<' + Prefixo + 'ReqConsultaLote' + NameSpace + '>' +
                             '<Cabecalho>' +
                               '<CodCidade>' +
                                 CodIBGEToCodTOM(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                               '</CodCidade>' +
                               '<CPFCNPJRemetente>' +
                                 OnlyNumber(Emitente.CNPJ) +
                               '</CPFCNPJRemetente>' +
                               '<Versao>1</Versao>' +
                               '<NumeroLote>' +
                                 Response.Protocolo +
                               '</NumeroLote>' +
                             '</Cabecalho>' +
                           '</' + Prefixo + 'ReqConsultaLote>';
end;

procedure TACBrNFSeProviderISSCampinas.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  AResumo: TNFSeResumoCollectionItem;
//  ANota: TNotaFiscal;
//  NumRps, NumNFSe: String;
begin
  Document := TACBrXmlDocument.Create;
//  NumRps := '';
//  NumNFSe := '';

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

      if (Document.Root.Name = 'RetornoConsultaLote') then
        ANode := Document.Root
      else
        ANode := Document.Root.Childrens.FindAnyNs('RetornoConsultaLote');

      if not Assigned(ANode) then
      begin
        AErro := Response.Erros.New;
        AErro.Codigo := Cod203;
        AErro.Descricao := ACBrStr(Desc203);
        Exit;
      end;

      ProcessarMensagemErros(ANode, Response);

      AuxNode := ANode.Childrens.FindAnyNs('Cabecalho');

      if AuxNode <> nil then
      begin
        ProcessarMensagemErros(AuxNode, Response);

        with Response do
        begin
          Protocolo := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('NumeroLote'), tcStr);
          Sucesso := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Sucesso'), tcStr) = 'true';
          Data := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('DataEnvioLote'), tcDatHor);

          { Verificar se mais alguma dessas informações são necessárias
          with InformacoesLote do
          begin
            InscricaoPrestador := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('InscricaoPrestador'), tcStr);
            CPFCNPJRemetente := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CPFCNPJRemetente'), tcStr);
            QtdNotasProcessadas := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('QtdNotasProcessadas'), tcInt);
            TempoProcessamento := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('TempoProcessamento'), tcInt);
            ValorTotalServico := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('ValorTotalServico'), tcDe2);
          end;
          }
        end;
      end;

      Response.Sucesso := (Response.Erros.Count = 0) and (Response.Alertas.Count = 0);

      ANode := ANode.Childrens.FindAnyNs('ListaNFSe');

      if ANode <> nil then
      begin
        ANodeArray := ANode.Childrens.FindAllAnyNs('ConsultaNFSe');

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

          AResumo := Response.Resumos.New;
          AResumo.NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('NumeroNFe'), tcStr);
          AResumo.CodigoVerificacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
          AResumo.NumeroRps := ObterConteudoTag(ANode.Childrens.FindAnyNs('NumeroRPS'), tcStr);
          AResumo.SerieRps := ObterConteudoTag(ANode.Childrens.FindAnyNs('SerieRPS'), tcStr);
          AResumo.Link := ObterConteudoTag(ANode.Childrens.FindAnyNs('URLNotaFiscal'), tcStr);
          AResumo.Link := StringReplace(AResumo.Link, '&amp;', '&', [rfReplaceAll]);

          with Response do
          begin
            NumeroNota := ObterConteudoTag(ANode.Childrens.FindAnyNs('NumeroNFe'), tcStr);
            CodigoVerificacao := ObterConteudoTag(ANode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
            NumeroRps := ObterConteudoTag(ANode.Childrens.FindAnyNs('NumeroRPS'), tcStr);
            SerieRps := ObterConteudoTag(ANode.Childrens.FindAnyNs('SerieRPS'), tcStr);
          end;
          {
          NumRps := LerChaveRPS(ANode);
          NumNFSe := LerChaveNFe(ANode);

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

          if ANota = nil then
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

          ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
          SalvarXmlNfse(ANota);
          }
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

procedure TACBrNFSeProviderISSCampinas.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, Prefixo, PrefixoTS: string;
begin
  if EstaVazio(Response.NumeroRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod102;
    AErro.Descricao := ACBrStr(Desc102);
    Exit;
  end;

  if EstaVazio(Response.SerieRps) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod103;
    AErro.Descricao := ACBrStr(Desc103);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  Prefixo := '';
  PrefixoTS := '';

  if EstaVazio(ConfigMsgDados.LoteRps.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.LoteRps.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' +
                               ConfigMsgDados.LoteRps.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.LoteRps.xmlns then
    begin
      if ConfigMsgDados.PrefixoTS = '' then
        NameSpace := NameSpace + ' xmlns="' + ConfigMsgDados.XmlRps.xmlns + '"'
      else
      begin
        NameSpace := NameSpace + ' xmlns:' + ConfigMsgDados.PrefixoTS + '="' +
                                             ConfigMsgDados.XmlRps.xmlns + '"';
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
      end;
    end
    else
    begin
      if ConfigMsgDados.PrefixoTS <> '' then
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
    end;
  end;

  NameSpace := NameSpace +
    ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
    ' xsi:schemaLocation="http://localhost:8080/WsNFe2/lote' +
    ' http://localhost:8080/WsNFe2/xsd/ReqConsultaNFSeRPS.xsd"';

  Response.ArquivoEnvio := '<' + Prefixo + 'ReqConsultaNFSeRPS' + NameSpace + '>' +
                             '<Cabecalho>' +
                               '<CodCidade>' +
                                 CodIBGEToCodTOM(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                               '</CodCidade>' +
                               '<CPFCNPJRemetente>' +
                                 OnlyNumber(Emitente.CNPJ) +
                               '</CPFCNPJRemetente>' +
                               '<transacao>false</transacao>' +
                               '<Versao>1</Versao>' +
                             '</Cabecalho>' +
                             '<Lote>' +
                               '<RPSConsulta>' +
                                 '<RPS>' +
                                   '<InscricaoMunicipalPrestador>' +
                                     OnlyNumber(Emitente.InscMun) +
                                   '</InscricaoMunicipalPrestador>' +
                                   '<NumeroRPS>' + Response.NumeroRps + '</NumeroRPS>' +
                                   '<SeriePrestacao>' + Response.SerieRps + '</SeriePrestacao>' +
                                 '</RPS>' +
                               '</RPSConsulta>' +
                             '</Lote>' +
                           '</' + Prefixo + 'ReqConsultaNFSeRPS>';
end;

procedure TACBrNFSeProviderISSCampinas.PrepararConsultarSeqRps(
  Response: TNFSeConsultarSeqRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, Prefixo, PrefixoTS: string;
begin
  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(Emitente.InscMun) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod129;
    AErro.Descricao := ACBrStr(Desc129);
    Exit;
  end;

  if EstaVazio(Emitente.CNPJ) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod130;
    AErro.Descricao := ACBrStr(Desc130);
    Exit;
  end;

  Prefixo := '';
  PrefixoTS := '';

  if EstaVazio(ConfigMsgDados.LoteRps.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.LoteRps.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' +
                               ConfigMsgDados.LoteRps.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.LoteRps.xmlns then
    begin
      if ConfigMsgDados.PrefixoTS = '' then
        NameSpace := NameSpace + ' xmlns="' + ConfigMsgDados.XmlRps.xmlns + '"'
      else
      begin
        NameSpace := NameSpace + ' xmlns:' + ConfigMsgDados.PrefixoTS + '="' +
                                             ConfigMsgDados.XmlRps.xmlns + '"';
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
      end;
    end
    else
    begin
      if ConfigMsgDados.PrefixoTS <> '' then
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
    end;
  end;

  NameSpace := NameSpace +
    ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
    ' xsi:schemaLocation="http://localhost:8080/WsNFe2/lote' +
    ' http://localhost:8080/WsNFe2/xsd/ConsultaSeqRps.xsd"';

  Response.ArquivoEnvio := '<' + Prefixo + 'ConsultaSeqRps' + NameSpace + '>' +
                             '<Cabecalho>' +
                               '<CodCid>' +
                                 CodIBGEToCodTOM(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                               '</CodCid>' +
                               '<IMPrestador>' +
                                 Emitente.InscMun +
                               '</IMPrestador>' +
                               '<CPFCNPJRemetente>' +
                                 OnlyNumber(Emitente.CNPJ) +
                               '</CPFCNPJRemetente>' +
                               '<Versao>1</Versao>' +
                             '</Cabecalho>' +
                           '</' + Prefixo + 'ConsultaSeqRps>';

end;

procedure TACBrNFSeProviderISSCampinas.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  ANota: TNotaFiscal;
  NumRps, NumNFSe: String;
begin
  Document := TACBrXmlDocument.Create;
  NumRps := '';
  NumNFSe := '';

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

      ANode := Document.Root.Childrens.FindAnyNs('RetornoConsultaNFSeRPS');

      ProcessarMensagemErros(ANode, Response);

      Response.Sucesso := (Response.Erros.Count = 0) and (Response.Alertas.Count = 0);

      ANode := ANode.Childrens.Find('NotasConsultadas');

      if ANode <> nil then
      begin
        ANodeArray := ANode.Childrens.FindAllAnyNs('Nota');

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

          NumRps := LerChaveRPS(ANode);

          if NumRps = '' then
            NumRps := ObterConteudoTag(ANode.Childrens.FindAnyNs('NumeroRPS'), tcStr);

          NumNFSe := LerChaveNFe(ANode);

          if NumNFSe = '' then
            NumNFSe := ObterConteudoTag(ANode.Childrens.FindAnyNs('NumeroNota'), tcStr);

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

          if ANota = nil then
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

          ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
          SalvarXmlNfse(ANota);
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

procedure TACBrNFSeProviderISSCampinas.TratarRetornoConsultarSeqRps(
  Response: TNFSeConsultarSeqRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
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

      ANode := Document.Root.Childrens.FindAnyNs('RetornoConsultaSeqRps');

      if ANode <> nil then
      begin
        ProcessarMensagemErros(ANode, Response);

        AuxNode := ANode.Childrens.FindAnyNs('Cabecalho');

        if AuxNode <> nil then
        begin
          ProcessarMensagemErros(AuxNode, Response);

          with Response do
          begin
            CodCid := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CodCid'), tcStr);
            CPFCNPJRemetente := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CPFCNPJRemetente'), tcStr);
            IMPrestador := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('IMPrestador'), tcStr);
            NroUltimoRps := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('NroUltimoRps'), tcInt);
          end;
        end;

        Response.Sucesso := (Response.Erros.Count = 0) and (Response.Alertas.Count = 0);
      end
      else
        Response.Sucesso := False;

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

procedure TACBrNFSeProviderISSCampinas.PrepararConsultaNFSeporNumero(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, Prefixo, PrefixoTS, xDataI, xDataF: string;
  wAno, wMes, wDia: Word;
begin
  if Response.InfConsultaNFSe.DataInicial = 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod115;
    AErro.Descricao := ACBrStr(Desc115);
    Exit;
  end;

  if Response.InfConsultaNFSe.DataFinal = 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod116;
    AErro.Descricao := ACBrStr(Desc116);
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;
  Prefixo := '';
  PrefixoTS := '';

  Response.Metodo := tmConsultarNFSe;

  if EstaVazio(ConfigMsgDados.LoteRps.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.LoteRps.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' +
                               ConfigMsgDados.LoteRps.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.LoteRps.xmlns then
    begin
      if ConfigMsgDados.PrefixoTS = '' then
        NameSpace := NameSpace + ' xmlns="' + ConfigMsgDados.XmlRps.xmlns + '"'
      else
      begin
        NameSpace := NameSpace + ' xmlns:' + ConfigMsgDados.PrefixoTS + '="' +
                                             ConfigMsgDados.XmlRps.xmlns + '"';
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
      end;
    end
    else
    begin
      if ConfigMsgDados.PrefixoTS <> '' then
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
    end;
  end;

  DecodeDate(VarToDateTime(Response.InfConsultaNFSe.DataInicial), wAno, wMes, wDia);
  xDataI := FormatFloat('0000', wAno) + '-' +
            FormatFloat('00', wMes) + '-' + FormatFloat('00', wDia);

  DecodeDate(VarToDateTime(Response.InfConsultaNFSe.DataFinal), wAno, wMes, wDia);
  xDataF := FormatFloat('0000', wAno) + '-' +
            FormatFloat('00', wMes) + '-' + FormatFloat('00', wDia);

  NameSpace := NameSpace +
    ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
    ' xsi:schemaLocation="http://localhost:8080/WsNFe2/lote' +
    ' http://localhost:8080/WsNFe2/xsd/ReqConsultaNotas.xsd"';

  Response.ArquivoEnvio := '<' + Prefixo + 'ReqConsultaNotas' + NameSpace + '>' +
                             '<Cabecalho>' +
                               '<CodCidade>' +
                                 CodIBGEToCodTOM(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                               '</CodCidade>' +
                               '<CPFCNPJRemetente>' +
                                 OnlyNumber(Emitente.CNPJ) +
                               '</CPFCNPJRemetente>' +
                               '<InscricaoMunicipalPrestador>' +
                                 OnlyNumber(Emitente.InscMun) +
                               '</InscricaoMunicipalPrestador>' +
                               '<dtInicio>' + xDataI + '</dtInicio>' +
                               '<dtFim>' + xDataF + '</dtFim>' +
                               '<Versao>1</Versao>' +
                             '</Cabecalho>' +
                           '</' + Prefixo + 'ReqConsultaNotas>';
end;

procedure TACBrNFSeProviderISSCampinas.TratarRetornoConsultaNFSeporNumero(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  ANota: TNotaFiscal;
  NumRps, NumNFSe: String;
begin
  Document := TACBrXmlDocument.Create;
  NumRps := '';
  NumNFSe := '';

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

      ANode := Document.Root.Childrens.FindAnyNs('RetornoConsultaNotas');

      ProcessarMensagemErros(ANode, Response);
      (*
      AuxNode := ANode.Childrens.FindAnyNs('Cabecalho');

      if AuxNode <> nil then
      begin
        ProcessarMensagemErros(AuxNode, Response);

        with Response do
        begin
          { Verificar se mais alguma dessas informações são necessárias
          with InformacoesLote do
          begin
            CPFCNPJRemetente := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('CPFCNPJRemetente'), tcStr);
          end;
          }
        end;
      end;
      *)
      Response.Sucesso := (Response.Erros.Count = 0) and (Response.Alertas.Count = 0);

      ANode := ANode.Childrens.Find('Notas');

      if ANode <> nil then
      begin
        ANodeArray := ANode.Childrens.FindAllAnyNs('Nota');

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

          NumRps := LerChaveRPS(ANode);
          NumNFSe := LerChaveNFe(ANode);

          ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(NumRps);

          if ANota = nil then
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

          ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
          SalvarXmlNfse(ANota);
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

procedure TACBrNFSeProviderISSCampinas.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, Prefixo, PrefixoTS: string;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := ACBrStr(Desc108);
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.CodVerificacao) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod117;
    AErro.Descricao := ACBrStr(Desc117);
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
  Prefixo := '';
  PrefixoTS := '';

  if EstaVazio(ConfigMsgDados.LoteRps.xmlns) then
    NameSpace := ''
  else
  begin
    if ConfigMsgDados.Prefixo = '' then
      NameSpace := ' xmlns="' + ConfigMsgDados.LoteRps.xmlns + '"'
    else
    begin
      NameSpace := ' xmlns:' + ConfigMsgDados.Prefixo + '="' +
                               ConfigMsgDados.LoteRps.xmlns + '"';
      Prefixo := ConfigMsgDados.Prefixo + ':';
    end;
  end;

  if ConfigMsgDados.XmlRps.xmlns <> '' then
  begin
    if ConfigMsgDados.XmlRps.xmlns <> ConfigMsgDados.LoteRps.xmlns then
    begin
      if ConfigMsgDados.PrefixoTS = '' then
        NameSpace := NameSpace + ' xmlns="' + ConfigMsgDados.XmlRps.xmlns + '"'
      else
      begin
        NameSpace := NameSpace + ' xmlns:' + ConfigMsgDados.PrefixoTS + '="' +
                                             ConfigMsgDados.XmlRps.xmlns + '"';
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
      end;
    end
    else
    begin
      if ConfigMsgDados.PrefixoTS <> '' then
        PrefixoTS := ConfigMsgDados.PrefixoTS + ':';
    end;
  end;

  NameSpace := NameSpace +
    ' xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"' +
    ' xsi:schemaLocation="http://localhost:8080/WsNFe2/lote' +
    ' http://localhost:8080/WsNFe2/xsd/ReqCancelamentoNFSe.xsd"';

  {
    Tag <transacao>
    Informe se as NFS-e a serem canceladas farão parte de uma mesma transação.

    True - As NFS-e só serão canceladas se não ocorrer nenhum evento de erro
           durante o processamento de todo o lote;

    False - As NFS-e aptas a serem canceladas serão canceladas, mesmo que
            ocorram eventos de erro durante processamento do cancelamento de
            outras NFS-e deste lote.
  }

  Response.ArquivoEnvio := '<' + Prefixo + 'ReqCancelamentoNFSe' + NameSpace + '>' +
                             '<Cabecalho>' +
                               '<CodCidade>' +
                                 CodIBGEToCodTOM(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                               '</CodCidade>' +
                               '<CPFCNPJRemetente>' +
                                 OnlyNumber(Emitente.CNPJ) +
                               '</CPFCNPJRemetente>' +
                               '<transacao>false</transacao>' +
                               '<Versao>1</Versao>' +
                             '</Cabecalho>' +
                             '<Lote>' +
                               '<Nota>' +
                                 '<InscricaoMunicipalPrestador>' +
                                   OnlyNumber(Emitente.InscMun) +
                                 '</InscricaoMunicipalPrestador>' +
                                 '<NumeroNota>' +
                                   Response.InfCancelamento.NumeroNFSe +
                                 '</NumeroNota>' +
                                 '<CodigoVerificacao>' +
                                   Response.InfCancelamento.CodVerificacao +
                                 '</CodigoVerificacao>' +
                                 '<MotivoCancelamento>' +
                                   Response.InfCancelamento.MotCancelamento +
                                 '</MotivoCancelamento>' +
                               '</Nota>' +
                             '</Lote>' +
                           '</' + Prefixo + 'ReqCancelamentoNFSe>';
end;

procedure TACBrNFSeProviderISSCampinas.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  i: Integer;
  ANodeArray: TACBrXmlNodeArray;
  ANota: TNotaFiscal;
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

      ANode := Document.Root.Childrens.FindAnyNs('RetornoCancelamentoNFSe');

      if ANode <> nil then
      begin
        ProcessarMensagemErros(ANode, Response);

        AuxNode := ANode.Childrens.FindAnyNs('Cabecalho');

        if AuxNode <> nil then
        begin
          ProcessarMensagemErros(AuxNode, Response);

          with Response do
          begin
            Sucesso := ObterConteudoTag(AuxNode.Childrens.FindAnyNs('Sucesso'), tcStr) = 'true';
          end;
        end;

        Response.Sucesso := (Response.Erros.Count = 0) and (Response.Alertas.Count = 0);
      end
      else
        Response.Sucesso := False;

      if ANode <> nil then
        ANode := ANode.Childrens.Find('NotasCanceladas');

      if ANode <> nil then
      begin
        ANodeArray := ANode.Childrens.FindAllAnyNs('Nota');

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

          try
            //Tenta localizar pelo número da nota
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(ANode.Childrens.FindAnyNs('NumeroNota').AsString);
          except
            //Se não achou tenta pelo número do RPS
            //if not Assigned(ANota) then
            ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByRps(ANode.Childrens.FindAnyNs('NumeroRPS').AsString);
          end;

          ANota := CarregarXmlNfse(ANota, ANode.OuterXml);
          SalvarXmlNfse(ANota);
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

{ TACBrNFSeXWebserviceISSCampinas }

function TACBrNFSeXWebserviceISSCampinas.GetNameSpace: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 1 then
    Result := TACBrNFSeX(FPDFeOwner).Provider.ConfigWebServices.Producao.NameSpace
  else
    Result := TACBrNFSeX(FPDFeOwner).Provider.ConfigWebServices.Homologacao.NameSpace;

  Result := 'xmlns:lot="' + Result + '"';
end;

function TACBrNFSeXWebserviceISSCampinas.Recepcionar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<lot:enviar>';
  Request := Request + '<mensagemXml>' + IncluirCDATA(AMSG) + '</mensagemXml>';
  Request := Request + '</lot:enviar>';

  Result := Executar('', Request, ['enviarReturn', 'RetornoEnvioLoteRPS'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceISSCampinas.RecepcionarSincrono(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<lot:enviarSincrono>';
  Request := Request + '<mensagemXml>' + IncluirCDATA(AMSG) + '</mensagemXml>';
  Request := Request + '</lot:enviarSincrono>';

  Result := Executar('', Request, ['enviarSincronoReturn', 'RetornoEnvioLoteRPS'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceISSCampinas.TesteEnvio(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<lot:testeEnviar>';
  Request := Request + '<mensagemXml>' + IncluirCDATA(AMSG) + '</mensagemXml>';
  Request := Request + '</lot:testeEnviar>';

  Result := Executar('', Request, ['testeEnviarReturn', 'RetornoEnvioLoteRPS'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceISSCampinas.ConsultarLote(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<lot:consultarLote>';
  Request := Request + '<mensagemXml>' + IncluirCDATA(AMSG) + '</mensagemXml>';
  Request := Request + '</lot:consultarLote>';

  Result := Executar('', Request, ['consultarLoteReturn'], [NameSpace]);
end;

function TACBrNFSeXWebserviceISSCampinas.ConsultarNFSePorRps(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<lot:consultarNFSeRps>';
  Request := Request + '<mensagemXml>' + IncluirCDATA(AMSG) + '</mensagemXml>';
  Request := Request + '</lot:consultarNFSeRps>';

  Result := Executar('', Request, ['consultarNFSeRpsReturn'], [NameSpace]);
end;

function TACBrNFSeXWebserviceISSCampinas.ConsultarSeqRps(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<lot:consultarSequencialRps>';
  Request := Request + '<mensagemXml>' + IncluirCDATA(AMSG) + '</mensagemXml>';
  Request := Request + '</lot:consultarSequencialRps>';

  Result := Executar('', Request, ['consultarSequencialRpsReturn'], [NameSpace]);
end;

function TACBrNFSeXWebserviceISSCampinas.ConsultarNFSe(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<lot:consultarNota>';
  Request := Request + '<mensagemXml>' + IncluirCDATA(AMSG) + '</mensagemXml>';
  Request := Request + '</lot:consultarNota>';

  Result := Executar('', Request, ['consultarNotaReturn'], [NameSpace]);
end;

function TACBrNFSeXWebserviceISSCampinas.Cancelar(const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<lot:cancelar>';
  Request := Request + '<mensagemXml>' + IncluirCDATA(AMSG) + '</mensagemXml>';
  Request := Request + '</lot:cancelar>';

  Result := Executar('', Request, ['cancelarReturn'], [NameSpace]);
end;

function TACBrNFSeXWebserviceISSCampinas.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := inherited TratarXmlRetornado(aXML);

  Result := ParseText(Result);
  Result := RemoverDeclaracaoXML(Result);
  Result := RemoverIdentacao(Result);
  Result := RemoverCaracteresDesnecessarios(Result);
  Result := RemoverPrefixosDesnecessarios(Result);
  Result := Trim(StringReplace(Result, '&', '&amp;', [rfReplaceAll]));
end;

{ TACBrNFSeXWebserviceISSCampinas203 }

function TACBrNFSeXWebserviceISSCampinas203.Recepcionar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:RecepcionarLoteRps>';
  Request := Request + AMSG;
  Request := Request + '</nfse:RecepcionarLoteRps>';

  Result := Executar('', Request,
                     ['RecepcionarLoteRpsResponse', 'EnviarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSCampinas203.RecepcionarSincrono(
  const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:EnviarLoteRpsSincrono>';
  Request := Request + AMSG;
  Request := Request + '</nfse:EnviarLoteRpsSincrono>';

  Result := Executar('', Request,
                     ['EnviarLoteRpsSincronoResponse', 'EnviarLoteRpsSincronoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSCampinas203.GerarNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:GerarNfse>';
  Request := Request + AMSG;
  Request := Request + '</nfse:GerarNfse>';

  Result := Executar('', Request,
                     ['GerarNfseResponseResponse', 'GerarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSCampinas203.ConsultarLote(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarLoteRps>';
  Request := Request + AMSG;
  Request := Request + '</nfse:ConsultarLoteRps>';

  Result := Executar('', Request,
                     ['ConsultarLoteRpsResponse', 'ConsultarLoteRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSCampinas203.ConsultarNFSePorRps(
  const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorRps>';
  Request := Request + AMSG;
  Request := Request + '</nfse:ConsultarNfsePorRps>';

  Result := Executar('', Request,
                     ['ConsultarNfsePorRpsResponse', 'ConsultarNfseRpsResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSCampinas203.ConsultarNFSePorFaixa(
  const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfsePorFaixa>';
  Request := Request + AMSG;
  Request := Request + '</nfse:ConsultarNfsePorFaixa>';

  Result := Executar('', Request,
                     ['ConsultarNfseResponse', 'ConsultarNfseFaixaResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSCampinas203.ConsultarNFSeServicoPrestado(
  const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoPrestado>';
  Request := Request + AMSG;
  Request := Request + '</nfse:ConsultarNfseServicoPrestado>';

  Result := Executar('', Request,
                     ['ConsultarNfseServicoPrestadoResponse', 'ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSCampinas203.ConsultarNFSeServicoTomado(
  const ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:ConsultarNfseServicoTomado>';
  Request := Request + AMSG;
  Request := Request + '</nfse:ConsultarNfseServicoTomado>';

  Result := Executar('', Request,
                     ['ConsultarNfseServicoTomadoResponse', 'ConsultarNfseServicoTomadoResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSCampinas203.Cancelar(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:CancelarNfse>';
  Request := Request + AMSG;
  Request := Request + '</nfse:CancelarNfse>';

  Result := Executar('', Request,
                     ['CancelarNfseResponse', 'CancelarNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSCampinas203.SubstituirNFSe(const ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfse:SubstituirNfse>';
  Request := Request + AMSG;
  Request := Request + '</nfse:SubstituirNfse>';

  Result := Executar('', Request,
                     ['SubstituirNfseResponse', 'SubstituirNfseResposta'],
                     ['xmlns:nfse="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceISSCampinas203.TratarXmlRetornado(
  const aXML: string): string;
begin
  Result := ConverteANSIparaUTF8(aXML);
  Result := RemoverDeclaracaoXML(Result);

  Result := inherited TratarXmlRetornado(Result);

end;

{ TACBrNFSeProviderISSCampinas203 }

procedure TACBrNFSeProviderISSCampinas203.Configuracao;
begin
  inherited Configuracao;

  with ConfigWebServices do
  begin
    VersaoDados := '2.03';
    VersaoAtrib := '2.03';
  end;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    RpsGerarNFSe := True;
    ConsultarLote := True;
    ConsultarNFSeRps := True;
    ConsultarNFSePorFaixa := True;
    ConsultarNFSeServicoPrestado := True;
    ConsultarNFSeServicoTomado := True;
    CancelarNFSe := True;
    RpsSubstituirNFSe := True;
    SubstituirNFSe := True;
  end;
end;

function TACBrNFSeProviderISSCampinas203.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_ISSCampinas203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSCampinas203.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_ISSCampinas203.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSCampinas203.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceISSCampinas203.Create(FAOwner, AMetodo, URL)
  else
  begin
    if ConfigGeral.Ambiente = taProducao then
      raise EACBrDFeException.Create(ERR_SEM_URL_PRO)
    else
      raise EACBrDFeException.Create(ERR_SEM_URL_HOM);
  end;
end;

end.
