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

unit ISSDSF.Provider;

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
  TACBrNFSeXWebserviceISSDSF = class(TACBrNFSeXWebserviceSoap11)
  private
    function GetNameSpace: string;
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function TesteEnvio(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

    property NameSpace: string read GetNameSpace;
  end;

  TACBrNFSeProviderISSDSF = class (TACBrNFSeProviderProprio)
  protected
    procedure Configuracao; override;

    procedure AssinaturaAdicional(Nota: NotaFiscal);

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

    procedure ProcessarMensagemErros(const RootNode: TACBrXmlNode;
                                     const Response: TNFSeWebserviceResponse;
                                     AListTag: string = '';
                                     AMessageTag: string = 'Erro'); override;

  end;

implementation

uses
  ACBrUtil, ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  ISSDSF.GravarXml, ISSDSF.LerXml;

{ TACBrNFSeProviderISSDSF }

procedure TACBrNFSeProviderISSDSF.AssinaturaAdicional(Nota: NotaFiscal);
var
  sSituacao, sISSRetido, sCPFCNPJTomador, sIndTomador, sTomador,
  sCPFCNPJInter, sIndInter, sISSRetidoInter, sInter, sAssinatura: String;
begin
  with Nota do
  begin
    sSituacao := EnumeradoToStr(NFSe.Status, ['N', 'C'], [srNormal, srCancelado]);

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
    sCPFCNPJInter := OnlyNumber(NFSe.IntermediarioServico.CpfCnpj);

    if Length(sCPFCNPJInter) = 11 then
      sIndInter := '1'
    else
      if Length(sCPFCNPJInter) = 14 then
        sIndInter := '2'
      else
        sIndInter := '3';

    sISSRetidoInter := EnumeradoToStr(NFSe.IntermediarioServico.IssRetido,
                                      ['N', 'S'], [stNormal, stRetencao]);

    if sIndInter <> '3' then
      sInter := sIndInter + Poem_Zeros(sCPFCNPJInter, 14) + sISSRetidoInter
    else
      sInter := '';

    sAssinatura := Poem_Zeros(NFSe.Prestador.IdentificacaoPrestador.InscricaoMunicipal, 8) +
                   PadRight(NFSe.IdentificacaoRps.Serie, 5 , ' ') +
                   Poem_Zeros(NFSe.IdentificacaoRps.Numero, 12) +
                   FormatDateTime('yyyymmdd', NFse.DataEmissao) +
                   TipoTributacaoRPSToStr(NFSe.TipoTributacaoRPS) +
                   sSituacao +
                   sISSRetido +
                   Poem_Zeros(OnlyNumber(FormatFloat('#0.00', NFSe.Servico.Valores.ValorServicos)), 15 ) +
                   Poem_Zeros(OnlyNumber(FormatFloat('#0.00', NFSe.Servico.Valores.ValorDeducoes)), 15 ) +
                   Poem_Zeros(OnlyNumber(NFSe.Servico.ItemListaServico ), 5 ) +
                   sTomador +
                   sInter;

    with TACBrNFSeX(FAOwner) do
      NFSe.Assinatura := SSL.CalcHash(sAssinatura, dgstSHA1, outBase64, True);
  end;
end;

procedure TACBrNFSeProviderISSDSF.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    QuebradeLinha := '<br >';
    ModoEnvio := meLoteSincrono;
  end;

  with ConfigAssinar do
  begin
    LoteRps := True;
    ConsultarLote := True;
    ConsultarNFSeRps := True;
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

    XmlRps.xmlns := 'http://localhost:8080/WsNFe2/tp';

    with LoteRps do
    begin
      InfElemento := 'Lote';
      DocElemento := 'ReqEnvioLoteRPS';
    end;

    with LoteRpsSincrono do
    begin
      InfElemento := 'Lote';
      DocElemento := 'ReqEnvioLoteRPS';
    end;

    with ConsultarLote do
    begin
      InfElemento := '';
      DocElemento := 'ReqConsultaLote';
    end;

    with ConsultarNFSeRps do
    begin
      InfElemento := 'Lote';
      DocElemento := 'ReqConsultaNFSeRPS';
    end;

    with ConsultarNFSe do
    begin
      InfElemento := 'Lote';
      DocElemento := 'ReqConsultaNotas';
    end;

    with CancelarNFSe do
    begin
      InfElemento := 'Lote';
      DocElemento := 'ReqCancelamentoNFSe';
    end;
  end;

  with ConfigSchemas do
  begin
    Recepcionar := 'ReqEnvioLoteRPS.xsd';
    ConsultarLote := 'ReqConsultaLote.xsd';
    ConsultarNFSeRps := 'ReqConsultaNFSeRPS.xsd';
    ConsultarNFSe := 'ReqConsultaNotas.xsd';
    CancelarNFSe := 'ReqCancelamentoNFSe.xsd';
    RecepcionarSincrono := 'ReqEnvioLoteRPS.xsd';
    Teste := 'ReqEnvioLoteRPS.xsd';
  end;
end;

function TACBrNFSeProviderISSDSF.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_ISSDSF.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSDSF.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_ISSDSF.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderISSDSF.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceISSDSF.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderISSDSF.ProcessarMensagemErros(
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
    AErro.Correcao := '';

    if AErro.Descricao = '' then
      AErro.Descricao := ANodeArray[I].AsString;
  end;
end;

procedure TACBrNFSeProviderISSDSF.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Nota: NotaFiscal;
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

  DataInicial := 0;
  DataFinal := 0;
  vTotServicos := 0;
  vTotDeducoes := 0;

  for i := 0 to TACBrNFSeX(FAOwner).NotasFiscais.Count -1 do
  begin
    Nota := TACBrNFSeX(FAOwner).NotasFiscais.Items[i];

    if EstaVazio(Nota.XMLAssinado) then
    begin
      AssinaturaAdicional(Nota);

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

    xRps := RemoverDeclaracaoXML(Nota.XMLOriginal);

    xRps := '<RPS>' + SeparaDados(xRps, 'RPS') + '</RPS>';

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

  xCabecalho := '<Cabecalho>' +
                  '<CodCidade>' +
                    CodIBGEToCodTOM(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                  '</CodCidade>' +
                  '<CPFCNPJRemetente>' +
                    OnlyNumber(Emitente.CNPJ) +
                  '</CPFCNPJRemetente>' +
                  '<RazaoSocialRemetente>' +
                    Trim(Emitente.RazSocial) +
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
        NameSpace := NameSpace+ ' xmlns:' + ConfigMsgDados.PrefixoTS + '="' +
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

  Response.XmlEnvio := '<' + Prefixo + 'ReqEnvioLoteRPS' + NameSpace + '>' +
                          xCabecalho +
                          '<Lote>' + ListaRps + '</Lote>' +
                       '</' + Prefixo + 'ReqEnvioLoteRPS>';
end;

procedure TACBrNFSeProviderISSDSF.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  AuxNode, AuxNodeChave: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response, 'Erros', 'Erro');

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('Sucesso'), tcStr);

          with InformacoesLote do
          begin
            NumeroLote := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('NumeroLote'), tcStr);
            InscricaoPrestador := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('InscricaoPrestador'), tcStr);
            CPFCNPJRemetente := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('CPFCNPJRemetente'), tcStr);
            DataEnvioLote := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('DataEnvioLote'), tcDatHor);
            QtdNotasProcessadas := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('QtdNotasProcessadas'), tcInt);
            TempoProcessamento := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('TempoProcessamento'), tcInt);
            ValorTotalServico := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('ValorTotalServico'), tcDe2);
          end;
        end;
      end;

      AuxNode := ANode.Childrens.FindAnyNs('ChavesNFeRPS');

      if AuxNode <> nil then
      begin
        AuxNodeChave := AuxNode.Childrens.FindAnyNs('ChaveRPS');

        if (AuxNodeChave <> nil) then
        begin
          with Response.InfRetorno.ChaveNFeRPS do
          begin
            InscricaoPrestador := ProcessarConteudoXml(AuxNodeChave.Childrens.FindAnyNs('InscricaoPrestador'), tcStr);
            SerieRPS := ProcessarConteudoXml(AuxNodeChave.Childrens.FindAnyNs('SerieRPS'), tcStr);
            NumeroRPS := ProcessarConteudoXml(AuxNodeChave.Childrens.FindAnyNs('NumeroRPS'), tcStr);
          end;
        end;

        AuxNodeChave := AuxNode.Childrens.FindAnyNs('ChaveNFe');

        if (AuxNodeChave <> nil) then
        begin
          with Response.InfRetorno.ChaveNFeRPS do
          begin
            InscricaoPrestador := ProcessarConteudoXml(AuxNodeChave.Childrens.FindAnyNs('InscricaoPrestador'), tcStr);
            Numero := ProcessarConteudoXml(AuxNodeChave.Childrens.FindAnyNs('NumeroNFe'), tcStr);
            CodigoVerificacao := ProcessarConteudoXml(AuxNodeChave.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
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

procedure TACBrNFSeProviderISSDSF.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, Prefixo, PrefixoTS: string;
begin
  if EstaVazio(Response.Lote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := Desc111;
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
        NameSpace := NameSpace+ ' xmlns:' + ConfigMsgDados.PrefixoTS + '="' +
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

  Response.XmlEnvio := '<' + Prefixo + 'ReqConsultaLote' + NameSpace + '>' +
                         '<Cabecalho>' +
                           '<CodCidade>' +
                             CodIBGEToCodTOM(TACBrNFSeX(FAOwner).Configuracoes.Geral.CodigoMunicipio) +
                           '</CodCidade>' +
                           '<CPFCNPJRemetente>' +
                             OnlyNumber(Emitente.CNPJ) +
                           '</CPFCNPJRemetente>' +
                           '<Versao>1</Versao>' +
                           '<NumeroLote>' +
                             Response.Lote +
                           '</NumeroLote>' +
                         '</Cabecalho>' +
                       '</' + Prefixo + 'ReqConsultaLote>';
end;

procedure TACBrNFSeProviderISSDSF.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  NumNFSe: String;
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

      ProcessarMensagemErros(ANode, Response, 'Erros', 'Erro');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('Sucesso'), tcStr);
        end;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('ListaNFSe');
//      ANodeArray := ANode.Childrens.FindAllAnyNs('NFe');
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
        AuxNode := ANode.Childrens.FindAnyNs('ChaveNFe');
        AuxNode := AuxNode.Childrens.FindAnyNs('NumeroNFe');
        NumNFSe := AuxNode.AsString;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

        if Assigned(ANota) then
          ANota.XML := ANode.OuterXml
        else
        begin
          TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
        end;

        SalvarXmlNfse(ANota);
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

procedure TACBrNFSeProviderISSDSF.PrepararConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, Prefixo, PrefixoTS: string;
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
        NameSpace := NameSpace+ ' xmlns:' + ConfigMsgDados.PrefixoTS + '="' +
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

  Response.XmlEnvio := '<' + Prefixo + 'ReqConsultaNFSeRPS' + NameSpace + '>' +
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
                               '<NumeroRPS>' + Response.NumRPS + '</NumeroRPS>' +
                               '<SeriePrestacao>' + Response.Serie + '</SeriePrestacao>' +
                             '</RPS>' +
                           '</RPSConsulta>' +
                         '</Lote>' +
                       '</' + Prefixo + 'ReqConsultaNFSeRPS>';
end;

procedure TACBrNFSeProviderISSDSF.TratarRetornoConsultaNFSeporRps(
  Response: TNFSeConsultaNFSeporRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  NumNFSe: String;
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

      ProcessarMensagemErros(ANode, Response, 'Erros', 'Erro');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('Sucesso'), tcStr);
        end;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('NFe');
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
        AuxNode := ANode.Childrens.FindAnyNs('ChaveNFe');
        AuxNode := AuxNode.Childrens.FindAnyNs('NumeroNFe');
        NumNFSe := AuxNode.AsString;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

        if Assigned(ANota) then
          ANota.XML := ANode.OuterXml
        else
        begin
          TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
        end;

        SalvarXmlNfse(ANota);
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

procedure TACBrNFSeProviderISSDSF.PrepararConsultaNFSe(
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
    AErro.Descricao := Desc115;
    Exit;
  end;

  if Response.InfConsultaNFSe.DataFinal = 0 then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod116;
    AErro.Descricao := Desc116;
    Exit;
  end;

  if EstaVazio(Response.InfConsultaNFSe.NumeroLote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := Desc111;
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
        NameSpace := NameSpace+ ' xmlns:' + ConfigMsgDados.PrefixoTS + '="' +
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

  Response.XmlEnvio := '<' + Prefixo + 'ReqConsultaNotas' + NameSpace + '>' +
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
//                           '<NotaInicial>' +
//                             Response.InfConsultaNFSe.NumeroIniNFSe +
//                           '</NotaInicial>' +
                           '<NumeroLote>' +
                             Response.InfConsultaNFSe.NumeroLote +
                           '</NumeroLote>' +
                           '<Versao>1</Versao>' +
                         '</Cabecalho>' +
                       '</' + Prefixo + 'ReqConsultaNotas>';
end;

procedure TACBrNFSeProviderISSDSF.TratarRetornoConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  NumNFSe: String;
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

      ProcessarMensagemErros(ANode, Response, 'Erros', 'Erro');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('Sucesso'), tcStr);
        end;
      end;

      ANodeArray := ANode.Childrens.FindAllAnyNs('NFe');
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
        AuxNode := ANode.Childrens.FindAnyNs('ChaveNFe');
        AuxNode := AuxNode.Childrens.FindAnyNs('NumeroNFe');
        NumNFSe := AuxNode.AsString;

        ANota := TACBrNFSeX(FAOwner).NotasFiscais.FindByNFSe(NumNFSe);

        if Assigned(ANota) then
          ANota.XML := ANode.OuterXml
        else
        begin
          TACBrNFSeX(FAOwner).NotasFiscais.LoadFromString(ANode.OuterXml, False);
          ANota := TACBrNFSeX(FAOwner).NotasFiscais.Items[TACBrNFSeX(FAOwner).NotasFiscais.Count-1];
        end;

        SalvarXmlNfse(ANota);
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

procedure TACBrNFSeProviderISSDSF.PrepararCancelaNFSe(
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
    AErro.Descricao := Desc108;
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.CodVerificacao) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod117;
    AErro.Descricao := Desc117;
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
        NameSpace := NameSpace+ ' xmlns:' + ConfigMsgDados.PrefixoTS + '="' +
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

  {
    Tag <transacao>
    Informe se as NFS-e a serem canceladas farão parte de uma mesma transação.

    True - As NFS-e só serão canceladas se não ocorrer nenhum evento de erro
           durante o processamento de todo o lote;

    False - As NFS-e aptas a serem canceladas serão canceladas, mesmo que
            ocorram eventos de erro durante processamento do cancelamento de
            outras NFS-e deste lote.
  }

  Response.XmlEnvio := '<' + Prefixo + 'ReqCancelamentoNFSe' + NameSpace + '>' +
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

procedure TACBrNFSeProviderISSDSF.TratarRetornoCancelaNFSe(
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

      ProcessarMensagemErros(ANode, Response, 'Erros', 'Erro');

      Response.Sucesso := (Response.Erros.Count = 0);

      AuxNode := ANode.Childrens.FindAnyNs('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('Sucesso'), tcStr);
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

{ TACBrNFSeXWebserviceISSDSF }

function TACBrNFSeXWebserviceISSDSF.GetNameSpace: string;
begin
  if FPConfiguracoes.WebServices.AmbienteCodigo = 1 then
    Result := TACBrNFSeX(FPDFeOwner).Provider.ConfigWebServices.Producao.NameSpace
  else
    Result := TACBrNFSeX(FPDFeOwner).Provider.ConfigWebServices.Homologacao.NameSpace;

  Result := 'xmlns:lot="' + Result + '"';
end;

function TACBrNFSeXWebserviceISSDSF.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<lot:enviar>';
  Request := Request + '<mensagemXml>' + AMSG + '</mensagemXml>';
  Request := Request + '</lot:enviar>';

  Result := Executar('', Request,
                     ['enviarReturn', 'ReqEnvioLoteRPS'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceISSDSF.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<lot:enviarSincrono>';
  Request := Request + '<mensagemXml>' + AMSG + '</mensagemXml>';
  Request := Request + '</lot:enviarSincrono>';

  Result := Executar('', Request,
                     ['enviarSincronoReturn', 'RetornoEnvioLoteRPS'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceISSDSF.TesteEnvio(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<lot:testeEnviar>';
  Request := Request + '<mensagemXml>' + AMSG + '</mensagemXml>';
  Request := Request + '</lot:testeEnviar>';

  Result := Executar('', Request,
                     ['testeEnviarReturn', 'RetornoEnvioLoteRPS'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceISSDSF.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<lot:consultarLote>';
  Request := Request + '<mensagemXml>' + AMSG + '</mensagemXml>';
  Request := Request + '</lot:consultarLote>';

  Result := Executar('', Request,
                     ['consultarLoteReturn', 'RetornoConsultaLote'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceISSDSF.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<lot:consultarNFSeRps>';
  Request := Request + '<mensagemXml>' + AMSG + '</mensagemXml>';
  Request := Request + '</lot:consultarNFSeRps>';

  Result := Executar('', Request,
                     ['consultarNFSeRpsReturn', 'RetornoConsultaNFSeRPS'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceISSDSF.ConsultarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<lot:consultarNota>';
  Request := Request + '<mensagemXml>' + AMSG + '</mensagemXml>';
  Request := Request + '</lot:consultarNota>';

  Result := Executar('', Request,
                     ['consultarNotaReturn', 'RetornoConsultaNotas'],
                     [NameSpace]);
end;

function TACBrNFSeXWebserviceISSDSF.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<lot:cancelar>';
  Request := Request + '<mensagemXml>' + AMSG + '</mensagemXml>';
  Request := Request + '</lot:cancelar>';

  Result := Executar('', Request,
                     ['cancelarReturn', 'RetornoCancelamentoNFSe'],
                     [NameSpace]);
end;

end.
