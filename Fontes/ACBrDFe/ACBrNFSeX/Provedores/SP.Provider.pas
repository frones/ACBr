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

unit SP.Provider;

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
  TACBrNFSeXWebserviceSP = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function TesteEnvio(ACabecalho, AMSG: String): string; override;
    function ConsultarSituacao(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSe(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderSP = class (TACBrNFSeProviderProprio)
  protected
    procedure Configuracao; override;

    procedure AssinaturaAdicional(Nota: NotaFiscal);

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
  SP.GravarXml, SP.LerXml;

{ TACBrNFSeProviderSP }

procedure TACBrNFSeProviderSP.AssinaturaAdicional(Nota: NotaFiscal);
var
  sSituacao, sISSRetido, sCPFCNPJTomador, sIndTomador, sTomador,
  sCPFCNPJInter, sIndInter, sISSRetidoInter, sInter, sAssinatura: String;
  xAssinatura: TStringList;
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

    xAssinatura := TStringList.Create;
    try
      xAssinatura.Add(sAssinatura);

      with TACBrNFSeX(FAOwner) do
        NFSe.Assinatura := string(SSL.CalcHash(xAssinatura, dgstSHA1, outBase64, True));
    finally
      xAssinatura.Free;
    end;
  end;
end;

procedure TACBrNFSeProviderSP.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';
    QuebradeLinha := '|';
    ModoEnvio := meLoteAssincrono;

    {italo
    TagRaizNFSe := 'NFe';
    TagRaizRps  := 'RPS';
    }
  end;

  with ConfigAssinar do
  begin
    LoteRps := True;
    ConsultarSituacao := True;
    ConsultarLote := True;
    ConsultarNFSeRps := True;
    ConsultarNFSe := True;
    CancelarNFSe := True;
    LoteGerarNFSe := True;

    IncluirURI := False;

    AssinaturaAdicional := True;
  end;

  SetXmlNameSpace('http://www.prefeitura.sp.gov.br/nfe');

  with ConfigMsgDados do
  begin
    with LoteRps do
    begin
      GerarNSLoteRps := True;
      InfElemento := 'RPS';
      DocElemento := 'PedidoEnvioLoteRPS';
    end;

    with ConsultarSituacao do
    begin
      InfElemento := '';
      DocElemento := 'PedidoInformacoesLote';
    end;

    with ConsultarLote do
    begin
      InfElemento := '';
      DocElemento := 'PedidoConsultaLote';
    end;

    with ConsultarNFSeRps do
    begin
      InfElemento := '';
      DocElemento := 'PedidoConsultaNFe';
    end;

    with ConsultarNFSe do
    begin
      InfElemento := '';
      DocElemento := 'PedidoConsultaNFe';
    end;

    with CancelarNFSe do
    begin
      InfElemento := '';
      DocElemento := 'PedidoCancelamentoNFe';
    end;

    with GerarNFSe do
    begin
      InfElemento := '';
      DocElemento := 'PedidoEnvioRPS';
    end;

    DadosCabecalho := '1';
  end;

  with ConfigSchemas do
  begin
    Teste := 'PedidoEnvioLoteRPS_v01.xsd';
    Recepcionar := 'PedidoEnvioLoteRPS_v01.xsd';
    ConsultarSituacao := 'PedidoInformacoesLote_v01.xsd';
    ConsultarLote := 'PedidoConsultaLote_v01.xsd';
    ConsultarNFSeRps := 'PedidoConsultaNFe_v01.xsd';
    ConsultarNFSe := 'PedidoConsultaNFe_v01.xsd';
    CancelarNFSe := 'PedidoCancelamentoNFe_v01.xsd';
    GerarNFSe := 'PedidoEnvioRPS_v01.xsd';
  end;
end;

function TACBrNFSeProviderSP.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_SP.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSP.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_SP.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderSP.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  if FAOwner.Configuracoes.WebServices.AmbienteCodigo = 2 then
  begin
   with ConfigWebServices.Homologacao do
    begin
      case AMetodo of
        tmRecepcionar: URL := Recepcionar;
        tmGerar: URL := GerarNFSe;
        tmTeste: URL := TesteEnvio;
        tmConsultarSituacao: URL := ConsultarSituacao;
        tmConsultarLote: URL := ConsultarLote;
        tmConsultarNFSePorRps: URL := ConsultarNFSeRps;
        tmConsultarNFSe: URL := ConsultarNFSe;
        tmCancelarNFSe: URL := CancelarNFSe;
      else
        URL := '';
      end;
    end;
  end
  else
  begin
    with ConfigWebServices.Producao do
    begin
      case AMetodo of
        tmRecepcionar: URL := Recepcionar;
        tmGerar: URL := GerarNFSe;
        tmTeste: URL := TesteEnvio;
        tmConsultarSituacao: URL := ConsultarSituacao;
        tmConsultarLote: URL := ConsultarLote;
        tmConsultarNFSePorRps: URL := ConsultarNFSeRps;
        tmConsultarNFSe: URL := ConsultarNFSe;
        tmCancelarNFSe: URL := CancelarNFSe;
      else
        URL := '';
      end;
    end;
  end;

  if URL <> '' then
    Result := TACBrNFSeXWebserviceSP.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderSP.ProcessarMensagemErros(
  const RootNode: TACBrXmlNode; const Response: TNFSeWebserviceResponse;
  AListTag, AMessageTag: string);
var
  I: Integer;
  ANodeArray: TACBrXmlNodeArray;
  AErro: TNFSeEventoCollectionItem;
begin
  ANodeArray := RootNode.Childrens.FindAllAnyNs(AMessageTag);

  if not Assigned(ANodeArray) then Exit;

  for I := Low(ANodeArray) to High(ANodeArray) do
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Codigo'), tcStr);
    AErro.Descricao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('Descricao'), tcStr);
    AErro.Correcao := '';
  end;
end;

procedure TACBrNFSeProviderSP.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Nota: NotaFiscal;
  IdAttr, NameSpace, ListaRps, xRps,
  TagEnvio, xCabecalho, xDataI, xDataF: string;
  I: Integer;
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

  for I := 0 to TACBrNFSeX(FAOwner).NotasFiscais.Count -1 do
  begin
    Nota := TACBrNFSeX(FAOwner).NotasFiscais.Items[I];

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

    vTotServicos := vTotServicos + Nota.NFSe.Servico.Valores.ValorServicos;
    vTotDeducoes := vTotDeducoes + Nota.NFSe.Servico.Valores.ValorDeducoes;

    xRps := RemoverDeclaracaoXML(Nota.XMLOriginal);

    xRps := '<RPS xmlns="">' + SeparaDados(xRps, 'RPS') + '</RPS>';

    ListaRps := ListaRps + xRps;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  case Response.ModoEnvio of
    meUnitario:
    begin
      TagEnvio := 'PedidoEnvioRPS';

      xCabecalho := '<Cabecalho xmlns="" Versao="1">' +
                      '<CPFCNPJRemetente>' +
                        '<CNPJ>' +
                          OnlyNumber(Emitente.CNPJ) +
                        '</CNPJ>' +
                      '</CPFCNPJRemetente>' +
                    '</Cabecalho>';

      if EstaVazio(ConfigMsgDados.GerarNFSe.xmlns) then
        NameSpace := ''
      else
        NameSpace := ' xmlns="' + ConfigMsgDados.GerarNFSe.xmlns + '"';
    end;
  else
    begin
      TagEnvio := 'PedidoEnvioLoteRPS';

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
      xCabecalho := '<Cabecalho xmlns="" Versao="1">' +
                      '<CPFCNPJRemetente>' +
                        '<CNPJ>' +
                          OnlyNumber(Emitente.CNPJ) +
                        '</CNPJ>' +
                      '</CPFCNPJRemetente>' +
                      '<transacao>false</transacao>' +
                      '<dtInicio>' + xDataI + '</dtInicio>' +
                      '<dtFim>' + xDataF + '</dtFim>' +
                      '<QtdRPS>' +
                         IntToStr(TACBrNFSeX(FAOwner).NotasFiscais.Count) +
                      '</QtdRPS>' +
                      '<ValorTotalServicos>' +
                         FloatToStr(vTotServicos) +
                      '</ValorTotalServicos>' +
                      '<ValorTotalDeducoes>' +
                         FloatToStr(vTotDeducoes) +
                      '</ValorTotalDeducoes>' +
                    '</Cabecalho>';

      if EstaVazio(ConfigMsgDados.LoteRps.xmlns) then
        NameSpace := ''
      else
        NameSpace := ' xmlns="' + ConfigMsgDados.LoteRps.xmlns + '"';
    end;
  end;

  Response.XmlEnvio := '<' + TagEnvio + NameSpace + '>' +
                          xCabecalho +
                          ListaRps +
                       '</' + TagEnvio + '>';
end;

procedure TACBrNFSeProviderSP.TratarRetornoEmitir(Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode: TACBrXmlNode;
  AuxNode, AuxNodeCPFCNPJ, AuxNodeChave: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.Find('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.Find('Sucesso'), tcStr);

          AuxNode := AuxNode.Childrens.Find('InformacoesLote');

          if AuxNode <> nil then
          begin
            with InformacoesLote do
            begin
              NumeroLote := ProcessarConteudoXml(AuxNode.Childrens.Find('NumeroLote'), tcStr);
              InscricaoPrestador := ProcessarConteudoXml(AuxNode.Childrens.Find('InscricaoPrestador'), tcStr);

              AuxNodeCPFCNPJ := AuxNode.Childrens.Find('CPFCNPJRemetente');

              if AuxNodeCPFCNPJ <> nil then
              begin
                CPFCNPJRemetente := ProcessarConteudoXml(AuxNodeCPFCNPJ.Childrens.Find('CNPJ'), tcStr);

                if CPFCNPJRemetente = '' then
                  CPFCNPJRemetente := ProcessarConteudoXml(AuxNodeCPFCNPJ.Childrens.Find('CPF'), tcStr);
              end;

              DataEnvioLote := ProcessarConteudoXml(AuxNode.Childrens.Find('DataEnvioLote'), tcDatHor);
              QtdNotasProcessadas := ProcessarConteudoXml(AuxNode.Childrens.Find('QtdNotasProcessadas'), tcInt);
              TempoProcessamento := ProcessarConteudoXml(AuxNode.Childrens.Find('TempoProcessamento'), tcInt);
              ValorTotalServico := ProcessarConteudoXml(AuxNode.Childrens.Find('ValorTotalServico'), tcDe2);
            end;
          end;
        end;
      end;

      AuxNode := ANode.Childrens.Find('ChaveNFeRPS');

      if AuxNode <> nil then
      begin
        AuxNodeChave := AuxNode.Childrens.Find('ChaveRPS');

        if (AuxNodeChave <> nil) then
        begin
          with Response.InfRetorno.ChaveNFeRPS do
          begin
            InscricaoPrestador := ProcessarConteudoXml(AuxNodeChave.Childrens.Find('InscricaoPrestador'), tcStr);
            SerieRPS := ProcessarConteudoXml(AuxNodeChave.Childrens.Find('SerieRPS'), tcStr);
            NumeroRPS := ProcessarConteudoXml(AuxNodeChave.Childrens.Find('NumeroRPS'), tcStr);
          end;
        end;

        AuxNodeChave := AuxNode.Childrens.Find('ChaveNFe');

        if (AuxNodeChave <> nil) then
        begin
          with Response.InfRetorno.ChaveNFeRPS do
          begin
            InscricaoPrestador := ProcessarConteudoXml(AuxNodeChave.Childrens.Find('InscricaoPrestador'), tcStr);
            Numero := ProcessarConteudoXml(AuxNodeChave.Childrens.Find('NumeroNFe'), tcStr);
            CodigoVerificacao := ProcessarConteudoXml(AuxNodeChave.Childrens.Find('CodigoVerificacao'), tcStr);
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

procedure TACBrNFSeProviderSP.PrepararConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace: string;
begin
  if EstaVazio(Response.Lote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := Desc111;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(ConfigMsgDados.ConsultarSituacao.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarSituacao.xmlns + '"';

  Response.XmlEnvio := '<PedidoInformacoesLote' + NameSpace + '>' +
                         '<Cabecalho xmlns="" Versao="1">' +
                           '<CPFCNPJRemetente>' +
                             '<CNPJ>' +
                               OnlyNumber(Emitente.CNPJ) +
                             '</CNPJ>' +
                           '</CPFCNPJRemetente>' +
                           '<NumeroLote>' +
                             Response.Lote +
                           '</NumeroLote>' +
                           '<InscricaoPrestador>' +
                             OnlyNumber(Emitente.InscMun) +
                           '</InscricaoPrestador>' +
                         '</Cabecalho>' +
                       '</PedidoInformacoesLote>';
end;

procedure TACBrNFSeProviderSP.TratarRetornoConsultaSituacao(
  Response: TNFSeConsultaSituacaoResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  ANode, AuxNode, AuxNodeCPFCNPJ: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.Find('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.Find('Sucesso'), tcStr);

          AuxNode := AuxNode.Childrens.Find('InformacoesLote');

          if AuxNode <> nil then
          begin
            with InformacoesLote do
            begin
              NumeroLote := ProcessarConteudoXml(AuxNode.Childrens.Find('NumeroLote'), tcStr);
              InscricaoPrestador := ProcessarConteudoXml(AuxNode.Childrens.Find('InscricaoPrestador'), tcStr);

              AuxNodeCPFCNPJ := AuxNode.Childrens.Find('CPFCNPJRemetente');

              if AuxNodeCPFCNPJ <> nil then
              begin
                CPFCNPJRemetente := ProcessarConteudoXml(AuxNodeCPFCNPJ.Childrens.Find('CNPJ'), tcStr);

                if CPFCNPJRemetente = '' then
                  CPFCNPJRemetente := ProcessarConteudoXml(AuxNodeCPFCNPJ.Childrens.Find('CPF'), tcStr);
              end;

              DataEnvioLote := ProcessarConteudoXml(AuxNode.Childrens.Find('DataEnvioLote'), tcDatHor);
              QtdNotasProcessadas := ProcessarConteudoXml(AuxNode.Childrens.Find('QtdNotasProcessadas'), tcInt);
              TempoProcessamento := ProcessarConteudoXml(AuxNode.Childrens.Find('TempoProcessamento'), tcInt);
              ValorTotalServico := ProcessarConteudoXml(AuxNode.Childrens.Find('ValorTotalServico'), tcDe2);
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

procedure TACBrNFSeProviderSP.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace: string;
begin
  if EstaVazio(Response.Lote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := Desc111;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(ConfigMsgDados.ConsultarLote.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarLote.xmlns + '"';

  Response.XmlEnvio := '<PedidoConsultaLote' + NameSpace + '>' +
                         '<Cabecalho xmlns="" Versao="1">' +
                           '<CPFCNPJRemetente>' +
                             '<CNPJ>' +
                               OnlyNumber(Emitente.CNPJ) +
                             '</CNPJ>' +
                           '</CPFCNPJRemetente>' +
                           '<NumeroLote>' +
                             Response.Lote +
                           '</NumeroLote>' +
                         '</Cabecalho>' +
                       '</PedidoConsultaLote>';
end;

procedure TACBrNFSeProviderSP.TratarRetornoConsultaLoteRps(
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

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.Find('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.Find('Sucesso'), tcStr);
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

procedure TACBrNFSeProviderSP.PrepararConsultaNFSeporRps(
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

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(ConfigMsgDados.ConsultarNFSeRps.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarNFSeRps.xmlns + '"';

  Response.XmlEnvio := '<PedidoConsultaNFe' + NameSpace + '>' +
                         '<Cabecalho xmlns="" Versao="1">' +
                           '<CPFCNPJRemetente>' +
                             '<CNPJ>' +
                               OnlyNumber(Emitente.CNPJ) +
                             '</CNPJ>' +
                           '</CPFCNPJRemetente>' +
                         '</Cabecalho>' +
                         '<Detalhe xmlns="">' +
                           '<ChaveRPS>' +
                             '<InscricaoPrestador>' +
                               OnlyNumber(Emitente.InscMun) +
                             '</InscricaoPrestador>' +
                             '<SerieRPS>' + Response.Serie + '</SerieRPS>' +
                             '<NumeroRPS>' + Response.NumRPS + '</NumeroRPS>' +
                           '</ChaveRPS>' +
                         '</Detalhe>' +
                       '</PedidoConsultaNFe>';
end;

procedure TACBrNFSeProviderSP.TratarRetornoConsultaNFSeporRps(
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

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.Find('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.Find('Sucesso'), tcStr);
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

procedure TACBrNFSeProviderSP.PrepararConsultaNFSe(
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

  if EstaVazio(ConfigMsgDados.ConsultarNFSe.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.ConsultarNFSe.xmlns + '"';

  Response.XmlEnvio := '<PedidoConsultaNFe' + NameSpace + '>' +
                         '<Cabecalho xmlns="" Versao="1">' +
                           '<CPFCNPJRemetente>' +
                             '<CNPJ>' +
                               OnlyNumber(Emitente.CNPJ) +
                             '</CNPJ>' +
                           '</CPFCNPJRemetente>' +
                         '</Cabecalho>' +
                         '<Detalhe xmlns="">' +
                           '<ChaveNFe>' +
                             '<InscricaoPrestador>' +
                               OnlyNumber(Emitente.InscMun) +
                             '</InscricaoPrestador>' +
                             '<NumeroNFe>' +
                                Response.InfConsultaNFSe.NumeroIniNFSe +
                             '</NumeroNFe>' +
                           '</ChaveNFe>' +
                         '</Detalhe>' +
                       '</PedidoConsultaNFe>';
end;

procedure TACBrNFSeProviderSP.TratarRetornoConsultaNFSe(
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

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.Find('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.Find('Sucesso'), tcStr);
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

procedure TACBrNFSeProviderSP.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  NameSpace, sAssinatura: string;
begin
  if EstaVazio(Response.InfCancelamento.NumeroNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod108;
    AErro.Descricao := Desc108;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if EstaVazio(ConfigMsgDados.CancelarNFSe.xmlns) then
    NameSpace := ''
  else
    NameSpace := ' xmlns="' + ConfigMsgDados.CancelarNFSe.xmlns + '"';

  {
    Tag <transacao>
    Informe se as NFS-e a serem canceladas farão parte de uma mesma transação.

    True - As NFS-e só serão canceladas se não ocorrer nenhum evento de erro
           durante o processamento de todo o lote;

    False - As NFS-e aptas a serem canceladas serão canceladas, mesmo que
            ocorram eventos de erro durante processamento do cancelamento de
            outras NFS-e deste lote.
  }

  sAssinatura := Poem_Zeros(Emitente.InscMun, 8) +
                 Poem_Zeros(Response.InfCancelamento.NumeroNFSe, 12);

  sAssinatura := string(TACBrNFSeX(FAOwner).SSL.CalcHash(AnsiString(sAssinatura),
                                                    dgstSHA1, outBase64, True));

  Response.XmlEnvio := '<PedidoCancelamentoNFe' + NameSpace + '>' +
                         '<Cabecalho xmlns="" Versao="1">' +
                           '<CPFCNPJRemetente>' +
                             '<CNPJ>' +
                               OnlyNumber(Emitente.CNPJ) +
                             '</CNPJ>' +
                           '</CPFCNPJRemetente>' +
                           '<transacao>false</transacao>' +
                         '</Cabecalho>' +
                         '<Detalhe xmlns="">' +
                           '<ChaveNFe>' +
                             '<InscricaoPrestador>' +
                               OnlyNumber(Emitente.InscMun) +
                             '</InscricaoPrestador>' +
                             '<NumeroNFe>' +
                                Response.InfCancelamento.NumeroNFSe +
                             '</NumeroNFe>' +
                           '</ChaveNFe>' +
                           '<AssinaturaCancelamento>' +
                              sAssinatura +
                           '</AssinaturaCancelamento>' +
                         '</Detalhe>' +
                       '</PedidoCancelamentoNFe>';
end;

procedure TACBrNFSeProviderSP.TratarRetornoCancelaNFSe(
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

      ProcessarMensagemErros(Document.Root, Response);

      Response.Sucesso := (Response.Erros.Count = 0);

      ANode := Document.Root;

      AuxNode := ANode.Childrens.Find('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.Find('Sucesso'), tcStr);
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

function TACBrNFSeXWebserviceSP.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:EnvioLoteRPSRequest>';
  Request := Request + '<nfe:VersaoSchema>' + ACabecalho + '</nfe:VersaoSchema>';
  Request := Request + '<nfe:MensagemXML>' + XmlToStr(AMSG) + '</nfe:MensagemXML>';
  Request := Request + '</nfe:EnvioLoteRPSRequest>';

  Result := Executar('http://www.prefeitura.sp.gov.br/nfe/ws/envioLoteRPS', Request,
                     ['RetornoXML', 'RetornoEnvioLoteRPS'],
                     ['xmlns:nfe="http://www.prefeitura.sp.gov.br/nfe"']);
end;

function TACBrNFSeXWebserviceSP.GerarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:EnvioRPSRequest>';
  Request := Request + '<nfe:VersaoSchema>' + ACabecalho + '</nfe:VersaoSchema>';
  Request := Request + '<nfe:MensagemXML>' + XmlToStr(AMSG) + '</nfe:MensagemXML>';
  Request := Request + '</nfe:EnvioRPSRequest>';

  Result := Executar('http://www.prefeitura.sp.gov.br/nfe/ws/envioRPS', Request,
                     ['RetornoXML', 'RetornoEnvioRPS'],
                     ['xmlns:nfe="http://www.prefeitura.sp.gov.br/nfe"']);
end;

function TACBrNFSeXWebserviceSP.TesteEnvio(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:TesteEnvioLoteRPSRequest>';
  Request := Request + '<nfe:VersaoSchema>' + ACabecalho + '</nfe:VersaoSchema>';
  Request := Request + '<nfe:MensagemXML>' + XmlToStr(AMSG) + '</nfe:MensagemXML>';
  Request := Request + '</nfe:TesteEnvioLoteRPSRequest>';

  Result := Executar('http://www.prefeitura.sp.gov.br/nfe/ws/testeenvio', Request,
                     ['RetornoXML', 'RetornoEnvioRPS'],
                     ['xmlns:nfe="http://www.prefeitura.sp.gov.br/nfe"']);
end;

function TACBrNFSeXWebserviceSP.ConsultarSituacao(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:ConsultaInformacoesLoteRequest>';
  Request := Request + '<nfe:VersaoSchema>' + ACabecalho + '</nfe:VersaoSchema>';
  Request := Request + '<nfe:MensagemXML>' + XmlToStr(AMSG) + '</nfe:MensagemXML>';
  Request := Request + '</nfe:ConsultaInformacoesLoteRequest>';

  Result := Executar('http://www.prefeitura.sp.gov.br/nfe/ws/consultaInformacoesLote', Request,
                     ['RetornoXML', 'RetornoInformacoesLote'],
                     ['xmlns:nfe="http://www.prefeitura.sp.gov.br/nfe"']);
end;

function TACBrNFSeXWebserviceSP.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:ConsultaLoteRequest>';
  Request := Request + '<nfe:VersaoSchema>' + ACabecalho + '</nfe:VersaoSchema>';
  Request := Request + '<nfe:MensagemXML>' + XmlToStr(AMSG) + '</nfe:MensagemXML>';
  Request := Request + '</nfe:ConsultaLoteRequest>';

  Result := Executar('http://www.prefeitura.sp.gov.br/nfe/ws/consultaLote', Request,
                     ['RetornoXML', 'RetornoConsulta'],
                     ['xmlns:nfe="http://www.prefeitura.sp.gov.br/nfe"']);
end;

function TACBrNFSeXWebserviceSP.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:ConsultaNFeRequest>';
  Request := Request + '<nfe:VersaoSchema>' + ACabecalho + '</nfe:VersaoSchema>';
  Request := Request + '<nfe:MensagemXML>' + XmlToStr(AMSG) + '</nfe:MensagemXML>';
  Request := Request + '</nfe:ConsultaNFeRequest>';

  Result := Executar('http://www.prefeitura.sp.gov.br/nfe/ws/consultaNFe', Request,
                     ['RetornoXML', 'RetornoConsulta'],
                     ['xmlns:nfe="http://www.prefeitura.sp.gov.br/nfe"']);
end;

function TACBrNFSeXWebserviceSP.ConsultarNFSe(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:ConsultaNFeRequest>';
  Request := Request + '<nfe:VersaoSchema>' + ACabecalho + '</nfe:VersaoSchema>';
  Request := Request + '<nfe:MensagemXML>' + XmlToStr(AMSG) + '</nfe:MensagemXML>';
  Request := Request + '</nfe:ConsultaNFeRequest>';

  Result := Executar('http://www.prefeitura.sp.gov.br/nfe/ws/consultaNFe', Request,
                     ['RetornoXML', 'RetornoConsulta'],
                     ['xmlns:nfe="http://www.prefeitura.sp.gov.br/nfe"']);
end;

function TACBrNFSeXWebserviceSP.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<nfe:CancelamentoNFeRequest>';
  Request := Request + '<nfe:VersaoSchema>' + ACabecalho + '</nfe:VersaoSchema>';
  Request := Request + '<nfe:MensagemXML>' + XmlToStr(AMSG) + '</nfe:MensagemXML>';
  Request := Request + '</nfe:CancelamentoNFeRequest>';

  Result := Executar('http://www.prefeitura.sp.gov.br/nfe/ws/cancelamentoNFe', Request,
                     ['RetornoXML', 'RetornoCancelamentoNFe'],
                     ['xmlns:nfe="http://www.prefeitura.sp.gov.br/nfe"']);
end;

end.
