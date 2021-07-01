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

unit Infisc.Provider;

interface

uses
  SysUtils, Classes,
  ACBrXmlBase, ACBrXmlDocument, ACBrNFSeXClass, ACBrNFSeXConversao,
  ACBrNFSeXGravarXml, ACBrNFSeXLerXml,
  ACBrNFSeXProviderProprio, ACBrNFSeXProviderABRASFv2,
  ACBrNFSeXWebserviceBase, ACBrNFSeXWebservicesResponse;

type

  TACBrNFSeXWebserviceInfisc = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderInfiscv100 = class (TACBrNFSeProviderProprio)
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

  TACBrNFSeProviderInfiscv110 = class (TACBrNFSeProviderInfiscv100)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
  end;

  TACBrNFSeXWebserviceInfiscv2 = class(TACBrNFSeXWebserviceSoap11)
  public
    function Recepcionar(ACabecalho, AMSG: String): string; override;
    function RecepcionarSincrono(ACabecalho, AMSG: String): string; override;
    function GerarNFSe(ACabecalho, AMSG: String): string; override;
    function ConsultarLote(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorRps(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSePorFaixa(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoPrestado(ACabecalho, AMSG: String): string; override;
    function ConsultarNFSeServicoTomado(ACabecalho, AMSG: String): string; override;
    function Cancelar(ACabecalho, AMSG: String): string; override;
    function SubstituirNFSe(ACabecalho, AMSG: String): string; override;

  end;

  TACBrNFSeProviderInfiscv2 = class (TACBrNFSeProviderABRASFv2)
  protected
    procedure Configuracao; override;

    function CriarGeradorXml(const ANFSe: TNFSe): TNFSeWClass; override;
    function CriarLeitorXml(const ANFSe: TNFSe): TNFSeRClass; override;
    function CriarServiceClient(const AMetodo: TMetodo): TACBrNFSeXWebservice; override;

    procedure ValidarSchema(Response: TNFSeWebserviceResponse; aMetodo: TMetodo); override;
  end;

implementation

uses
  ACBrUtil, ACBrDFeException,
  ACBrNFSeX, ACBrNFSeXConfiguracoes, ACBrNFSeXConsts,
  ACBrNFSeXNotasFiscais, Infisc.GravarXml, Infisc.LerXml;

{ TACBrNFSeProviderInfisc }

procedure TACBrNFSeProviderInfiscv100.Configuracao;
begin
  inherited Configuracao;

  with ConfigGeral do
  begin
    Identificador := '';
    ModoEnvio := meLoteAssincrono;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '1.00';
    VersaoAtrib := '1.0';
    AtribVerLote := 'versao';
  end;

  SetXmlNameSpace('http://ws.pc.gif.com.br/');
end;

function TACBrNFSeProviderInfiscv100.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Infiscv100.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderInfiscv100.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Infisc.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderInfiscv100.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceInfisc.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderInfiscv100.PrepararEmitir(Response: TNFSeEmiteResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Nota: NotaFiscal;
  IdAttr, Versao, ListaRps, xRps: string;
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

    xRps := '<NFS-e>' + SeparaDados(xRps, 'NFS-e') + '</NFS-e>';

    ListaRps := ListaRps + xRps;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if ConfigWebServices.AtribVerLote <> '' then
    Versao := ' ' + ConfigWebServices.AtribVerLote + '="' +
              ConfigWebServices.VersaoAtrib + '"'
  else
    Versao := '';

  Response.XmlEnvio := '<envioLote' + Versao + '>' +
                         '<CNPJ>' +
                            OnlyNumber(Emitente.CNPJ) +
                          '</CNPJ>' +
                         '<dhTrans>' +
                            FormatDateTime('yyyy-mm-dd hh:mm:ss', Now) +
                          '</dhTrans>' +
                          ListaRps +
                       '</envioLote>';
end;

procedure TACBrNFSeProviderInfiscv100.TratarRetornoEmitir(
  Response: TNFSeEmiteResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
//  ANode: TACBrXmlNode;
//  AuxNode, AuxNodeCPFCNPJ, AuxNodeChave: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response, '', 'mot');

      Response.Sucesso := (Response.Erros.Count = 0);

      {
      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('Sucesso'), tcStr);

          AuxNode := AuxNode.Childrens.FindAnyNs('InformacoesLote');

          if AuxNode <> nil then
          begin
            with InformacoesLote do
            begin
              NumeroLote := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('NumeroLote'), tcStr);
              InscricaoPrestador := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('InscricaoPrestador'), tcStr);

              AuxNodeCPFCNPJ := AuxNode.Childrens.FindAnyNs('CPFCNPJRemetente');

              if AuxNodeCPFCNPJ <> nil then
              begin
                CPFCNPJRemetente := ProcessarConteudoXml(AuxNodeCPFCNPJ.Childrens.FindAnyNs('CNPJ'), tcStr);

                if CPFCNPJRemetente = '' then
                  CPFCNPJRemetente := ProcessarConteudoXml(AuxNodeCPFCNPJ.Childrens.FindAnyNs('CPF'), tcStr);
              end;

              DataEnvioLote := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('DataEnvioLote'), tcDatHor);
              QtdNotasProcessadas := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('QtdNotasProcessadas'), tcInt);
              TempoProcessamento := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('TempoProcessamento'), tcInt);
              ValorTotalServico := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('ValorTotalServico'), tcDe2);
            end;
          end;
        end;
      end;

      AuxNode := ANode.Childrens.FindAnyNs('ChaveNFeRPS');

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
      }
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

procedure TACBrNFSeProviderInfiscv100.PrepararConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Versao: string;
begin
  if EstaVazio(Response.Lote) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod111;
    AErro.Descricao := Desc111;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if ConfigWebServices.AtribVerLote <> '' then
    Versao := ' ' + ConfigWebServices.AtribVerLote + '="' +
              ConfigWebServices.VersaoAtrib + '"'
  else
    Versao := '';

  Response.XmlEnvio := '<pedidoStatusLote' + Versao + '>' +
                         '<CNPJ>' +
                            OnlyNumber(Emitente.CNPJ) +
                          '</CNPJ>' +
                         '<cLote>' +
                            Response.Lote +
                          '</cLote>' +
                       '</pedidoStatusLote>';
end;

procedure TACBrNFSeProviderInfiscv100.TratarRetornoConsultaLoteRps(
  Response: TNFSeConsultaLoteRpsResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  {
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  NumRps: String;
  ANota: NotaFiscal;
  }
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

      ProcessarMensagemErros(Document.Root, Response, '', 'mot');

      Response.Sucesso := (Response.Erros.Count = 0);
      {
      ANode := Document.Root;

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
      }
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

procedure TACBrNFSeProviderInfiscv100.PrepararConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Versao: string;
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

  if ConfigWebServices.AtribVerLote <> '' then
    Versao := ' ' + ConfigWebServices.AtribVerLote + '="' +
              ConfigWebServices.VersaoAtrib + '"'
  else
    Versao := '';

  Response.XmlEnvio := '<pedidoLoteNFSe' + Versao + '>' +
                         '<CNPJ>' +
                            OnlyNumber(Emitente.CNPJ) +
                          '</CNPJ>' +
                         '<notaInicial>' +
                            Response.InfConsultaNFSe.NumeroIniNFSe +
                          '</notaInicial>' +
                         '<notaFinal>' +
                            Response.InfConsultaNFSe.NumeroFinNFSe +
                          '</notaFinal>' +
                       '</pedidoLoteNFSe>';
end;

procedure TACBrNFSeProviderInfiscv100.TratarRetornoConsultaNFSe(
  Response: TNFSeConsultaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
  {
  ANode, AuxNode: TACBrXmlNode;
  ANodeArray: TACBrXmlNodeArray;
  i: Integer;
  NumRps: String;
  ANota: NotaFiscal;
  }
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

      ProcessarMensagemErros(Document.Root, Response, '', 'mot');

      Response.Sucesso := (Response.Erros.Count = 0);
      {
      ANode := Document.Root;

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
      }
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

procedure TACBrNFSeProviderInfiscv100.PrepararCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  AErro: TNFSeEventoCollectionItem;
  Emitente: TEmitenteConfNFSe;
  Versao: string;
begin
  if EstaVazio(Response.InfCancelamento.ChaveNFSe) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod118;
    AErro.Descricao := Desc118;
    Exit;
  end;

  if EstaVazio(Response.InfCancelamento.CodCancelamento) then
  begin
    AErro := Response.Erros.New;
    AErro.Codigo := Cod109;
    AErro.Descricao := Desc109;
    Exit;
  end;

  Emitente := TACBrNFSeX(FAOwner).Configuracoes.Geral.Emitente;

  if ConfigWebServices.AtribVerLote <> '' then
    Versao := ' ' + ConfigWebServices.AtribVerLote + '="' +
              ConfigWebServices.VersaoAtrib + '"'
  else
    Versao := '';

  Response.XmlEnvio := '<pedCancelaNFSe' + Versao + '>' +
                         '<CNPJ>' +
                            OnlyNumber(Emitente.CNPJ) +
                          '</CNPJ>' +
                         '<chvAcessoNFS-e>' +
                            Response.InfCancelamento.ChaveNFSe +
                          '</chvAcessoNFS-e>' +
                         '<motivo>' +
                            Response.InfCancelamento.CodCancelamento +
                          '</motivo>' +
                       '</pedCancelaNFSe>';
end;

procedure TACBrNFSeProviderInfiscv100.TratarRetornoCancelaNFSe(
  Response: TNFSeCancelaNFSeResponse);
var
  Document: TACBrXmlDocument;
  AErro: TNFSeEventoCollectionItem;
//  ANode, AuxNode: TACBrXmlNode;
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

      ProcessarMensagemErros(Document.Root, Response, '', 'mot');

      Response.Sucesso := (Response.Erros.Count = 0);
      {
      ANode := Document.Root;

      AuxNode := ANode.Childrens.FindAnyNs('Cabecalho');

      if AuxNode <> nil then
      begin
        with Response.InfRetorno do
        begin
          Sucesso := ProcessarConteudoXml(AuxNode.Childrens.FindAnyNs('Sucesso'), tcStr);
        end;
      end;
      }
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

procedure TACBrNFSeProviderInfiscv100.ProcessarMensagemErros(
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
    AErro.Codigo := '';
    AErro.Descricao := ProcessarConteudoXml(ANodeArray[I].Childrens.FindAnyNs('mot'), tcStr);

    if AErro.Descricao = '' then
      AErro.Descricao := ANodeArray[I].AsString;

    AErro.Correcao := '';
  end;
end;

{ TACBrNFSeProviderInfiscv110 }

procedure TACBrNFSeProviderInfiscv110.Configuracao;
begin
  inherited Configuracao;

  with ConfigAssinar do
  begin
    LoteRps := True;
    IncluirURI := False;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '1.10';
    VersaoAtrib := '1.0';
    AtribVerLote := 'versao';
  end;

  with ConfigMsgDados do
  begin
    with LoteRps do
    begin
      InfElemento := 'envioLote';
      DocElemento := 'envioLote';
    end;
  end;
end;

function TACBrNFSeProviderInfiscv110.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Infiscv110.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderInfiscv110.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Infisc.Create(Self);
  Result.NFSe := ANFSe;
end;

{ TACBrNFSeProviderInfiscv2 }

procedure TACBrNFSeProviderInfiscv2.Configuracao;
begin
  inherited Configuracao;

  with ConfigAssinar do
  begin
    Rps := True;
    LoteRps := True;
    ConsultarLote := True;
    ConsultarNFSeRps := True;
    ConsultarNFSePorFaixa := True;
    ConsultarNFSeServicoPrestado := True;
    ConsultarNFSeServicoTomado := True;
    CancelarNFSe := True;
  end;

  with ConfigWebServices do
  begin
    VersaoDados := '2.01';
    VersaoAtrib := '2.01';
  end;

  with ConfigMsgDados do
  begin
    DadosCabecalho := '<cabecalho versao="2.01">' +
                        '<versaoDados>2.01</versaoDados>' +
                      '</cabecalho>';
  end;
end;

function TACBrNFSeProviderInfiscv2.CriarGeradorXml(
  const ANFSe: TNFSe): TNFSeWClass;
begin
  Result := TNFSeW_Infiscv2.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderInfiscv2.CriarLeitorXml(
  const ANFSe: TNFSe): TNFSeRClass;
begin
  Result := TNFSeR_Infiscv2.Create(Self);
  Result.NFSe := ANFSe;
end;

function TACBrNFSeProviderInfiscv2.CriarServiceClient(
  const AMetodo: TMetodo): TACBrNFSeXWebservice;
var
  URL: string;
begin
  URL := GetWebServiceURL(AMetodo);

  if URL <> '' then
    Result := TACBrNFSeXWebserviceInfiscv2.Create(FAOwner, AMetodo, URL)
  else
    raise EACBrDFeException.Create(ERR_NAO_IMP);
end;

procedure TACBrNFSeProviderInfiscv2.ValidarSchema(
  Response: TNFSeWebserviceResponse; aMetodo: TMetodo);
begin
  inherited ValidarSchema(Response, aMetodo);

  Response.XmlEnvio := StringReplace(Response.XmlEnvio,
         ' xmlns="http://www.abrasf.org.br/nfse.xsd"', '', [rfReplaceAll]);
end;

{ TACBrNFSeXWebserviceInfisc }

function TACBrNFSeXWebserviceInfisc.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<enviarLoteNotasRequest>';
  Request := Request + AMSG;
  Request := Request + '</enviarLoteNotasRequest>';

  Result := Executar('', Request, ['confirmaLote'],
                     ['xmlns="http://ws.pc.gif.com.br/"']);
end;

function TACBrNFSeXWebserviceInfisc.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<obterCriticaLoteRequest>';
  Request := Request + AMSG;
  Request := Request + '</obterCriticaLoteRequest>';

  Result := Executar('', Request, ['resultadoLote'],
                     ['xmlns="http://ws.pc.gif.com.br/"']);
end;

function TACBrNFSeXWebserviceInfisc.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<obterLoteNotaFiscalRequest>';
  Request := Request + AMSG;
  Request := Request + '</obterLoteNotaFiscalRequest>';

  Result := Executar('', Request, ['resPedidoLoteNFSe'],
                     ['xmlns="http://ws.pc.gif.com.br/"']);
end;

function TACBrNFSeXWebserviceInfisc.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<cancelarNotaFiscalRequest>';
  Request := Request + AMSG;
  Request := Request + '</cancelarNotaFiscalRequest>';

  Result := Executar('', Request, ['resCancelaNFSe'],
                     ['xmlns="http://ws.pc.gif.com.br/"']);
end;

{ TACBrNFSeXWebserviceInfiscv2 }

function TACBrNFSeXWebserviceInfiscv2.Recepcionar(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<RecepcionarLoteRps>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</RecepcionarLoteRps>';

  Result := Executar('', Request, [''], ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfiscv2.RecepcionarSincrono(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<RecepcionarLoteRpsSincrono>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</RecepcionarLoteRpsSincrono>';

  Result := Executar('', Request, ['EnviarLoteRpsSincronoResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfiscv2.GerarNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<GerarNfse>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</GerarNfse>';

  Result := Executar('', Request, ['GerarNfseResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfiscv2.ConsultarLote(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarLoteRps>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</ConsultarLoteRps>';

  Result := Executar('', Request, ['ConsultarLoteRpsResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfiscv2.ConsultarNFSePorFaixa(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  {
    O WebService esta retornando a tag de fechamento da Resposta sem o caracter "/"
  }
  FPMsgOrig := AMSG;

  Request := '<ConsultarNfseFaixa>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</ConsultarNfseFaixa>';

  Result := Executar('', Request, ['ConsultarNfseFaixaResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfiscv2.ConsultarNFSePorRps(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<ConsultarNfsePorRps>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</ConsultarNfsePorRps>';

  Result := Executar('', Request, ['ConsultarNfseRpsResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfiscv2.ConsultarNFSeServicoPrestado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  {
    O WebService esta retornando a tag de fechamento da Resposta sem o caracter "/"
  }
  FPMsgOrig := AMSG;

  Request := '<ConsultarNfseServicoPrestado>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</ConsultarNfseServicoPrestado>';

  Result := Executar('', Request, ['ConsultarNfseServicoPrestadoResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfiscv2.ConsultarNFSeServicoTomado(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  {
    O WebService esta retornando a tag de fechamento da Resposta sem o caracter "/"
  }
  FPMsgOrig := AMSG;

  Request := '<ConsultarNfseServicoTomado>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</ConsultarNfseServicoTomado>';

  Result := Executar('', Request, ['ConsultarNfseServicoTomadoResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfiscv2.Cancelar(ACabecalho, AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<CancelarNfse>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</CancelarNfse>';

  Result := Executar('', Request, ['CancelarNfseResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

function TACBrNFSeXWebserviceInfiscv2.SubstituirNFSe(ACabecalho,
  AMSG: String): string;
var
  Request: string;
begin
  FPMsgOrig := AMSG;

  Request := '<SubstituirNfse>';
  Request := Request + '<nfseCabecMsg>' + ACabecalho + '</nfseCabecMsg>';
  Request := Request + '<nfseDadosMsg>' + AMSG + '</nfseDadosMsg>';
  Request := Request + '</SubstituirNfse>';

  Result := Executar('', Request, ['SubstituirNfseResposta'],
                     ['xmlns="http://nfse.abrasf.org.br"']);
end;

end.
