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

unit NFSeBrasil.LerXml;

interface

uses
  SysUtils, Classes, StrUtils,
  ACBrNFSeXLerXml_ABRASFv1,
  ACBrXmlDocument;

type
  { TNFSeR_NFSeBrasil }

  TNFSeR_NFSeBrasil = class(TNFSeR_ABRASFv1)
  protected
    procedure LerPrestadorServico(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoPrestador(const ANode: TACBrXmlNode);

    procedure LerTomadorServico(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoTomador(const ANode: TACBrXmlNode);

    procedure LerInfNfse(const ANode: TACBrXmlNode);
    procedure LerIdentificacaoRps(const ANode: TACBrXmlNode);
    procedure LerServico(const ANode: TACBrXmlNode);
    procedure LerValores(const ANode: TACBrXmlNode);

  public
    function LerXml: Boolean; override;
    function LerXmlNfse(const ANode: TACBrXmlNode): Boolean;

  end;

implementation

uses
  ACBrUtil.Base,
  ACBrUtil.Strings,
  ACBrUtil.XMLHTML,
  ACBrConsts,
  ACBrXMLBase, ACBrNFSeXConversao;

//==============================================================================
// Essa unit tem por finalidade exclusiva ler o XML do provedor:
//     NFSeBrasil
//==============================================================================

{ TNFSeR_NFSeBrasil }

procedure TNFSeR_NFSeBrasil.LerIdentificacaoPrestador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoPrestador');

  with NFSe.Prestador.IdentificacaoPrestador do
  begin
    CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('CNPJ'), tcStr);
    InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
  end;
end;

procedure TNFSeR_NFSeBrasil.LerIdentificacaoRps(const ANode: TACBrXmlNode);
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  with NFSe.IdentificacaoRps do
  begin
    Numero := ObterConteudo(ANode.Childrens.FindAnyNs('NumeroRps'), tcStr);
    Serie  := '1'; // Não possui série
    Tipo   := trRPS;
  end;
end;

procedure TNFSeR_NFSeBrasil.LerIdentificacaoTomador(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('IdentificacaoTomador');

  if AuxNode <> nil then
  begin
    with NFSe.Tomador.IdentificacaoTomador do
    begin
      CpfCnpj := ObterConteudo(AuxNode.Childrens.FindAnyNs('CpfCnpj'), tcStr);
      InscricaoMunicipal := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoMunicipal'), tcStr);
      InscricaoEstadual := ObterConteudo(AuxNode.Childrens.FindAnyNs('InscricaoEstadual'), tcStr);
    end;
  end;
end;

procedure TNFSeR_NFSeBrasil.LerInfNfse(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
  SCompet: string;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('InfNfse');

  if AuxNode <> nil then
  begin
    NFSe.Numero            := ObterConteudo(AuxNode.Childrens.FindAnyNs('Numero'), tcStr);
    NFSe.CodigoVerificacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoVerificacao'), tcStr);
    NFSe.DataEmissao       := ObterConteudo(AuxNode.Childrens.FindAnyNs('DataEmissao'), tcDatVcto);
    NFSe.NfseSubstituida   := ObterConteudo(AuxNode.Childrens.FindAnyNs('NfseSubstituida'), tcStr);

    LerIdentificacaoRps(AuxNode);

    NFSe.DataEmissaoRps           := ObterConteudo(AuxNode.Childrens.FindAnyNs('DataEmissaoRps'), tcDatVcto);
    NFSe.NaturezaOperacao         := StrToNaturezaOperacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('NaturezaOperacao'), tcStr));
    NFSe.RegimeEspecialTributacao := FpAOwner.StrToRegimeEspecialTributacao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('RegimeEspecialTributacao'), tcStr));
    NFSe.OptanteSimplesNacional   := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('OptanteSimplesNacional'), tcStr));
    NFSe.IncentivadorCultural     := FpAOwner.StrToSimNao(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('IncentivadorCultural'), tcStr));

    SCompet := ObterConteudo(AuxNode.Childrens.FindAnyNs('Competencia'), tcStr);

    NFSe.Competencia := EncodeDate(StrToIntDef(Copy(SCompet, 1, 4), 0),
                                   StrToIntDef(Copy(SCompet, 5, 2), 0), 1);

    NFSe.NfseSubstituida := ObterConteudo(AuxNode.Childrens.FindAnyNs('NfseSubstituida'), tcStr);
    NFSe.OutrasInformacoes := ObterConteudo(AuxNode.Childrens.FindAnyNs('OutrasInformacoes'), tcStr);
    NFSe.OutrasInformacoes := StringReplace(NFSe.OutrasInformacoes, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

    LerServico(AuxNode);

    NFSe.ValorCredito := ObterConteudo(AuxNode.Childrens.FindAnyNs('ValorCredito'), tcDe2);

    LerPrestadorServico(AuxNode);
    LerTomadorServico(AuxNode);
    LerIntermediarioServico(AuxNode);
    LerOrgaoGerador(AuxNode);
    LerConstrucaoCivil(AuxNode);
  end;
end;

procedure TNFSeR_NFSeBrasil.LerPrestadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('PrestadorServico');

  if AuxNode <> nil then
  begin
    LerIdentificacaoPrestador(AuxNode);

    with NFSe.Prestador do
    begin
      RazaoSocial  := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);
      NomeFantasia := ObterConteudo(AuxNode.Childrens.FindAnyNs('NomeFantasia'), tcStr);
    end;

    LerEnderecoPrestadorServico(AuxNode, 'Endereco');
    LerContatoPrestador(AuxNode);
  end;
end;

procedure TNFSeR_NFSeBrasil.LerServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  CodigoItemServico, UFMunicipioPrestacao: string;
  CodMunicipioPrestacao: Integer;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Servico');

  if AuxNode <> nil then
  begin
    LerValores(AuxNode);

    CodigoItemServico := ObterConteudo(AuxNode.Childrens.FindAnyNs('ItemListaServico'), tcStr);

    with NFSe.Servico do
    begin
      ItemListaServico          := NormatizarItemListaServico(CodigoItemServico);
      xItemListaServico         := ItemListaServicoDescricao(ItemListaServico);

      if xItemListaServico = '' then
        xItemListaServico := ObterConteudo(AuxNode.Childrens.FindAnyNs('DescricaoItemListaServico'), tcStr);

      CodigoCnae                := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoCnae'), tcStr);
      CodigoTributacaoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoTributacaoMunicipio'), tcStr);
      Discriminacao             := ObterConteudo(AuxNode.Childrens.FindAnyNs('Discriminacao'), tcStr);
      Discriminacao := StringReplace(Discriminacao, FpQuebradeLinha,
                                      sLineBreak, [rfReplaceAll, rfIgnoreCase]);

      VerificarSeConteudoEhLista(Discriminacao);

      CodigoMunicipio := ObterConteudo(AuxNode.Childrens.FindAnyNs('CodigoMunicipio'), tcStr);

      CodMunicipioPrestacao := ObterConteudo(AuxNode.Childrens.FindAnyNs('MunicipioPrestacaoServico'), tcInt);

      if CodMunicipioPrestacao > 0 then
        MunicipioPrestacaoServico := ObterNomeMunicipioUF(CodMunicipioPrestacao, UFMunicipioPrestacao);
    end;
  end;
end;

procedure TNFSeR_NFSeBrasil.LerTomadorServico(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('TomadorServico');

  if AuxNode <> nil then
  begin
    LerIdentificacaoTomador(AuxNode);

    NFSe.Tomador.RazaoSocial := ObterConteudo(AuxNode.Childrens.FindAnyNs('RazaoSocial'), tcStr);

    LerEnderecoTomador(AuxNode);
    LerContatoTomador(AuxNode);
  end;
end;

procedure TNFSeR_NFSeBrasil.LerValores(const ANode: TACBrXmlNode);
var
  AuxNode: TACBrXmlNode;
  Ok: Boolean;
  Valor: Currency;
begin
  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode := ANode.Childrens.FindAnyNs('Valores');

  if AuxNode <> nil then
  begin
    with NFSe.Servico.Valores do
    begin
      ValorServicos   := StringToFloatDef(StringReplace(AuxNode.Childrens.FindAnyNs('ValorServicos').Content, '.', '', [rfReplaceAll]), 0);
      ValorDeducoes   := StringToFloatDef(StringReplace(AuxNode.Childrens.FindAnyNs('ValorDeducoes').Content, '.', '', [rfReplaceAll]), 0);
      ValorPis        := StringToFloatDef(StringReplace(AuxNode.Childrens.FindAnyNs('ValorPIS').Content, '.', '', [rfReplaceAll]), 0);
      ValorCofins     := StringToFloatDef(StringReplace(AuxNode.Childrens.FindAnyNs('ValorCOFINS').Content, '.', '', [rfReplaceAll]), 0);
      ValorInss       := StringToFloatDef(StringReplace(AuxNode.Childrens.FindAnyNs('ValorINSS').Content, '.', '', [rfReplaceAll]), 0);
      ValorIr         := StringToFloatDef(StringReplace(AuxNode.Childrens.FindAnyNs('ValorIR').Content, '.', '', [rfReplaceAll]), 0);
      ValorCsll       := StringToFloatDef(StringReplace(AuxNode.Childrens.FindAnyNs('ValorCSLL').Content, '.', '', [rfReplaceAll]), 0);
      IssRetido       := FpAOwner.StrToSituacaoTributaria(Ok, ObterConteudo(AuxNode.Childrens.FindAnyNs('ISSRetido'), tcStr));
      ValorIss        := StringToFloatDef(StringReplace(AuxNode.Childrens.FindAnyNs('ValorISS').Content, '.', '', [rfReplaceAll]), 0);
      OutrasRetencoes := StringToFloatDef(StringReplace(AuxNode.Childrens.FindAnyNs('OutrasRetencoes').Content, '.', '', [rfReplaceAll]), 0);
      BaseCalculo     := StringToFloatDef(StringReplace(AuxNode.Childrens.FindAnyNs('BaseCalculo').Content, '.', '', [rfReplaceAll]), 0);
      Aliquota        := StringToFloatDef(StringReplace(AuxNode.Childrens.FindAnyNs('AliquotaServicos').Content, '.', '', [rfReplaceAll]), 0);
      Valor           := StringToFloatDef(StringReplace(AuxNode.Childrens.FindAnyNs('ValorLiquidoNFSe').Content, '.', '', [rfReplaceAll]), 0);

      if Valor <> 0 then
        ValorLiquidoNfse := Valor;

      ValorIssRetido := StringToFloatDef(StringReplace(AuxNode.Childrens.FindAnyNs('ValorISSRetido').Content, '.', '', [rfReplaceAll]), 0);

      DescontoCondicionado   := StringToFloatDef(StringReplace(AuxNode.Childrens.FindAnyNs('ValorDescontoCondicionado').Content, '.', '', [rfReplaceAll]), 0);
      DescontoIncondicionado := StringToFloatDef(StringReplace(AuxNode.Childrens.FindAnyNs('ValorDescontoIncondicionado').Content, '.', '', [rfReplaceAll]), 0);

      RetencoesFederais := ValorPis + ValorCofins + ValorInss + ValorIr + ValorCsll;

      ValorTotalNotaFiscal := ValorServicos - DescontoCondicionado -
                              DescontoIncondicionado;
    end;
  end;
end;

function TNFSeR_NFSeBrasil.LerXml: Boolean;
var
  XmlNode: TACBrXmlNode;
begin
  FpQuebradeLinha := FpAOwner.ConfigGeral.QuebradeLinha;

  if EstaVazio(Arquivo) then
    raise Exception.Create('Arquivo xml não carregado.');

  // Se o XML não tiver a codificação incluir ela.
  if ObtemDeclaracaoXML(Arquivo) = '' then
    Arquivo := CUTF8DeclaracaoXML + Arquivo;

  // Alguns provedores não retornam o XML em UTF-8
  Arquivo := ConverteXMLtoUTF8(Arquivo);

  LerParamsTabIni(True);

  Arquivo := NormatizarXml(Arquivo);

  tpXML := TipodeXMLLeitura(Arquivo);

  if FDocument = nil then
    FDocument := TACBrXmlDocument.Create();

  Document.Clear();
  Document.LoadFromXml(Arquivo);

  XmlNode := Document.Root;

  if XmlNode = nil then
    raise Exception.Create('Arquivo xml vazio.');

  NFSe.Clear;

  if tpXML = txmlNFSe then
    Result := LerXmlNfse(XmlNode)
  else
    Result := LerXmlRps(XmlNode);

  FreeAndNil(FDocument);
end;

function TNFSeR_NFSeBrasil.LerXmlNfse(const ANode: TACBrXmlNode): Boolean;
var
  AuxNode1, AuxNode2: TACBrXmlNode;
begin
  Result := True;

  if not Assigned(ANode) or (ANode = nil) then Exit;

  AuxNode1 := ANode.Childrens.FindAnyNs('CompNfse');

  if AuxNode1 = nil then
  begin
    AuxNode1 := ANode;
    AuxNode2 := ANode.Childrens.FindAnyNs('Nfse')
  end
  else
    AuxNode2 := AuxNode1.Childrens.FindAnyNs('Nfse');

  LerInfNfse(AuxNode2);

  LerNfseCancelamento(AuxNode1);
  LerNfseSubstituicao(AuxNode1);

  LerCampoLink;
end;

end.
